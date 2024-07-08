# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2021-2024, Science and Technology Facilities Council.
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
#
# * Redistributions of source code must retain the above copyright notice, this
#   list of conditions and the following disclaimer.
#
# * Redistributions in binary form must reproduce the above copyright notice,
#   this list of conditions and the following disclaimer in the documentation
#   and/or other materials provided with the distribution.
#
# * Neither the name of the copyright holder nor the names of its
#   contributors may be used to endorse or promote products derived from
#   this software without specific prior written permission.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
# "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
# LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
# FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
# COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
# INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
# BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
# LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
# CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
# LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
# ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
# POSSIBILITY OF SUCH DAMAGE.
# -----------------------------------------------------------------------------
# Authors R. W. Ford, A. R. Porter and S. Siso, STFC Daresbury Lab
#         I. Kavcic,    Met Office
#         C.M. Maynard, Met Office / University of Reading
#         J. Henrichs, Bureau of Meteorology
# Modified A. B. G. Chalk, STFC Daresbury Lab.
# -----------------------------------------------------------------------------

''' This module contains the Directive, RegionDirective, StandaloneDirective
    node implementation.'''

import abc
from collections import OrderedDict

from psyclone.configuration import Config
from psyclone.core import Signature, VariablesAccessInfo
from psyclone.errors import InternalError
from psyclone.f2pygen import CommentGen
from psyclone.psyir.nodes.array_of_structures_reference import (
    ArrayOfStructuresReference)
from psyclone.psyir.nodes.loop import Loop
from psyclone.psyir.nodes.reference import Reference
from psyclone.psyir.nodes.schedule import Schedule
from psyclone.psyir.nodes.statement import Statement
from psyclone.psyir.nodes.structure_reference import StructureReference
from psyclone.psyir.symbols.datatypes import ScalarType


class Directive(Statement, metaclass=abc.ABCMeta):
    '''
    Abstract base class for all Directive statements.

    '''
    # The prefix to use when code-generating this directive
    # (e.g. "OMP") must be set by a mixin or sub-class.
    _PREFIX = ""
    _colour = "green"

    @property
    @abc.abstractmethod
    def clauses(self):
        '''
        :returns: the Clauses associated with this directive.
        :rtype: List of :py:class:`psyclone.psyir.nodes.Clause`
        '''

    def create_data_movement_deep_copy_refs(self):
        '''
        Creates the References required to perform a deep copy (in e.g.
        OpenACC or OpenMP) of all of the quantities accessed in Nodes below
        this one in the tree. It distringuishes between those quantities that
        are only read, only written or are both read and written. The necessary
        References are added to the returned OrderedDicts in the order in which
        they must be copied.

        :returns: a 3-tuple containing dicts describing the quantities that
            are read-only, write-only and readwrite. Each dict contains
            References indexed by Signatures.
        :rtype: Tuple[OrderedDict[:py:class:`psyclone.core.Signature`,
                                  :py:class:`psyclone.psyir.nodes.Reference`]]

        '''
        readwrites = OrderedDict()
        read_only = OrderedDict()
        write_only = OrderedDict()
        table = self.scope.symbol_table

        var_info = VariablesAccessInfo()
        self.reference_accesses(var_info)

        for sig in var_info.all_signatures:
            vinfo = var_info[sig]
            node = vinfo.all_accesses[0].node
            sym = table.lookup(sig.var_name)

            if isinstance(sym.datatype, ScalarType):
                # We ignore scalars as these are typically copied by value.
                continue

            if var_info.has_read_write(sig):
                access_dict = readwrites
            else:
                if var_info.is_read(sig):
                    if var_info.is_written(sig):
                        if vinfo.is_written_first():
                            access_dict = write_only
                        else:
                            access_dict = readwrites
                    else:
                        access_dict = read_only
                else:
                    access_dict = write_only

            if not sig.is_structure:
                # This must be an array.
                # TODO #2304 - in languages such as C++ it will be necessary to
                # supply the extent of an array that is being accessed. For now
                # we only supply a Reference (which is sufficient in Fortran).
                if sig not in access_dict:
                    access_dict[sig] = Reference(node.symbol)
                continue

            # We have a structure access and so we need the list of
            # references required to do a 'deep copy'. This means that if
            # we have an access `a%b%c(i)` then we need references to `a`,
            # `a%b` and then `a%b%c`.

            # A Signature does not contain indexing information so we use
            # a PSyIR node that corresponds to this access.
            _, index_lists = node.get_signature_and_indices()

            # First add the root access (`a` in the above example).
            if Signature(node.symbol.name) not in access_dict:
                access_dict[Signature(node.symbol.name)] = Reference(
                    node.symbol)

            # Then work our way down the various members.
            for depth in range(1, len(sig)):
                if sig[:depth+1] not in access_dict:
                    if node.is_array:
                        base_cls = ArrayOfStructuresReference
                        # Copy the indices so as not to modify the original
                        # node.
                        base_args = [node.symbol,
                                     [idx.copy() for idx in node.indices]]
                    else:
                        base_cls = StructureReference
                        base_args = [node.symbol]
                    # Create the new lists of indices, one list for each
                    # member of the structure access apart from the last
                    # one where we assume the whole array (if it is an
                    # array) is accessed. Hence the loop is 1:depth and
                    # then we set the last one separately.
                    new_lists = []
                    for idx_list in index_lists[1:depth]:
                        new_lists.append([idx.copy() for idx in idx_list])
                    members = list(zip(sig[1:depth], new_lists))
                    # The last member has no array indexing.
                    members.append(sig[depth])
                    access_dict[sig[:depth+1]] = base_cls.create(
                        *base_args, members)
        return read_only, write_only, readwrites


class RegionDirective(Directive):
    '''
    Base class for all Directive nodes that have an associated
    region of code with them.

    All classes that generate RegionDirective statements (e.g. OpenMP,
    OpenACC, compiler-specific) inherit from this class.

    :param ast: the entry in the fparser2 parse tree representing the code
                contained within this directive or None.
    :type ast: Optional[:py:class:`fparser.two.Fortran2003.Base`]
    :param children: the nodes that will be children of this
                     Directive node or None.
    :type children: Optional[List[:py:class:`psyclone.psyir.nodes.Node`]]
    :param parent: PSyIR node that is the parent of this Directive or None.
    :type parent: Optional[:py:class:`psyclone.psyir.nodes.Node`]

    '''
    # Textual description of the node.
    _children_valid_format = "Schedule"

    def __init__(self, ast=None, children=None, parent=None):
        # A Directive always contains a Schedule
        super().__init__(ast, parent=parent)
        self.addchild(Schedule(children=children))

    @staticmethod
    def _validate_child(position, child):
        '''
        :param int position: the position to be validated.
        :param child: a child to be validated.
        :type child: :py:class:`psyclone.psyir.nodes.Node`

        :return: whether the given child and position are valid for this node.
        :rtype: bool

        '''
        return position == 0 and isinstance(child, Schedule)

    @property
    def dir_body(self):
        '''
        :returns: the Schedule associated with this directive.
        :rtype: :py:class:`psyclone.psyir.nodes.Schedule`

        :raises InternalError: if this node does not have a Schedule as \
                               its first child.
        '''
        if len(self.children) < 1 or not isinstance(self.children[0],
                                                    Schedule):
            raise InternalError(
                "Directive malformed or incomplete. It should have a "
                "Schedule as child 0 but found: "
                f"{[type(child).__name__ for child in self.children]}")
        return self.children[0]

    @property
    def clauses(self):
        '''
        :returns: the Clauses associated with this directive.
        :rtype: List of :py:class:`psyclone.psyir.nodes.Clause`
        '''
        if len(self.children) > 1:
            return self.children[1:]
        return []

    def gen_post_region_code(self, parent):
        '''
        Generates any code that must be executed immediately after the end of
        the region defined by this directive.

        TODO #1648 this method is only used by the gen_code() code-generation
        path and should be replaced by functionality in a
        'lower_to_language_level' method in an LFRic-specific subclass
        of the appropriate directive.

        :param parent: where to add new f2pygen nodes.
        :type parent: :py:class:`psyclone.f2pygen.BaseGen`

        '''
        if not Config.get().distributed_memory or self.ancestor(Loop):
            return
        # Have to import PSyLoop here to avoid a circular dependence.
        # pylint: disable=import-outside-toplevel
        from psyclone.domain.common.psylayer import PSyLoop

        commented = False
        for loop in self.walk(PSyLoop):
            if not isinstance(loop.parent, Loop):
                if not commented and loop.unique_modified_args("gh_field"):
                    commented = True
                    parent.add(CommentGen(parent, ""))
                    parent.add(CommentGen(parent,
                                          " Set halos dirty/clean for fields "
                                          "modified in the above loop(s)"))
                    parent.add(CommentGen(parent, ""))
                loop.gen_mark_halos_clean_dirty(parent)

        if commented:
            parent.add(CommentGen(parent, ""))
            parent.add(CommentGen(parent,
                                  " End of set dirty/clean section for "
                                  "above loop(s)"))
            parent.add(CommentGen(parent, ""))


class StandaloneDirective(Directive):
    '''
    Base class for all StandaloneDirective statements. This class is
    designed for directives which do not have code associated with
    them, e.g. OpenMP's taskwait.

    All classes that generate StandaloneDirective statements
    (e.g. OpenMP, OpenACC, compiler-specific) inherit from this class.

    '''
    # Textual description of the node.
    _children_valid_format = None

    @staticmethod
    def _validate_child(position, child):
        '''
        :param int position: the position to be validated.
        :param child: a child to be validated.
        :type child: :py:class:`psyclone.psyir.nodes.Node`

        :return: whether the given child and position are valid for this node.
        :rtype: bool

        '''
        # Children are not allowed for StandaloneDirective
        return False

    @property
    def clauses(self):
        '''
        :returns: the Clauses associated with this directive.
        :rtype: List of :py:class:`psyclone.psyir.nodes.Clause`
        '''
        # This should be uncommented once a standalone directive with
        # clauses exists
        # if len(self.children) > 0:
        #    return self.children
        return []


# For automatic API documentation generation
__all__ = ["Directive", "RegionDirective", "StandaloneDirective"]
