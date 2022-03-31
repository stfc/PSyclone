# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2021-2022, Science and Technology Facilities Council.
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
# Modified A. B. G. Chalk, STFC Daresbury Lab
# -----------------------------------------------------------------------------

''' This module contains the implementation of the various OpenACC Directive
nodes.'''

import abc
import six
from collections import OrderedDict
from psyclone.core import AccessType, VariablesAccessInfo
from psyclone.errors import GenerationError, InternalError
from psyclone.f2pygen import DirectiveGen, CommentGen
from psyclone.psyir.nodes.acc_clauses import (ACCCopyClause, ACCCopyInClause,
                                              ACCCopyOutClause)
from psyclone.psyir.nodes.array_of_structures_reference import \
    ArrayOfStructuresReference
from psyclone.psyir.nodes.codeblock import CodeBlock
from psyclone.psyir.nodes.directive import StandaloneDirective, \
    RegionDirective
from psyclone.psyir.nodes.loop import Loop
from psyclone.psyir.nodes.reference import Reference
from psyclone.psyir.nodes.routine import Routine
from psyclone.psyir.nodes.schedule import Schedule
from psyclone.psyir.nodes.psy_data_node import PSyDataNode
from psyclone.psyir.symbols import DataSymbol, ScalarType


@six.add_metaclass(abc.ABCMeta)
class ACCDirective():
    '''
    Base mixin class for all OpenACC directive statements.

    This class is useful to provide a unique common ancestor to all the
    OpenACC directives, for instance when traversing the tree with
    `node.walk(ACCDirective)`

    Note that classes inheriting from it must place the ACCDirective in
    front of the other Directive node sub-class, so that the Python
    MRO gives preference to this class's attributes.
    '''
    _PREFIX = "ACC"


@six.add_metaclass(abc.ABCMeta)
class ACCRegionDirective(ACCDirective, RegionDirective):
    ''' Base class for all OpenACC region directive statements. '''
    def validate_global_constraints(self):
        '''
        Perform validation checks for any global constraints. This can only
        be done at code-generation time.

        :raises GenerationError: if this ACCRegionDirective encloses any form \
            of PSyData node since calls to PSyData routines within OpenACC \
            regions are not supported.

        '''
        super(ACCRegionDirective, self).validate_global_constraints()

        data_nodes = self.walk((PSyDataNode, CodeBlock))
        if data_nodes:
            raise GenerationError(
                "Cannot include CodeBlocks or calls to PSyData routines within"
                " OpenACC regions but found {0} within a region enclosed "
                "by an '{1}'".format(
                    [type(node).__name__ for node in data_nodes],
                    type(self).__name__))


@six.add_metaclass(abc.ABCMeta)
class ACCStandaloneDirective(ACCDirective, StandaloneDirective):
    ''' Base class for all standalone OpenACC directive statements. '''


class ACCRoutineDirective(ACCStandaloneDirective):
    ''' Class representing a "!$ACC routine" OpenACC directive in PSyIR. '''

    def gen_code(self, parent):
        '''Generate the fortran ACC Routine Directive and any associated
        code.

        :param parent: the parent Node in the Schedule to which to add our \
                       content.
        :type parent: sub-class of :py:class:`psyclone.f2pygen.BaseGen`
        '''
        # Check the constraints are correct
        self.validate_global_constraints()

        # Generate the code for this Directive
        parent.add(DirectiveGen(parent, "acc", "begin", "routine", ""))

    def begin_string(self):
        '''Returns the beginning statement of this directive, i.e.
        "acc routine". The visitor is responsible for adding the
        correct directive beginning (e.g. "!$").

        :returns: the opening statement of this directive.
        :rtype: str

        '''
        # pylint: disable=no-self-use
        return "acc routine"


class ACCEnterDataDirective(ACCStandaloneDirective):
    '''
    Class representing a "!$ACC enter data" OpenACC directive in
    an InvokeSchedule. Must be sub-classed for a particular API because the way
    in which fields are marked as being on the remote device is API-
    -dependent.

    :param children: list of nodes which this directive should \
                     have as children.
    :type children: list of :py:class:`psyclone.psyir.nodes.Node`.
    :param parent: the node in the InvokeSchedule to which to add this \
                   directive as a child.
    :type parent: :py:class:`psyclone.psyir.nodes.Node`.
    '''
    def __init__(self, children=None, parent=None):
        super(ACCEnterDataDirective, self).__init__(children=children,
                                                    parent=parent)
        self._acc_dirs = None  # List of parallel directives

        # The _variables_to_copy are computed dynamically until the
        # _node_lowered flag is set to True, after that re-use the stored ones.
        self._variables_to_copy = []
        self._node_lowered = False

    def gen_code(self, parent):
        '''Generate the elements of the f2pygen AST for this Node in the
        Schedule.

        :param parent: node in the f2pygen AST to which to add node(s).
        :type parent: :py:class:`psyclone.f2pygen.BaseGen`

        :raises GenerationError: if no data is found to copy in.

        '''
        from psyclone.psyGen import InvokeSchedule
        self.validate_global_constraints()

        # We must generate a list of all of the fields accessed by
        # OpenACC kernels (calls within an OpenACC parallel or kernels
        # directive)
        # 1. Find all parallel and kernels directives. We store this list for
        #    later use in any sub-class.
        self._acc_dirs = self.ancestor(InvokeSchedule).walk(
                (ACCParallelDirective, ACCKernelsDirective))
        # 2. For each directive, loop over each of the fields used by
        #    the kernels it contains (this list is given by ref_list)
        #    and add it to our list if we don't already have it
        var_list = []
        # TODO grid properties are effectively duplicated in this list (but
        # the OpenACC deep-copy support should spot this).
        for pdir in self._acc_dirs:
            for var in pdir.ref_list:
                if var not in var_list:
                    var_list.append(var)
        # 3. Convert this list of objects into a comma-delimited string
        var_str = ",".join(var_list)
        # 4. Add the enter data directive.
        if var_str:
            copy_in_str = "copyin("+var_str+")"
        else:
            # There should be at least one variable to copyin.
            raise GenerationError(
                "ACCEnterData directive did not find any data to copyin. "
                "Perhaps there are no ACCParallel or ACCKernels directives "
                "within the region.")
        parent.add(DirectiveGen(parent, "acc", "begin", "enter data",
                                copy_in_str))
        # 5. Call an API-specific subclass of this class in case
        # additional declarations are required.
        self.data_on_device(parent)
        parent.add(CommentGen(parent, ""))

    def lower_to_language_level(self):
        '''
        In-place replacement of this directive concept into language level
        PSyIR constructs.

        '''
        from psyclone.psyGen import InvokeSchedule
        if not self._node_lowered:
            # We must generate a list of all of the fields accessed by
            # OpenACC kernels (calls within an OpenACC parallel or kernels
            # directive)
            # 1. Find all parallel and kernels directives. We store this list
            # for later use in any sub-class.
            self._acc_dirs = self.ancestor(InvokeSchedule).walk(
                    (ACCParallelDirective, ACCKernelsDirective))
            # 2. For each directive, loop over each of the fields used by
            #    the kernels it contains (this list is given by ref_list)
            #    and add it to our list if we don't already have it
            self._variables_to_copy = []
            # TODO grid properties are effectively duplicated in this list (but
            # the OpenACC deep-copy support should spot this).
            for pdir in self._acc_dirs:
                for var in pdir.ref_list:
                    if var not in self._variables_to_copy:
                        self._variables_to_copy.append(var)
            self._node_lowered = True

        super(ACCEnterDataDirective, self).lower_to_language_level()

    def begin_string(self):
        '''Returns the beginning statement of this directive. The visitor is
        responsible for adding the correct directive beginning (e.g. "!$").

        :returns: the opening statement of this directive.
        :rtype: str

        '''
        # The enter data clauses are given by the _variables_to_copy list
        var_str = ",".join(self._variables_to_copy)
        if var_str:
            copy_in_str = "copyin("+var_str+")"
        else:
            # There should be at least one variable to copyin.
            raise GenerationError(
                "ACCEnterData directive did not find any data to copyin. "
                "Perhaps there are no ACCParallel or ACCKernels directives "
                "within the region.")

        return "acc enter data " + copy_in_str

    def data_on_device(self, parent):
        '''
        Adds nodes into an InvokeSchedule to flag that the data required by the
        kernels in the data region is now on the device. The generic
        implementation doesn't add any node but this can be redefined in the
        APIs if any infrastructure call is needed.

        :param parent: the node in the InvokeSchedule to which to add nodes
        :type parent: :py:class:`psyclone.psyir.nodes.Node`
        '''


class ACCParallelDirective(ACCRegionDirective):
    '''
    Class representing the !$ACC PARALLEL directive of OpenACC
    in the PSyIR. By default it includes the 'DEFAULT(PRESENT)' clause which
    means this node must either come after an EnterDataDirective or within
    a DataDirective.

    '''
    def validate_global_constraints(self):
        '''
        Check that the PSyIR tree containing this node is valid. Since we
        use 'default(present)', this node must either be the child of an
        ACCDataDirective or the parent Schedule must contain an
        ACCEnterDataDirective before this one.

        :raises GenerationError: if this ACCParallel node is not preceded by \
            an ACCEnterDataDirective and is not the child of an \
            ACCDataDirective.

        '''
        # We can't use Node.ancestor() because the enter data directive does
        # not have children. Instead, we go back up to the Schedule and
        # walk down from there.
        routine = self.ancestor(Routine)
        nodes = routine.walk(ACCEnterDataDirective)
        # Check that any enter-data directive comes before this parallel
        # directive
        if nodes and nodes[0].abs_position > self.abs_position:
            raise GenerationError(
                "An ACC parallel region must be preceded by an ACC enter-"
                "data directive but in '{0}' this is not the case.".
                format(routine.name))

        if not nodes and not self.ancestor(ACCDataDirective):
            raise GenerationError(
                "An ACC parallel region must either be preceded by an ACC "
                "enter data directive or enclosed within an ACC data region "
                "but in '{0}' this is not the case.".format(routine.name))

        super(ACCParallelDirective, self).validate_global_constraints()

    def gen_code(self, parent):
        '''
        Generate the elements of the f2pygen AST for this Node in the Schedule.

        :param parent: node in the f2pygen AST to which to add node(s).
        :type parent: :py:class:`psyclone.f2pygen.BaseGen`

        '''
        self.validate_global_constraints()

        parent.add(DirectiveGen(parent, "acc", "begin", "parallel",
                                "default(present)"))

        for child in self.children:
            child.gen_code(parent)

        parent.add(DirectiveGen(parent, *self.end_string().split()))

    def begin_string(self):
        '''
        Returns the beginning statement of this directive, i.e.
        "acc parallel" plus any qualifiers. The backend is responsible
        for adding the correct characters to mark this as a directive (e.g.
        "!$").

        :returns: the opening statement of this directive.
        :rtype: str

        '''
        # pylint: disable=no-self-use
        # "default(present)" means that the compiler is to assume that
        # all data required by the parallel region is already present
        # on the device. If we've made a mistake and it isn't present
        # then we'll get a run-time error.
        return "acc parallel default(present)"

    def end_string(self):
        '''
        :returns: the closing statement for this directive.
        :rtype: str
        '''
        # pylint: disable=no-self-use
        return "acc end parallel"

    @property
    def ref_list(self):
        '''
        Returns a list of the references (whether to arrays or objects)
        required by the Kernel call(s) that are children of this
        directive. This is the list of quantities that must be
        available on the remote device (probably a GPU) before
        the parallel region can be begun.

        :returns: list of variable names
        :rtype: list of str
        '''
        variables = []

        # Look-up the kernels that are children of this node
        for call in self.kernels():
            for arg in call.arguments.acc_args:
                if arg not in variables:
                    variables.append(arg)
        return variables

    @property
    def fields(self):
        '''
        Returns a list of the names of field objects required by the Kernel
        call(s) that are children of this directive.

        :returns: list of names of field arguments.
        :rtype: list of str
        '''
        # Look-up the kernels that are children of this node
        fld_list = []
        for call in self.kernels():
            for arg in call.arguments.fields:
                if arg not in fld_list:
                    fld_list.append(arg)
        return fld_list


class ACCLoopDirective(ACCRegionDirective):
    '''
    Class managing the creation of a '!$acc loop' OpenACC directive.

    :param children: list of nodes that will be children of this directive.
    :type children: list of :py:class:`psyclone.psyir.nodes.Node`.
    :param parent: the node in the Schedule to which to add this directive.
    :type parent: :py:class:`psyclone.psyir.nodes.Node`.
    :param int collapse: Number of nested loops to collapse into a single \
                         iteration space or None.
    :param bool independent: Whether or not to add the `independent` clause \
                             to the loop directive.
    '''
    def __init__(self, children=None, parent=None, collapse=None,
                 independent=True, sequential=False):
        self._collapse = collapse
        self._independent = independent
        self._sequential = sequential
        super(ACCLoopDirective, self).__init__(children=children,
                                               parent=parent)

    def node_str(self, colour=True):
        '''
        Returns the name of this node with (optional) control codes
        to generate coloured output in a terminal that supports it.

        :param bool colour: whether or not to include colour control codes.

        :returns: description of this node, possibly coloured.
        :rtype: str
        '''
        text = self.coloured_name(colour)
        text += "[sequential={0},".format(self._sequential)
        text += "collapse={0},".format(self._collapse)
        text += "independent={0}".format(self._independent)
        text += "]"
        return text

    def validate_global_constraints(self):
        '''
        Perform validation of those global constraints that can only be done
        at code-generation time.

        :raises GenerationError: if this ACCLoopDirective is not enclosed \
                            within some OpenACC parallel or kernels region.
        '''
        # It is only at the point of code generation that we can check for
        # correctness (given that we don't mandate the order that a user can
        # apply transformations to the code). As an orphaned loop directive,
        # we must have an ACCParallelDirective or an ACCKernelsDirective as
        # an ancestor somewhere back up the tree.
        if not self.ancestor((ACCParallelDirective, ACCKernelsDirective)):
            raise GenerationError(
                "ACCLoopDirective must have an ACCParallelDirective or "
                "ACCKernelsDirective as an ancestor in the Schedule")

        super(ACCLoopDirective, self).validate_global_constraints()

    def gen_code(self, parent):
        '''
        Generate the f2pygen AST entries in the Schedule for this OpenACC
        loop directive.

        :param parent: the parent Node in the Schedule to which to add our
                       content.
        :type parent: sub-class of :py:class:`psyclone.f2pygen.BaseGen`
        :raises GenerationError: if this "!$acc loop" is not enclosed within \
                                 an ACC Parallel region.
        '''
        self.validate_global_constraints()

        # Add any clauses to the directive. We use self.begin_string() to avoid
        # code duplication.
        options_str = self.begin_string(leading_acc=False)

        parent.add(DirectiveGen(parent, "acc", "begin", "loop", options_str))

        for child in self.children:
            child.gen_code(parent)

    def begin_string(self, leading_acc=True):
        ''' Returns the opening statement of this directive, i.e.
        "acc loop" plus any qualifiers. If `leading_acc` is False then
        the leading "acc loop" text is not included.

        :param bool leading_acc: whether or not to include the leading \
                                 "acc loop" in the text that is returned.

        :returns: the opening statement of this directive.
        :rtype: str

        '''
        clauses = []
        if leading_acc:
            clauses = ["acc", "loop"]

        if self._sequential:
            clauses.append("seq")
        else:
            if self._independent:
                clauses.append("independent")
            if self._collapse:
                clauses.append("collapse({0})".format(self._collapse))
        return " ".join(clauses)

    def end_string(self):
        '''
        Would return the end string for this directive but "acc loop"
        doesn't have a closing directive.

        :returns: empty string.
        :rtype: str

        '''
        # pylint: disable=no-self-use
        return ""


class ACCKernelsDirective(ACCRegionDirective):
    '''
    Class representing the !$ACC KERNELS directive in the PSyIR.

    :param children: the PSyIR nodes to be enclosed in the Kernels region \
                     and which are therefore children of this node.
    :type children: list of sub-classes of \
                    :py:class:`psyclone.psyir.nodes.Node`
    :param parent: the parent of this node in the PSyIR.
    :type parent: sub-class of :py:class:`psyclone.psyir.nodes.Node`
    :param bool default_present: whether or not to add the "default(present)" \
                                 clause to the kernels directive.

    :raises NotImplementedError: if default_present is False.

    '''
    def __init__(self, children=None, parent=None, default_present=True):
        super(ACCKernelsDirective, self).__init__(children=children,
                                                  parent=parent)
        self._default_present = default_present

    def gen_code(self, parent):
        '''
        Generate the f2pygen AST entries in the Schedule for this
        OpenACC Kernels directive.

        :param parent: the parent Node in the Schedule to which to add this \
                       content.
        :type parent: sub-class of :py:class:`psyclone.f2pygen.BaseGen`

        '''
        self.validate_global_constraints()

        # We re-use the 'begin_string' method but must skip the leading 'acc'
        # that it includes.
        parent.add(DirectiveGen(parent, "acc", "begin",
                                *self.begin_string().split()[1:]))
        for child in self.children:
            child.gen_code(parent)
        parent.add(DirectiveGen(parent, *self.end_string().split()))

    @property
    def ref_list(self):
        '''
        Returns a list of the references (whether to arrays or objects)
        required by the Kernel call(s) that are children of this
        directive. This is the list of quantities that must be
        available on the remote device (probably a GPU) before
        the parallel region can be begun.

        :returns: list of variable names
        :rtype: list of str
        '''
        variables = []

        # Look-up the kernels that are children of this node
        for call in self.kernels():
            for arg in call.arguments.acc_args:
                if arg not in variables:
                    variables.append(arg)
        return variables

    def begin_string(self):
        '''Returns the beginning statement of this directive, i.e.
        "acc kernels ...". The backend is responsible for adding the
        correct directive beginning (e.g. "!$").

        :returns: the beginning statement for this directive.
        :rtype: str

        '''
        result = "acc kernels"
        if self._default_present:
            result += " default(present)"
        return result

    def end_string(self):
        '''
        Returns the ending statement for this directive. The backend is
        responsible for adding the language-specific syntax that marks this
        as a directive.

        :returns: the closing statement for this directive.
        :rtype: str

        '''
        # pylint: disable=no-self-use
        return "acc end kernels"


class ACCDataDirective(ACCRegionDirective):
    '''
    Class representing the !$ACC DATA ... !$ACC END DATA directive
    in the PSyIR.

    '''
    def __init__(self, children=None, parent=None):
        super().__init__(children=children, parent=parent)

        self._parent_loop_vars = []
        cursor = self.ancestor(Loop)
        while cursor:
            self._parent_loop_vars.append(cursor.variable)
            cursor = cursor.ancestor(Loop)

        # Identify the inputs and outputs to the region (variables that
        # are read and written).
        var_accesses = VariablesAccessInfo(children)
        table = self.scope.symbol_table
        readers = set()
        writers = set()
        for signature in var_accesses.all_signatures:
            sym = table.lookup(signature.var_name)
            accesses = var_accesses[signature]
            if isinstance(sym.datatype, ScalarType):
                # We ignore scalars as these are passed by value when OpenACC
                # kernels are launched.
                continue
            if accesses.is_read():
                readers.add(signature)
            if accesses.is_written():
                writers.add(signature)
        readwrites = readers.intersection(writers)
        # Are any of the read-writes written before they are read?
        for signature in list(readwrites)[:]:
            accesses = var_accesses[signature]
            if accesses[0].access_type == AccessType.WRITE:
                # First access is a write so treat as a write
                writers.add(signature)
                readers.discard(signature)
                readwrites.discard(signature)
        readers_list = sorted(list(readers - readwrites))
        writers_list = sorted(list(writers - readwrites))
        readwrites_list = sorted(list(readwrites))

        # We now need to create PSyIR references for all of the signatures
        # and add them as children of the appropriate clauses.
        nodes_dict = OrderedDict()
        for sig in readers_list:
            self.sig2ref(var_accesses, sig, nodes_dict)
        if nodes_dict:
            self.addchild(ACCCopyInClause(children=list(nodes_dict.values())))
        nodes_dict = OrderedDict()
        for sig in writers_list:
            self.sig2ref(var_accesses, sig, nodes_dict)
        if nodes_dict:
            self.addchild(ACCCopyOutClause(children=list(nodes_dict.values())))
        nodes_dict = OrderedDict()
        for sig in readwrites_list:
            self.sig2ref(var_accesses, sig, nodes_dict)
        if nodes_dict:
            self.addchild(ACCCopyClause(children=list(nodes_dict.values())))

    @staticmethod
    def sig2ref(var_accesses, sig, refs_dict):
        '''
        Update the supplied dict of accesses with those required by the
        supplied signature.

        :param var_accesses:
        :type var_accesses: :py:class:`psyclone.core.VariablesAccessInfo`
        :param sig: 
        :type sig: :py:class:`psyclone.core.Signature`
        :param OrderedDict refs_dict: the dict of accesses to update.

        '''
        from psyclone.core.signature import Signature
        from psyclone.psyir.nodes.array_mixin import ArrayMixin
        from psyclone.psyir.nodes.structure_reference import StructureReference
        # Having this import at the top level causes a circular dependency due
        # to psyGen importing FortranWriter at the top level.
        # pylint: disable=import-outside-toplevel
        from psyclone.psyGen import Kern

        node = var_accesses[sig].all_accesses[0].node

        if isinstance(node, StructureReference):

            for accesses in var_accesses[sig].all_accesses:
                # Find the loop variables for all Loops that contain this
                # access and are themselves within the data region.
                loop_vars = []
                cursor = accesses.node.ancestor(Loop)
                while cursor and cursor.ancestor(ACCDataDirective):
                    loop_vars.append(Signature(cursor.variable.name))
                    cursor = cursor.ancestor(Loop)
                # Now check whether any of these loop variables appear within
                # the structure reference
                active_loop_vars = {}
                # Loop over each component of the structure reference that is
                # an array access.
                array_accesses = accesses.node.walk(ArrayMixin)
                for access in array_accesses:
                    active_loop_vars[access.name] = []
                    var_accesses = VariablesAccessInfo(access.indices)
                    for var in loop_vars:
                        if var in var_accesses.all_signatures:
                            active_loop_vars[access.name].append(var.var_name)
                active_accesses = [pair for pair in active_loop_vars.items()
                                   if pair[1]]

                if len(active_accesses) > 1:
                    # For an access such as my_struct(ii)%my_array(ji) then
                    # if we're inside a loop over ii we'll actually need
                    # a loop to do the deep copy:
                    #   do ii = 1, N
                    #   !$ acc data copyin(my_struct(ii)%my_array)
                    #   end do
                    loop_vars = ', '.join(str(pair[1]) for pair in
                                          active_accesses)
                    raise NotImplementedError(
                        f"Data region contains a structure access "
                        f"'{node.name}' where more than one component "
                        f"({', '.join(pair[0] for pair in active_accesses)}) "
                        f"is an array and is iterated over (variables "
                        f"{loop_vars}). Deep copying of data for such an"
                        f" access is not implemented.")

            member_sig, index_lists = node.get_signature_and_indices()

            if Signature(node.symbol.name) not in refs_dict:
                refs_dict[Signature(node.symbol.name)] = Reference(node.symbol)

            for depth in range(1, len(member_sig)):
                if member_sig[:depth+1] not in refs_dict:
                    if node.is_array:
                        base_cls = ArrayOfStructuresReference
                        # Copy the indices so as not to modify the original
                        # node.
                        base_args = [node.symbol,
                                     [idx.copy() for idx in node.indices]]
                    else:
                        base_cls = StructureReference
                        base_args = [node.symbol]
                    # Create the new lists of indices, one list for each member
                    # of the structure access apart from the last one where
                    # we assume the whole array (if it is an array) is
                    # accessed. Hence the loop is 1:depth and then we set the
                    # last one separately.
                    new_lists = []
                    for idx_list in index_lists[1:depth]:
                        new_lists.append([idx.copy() for idx in idx_list])
                    new_lists.append([])
                    members = list(zip(member_sig[1:depth+1], new_lists))
                    refs_dict[member_sig[:depth+1]] = base_cls.create(
                        *base_args, members)
            return refs_dict


        if isinstance(node, Kern):
            if sig not in refs_dict:
                refs_dict[sig] = Reference(node.scope.symbol_table.lookup(
                    str(sig)))

        # TODO lookup array bounds here.
        if sig not in refs_dict:
            refs_dict[sig] = Reference(node.symbol)

    def gen_code(self, _):
        '''
        :raises InternalError: the ACC data directive is currently only \
                               supported for the NEMO API and that uses the \
                               PSyIR backend to generate code.
                               fparser2 parse tree.

        '''
        raise InternalError(
            "ACCDataDirective.gen_code should not have been called.")

    @staticmethod
    def _validate_child(position, child):
        '''
        '''
        if position == 0:
            return isinstance(child, Schedule)
        return isinstance(child, (ACCCopyClause, ACCCopyInClause,
                                  ACCCopyOutClause))

    def begin_string(self):
        '''Returns the beginning statement of this directive, i.e.
        "acc data". The backend is responsible for adding the
        correct directive beginning (e.g. "!$").

        :returns: the opening statement of this directive.
        :rtype: str

        TODO #1396 - remove this whole method in favour of having the
        visitor backend generate the code.

        '''
        def _create_access_list(signatures, var_accesses):
            '''
            Constructs a list of variables for inclusion in a data-access
            clause.

            :param signatures: the list of Signatures for which to create \
                entries in the list.
            :type signatures: list of :py:class:`psyclone.core.Signature`
            :param var_accesses: object holding details on all variable \
                accesses in the region to which the data-access clause applies.
            :type var_accesses: :py:class:`psyclone.core.VariablesAccessInfo`

            :returns: list of variable accesses.
            :rtype: list of str

            '''
            access_list = []
            for sig in signatures:
                if sig.is_structure:
                    # We have to do a 'deep copy' of any structure access. This
                    # means that if we have an access `a%b%c(i)` then we need
                    # to copy `a`, `a%b` and then `a%b%c`.
                    # Look up a PSyIR node that corresponds to this access.
                    current = var_accesses[sig].all_accesses[0].node
                    part_list = [current.name]
                    if current.name not in access_list:
                        access_list.append(current.name)
                    while hasattr(current, "member"):
                        current = current.member
                        # Currently this is hardwired to generate Fortran (i.e.
                        # we use '%' when accessing a component of a struct).
                        # TODO #1386 a new StructureReference needs to be
                        # created for 'current' and then given to an
                        # appropriate backend.
                        ref_string = "%".join(part_list[:]+[current.name])
                        if ref_string not in access_list:
                            access_list.append(ref_string)
                else:
                    ref_string = str(sig)
                    if ref_string not in access_list:
                        access_list.append(ref_string)
            return access_list

        result = "acc data"
        return result
        # TODO delete the rest!

        # Identify the inputs and outputs to the region (variables that
        # are read and written).
        var_accesses = VariablesAccessInfo(self)
        table = self.scope.symbol_table
        readers = set()
        writers = set()
        for signature in var_accesses.all_signatures:
            sym = table.lookup(signature.var_name)
            accesses = var_accesses[signature]
            if isinstance(sym.datatype, ScalarType):
                # We ignore scalars as these are passed by value when OpenACC
                # kernels are launched.
                continue
            if accesses.is_read():
                readers.add(signature)
            if accesses.is_written():
                writers.add(signature)
        readwrites = readers.intersection(writers)
        # Are any of the read-writes written before they are read?
        for signature in list(readwrites)[:]:
            accesses = var_accesses[signature]
            if accesses[0].access_type == AccessType.WRITE:
                # First access is a write so treat as a write
                writers.add(signature)
                readers.discard(signature)
                readwrites.discard(signature)
        readers_list = sorted(list(readers - readwrites))
        writers_list = sorted(list(writers - readwrites))
        readwrites_list = sorted(list(readwrites))
        if readers_list:
            result += " copyin({0})".format(
                ",".join(_create_access_list(readers_list, var_accesses)))
        if writers_list:
            result += " copyout({0})".format(
                ",".join(_create_access_list(writers_list, var_accesses)))
        if readwrites_list:
            result += " copy({0})".format(",".join(
                _create_access_list(readwrites_list, var_accesses)))

        return result

    def end_string(self):
        '''
        :returns: the text for the end of this directive region.
        :rtype: str

        '''
        # pylint: disable=no-self-use
        return "acc end data"


class ACCUpdateDirective(ACCStandaloneDirective):
    ''' Class representing the !$ACC UPDATE directive of OpenACC in the PSyIR.
    It includes a direction attribute that can be set to 'host' or 'device' and
    the symbol that is being updated.

    :param symbol: the symbol to synchronise with the accelerator.
    :type symbol: :py:class:`psyclone.psyir.symbols.DataSymbol`.
    :param str direction: the direction of the synchronisation.
    :param children: list of nodes which this directive should \
                     have as children.
    :type children: list of :py:class:`psyclone.psyir.nodes.Node`.
    :param parent: the node in the InvokeSchedule to which to add this \
                   directive as a child.
    :type parent: :py:class:`psyclone.psyir.nodes.Node`.


    :raises ValueError: if the direction argument is not a string with \
                            value 'host' or 'device'.
    :raises TypeError: if the symbol is not a DataSymbol.

    '''

    _VALID_DIRECTIONS = ("host", "device")

    def __init__(self, symbol, direction, children=None, parent=None):
        super(ACCUpdateDirective, self).__init__(children=children,
                                                 parent=parent)
        if not isinstance(direction, six.string_types) or direction not in \
                self._VALID_DIRECTIONS:
            raise ValueError(
                "The ACCUpdateDirective direction argument must be a string "
                "with any of the values in '{0}' but found '{1}'.".format(
                    self._VALID_DIRECTIONS, direction))

        if not isinstance(symbol, DataSymbol):
            raise TypeError(
                "The ACCUpdateDirective symbol argument must be a 'DataSymbol"
                "' but found '{0}'.".format(type(symbol).__name__))

        self._direction = direction
        self._symbol = symbol

    def begin_string(self):
        '''
        Returns the beginning statement of this directive, i.e.
        "acc update host(symbol)". The backend is responsible
        for adding the correct characters to mark this as a directive (e.g.
        "!$").

        :returns: the opening statement of this directive.
        :rtype: str

        '''
        return "acc update " + self._direction + "(" + self._symbol.name + ")"


# For automatic API documentation generation
__all__ = ["ACCRegionDirective", "ACCEnterDataDirective",
           "ACCParallelDirective", "ACCLoopDirective", "ACCKernelsDirective",
           "ACCDataDirective", "ACCUpdateDirective", "ACCStandaloneDirective",
           "ACCDirective", "ACCRoutineDirective"]
