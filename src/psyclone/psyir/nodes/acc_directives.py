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
# Authors R. W. Ford, A. R. Porter, S. Siso and N. Nobre, STFC Daresbury Lab
#         I. Kavcic,    Met Office
#         C.M. Maynard, Met Office / University of Reading
#         J. Henrichs, Bureau of Meteorology
# Modified A. B. G. Chalk, STFC Daresbury Lab
# -----------------------------------------------------------------------------

''' This module contains the implementation of the various OpenACC Directive
nodes.'''

from __future__ import absolute_import
import abc
import six
from psyclone.core import AccessType, VariablesAccessInfo
from psyclone.f2pygen import DirectiveGen, CommentGen
from psyclone.errors import GenerationError, InternalError
from psyclone.psyir.nodes.codeblock import CodeBlock
from psyclone.psyir.nodes.directive import StandaloneDirective, RegionDirective
from psyclone.psyir.nodes.psy_data_node import PSyDataNode
from psyclone.psyir.nodes.routine import Routine
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
                f"Cannot include CodeBlocks or calls to PSyData routines "
                f"within OpenACC regions but found "
                f"{[type(node).__name__ for node in data_nodes]} within a "
                f"region enclosed by an '{type(self).__name__}'")

    @property
    def kernel_references(self):
        '''
        Returns a set of the references (whether to arrays or objects)
        required by the Kernel call(s) that are children of this directive.
        This is the set of quantities that must be available on the remote
        device (probably a GPU) before the parallel region can be begun.

        :returns: set of variable names
        :rtype: Set[str]
        '''
        variables = set()

        # Look-up the kernels that are children of this node
        for call in self.kernels():
            for arg in call.arguments.acc_args:
                variables.add(arg)
        return variables


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
    :type children: List[:py:class:`psyclone.psyir.nodes.Node`]
    :param parent: the node in the InvokeSchedule to which to add this \
                   directive as a child.
    :type parent: :py:class:`psyclone.psyir.nodes.Node`
    '''
    def __init__(self, children=None, parent=None):
        super().__init__(children=children, parent=parent)
        self._acc_dirs = None  # List of parallel directives

        # The _sig_list are computed dynamically until the
        # _node_lowered flag is set to True, after that re-use the stored ones.
        self._sig_list = set()
        self._node_lowered = False

    def gen_code(self, parent):
        '''Generate the elements of the f2pygen AST for this Node in the
        Schedule.

        :param parent: node in the f2pygen AST to which to add node(s).
        :type parent: :py:class:`psyclone.f2pygen.BaseGen`

        :raises GenerationError: if no data is found to copy in.

        '''
        self.validate_global_constraints()
        self.lower_to_language_level()
        # Leverage begin_string() to raise an exception if there are no
        # variables to copyin but discard the generated string since it is
        # incompatible with class DirectiveGen() we are using below.
        self.begin_string()

        # Add the enter data directive.
        copy_in_str = "copyin(" + ",".join(sorted(self._sig_list)) + ")"
        parent.add(DirectiveGen(parent, "acc", "begin", "enter data",
                                copy_in_str))
        # Call an API-specific subclass of this class in case
        # additional declarations are required.
        self.data_on_device(parent)
        parent.add(CommentGen(parent, ""))

    def lower_to_language_level(self):
        '''
        In-place replacement of this directive concept into language level
        PSyIR constructs.

        '''
        # pylint: disable=import-outside-toplevel
        from psyclone.psyGen import InvokeSchedule

        if not self._node_lowered:
            # We must generate a list of all of the fields accessed by OpenACC
            # kernels (calls within an OpenACC parallel or kernels directive)
            # 1. Find all parallel and kernels directives. We store this list
            # for later use in any sub-class.
            self._acc_dirs = self.ancestor(InvokeSchedule).walk(
                    (ACCParallelDirective, ACCKernelsDirective))
            # 2. For each directive, add the fields used by the kernels it
            # contains (as given by kernel_references) and add it to our set.
            # TODO GOcean grid properties are duplicated in this set under
            # different names (the OpenACC deep copy support should spot this).
            for pdir in self._acc_dirs:
                self._sig_list.update(pdir.kernel_references)
            self._node_lowered = True

        super().lower_to_language_level()

    def begin_string(self):
        '''Returns the beginning statement of this directive. The visitor is
        responsible for adding the correct directive beginning (e.g. "!$").

        :returns: the opening statement of this directive.
        :rtype: str

        '''
        if not self._sig_list:
            # There should be at least one variable to copyin.
            raise GenerationError(
                "ACCEnterData directive did not find any data to copyin. "
                "Perhaps there are no ACCParallel or ACCKernels directives "
                "within the region?")

        # Variables need lexicographic sorting since sets guarantee no ordering
        # and members of composite variables must appear later in deep copies.
        return f"acc enter data copyin({','.join(sorted(self._sig_list))})"

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
                f"An ACC parallel region must be preceded by an ACC enter data"
                f" directive but in '{routine.name}' this is not the case.")

        if not nodes and not self.ancestor(ACCDataDirective):
            raise GenerationError(
                f"An ACC parallel region must either be preceded by an ACC "
                f"enter data directive or enclosed within an ACC data region "
                f"but in '{routine.name}' this is not the case.")

        super().validate_global_constraints()

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
        "acc parallel" plus any qualifiers. The backend is responsible for
        adding the correct characters to mark this as a directive (e.g. "!$").

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
    def fields(self):
        '''
        Returns a list of the names of field objects required by the Kernel
        call(s) that are children of this directive.

        :returns: list of names of field arguments.
        :rtype: List[str]
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
    :type children: List[:py:class:`psyclone.psyir.nodes.Node`]
    :param parent: the node in the Schedule to which to add this directive.
    :type parent: :py:class:`psyclone.psyir.nodes.Node`
    :param int collapse: Number of nested loops to collapse into a single \
                         iteration space or None.
    :param bool independent: Whether or not to add the `independent` clause \
                             to the loop directive.
    :param bool sequential: whether or not to add the `seq` clause to the \
                            loop directive.
    '''
    def __init__(self, children=None, parent=None, collapse=None,
                 independent=True, sequential=False):
        self._collapse = collapse
        self._independent = independent
        self._sequential = sequential
        super().__init__(children=children, parent=parent)

    def __eq__(self, other):
        '''
        Checks whether two nodes are equal. Two ACCLoopDirective nodes are
        equal if their collapse, independent and sequential members are equal.

        :param object other: the object to check equality to.

        :returns: whether other is equal to self.
        :rtype: bool
        '''
        is_eq = super().__eq__(other)
        is_eq = is_eq and self.collapse == other.collapse
        is_eq = is_eq and self.independent == other.independent
        is_eq = is_eq and self.sequential == other.sequential

        return is_eq

    @property
    def collapse(self):
        ''' Returns the number of nested loops to collapse into a single
        iteration space for this node.

        :returns: the number of nested loops to collapse into a single \
                  iteration space for this node.
        :rtype: int or None
        '''
        return self._collapse

    @property
    def independent(self):
        ''' Returns whether the independent clause will be added to this
        loop directive.

        :returns: whether the independent clause will be added to this loop \
                  directive.
        :rtype: bool
        '''
        return self._independent

    @property
    def sequential(self):
        '''
        Returns whether or not to add the `seq` clause to the loop directive.

        :returns: whether or not the `seq` clause is added to this loop \
                  directive.
        :rtype: bool
        '''
        return self._sequential

    def node_str(self, colour=True):
        '''
        Returns the name of this node with (optional) control codes
        to generate coloured output in a terminal that supports it.

        :param bool colour: whether or not to include colour control codes.

        :returns: description of this node, possibly coloured.
        :rtype: str
        '''
        text = self.coloured_name(colour)
        text += f"[sequential={self._sequential},"
        text += f"collapse={self._collapse},"
        text += f"independent={self._independent}]"
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

        super().validate_global_constraints()

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
                clauses.append(f"collapse({self._collapse})")
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
    :type children: List[:py:class:`psyclone.psyir.nodes.Node`]
    :param parent: the parent of this node in the PSyIR.
    :type parent: sub-class of :py:class:`psyclone.psyir.nodes.Node`
    :param bool default_present: whether or not to add the "default(present)" \
                                 clause to the kernels directive.

    :raises NotImplementedError: if default_present is False.

    '''
    def __init__(self, children=None, parent=None, default_present=True):
        super().__init__(children=children, parent=parent)
        self._default_present = default_present

    def __eq__(self, other):
        '''
        Checks whether two nodes are equal. Two ACCKernelsDirective nodes are
        equal if their default_present members are equal.

        :param object other: the object to check equality to.

        :returns: whether other is equal to self.
        :rtype: bool
        '''
        is_eq = super().__eq__(other)
        is_eq = is_eq and self.default_present == other.default_present

        return is_eq

    @property
    def default_present(self):
        ''' Returns whether the "default(present)" clause is added to the
        kernels directive.

        :returns: whether the "default(present)" clause is added to the \
                  kernels directive.
        :rtype: bool
        '''
        return self._default_present

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
    def gen_code(self, _):
        '''
        :raises InternalError: the ACC data directive is currently only \
                               supported for the NEMO API and that uses the \
                               PSyIR backend to generate code.
                               fparser2 parse tree.

        '''
        raise InternalError(
            "ACCDataDirective.gen_code should not have been called.")

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
            :type signatures: List[:py:class:`psyclone.core.Signature`]
            :param var_accesses: object holding details on all variable \
                accesses in the region to which the data-access clause applies.
            :type var_accesses: :py:class:`psyclone.core.VariablesAccessInfo`

            :returns: list of variable accesses.
            :rtype: List[str]

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
            result += f""" copyin({",".join(
                _create_access_list(readers_list, var_accesses))})"""
        if writers_list:
            result += f""" copyout({",".join(
                _create_access_list(writers_list, var_accesses))})"""
        if readwrites_list:
            result += f""" copy({",".join(
                _create_access_list(readwrites_list, var_accesses))})"""

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
    It includes a direction attribute that can be set to 'self', 'host' or
    'device' and the symbol that is being updated.

    :param symbol: the symbol to synchronise with the accelerator.
    :type symbol: :py:class:`psyclone.psyir.symbols.DataSymbol`
    :param str direction: the direction of the synchronisation.
    :param children: list of nodes which the directive should have as children.
    :type children: List[:py:class:`psyclone.psyir.nodes.Node`]
    :param parent: the node in the InvokeSchedule to which to add this \
                   directive as a child.
    :type parent: :py:class:`psyclone.psyir.nodes.Node`


    :raises ValueError: if the direction argument is not a string with \
                        value 'self', 'host' or 'device'.
    :raises TypeError: if the symbol is not a DataSymbol.

    '''

    _VALID_DIRECTIONS = ("self", "host", "device")

    def __init__(self, symbol, direction, children=None, parent=None):
        super().__init__(children=children, parent=parent)
        if not isinstance(direction, six.string_types) or direction not in \
                self._VALID_DIRECTIONS:
            raise ValueError(
                f"The ACCUpdateDirective direction argument must be a string "
                f"with any of the values in '{self._VALID_DIRECTIONS}' "
                f"but found '{direction}'.")

        if not isinstance(symbol, DataSymbol):
            raise TypeError(
                f"The ACCUpdateDirective symbol argument must be a 'DataSymbol"
                f"' but found '{type(symbol).__name__}'.")

        self._direction = direction
        self._symbol = symbol

    def __eq__(self, other):
        '''
        Checks whether two nodes are equal. Two ACCUpdateDirective nodes are
        equal if their direction and symbol members are equal.

        :param object other: the object to check equality to.

        :returns: whether other is equal to self.
        :rtype: bool
        '''
        is_eq = super().__eq__(other)
        is_eq = is_eq and self.direction == other.direction
        is_eq = is_eq and self.symbol == other.symbol

        return is_eq

    @property
    def direction(self):
        '''
        Returns the direction of the synchronisation.

        :returns: the direction of the synchronisation.
        :rtype: str
        '''
        return self._direction

    @property
    def symbol(self):
        '''
        Returns the symbol to synchronise with the accelerator.

        :returns: the symbol to synchronise with the accelerator.
        :rtype: :py:class:`psyclone.psyir.symbols.DataSymbol`
        '''
        return self._symbol

    def begin_string(self):
        '''
        Returns the beginning statement of this directive, i.e.
        "acc update host(symbol)". The backend is responsible for adding the
        correct characters to mark this as a directive (e.g. "!$").

        :returns: the opening statement of this directive.
        :rtype: str

        '''
        return "acc update " + self._direction + "(" + self._symbol.name + ")"


# For automatic API documentation generation
__all__ = ["ACCRegionDirective", "ACCEnterDataDirective",
           "ACCParallelDirective", "ACCLoopDirective", "ACCKernelsDirective",
           "ACCDataDirective", "ACCUpdateDirective", "ACCStandaloneDirective",
           "ACCDirective", "ACCRoutineDirective"]
