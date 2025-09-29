# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2021-2025, Science and Technology Facilities Council.
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
#          S. Valat, Inria / Laboratoire Jean Kuntzmann
#          J. G. Wallwork, Met Office / University of Cambridge
#          M. Schreiber, Univ. Grenoble Alpes / Inria / Lab. Jean Kuntzmann
# -----------------------------------------------------------------------------

''' This module contains the implementation of the various OpenACC Directive
nodes.'''

import abc
from typing import Dict, List, Optional, Set, Tuple, Union

from psyclone.core import Signature
from psyclone.errors import GenerationError
from psyclone.psyir.nodes.acc_clauses import (
    ACCAsyncQueueClause, ACCCopyClause, ACCCopyInClause,
    ACCCopyOutClause)
from psyclone.psyir.nodes.acc_mixins import ACCAsyncMixin
from psyclone.psyir.nodes.atomic_mixin import AtomicDirectiveMixin
from psyclone.psyir.nodes.clause import Clause
from psyclone.psyir.nodes.codeblock import CodeBlock
from psyclone.psyir.nodes.datanode import DataNode
from psyclone.psyir.nodes.directive import (StandaloneDirective,
                                            RegionDirective)
from psyclone.psyir.nodes.node import Node
from psyclone.psyir.nodes.psy_data_node import PSyDataNode
from psyclone.psyir.nodes.routine import Routine
from psyclone.psyir.nodes.reference import Reference
from psyclone.psyir.nodes.schedule import Schedule


class ACCDirective(metaclass=abc.ABCMeta):
    # pylint: disable=too-few-public-methods
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


class ACCRegionDirective(ACCDirective, RegionDirective, metaclass=abc.ABCMeta):
    ''' Base class for all OpenACC region directive statements.

    '''
    # Textual description of the node.
    _children_valid_format = "Schedule, Clause*"

    def validate_global_constraints(self):
        '''
        Perform validation checks for any global constraints. This can only
        be done at code-generation time.

        :raises GenerationError: if this ACCRegionDirective encloses any form
            of PSyData node since calls to PSyData routines within OpenACC
            regions are not supported.

        '''
        # We need to make sure to call the right constructor here:
        # pylint: disable=bad-super-call
        super(RegionDirective, self).validate_global_constraints()

        data_nodes = self.walk((PSyDataNode, CodeBlock))
        if data_nodes:
            raise GenerationError(
                f"Cannot include CodeBlocks or calls to PSyData routines "
                f"within OpenACC regions but found "
                f"{[type(node).__name__ for node in data_nodes]} within a "
                f"region enclosed by an '{type(self).__name__}'")

    @staticmethod
    def _validate_child(position, child):
        '''
        :param int position: the position to be validated.
        :param child: a child to be validated.
        :type child: :py:class:`psyclone.psyir.nodes.Node`

        :return: whether the given child and position are valid for this node.
        :rtype: bool

        '''
        if position == 0:
            return isinstance(child, Schedule)
        return isinstance(child, Clause)

    @property
    def signatures(self) -> Union[
                                Tuple[Set[Signature]],
                                Tuple[Set[Signature], Set[Signature]]
                            ]:
        '''
        Returns a 1-tuple or a 2-tuple of sets depending on the working API.
        If a 1-tuple, the set includes both input and output signatures
        (whether to arrays or objects) required by the Kernel call(s) that are
        children of this directive. If a 2-tuple, the first entry is the set of
        input signatures and the second entry is the set of output signatures.
        The set(s) describe the quantities that must be available on the remote
        device (probably a GPU) before the parallel region can be begun.

        :returns: 1-tuple or 2-tuple of input and output sets of variable names

        '''
        # pylint: disable=import-outside-toplevel
        from psyclone.domain.lfric import LFRicInvokeSchedule
        from psyclone.gocean1p0 import GOInvokeSchedule
        from psyclone.psyir.tools.call_tree_utils import CallTreeUtils

        if self.ancestor((LFRicInvokeSchedule, GOInvokeSchedule)):
            # Look-up the kernels that are children of this node
            sig_set = set()
            for call in self.kernels():
                for arg_str in call.arguments.acc_args:
                    sig_set.add(Signature(arg_str))
            return (sig_set, )

        rwi = CallTreeUtils().get_in_out_parameters(self.children)
        return (set(rwi.signatures_read),
                set(rwi.signatures_written))


class ACCStandaloneDirective(ACCDirective, StandaloneDirective,
                             metaclass=abc.ABCMeta):
    ''' Base class for all standalone OpenACC directive statements. '''

    @staticmethod
    def _validate_child(position: int, child: Node) -> bool:
        '''
        :param position: the position to be validated.
        :param child: a child to be validated.

        :return: whether the given child and position are valid for this node.

        '''
        # Ensure we call the _validate_child() in the correct parent class..
        return StandaloneDirective._validate_child(position, child)


class ACCRoutineDirective(ACCStandaloneDirective):
    '''
    Class representing an "ACC routine" OpenACC directive in PSyIR.

    :param parallelism: the level of parallelism in the routine, one of
        "gang", "seq", "vector", "worker".

    '''
    SUPPORTED_PARALLELISM = ["gang", "seq", "vector", "worker"]

    def __init__(self, parallelism: str = "seq", **kwargs):
        self.parallelism = parallelism
        super().__init__(self, **kwargs)

    @property
    def parallelism(self):
        '''
        :returns: the clause describing the level of parallelism within this
                  routine (or a called one).
        :rtype: str

        '''
        return self._parallelism

    @parallelism.setter
    def parallelism(self, value: str):
        '''
        :param value: the new value for the level-of-parallelism within
            this routine (or a called one).

        :raises TypeError: if `value` is not a str.
        :raises ValueError: if `value` is not a recognised level of
            parallelism.

        '''
        if not isinstance(value, str):
            raise TypeError(
                f"Expected a str to specify the level of parallelism but got "
                f"'{type(value).__name__}'")
        if value.lower() not in self.SUPPORTED_PARALLELISM:
            raise ValueError(
                f"Expected one of {self.SUPPORTED_PARALLELISM} for the level "
                f"of parallelism but got '{value}'")
        self._parallelism = value.lower()

    def begin_string(self) -> str:
        '''Returns the beginning statement of this directive, i.e.
        "acc routine". The visitor is responsible for adding the
        correct directive beginning (e.g. "!$").

        :returns: the opening statement of this directive.

        '''
        return f"acc routine {self.parallelism}"


class ACCEnterDataDirective(ACCStandaloneDirective, ACCAsyncMixin):
    '''
    Class representing a "!$ACC enter data" OpenACC directive in
    an InvokeSchedule. Must be sub-classed for a particular API because the way
    in which fields are marked as being on the remote device is API-dependent.

    :param children: list of nodes which the directive should have as children.
    :param parent: the node in the InvokeSchedule to which to add this
        directive as a child.
    :param async_queue: Enable async support and attach it to the given queue.
        Can use False to disable, True to enable on default
        stream. Int to attach to the given stream ID or use a PSyIR
        expression to say at runtime what stream to be used.

    :raises TypeError: if async_queue is of the wrong type.

    '''
    def __init__(
                self,
                children: List[Node] = None,
                parent: Node = None,
                async_queue: Union[bool, int, DataNode] = False
            ):
        super().__init__(children=children, parent=parent)

        ACCAsyncMixin.__init__(self, async_queue)
        self._acc_dirs = None  # List of parallel directives

        self._sig_set = set()

    def lower_to_language_level(self) -> Node:
        '''
        In-place replacement of this directive concept into language level
        PSyIR constructs.

        :returns: the lowered version of this node.

        '''
        # We must generate a list of all of the fields accessed within OpenACC
        # compute constructs (i.e. OpenACC parallel and kernels directives)
        # 1. Find all parallel and kernels directives. We store this list
        # for later use in any sub-class.
        self._acc_dirs = self.ancestor(Routine).walk(
                (ACCParallelDirective, ACCKernelsDirective))
        # 2. For each directive, add the fields used by the kernels it
        # contains (as given by signatures) and add it to our set.
        # TODO GOcean grid properties are duplicated in this set under
        # different names (the OpenACC deep copy support should spot this).
        for pdir in self._acc_dirs:
            self._sig_set.update(*pdir.signatures)

        return super().lower_to_language_level()

    def begin_string(self):
        '''Returns the beginning statement of this directive. The visitor is
        responsible for adding the correct directive beginning (e.g. "!$").

        :returns: the opening statement of this directive.
        :rtype: str

        :raises GenerationError: if there are no variables to copy to
                                 the device.
        '''
        if not self._sig_set:
            # There should be at least one variable to copyin.
            # TODO #1872: this directive needs reimplementing using the Clause
            # class and proper lowering. When this is fixed it may be possible
            # to change RegionTrans.validate() so that it always uses
            # Node.debug_string() rather than only for CodeBlocks.
            raise GenerationError(
                "ACCEnterData directive did not find any data to copyin. "
                "Perhaps there are no ACCParallel or ACCKernels directives "
                "within the region?")

        # Variables need lexicographic sorting since sets guarantee no ordering
        # and members of composite variables must appear later in deep copies.
        sym_list = _sig_set_to_string(self._sig_set)

        # options
        options = f" copyin({sym_list})"

        return f"acc enter data{options}"

    def data_on_device(self, parent: Node):
        '''
        Adds nodes into an InvokeSchedule to flag that the data required by the
        kernels in the data region is now on the device. The generic
        implementation doesn't add any node but this can be redefined in the
        APIs if any infrastructure call is needed.

        :param parent: the node in the InvokeSchedule to which to add nodes
        '''


class ACCParallelDirective(ACCRegionDirective, ACCAsyncMixin):
    '''
    Class representing the !$ACC PARALLEL directive of OpenACC
    in the PSyIR. By default it includes the `DEFAULT(PRESENT)` clause which
    means this node must either come after an EnterDataDirective or within
    a DataDirective.

    :param default_present: whether this directive includes the
        `DEFAULT(PRESENT)` clause or not.
    :param async_queue: Enable async support and attach it to the given queue.
        Can use False to disable, True to enable on default
        stream. Int to attach to the given stream ID or use a PSyIR
        expression to say at runtime what stream to be used.

    '''
    def __init__(
                self,
                async_queue: Union[bool, int, DataNode] = False,
                default_present: bool = True,
                **kwargs
            ):
        super().__init__(**kwargs)
        ACCAsyncMixin.__init__(self, async_queue)
        self.default_present = default_present

    def __eq__(self, other) -> bool:
        '''
        Checks whether two nodes are equal. Two ACCParallelDirective nodes are
        equal if their default_present members are equal and they use the
        same async_queue.

        :param object other: the object to check equality to.

        :returns: whether other is equal to self.

        '''
        is_eq = super().__eq__(other)
        is_eq = is_eq and self.default_present == other.default_present
        is_eq = is_eq and ACCAsyncMixin.__eq__(self, other)

        return is_eq

    def begin_string(self) -> str:
        '''
        Returns the beginning statement of this directive, i.e.
        "acc parallel" plus any qualifiers. The backend is responsible for
        adding the correct characters to mark this as a directive (e.g. "!$").

        :returns: the opening statement of this directive.

        '''
        options = ""
        if self._default_present:
            # "default(present)" means that the compiler is to assume that
            # all data required by the parallel region is already present
            # on the device. If we've made a mistake and it isn't present
            # then we'll get a run-time error.
            options = " default(present)"

        return f"acc parallel{options}"

    def end_string(self) -> str:
        '''
        :returns: the closing statement for this directive.

        '''
        return "acc end parallel"

    @property
    def default_present(self) -> bool:
        '''
        :returns: whether the directive includes the 'default(present)' clause.

        '''
        return self._default_present

    @default_present.setter
    def default_present(self, value):
        '''
        :param bool value: whether the directive should include the
            'default(present)' clause.

        :raises TypeError: if the given value is not a boolean.

        '''
        if not isinstance(value, bool):
            raise TypeError(
                f"The ACCParallelDirective default_present property must be "
                f"a boolean but value '{value}' has been given.")
        self._default_present = value

    @property
    def fields(self) -> List[str]:
        '''
        Returns a list of the names of field objects required by the Kernel
        call(s) that are children of this directive.

        :returns: list of names of field arguments.

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

    :param collapse: Number of nested loops to collapse into a single
        iteration space or None.
    :param independent: Whether or not to add the `independent` clause
        to the loop directive.
    :param sequential: whether or not to add the `seq` clause to the
        loop directive.
    :param gang: whether or not to add the `gang` clause to the
        loop directive.
    :param vector: whether or not to add the `vector` clause to the
        loop directive.
    :param kwargs: additional keyword arguments provided to the super class.

    '''
    def __init__(
                self,
                collapse: int = None,
                independent: bool = True,
                sequential: bool = False,
                gang: bool = False,
                vector: bool = False,
                **kwargs: Dict
            ):
        self.collapse = collapse
        self._independent = independent
        self._sequential = sequential
        self._gang = gang
        self._vector = vector
        self._check_clauses_consistent()
        super().__init__(**kwargs)

    def __eq__(self, other) -> bool:
        '''
        Checks whether two nodes are equal. Two ACCLoopDirective nodes are
        equal if their collapse, independent, sequential, gang, and vector
        members are equal.

        :param object other: the object to check equality to.

        :returns: whether other is equal to self.

        '''
        is_eq = super().__eq__(other)
        is_eq = is_eq and self.collapse == other.collapse
        is_eq = is_eq and self.independent == other.independent
        is_eq = is_eq and self.sequential == other.sequential
        is_eq = is_eq and self.gang == other.gang
        is_eq = is_eq and self.vector == other.vector

        return is_eq

    def _check_clauses_consistent(self):
        '''
        Check that the clauses applied to the loop directive make sense.

        :raises ValueError: if sequential is used in conjunction with gang
            and/or vector
        '''
        if self.sequential and (self.gang or self.vector):
            raise ValueError(
                "The OpenACC seq clause cannot be used in conjunction with the"
                " gang or vector clauses."
            )

    @property
    def collapse(self) -> Union[int, None]:
        '''
        :returns: the number of nested loops to collapse into a single \
                  iteration space for this node.

        '''
        return self._collapse

    @collapse.setter
    def collapse(self, value: Optional[int]):
        '''
        :param value: optional number of nested loop to collapse into a \
            single iteration space to parallelise. Defaults to None.

        :raises TypeError: if the collapse value given is not an integer \
            or NoneType.
        :raises ValueError: if the collapse integer given is not positive.

        '''
        if value is not None and not isinstance(value, int):
            raise TypeError(
                f"The ACCLoopDirective collapse clause must be a positive "
                f"integer or None, but value '{value}' has been given.")

        if value is not None and value <= 0:
            raise ValueError(
                f"The ACCLoopDirective collapse clause must be a positive "
                f"integer or None, but value '{value}' has been given.")

        self._collapse = value

    @property
    def independent(self) -> bool:
        ''' Returns whether the independent clause will be added to this
        loop directive.

        :returns: whether the independent clause will be added to this loop \
                  directive.

        '''
        return self._independent

    @property
    def sequential(self) -> bool:
        '''
        :returns: whether or not the `seq` clause is added to this loop \
                  directive.

        '''
        return self._sequential

    @property
    def gang(self) -> bool:
        '''
        :returns: whether or not the `gang` clause is added to this loop
                  directive.

        '''
        return self._gang

    @property
    def vector(self) -> bool:
        '''
        :returns: whether or not the `vector` clause is added to this loop
                  directive.

        '''
        return self._vector

    def node_str(self, colour: bool = True) -> str:
        '''Returns the name of this node with (optional) control codes
        to generate coloured output in a terminal that supports it.

        :param colour: whether or not to include colour control codes.

        :returns: description of this node, possibly coloured.

        '''
        self._check_clauses_consistent()
        text = self.coloured_name(colour)
        text += f"[sequential={self.sequential},"
        text += f"gang={self.gang},"
        text += f"vector={self.vector},"
        text += f"collapse={self.collapse},"
        text += f"independent={self.independent}]"
        return text

    def validate_global_constraints(self):
        '''Perform validation of those global constraints that can only be done
        at code-generation time.

        :raises GenerationError: if this ACCLoopDirective is not enclosed
            within some OpenACC parallel or kernels region and is not in a
            Routine that has been marked up with an 'ACC Routine' directive.
        '''
        parent_routine = self.ancestor(Routine)
        if not (self.ancestor((ACCParallelDirective, ACCKernelsDirective),
                              limit=parent_routine) or
                (parent_routine and parent_routine.walk(ACCRoutineDirective))):
            location = (f"in routine '{parent_routine.name}' " if
                        parent_routine else "")
            raise GenerationError(
                f"ACCLoopDirective {location}must either have an "
                f"ACCParallelDirective or ACCKernelsDirective as an ancestor "
                f"in the Schedule or the routine must contain an "
                f"ACCRoutineDirective.")

        super().validate_global_constraints()

    def begin_string(self, leading_acc: bool = True) -> str:
        ''' Returns the opening statement of this directive, i.e.
        "acc loop" plus any qualifiers. If `leading_acc` is False then
        the leading "acc loop" text is not included.

        :param leading_acc: whether or not to include the leading
            "acc loop" in the text that is returned.

        :returns: the opening statement of this directive.

        '''
        clauses = []
        if leading_acc:
            clauses = ["acc", "loop"]

        self._check_clauses_consistent()
        if self.sequential:
            clauses += ["seq"]
        else:
            if self.gang:
                clauses += ["gang"]
            if self.vector:
                clauses += ["vector"]
            if self.independent:
                clauses += ["independent"]
            if self.collapse:
                clauses += [f"collapse({self.collapse})"]
        return " ".join(clauses)

    def end_string(self) -> str:
        '''
        Would return the end string for this directive but "acc loop"
        doesn't have a closing directive.

        :returns: empty string.

        '''
        return ""


class ACCKernelsDirective(ACCRegionDirective, ACCAsyncMixin):
    '''Class representing the `!$ACC KERNELS` directive in the PSyIR.

    :param children: the PSyIR nodes to be enclosed in the Kernels region
        and which are therefore children of this node.
    :param parent: the parent of this node in the PSyIR.
    :param bool default_present: whether or not to add the
        "default(present)" clause to the kernels directive.
    :param async_queue: Enable async support and attach it to the given queue.
        Can use False to disable, True to enable on default
        stream. Int to attach to the given stream ID or use a PSyIR
        expression to say at runtime what stream to be used.

    '''
    _children_valid_format = "Schedule, Clause*"

    def __init__(
                self,
                children: List[Node] = None,
                parent: Node = None,
                default_present: bool = True,
                async_queue: Union[bool, int, DataNode] = False
            ):
        super().__init__(children=children, parent=parent)
        ACCAsyncMixin.__init__(self, async_queue)
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
        is_eq = is_eq and ACCAsyncMixin.__eq__(self, other)

        return is_eq

    @staticmethod
    def _validate_child(position: int, child: Node) -> bool:
        '''
        :param int position: the position to be validated.
        :param child: a child to be validated.
        :type child: :py:class:`psyclone.psyir.nodes.Node`

        :return: whether the given child and position are valid for this node.
        :rtype: bool

        '''
        if position == 0:
            return isinstance(child, Schedule)
        return isinstance(child, ACCAsyncQueueClause)

    @property
    def default_present(self) -> bool:
        '''
        :returns: whether the "default(present)" clause is added to the
            kernels directive.

        '''
        return self._default_present

    def begin_string(self) -> str:
        '''Returns the beginning statement of this directive, i.e.
        "acc kernels ...". The backend is responsible for adding the
        correct directive beginning (e.g. "!$").

        :returns: the beginning statement for this directive.

        '''
        result = "acc kernels"

        # present
        if self._default_present:
            result += " default(present)"

        return result

    def end_string(self) -> str:
        '''Returns the ending statement for this directive. The backend is
        responsible for adding the language-specific syntax that marks this
        as a directive.

        :returns: the closing statement for this directive.

        '''
        return "acc end kernels"


class ACCDataDirective(ACCRegionDirective):
    '''
    Class representing the !$ACC DATA ... !$ACC END DATA directive
    in the PSyIR.

    '''

    @staticmethod
    def _validate_child(position: int, child: Node) -> bool:
        '''
        Check that the supplied node is a valid child of this node at the
        specified position.

        :param position: the proposed position of this child in the list
            of children.
        :param child: the proposed child node.

        :returns: whether or not the proposed child and position are valid.

        '''
        if position == 0:
            return isinstance(child, Schedule)
        return isinstance(child, (ACCCopyClause, ACCCopyInClause,
                                  ACCCopyOutClause))

    def begin_string(self) -> str:
        '''
        :returns: the beginning of the opening statement of this directive.

        '''
        return "acc data"

    def end_string(self) -> str:
        '''
        :returns: the text for the end of this directive region.

        '''
        return "acc end data"

    def _update_node(self):
        '''
        Called whenever there is a change in the PSyIR tree below this node.

        Ensures that the various data-movement clauses are up-to-date.

        '''
        super()._update_node()
        self._update_data_movement_clauses()

    def _update_data_movement_clauses(self):
        '''
        Updates the data-movement clauses on this directive.

        First removes any such clauses and then regenerates them using
        dependence analysis to determine which variables (if any) need moving.

        '''
        # Remove the clauses that we will update.
        for child in self.children[:]:
            if isinstance(child,
                          (ACCCopyInClause, ACCCopyOutClause, ACCCopyClause)):
                self.children.remove(child)

        # Use dependence analysis to identify the variables that are read,
        # written and read+written within the tree below this node.
        reads, writes, readwrites = self.create_data_movement_deep_copy_refs()

        if reads:
            self.addchild(ACCCopyInClause(children=list(reads.values())))

        if writes:
            self.addchild(ACCCopyOutClause(children=list(writes.values())))

        if readwrites:
            self.addchild(ACCCopyClause(children=list(readwrites.values())))


class ACCUpdateDirective(ACCStandaloneDirective, ACCAsyncMixin):
    ''' Class representing the OpenACC update directive in the PSyIR. It has
    a direction attribute that can be set to `self`, `host` or `device`, the
    set of symbols being updated and an optional if_present clause.

    :param signatures: the access signature(s) that need to be synchronised
        with the device.
    :param direction: the direction of the synchronisation.
    :param children: list of nodes which the directive should have as children.
    :param parent: the node in the InvokeSchedule to which to add this
        directive as a child.
    :param if_present: whether or not to include the `if_present`
        clause on the update directive (this instructs the
        directive to silently ignore any variables that are not
        on the device).
    :param async_queue: Enable async support and attach it to the given queue.
        Can use False to disable, True to enable on default
        stream. Int to attach to the given stream ID or use a PSyIR
        expression to say at runtime what stream to be used.

    '''

    _VALID_DIRECTIONS = ("self", "host", "device")

    def __init__(
                self,
                signatures: Signature,
                direction: str,
                children: List[Node] = None,
                parent: Node = None,
                if_present: Optional[bool] = True,
                async_queue: Union[bool, int, DataNode] = False
            ):
        super().__init__(children=children, parent=parent)
        ACCAsyncMixin.__init__(self, async_queue)
        self.sig_set = signatures
        self.direction = direction
        self.if_present = if_present

    def __eq__(self, other) -> bool:
        '''
        Checks whether two nodes are equal. Two ACCUpdateDirective nodes are
        equal if their sig_set, direction and if_present members are equal.

        :param object other: the object to check equality to.

        :returns: whether other is equal to self.

        '''
        is_eq = super().__eq__(other)
        is_eq = is_eq and self.sig_set == other.sig_set
        is_eq = is_eq and self.direction == other.direction
        is_eq = is_eq and self.if_present == other.if_present
        is_eq = is_eq and ACCAsyncMixin.__eq__(self, other)

        return is_eq

    @property
    def sig_set(self) -> Signature:
        '''
        :returns: the set of signatures to synchronise with the device.

        '''
        return self._sig_set

    @property
    def direction(self) -> str:
        '''
        :returns: the direction of the synchronisation.

        '''
        return self._direction

    @property
    def if_present(self) -> bool:
        '''
        :returns: whether or not to add the 'if_present' clause.

        '''
        return self._if_present

    @sig_set.setter
    def sig_set(self, signatures: Signature):
        '''
        :param signatures: the access signature(s) that need to be \
                           synchronised with the device.

        :raises TypeError: if signatures is not a set of access signatures.
        '''
        if not all(isinstance(sig, Signature) for sig in signatures):
            raise TypeError(
                f"The ACCUpdateDirective signatures argument must be a "
                f"set of signatures but got "
                f"{set(type(sig).__name__ for sig in signatures)}")

        self._sig_set = signatures

    @direction.setter
    def direction(self, direction: str):
        '''
        :param direction: the direction of the synchronisation.

        :raises ValueError: if the direction argument is not a string with \
                        value 'self', 'host' or 'device'.
        '''
        if direction not in self._VALID_DIRECTIONS:
            raise ValueError(
                f"The ACCUpdateDirective direction argument must be a string "
                f"with any of the values in '{self._VALID_DIRECTIONS}' but "
                f"found '{direction}'.")

        self._direction = direction

    @if_present.setter
    def if_present(self, if_present: bool):
        '''
        :param if_present: whether or not to add the 'if_present' \
                                    clause.

        :raises TypeError: if `if_present` is not a boolean.
        '''
        if not isinstance(if_present, bool):
            raise TypeError(
                f"The ACCUpdateDirective if_present argument must be a "
                f"boolean but got {type(if_present).__name__}")

        self._if_present = if_present

    def begin_string(self) -> str:
        '''
        Returns the beginning statement of this directive, i.e.
        "acc update host(symbol)". The backend is responsible for adding the
        correct characters to mark this as a directive (e.g. "!$").

        :returns: the opening statement of this directive.

        '''
        if not self._sig_set:
            # There should be at least one variable to update.
            # TODO #1872: this directive needs reimplementing using the Clause
            # class and proper lowering.
            raise GenerationError(
                "ACCUpdate directive did not find any data to update. "
                "This most likely happened because a specialisation of "
                "ACCUpdateDirective.lower_to_level_language removed all the "
                "variables this directive was created to update.")

        condition = "if_present " if self._if_present else ""
        sym_list = _sig_set_to_string(self._sig_set)

        return f"acc update {condition}{self._direction}({sym_list})"


def _sig_set_to_string(sig_set: Set[Signature]) -> str:
    '''
    Converts the provided set of signatures into a lexically sorted
    string of comma-separated signatures which also includes, for signatures
    that represent variables of a derived type, the composing subsignatures.

    :param sig_set: set of signature(s) to include in the string.
    :returns: a lexically sorted string of comma-separated (sub)signatures.

    '''
    names = {s[:i+1].to_language() for s in sig_set for i in range(len(s))}
    return ",".join(sorted(names))


class ACCWaitDirective(ACCStandaloneDirective):
    '''
    Class representing the !$ACC WAIT directive in the PSyIR.

    :param wait_queue: Which ACC async stream to wait. None to wait all.

    '''
    def __init__(self, wait_queue: Union[Reference, int, None] = None):
        # call parent
        super().__init__()
        self.wait_queue = wait_queue

    def __eq__(self, other) -> bool:
        '''
        Test the equality of two directives.

        :returns: If the two directives are equals.

        '''
        is_eq = super().__eq__(other)
        is_eq = is_eq and self._wait_queue == other._wait_queue
        return is_eq

    @property
    def wait_queue(self) -> Union[int, Reference, None]:
        '''
        :returns: The queue to wait on.

        '''
        return self._wait_queue

    @wait_queue.setter
    def wait_queue(self, wait_queue: Union[int, Reference, None]):
        '''
        Setter to assign a specific wait queue to wait for.

        :param wait_queue: The wait queue to expect, or None for all.

        :raises TypeError: if `wait_queue` is of the wrong type
        '''
        # check
        if (wait_queue is not None
           and not isinstance(wait_queue, (int, Reference))):
            raise TypeError("Invalid value type as wait_group, shoule be"
                            "in (None, int, Signature) !")

        # set
        self._wait_queue = wait_queue

    def begin_string(self) -> str:
        '''Returns the beginning statement of this directive, i.e.
        "acc wait ...". The backend is responsible for adding the
        correct directive beginning (e.g. "!$").

        :returns: the beginning statement for this directive.

        '''
        # default basic directive
        result = "acc wait"

        # handle specifying groups
        if self._wait_queue is not None:
            if isinstance(self._wait_queue, Reference):
                result += f" ({self._wait_queue.name})"
            else:
                result += f" ({self._wait_queue})"

        # ok return it
        return result


class ACCAtomicDirective(ACCRegionDirective, AtomicDirectiveMixin):
    '''
    OpenACC directive to represent that the memory accesses in the associated
    assignment must be performed atomically.
    Note that the standard supports blocks with 2 assignments but this is
    currently unsupported in the PSyIR.

    '''
    def begin_string(self) -> str:
        '''
        :returns: the opening string statement of this directive.

        '''
        return "acc atomic"

    def end_string(self) -> str:
        '''
        :returns: the ending string statement of this directive.

        '''
        return "acc end atomic"

    def validate_global_constraints(self):
        ''' Perform validation of those global constraints that can only be
        done at code-generation time.

        :raises GenerationError: if the ACCAtomicDirective associated
            statement does not conform to a valid OpenACC atomic operation.
        '''
        if not self.children or len(self.dir_body.children) != 1:
            raise GenerationError(
                f"Atomic directives must always have one and only one"
                f" associated statement, but found '{self.debug_string()}'")
        stmt = self.dir_body[0]
        if not self.is_valid_atomic_statement(stmt):
            raise GenerationError(
                f"Statement '{self.children[0].debug_string()}' is not a "
                f"valid OpenACC Atomic statement.")


# For automatic API documentation generation
__all__ = ["ACCRegionDirective", "ACCEnterDataDirective",
           "ACCParallelDirective", "ACCLoopDirective", "ACCKernelsDirective",
           "ACCDataDirective", "ACCUpdateDirective", "ACCStandaloneDirective",
           "ACCDirective", "ACCRoutineDirective", "ACCAtomicDirective",
           "ACCWaitDirective"]
