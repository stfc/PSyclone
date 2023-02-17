# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2021-2023, Science and Technology Facilities Council.
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
#         A. B. G. Chalk, STFC Daresbury Lab
#         I. Kavcic,    Met Office
#         C.M. Maynard, Met Office / University of Reading
#         J. Henrichs, Bureau of Meteorology
# -----------------------------------------------------------------------------

''' This module contains the implementation of the various OpenMP Directive
nodes.'''


import abc
import itertools
import sys

from psyclone.configuration import Config
from psyclone.core import AccessType, VariablesAccessInfo
from psyclone.errors import (GenerationError, InternalError,
                             UnresolvedDependencyError)
from psyclone.f2pygen import (AssignGen, UseGen, DeclGen, DirectiveGen,
                              CommentGen)
from psyclone.psyir.nodes.array_mixin import ArrayMixin
from psyclone.psyir.nodes.array_reference import ArrayReference
from psyclone.psyir.nodes.assignment import Assignment
from psyclone.psyir.nodes.call import Call
from psyclone.psyir.nodes.directive import StandaloneDirective, \
    RegionDirective
from psyclone.psyir.nodes.loop import Loop
from psyclone.psyir.nodes.literal import Literal
from psyclone.psyir.nodes.member import Member
from psyclone.psyir.nodes.omp_clauses import OMPGrainsizeClause, \
    OMPNowaitClause, OMPNogroupClause, OMPNumTasksClause, OMPPrivateClause,\
    OMPDefaultClause, OMPReductionClause, OMPScheduleClause
from psyclone.psyir.nodes.operation import BinaryOperation
from psyclone.psyir.nodes.ranges import Range
from psyclone.psyir.nodes.reference import Reference
from psyclone.psyir.nodes.routine import Routine
from psyclone.psyir.nodes.schedule import Schedule
from psyclone.psyir.nodes.structure_reference import StructureReference
from psyclone.psyir.symbols import INTEGER_TYPE

# OMP_OPERATOR_MAPPING is used to determine the operator to use in the
# reduction clause of an OpenMP directive.
OMP_OPERATOR_MAPPING = {AccessType.SUM: "+"}


class OMPDirective(metaclass=abc.ABCMeta):
    '''
    Base mixin class for all OpenMP-related directives.

    This class is useful to provide a unique common ancestor to all the
    OpenMP directives, for instance when traversing the tree with
    `node.walk(OMPDirective)`

    Note that classes inheriting from it must place the OMPDirective in
    front of the other Directive node sub-class, so that the Python
    MRO gives preference to this class's attributes.
    '''
    _PREFIX = "OMP"


class OMPRegionDirective(OMPDirective, RegionDirective, metaclass=abc.ABCMeta):
    '''
    Base class for all OpenMP region-related directives.

    '''
    def _get_reductions_list(self, reduction_type):
        '''
        Returns the names of all scalars within this region that require a
        reduction of type 'reduction_type'. Returned names will be unique.

        :param reduction_type: the reduction type (e.g. AccessType.SUM) to \
                               search for.
        :type reduction_type: :py:class:`psyclone.core.access_type.AccessType`

        :returns: names of scalar arguments with reduction access.
        :rtype: list of str

        '''
        result = []
        const = Config.get().api_conf().get_constants()
        for call in self.kernels():
            if not call.arguments:
                continue
            for arg in call.arguments.args:
                if arg.argument_type in const.VALID_SCALAR_NAMES:
                    if arg.descriptor.access == reduction_type:
                        if arg.name not in result:
                            result.append(arg.name)
        return result


class OMPStandaloneDirective(OMPDirective, StandaloneDirective,
                             metaclass=abc.ABCMeta):
    ''' Base class for all OpenMP-related standalone directives. '''


class OMPDeclareTargetDirective(OMPStandaloneDirective):
    '''
    Class representing an OpenMP Declare Target directive in the PSyIR.

    '''
    def gen_code(self, parent):
        '''Generate the fortran OMP Declare Target Directive and any
        associated code.

        :param parent: the parent Node in the Schedule to which to add our \
                       content.
        :type parent: sub-class of :py:class:`psyclone.f2pygen.BaseGen`
        '''
        # Check the constraints are correct
        self.validate_global_constraints()

        # Generate the code for this Directive
        parent.add(DirectiveGen(parent, "omp", "begin", "declare", "target"))

    def begin_string(self):
        '''Returns the beginning statement of this directive, i.e.
        "omp routine". The visitor is responsible for adding the
        correct directive beginning (e.g. "!$").

        :returns: the opening statement of this directive.
        :rtype: str

        '''
        return "omp declare target"

    def validate_global_constraints(self):
        '''
        Perform validation checks that can only be done at code-generation
        time.

        :raises GenerationError: if this directive is not the first statement \
            in a routine.

        '''
        if self.parent and (not isinstance(self.parent, Routine) or
                            self.parent.children[0] is not self):
            raise GenerationError(
                f"A OMPDeclareTargetDirective must be the first child (index "
                f"0) of a Routine but found one as child {self.position} of a "
                f"{type(self.parent).__name__}.")

        super().validate_global_constraints()


class OMPTaskwaitDirective(OMPStandaloneDirective):
    '''
    Class representing an OpenMP TASKWAIT directive in the PSyIR.

    '''
    def validate_global_constraints(self):
        '''
        Perform validation checks that can only be done at code-generation
        time.

        :raises GenerationError: if this OMPTaskwait is not enclosed \
                            within some OpenMP parallel region.

        '''
        # It is only at the point of code generation that we can check for
        # correctness (given that we don't mandate the order that a user
        # can apply transformations to the code). As a Parallel Child
        # directive, we must have an OMPParallelDirective as an ancestor
        # somewhere back up the tree.
        if not self.ancestor(OMPParallelDirective,
                             excluding=OMPParallelDoDirective):
            raise GenerationError(
                "OMPTaskwaitDirective must be inside an OMP parallel region "
                "but could not find an ancestor OMPParallelDirective node")

        super().validate_global_constraints()

    def gen_code(self, parent):
        '''Generate the fortran OMP Taskwait Directive and any associated
        code

        :param parent: the parent Node in the Schedule to which to add our \
                       content.
        :type parent: sub-class of :py:class:`psyclone.f2pygen.BaseGen`
        '''
        # Check the constraints are correct
        self.validate_global_constraints()

        # Generate the code for this Directive
        parent.add(DirectiveGen(parent, "omp", "begin", "taskwait", ""))
        # No children or end code for this node

    def begin_string(self):
        '''Returns the beginning statement of this directive, i.e.
        "omp taskwait". The visitor is responsible for adding the
        correct directive beginning (e.g. "!$").

        :returns: the opening statement of this directive.
        :rtype: str

        '''
        return "omp taskwait"


class OMPSerialDirective(OMPRegionDirective, metaclass=abc.ABCMeta):
    '''
    Abstract class representing OpenMP serial regions, e.g.
    OpenMP SINGLE or OpenMP Master.

    '''

    def _valid_dependence_literals(self, lit1, lit2):
        '''
        Compares two Nodes to check whether they are a valid dependency
        pair. For two Nodes where at least one is a Literal, a valid
        dependency is any pair of Literals.

        :param lit1: the first node to compare.
        :type lit1: :py:class:`psyclone.psyir.nodes.Node`
        :param lit2: the second node to compare.
        :type lit2: :py:class:`psyclone.psyir.nodes.Node`

        :returns: whether or not these two nodes can be used as a valid \
                  dependency pair in OpenMP.
        :rtype: bool

        '''
        # Check both are Literals

        # If a Literal index to dependency has calculated dependency to a
        # non-Literal index, this will return False, as this is not
        # currently supported in PSyclone.

        # If literals are not the same its fine, since a(1) is not a
        # dependency to a(2), so as long as both are Literals this is ok.
        return isinstance(lit1, Literal) and isinstance(lit2, Literal)

    def _valid_dependence_ranges(self, arraymixin1, arraymixin2, index):
        '''
        Compares two ArrayMixin Nodes to check whether they are a valid
        dependency pair on the provided index. For two Nodes where at least
        one has a Range at this index, they must both have Ranges, and both be
        full ranges, i.e. ":".

        :param arraymixin1: the first node to validate.
        :type arraymixin1: :py:class:`psyclone.psyir.nodes.ArrayMixin`
        :param arraymixin2: the second node to validate.
        :type arraymixin2: :py:class:`psyclone.psyir.nodes.ArrayMixin`

        :returns: whether or not these two nodes can be used as a valid \
                  dependency pair in OpenMP, based upon the provided index.
        :rtype: bool
        '''
        # We know both inputs are always ArrayMixin as this is a private
        # function.
        # Check both are Ranges
        if (not isinstance(arraymixin1.indices[index], Range) or not
                isinstance(arraymixin2.indices[index], Range)):
            # Range index to a dependency has calculated dependency to a
            # non-Range index, which is not currently supported in PSyclone
            return False

        # To be valid, both ranges need to be full ranges.

        # If we have a range index between dependencies which does not cover
        # the full array range, it is not currently supported in
        # PSyclone (due to OpenMP limitations), so False will be returned.
        return (arraymixin1.is_full_range(index) and
                arraymixin2.is_full_range(index))

    def _compute_accesses(self, ref, preceding_nodes, task):
        '''
        Computes the set of accesses for a Reference or BinaryOperation
        Node, based upon the preceding_nodes and containing task.

        The returned result is either a set of Literals, or Literals
        and BinaryOperations.

        If ref is a BinaryOperation, it needs to be of the following formats:
        1. Reference ADD/SUB Literal
        2. Literal ADD Reference
        3. Binop(Literal MUL Literal) ADD Reference
        4. Reference ADD/SUB Binop(Literal MUL Literal)


        :param ref: the Reference or BinaryOperation node to compute \
                    accesses for.
        :type ref: Union[:py:class:`psyclone.psyir.nodes.Reference, \
                   :py:class:`psyclone.psyir.nodes.BinaryOperation]
        :param preceding_nodes: a list of nodes that precede the task in the \
                                tree.
        :type preceding_nodes: List[:py:class:`psyclone.psyir.nodes.Node`]
        :param task: the OMPTaskDirective node containing ref as a child.
        :type task: :py:class:`psyclone.psyir.nodes.OMPTaskDirective`

        :raises UnresolvedDependencyError: If the ref contains an unsupported \
                                           BinaryOperation structure, such as \
                                           a non-ADD/SUB/MUL operator. Check \
                                           error message for more details.
        :raises UnresolvedDependencyError: If preceding_nodes contains a Call \
                                           node.
        :raises UnresolvedDependencyError: If ref is a BinaryOperation and \
                                           neither child of ref is a Literal \
                                           or BinaryOperation.
        :raises UnresolvedDependencyError: If there is a dependency between \
                                           ref (a BinaryOperation) and a \
                                           previously set constant.
        :raises UnresolvedDependencyError: If there is a dependency between \
                                           ref and a Loop variable that is \
                                           not an ancestor of task.
        :raises UnresolvedDependencyError: If preceding_nodes contains a \
                                           dependent loop with a non-Literal \
                                           step.

        :returns: a list of the dependency values for the input ref.
        :rtype: List[Union[:py:class:`psyclone.psyir.nodes.Literal`, \
                :py:class:`psyclone.psyir.nodes.BinaryOperation`]]
        '''
        if isinstance(ref, Reference):
            symbol = ref.symbol
        else:
            # Get the symbol out of the Binop, and store some other
            # important information. We store the step value of the
            # ancestor loop (which will be the value of the Literal, or
            # the one of the Literals if an operand is a BinaryOperation).
            # In the case that one of the operands is a BinaryOperation,
            # we also store a "num_entries" value, which is based upon the
            # multiplier of the step value. This is how we can handle
            # cases such as array(i+57) if the step value is 32, the
            # dependencies stored would be array(i+32) and array(i+64).
            if isinstance(ref.children[0], Literal):
                if ref.operator == BinaryOperation.Operator.ADD:
                    # Have Literal + Reference. Store the symbol of the
                    # Reference and the integer value of the Literal.
                    symbol = ref.children[1].symbol
                    binop_val = int(ref.children[0].value)
                    num_entries = 2
                else:
                    raise UnresolvedDependencyError(
                            "Found a dependency index that is "
                            "a BinaryOperation where the "
                            "format is Literal OP Reference "
                            "with a non-ADD operand "
                            "which is not supported.")
            elif isinstance(ref.children[1], Literal):
                # Have Reference OP Literal. Store the symbol of the
                # Reference, and the integer value of the Literal. If the
                # operator is negative, then we store the value negated.
                if ref.operator == BinaryOperation.Operator.ADD:
                    symbol = ref.children[0].symbol
                    binop_val = int(ref.children[1].value)
                    num_entries = 2
                elif ref.operator == BinaryOperation.Operator.SUB:
                    symbol = ref.children[0].symbol
                    binop_val = 0-(int(ref.children[1].value))
                    num_entries = 2
                else:
                    raise UnresolvedDependencyError(
                            "Found a dependency index that is "
                            "a BinaryOperation where the "
                            "Operator is neither ADD not SUB "
                            "which is not supported.")

            elif isinstance(ref.children[0], BinaryOperation):
                if ref.operator == BinaryOperation.Operator.ADD:
                    # Have Binop ADD Reference. Store the symbol of the
                    # Reference, and store the binop. The binop is of
                    # structure Literal MUL Literal, where the second
                    # Literal is to the step of a parent loop.
                    symbol = ref.children[1].symbol
                    binop = ref.children[0]
                    if binop.operator != BinaryOperation.Operator.MUL:
                        raise UnresolvedDependencyError(
                                "Found a dependency index that is a "
                                "BinaryOperation with a child "
                                "BinaryOperation with a non-MUL operand "
                                "which is not supported.")
                    # These binary operations are format of Literal MUL Literal
                    # where step_val is the 2nd literal and the multiplier
                    # is the first literal
                    if (not (isinstance(binop.children[0], Literal) and
                             isinstance(binop.children[1], Literal))):
                        raise UnresolvedDependencyError(
                                "Found a dependency index that is a "
                                "BinaryOperation with a child "
                                "BinaryOperation with a non-Literal child "
                                "which is not supported.")
                    # We store the step of the parent loop in binop_val, and
                    # use the other operand to compute how many entries we
                    # need to compute to validate this dependency list.
                    binop_val = int(binop.children[1].value)
                    num_entries = int(binop.children[0].value)+1
                else:
                    raise UnresolvedDependencyError(
                            "Found a dependency index that is "
                            "a BinaryOperation where the "
                            "format is BinaryOperator OP "
                            "Reference with a non-ADD operand "
                            "which is not supported.")
            elif isinstance(ref.children[1], BinaryOperation):
                # Have Reference ADD/SUB Binop. Store the symbol of the
                # Reference, and store the binop. The binop is of
                # structure Literal MUL Literal, where the second
                # Literal is to the step of a parent loop.
                if ref.operator == BinaryOperation.Operator.ADD:
                    symbol = ref.children[0].symbol
                    binop = ref.children[1]
                    if binop.operator != BinaryOperation.Operator.MUL:
                        raise UnresolvedDependencyError(
                                "Found a dependency index that is a "
                                "BinaryOperation with a child "
                                "BinaryOperation with a non-MUL operand "
                                "which is not supported.")
                    # These binary operations are format of Literal MUL Literal
                    # where step_val is the 2nd literal.
                    if (not (isinstance(binop.children[0], Literal) and
                             isinstance(binop.children[1], Literal))):
                        raise UnresolvedDependencyError(
                                "Found a dependency index that is a "
                                "BinaryOperation with an operand "
                                "BinaryOperation with a non-Literal operand "
                                "which is not supported.")
                    # We store the step of the parent loop in binop_val, and
                    # use the other operand to compute how many entries we
                    # need to compute to validate this dependency list.
                    binop_val = int(binop.children[1].value)
                    num_entries = int(binop.children[0].value)+1
                elif ref.operator == BinaryOperation.Operator.SUB:
                    symbol = ref.children[0].symbol
                    binop = ref.children[1]
                    if binop.operator != BinaryOperation.Operator.MUL:
                        raise UnresolvedDependencyError(
                                "Found a dependency index that is a "
                                "BinaryOperation with a child "
                                "BinaryOperation with a non-MUL operator "
                                "which is not supported.")
                    # These binary operations are format of Literal MUL Literal
                    # where step_val is the 2nd literal.
                    if (not (isinstance(binop.children[0], Literal) and
                             isinstance(binop.children[1], Literal))):
                        raise UnresolvedDependencyError(
                                "Found a dependency index that is a "
                                "BinaryOperation with an operand "
                                "BinaryOperation with a non-Literal operand "
                                "which is not supported.")
                    # We store the step of the parent loop in binop_val, but
                    # negated as the operator is SUB, and
                    # use the other operand to compute how many entries we
                    # need to compute to validate this dependency list.
                    binop_val = -int(binop.children[1].value)
                    num_entries = int(binop.children[0].value)
                else:
                    raise UnresolvedDependencyError(
                            "Found a dependency index that is "
                            "a BinaryOperation where the "
                            "format is Reference OP "
                            "BinaryOperation with a non-ADD, "
                            "non-SUB operand "
                            "which is not supported.")
            else:
                raise UnresolvedDependencyError(
                        "Found a dependency index that is a "
                        "BinaryOperation where neither child "
                        "is a Literal or BinaryOperation. "
                        "PSyclone can't validate "
                        "this dependency.")
        start = None
        stop = None
        step = None
        for node in preceding_nodes:
            # Only Assignment, Loop or Call nodes can modify the symbol in our
            # Reference
            if not isinstance(node, (Assignment, Loop, Call)):
                continue
            if isinstance(node, Call):
                # Currently opting to fail on any Call.
                # Potentially it might be possible to check if the Symbol is
                # written to and only if so then raise an error
                raise UnresolvedDependencyError(
                        "Found a Call in preceding_nodes, which "
                        "is not yet supported.")
            if isinstance(node, Assignment) and node.lhs.symbol == symbol:
                start = node.rhs.copy()
                break
            if isinstance(node, Loop) and node.variable == symbol:
                # If the loop is not an ancestor of the task then
                # we don't currently support it.
                ancestor_loop = task.ancestor(Loop, limit=self)
                is_ancestor = False
                while ancestor_loop is not None:
                    if ancestor_loop == node:
                        is_ancestor = True
                        break
                    ancestor_loop = ancestor_loop.ancestor(Loop, limit=self)
                if not is_ancestor:
                    raise UnresolvedDependencyError(
                            "Found a dependency index that "
                            "was updated as a Loop variable "
                            "that is not an ancestor Loop of "
                            "the task.")
                # It has to be an ancestor loop, so we want to find the start,
                # stop and step Nodes
                start, stop, step = node.start_expr, node.stop_expr, \
                    node.step_expr
                break

        if isinstance(ref, BinaryOperation):
            output_list = []
            if step is None:
                # Found no ancestor loop, PSyclone cannot handle
                # this case, as BinaryOperations created by OMPTaskDirective
                # in dependencies will always be based on ancestor loops.
                raise UnresolvedDependencyError(
                        "Found a dependency between a "
                        "BinaryOperation and a previously "
                        "set constant value. "
                        "PSyclone cannot yet handle this "
                        "interaction.")
            # If the step isn't a Literal value, then we can't compute what
            # the address accesses at compile time, so we can't validate the
            # dependency.
            if not isinstance(step, Literal):
                raise UnresolvedDependencyError(
                        "Found a dependency index that is a "
                        "Loop variable with a non-Literal step "
                        "which we can't resolve in PSyclone.")
            # If the start and stop are both Literals, we can compute a set
            # of accesses this BinaryOperation is related to precisely.
            if (isinstance(start, Literal) and isinstance(stop, Literal)):
                # Fill the output list with all values from start to stop
                # incremented by step
                startval = int(start.value)
                stopval = int(stop.value)
                stepval = int(step.value)
                # We loop from startval to stopval + 1 as PSyIR loops will
                # include stopval, wheras Python loops do not.
                for i in range(startval, stopval + 1, stepval):
                    new_x = i + binop_val
                    output_list.append(Literal(f"{new_x}", INTEGER_TYPE))
                return output_list

            # If they are not all literals, then we create a small number of
            # entries, based on the num_entries value computed previously.
            # The list will contain num_entries+1 entries, as the first entry
            # is a starting value, and then we create num_entries value
            # based on the step value.
            # Each entry in the list is a BinaryOperation, the first value is
            # the original (start + binop_val), whilst the following elements
            # are incremented/decremented by the step value accordingly.
            first_val = BinaryOperation.create(
                    BinaryOperation.Operator.ADD,
                    start.copy(),
                    Literal(f"{binop_val}", INTEGER_TYPE))
            output_list.append(first_val)
            for i in range(1, num_entries+1):
                if binop_val >= 0:
                    val = binop_val + i * int(step.value)
                else:
                    val = binop_val - i * int(step.value)
                output_list.append(
                        BinaryOperation.create(
                            BinaryOperation.Operator.ADD,
                            start.copy(),
                            Literal(f"{val}", INTEGER_TYPE)))
            return output_list
        if step is None:
            # Result for an assignment.
            output_list = [start]
            return output_list
        output_list = []
        # If step is not a Literal then we probably can't resolve this
        if not isinstance(step, Literal):
            raise UnresolvedDependencyError(
                    "Found a dependency index that is a "
                    "Loop variable with a non-Literal step "
                    "which we can't resolve in PSyclone.")
        # Special case when all are Literals
        if (isinstance(start, Literal) and isinstance(stop, Literal)):
            # Fill the output list with all values from start to stop
            # incremented by step
            startval = int(start.value)
            stopval = int(stop.value)
            stepval = int(step.value)
            # We loop from startval to stopval + 1 as PSyIR loops will include
            # stopval, wheras Python loops do not.
            for i in range(startval, stopval + 1, stepval):
                output_list.append(Literal(f"{i}", INTEGER_TYPE))
            return output_list

        # If ref is a reference then we generate the first two values of
        # the sequence only. In this case, we have a non-parent loop reference
        # which is also firstprivate (as shared indices are forbidden in
        # OMPTaskDirective already), so is essentially a constant. In this
        # case therefore we will have an unknown "stop" value, so we just
        # verify values close to the step
        output_list.append(start.copy())
        second_val = BinaryOperation.create(
                BinaryOperation.Operator.ADD,
                start.copy(),
                step.copy())
        output_list.append(second_val)
        return output_list

    def _valid_dependence_ref_binop(self, ref1, ref2, task1, task2):
        '''
        Compares two Reference/BinaryOperation Nodes to check they are a set
        of dependencies that are valid according to OpenMP. Both these nodes
        are array indices on the same array symbol, so for OpenMP to correctly
        compute this dependency, we must guarantee at compile time that we
        know the addresses/array sections covered by this index are identical.

        :param ref1: the first Node to compare.
        :type ref1: Union[:py:class:`psyclone.psyir.nodes.Reference`, \
                    :py:class:`psyclone.psyir.nodes.BinaryOperation`]
        :param ref2: the second Node to compare.
        :type ref2: Union[:py:class:`psyclone.psyir.nodes.Reference`, \
                    :py:class:`psyclone.psyir.nodes.BinaryOperation`]
        :param task1: the task containing ref1 as a child.
        :type task1: :py:class:`psyclone.psyir.nodes.OMPTaskDirective`
        :param task2: the task containing ref2 as a child.
        :type task2: :py:class:`psyclone.psyir.nodes.OMPTaskDirective`

        :raises GenerationError: If ref1 and ref2 are dependencies on the \
                                 same array, and one does not contain a \
                                 Reference but the other does.
        :raises GenerationError: If ref1 and ref2 are dependencies on the \
                                 same array but are References to different \
                                 variables.
        :raises GenerationError: If ref1 and ref2 are dependencies on the \
                                 same array, but the computed index values \
                                 are not dependent according to OpenMP.

        :returns: whether or not these two nodes can be used as a valid \
                  dependency on the same array in OpenMP.
        :rtype: bool

        '''
        # In this case we have two Reference/BinaryOperation as indices.
        # We need to attempt to find their value set and check the value
        # set matches.
        # Find all the nodes before these tasks
        preceding_t1 = task1.preceding(reverse=True)
        preceding_t2 = task2.preceding(reverse=True)
        # Get access list for each ref
        try:
            ref1_accesses = self._compute_accesses(ref1, preceding_t1, task1)
            ref2_accesses = self._compute_accesses(ref2, preceding_t2, task2)
        except UnresolvedDependencyError:
            # If we get a UnresolvedDependencyError from compute_accesses, then
            # we found an access that isn't able to be handled by PSyclone, so
            # dependencies based on it need to be handled by a taskwait
            return False

        # If the first access in each accesses contains a Reference
        # we should check that both are to the same symbol
        ref1_ref = ref1_accesses[0].walk(Reference)
        ref2_ref = ref2_accesses[0].walk(Reference)
        if ((len(ref1_ref) > 0 and len(ref2_ref) == 0) or
                (len(ref1_ref) == 0 and len(ref2_ref) > 0)):
            # Found a pair of dependencies on the same array which are not
            # valid under OpenMP, as one contains a Reference while the
            # other does not.
            return False
        # If we have any References in the accesses lists, check the first
        # Reference is the same in both.
        if len(ref1_ref) > 0 and ref1_ref[0] != ref2_ref[0]:
            # Found a pair of dependencies on the same array which are not
            # supported in PSyclone, as the index accesses are both References
            # but to different variables so we don't know if they are
            # equivalent at compile time.
            return False

        # pylint: disable=import-outside-toplevel
        from psyclone.psyir.backend.sympy_writer import SymPyWriter
        # Now we know if there is a Reference, both are to the same Symbol
        if len(ref1_ref) > 0:
            # Handle reference case
            values = []
            # r1_min will contain the minimum computed value for (ref + value)
            # from the list. r1_max will contain the maximum computed value.
            r1_min = sys.maxsize
            r1_max = 0
            for member in ref1_accesses:
                # If its a reference (not a BinaryOperation) we need to treat
                # it as a special case, as this means that we essentially have
                # a `Reference + 0` case.
                if isinstance(member, Reference):
                    # If we find the Reference then we need to set the min
                    # to 0 as we have Reference + 0
                    r1_min = 0
                    continue
                # We know its a BinaryOperation of the Reference and something.
                # Take the second child of the BinaryOperation to compute
                values.append(member.children[1])
            # We have a set of Literal values, we use the SymPyWriter to
            # convert these objects to expressions we can use to obtain
            # integer values for these Literals
            sympy_ref1s = SymPyWriter.convert_to_sympy_expressions(values)

            values = []
            # Loop through the values, and compute the maximum and minumum
            # values in that list. These correspond to the maximum and
            # minimum values used for accessing the array relative to the
            # symbol used as a base access.
            for member in sympy_ref1s:
                val = int(member)
                values.append(val)
                if val < r1_min:
                    r1_min = val
                if val > r1_max:
                    r1_max = val

            # val2s stores all the values added to the base reference in
            # the BinaryOperations in ref2_accesses, so if ref2_accesses
            # contains ref+32, ref+64, then ref2_accesses will contain the
            # Literals for 32 and 64.
            val2s = []
            for member in ref2_accesses:
                # If its a reference only we can ignore it as we know both
                # feature the Reference.
                if isinstance(member, Reference):
                    continue
                # We know its a BinaryOperation of the Reference and something.
                # Take the second child of the BinaryOperation to compute
                val2s.append(member.children[1])
            sympy_ref2s = SymPyWriter.convert_to_sympy_expressions(val2s)
            for member in sympy_ref2s:
                # If the value is between min and max of r1 then we check that
                # the value is in the values list
                val = int(member)
                if val >= r1_min and val <= r1_max:
                    if val not in values:
                        # Found incompatible dependency between two
                        # array accesses, ref1 is in range {r1_min}
                        # to {r1_max}, but doesn't contain {val}.
                        # This can happen if we have two loops with
                        # different start values or steps.
                        return False
        else:
            # Handle no Reference case
            # Find the min and max values of the first reference accesses
            r1_min = sys.maxsize
            r1_max = 0
            # We have a set of Literal values, we use the SymPyWriter to
            # convert these objects to expressions we can use to obtain
            # integer values for these Literals
            sympy_ref1s = \
                SymPyWriter.convert_to_sympy_expressions(ref1_accesses)
            values = []
            for member in sympy_ref1s:
                # Since we were all literals, the SymPyWriter converts this
                # to a single value.
                val = int(member)
                values.append(val)
                if val < r1_min:
                    r1_min = val
                if val > r1_max:
                    r1_max = val

            # We have a set of Literal values, we use the SymPyWriter to
            # convert these objects to expressions we can use to obtain
            # integer values for these Literals
            sympy_ref2s = \
                SymPyWriter.convert_to_sympy_expressions(ref2_accesses)
            for member in sympy_ref2s:
                # If the value is between min and max of r1 then we check that
                # the value is in the values list
                val = int(member)
                if val >= r1_min and val <= r1_max:
                    if val not in values:
                        # Found incompatible dependency between two
                        # array accesses, ref1 is in range {r1_min}
                        # to {r1_max}, but doesn't contain {val}.
                        # This can happen if we have two loops with
                        # different start values or steps.
                        return False

        return True

    def _check_dependency_pairing_valid(self, node1, node2, task1, task2):
        '''
        Given a pair of nodes which are children of a OMPDependClause, this
        function checks whether the described dependence is correctly
        described by the OpenMP standard.
        If the dependence is not going to be handled safely, this function
        returns False, else it returns true.

        :param node1: the first input node to check.
        :type node1: :py:class:`psyclone.psyir.nodes.Node`
        :param node2: the second input node to check.
        :type node2: :py:class:`psyclone.psyir.nodes.Node`
        :param task1: the OMPTaskDirective node containing node1 as a \
                      dependency
        :type task1: :py:class:`psyclone.psyir.nodes.OMPTaskDirective`
        :param task2: the OMPTaskDirective node containing node2 as a \
                      dependency
        :type task2: :py:class:`psyclone.psyir.nodes.OMPTaskDirective`

        :returns: whether the dependence is going to be handled safely \
                  according to the OpenMP standard.
        :rtype: bool
        '''
        # Checking the symbol is the same works. If the symbol is not the same
        # then there's no dependence, so its valid.
        if node1.symbol != node2.symbol:
            return True
        # For structure reference we need to check they access
        # the same member. If they don't, no dependence so valid.
        if isinstance(node1, StructureReference):
            # If either is a StructureReference here they must both be,
            # as they access the same symbol.
            member = node1
            member1 = node2

            # We can't just do == on the Member child, as that
            # will recurse and check the array indices for any
            # ArrayMixin children

            # Check the signature of both StructureReference
            # to see if they are accessing the same data
            ref0_sig = node1.get_signature_and_indices()[0]
            ref1_sig = node2.get_signature_and_indices()[0]
            if ref0_sig != ref1_sig:
                return True

        # If we have (exactly) Reference objects we filter out
        # non-matching ones with the symbol check, and matching ones
        # are always valid since they are simple accesses.
        if type(node1) is Reference:
            return True

        # All remaining objects are some sort of Array access
        array1 = None
        array2 = None
        if isinstance(node1, ArrayReference):
            array1 = node1
            array2 = node2
        else:
            array1 = node1.walk(ArrayMixin)[0]
            array2 = node2.walk(ArrayMixin)[0]
        valid = True
        for i, index in enumerate(array1.indices):
            if (isinstance(index, Literal) or
                    isinstance(array2.indices[i], Literal)):
                valid = valid and self._valid_dependence_literals(
                            index, array2.indices[i])
            elif (isinstance(index, Range) or
                  isinstance(array2.indices[i], Range)):
                valid = valid and self._valid_dependence_ranges(
                            array1, array2, i)
            else:
                # The only remaining option is that the indices are
                # References or BinaryOperations
                valid = valid and self._valid_dependence_ref_binop(
                            index, array2.indices[i], task1, task2)

        return valid

    def _validate_task_dependencies(self):
        '''
        Validates all task dependencies in this OMPSerialDirective region are
        valid within the restraints of OpenMP & PSyclone. This is done through
        a variety of helper functions, and checks each pair of tasks' inout,
        outin and outout combinations.

        Any task dependencies that are detected and will not be handled by
        OpenMP's depend clause will be handled through the addition of
        OMPTaskwaitDirective nodes.

        :raises NotImplementedError: If this region contains both an \
                                 OMPTaskDirective and an OMPTaskloopDirective.
        '''
        # pylint: disable=import-outside-toplevel
        from psyclone.psyir.nodes.omp_task_directive import OMPTaskDirective
        tasks = self.walk(OMPTaskDirective)
        # For now we disallow Tasks and Taskloop directives in the same Serial
        # Region
        if len(tasks) > 0 and len(self.walk(OMPTaskloopDirective)) > 0:
            raise NotImplementedError("OMPTaskDirectives and "
                                      "OMPTaskloopDirectives are not "
                                      "currently supported inside the same "
                                      "parent serial region.")

        pairs = itertools.combinations(tasks, 2)

        # List of tuples of dependent nodes that aren't handled by OpenMP
        unhandled_dependent_nodes = []
        # Lowest and highest position nodes contain the abs_position of each
        # tuple inside unhandled_dependent_nodes, used for sorting the arrays
        # and checking if the unhandled dependency has a taskwait inbetween.
        lowest_position_nodes = []
        highest_position_nodes = []

        for pair in pairs:
            task1 = pair[0]
            task2 = pair[1]

            # Keep track of whether the dependencies are satisfiable
            satisfiable = True
            # Find all References in each tasks' depend clauses
            # Should we cache these instead?
            task1_in = [x for x in task1.input_depend_clause.children
                        if isinstance(x, Reference)]
            task1_out = [x for x in task1.output_depend_clause.children
                         if isinstance(x, Reference)]
            task2_in = [x for x in task2.input_depend_clause.children
                        if isinstance(x, Reference)]
            task2_out = [x for x in task2.output_depend_clause.children
                         if isinstance(x, Reference)]

            inout = list(itertools.product(task1_in, task2_out))
            outin = list(itertools.product(task1_out, task2_in))
            outout = list(itertools.product(task1_out, task2_out))
            # Loop through each potential dependency pair and check they
            # will be handled correctly.
            for mem in inout + outin + outout:
                # As soon as any is not satisfiable, then we don't need to
                # continue checking.
                if not satisfiable:
                    break
                satisfiable = satisfiable and \
                    self._check_dependency_pairing_valid(mem[0], mem[1],
                                                         task1, task2)

            # If we have an unsatisfiable dependency between two tasks, then we
            # need to have a taskwait between them always. We need to loop up
            # to find these tasks' parents which are closest to the Schedule
            # which contains both tasks, and use them as the nodes which are
            # dependent.
            if not satisfiable:
                # Find the lowest schedule containing both nodes.
                schedule1 = task1.ancestor(Schedule)
                schedule2 = task2.ancestor(Schedule)
                # While they're not distinct
                while schedule1 is not schedule2:
                    if schedule1.depth > schedule2.depth:
                        schedule1 = schedule1.ancestor(Schedule)
                    else:
                        schedule2 = schedule2.ancestor(Schedule)
                task1_proxy = task1
                while task1_proxy.parent is not schedule1:
                    task1_proxy = task1_proxy.parent
                task2_proxy = task2
                while task2_proxy.parent is not schedule1:
                    task2_proxy = task2_proxy.parent

                # Now we have the closest nodes to the closest common ancestor
                # schedule, so add them to the unhandled_dependent_nodes list.
                if task1_proxy is not task2_proxy:
                    # If they end up with the same proxy, they have the same
                    # ancestor tree but are in different schedules. This means
                    # that they are in something like an if/else block with
                    # one node in an if block and the other in the else block.
                    # These dependencies we can ignore as they are not ever
                    # both executed
                    unhandled_dependent_nodes.append(
                            (task1_proxy, task2_proxy))
                    lowest_position_nodes.append(min(task1_proxy.abs_position,
                                                     task2_proxy.abs_position))
                    highest_position_nodes.append(
                            max(task1_proxy.abs_position,
                                task2_proxy.abs_position))

        # Need to sort lists by highest_position_nodes value, and then
        # by lowest value if tied.
        # Based upon
        # https://stackoverflow.com/questions/9764298/how-to-sort-two-
        # lists-which-reference-each-other-in-the-exact-same-way
        if len(unhandled_dependent_nodes) > 0:
            # sorted_highest_positions and sorted_lowest_positions contain
            # the abs_positions for the corresponding Nodes in the tuple at
            # the same index in sorted_dependency_pairs. The
            # sorted_dependency_pairs list contains each pair of unhandled
            # dependency nodes that were previously computed, but sorted
            # according to abs_position in the tree.
            sorted_highest_positions, sorted_lowest_positions, \
                    sorted_dependency_pairs = (list(t) for t in
                                               zip(*sorted(zip(
                                                   highest_position_nodes,
                                                   lowest_position_nodes,
                                                   unhandled_dependent_nodes)
                                                   )))
        else:
            # We have no invalid dependencies so we can return early
            return

        # The location of any node where need to place an OMPTaskwaitDirective
        # to ensure code correctness. The size of this list should be
        # minimised during construction as we will not add another
        # OMPTaskwaitDirective when a dependency will be handled already by
        # an existing OMPTaskwaitDirective or one that will be created during
        # this process.
        taskwait_location_nodes = []
        # Stores the abs_position for each of the OMPTaskwaitDirective nodes
        # that does or will exist.
        taskwait_location_abs_pos = []
        for taskwait in self.walk(OMPTaskwaitDirective):
            taskwait_location_nodes.append(taskwait)
            taskwait_location_abs_pos.append(taskwait.abs_position)
        # Add the first node to have a taskwait placed in front of it into the
        # list
        taskwait_location_nodes.append(sorted_dependency_pairs[0][1])
        taskwait_location_abs_pos.append(sorted_highest_positions[0])
        for index, pairs in enumerate(sorted_dependency_pairs[1:]):
            # Add 1 to index here because we're looking from [1:]
            lo_abs_pos = sorted_lowest_positions[index+1]
            hi_abs_pos = sorted_highest_positions[index+1]
            for ind, taskwait_loc in enumerate(taskwait_location_nodes):
                if (taskwait_location_abs_pos[ind] <= hi_abs_pos and
                        taskwait_location_abs_pos[ind] >= lo_abs_pos):
                    # We have a taskwait meant to be placed here that is
                    # potentially already satisfied. To check we need to
                    # ensure that the ancestor schedules of the nodes
                    # are identical
                    if (pairs[0].ancestor(Schedule) is
                            taskwait_loc.ancestor(Schedule)):
                        break
            else:
                # If we didn't find a taskwait we plan to add that satisfies
                # this dependency, add it to the list
                taskwait_location_nodes.append(pairs[1])
                taskwait_location_abs_pos.append(hi_abs_pos)
        # Now loop through the list in reverse and add taskwaits
        taskwait_location_nodes.reverse()
        for taskwait_loc in taskwait_location_nodes:
            node_parent = taskwait_loc.parent
            loc = taskwait_loc.position
            node_parent.addchild(OMPTaskwaitDirective(), loc)

    def lower_to_language_level(self):
        '''
        Checks that any task dependencies inside this node are valid.
        '''
        # Perform parent ops
        super().lower_to_language_level()

        # Validate any task dependencies in this OMPSerialRegion.
        self._validate_task_dependencies()

    def validate_global_constraints(self):
        '''
        Perform validation checks that can only be done at code-generation
        time.

        :raises GenerationError: if this OMPSerial is not enclosed \
                                 within some OpenMP parallel region.
        :raises GenerationError: if this OMPSerial is enclosed within \
                                 any OMPSerialDirective subclass region.

        '''
        # It is only at the point of code generation that we can check for
        # correctness (given that we don't mandate the order that a user
        # can apply transformations to the code). As a Parallel Child
        # directive, we must have an OMPParallelDirective as an ancestor
        # somewhere back up the tree.
        # Also check the single region is not enclosed within another OpenMP
        # single region.
        # It could in principle be allowed for that parent to be a ParallelDo
        # directive, however I can't think of a use case that would be done
        # best in a parallel code by that pattern
        if not self.ancestor(OMPParallelDirective,
                             excluding=OMPParallelDoDirective):
            raise GenerationError(
                f"{self._text_name} must be inside an OMP parallel region but "
                f"could not find an ancestor OMPParallelDirective node")

        if self.ancestor(OMPSerialDirective):
            raise GenerationError(
                f"{self._text_name} must not be inside another OpenMP "
                f"serial region")

        super().validate_global_constraints()


class OMPSingleDirective(OMPSerialDirective):
    '''
    Class representing an OpenMP SINGLE directive in the PSyIR.

    :param bool nowait: argument describing whether this single should have \
        a nowait clause applied. Default value is False.
    :param kwargs: additional keyword arguments provided to the PSyIR node.
    :type kwargs: unwrapped dict.

    '''
    _children_valid_format = "Schedule, [OMPNowaitClause]"
    # Textual description of the node
    _text_name = "OMPSingleDirective"

    def __init__(self, nowait=False, **kwargs):

        self._nowait = nowait
        # Call the init method of the base class once we've stored
        # the nowait requirement
        super().__init__(**kwargs)
        if self._nowait:
            self.children.append(OMPNowaitClause())

    @staticmethod
    def _validate_child(position, child):
        '''
         Decides whether a given child and position are valid for this node.
         The rules are:
         1. Child 0 must always be a Schedule.
         2. Child 1 can only be a OMPNowaitClause.

        :param int position: the position to be validated.
        :param child: a child to be validated.
        :type child: :py:class:`psyclone.psyir.nodes.Node`

        :return: whether the given child and position are valid for this node.
        :rtype: bool

        '''
        if position == 0:
            return isinstance(child, Schedule)
        if position == 1:
            return isinstance(child, OMPNowaitClause)
        return False

    @property
    def nowait(self):
        '''
        :returns: whether the nowait clause is specified for this directive.
        :rtype: bool

        '''
        return self._nowait

    def gen_code(self, parent):
        '''Generate the fortran OMP Single Directive and any associated
        code

        :param parent: the parent Node in the Schedule to which to add our \
                       content.
        :type parent: sub-class of :py:class:`psyclone.f2pygen.BaseGen`
        '''
        # Check the constraints are correct
        self.validate_global_constraints()

        # Capture the nowait section of the string if required
        nowait_string = ""
        if self._nowait:
            nowait_string = "nowait"

        parent.add(DirectiveGen(parent, "omp", "begin", "single",
                                nowait_string))

        # Generate the code for all of this node's children
        for child in self.dir_body:
            child.gen_code(parent)

        # Generate the end code for this node
        parent.add(DirectiveGen(parent, "omp", "end", "single", ""))

    def begin_string(self):
        '''Returns the beginning statement of this directive, i.e.
        "omp single". The visitor is responsible for adding the
        correct directive beginning (e.g. "!$").

        :returns: the opening statement of this directive.
        :rtype: str

        '''
        return "omp single"

    def end_string(self):
        '''Returns the end (or closing) statement of this directive, i.e.
        "omp end single". The visitor is responsible for adding the
        correct directive beginning (e.g. "!$").

        :returns: the end statement for this directive.
        :rtype: str

        '''
        return "omp end single"


class OMPMasterDirective(OMPSerialDirective):
    '''
    Class representing an OpenMP MASTER directive in the PSyclone AST.

    '''

    # Textual description of the node
    _text_name = "OMPMasterDirective"

    def gen_code(self, parent):
        '''Generate the Fortran OMP Master Directive and any associated
        code

        :param parent: the parent Node in the Schedule to which to add our \
                       content.
        :type parent: sub-class of :py:class:`psyclone.f2pygen.BaseGen`
        '''

        # Check the constraints are correct
        self.validate_global_constraints()

        parent.add(DirectiveGen(parent, "omp", "begin", "master", ""))

        # Generate the code for all of this node's children
        for child in self.children:
            child.gen_code(parent)

        # Generate the end code for this node
        parent.add(DirectiveGen(parent, "omp", "end", "master", ""))

    def begin_string(self):
        '''Returns the beginning statement of this directive, i.e.
        "omp master". The visitor is responsible for adding the
        correct directive beginning (e.g. "!$").

        :returns: the opening statement of this directive.
        :rtype: str

        '''
        return "omp master"

    def end_string(self):
        '''Returns the end (or closing) statement of this directive, i.e.
        "omp end master". The visitor is responsible for adding the
        correct directive beginning (e.g. "!$").

        :returns: the end statement for this directive.
        :rtype: str

        '''
        return "omp end master"


class OMPParallelDirective(OMPRegionDirective):
    ''' Class representing an OpenMP Parallel directive.
    '''

    _children_valid_format = ("Schedule, OMPDefaultClause, OMPPrivateClause,"
                              " [OMPReductionClause]*")

    @staticmethod
    def create(children=None):
        '''
        Create an OMPParallelDirective.

        :param children: The child nodes of the new directive.
        :type children: List of :py:class:`psyclone.psyir.nodes.Node`

        :returns: A new OMPParallelDirective.
        :rtype: :py:class:`psyclone.psyir.nodes.OMPParallelDirective`
        '''

        instance = OMPParallelDirective(children=children)

        # An OMPParallelDirective must have 3 children.
        # Child 0 is a Schedule, create in the constructor.
        # The create function adds the other two children, and OMPDefaultClause
        # and an OMPPrivateClause.
        instance.addchild(OMPDefaultClause(clause_type=OMPDefaultClause.
                                           DefaultClauseTypes.SHARED))
        instance.addchild(OMPPrivateClause())

        return instance

    @staticmethod
    def _validate_child(position, child):
        '''
        :param int position: the position to be validated.
        :param child: a child to be validated.
        :type child: :py:class:`psyclone.psyir.nodes.Node`

        :return: whether the given child and position are valid for this node.
        :rtype: bool

        '''
        if position == 0 and isinstance(child, Schedule):
            return True
        if position == 1 and isinstance(child, OMPDefaultClause):
            return True
        if position == 2 and isinstance(child, OMPPrivateClause):
            return True
        if position >= 3 and isinstance(child, OMPReductionClause):
            return True
        return False

    @property
    def default_clause(self):
        '''
        :returns: The OMPDefaultClause associated with this Directive.
        :rtype: :py:class:`psyclone.psyir.nodes.OMPDefaultClause`
        '''
        return self.children[1]

    @property
    def private_clause(self):
        '''
        :returns: The current OMPPrivateClause associated with this Directive.
        :rtype: :py:class:`psyclone.psyir.nodes.OMPPrivateClause`
        '''
        return self.children[2]

    def gen_code(self, parent):
        '''Generate the fortran OMP Parallel Directive and any associated
        code'''
        # pylint: disable=import-outside-toplevel
        from psyclone.psyGen import zero_reduction_variables

        # We're not doing nested parallelism so make sure that this
        # omp parallel region is not already within some parallel region
        self.validate_global_constraints()

        # Check that this OpenMP PARALLEL directive encloses other
        # OpenMP directives. Although it is valid OpenMP if it doesn't,
        # this almost certainly indicates a user error.
        self._encloses_omp_directive()

        # Update/generate the private clause if the code has changed.
        private_clause = self._get_private_clause()
        if private_clause != self.private_clause:
            self._children[2] = private_clause

        reprod_red_call_list = self.reductions(reprod=True)
        if reprod_red_call_list:
            # we will use a private thread index variable
            thread_idx = self.scope.symbol_table.\
                lookup_with_tag("omp_thread_index")
            private_clause.addchild(Reference(thread_idx))
            thread_idx = thread_idx.name
            # declare the variable
            parent.add(DeclGen(parent, datatype="integer",
                               entity_decls=[thread_idx]))

        calls = self.reductions()

        # first check whether we have more than one reduction with the same
        # name in this Schedule. If so, raise an error as this is not
        # supported for a parallel region.
        names = []
        for call in calls:
            name = call.reduction_arg.name
            if name in names:
                raise GenerationError(
                    f"Reduction variables can only be used once in an invoke. "
                    f"'{name}' is used multiple times, please use a different "
                    f"reduction variable")
            names.append(name)

        zero_reduction_variables(calls, parent)

        default_str = self.default_clause._clause_string
        private_list = []
        for child in self.private_clause.children:
            private_list.append(child.symbol.name)
        private_str = "private(" + ",".join(private_list) + ")"
        parent.add(DirectiveGen(parent, "omp", "begin", "parallel",
                                f"{default_str}, {private_str}"))

        if reprod_red_call_list:
            # add in a local thread index
            parent.add(UseGen(parent, name="omp_lib", only=True,
                              funcnames=["omp_get_thread_num"]))
            parent.add(AssignGen(parent, lhs=thread_idx,
                                 rhs="omp_get_thread_num()+1"))

        first_type = type(self.dir_body[0])
        for child in self.dir_body.children:
            if first_type != type(child):
                raise NotImplementedError("Cannot correctly generate code"
                                          " for an OpenMP parallel region"
                                          " containing children of "
                                          "different types")
            child.gen_code(parent)

        parent.add(DirectiveGen(parent, "omp", "end", "parallel", ""))

        if reprod_red_call_list:
            parent.add(CommentGen(parent, ""))
            parent.add(CommentGen(parent, " sum the partial results "
                                  "sequentially"))
            parent.add(CommentGen(parent, ""))
            for call in reprod_red_call_list:
                call.reduction_sum_loop(parent)

        self.gen_post_region_code(parent)

    def lower_to_language_level(self):
        '''
        In-place construction of clauses as PSyIR constructs.
        At the higher level these clauses rely on dynamic variable dependence
        logic to decide what is private and what is shared, so we use this
        lowering step to find out which References are private, and place them
        explicitly in the lower-level tree to be processed by the backend
        visitor.
        '''
        private_clause = self._get_private_clause()
        if private_clause != self.private_clause:
            self._children[2] = private_clause

        super().lower_to_language_level()

    def begin_string(self):
        '''Returns the beginning statement of this directive, i.e.
        "omp parallel". The visitor is responsible for adding the
        correct directive beginning (e.g. "!$").

        :returns: the opening statement of this directive.
        :rtype: str

        '''
        result = "omp parallel"
        # TODO #514: not yet working with NEMO, so commented out for now
        # if not self._reprod:
        #     result += self._reduction_string()

        return result

    def end_string(self):
        '''Returns the end (or closing) statement of this directive, i.e.
        "omp end parallel". The visitor is responsible for adding the
        correct directive beginning (e.g. "!$").

        :returns: the end statement for this directive.
        :rtype: str

        '''
        return "omp end parallel"

    def _get_private_clause(self):
        '''
        Returns the variable names used for any loops within a directive
        and any variables that have been declared private by a Kernel
        within the directive.

        :returns: a private clause containing the variables that need to be \
                  private for this directive.
        :rtype: :py:class:`psyclone.psyir.nodes.omp_clauses.OMPPrivateClause`

        :raises GenerationError: if the DefaultClauseType associated with \
                                 this OMPParallelDirective is not shared.
        :raises InternalError: if a Kernel has local variable(s) but they \
                               aren't named.
        '''
        # pylint: disable=import-outside-toplevel
        from psyclone.psyGen import InvokeSchedule

        if (self.default_clause.clause_type !=
                OMPDefaultClause.DefaultClauseTypes.SHARED):
            raise GenerationError("OMPParallelClause cannot correctly generate"
                                  " the private clause when its default "
                                  "data sharing attribute in its default "
                                  "clause is not shared.")

        result = set()
        # get variable names from all calls that are a child of this node
        for call in self.kernels():
            for variable_name in call.local_vars():
                if variable_name == "":
                    raise InternalError(
                        f"call '{call.name}' has a local variable but its "
                        f"name is not set.")
                result.add(variable_name.lower())

        # Now determine scalar variables that must be private:
        var_accesses = VariablesAccessInfo()
        self.reference_accesses(var_accesses)
        for signature in var_accesses.all_signatures:
            accesses = var_accesses[signature].all_accesses
            # Ignore variables that have indices, we only look at scalar
            if accesses[0].is_array():
                continue

            # If a variable is only accessed once, it is either an error
            # or a shared variable - anyway it is not private
            if len(accesses) == 1:
                continue

            # We have at least two accesses. If the first one is a write,
            # assume the variable should be private:
            if accesses[0].access_type == AccessType.WRITE:
                # Check if the write access is inside the parallel loop. If
                # the write is outside of a loop, it is an assignment to
                # a shared variable. Example where jpk is likely used
                # outside of the parallel section later, so it must be
                # declared as shared in order to have its value in other loops:
                # !$omp parallel
                # jpk = 100
                # !omp do
                # do ji = 1, jpk

                # TODO #598: improve the handling of scalar variables.

                # Go up the tree till we either find the InvokeSchedule,
                # which is at the top, or a Loop statement (or no parent,
                # which means we have reached the end of a called kernel).
                parent = accesses[0].node.ancestor((Loop, InvokeSchedule),
                                                   include_self=True)

                if parent and isinstance(parent, Loop):
                    # The assignment to the variable is inside a loop, so
                    # declare it to be private
                    name = str(signature).lower()
                    symbol = accesses[0].node.scope.symbol_table.lookup(name)
                    result.add((name, symbol))

        # Convert the set into a list and sort it, so that we get
        # reproducible results
        list_result = list(result)
        list_result.sort(key=lambda x: x[0])

        # Create the OMPPrivateClause corresponding to the results
        priv_clause = OMPPrivateClause()
        for _, symbol in list_result:
            ref = Reference(symbol)
            priv_clause.addchild(ref)
        return priv_clause

    def validate_global_constraints(self):
        '''
        Perform validation checks that can only be done at code-generation
        time.

        :raises GenerationError: if this OMPDoDirective is not enclosed \
                            within some OpenMP parallel region.
        '''
        if self.ancestor(OMPParallelDirective) is not None:
            raise GenerationError("Cannot nest OpenMP parallel regions.")

    def _encloses_omp_directive(self):
        ''' Check that this Parallel region contains other OpenMP
            directives. While it doesn't have to (in order to be valid
            OpenMP), it is likely that an absence of directives
            is an error on the part of the user. '''
        # We need to recurse down through all our children and check
        # whether any of them are an OMPRegionDirective.
        node_list = self.walk(OMPRegionDirective)
        if not node_list:
            # TODO raise a warning here so that the user can decide
            # whether or not this is OK.
            pass
            # raise GenerationError("OpenMP parallel region does not enclose "
            #                       "any OpenMP directives. This is probably "
            #                       "not what you want.")


class OMPTaskloopDirective(OMPRegionDirective):
    '''
    Class representing an OpenMP TASKLOOP directive in the PSyIR.

    :param grainsize: The grainsize value used to specify the grainsize \
                      clause on this OpenMP directive. If this is None \
                      the grainsize clause is not applied. Default \
                      value is None.
    :type grainsize: int or None.
    :param num_tasks: The num_tasks value used to specify the num_tasks \
                      clause on this OpenMP directive. If this is None \
                      the num_tasks clause is not applied. Default value \
                      is None.
    :type num_tasks: int or None.
    :param nogroup: Whether the nogroup clause should be used for this node. \
                    Default value is False
    :type nogroup: bool
    :param kwargs: additional keyword arguments provided to the PSyIR node.
    :type kwargs: unwrapped dict.

    :raises GenerationError: if this OMPTaskloopDirective has both \
                             a grainsize and num_tasks value \
                             specified.
    '''
    # This specification respects the mutual exclusion of OMPGransizeClause
    # and OMPNumTasksClause, but adds an additional ordering requirement.
    # Other specifications to soften the ordering requirement are possible,
    # but need additional checks in the global constraints instead.
    _children_valid_format = ("Schedule, [OMPGrainsizeClause | "
                              "OMPNumTasksClause], [OMPNogroupClause]")

    def __init__(self, grainsize=None, num_tasks=None, nogroup=False,
                 **kwargs):
        # These remain primarily for the gen_code interface
        self._grainsize = grainsize
        self._num_tasks = num_tasks
        self._nogroup = nogroup
        if self._grainsize is not None and self._num_tasks is not None:
            raise GenerationError(
                "OMPTaskloopDirective must not have both grainsize and "
                "numtasks clauses specified.")
        super().__init__(**kwargs)
        if self._grainsize is not None:
            child = [Literal(f"{grainsize}", INTEGER_TYPE)]
            self._children.append(OMPGrainsizeClause(children=child))
        if self._num_tasks is not None:
            child = [Literal(f"{num_tasks}", INTEGER_TYPE)]
            self._children.append(OMPNumTasksClause(children=child))
        if self._nogroup:
            self._children.append(OMPNogroupClause())

    @staticmethod
    def _validate_child(position, child):
        '''
         Decides whether a given child and position are valid for this node.
         The rules are:
         1. Child 0 must always be a Schedule.
         2. Child 1 may be either a OMPGrainsizeClause or OMPNumTasksClause, \
            or if neither of those clauses are present, it may be a \
            OMPNogroupClause.
         3. Child 2 must always be a OMPNogroupClause, and can only exist if \
            child 1 is a OMPGrainsizeClause or OMPNumTasksClause.

        :param int position: the position to be validated.
        :param child: a child to be validated.
        :type child: :py:class:`psyclone.psyir.nodes.Node`

        :return: whether the given child and position are valid for this node.
        :rtype: bool

        '''
        if position == 0:
            return isinstance(child, Schedule)
        if position == 1:
            return isinstance(child, (OMPGrainsizeClause, OMPNumTasksClause,
                                      OMPNogroupClause))
        if position == 2:
            return isinstance(child, OMPNogroupClause)
        return False

    @property
    def nogroup(self):
        '''
        :returns: the nogroup clause status of this node.
        :rtype: bool
        '''
        return self._nogroup

    def validate_global_constraints(self):
        '''
        Perform validation checks that can only be done at code-generation
        time.

        :raises GenerationError: if this OMPTaskloopDirective is not \
                                 enclosed within an OpenMP serial region.
        :raises GenerationError: if this OMPTaskloopDirective has two
                                 Nogroup clauses as children.
        '''
        # It is only at the point of code generation that we can check for
        # correctness (given that we don't mandate the order that a user
        # can apply transformations to the code). A taskloop directive, we must
        # have an OMPSerialDirective as an ancestor back up the tree.
        if not self.ancestor(OMPSerialDirective):
            raise GenerationError(
                "OMPTaskloopDirective must be inside an OMP Serial region "
                "but could not find an ancestor node")

        # Check children are well formed.
        # _validate_child will ensure position 0 and 1 are valid.
        if len(self._children) == 3 and isinstance(self._children[1],
                                                   OMPNogroupClause):
            raise GenerationError(
                "OMPTaskloopDirective has two Nogroup clauses as children "
                "which is not allowed.")

        super().validate_global_constraints()

    def gen_code(self, parent):
        '''
        Generate the f2pygen AST entries in the Schedule for this OpenMP
        taskloop directive.

        :param parent: the parent Node in the Schedule to which to add our \
                       content.
        :type parent: sub-class of :py:class:`psyclone.f2pygen.BaseGen`
        :raises GenerationError: if this "!$omp taskloop" is not enclosed \
                                 within an OMP Parallel region and an OMP \
                                 Serial region.

        '''
        self.validate_global_constraints()

        extra_clauses = ""
        # Find the specified clauses
        clause_list = []
        if self._grainsize is not None:
            clause_list.append(f"grainsize({self._grainsize})")
        if self._num_tasks is not None:
            clause_list.append(f"num_tasks({self._num_tasks})")
        if self._nogroup:
            clause_list.append("nogroup")
        # Generate the string containing the required clauses
        extra_clauses = ", ".join(clause_list)

        parent.add(DirectiveGen(parent, "omp", "begin", "taskloop",
                                extra_clauses))

        self.dir_body.gen_code(parent)

        # make sure the directive occurs straight after the loop body
        position = parent.previous_loop()
        parent.add(DirectiveGen(parent, "omp", "end", "taskloop", ""),
                   position=["after", position])

    def begin_string(self):
        '''Returns the beginning statement of this directive, i.e.
        "omp taskloop ...". The visitor is responsible for adding the
        correct directive beginning (e.g. "!$").

        :returns: the beginning statement for this directive.
        :rtype: str

        '''
        return "omp taskloop"

    def end_string(self):
        '''Returns the end (or closing) statement of this directive, i.e.
        "omp end taskloop". The visitor is responsible for adding the
        correct directive beginning (e.g. "!$").

        :returns: the end statement for this directive.
        :rtype: str

        '''
        return "omp end taskloop"


class OMPDoDirective(OMPRegionDirective):
    '''
    Class representing an OpenMP DO directive in the PSyIR.

    :param str omp_schedule: the OpenMP schedule to use (defaults to
        "none" which means it is implementation dependent).
    :param Optional[int] collapse: optional number of nested loops to \
        collapse into a single iteration space to parallelise. Defaults to \
        None.
    :param Optional[bool] reprod: whether or not to generate code for \
        run-reproducible OpenMP reductions (if not specified the value is \
        provided by the PSyclone Config file).
    :param kwargs: additional keyword arguments provided to the PSyIR node.
    :type kwargs: unwrapped dict.

    '''
    _directive_string = "do"

    def __init__(self, omp_schedule="none", collapse=None, reprod=None,
                 **kwargs):

        super().__init__(**kwargs)
        if reprod is None:
            self._reprod = Config.get().reproducible_reductions
        else:
            self._reprod = reprod

        self._omp_schedule = omp_schedule
        self._collapse = None
        self.collapse = collapse  # Use setter with error checking

    def __eq__(self, other):
        '''
        Checks whether two nodes are equal. Two OMPDoDirective nodes are equal
        if they have the same schedule, the same reproducible reduction option
        (and the inherited equality is True).

        :param object other: the object to check equality to.

        :returns: whether other is equal to self.
        :rtype: bool
        '''
        is_eq = super().__eq__(other)
        is_eq = is_eq and self.omp_schedule == other.omp_schedule
        is_eq = is_eq and self.reprod == other.reprod
        is_eq = is_eq and self.collapse == other.collapse

        return is_eq

    @property
    def collapse(self):
        '''
        :returns: the value of the collapse clause.
        :rtype: int or NoneType
        '''
        return self._collapse

    @collapse.setter
    def collapse(self, value):
        '''
        TODO #1648: Note that gen_code ignores the collapse clause but the
        generated code is still valid. Since gen_code is going to be removed
        and it is only used for LFRic (which does not support GPU offloading
        that gets improved with the collapse clause) it will not be supported.

        :param value: optional number of nested loop to collapse into a \
            single iteration space to parallelise. Defaults to None.
        :type value: int or NoneType.

        :raises TypeError: if the collapse value given is not an integer \
            or NoneType.
        :raises ValueError: if the collapse integer given is not positive.

        '''
        if value is not None and not isinstance(value, int):
            raise TypeError(
                f"The {type(self).__name__} collapse clause must be a positive"
                f" integer or None, but value '{value}' has been given.")

        if value is not None and value <= 0:
            raise ValueError(
                f"The {type(self).__name__} collapse clause must be a positive"
                f" integer or None, but value '{value}' has been given.")

        self._collapse = value

    def node_str(self, colour=True):
        '''
        Returns the name of this node with (optional) control codes
        to generate coloured output in a terminal that supports it.

        :param bool colour: whether or not to include colour control codes.

        :returns: description of this node, possibly coloured.
        :rtype: str
        '''
        parts = []
        if self.omp_schedule != "none":
            parts.append(f"omp_schedule={self.omp_schedule}")
        if self.reductions():
            parts.append(f"reprod={self._reprod}")
        if self._collapse and self._collapse > 1:
            parts.append(f"collapse={self._collapse}")
        return f"{self.coloured_name(colour)}[{','.join(parts)}]"

    def _reduction_string(self):
        '''
        :returns: the OMP reduction information.
        :rtype: str
        '''
        for reduction_type in AccessType.get_valid_reduction_modes():
            reductions = self._get_reductions_list(reduction_type)
            parts = []
            for reduction in reductions:
                parts.append(f"reduction("
                             f"{OMP_OPERATOR_MAPPING[reduction_type]}:"
                             f"{reduction})")
        return ", ".join(parts)

    @property
    def omp_schedule(self):
        '''
        :returns: the omp_schedule for this object.
        :rtype: str
        '''
        return self._omp_schedule

    @omp_schedule.setter
    def omp_schedule(self, value):
        '''
        :param str value: the omp_schedule for this object.

        :raises TypeError: if the provided omp_schedule is not a valid \
            schedule string.
        '''
        if not isinstance(value, str):
            raise TypeError(
                f"{type(self).__name__} omp_schedule should be a str "
                f"but found '{type(value).__name__}'.")
        if (value.split(',')[0].lower() not in
                OMPScheduleClause.VALID_OMP_SCHEDULES):
            raise TypeError(
                f"{type(self).__name__} omp_schedule should be one of "
                f"{OMPScheduleClause.VALID_OMP_SCHEDULES} but found "
                f"'{value}'.")
        self._omp_schedule = value

    @property
    def reprod(self):
        '''
        :returns: whether reprod has been set for this object or not.
        '''
        return self._reprod

    @reprod.setter
    def reprod(self, value):
        '''
        :param bool value: enable or disable reproducible loop parallelism.
        '''
        self._reprod = value

    def validate_global_constraints(self):
        '''
        Perform validation checks that can only be done at code-generation
        time.

        :raises GenerationError: if this OMPDoDirective is not enclosed \
                            within some OpenMP parallel region.
        '''
        # It is only at the point of code generation that we can check for
        # correctness (given that we don't mandate the order that a user
        # can apply transformations to the code). As a loop
        # directive, we must have an OMPParallelDirective as an ancestor
        # somewhere back up the tree.
        if not self.ancestor(OMPParallelDirective,
                             excluding=OMPParallelDoDirective):
            raise GenerationError(
                "OMPDoDirective must be inside an OMP parallel region but "
                "could not find an ancestor OMPParallelDirective node")

        self._validate_single_loop()
        self._validate_collapse_value()

        super().validate_global_constraints()

    def _validate_collapse_value(self):
        '''
        Checks that if there is a collapse clause, there must be as many
        immediately nested loops as the collapse value.

        :raises GenerationError: if this OMPLoopDirective has a collapse \
            clause but it doesn't have the expected number of nested Loops.
        '''
        if self._collapse:
            cursor = self.dir_body.children[0]
            for depth in range(self._collapse):
                if (len(cursor.parent.children) != 1 or
                        not isinstance(cursor, Loop)):
                    raise GenerationError(
                        f"{type(self).__name__} must have as many immediately "
                        f"nested loops as the collapse clause specifies but "
                        f"'{self}' has a collapse={self._collapse} and the "
                        f"nested body at depth {depth} cannot be "
                        f"collapsed.")
                cursor = cursor.loop_body.children[0]

    def _validate_single_loop(self):
        '''
        Checks that this directive is only applied to a single Loop node.

        :raises GenerationError: if this directive has more than one child.
        :raises GenerationError: if the child of this directive is not a Loop.

        '''
        if len(self.dir_body.children) != 1:
            raise GenerationError(
                f"An {type(self).__name__} can only be applied to a single "
                f"loop but this Node has {len(self.dir_body.children)} "
                f"children: {self.dir_body.children}")

        if not isinstance(self.dir_body[0], Loop):
            raise GenerationError(
                f"An {type(self).__name__} can only be applied to a loop but "
                f"this Node has a child of type "
                f"'{type(self.dir_body[0]).__name__}'")

    def gen_code(self, parent):
        '''
        Generate the f2pygen AST entries in the Schedule for this OpenMP do
        directive.

        TODO #1648: Note that gen_code ignores the collapse clause but the
        generated code is still valid. Since gen_code is going to be removed
        and it is only used for LFRic (which does not support GPU offloading
        that gets improved with the collapse clause) it will not be supported.

        :param parent: the parent Node in the Schedule to which to add our \
                       content.
        :type parent: sub-class of :py:class:`psyclone.f2pygen.BaseGen`
        :raises GenerationError: if this "!$omp do" is not enclosed within \
                                 an OMP Parallel region.

        '''
        self.validate_global_constraints()

        parts = []

        if self.omp_schedule != "none":
            parts.append(f"schedule({self.omp_schedule})")

        if not self._reprod:
            red_str = self._reduction_string()
            if red_str:
                parts.append(red_str)

        # As we're a loop we don't specify the scope
        # of any variables so we don't have to generate the
        # list of private variables
        options = ", ".join(parts)
        parent.add(DirectiveGen(parent, "omp", "begin", "do", options))

        for child in self.children:
            child.gen_code(parent)

        # make sure the directive occurs straight after the loop body
        position = parent.previous_loop()
        parent.add(DirectiveGen(parent, "omp", "end", "do", ""),
                   position=["after", position])

    def begin_string(self):
        '''Returns the beginning statement of this directive, i.e.
        "omp do ...". The visitor is responsible for adding the
        correct directive beginning (e.g. "!$").

        :returns: the beginning statement for this directive.
        :rtype: str

        '''
        string = f"omp {self._directive_string}"
        if self.omp_schedule != "none":
            string += f" schedule({self.omp_schedule})"
        if self._collapse:
            string += f" collapse({self._collapse})"
        return string

    def end_string(self):
        '''Returns the end (or closing) statement of this directive, i.e.
        "omp end do". The visitor is responsible for adding the
        correct directive beginning (e.g. "!$").

        :returns: the end statement for this directive.
        :rtype: str

        '''
        return f"omp end {self._directive_string}"


class OMPParallelDoDirective(OMPParallelDirective, OMPDoDirective):
    ''' Class for the !$OMP PARALLEL DO directive. This inherits from
        both OMPParallelDirective (because it creates a new OpenMP
        thread-parallel region) and OMPDoDirective (because it
        causes a loop to be parallelised).

        :param kwargs: additional keyword arguments provided to the PSyIR node.
        :type kwargs: unwrapped dict.
    '''

    _children_valid_format = ("Schedule, OMPDefaultClause, OMPPrivateClause, "
                              "OMPScheduleClause, [OMPReductionClause]*")
    _directive_string = "parallel do"

    def __init__(self, **kwargs):
        OMPDoDirective.__init__(self, **kwargs)
        self.addchild(OMPDefaultClause(
            clause_type=OMPDefaultClause.DefaultClauseTypes.SHARED))

    @staticmethod
    def _validate_child(position, child):
        '''
        :param int position: the position to be validated.
        :param child: a child to be validated.
        :type child: :py:class:`psyclone.psyir.nodes.Node`

        :return: whether the given child and position are valid for this node.
        :rtype: bool

        '''
        if position == 0 and isinstance(child, Schedule):
            return True
        if position == 1 and isinstance(child, OMPDefaultClause):
            return True
        if position == 2 and isinstance(child, OMPPrivateClause):
            return True
        if position == 3 and isinstance(child, OMPScheduleClause):
            return True
        if position >= 4 and isinstance(child, OMPReductionClause):
            return True
        return False

    def gen_code(self, parent):
        '''
        Generate the f2pygen AST entries in the Schedule for this OpenMP
        directive.

        TODO #1648: Note that gen_code ignores the collapse clause but the
        generated code is still valid. Since gen_code is going to be removed
        and it is only used for LFRic (which does not support GPU offloading
        that gets improved with the collapse clause) it will not be supported.

        :param parent: the parent Node in the Schedule to which to add our \
                       content.
        :type parent: sub-class of :py:class:`psyclone.f2pygen.BaseGen`

        '''
        # We're not doing nested parallelism so make sure that this
        # omp parallel do is not already within some parallel region
        # pylint: disable=import-outside-toplevel
        from psyclone.psyGen import zero_reduction_variables
        self.validate_global_constraints()

        calls = self.reductions()
        zero_reduction_variables(calls, parent)
        private_clause = self._get_private_clause()
        if len(self._children) >= 3 and private_clause != self._children[2]:
            # Replace the current private clause.
            self._children[2] = private_clause
        elif len(self._children) < 3:
            self.addchild(private_clause, index=2)
        default_str = self.children[1]._clause_string
        private_list = []
        for child in self.children[2].children:
            private_list.append(child.symbol.name)
        private_str = "private(" + ",".join(private_list) + ")"

        sched_clause = OMPScheduleClause(self._omp_schedule)
        if len(self._children) >= 4 and sched_clause != self._children[3]:
            self._children[3] = sched_clause
        elif len(self._children) < 4:
            self.addchild(sched_clause, index=3)
        if sched_clause.schedule != "none":
            schedule_str = f"schedule({sched_clause.schedule})"
        else:
            schedule_str = ""
        parent.add(
            DirectiveGen(
                parent, "omp", "begin", "parallel do",
                ", ".join(text for text in
                          [default_str, private_str, schedule_str,
                           self._reduction_string()] if text)))

        for child in self.dir_body:
            child.gen_code(parent)

        # make sure the directive occurs straight after the loop body
        position = parent.previous_loop()
        parent.add(DirectiveGen(parent, *self.end_string().split()),
                   position=["after", position])

        self.gen_post_region_code(parent)

    def lower_to_language_level(self):
        '''
        In-place construction of clauses as PSyIR constructs.
        The clauses here may need to be updated if code has changed, or be
        added if not yet present.
        '''
        private_clause = self._get_private_clause()
        if len(self._children) >= 3 and private_clause != self._children[2]:
            self._children[2] = private_clause
        elif len(self._children) < 3:
            self.addchild(private_clause, index=2)
        sched_clause = OMPScheduleClause(self._omp_schedule)
        if len(self._children) >= 4 and sched_clause != self._children[3]:
            self._children[3] = sched_clause
        elif len(self._children) < 4:
            self.addchild(sched_clause, index=3)

        super().lower_to_language_level()

    def begin_string(self):
        '''Returns the beginning statement of this directive, i.e.
        "omp do ...". The visitor is responsible for adding the
        correct directive beginning (e.g. "!$").

        :returns: the beginning statement for this directive.
        :rtype: str

        '''
        string = f"omp {self._directive_string}"
        if self._collapse:
            string += f" collapse({self._collapse})"
        string += self._reduction_string()
        return string

    def end_string(self):
        '''Returns the end (or closing) statement of this directive, i.e.
        "omp end do". The visitor is responsible for adding the
        correct directive beginning (e.g. "!$").

        :returns: the end statement for this directive.
        :rtype: str

        '''
        return f"omp end {self._directive_string}"

    def validate_global_constraints(self):
        '''
        Perform validation checks that can only be done at code-generation
        time.

        '''
        OMPParallelDirective.validate_global_constraints(self)

        self._validate_single_loop()
        self._validate_collapse_value()


class OMPTeamsDistributeParallelDoDirective(OMPParallelDoDirective):
    ''' Class representing the OMP teams distribute parallel do directive. '''
    _directive_string = "teams distribute parallel do"


class OMPTargetDirective(OMPRegionDirective):
    ''' Class for the !$OMP TARGET directive that offloads the code contained
    in its region into an accelerator device. '''

    def begin_string(self):
        '''Returns the beginning statement of this directive, i.e.
        "omp target". The visitor is responsible for adding the
        correct directive beginning (e.g. "!$").

        :returns: the opening statement of this directive.
        :rtype: str

        '''
        return "omp target"

    def end_string(self):
        '''Returns the end (or closing) statement of this directive, i.e.
        "omp end target". The visitor is responsible for adding the
        correct directive beginning (e.g. "!$").

        :returns: the end statement for this directive.
        :rtype: str

        '''
        return "omp end target"


class OMPLoopDirective(OMPRegionDirective):
    ''' Class for the !$OMP LOOP directive that specifies that the iterations
    of the associated loops may execute concurrently.

    :param Optional[int] collapse: optional number of nested loops to \
        collapse into a single iteration space to parallelise. Defaults \
        to None.
    :param kwargs: additional keyword arguments provided to the PSyIR node.
    :type kwargs: unwrapped dict.
    '''

    def __init__(self, collapse=None, **kwargs):
        super().__init__(**kwargs)
        self._collapse = None
        self.collapse = collapse  # Use setter with error checking

    def __eq__(self, other):
        '''
        Checks whether two nodes are equal. Two OMPLoopDirective nodes are
        equal if they have the same collapse status and the inherited
        equality is true.

        :param object other: the object to check equality to.

        :returns: whether other is equal to self.
        :rtype: bool
        '''
        is_eq = super().__eq__(other)
        is_eq = is_eq and self.collapse == other.collapse

        return is_eq

    @property
    def collapse(self):
        '''
        :returns: the value of the collapse clause.
        :rtype: int or NoneType
        '''
        return self._collapse

    @collapse.setter
    def collapse(self, value):
        '''
        TODO #1648: Note that gen_code ignores the collapse clause but the
        generated code is still valid. Since gen_code is going to be removed
        and it is only used for LFRic (which does not support GPU offloading
        that gets improved with the collapse clause) it will not be supported.

        :param value: optional number of nested loop to collapse into a \
            single iteration space to parallelise. Defaults to None.
        :type value: int or NoneType.

        :raises TypeError: if the collapse value given is not an integer \
            or NoneType.
        :raises ValueError: if the collapse integer given is not positive.

        '''
        if value is not None and not isinstance(value, int):
            raise TypeError(
                f"The OMPLoopDirective collapse clause must be a positive "
                f"integer or None, but value '{value}' has been given.")

        if value is not None and value <= 0:
            raise ValueError(
                f"The OMPLoopDirective collapse clause must be a positive "
                f"integer or None, but value '{value}' has been given.")

        self._collapse = value

    def node_str(self, colour=True):
        ''' Returns the name of this node with (optional) control codes
        to generate coloured output in a terminal that supports it.

        :param bool colour: whether or not to include colour control codes.

        :returns: description of this node, possibly coloured.
        :rtype: str
        '''
        text = self.coloured_name(colour)
        if self._collapse:
            text += f"[collapse={self._collapse}]"
        else:
            text += "[]"
        return text

    def begin_string(self):
        ''' Returns the beginning statement of this directive, i.e. "omp loop".
        The visitor is responsible for adding the correct directive beginning
        (e.g. "!$").

        :returns: the opening statement of this directive.
        :rtype: str

        '''
        string = "omp loop"
        if self._collapse:
            string += f" collapse({self._collapse})"
        return string

    def end_string(self):
        '''Returns the end (or closing) statement of this directive, i.e.
        "omp end loop". The visitor is responsible for adding the
        correct directive beginning (e.g. "!$").

        :returns: the end statement for this directive.
        :rtype: str

        '''
        return "omp end loop"

    def validate_global_constraints(self):
        ''' Perform validation of those global constraints that can only be
        done at code-generation time.

        :raises GenerationError: if this OMPLoopDirective has more than one \
            child in its associated schedule.
        :raises GenerationError: if the schedule associated with this \
            OMPLoopDirective does not contain a Loop.
        :raises GenerationError: this directive must be inside a omp target \
            or parallel region.
        :raises GenerationError: if this OMPLoopDirective has a collapse \
            clause but it doesn't have the expected number of nested Loops.

        '''
        if len(self.dir_body.children) != 1:
            raise GenerationError(
                f"OMPLoopDirective must have exactly one child in its "
                f"associated schedule but found {self.dir_body.children}.")

        if not isinstance(self.dir_body.children[0], Loop):
            raise GenerationError(
                f"OMPLoopDirective must have a Loop as child of its associated"
                f" schedule but found '{self.dir_body.children[0]}'.")

        if not self.ancestor((OMPTargetDirective, OMPParallelDirective)):
            # Also omp teams or omp threads regions but these are not supported
            # in the PSyIR
            raise GenerationError(
                f"OMPLoopDirective must be inside a OMPTargetDirective or a "
                f"OMPParallelDirective, but '{self}' is not.")

        # If there is a collapse clause, there must be as many immediately
        # nested loops as the collapse value
        if self._collapse:
            cursor = self.dir_body.children[0]
            for depth in range(self._collapse):
                if not isinstance(cursor, Loop):
                    raise GenerationError(
                        f"OMPLoopDirective must have as many immediately "
                        f"nested loops as the collapse clause specifies but "
                        f"'{self}' has a collapse={self._collapse} and the "
                        f"nested statement at depth {depth} is a "
                        f"{type(cursor).__name__} rather than a Loop.")
                cursor = cursor.loop_body.children[0]

        super().validate_global_constraints()


# For automatic API documentation generation
__all__ = ["OMPRegionDirective", "OMPParallelDirective", "OMPSingleDirective",
           "OMPMasterDirective", "OMPDoDirective", "OMPParallelDoDirective",
           "OMPSerialDirective", "OMPTaskloopDirective", "OMPTargetDirective",
           "OMPTaskwaitDirective", "OMPDirective", "OMPStandaloneDirective",
           "OMPLoopDirective", "OMPDeclareTargetDirective"]
