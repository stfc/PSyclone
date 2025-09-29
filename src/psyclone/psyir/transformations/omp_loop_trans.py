# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017-2025, Science and Technology Facilities Council.
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
# Authors: S. Siso and N. Nobre, STFC Daresbury Lab
# Modified: A. B. G. Chalk, STFC Daresbury Lab
#           M. Naylor, University of Cambridge, UK

''' Transformation to insert OpenMP directives to parallelise PSyIR Loops. '''

from psyclone.configuration import Config
from psyclone.psyir.nodes import (
    Directive, Schedule,
    Routine, OMPDoDirective, OMPLoopDirective, OMPParallelDoDirective,
    OMPTeamsDistributeParallelDoDirective, OMPTeamsLoopDirective,
    OMPScheduleClause, OMPBarrierDirective, OMPParallelDirective,
    OMPReductionClause, BinaryOperation, IntrinsicCall
)
from psyclone.psyir.transformations.parallel_loop_trans import \
    ParallelLoopTrans
from psyclone.utils import transformation_documentation_wrapper

#: Mapping from simple string to actual directive class.
MAP_STR_TO_LOOP_DIRECTIVES = {
    "do": OMPDoDirective,
    "paralleldo": OMPParallelDoDirective,
    "teamsdistributeparalleldo": OMPTeamsDistributeParallelDoDirective,
    "teamsloop": OMPTeamsLoopDirective,
    "loop": OMPLoopDirective
}

#: Mapping from simple string to corresponding barrier type.
MAP_STR_TO_BARRIER_DIRECTIVE = {
    "do": OMPBarrierDirective,
}
#: List containing the valid names for OMP directives.
VALID_OMP_DIRECTIVES = list(MAP_STR_TO_LOOP_DIRECTIVES.keys())

#: Mapping from PSyIR reduction operator to OMP reduction operator.
MAP_REDUCTION_OP_TO_OMP = {
    BinaryOperation.Operator.ADD:
        OMPReductionClause.ReductionClauseTypes.ADD,
    BinaryOperation.Operator.SUB:
        OMPReductionClause.ReductionClauseTypes.SUB,
    BinaryOperation.Operator.MUL:
        OMPReductionClause.ReductionClauseTypes.MUL,
    BinaryOperation.Operator.AND:
        OMPReductionClause.ReductionClauseTypes.AND,
    BinaryOperation.Operator.OR:
        OMPReductionClause.ReductionClauseTypes.OR,
    BinaryOperation.Operator.EQV:
        OMPReductionClause.ReductionClauseTypes.EQV,
    BinaryOperation.Operator.NEQV:
        OMPReductionClause.ReductionClauseTypes.NEQV,
    IntrinsicCall.Intrinsic.MAX:
        OMPReductionClause.ReductionClauseTypes.MAX,
    IntrinsicCall.Intrinsic.MIN:
        OMPReductionClause.ReductionClauseTypes.MIN,
    IntrinsicCall.Intrinsic.IAND:
        OMPReductionClause.ReductionClauseTypes.IAND,
    IntrinsicCall.Intrinsic.IOR:
        OMPReductionClause.ReductionClauseTypes.IOR,
    IntrinsicCall.Intrinsic.IEOR:
        OMPReductionClause.ReductionClauseTypes.IEOR
}


@transformation_documentation_wrapper
class OMPLoopTrans(ParallelLoopTrans):
    '''
    Adds an OpenMP directive to parallelise this loop. It can insert different
    directives such as "omp do/for", "omp parallel do/for", "omp teams
    distribute parallel do/for" or "omp loop" depending on the provided
    parameters.

    The OpenMP schedule to use can also be specified, but this will be ignored
    in case of the "omp loop" (as the 'schedule' clause is not valid for this
    specific directive). The configuration-defined 'reprod' parameter
    also specifies whether a manual reproducible reproduction is to be used.
    Note, reproducible in this case means obtaining the same results with the
    same number of OpenMP threads, not for different numbers of OpenMP threads.

    :param str omp_schedule: the OpenMP schedule to use. Defaults to 'auto'.
    :param str omp_directive: choose which OpenMP loop directive to use.
        Defaults to "do". The available options are "do" for "omp do";
        "paralleldo" for "omp parallel do"; "teamsdistributeparalleldo"
        for "omp teams distribute parallel do"; "teamsloop" for
        "omp teams loop"; and "loop" for "omp loop".

    For example:

    >>> from psyclone.psyir.frontend.fortran import FortranReader
    >>> from psyclone.psyir.backend.fortran import FortranWriter
    >>> from psyclone.psyir.nodes import Loop
    >>> from psyclone.psyir.transformations import OMPLoopTrans
    >>> from psyclone.transformations import OMPParallelTrans
    >>>
    >>> psyir = FortranReader().psyir_from_source("""
    ...     subroutine my_subroutine()
    ...         integer, dimension(10, 10) :: A
    ...         integer :: i
    ...         integer :: j
    ...         do i = 1, 10
    ...             do j = 1, 10
    ...                 A(i, j) = 0
    ...             end do
    ...         end do
    ...     end subroutine
    ...     """)
    >>> loop = psyir.walk(Loop)[0]
    >>> omplooptrans1 = OMPLoopTrans(omp_schedule="dynamic",
    ...                              omp_directive="paralleldo")
    >>> omplooptrans1.apply(loop)
    >>> print(FortranWriter()(psyir))
    subroutine my_subroutine()
      integer, dimension(10,10) :: a
      integer :: i
      integer :: j
    <BLANKLINE>
      !$omp parallel do default(shared), private(i,j), schedule(dynamic)
      do i = 1, 10, 1
        do j = 1, 10, 1
          a(i,j) = 0
        enddo
      enddo
      !$omp end parallel do
    <BLANKLINE>
    end subroutine my_subroutine
    <BLANKLINE>

    '''
    def __init__(self, omp_directive="do", omp_schedule="auto"):
        super().__init__()
        # Whether or not to generate code for (run-to-run on n threads)
        # reproducible OpenMP reductions. This setting can be overridden
        # via the `reprod` argument to the apply() method.
        self._reprod = Config.get().reproducible_reductions

        # Use setters to set up attributes
        self._omp_schedule = ""
        self.omp_schedule = omp_schedule

        self._omp_directive = ""
        self.omp_directive = omp_directive

    def __str__(self):
        return "Adds an OpenMP directive to parallelise the target loop"

    def _add_asynchronicity(self, instance: Directive):
        ''' Adds asynchronicity to the provided directive if possible. If
        PSyclone's analysis suggests that it is not possible, the directive
        is left unchanged.

        The only directive that this method can act on is the OMPDoDirective.

        :param instance: The directive to make asynchronous if possible.
        '''
        # Of the various directives supported by this transformation, only the
        # OMPDoDirective supports the nowait clause. This needs to be an
        # exact type check
        if type(instance) is not OMPDoDirective:
            return
        # The loop is the first child of the schedule of an OMPDoDirective.
        node = instance.dir_body.children[0]
        # Otherwise find the next dependency.
        next_depend = self._find_next_dependency(node, instance)
        # If find_next_dependency returns False, then this loop is its own
        # next dependency so we can't add an asynchronous clause.
        if not next_depend:
            return

        barrier_type = MAP_STR_TO_BARRIER_DIRECTIVE[self.omp_directive]
        # If find next_dependency returns True there is no follow up
        # dependency, so we just need a barrier at the end of the containing
        # Routine.
        if next_depend is True:
            # Add nowait to the instance.
            instance.nowait = True
            # Add a barrier to the end of the containing Routine if there
            # isn't one already.
            containing_routine = node.ancestor((Routine, OMPParallelDirective))
            containing_schedule = containing_routine.walk(Schedule)[0]
            # Check barrier that corresponds to self.omp_directive and add the
            # correct barrier type
            if not isinstance(containing_schedule.children[-1], barrier_type):
                containing_schedule.addchild(barrier_type())
            return

        # Otherwise we have the next dependencies and we need to find where
        # the correct place for the preceding barrier is. Need to find a
        # guaranteed control flow path to place it.
        for depend in next_depend:
            # Find the deepest schedule in the tree containing both.
            sched = depend.ancestor(Schedule, shared_with=node)
            # Get the path from sched to depend
            path = depend.path_from(sched)
            # The first element of path is the position of the ancestor
            # of next_depend that is in sched, so we add the barrier there.
            sched.addchild(barrier_type(), path[0])
        instance.nowait = True

    @property
    def omp_directive(self):
        '''
        :returns: the type of OMP directive that this transformation will
            insert.
        :rtype: str
        '''
        return self._omp_directive

    @omp_directive.setter
    def omp_directive(self, value):
        '''
        :param str value: the type of OMP directive to add.

        :raises TypeError: if the provided value is not a valid str.
        '''
        if not isinstance(value, str) or value not in VALID_OMP_DIRECTIVES:
            raise TypeError(
                f"The {type(self).__name__}.omp_directive property must be "
                f"a str with the value of {VALID_OMP_DIRECTIVES}"
                f" but found a '{type(value).__name__}' with value '{value}'.")
        self._omp_directive = value

    @property
    def omp_schedule(self):
        '''
        :returns: the OpenMP schedule that will be specified by
            this transformation.
        :rtype: str

        '''
        return self._omp_schedule

    @omp_schedule.setter
    def omp_schedule(self, value):
        '''
        :param str value: Sets the OpenMP schedule value that will be
            specified by this transformation, unless adding an OMP Loop
            directive (in which case it is not applicable).

        :raises TypeError: if the provided value is not a string.
        :raises ValueError: if the provided string is not a valid OpenMP
            schedule format.
        '''

        if not isinstance(value, str):
            raise TypeError(
                f"The OMPLoopTrans.omp_schedule property must be a 'str'"
                f" but found a '{type(value).__name__}'.")

        # Some schedules have an optional chunk size following a ','
        value_parts = value.split(',')
        if value_parts[0].lower() not in OMPScheduleClause.VALID_OMP_SCHEDULES:
            raise ValueError(
                f"Valid OpenMP schedules are "
                f"{OMPScheduleClause.VALID_OMP_SCHEDULES} but got "
                f"'{value_parts[0]}'.")

        if len(value_parts) > 1:
            if value_parts[0] == "auto":
                raise ValueError("Cannot specify a chunk size when using an "
                                 "OpenMP schedule of 'auto'.")
            try:
                int(value_parts[1].strip())
            except ValueError as err:
                raise ValueError(f"Supplied OpenMP schedule '{value}' has an "
                                 f"invalid chunk-size.") from err

        self._omp_schedule = value

    def _directive(self, children, collapse=None):
        ''' Creates the type of directive needed for this sub-class of
        transformation.

        :param children: list of Nodes that will be the children of
            the created directive.
        :type children: List[:py:class:`psyclone.psyir.nodes.Node`]
        :param int collapse: number of nested loops to collapse or None if
            no collapse attribute is required.

        :returns: the new node representing the directive in the AST
        :rtype: :py:class:`psyclone.psyir.nodes.OMPDoDirective` |
            :py:class:`psyclone.psyir.nodes.OMPParallelDoDirective` |
            :py:class:`psyclone.psyir.nodes.
            OMPTeamsDistributeParallelDoDirective` |
            :py:class:`psyclone.psyir.nodes.OMPLoopDirective`
        '''
        node = MAP_STR_TO_LOOP_DIRECTIVES[self._omp_directive](
                children=children,
                collapse=collapse)
        # OMP loop does not support 'schedule' or 'reprod' attributes, so we do
        # not attempt to set these properties for this specific directive
        if self._omp_directive != "loop":
            node.omp_schedule = self._omp_schedule
            node.reprod = self._reprod
        return node

    def apply(self, node, options=None,
              reprod: bool = None,
              enable_reductions: bool = False,
              **kwargs):
        '''Apply the OMPLoopTrans transformation to the specified PSyIR Loop.

        :param node: the supplied node to which we will apply the
                     OMPLoopTrans transformation
        :type node: :py:class:`psyclone.psyir.nodes.Node`
        :param bool reprod: indicating whether reproducible reductions should
            be used. By default the value from the config file will be used.
        :param options: a dictionary with options for transformations
                        and validation.
        :type options: Optional[Dict[str, Any]]
        :param enable_reductions: whether to attempt to infer reduction
            clauses or not.
        '''
        if enable_reductions:
            red_ops = list(MAP_REDUCTION_OP_TO_OMP.keys())
        else:
            red_ops = []

        # TODO 2668 - options dict is deprecated.
        local_options = options.copy() if options is not None else None
        if not options:
            if reprod is None:
                reprod = Config.get().reproducible_reductions
            self.validate_options(
                    reprod=reprod,
                    enable_reductions=enable_reductions,
                    reduction_ops=red_ops,
                    **kwargs
            )
            self._reprod = reprod
        else:
            self._reprod = options.get("reprod",
                                       Config.get().reproducible_reductions)
            if options.get("enable_reductions", False):
                local_options["reduction_ops"] = list(
                    MAP_REDUCTION_OP_TO_OMP.keys())
            else:
                local_options["reduction_ops"] = []

        parent = node.parent
        position = node.position
        super().apply(node, local_options, reduction_ops=red_ops, **kwargs)

        # Add reduction clauses to the newly introduced directive
        directive = parent.children[position]
        for (op, ref) in self.inferred_reduction_clauses:
            clause = OMPReductionClause(MAP_REDUCTION_OP_TO_OMP[op])
            clause.addchild(ref)
            directive.addchild(clause)
