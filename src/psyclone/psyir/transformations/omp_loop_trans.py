# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017-2024, Science and Technology Facilities Council.
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

''' Transformation to insert OpenMP directives to parallelise PSyIR Loops. '''

from psyclone.configuration import Config
from psyclone.psyir.nodes import (
    Routine, OMPDoDirective, OMPLoopDirective, OMPParallelDoDirective,
    OMPTeamsDistributeParallelDoDirective, OMPScheduleClause)
from psyclone.psyir.symbols import DataSymbol, INTEGER_TYPE
from psyclone.psyir.transformations.parallel_loop_trans import \
    ParallelLoopTrans

MAP_STR_TO_LOOP_DIRECTIVES = {
    "do": OMPDoDirective,
    "paralleldo": OMPParallelDoDirective,
    "teamsdistributeparalleldo": OMPTeamsDistributeParallelDoDirective,
    "loop": OMPLoopDirective
}
VALID_OMP_DIRECTIVES = list(MAP_STR_TO_LOOP_DIRECTIVES.keys())


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
    :param str omp_directive: choose which OpenMP loop directive to use. \
        Defaults to "omp do"

    For example:

    >>> from psyclone.psyir.frontend.fortran import FortranReader
    >>> from psyclone.psyir.backend.fortran import FortranWriter
    >>> from psyclone.psyir.nodes import Loop
    >>> from psyclone.transformations import OMPLoopTrans, OMPParallelTrans
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

    @property
    def omp_directive(self):
        '''
        :returns: the type of OMP directive that this transformation will \
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
        :returns: the OpenMP schedule that will be specified by \
            this transformation.
        :rtype: str

        '''
        return self._omp_schedule

    @omp_schedule.setter
    def omp_schedule(self, value):
        '''
        :param str value: Sets the OpenMP schedule value that will be \
            specified by this transformation, unless adding an OMP Loop \
            directive (in which case it is not applicable).

        :raises TypeError: if the provided value is not a string.
        :raises ValueError: if the provided string is not a valid OpenMP \
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

        :param children: list of Nodes that will be the children of \
            the created directive.
        :type children: List[:py:class:`psyclone.psyir.nodes.Node`]
        :param int collapse: number of nested loops to collapse or None if \
            no collapse attribute is required.

        :returns: the new node representing the directive in the AST
        :rtype: :py:class:`psyclone.psyir.nodes.OMPDoDirective` | \
            :py:class:`psyclone.psyir.nodes.OMPParallelDoDirective` | \
            :py:class:`psyclone.psyir.nodes. \
            OMPTeamsDistributeParallelDoDirective` | \
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

    def apply(self, node, options=None):
        '''Apply the OMPLoopTrans transformation to the specified PSyIR Loop.

        :param node: the supplied node to which we will apply the \
                     OMPLoopTrans transformation
        :type node: :py:class:`psyclone.psyir.nodes.Node`
        :param options: a dictionary with options for transformations\
                        and validation.
        :type options: Optional[Dict[str, Any]]
        :param bool options["reprod"]:
                indicating whether reproducible reductions should be used. \
                By default the value from the config file will be used.

        '''
        if not options:
            options = {}
        self._reprod = options.get("reprod",
                                   Config.get().reproducible_reductions)

        if self._reprod:
            # When reprod is True, the variables th_idx and nthreads are
            # expected to be declared in the scope.
            root = node.ancestor(Routine)

            symtab = root.symbol_table
            try:
                symtab.lookup_with_tag("omp_thread_index")
            except KeyError:
                symtab.new_symbol(
                    "th_idx", tag="omp_thread_index",
                    symbol_type=DataSymbol, datatype=INTEGER_TYPE)
            try:
                symtab.lookup_with_tag("omp_num_threads")
            except KeyError:
                symtab.new_symbol(
                    "nthreads", tag="omp_num_threads",
                    symbol_type=DataSymbol, datatype=INTEGER_TYPE)

        super().apply(node, options)
