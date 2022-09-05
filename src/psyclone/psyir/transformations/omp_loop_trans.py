# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017-2022, Science and Technology Facilities Council.
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
# Author: S. Siso, STFC Daresbury Lab

''' '''

from psyclone.transformations import ParallelLoopTrans


class OMPLoopTrans(ParallelLoopTrans):
    '''
    Adds an OpenMP directive to a loop. This can be the loop worksharing
    OpenMP Do/For directive to distribute the iterations of the enclosed
    loop or a descriptive OpenMP loop directive to let the compiler decide
    the best implementation. The OpenMP schedule used for the worksharing
    directive can also be specified, but this will be ignored in case of the
    descriptive OpenMP loop. The configuration-defined 'reprod' parameter
    also specifies whether a manual reproducible reproduction is to be used.

    :param str omp_schedule: the OpenMP schedule to use. Defaults to 'static'.
    :param bool omp_worksharing: whether to generate OpenMP loop worksharing \
        directives (e.g. omp do/for) or an OpenMP loop directive. Defaults to \
        True.

    For example:

    >>> from psyclone.psyir.frontend.fortran import FortranReader
    >>> from psyclone.psyir.nodes import Routine
    >>> from psyclone.transformations import OMPLoopTrans, OMPParallelTrans
    >>>
    >>> tree = FortranReader().psyir_from_source("""
    ...     subroutine my_subroutine()
    ...         integer, dimension(10, 10) :: A
    ...         integer :: i
    ...         integer :: j
    ...         do i = 1, 10
    ...             do j = 1, 10
    ...                 A(i, j) = 0
    ...             end do
    ...         end do
    ...         do i = 1, 10
    ...             do j = 1, 10
    ...                 A(i, j) = 0
    ...             end do
    ...         end do
    ...     end subroutine
    ...     """
    >>> routine.walk(Routine)
    >>> ompparalleltrans = OMPParallelTrans()  # Necessary in loop worksharing
    >>> omplooptrans1 = OMPLoopTrans(omp_schedule="auto")
    >>> omplooptrans2 = OMPLoopTrans(omp_worksharing=False)
    >>> omplooptrans1.apply(routine.children[0])
    >>> ompparalleltrans.apply(routine.children[0])
    >>> omplooptrans2.apply(routine.children[1])

    will generate:

    .. code-block:: fortran

        subroutine my_subroutine()
            integer, dimension(10, 10) :: A
            integer :: i
            integer :: j
            !$omp parallel
            !$omp do schedule(auto)
            do i = 1, 10
                do j = 1, 10
                    A(i, j) = 0
                end do
            end do
            !$omp end do
            !$omp end parallel
            !$omp loop
            do i = 1, 10
                do j = 1, 10
                    A(i, j) = 0
                end do
            end do
            !$omp end loop
        end subroutine

    '''
    def __init__(self, omp_schedule="static", omp_worksharing=True):
        # Whether or not to generate code for (run-to-run on n threads)
        # reproducible OpenMP reductions. This setting can be overridden
        # via the `reprod` argument to the apply() method.
        self._reprod = Config.get().reproducible_reductions

        # Declare the attributes but use the property setter for proper
        # error checking
        self._omp_worksharing = None
        self.omp_worksharing = omp_worksharing

        self._omp_schedule = ""
        self.omp_schedule = omp_schedule

        super().__init__()

    def __str__(self):
        return "Adds an 'OpenMP DO' directive to a loop"

    @property
    def omp_worksharing(self):
        '''
        :returns: the value of the omp_worksharing attribute.
        :rtype: bool
        '''
        return self._omp_worksharing

    @omp_worksharing.setter
    def omp_worksharing(self, value):
        '''
        :param bool value: new value of the omp_worksharing attribute.

        :raises TypeError: if the provided value is not a boolean.
        '''
        if not isinstance(value, bool):
            raise TypeError(
                f"The OMPLoopTrans.omp_worksharing property must be a boolean"
                f" but found a '{type(value).__name__}'.")
        self._omp_worksharing = value

    @property
    def omp_schedule(self):
        '''
        :returns: the OpenMP schedule that will be specified by \
            this transformation. The default schedule is 'static'.
        :rtype: str

        '''
        return self._omp_schedule

    @omp_schedule.setter
    def omp_schedule(self, value):
        '''
        :param str value: Sets the OpenMP schedule value that will be \
            specified by this transformation.

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
        if value_parts[0].lower() not in VALID_OMP_SCHEDULES:
            raise ValueError(f"Valid OpenMP schedules are "
                             f"{VALID_OMP_SCHEDULES} but got "
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
        '''
        Creates the type of directive needed for this sub-class of
        transformation.

        :param children: list of Nodes that will be the children of \
                         the created directive.
        :type children: list of :py:class:`psyclone.psyir.nodes.Node`
        :param int collapse: number of nested loops to collapse or None if \
                             no collapse attribute is required.

        :returns: the new node representing the directive in the AST
        :rtype: :py:class:`psyclone.psyir.nodes.OMPDoDirective` or \
                :py:class:`psyclone.psyir.nodes.OMPLoopDirective`

        '''
        if self._omp_worksharing:
            # TODO 1370: OpenMP Do Directive don't support collapse yet.
            _directive = OMPDoDirective(children=children,
                                        omp_schedule=self.omp_schedule,
                                        reprod=self._reprod)
        else:
            _directive = OMPLoopDirective(children=children,
                                          collapse=collapse)

        return _directive

    def apply(self, node, options=None):
        '''Apply the OMPLoopTrans transformation to the specified node in a
        Schedule. This node must be a Loop since this transformation
        corresponds to wrapping the generated code with directives like so:

        .. code-block:: fortran

          !$OMP DO
          do ...
             ...
          end do
          !$OMP END DO

        At code-generation time (when
        :py:meth:`OMPLoopDirective.gen_code` is called), this node must be
        within (i.e. a child of) an OpenMP PARALLEL region.

        If the keyword "reprod" is specified in the options, it will cause a
        reproducible reduction to be generated if it is set to True, otherwise
        the default value (as read from the psyclone.cfg file) will be used.
        Note, reproducible in this case means obtaining the same results
        with the same number of OpenMP threads, not for different
        numbers of OpenMP threads.

        :param node: the supplied node to which we will apply the \
                     OMPLoopTrans transformation
        :type node: :py:class:`psyclone.psyir.nodes.Node`
        :param options: a dictionary with options for transformations\
                        and validation.
        :type options: dictionary of string:values or None
        :param bool options["reprod"]:
                indicating whether reproducible reductions should be used. \
                By default the value from the config file will be used.

        '''
        if not options:
            options = {}
        self._reprod = options.get("reprod",
                                   Config.get().reproducible_reductions)

        # Add variable names for OMP functions into the InvokeSchedule
        # (a Routine) symboltable if they don't already exist
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



