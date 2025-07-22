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
# Authors R. W. Ford, A. R. Porter, S. Siso and N. Nobre, STFC Daresbury Lab
#         A. B. G. Chalk, V. K. Atkinson, STFC Daresbury Lab

'''
This module provides the implementation of OMPDeclareTargetTrans

'''

from psyclone.psyir.nodes import OMPDeclareTargetDirective
from psyclone.psyGen import Transformation, Kern
from psyclone.psyir.transformations.mark_routine_for_gpu_mixin import (
        MarkRoutineForGPUMixin)


class OMPDeclareTargetTrans(Transformation, MarkRoutineForGPUMixin):
    '''
    Adds an OpenMP declare target directive to the specified routine.

    For example:

    >>> from psyclone.psyir.frontend.fortran import FortranReader
    >>> from psyclone.psyir.nodes import Loop
    >>> from psyclone.transformations import OMPDeclareTargetTrans
    >>>
    >>> tree = FortranReader().psyir_from_source("""
    ...     subroutine my_subroutine(A)
    ...         integer, dimension(10, 10), intent(inout) :: A
    ...         integer :: i
    ...         integer :: j
    ...         do i = 1, 10
    ...             do j = 1, 10
    ...                 A(i, j) = 0
    ...             end do
    ...         end do
    ...     end subroutine
    ...     """
    >>> omptargettrans = OMPDeclareTargetTrans()
    >>> omptargettrans.apply(tree.walk(Routine)[0])

    will generate:

    .. code-block:: fortran

        subroutine my_subroutine(A)
            integer, dimension(10, 10), intent(inout) :: A
            integer :: i
            integer :: j
            !$omp declare target
            do i = 1, 10
                do j = 1, 10
                    A(i, j) = 0
                end do
            end do
        end subroutine

    '''
    def apply(self, node, options=None):
        ''' Insert an OMPDeclareTargetDirective inside the provided routine or
        associated PSyKAl kernel.

        :param node: the kernel or routine which is the target of this
            transformation.
        :type node: :py:class:`psyclone.psyir.nodes.Routine` |
                    :py:class:`psyclone.psyGen.Kern`
        :param options: a dictionary with options for transformations.
        :type options: Optional[Dict[str, Any]]
        :param bool options["force"]: whether to allow routines with
            CodeBlocks to run on the GPU.
        :param str options["device_string"]: provide a compiler-platform
                    identifier.

        '''
        self.validate(node, options)

        if isinstance(node, Kern):
            # Flag that the kernel has been modified
            node.modified = True

            # Get the schedule representing the kernel subroutine
            routines = node.get_callees()
        else:
            routines = [node]

        for routine in routines:
            if not any(isinstance(child, OMPDeclareTargetDirective) for
                       child in routine.children):
                routine.children.insert(0, OMPDeclareTargetDirective())

    def validate(self, node, options=None):
        ''' Check that an OMPDeclareTargetDirective can be inserted.

        :param node: the kernel or routine which is the target of this
            transformation.
        :type node: :py:class:`psyclone.psyGen.Kern` |
                    :py:class:`psyclone.psyir.nodes.Routine`
        :param options: a dictionary with options for transformations.
        :type options: Optional[Dict[str, Any]]
        :param bool options["force"]: whether to allow routines with
                    CodeBlocks to run on the GPU.
        :param str options["device_string"]: provide a compiler-platform
            identifier.

        :raises TransformationError: if the node is not a kernel or a routine.
        :raises TransformationError: if the target is a built-in kernel.
        :raises TransformationError: if it is a kernel but without an
                                     associated PSyIR.
        :raises TransformationError: if any of the symbols in the kernel are
                                     accessed via a module use statement.
        :raises TransformationError: if the kernel contains any calls to other
                                     routines.

        '''
        super().validate(node, options=options)

        self.validate_it_can_run_on_gpu(node, options)
