# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2023, Science and Technology Facilities Council.
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
# Author: R. W. Ford, STFC Daresbury Lab

'''This module implements tests for the check_kernel_args function; a
function that checks that LFRic kernel arguments are consistent with
their metadata.

'''
import pytest

from psyclone.domain.lfric import check_kernel_args
from psyclone.domain.lfric.transformations import RaisePSyIR2LFRicKernTrans
from psyclone.errors import GenerationError


KERNEL_CODE = '''
module dg_matrix_vector_kernel_mod
  use constants_mod,     only : r_def, i_def
  type, public, extends(kernel_type) :: dg_matrix_vector_kernel_type
    private
    type(arg_type) :: meta_args(3) = (/                                     &
      arg_type(GH_FIELD, GH_REAL, GH_READWRITE, ANY_DISCONTINUOUS_SPACE_1), &
      arg_type(GH_FIELD,    GH_REAL, GH_READ, ANY_SPACE_1),                 &
      arg_type(GH_OPERATOR, GH_REAL, GH_READ, ANY_DISCONTINUOUS_SPACE_1,    &
                                                      ANY_SPACE_1)          &
         /)
    integer :: operates_on = CELL_COLUMN
  contains
    procedure, nopass :: dg_matrix_vector_kernel_code
  end type
  public dg_matrix_vector_kernel_code
contains
    subroutine dg_matrix_vector_kernel_code(cell, nlayers, lhs, x, ncell_3d, &
                                            matrix, ndf1, undf1, map1,       &
                                            ndf2, undf2, map2)
      implicit none
      integer(kind=i_def),                  intent(in)    :: cell, nlayers, &
                                                             ncell_3d
      integer(kind=i_def),                  intent(in)    :: undf1, ndf1
      integer(kind=i_def),                  intent(in)    :: undf2, ndf2
      integer(kind=i_def), dimension(ndf1), intent(in)    :: map1
      integer(kind=i_def), dimension(ndf2), intent(in)    :: map2
      real(kind=r_def), dimension(undf2),              intent(in)    :: x
      real(kind=r_def), dimension(undf1),              intent(inout) :: lhs
      real(kind=r_def), dimension(ndf1,ndf2,ncell_3d), intent(in)    :: matrix
    end subroutine dg_matrix_vector_kernel_code
end module dg_matrix_vector_kernel_mod
'''


def test_invalid_arg():
    '''Test that the expected exception is raised if the argument
    to check_kernel_args is the wrong type.

    '''
    with pytest.raises(TypeError) as info:
        check_kernel_args(None)
    assert("kernel argument to check_kernel_args function should be a "
           "Container but found 'NoneType'." in str(info))


def test_consistent_args(fortran_reader):
    '''Test that no exception is raised if the number of arguments
    expected by the metadata matches the number of kernel
    arguments.

    '''
    kernel_psyir = fortran_reader.psyir_from_source(KERNEL_CODE)
    kernel_trans = RaisePSyIR2LFRicKernTrans()
    kernel_trans.apply(kernel_psyir, options={"metadata_name":
                                              "dg_matrix_vector_kernel_type"})
    check_kernel_args(kernel_psyir)


def test_inconsistent_args(fortran_reader):
    '''Test that an exception is raised if the number of arguments
    expected by the metadata does not match the number of kernel
    arguments.

    '''
    broken_kernel_code = KERNEL_CODE.replace("lhs, x, ", "")
    broken_kernel_code = broken_kernel_code.replace(
        ",              intent(in)    :: x", " :: x")
    broken_kernel_code = broken_kernel_code.replace(
        ",              intent(inout) :: lhs", " :: lhs")
    kernel_psyir = fortran_reader.psyir_from_source(broken_kernel_code)
    kernel_trans = RaisePSyIR2LFRicKernTrans()
    kernel_trans.apply(kernel_psyir, options={"metadata_name":
                                              "dg_matrix_vector_kernel_type"})
    with pytest.raises(GenerationError) as info:
        check_kernel_args(kernel_psyir)
    assert ("The kernel metadata in 'dg_matrix_vector_kernel_type' specifies "
            "that there should be 11 kernel arguments, but the kernel "
            "routine 'dg_matrix_vector_kernel_code' has 10."
            in str(info.value))
