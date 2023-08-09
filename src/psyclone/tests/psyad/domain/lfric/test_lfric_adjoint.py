# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2022-2023, Science and Technology Facilities Council.
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
# Authors R. W. Ford and A. R. Porter, STFC Daresbury Lab

'''
A module to perform pytest tests on the
psyad/domain/lfric/lfric_adjoint.py file.

'''
import pytest

from psyclone.errors import InternalError, GenerationError
from psyclone.psyad.domain.lfric import generate_lfric_adjoint
from psyclone.psyir.transformations import TransformationError


def test_generate_lfric_adjoint_no_container_error(fortran_reader):
    '''
    Check that generate_lfric_adjoint raises the expected error when
    provided PSyIR that does not have a Container.

    '''
    psyir = fortran_reader.psyir_from_source("""\
program test
  implicit none
  integer :: var1, var2
  var1 = var2
end program test
""")
    with pytest.raises(GenerationError) as err:
        generate_lfric_adjoint(psyir, ["var1", "var2"])
    assert ("If the LFRic kernel contains a single container, it should not "
            "be a FileContainer (as that means the kernel does not contain "
            "a module)." in str(err.value))


def test_generate_lfric_adjoint_no_routines_error(fortran_reader):
    '''
    Check that generate_lfric_adjoint raises the expected error when
    provided with PSyIR that does not contain any Routines.

    '''
    psyir = fortran_reader.psyir_from_source("""\
module test_mod
  implicit none
  integer :: var1
end module test_mod
""")
    with pytest.raises(InternalError) as err:
        generate_lfric_adjoint(psyir, ["var1", "var2"])
    assert "The supplied PSyIR does not contain any routines" in str(err.value)


def test_generate_lfric_adjoint_no_metadata(fortran_reader):
    '''Check that the expected error is raised when the metadata does not
    exist or has an unexpected name.

    '''
    psyir = fortran_reader.psyir_from_source(
        "module test_mod\n"
        "  implicit none\n"
        "  use kernel_mod\n"
        "  use argument_mod\n"
        "  type, extends(kernel_type) :: wrong_name\n"
        "     type(arg_type), dimension(2) :: meta_args = (/  &\n"
        "          arg_type(gh_field,  gh_real, gh_inc, w0),  &\n"
        "          arg_type(gh_field,  gh_real, gh_read, w0)  &\n"
        "          /)\n"
        "     integer :: operates_on = cell_column\n"
        "   contains\n"
        "     procedure, nopass :: code => kern_code\n"
        "  end type wrong_name\n"
        "contains\n"
        "  subroutine test_kern()\n"
        "    integer :: var1\n"
        "    var1 = 0.0\n"
        "  end subroutine test_kern\n"
        "end module test_mod")
    with pytest.raises(TransformationError) as err:
        generate_lfric_adjoint(psyir, ["var1"])
    assert ("The metadata name 'test_type' provided to the transformation "
            "does not correspond to a symbol in the supplied PSyIR."
            in str(err.value))


def test_generate_lfric_adjoint_multi_precision(fortran_reader, fortran_writer):
    '''Check that generate_lfric_adjoint makes no changes to the metadata
    if the this is a multi-precision kernel (due to issue #2236). We
    can't yet parse multi-precision metadata (again due to issue
    #2236) so modify the PSyIR directly (removing the procedure part
    of the metadata).

    '''
    tl_fortran_str = (
        "module test_mod\n"
        "  use kernel_mod\n"
        "  use argument_mod\n"
        "  type, extends(kernel_type) :: test_type\n"
        "     type(arg_type), dimension(2) :: meta_args = (/  &\n"
        "          arg_type(gh_field,  gh_real, gh_inc, w0),  &\n"
        "          arg_type(gh_field,  gh_real, gh_read, w0)  &\n"
        "          /)\n"
        "     integer :: operates_on = cell_column\n"
        "     contains\n"
        "     procedure, nopass :: kern_code\n"
        "  end type test_type\n"
        "contains\n"
        "  SUBROUTINE kern_code(nlayers, field_1_w0, field_2_w0, ndf_w0, "
        "undf_w0, map_w0)\n"
        "    USE constants_mod\n"
        "    IMPLICIT NONE\n"
        "    INTEGER(KIND=i_def), intent(in) :: nlayers\n"
        "    INTEGER(KIND=i_def), intent(in) :: ndf_w0\n"
        "    INTEGER(KIND=i_def), intent(in), dimension(ndf_w0) :: map_w0\n"
        "    INTEGER(KIND=i_def), intent(in) :: undf_w0\n"
        "    REAL(KIND=r_def), intent(inout), dimension(undf_w0) :: "
        "field_1_w0\n"
        "    REAL(KIND=r_def), intent(in), dimension(undf_w0) :: field_2_w0\n"
        "    field_1_w0(:) = field_1_w0(:)*2.0_r_def + field_2_w0(:)\n"
        "  END SUBROUTINE kern_code\n"
        "end module test_mod")
    psyir = fortran_reader.psyir_from_source(tl_fortran_str)
    sym_table = psyir.children[0].symbol_table
    test_type_symbol = sym_table.lookup("test_type")
    datatype = test_type_symbol.datatype
    # Remove procedure metadata
    new_declaration = (datatype.declaration.
                       replace("PROCEDURE, NOPASS :: kern_code", "").
                       replace("CONTAINS", ""))
    datatype._declaration = new_declaration
    ad_psyir = generate_lfric_adjoint(psyir, ["field_1_w0", "field_2_w0"])
    result = fortran_writer(ad_psyir)
    # Check that the metadata intents (gh_inc etc.) do not change.
    assert (
        "  type(ARG_TYPE) :: META_ARGS(2) = (/ &\n"
        "    arg_type(gh_field, gh_real, gh_inc, w0), &\n"
        "    arg_type(gh_field, gh_real, gh_read, w0)/)\n" in result)


def test_generate_lfric_adjoint_local_active(fortran_reader, fortran_writer):
    '''Check that changes to the kernel metadata, when translating from
    tangent-linear to adjoint, work correctly with a mixture of active
    variables that are locally declared (so have no associated
    metadata) and ones that are passed by argument (so do have
    associated metadata).

    '''
    tl_fortran_str = (
        "module test_mod\n"
        "  use kernel_mod\n"
        "  use argument_mod\n"
        "  type, extends(kernel_type) :: test_type\n"
        "     type(arg_type), dimension(2) :: meta_args = (/  &\n"
        "          arg_type(gh_field,  gh_real, gh_inc, w0),  &\n"
        "          arg_type(gh_field,  gh_real, gh_read, w0)  &\n"
        "          /)\n"
        "     integer :: operates_on = cell_column\n"
        "   contains\n"
        "     procedure, nopass :: code => kern_code\n"
        "  end type test_type\n"
        "contains\n"
        "  SUBROUTINE kern_code(nlayers, field_1_w0, field_2_w0, ndf_w0, "
        "undf_w0, map_w0)\n"
        "    USE constants_mod\n"
        "    IMPLICIT NONE\n"
        "    INTEGER(KIND=i_def), intent(in) :: nlayers\n"
        "    INTEGER(KIND=i_def), intent(in) :: ndf_w0\n"
        "    INTEGER(KIND=i_def), intent(in), dimension(ndf_w0) :: map_w0\n"
        "    INTEGER(KIND=i_def), intent(in) :: undf_w0\n"
        "    REAL(KIND=r_def), intent(inout), dimension(undf_w0) :: "
        "field_1_w0\n"
        "    REAL(KIND=r_def), intent(in), dimension(undf_w0) :: field_2_w0\n"
        "    REAL(KIND=r_def), dimension(undf_w0) :: field_3\n"
        "    field_1_w0(:) = field_1_w0(:)*2.0_r_def + field_2_w0(:)\n"
        "    field_3(:) = field_2_w0(:) + field_1_w0(:)\n"
        "  END SUBROUTINE kern_code\n"
        "end module test_mod")

    psyir = fortran_reader.psyir_from_source(tl_fortran_str)
    ad_psyir = generate_lfric_adjoint(psyir, [
        "field_1_w0", "field_2_w0", "field_3"])
    ad_fortran_str = fortran_writer(ad_psyir)

    expected_adjoint_code = (
        "    field_3 = 0.0_r_def\n"
        "    field_2_w0(:) = field_2_w0(:) + field_3(:)\n"
        "    field_1_w0(:) = field_1_w0(:) + field_3(:)\n"
        "    field_3(:) = 0.0\n"
        "    field_2_w0(:) = field_2_w0(:) + field_1_w0(:)\n"
        "    field_1_w0(:) = field_1_w0(:) * 2.0_r_def\n")
    assert expected_adjoint_code in ad_fortran_str
    expected_meta_args = (
        "  type(ARG_TYPE) :: META_ARGS(2) = (/ &\n"
        "    arg_type(gh_field, gh_real, gh_inc, w0), &\n"
        "    arg_type(gh_field, gh_real, gh_inc, w0)/)\n")
    assert expected_meta_args in ad_fortran_str


def test_generate_lfric_adjoint_unmatched(fortran_reader):
    '''Check that an exception is raised if the actual arguments do not
    match those expected by the metadata. This is a problem as we need
    to associate the metadata with the arguments so we can update the
    metadata intents. Arguments may not match the metadata if there is
    a coding error or if the code is PSyKAl-lite. In this example we
    remove the 'nlayers' argument from the kernel code.

    '''
    tl_fortran_str = (
        "module test_mod\n"
        "  use kernel_mod\n"
        "  use argument_mod\n"
        "  type, extends(kernel_type) :: test_type\n"
        "     type(arg_type), dimension(2) :: meta_args = (/  &\n"
        "          arg_type(gh_field,  gh_real, gh_inc, w0),  &\n"
        "          arg_type(gh_field,  gh_real, gh_read, w0)  &\n"
        "          /)\n"
        "     integer :: operates_on = cell_column\n"
        "   contains\n"
        "     procedure, nopass :: code => kern_code\n"
        "  end type test_type\n"
        "contains\n"
        "  SUBROUTINE kern_code(field_1_w0, field_2_w0, ndf_w0, "
        "undf_w0, map_w0)\n"
        "    USE constants_mod\n"
        "    IMPLICIT NONE\n"
        "    INTEGER(KIND=i_def), intent(in) :: ndf_w0\n"
        "    INTEGER(KIND=i_def), intent(in), dimension(ndf_w0) :: map_w0\n"
        "    INTEGER(KIND=i_def), intent(in) :: undf_w0\n"
        "    REAL(KIND=r_def), intent(inout), dimension(undf_w0) :: "
        "field_1_w0\n"
        "    REAL(KIND=r_def), intent(in), dimension(undf_w0) :: field_2_w0\n"
        "    field_1_w0(:) = field_1_w0(:)*2.0_r_def + field_2_w0(:)\n"
        "  END SUBROUTINE kern_code\n"
        "end module test_mod")

    psyir = fortran_reader.psyir_from_source(tl_fortran_str)
    with pytest.raises(GenerationError) as info:
        _ = generate_lfric_adjoint(psyir, ["field_1_w0", "field_2_w0"])
    assert ("The argument position '0' of the active variable 'field_1_w0' "
            "does not match any position as specified by the metadata. "
            "The expected meta_arg positions from argument positions are "
            "'{1: 0, 2: 1}'. The most likely reason for this is that the "
            "argument list does not conform to the LFRic rules - perhaps "
            "it is a PSyKAl-lite kernel?" in str(info.value))


def test_generate_lfric_adjoint_unmatched(fortran_reader, fortran_writer):
    '''Check that gh_sum metadata can be created for a scalar,
    gh_readwrite metadata can be created for an operator or a
    discontinuous field and that gh_inc metadata can be created for a
    continuous field.

    '''
    tl_fortran_str = (
        "module test_mod\n"
        "  use kernel_mod\n"
        "  use argument_mod\n"
        "  implicit none\n"
        "  type, extends(kernel_type), public :: test_type\n"
        "  TYPE(arg_type), DIMENSION(5) :: meta_args = (/arg_type(gh_field, gh_real, gh_inc, w0), arg_type(gh_field, gh_real, gh_read, w3), arg_type(gh_scalar, gh_real, gh_read), arg_type(gh_field, gh_real, gh_read, w0), arg_type(gh_operator, gh_real, gh_read, w0, w3)/)\n"
        "  INTEGER :: operates_on = cell_column\n"
        "  CONTAINS\n"
        "  PROCEDURE, NOPASS :: code => kern_code\n"
        "END TYPE test_type\n"
        "  public\n"
        "  contains\n"
        "    SUBROUTINE kern_code(cell, nlayers, field_1_w0, field_2_w3, rscalar_3, field_4_w0, op_5_ncell_3d, op_5, ndf_w0, undf_w0, map_w0, ndf_w3, undf_w3, map_w3)\n"
        "      USE constants_mod\n"
        "      IMPLICIT NONE\n"
        "      INTEGER(KIND=i_def), intent(in) :: nlayers\n"
        "      INTEGER(KIND=i_def), intent(in) :: ndf_w0\n"
        "      INTEGER(KIND=i_def), intent(in), dimension(ndf_w0) :: map_w0\n"
        "      INTEGER(KIND=i_def), intent(in) :: ndf_w3\n"
        "      INTEGER(KIND=i_def), intent(in), dimension(ndf_w3) :: map_w3\n"
        "      INTEGER(KIND=i_def), intent(in) :: undf_w0, undf_w3\n"
        "      REAL(KIND=r_def), intent(in) :: rscalar_3\n"
        "      REAL(KIND=r_def), intent(inout), dimension(undf_w0) :: field_1_w0\n"
        "      REAL(KIND=r_def), intent(in), dimension(undf_w3) :: field_2_w3\n"
        "      REAL(KIND=r_def), intent(in), dimension(undf_w0) :: field_4_w0\n"
        "      INTEGER(KIND=i_def), intent(in) :: cell\n"
        "      INTEGER(KIND=i_def), intent(in) :: op_5_ncell_3d\n"
        "      REAL(KIND=r_def), intent(in), dimension(ndf_w0,ndf_w3,op_5_ncell_3d) :: op_5\n"
        "      field_1_w0(:)=field_2_w3(:)+rscalar_3+field_4_w0(:)+op_5(:,1,1)\n"
        "    END SUBROUTINE kern_code\n"
        "end module test_mod")

    psyir = fortran_reader.psyir_from_source(tl_fortran_str)
    ad_psyir = generate_lfric_adjoint(psyir, ["field_1_w0", "field_2_w3", "rscalar_3", "field_4_w0", "op_5"])
    ad_fortran_str = fortran_writer(ad_psyir)
    assert (
        "  type(ARG_TYPE) :: META_ARGS(5) = (/ &\n"
        "    arg_type(gh_field, gh_real, gh_inc, w0), &\n"
        "    arg_type(gh_field, gh_real, gh_readwrite, w3), &\n"
        "    arg_type(gh_scalar, gh_real, gh_sum), &\n"
        "    arg_type(gh_field, gh_real, gh_inc, w0), &\n"
        "    arg_type(gh_operator, gh_real, gh_readwrite, w0, w3)/)\n"
        in ad_fortran_str)


# not implemented exception

# Above also check for access names being added
# Check access name imported from argument_mod?
# Raise error if argument_mod does not exist
# Works if generic import for argument_mod?

    
def test_generate_lfric_adjoint_multi_kernel(fortran_reader, fortran_writer):
    '''Check that generate_lfric_adjoint creates the expected code when
    there are multiple kernels in a module. All kernel subroutine
    names should be renamed.

    '''
    tl_fortran_str = (
        "module test_mod\n"
        "  use kernel_mod\n"
        "  use argument_mod\n"
        "  type, extends(kernel_type) :: test_type\n"
        "     type(arg_type), dimension(2) :: meta_args = (/  &\n"
        "          arg_type(gh_field,  gh_real, gh_inc, w0),  &\n"
        "          arg_type(gh_field,  gh_real, gh_read, w0)  &\n"
        "          /)\n"
        "     integer :: operates_on = cell_column\n"
        "   contains\n"
        "     procedure, nopass :: code => kern1_code\n"
        "  end type test_type\n"
        "contains\n"
        "  SUBROUTINE kern1_code(nlayers, field_1_w0, field_2_w0, ndf_w0, "
        "undf_w0, map_w0)\n"
        "    USE constants_mod\n"
        "    IMPLICIT NONE\n"
        "    INTEGER(KIND=i_def), intent(in) :: nlayers\n"
        "    INTEGER(KIND=i_def), intent(in) :: ndf_w0\n"
        "    INTEGER(KIND=i_def), intent(in), dimension(ndf_w0) :: map_w0\n"
        "    INTEGER(KIND=i_def), intent(in) :: undf_w0\n"
        "    REAL(KIND=r_def), intent(inout), dimension(undf_w0) :: "
        "field_1_w0\n"
        "    REAL(KIND=r_def), intent(in), dimension(undf_w0) :: field_2_w0\n"
        "    field_1_w0(:) = field_1_w0(:)*2.0_r_def + field_2_w0(:)\n"
        "  END SUBROUTINE kern1_code\n"
        "  SUBROUTINE kern2_code(nlayers, field_1_w0, field_2_w0, ndf_w0, "
        "undf_w0, map_w0)\n"
        "    USE constants_mod\n"
        "    IMPLICIT NONE\n"
        "    INTEGER(KIND=i_def), intent(in) :: nlayers\n"
        "    INTEGER(KIND=i_def), intent(in) :: ndf_w0\n"
        "    INTEGER(KIND=i_def), intent(in), dimension(ndf_w0) :: map_w0\n"
        "    INTEGER(KIND=i_def), intent(in) :: undf_w0\n"
        "    REAL(KIND=r_solver), intent(inout), dimension(undf_w0) :: "
        "field_1_w0\n"
        "    REAL(KIND=r_solver), intent(in), dimension(undf_w0) :: "
        "field_2_w0\n"
        "    field_1_w0(:) = field_1_w0(:)*2.0_r_solver + field_2_w0(:)\n"
        "  END SUBROUTINE kern2_code\n"
        "end module test_mod")

    expected = (
        "module adj_test_mod\n"
        "  use kernel_mod\n"
        "  use argument_mod\n"
        "  implicit none\n"
        "  type, public, extends(kernel_type) :: adj_test_type\n"
        "  type(ARG_TYPE) :: META_ARGS(2) = (/ &\n"
        "    arg_type(gh_field, gh_real, gh_inc, w0), &\n"
        "    arg_type(gh_field, gh_real, gh_inc, w0)/)\n"
        "  INTEGER :: OPERATES_ON = cell_column\n"
        "  CONTAINS\n"
        "    PROCEDURE, NOPASS :: adj_kern1_code\n"
        "END TYPE adj_test_type\n\n"
        "  public\n\n"
        "  contains\n"
        "  subroutine adj_kern1_code(nlayers, field_1_w0, field_2_w0, "
        "ndf_w0, undf_w0, map_w0)\n"
        "    use constants_mod\n"
        "    integer(kind=i_def), intent(in) :: nlayers\n"
        "    integer(kind=i_def), intent(in) :: ndf_w0\n"
        "    integer(kind=i_def), dimension(ndf_w0), intent(in) :: map_w0\n"
        "    integer(kind=i_def), intent(in) :: undf_w0\n"
        "    real(kind=r_def), dimension(undf_w0), intent(inout) :: "
        "field_1_w0\n"
        "    real(kind=r_def), dimension(undf_w0), intent(inout) :: "
        "field_2_w0\n\n"
        "    field_2_w0(:) = field_2_w0(:) + field_1_w0(:)\n"
        "    field_1_w0(:) = field_1_w0(:) * 2.0_r_def\n\n"
        "  end subroutine adj_kern1_code\n"
        "  subroutine adj_kern2_code(nlayers, field_1_w0, field_2_w0, "
        "ndf_w0, undf_w0, map_w0)\n"
        "    use constants_mod\n"
        "    integer(kind=i_def), intent(in) :: nlayers\n"
        "    integer(kind=i_def), intent(in) :: ndf_w0\n"
        "    integer(kind=i_def), dimension(ndf_w0), intent(in) :: map_w0\n"
        "    integer(kind=i_def), intent(in) :: undf_w0\n"
        "    real(kind=r_solver), dimension(undf_w0), intent(inout) :: "
        "field_1_w0\n"
        "    real(kind=r_solver), dimension(undf_w0), intent(inout) :: "
        "field_2_w0\n\n"
        "    field_2_w0(:) = field_2_w0(:) + field_1_w0(:)\n"
        "    field_1_w0(:) = field_1_w0(:) * 2.0_r_solver\n\n"
        "  end subroutine adj_kern2_code\n\n"
        "end module adj_test_mod\n")

    psyir = fortran_reader.psyir_from_source(tl_fortran_str)
    ad_psyir = generate_lfric_adjoint(psyir, ["field_1_w0", "field_2_w0"])
    ad_fortran_str = fortran_writer(ad_psyir)
    assert ad_fortran_str == expected
