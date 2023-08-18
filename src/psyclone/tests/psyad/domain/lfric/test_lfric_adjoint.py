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

from psyclone.domain.lfric import ArgIndexToMetadataIndex
from psyclone.domain.lfric.kernel import (
    LFRicKernelMetadata, FieldArgMetadata, ScalarArgMetadata,
    OperatorArgMetadata)
from psyclone.domain.lfric.lfric_types import LFRicTypes
from psyclone.errors import InternalError, GenerationError
from psyclone.psyad.domain.lfric import generate_lfric_adjoint
from psyclone.psyad.domain.lfric.lfric_adjoint import update_access_metadata
from psyclone.psyir.symbols import (
    ScalarType, DataSymbol, ArgumentInterface, INTEGER_TYPE, REAL_TYPE)
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
    if this is a multi-precision kernel (due to issue #2236). We
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


def get_metadata_args():
    '''Utility method to create metadata and symbols for testing the
    update_access_metadata method.

    :returns: a 7-tuple containing metadata describing a kernel and 6
        kernel arguments which reflect the metadata description.
    :rtype: Tuple[
        :py:class:`psyclone.domain.lfric.kernel.LFRicKernelMetadata`,
        :py:class:`psyclone.psyir.symbols.datasymbol.DataSymbol`,
        :py:class:`psyclone.psyir.symbols.datasymbol.DataSymbol`,
        :py:class:`psyclone.psyir.symbols.datasymbol.DataSymbol`,
        :py:class:`psyclone.psyir.symbols.datasymbol.DataSymbol`,
        :py:class:`psyclone.psyir.symbols.datasymbol.DataSymbol`,
        :py:class:`psyclone.psyir.symbols.datasymbol.DataSymbol`]

    '''
    metadata = LFRicKernelMetadata(
        meta_args=[FieldArgMetadata("gh_real", "gh_write", "w3"),
                   FieldArgMetadata("gh_real", "gh_read", "w3"),
                   FieldArgMetadata("gh_real", "gh_read", "w0"),
                   ScalarArgMetadata("gh_real", "gh_read"),
                   OperatorArgMetadata("gh_real", "gh_read", "w0", "w3")],
        operates_on="cell_column",
        procedure_name="kern_code",
        name="kern_type")
    metadata.validate()
    # This can be used for any additional arguments as we don't use
    # these in our tests.
    dummy = DataSymbol("dummy", INTEGER_TYPE,
                         interface=ArgumentInterface(
                             ArgumentInterface.Access.READ))
    # It doesn't matter that field_1, field_2, field_3 and operator
    # are the wrong type as we only care about their intent.
    field_1 = DataSymbol("field_1", REAL_TYPE,
                         interface=ArgumentInterface(
                             ArgumentInterface.Access.READWRITE))
    field_2 = DataSymbol("field_2", REAL_TYPE,
                         interface=ArgumentInterface(
                             ArgumentInterface.Access.READWRITE))
    field_3 = DataSymbol("field_3", REAL_TYPE,
                         interface=ArgumentInterface(
                             ArgumentInterface.Access.READWRITE))
    scalar = DataSymbol("scalar", REAL_TYPE,
                        interface=ArgumentInterface(
                            ArgumentInterface.Access.READWRITE))
    operator = DataSymbol("operator", REAL_TYPE,
                         interface=ArgumentInterface(
                             ArgumentInterface.Access.READWRITE))
    return (metadata, dummy, field_1, field_2, field_3, scalar, operator)


def test_update_access_metadata_none():
    '''Test that the update_access_metadata method returns None and does
    not modify the metadata if the supplied symbol name is not an
    argument.

    '''
    metadata, dummy, field_1, field_2, field_3, scalar, operator = get_metadata_args()
    assert metadata.meta_args[0].access.lower() == "gh_write"
    assert metadata.meta_args[1].access.lower() == "gh_read"
    assert metadata.meta_args[2].access.lower() == "gh_read"
    assert metadata.meta_args[3].access.lower() == "gh_read"
    assert metadata.meta_args[4].access.lower() == "gh_read"
    arguments = [dummy, dummy, field_1, field_2, field_3, scalar, dummy, operator]
    access = update_access_metadata("internal", arguments, metadata)
    assert access is None
    assert metadata.meta_args[0].access.lower() == "gh_write"
    assert metadata.meta_args[1].access.lower() == "gh_read"
    assert metadata.meta_args[2].access.lower() == "gh_read"
    assert metadata.meta_args[3].access.lower() == "gh_read"
    assert metadata.meta_args[4].access.lower() == "gh_read"


def test_update_access_metadata_index_error():
    '''Test that the update_access_metadata method raises the expected
    exception if the position of the active variable does not match
    the position expected by the metadata.

    '''
    metadata, dummy, field_1, field_2, field_3, scalar, operator = get_metadata_args()
    arguments = [field_1]
    with pytest.raises(GenerationError) as info:
        _ = update_access_metadata("field_1", arguments, metadata)
    print(str(info.value))
    assert ("The argument position '0' of the active variable 'field_1' "
            "does not match any position as specified by the metadata. "
            "The expected meta_arg positions from argument positions are "
            "'{2: 0, 3: 1, 4: 2, 5: 3, 7: 4}'. The most likely reason for "
            "this is that the argument list does not conform to the LFRic "
            "rules - perhaps it is a PSyKAl-lite kernel?" in str(info.value))


def test_update_access_metadata_field_inc():
    '''Test that a continuous field with intent inout results in metadata with
    access gh_inc.

    '''
    metadata, dummy, field_1, field_2, field_3, scalar, operator = get_metadata_args()
    assert metadata.meta_args[2].access.lower() == "gh_read"
    arguments = [dummy, dummy, field_1, field_2, field_3, scalar, dummy, operator]
    access = update_access_metadata("field_3", arguments, metadata)
    assert access.lower() == "gh_inc"
    assert metadata.meta_args[2].access.lower() == access.lower()
    

def test_update_access_metadata_scalar_sum():
    '''Test that a scalar with intent inout results in metadata with
    access gh_sum.

    '''
    metadata, dummy, field_1, field_2, field_3, scalar, operator = get_metadata_args()
    assert metadata.meta_args[3].access.lower() == "gh_read"
    arguments = [dummy, dummy, field_1, field_2, field_3, scalar, dummy, operator]
    access = update_access_metadata("scalar", arguments, metadata)
    assert access.lower() == "gh_sum"
    assert metadata.meta_args[3].access.lower() == access.lower()

# ROODO discontinuous field

def test_update_access_metadata_operator_readwrite():
    '''Test that an operator with intent inout results in metadata with
    access gh_readwrite.

    '''
    metadata, dummy, field_1, field_2, field_3, scalar, operator = get_metadata_args()
    assert metadata.meta_args[4].access.lower() == "gh_read"
    arguments = [dummy, dummy, field_1, field_2, field_3, scalar, dummy, operator]
    access = update_access_metadata("operator", arguments, metadata)
    assert access.lower() == "gh_readwrite"
    assert metadata.meta_args[4].access.lower() == access.lower()


def test_update_access_metadata_metaarg(monkeypatch):
    '''Test that the update_access_metadata method raises the expected
    exception when an unsupported metaarg is found. We need to
    monkeypatch to force this exception.

    '''
    metadata, dummy, field_1, field_2, field_3, scalar, operator = get_metadata_args()
    monkeypatch.setattr(metadata._meta_args, "_meta_args_args",
                        [metadata.meta_args[0], None])
    # Avoid validity checks
    monkeypatch.setattr(ArgIndexToMetadataIndex, "mapping", lambda _: {1:0, 2:1})
    arguments = [dummy, field_1, field_2]
    with pytest.raises(InternalError) as info:
        _ = update_access_metadata("field_2", arguments, metadata)
    assert ("Found unexpected meta arg class 'NoneType'." in str(info.value))


def test_update_access_metadata_write(monkeypatch):
    '''Test that a field with intent out results in metadata with access
    gh_write. We need to monkeypatch the field to create this case.

    '''
    metadata, dummy, field_1, field_2, field_3, scalar, operator = get_metadata_args()
    assert metadata.meta_args[1].access.lower() == "gh_read"
    monkeypatch.setattr(field_2.interface, "_access", ArgumentInterface.Access.WRITE)
    arguments = [dummy, dummy, field_1, field_2, field_3, scalar, dummy, operator]
    access = update_access_metadata("field_2", arguments, metadata)
    assert access.lower() == "gh_write"
    assert metadata.meta_args[1].access.lower() == access.lower()


def test_update_access_metadata_read(monkeypatch):
    '''Test that a field with intent out results in metadata with access
    gh_read. We need to monkeypatch the field to create this case.

    '''
    metadata, dummy, field_1, field_2, field_3, scalar, operator = get_metadata_args()
    assert metadata.meta_args[0].access.lower() == "gh_write"
    monkeypatch.setattr(field_1.interface, "_access", ArgumentInterface.Access.READ)
    arguments = [dummy, dummy, field_1, field_2, field_3, scalar, dummy, operator]
    access = update_access_metadata("field_1", arguments, metadata)
    assert access.lower() == "gh_read"
    assert metadata.meta_args[1].access.lower() == access.lower()


# ROODO: unexpected access - could use readinc?

    
@pytest.mark.parametrize("meta_index,arg_name,orig_access,new_access",
                         [(0, "field_1", "gh_write", "gh_readwrite"),
                          (1, "field_2", "gh_read", "gh_readwrite"),
                          (2, "field_3", "gh_read", "gh_inc"),
                          (3, "scalar", "gh_read", "gh_sum"),
                          (4, "operator", "gh_read", "gh_readwrite")])
def test_update_access_metadata_ok(meta_index, arg_name, orig_access, new_access):
    '''Test that the update_access_metadata method works as expected when
    given valid input values.

    '''
    metadata, dummy, field_1, field_2, field_3, scalar, operator = get_metadata_args()
    arguments = [dummy, dummy, field_1, field_2, field_3, scalar, dummy, operator]

    assert metadata.meta_args[meta_index].access.lower() == orig_access
    access = update_access_metadata(arg_name, arguments, metadata)
    assert access.lower() == new_access
    assert metadata.meta_args[meta_index].access.lower() == access.lower()


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
