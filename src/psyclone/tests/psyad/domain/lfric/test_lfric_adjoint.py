# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2022-2024, Science and Technology Facilities Council.
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
import logging

import pytest

from psyclone.domain.lfric import ArgIndexToMetadataIndex
from psyclone.domain.lfric.kernel import (
    LFRicKernelMetadata, FieldArgMetadata, ScalarArgMetadata,
    OperatorArgMetadata)
from psyclone.errors import InternalError, GenerationError
from psyclone.psyad.domain.lfric import generate_lfric_adjoint
from psyclone.psyad.domain.lfric.lfric_adjoint import (
    _update_access_metadata, _check_or_add_access_symbol)
from psyclone.psyir.symbols import (
    DataSymbol, ArgumentInterface, INTEGER_TYPE, REAL_TYPE)
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
    assert ("If the LFRic kernel PSyIR contains a single container, it "
            "should not be a FileContainer (as that means the kernel "
            "source is not within a module)." in str(err.value))


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


# We can't yet parse metadata that does not contain a procedure (see
# issue #2236) so instead we simply add multiple routines even though
# only one is specified in the metadata.
MULTI_ROUTINE_CODE = (
    "module test_mod\n"
    "  implicit none\n"
    "  use kernel_mod\n"
    "  use argument_mod\n"
    "  type, extends(kernel_type) :: test_type\n"
    "     type(arg_type), dimension(2) :: meta_args = (/  &\n"
    "          arg_type(gh_field,  gh_real, gh_inc, w0),  &\n"
    "          arg_type(gh_field,  gh_real, gh_read, w0)  &\n"
    "          /)\n"
    "     integer :: operates_on = cell_column\n"
    "  contains\n"
    "    procedure, nopass :: test_code_r4\n"
    "  end type test_type\n"
    "contains\n"
    "  subroutine test_code_r4()\n"
    "    real*4 :: var1\n"
    "    var1 = 0.0\n"
    "  end subroutine test_code_r4\n"
    "  subroutine test_code_r8()\n"
    "    real*8 :: var1\n"
    "    var1 = 0.0\n"
    "  end subroutine test_code_r8\n"
    "end module test_mod")


def test_generate_lfric_adjoint_multi_routine(fortran_reader, fortran_writer):
    '''Test that if there are multiple routines in the PSyIR (i.e. for
    multi-precision kernels), all of their (tl) names are modified to
    their expected adjoint names.

    '''
    psyir = fortran_reader.psyir_from_source(MULTI_ROUTINE_CODE)
    adj_psyir = generate_lfric_adjoint(psyir, ["var1"])
    result = fortran_writer(adj_psyir)
    assert "subroutine adj_test_code_r4()" in result
    assert "subroutine adj_test_code_r8()" in result


@pytest.mark.xfail(reason="issue #1235: caplog returns an empty string in "
                   "github actions.", strict=False)
def test_generate_lfric_adjoint_multi_routine_logging(fortran_reader, caplog):
    '''Test that renamed routines are captured in the logging output. '''
    psyir = fortran_reader.psyir_from_source(MULTI_ROUTINE_CODE)
    with caplog.at_level(logging.INFO):
        _ = generate_lfric_adjoint(psyir, ["var1"])
    assert caplog.text == ""
    with caplog.at_level(logging.DEBUG):
        _ = generate_lfric_adjoint(psyir, ["var1"])
    assert "AD LFRic kernel will be named 'adj_test_code_r4'" in caplog.text
    assert "AD LFRic kernel will be named 'adj_test_code_r8'" in caplog.text


SINGLE_ROUTINE_CODE = (
    "module test_mod\n"
    "  implicit none\n"
    "  use kernel_mod\n"
    "  use argument_mod\n"
    "  type, extends(kernel_type) :: test_type\n"
    "     type(arg_type), dimension(2) :: meta_args = (/  &\n"
    "          arg_type(gh_field,  gh_real, gh_inc, w0),  &\n"
    "          arg_type(gh_field,  gh_real, gh_read, w3)  &\n"
    "          /)\n"
    "     integer :: operates_on = cell_column\n"
    "   contains\n"
    "     procedure, nopass :: code => kern_code\n"
    "  end type test_type\n"
    "contains\n"
    "  subroutine kern_code(nlayers, var1, var2)\n"
    "    integer, intent(in) :: nlayers\n"
    "    real, intent(inout) :: var1\n"
    "    real, intent(in) :: var2\n"
    "    var1 = var2\n"
    "  end subroutine kern_code\n"
    "end module test_mod")


def test_generate_lfric_adjoint_no_metadata(fortran_reader):
    '''Check that the expected error is raised when the metadata has an
    unexpected name (i.e. does not conform to the LFRic coding
    standards).

    '''
    psyir = fortran_reader.psyir_from_source(
        SINGLE_ROUTINE_CODE.replace("test_type", "wrong_name"))
    with pytest.raises(TransformationError) as err:
        generate_lfric_adjoint(psyir, ["var1", "var2"])
    assert ("The metadata name 'test_type' provided to the transformation "
            "does not correspond to a symbol in the supplied PSyIR."
            in str(err.value))


def test_generate_lfric_adjoint_type_and_procedure_names(
        fortran_reader, fortran_writer):
    '''Test that the metadata type and procedure names are modified as
    expected.

    '''
    psyir = fortran_reader.psyir_from_source(SINGLE_ROUTINE_CODE)
    adj_psyir = generate_lfric_adjoint(psyir, ["var1", "var2"])
    result = fortran_writer(adj_psyir)
    assert "type, public, extends(kernel_type) :: adj_test_type" in result
    assert "END TYPE adj_test_type" in result
    assert "PROCEDURE, NOPASS :: adj_kern_code" in result


def test_generate_lfric_adjoint_multi_precision(
        fortran_reader, fortran_writer):
    '''Check that generate_lfric_adjoint makes no changes to the metadata
    apart from the name of the type if this is a multi-precision
    kernel (due to issue #2236). We can't yet parse multi-precision
    metadata (again due to issue #2236) so modify the PSyIR directly
    (removing the procedure part of the metadata).

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
    # Check that the metadata type name is updated.
    assert "type, public, extends(kernel_type) :: adj_test_type" in result
    assert "END TYPE adj_test_type" in result
    # Check that the metadata intents (gh_inc etc.) do not change.
    assert (
        "  type(ARG_TYPE) :: META_ARGS(2) = (/ &\n"
        "    arg_type(gh_field, gh_real, gh_inc, w0), &\n"
        "    arg_type(gh_field, gh_real, gh_read, w0)/)\n" in result)


def test_generate_lfric_adjoint_check_add_access_symbol(
        fortran_reader, fortran_writer):
    '''Check that the _check_or_add_access_symbol method is called from
    generate_lfric_adjoint. We do this by checking that the access
    declarations are updated.

    '''
    psyir = fortran_reader.psyir_from_source(
        SINGLE_ROUTINE_CODE.replace(
            "argument_mod", "argument_mod, only: gh_read, gh_inc"))
    ad_psyir = generate_lfric_adjoint(psyir, ["var1", "var2"])
    result = fortran_writer(ad_psyir)
    assert "use argument_mod, only : gh_inc, gh_read, gh_readwrite" in result


# _update_access_metadata

def get_metadata_args():
    '''Utility method to create metadata and symbols for testing the
    _update_access_metadata method.

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
    metadata, dummy, field_1, field_2, field_3, scalar, operator = \
        get_metadata_args()
    assert metadata.meta_args[0].access.lower() == "gh_write"
    assert metadata.meta_args[1].access.lower() == "gh_read"
    assert metadata.meta_args[2].access.lower() == "gh_read"
    assert metadata.meta_args[3].access.lower() == "gh_read"
    assert metadata.meta_args[4].access.lower() == "gh_read"
    arguments = [
        dummy, dummy, field_1, field_2, field_3, scalar, dummy, operator]
    access = _update_access_metadata("internal", arguments, metadata)
    assert access is None
    assert metadata.meta_args[0].access.lower() == "gh_write"
    assert metadata.meta_args[1].access.lower() == "gh_read"
    assert metadata.meta_args[2].access.lower() == "gh_read"
    assert metadata.meta_args[3].access.lower() == "gh_read"
    assert metadata.meta_args[4].access.lower() == "gh_read"


def test_update_access_metadata_index_error():
    '''Test that the _update_access_metadata method raises the expected
    exception if the position of the active variable does not match
    the position expected by the metadata.

    '''
    metadata, _1, field_1, _2, _3, _4, _6 = get_metadata_args()
    arguments = [field_1]
    with pytest.raises(GenerationError) as info:
        _ = _update_access_metadata("field_1", arguments, metadata)

    assert ("The position in the kernel subroutine argument list '0' of the "
            "active variable 'field_1' does not match any of the positions "
            "expected by the kernel argument ('meta_arg') metadata "
            "descriptions. The expected mapping of kernel subroutine argument "
            "positions to kernel meta_arg positions is '{2: 0, 3: 1, 4: 2, "
            "5: 3, 7: 4}'. The most likely reason for this is that the kernel "
            "subroutine argument list does not conform to the LFRic rules - "
            "perhaps it is a PSyKAl-lite kernel?" in str(info.value))


def test_update_access_metadata_scalar_sum():
    '''Test that a scalar with intent inout results in metadata with
    access gh_sum.

    TODO #2333 - only Builtin kernels are permitted to write to scalars so
    really we need to check with the user or flag it as an error.

    '''
    metadata, dummy, field_1, field_2, field_3, scalar, operator = \
        get_metadata_args()
    assert metadata.meta_args[3].access.lower() == "gh_read"
    arguments = [
        dummy, dummy, field_1, field_2, field_3, scalar, dummy, operator]
    access = _update_access_metadata("scalar", arguments, metadata)
    assert access.lower() == "gh_sum"
    assert metadata.meta_args[3].access.lower() == access.lower()


def test_update_access_metadata_operator_readwrite():
    '''Test that an operator with intent inout results in metadata with
    access gh_readwrite.

    '''
    metadata, dummy, field_1, field_2, field_3, scalar, operator = \
        get_metadata_args()
    assert metadata.meta_args[4].access.lower() == "gh_read"
    arguments = [
        dummy, dummy, field_1, field_2, field_3, scalar, dummy, operator]
    access = _update_access_metadata("operator", arguments, metadata)
    assert access.lower() == "gh_readwrite"
    assert metadata.meta_args[4].access.lower() == access.lower()


def test_update_access_metadata_inc():
    '''Test that a discontinuous field with intent inout results in
    metadata with access gh_readwrite.

    '''
    metadata, dummy, field_1, field_2, field_3, scalar, operator = \
        get_metadata_args()
    assert metadata.meta_args[1].access.lower() == "gh_read"
    arguments = [
        dummy, dummy, field_1, field_2, field_3, scalar, dummy, operator]
    access = _update_access_metadata("field_2", arguments, metadata)
    assert access.lower() == "gh_readwrite"
    assert metadata.meta_args[1].access.lower() == access.lower()


def test_update_access_metadata_field_inc():
    '''Test that a continuous field with intent inout results in metadata with
    access gh_inc.

    '''
    metadata, dummy, field_1, field_2, field_3, scalar, operator = \
        get_metadata_args()
    assert metadata.meta_args[2].access.lower() == "gh_read"
    arguments = [
        dummy, dummy, field_1, field_2, field_3, scalar, dummy, operator]
    access = _update_access_metadata("field_3", arguments, metadata)
    assert access.lower() == "gh_inc"
    assert metadata.meta_args[2].access.lower() == access.lower()


def test_update_access_metadata_metaarg(monkeypatch):
    '''Test that the _update_access_metadata method raises the expected
    exception when an unsupported metaarg is found. We need to
    monkeypatch to force this exception.

    '''
    metadata, dummy, field_1, field_2, _1, _2, _3 = \
        get_metadata_args()
    monkeypatch.setattr(metadata._meta_args, "_meta_args_args",
                        [metadata.meta_args[0], None])
    # Avoid validity checks
    monkeypatch.setattr(
        ArgIndexToMetadataIndex, "mapping", lambda _: {1: 0, 2: 1})
    arguments = [dummy, field_1, field_2]
    with pytest.raises(InternalError) as info:
        _ = _update_access_metadata("field_2", arguments, metadata)
    assert "Found unexpected meta arg class 'NoneType'." in str(info.value)


def test_update_access_metadata_write(monkeypatch):
    '''Test that a field with intent out results in metadata with access
    gh_write. We monkeypatch the field to create this case.

    '''
    metadata, dummy, field_1, field_2, field_3, scalar, operator = \
        get_metadata_args()
    assert metadata.meta_args[1].access.lower() == "gh_read"
    monkeypatch.setattr(
        field_2.interface, "_access", ArgumentInterface.Access.WRITE)
    arguments = [
        dummy, dummy, field_1, field_2, field_3, scalar, dummy, operator]
    access = _update_access_metadata("field_2", arguments, metadata)
    assert access.lower() == "gh_write"
    assert metadata.meta_args[1].access.lower() == access.lower()


def test_update_access_metadata_read(monkeypatch):
    '''Test that a field which specifies gh_write in its tangent linear
    metadata, but the adjoint code specifies that it is now intent in,
    results in metadata with access gh_read. We monkeypatch the field
    to create this case.

    '''
    metadata, dummy, field_1, field_2, field_3, scalar, operator = \
        get_metadata_args()
    assert metadata.meta_args[0].access.lower() == "gh_write"
    monkeypatch.setattr(
        field_1.interface, "_access", ArgumentInterface.Access.READ)
    arguments = [
        dummy, dummy, field_1, field_2, field_3, scalar, dummy, operator]
    access = _update_access_metadata("field_1", arguments, metadata)
    assert access.lower() == "gh_read"
    assert metadata.meta_args[1].access.lower() == access.lower()


def test_update_access_metadata_unexpected(monkeypatch):
    '''Test that a field with an unexpected metadata access type raises
    the expected exception. Monkeypatch with Access.UNKNOWN.

    '''
    metadata, dummy, field_1, field_2, field_3, scalar, operator = \
        get_metadata_args()
    assert metadata.meta_args[0].access.lower() == "gh_write"
    monkeypatch.setattr(
        field_1.interface, "_access", ArgumentInterface.Access.UNKNOWN)
    arguments = [
        dummy, dummy, field_1, field_2, field_3, scalar, dummy, operator]
    with pytest.raises(InternalError) as info:
        _ = _update_access_metadata("field_1", arguments, metadata)
    assert ("Found unexpected access 'Access.UNKNOWN' for 'field_1'."
            in str(info.value))


# _check_or_add_access_symbol

def test_check_or_add_access_symbol_exists(fortran_reader, fortran_writer):
    '''Test that nothing changes if the access type already exists
    explicitly in the code and is imported from argument_mod.

    '''
    adj_code = (
        "module test_mod\n"
        "  use argument_mod, only : gh_readwrite\n"
        "  implicit none\n"
        "  public\n\n"
        "  contains\n\n"
        "end module test_mod\n")
    psyir = fortran_reader.psyir_from_source(adj_code)
    _check_or_add_access_symbol(psyir, "gh_readwrite")
    result = fortran_writer(psyir)
    assert result == adj_code


def test_check_or_add_access_symbol_no_import(fortran_reader):
    '''Test that the expected exeption is raised if the access type
    already exists explicitly in the code but is not imported.

    '''
    adj_code = (
        "module test_mod\n"
        "  integer :: gh_readwrite\n"
        "  implicit none\n"
        "  public\n\n"
        "  contains\n\n"
        "end module test_mod\n")
    psyir = fortran_reader.psyir_from_source(adj_code)
    with pytest.raises(GenerationError) as info:
        _check_or_add_access_symbol(psyir, "gh_readwrite")
    assert ("The existing symbol 'gh_readwrite' is not imported from a use "
            "statement." in str(info.value))


def test_check_or_add_access_symbol_argument_mod(fortran_reader):
    '''Test that an exception is raised if the access type already exists
    explicitly in the code but is imported from a module other than
    argument_mod.

    '''
    adj_code = (
        "module test_mod\n"
        "  use another_mod, only : gh_readwrite\n"
        "  implicit none\n"
        "  public\n\n"
        "  contains\n\n"
        "end module test_mod\n")
    psyir = fortran_reader.psyir_from_source(adj_code)
    with pytest.raises(GenerationError) as info:
        _check_or_add_access_symbol(psyir, "gh_readwrite")
    assert ("The existing symbol 'gh_readwrite' is imported from "
            "'another_mod' but should be imported from 'argument_mod'."
            in str(info.value))


def test_check_or_add_access_symbol_argument_mod_wildcard(
        fortran_reader, fortran_writer):
    '''Test that there is no change in the code if argument_mod is
    imported as a wildcard import.

    '''
    adj_code = (
        "module test_mod\n"
        "  use argument_mod\n"
        "  implicit none\n"
        "  public\n\n"
        "  contains\n\n"
        "end module test_mod\n")
    psyir = fortran_reader.psyir_from_source(adj_code)
    _check_or_add_access_symbol(psyir, "gh_readwrite")
    result = fortran_writer(psyir)
    assert result == adj_code


def test_check_or_add_access_symbol_argument_mod_argument_mod_exists(
        fortran_reader, fortran_writer):
    '''Test that that if if argument_mod already imports something else
    then the access type is also added.

    '''
    adj_code = (
        "module test_mod\n"
        "  use argument_mod, only : gh_read\n"
        "end module test_mod\n")
    psyir = fortran_reader.psyir_from_source(adj_code)
    _check_or_add_access_symbol(psyir, "gh_readwrite")
    result = fortran_writer(psyir)
    assert "use argument_mod, only : gh_read, gh_readwrite" in result


def test_check_or_add_access_symbol_argument_mod_argument_mod_not_exist(
        fortran_reader, fortran_writer):
    '''Test that that if if argument_mod does not exist then it is added
    as well as the access type.

    '''
    adj_code = (
        "module test_mod\n"
        "end module test_mod\n")
    psyir = fortran_reader.psyir_from_source(adj_code)
    _check_or_add_access_symbol(psyir, "gh_readwrite")
    result = fortran_writer(psyir)
    assert "use argument_mod, only : gh_readwrite" in result
