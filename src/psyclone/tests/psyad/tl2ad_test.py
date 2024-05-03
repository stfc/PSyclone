# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2021-2024, Science and Technology Facilities Council.
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
# Authors R. W. Ford, A. R. Porter and S. Siso, STFC Daresbury Lab

'''A module to perform pytest tests on the code in the tl2ad.py file
within the psyad directory.

'''
import logging
import os
import pytest

from psyclone.errors import InternalError
from psyclone.psyad import (
    generate_adjoint_str, generate_adjoint, generate_adjoint_test)
from psyclone.psyad.tl2ad import (
    _create_inner_product, _create_array_inner_product,
    _get_active_variables_datatype, _add_precision_symbol)
from psyclone.psyir.nodes import (
    Container, FileContainer, Return, Routine, Assignment, BinaryOperation,
    IntrinsicCall, Literal)
from psyclone.psyir.symbols import (
    DataSymbol, SymbolTable, REAL_DOUBLE_TYPE, INTEGER_TYPE, REAL_TYPE,
    ArrayType, RoutineSymbol, ImportInterface, ScalarType, ContainerSymbol,
    ArgumentInterface, UnsupportedFortranType, UnresolvedType)
from psyclone.tests.utilities import Compile


TESTS_DIR = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
LFRIC_TEST_FILES_DIR = os.path.join(TESTS_DIR, "test_files", "dynamo0p3")


TL_CODE = (
    "module my_mod\n"
    "  contains\n"
    "  subroutine kern(field)\n"
    "    real, intent(inout) :: field\n"
    "    field = 0.0\n"
    "  end subroutine kern\n"
    "end module my_mod\n"
)

# generate_adjoint_str function


def test_generate_adjoint_str(caplog, tmpdir):
    '''Test that the generate_adjoint_str() function works as expected
    including logging.

    '''
    tl_code = (
        "program test\n"
        "integer :: a,b\n"
        "a = b\n"
        "end program test\n")
    expected = (
        "program adj_test\n"
        "  integer :: a\n"
        "  integer :: b\n\n"
        "  a = 0\n"
        "  b = 0\n"
        "  b = b + a\n"
        "  a = 0.0\n\n"
        "end program adj_test\n")

    with caplog.at_level(logging.INFO):
        result, test_harness = generate_adjoint_str(tl_code, ["a", "b"])

    assert caplog.text == ""
    assert expected in result
    assert test_harness == ""

    with caplog.at_level(logging.DEBUG):
        result, test_harness = generate_adjoint_str(tl_code, ["a", "b"])

    if not caplog.text:
        pytest.xfail(reason="#1235: caplog returns an empty string in "
                     "github actions.")
    else:
        assert tl_code in caplog.text
        assert ("PSyIR\n"
                "FileContainer[]\n"
                "    Routine[name:'test']\n"
                "        0: Assignment[]\n"
                "            Reference[name:'a']\n"
                "            Reference[name:'b']\n" in caplog.text)
        assert "Preprocessing\n" in caplog.text
        assert ("PSyIR after TL preprocessing\n"
                "FileContainer[]\n"
                "    Routine[name:'test']\n"
                "        0: Assignment[]\n"
                "            Reference[name:'a']\n"
                "            Reference[name:'b']\n" in caplog.text)
        assert "Translating from LFRic TL to AD." in caplog.text
    assert expected in result
    assert test_harness == ""
    assert Compile(tmpdir).string_compiles(result)


def test_generate_adjoint_str_lfric_api():
    '''
    Check that specifying the LFRic (dynamo0p3) API to the generate_adjoint_str
    routine works as expected.

    '''
    testkern = os.path.join(LFRIC_TEST_FILES_DIR, "tl_testkern_mod.F90")
    with open(testkern, mode="r", encoding="utf-8") as kfile:
        tl_code = kfile.read()
    result, _ = generate_adjoint_str(tl_code,
                                     ["xi", "u", "res_dot_product", "curl_u"],
                                     api="dynamo0.3")
    assert "subroutine adj_testkern_code" in result.lower()


def test_generate_adjoint_str_function():
    '''Test that an exception is raised if a function is found.'''
    tl_code = (
        "real function test(a)\n"
        "  real :: a\n"
        "  test = a\n"
        "end function test\n")
    with pytest.raises(NotImplementedError) as info:
        _, _ = generate_adjoint_str(tl_code, ["a", "test"])
    assert ("PSyAD does not support tangent-linear code written as a "
            "function. Please re-write 'test' as a subroutine."
            in str(info.value))


def test_generate_adjoint_str_wrong_api():
    '''Test that an exception is raised for an unsupported API.'''
    tl_code = (
        "program test\n"
        "integer :: a,b\n"
        "a = b\n"
        "end program test\n")
    with pytest.raises(NotImplementedError) as err:
        generate_adjoint_str(tl_code, ["a", "b"], api="gocean1.0")
    assert ("PSyAD only supports generic routines/programs or LFRic "
            "(dynamo0.3) kernels but got API 'gocean1.0'" in str(err.value))


def test_generate_adjoint_str_trans(tmpdir):
    '''Test that the generate_adjoint_str() function successfully calls
    the preprocess_trans() function.

    '''
    tl_code = (
        "program test\n"
        "real :: a, b(10), c(10)\n"
        "a = dot_product(b(:), c(:))\n"
        "end program test\n")
    expected = (
        "program adj_test\n"
        "  real :: a\n"
        "  real, dimension(10) :: b\n"
        "  real, dimension(10) :: c\n"
        "  integer :: i\n"
        "  real :: res_dot_product\n\n"
        "  a = 0.0\n  b = 0.0\n  res_dot_product = 0.0\n"
        "  res_dot_product = res_dot_product + a\n"
        "  a = 0.0\n"
        "  do i = 10, 1, -1\n"
        "    b(i) = b(i) + res_dot_product * c(i)\n"
        "  enddo\n"
        "  res_dot_product = 0.0\n\n"
        "end program adj_test\n")
    result, test_harness = generate_adjoint_str(tl_code,
                                                ["a", "b", "res_dot_product"])
    assert expected in result
    assert not test_harness
    assert Compile(tmpdir).string_compiles(result)


def test_generate_adjoint_str_generate_harness_no_api(tmpdir):
    '''Test the create_test option to generate_adjoint_str() when no
    API is specified.'''
    result, harness = generate_adjoint_str(TL_CODE, ["field"],
                                           create_test=True)
    assert "subroutine adj_kern(field)\n" in result
    assert "program adj_test\n" in harness
    assert "! Call the tangent-linear kernel\n" in harness
    assert "end program adj_test\n" in harness
    assert Compile(tmpdir).string_compiles(result)


def test_generate_adjoint_str_generate_harness_invalid_api():
    '''Test that passing an unsupported API to generate_adjoint_str()
    raises the expected error.'''
    with pytest.raises(NotImplementedError) as err:
        _ = generate_adjoint_str(TL_CODE, ["field"], api="gocean1.0",
                                 create_test=True)
    assert ("PSyAD only supports generic routines/programs or LFRic "
            "(dynamo0.3) kernels but got API 'gocean1.0'" in str(err.value))


def test_generate_adjoint_str_generate_harness_lfric():
    '''Test the create_test option to generate_adjoint_str() when the
    LFRic (dynamo0p3) API is specified.'''
    tl_code = (
        "module testkern_mod\n"
        "  use kinds_mod, only: i_def, r_def\n"
        "  use kernel_mod, only: kernel_type\n"
        "  use argument_mod, only: arg_type, gh_field, gh_real, &\n"
        "gh_write, w3, cell_column\n"
        "  type, extends(kernel_type) :: testkern_type\n"
        "     type(arg_type), dimension(1) :: meta_args =          & \n"
        "          (/ arg_type(gh_field,  gh_real, gh_write,  w3)  & \n"
        "           /)\n"
        "     integer :: operates_on = cell_column\n"
        "   contains\n"
        "     procedure, nopass :: code => testkern_code\n"
        "  end type testkern_type\n"
        "contains\n"
        "  subroutine testkern_code(nlayers, field, ndf_w3, undf_w3, map_w3)\n"
        "    integer(kind=i_def), intent(in) :: nlayers\n"
        "    integer(kind=i_def), intent(in) :: ndf_w3, undf_w3\n"
        "    integer(kind=i_def), intent(in), dimension(ndf_w3) :: map_w3\n"
        "    real(kind=r_def), intent(inout), dimension(undf_w3) :: field\n"
        "    field = 0.0\n"
        "  end subroutine testkern_code\n"
        "end module testkern_mod\n"
    )
    result, harness = generate_adjoint_str(tl_code, ["field"],
                                           create_test=True,
                                           api="dynamo0.3")
    assert ("subroutine adj_testkern_code(nlayers, field, ndf_w3, "
            "undf_w3, map_w3)\n" in result)
    assert "module adjoint_test_mod\n" in harness
    assert "subroutine adjoint_test(mesh, chi, panel_id)" in harness
    assert "call the tangent-linear kernel.\n" in harness
    assert "end module adjoint_test_mod\n" in harness


def test_generate_adjoint_str_generate_harness_logging(caplog):
    ''' Test the create_test option to generate_adjoint_str() produces the
    expected logging output. '''
    tl_code = (
        "module my_mod\n"
        "  contains\n"
        "  subroutine kern(field, n)\n"
        "    integer, intent(in) :: n\n"
        "    real, intent(inout), dimension(n) :: field\n"
        "    field = 0.0\n"
        "  end subroutine kern\n"
        "end module my_mod\n"
    )
    with caplog.at_level(logging.INFO):
        _ = generate_adjoint_str(tl_code, ["field"], create_test=True)
    assert caplog.text == ""
    with caplog.at_level(logging.DEBUG):
        _, harness = generate_adjoint_str(tl_code, ["field"],
                                          create_test=True)
    if not caplog.text:
        pytest.xfail(reason="#1235: caplog returns an empty string in "
                     "github actions.")
    else:
        assert ("Creating test harness for TL kernel 'kern' and AD kernel "
                "'adj_kern'" in caplog.text)
        assert ("Kernel 'kern' has the following dimensioning arguments: "
                "['n']" in caplog.text)
        assert ("Generated symbols for new argument list: ['field', 'n']" in
                caplog.text)
        assert "Created test-harness program named 'adj_test'" in caplog.text
        assert harness in caplog.text


# _get_active_variables_datatype function

def test_get_active_variables_datatype_error(fortran_reader):
    ''' Test that the _get_active_variables_datatype raises the expected
    errors if no active variables are supplied or if they are of different
    type or precision. '''
    tl_fortran_str = (
        "program test\n"
        "use kinds_mod, only: wp\n"
        "real :: a, b\n"
        "real(kind=wp) :: c\n"
        "integer :: idx\n"
        "a = b + c + idx\n"
        "end program test\n")
    prog_psyir = fortran_reader.psyir_from_source(tl_fortran_str)
    tl_psyir = prog_psyir.children[0]
    with pytest.raises(InternalError) as err:
        _get_active_variables_datatype(tl_psyir, [])
    assert "No active variables have been supplied." in str(err.value)

    with pytest.raises(NotImplementedError) as err:
        _get_active_variables_datatype(tl_psyir, ["a", "c"])
    assert ("active variables of different datatype: 'a' is of intrinsic "
            "type 'Intrinsic.REAL' and precision 'Precision.UNDEFINED' while "
            "'c' is of intrinsic type 'Intrinsic.REAL' and precision 'wp: "
            in str(err.value))

    with pytest.raises(NotImplementedError) as err:
        _get_active_variables_datatype(tl_psyir, ["a", "idx"])
    assert ("active variables of different datatype: 'a' is of intrinsic "
            "type 'Intrinsic.REAL' and precision 'Precision.UNDEFINED' while "
            "'idx' is of intrinsic type 'Intrinsic.INTEGER' and precision "
            "'Precision.UNDEFINED'" in str(err.value))


def test_get_active_variables_datatype(fortran_reader):
    ''' Test that _get_active_variables_datatype() works as expected. '''
    tl_fortran_str = (
        "program test\n"
        "use kind_mod, only: wp, i_def\n"
        "real :: a, b, c\n"
        "real(wp) :: d, e\n"
        "integer(i_def) :: ii, jj, kk\n"
        "a = b + c\n"
        "d = 2.0*e\n"
        "ii = jj + kk\n"
        "end program test\n")
    prog_psyir = fortran_reader.psyir_from_source(tl_fortran_str)
    tl_psyir = prog_psyir.children[0]
    # Real, default precision
    atype = _get_active_variables_datatype(tl_psyir, ["a", "b"])
    assert isinstance(atype, ScalarType)
    assert atype.intrinsic == ScalarType.Intrinsic.REAL
    assert atype.precision == ScalarType.Precision.UNDEFINED
    # Real, specified KIND
    atype = _get_active_variables_datatype(tl_psyir, ["d", "e"])
    assert atype.intrinsic == ScalarType.Intrinsic.REAL
    assert isinstance(atype.precision, DataSymbol)
    assert atype.precision.name == "wp"
    # Integer, specified KIND
    atype = _get_active_variables_datatype(tl_psyir, ["ii", "jj", "kk"])
    assert atype.intrinsic == ScalarType.Intrinsic.INTEGER
    assert isinstance(atype.precision, DataSymbol)
    assert atype.precision.name == "i_def"


# generate_adjoint function

def test_generate_adjoint(fortran_reader, fortran_writer, tmpdir):
    '''Test that the generate_adjoint() function works as expected.'''

    tl_fortran_str = (
        "program test\n"
        "real :: a, b, c\n"
        "a = b + c\n"
        "end program test\n")
    expected_ad_fortran_str = (
        "program adj_test\n"
        "  real :: a\n  real :: b\n  real :: c\n\n"
        "  a = 0.0\n  b = 0.0\n  c = 0.0\n"
        "  b = b + a\n"
        "  c = c + a\n"
        "  a = 0.0\n\n"
        "end program adj_test\n")
    tl_psyir = fortran_reader.psyir_from_source(tl_fortran_str)

    ad_psyir = generate_adjoint(tl_psyir, ["a", "b", "c"])

    ad_fortran_str = fortran_writer(ad_psyir)
    assert ad_fortran_str in expected_ad_fortran_str
    assert Compile(tmpdir).string_compiles(ad_fortran_str)


def test_generate_adjoint_kind(fortran_reader, fortran_writer):
    '''Test that the generate_adjoint() function works as expected when
    the active variables have a kind.'''

    tl_fortran_str = (
        "program test\n"
        "use kinds_mod, only: r_def\n"
        "real(kind=r_def) :: a, b, c\n"
        "a = b + c\n"
        "end program test\n")
    expected_ad_fortran_str = (
        "program adj_test\n"
        "  use kinds_mod, only : r_def\n"
        "  real(kind=r_def) :: a\n  real(kind=r_def) :: b\n  "
        "real(kind=r_def) :: c\n\n"
        "  a = 0.0_r_def\n  b = 0.0_r_def\n  c = 0.0_r_def\n"
        "  b = b + a\n"
        "  c = c + a\n"
        "  a = 0.0\n\n"
        "end program adj_test\n")
    tl_psyir = fortran_reader.psyir_from_source(tl_fortran_str)

    ad_psyir = generate_adjoint(tl_psyir, ["a", "b", "c"])

    ad_fortran_str = fortran_writer(ad_psyir)
    assert ad_fortran_str in expected_ad_fortran_str


def test_generate_adjoint_multi_kernel(fortran_reader, fortran_writer, tmpdir):
    '''Check that generate_adjoint works as expected when there are
    multiple kernels in a module. The adjoint of each routine should
    be taken and the names of each routine should be modified.

    '''
    tl_fortran_str = (
        "module test_mod\n"
        "  contains\n"
        "  subroutine kern1()\n"
        "    real :: psyir_tmp, psyir_tmp_1\n"
        "    psyir_tmp = psyir_tmp_1\n"
        "  end subroutine kern1\n"
        "  subroutine kern2()\n"
        "    real :: psyir_tmp, psyir_tmp_1\n"
        "    psyir_tmp = psyir_tmp_1\n"
        "  end subroutine kern2\n"
        "end module test_mod\n")
    expected_ad_fortran_str = (
        "module adj_test_mod\n"
        "  implicit none\n"
        "  public\n\n"
        "  contains\n"
        "  subroutine adj_kern1()\n"
        "    real :: psyir_tmp\n"
        "    real :: psyir_tmp_1\n\n"
        "    psyir_tmp = 0.0\n"
        "    psyir_tmp_1 = 0.0\n"
        "    psyir_tmp_1 = psyir_tmp_1 + psyir_tmp\n"
        "    psyir_tmp = 0.0\n\n"
        "  end subroutine adj_kern1\n"
        "  subroutine adj_kern2()\n"
        "    real :: psyir_tmp\n"
        "    real :: psyir_tmp_1\n\n"
        "    psyir_tmp = 0.0\n"
        "    psyir_tmp_1 = 0.0\n"
        "    psyir_tmp_1 = psyir_tmp_1 + psyir_tmp\n"
        "    psyir_tmp = 0.0\n\n"
        "  end subroutine adj_kern2\n\n"
        "end module adj_test_mod\n")
    psyir = fortran_reader.psyir_from_source(tl_fortran_str)
    ad_psyir = generate_adjoint(psyir, ["psyir_tmp", "psyir_tmp_1"])
    ad_fortran_str = fortran_writer(ad_psyir)
    assert ad_fortran_str == expected_ad_fortran_str
    assert Compile(tmpdir).string_compiles(ad_fortran_str)


def test_generate_adjoint_no_module(fortran_reader, fortran_writer, tmpdir):
    '''Check that generate_adjoint works as expected when the subroutine
    is not in a module.

    '''
    tl_fortran_str = (
        "subroutine kern2()\n"
        "  real :: psyir_tmp, psyir_tmp_1\n"
        "  psyir_tmp = psyir_tmp_1\n"
        "end subroutine kern2\n")
    expected_ad_fortran_str = (
        "subroutine adj_kern2()\n"
        "  real :: psyir_tmp\n"
        "  real :: psyir_tmp_1\n\n"
        "  psyir_tmp = 0.0\n"
        "  psyir_tmp_1 = 0.0\n"
        "  psyir_tmp_1 = psyir_tmp_1 + psyir_tmp\n"
        "  psyir_tmp = 0.0\n\n"
        "end subroutine adj_kern2\n")
    psyir = fortran_reader.psyir_from_source(tl_fortran_str)
    ad_psyir = generate_adjoint(psyir, ["psyir_tmp", "psyir_tmp_1"])
    ad_fortran_str = fortran_writer(ad_psyir)
    assert ad_fortran_str == expected_ad_fortran_str
    assert Compile(tmpdir).string_compiles(ad_fortran_str)


def test_generate_adjoint_multi_kernel_mixed(
        fortran_reader, fortran_writer, tmpdir):
    '''Check that generate_adjoint works as expected when there are
    multiple kernels and some are in a module and some are not. The
    adjoint of each routine should be taken and the names of each
    routine should be modified.

    '''
    tl_fortran_str = (
        "module test_mod\n"
        "  contains\n"
        "  subroutine kern1()\n"
        "    real :: psyir_tmp, psyir_tmp_1\n"
        "    psyir_tmp = psyir_tmp_1\n"
        "  end subroutine kern1\n"
        "end module test_mod\n"
        "subroutine kern2()\n"
        "  real :: psyir_tmp, psyir_tmp_1\n"
        "  psyir_tmp = psyir_tmp_1\n"
        "end subroutine kern2\n")
    expected_ad_fortran_str = (
        "module adj_test_mod\n"
        "  implicit none\n"
        "  public\n\n"
        "  contains\n"
        "  subroutine adj_kern1()\n"
        "    real :: psyir_tmp\n"
        "    real :: psyir_tmp_1\n\n"
        "    psyir_tmp = 0.0\n"
        "    psyir_tmp_1 = 0.0\n"
        "    psyir_tmp_1 = psyir_tmp_1 + psyir_tmp\n"
        "    psyir_tmp = 0.0\n\n"
        "  end subroutine adj_kern1\n"
        "end module adj_test_mod\n"
        "subroutine adj_kern2()\n"
        "  real :: psyir_tmp\n"
        "  real :: psyir_tmp_1\n\n"
        "  psyir_tmp = 0.0\n"
        "  psyir_tmp_1 = 0.0\n"
        "  psyir_tmp_1 = psyir_tmp_1 + psyir_tmp\n"
        "  psyir_tmp = 0.0\n\n"
        "end subroutine adj_kern2\n")
    psyir = fortran_reader.psyir_from_source(tl_fortran_str)
    with pytest.raises(KeyError):
        # There should not be a KeyError here.
        ad_psyir = generate_adjoint(psyir, ["psyir_tmp", "psyir_tmp_1"])
    pytest.xfail(reason="issue #1949: psyad does not work with a mixture "
                 "of module and non-module kernels")
    ad_fortran_str = fortran_writer(ad_psyir)
    assert ad_fortran_str == expected_ad_fortran_str
    assert Compile(tmpdir).string_compiles(ad_fortran_str)


def test_generate_adjoint_errors():
    ''' Check that generate_adjoint() raises the expected exceptions when
    given invalid input. '''
    # Only a FileContainer
    psyir = FileContainer("test_file")
    with pytest.raises(InternalError) as err:
        generate_adjoint(psyir, ["dummy"])
    assert ("The supplied PSyIR does not contain any routines." in
            str(err.value))
    with pytest.raises(InternalError) as err:
        generate_adjoint(Container.create("test_mod", SymbolTable(),
                                          [psyir.copy()]), ["dummy"])
    assert ("The supplied PSyIR contains two Containers but the innermost is "
            "a FileContainer. This should not be possible" in str(err.value))
    # No kernel code
    cont = Container("test_mod")
    with pytest.raises(InternalError) as err:
        generate_adjoint(cont, ["dummy"])
    assert ("The supplied PSyIR does not contain any routines." in
            str(err.value))


def test_generate_adjoint_logging(
        caplog, fortran_reader, fortran_writer, tmpdir):
    '''Test that logging works as expected in the generate_adjoint()
    function.

    '''
    tl_fortran_str = (
        "module test_mod\n"
        "contains\n"
        "  subroutine test1(a)\n"
        "  real :: a\n"
        "  a = 0.0\n"
        "  end subroutine\n"
        "  subroutine test2(a)\n"
        "  real :: a\n"
        "  a = 0.0\n"
        " end subroutine\n"
        "end module\n")
    expected_ad_fortran_str = (
        "module adj_test_mod\n"
        "  implicit none\n"
        "  public\n\n"
        "  contains\n"
        "  subroutine adj_test1(a)\n"
        "    real, intent(out) :: a\n\n"
        "    a = 0.0\n\n"
        "  end subroutine adj_test1\n"
        "  subroutine adj_test2(a)\n"
        "    real, intent(out) :: a\n\n"
        "    a = 0.0\n\n"
        "  end subroutine adj_test2\n\n"
        "end module adj_test_mod\n")
    tl_psyir = fortran_reader.psyir_from_source(tl_fortran_str)

    with caplog.at_level(logging.INFO):
        ad_psyir = generate_adjoint(tl_psyir, ["a"])
    assert caplog.text == ""

    ad_fortran_str = fortran_writer(ad_psyir)
    assert ad_fortran_str in expected_ad_fortran_str

    with caplog.at_level(logging.DEBUG):
        ad_psyir = generate_adjoint(tl_psyir, ["a"])
    if not caplog.text:
        pytest.xfail(reason="#1235: caplog returns an empty string in "
                     "github actions.")
    else:
        assert "Translating from LFRic TL to AD." in caplog.text
        assert "AD kernel will be named 'adj_test1'" in caplog.text
        assert "AD kernel will be named 'adj_test2'" in caplog.text

    ad_fortran_str = fortran_writer(ad_psyir)
    assert expected_ad_fortran_str in ad_fortran_str
    assert Compile(tmpdir).string_compiles(ad_fortran_str)


# generate_adjoint_test

def test_generate_adjoint_test_errors():
    ''' Check that generate_adjoint_test() raises the expected exceptions if
    the input is not valid for test-harness generation. '''
    with pytest.raises(NotImplementedError) as err:
        generate_adjoint_test(FileContainer("test_file"),
                              FileContainer("test_adj_file"), ['field'])
    assert ("Generation of a test harness is only supported for a TL kernel "
            "implemented as a subroutine within a module but failed to find "
            "enclosing module." in str(err.value))
    cont = Container("test_mod")
    # Only one routine is permitted
    kern1 = Routine.create("my_kern1", SymbolTable(), [Return()])
    cont.addchild(kern1)
    cont.addchild(Routine.create("my_kern2", SymbolTable(), [Return()]))
    with pytest.raises(NotImplementedError) as err:
        generate_adjoint_test(cont, cont.copy(), ['field'])
    assert ("The supplied Fortran must contain one and only one subroutine "
            "but found: ['my_kern1', 'my_kern2']" in str(err.value))
    cont.pop_all_children()
    kern1._is_program = True
    cont.addchild(kern1)
    with pytest.raises(NotImplementedError) as err:
        generate_adjoint_test(cont, cont.copy(), ['field'])
    assert ("Generation of a test harness for a kernel defined as a Program "
            "(as opposed to a Subroutine) is not currently supported. (Found "
            "'my_kern1' which is a Program.)" in str(err.value))


def test_generate_adjoint_test(fortran_reader, fortran_writer):
    ''' Full test for generating test harness for the adjoint created
    from a supplied subroutine within a module. The subroutine has one
    array argument with extent supplied as a second argument. '''
    tl_code = (
        "module my_mod\n"
        "  contains\n"
        "  subroutine kern(field, npts)\n"
        "    integer, intent(in) :: npts\n"
        "    real, intent(inout) :: field(npts)\n"
        "    field = 0.0\n"
        "  end subroutine kern\n"
        "end module my_mod\n"
    )
    tl_psyir = fortran_reader.psyir_from_source(tl_code)
    ad_psyir = generate_adjoint(tl_psyir, ["field"])
    test_psyir = generate_adjoint_test(tl_psyir, ad_psyir, ["field"])
    assert isinstance(test_psyir, Routine)
    assert test_psyir.is_program is True
    sym_table = test_psyir.symbol_table
    tl_kern = sym_table.lookup("kern")
    adj_kern = sym_table.lookup("adj_kern")
    assert isinstance(tl_kern, RoutineSymbol)
    assert isinstance(adj_kern, RoutineSymbol)
    assert isinstance(tl_kern.interface, ImportInterface)
    assert isinstance(adj_kern.interface, ImportInterface)
    assert tl_kern.interface.container_symbol.name == "my_mod"
    assert adj_kern.interface.container_symbol.name == "adj_my_mod"
    harness = fortran_writer(test_psyir)
    assert ("  real, dimension(npts) :: field\n"
            "  real, dimension(npts) :: field_input" in harness)
    assert ("  call random_number(field)\n"
            "  field_input = field\n"
            "  ! call the tangent-linear kernel\n"
            "  call kern(field, npts)\n"
            "  ! compute the inner product of the results of the tangent-"
            "linear kernel\n"
            "  inner1 = 0.0\n"
            "  inner1 = inner1 + dot_product(field, field)\n"
            "  ! call the adjoint of the kernel\n"
            "  call adj_kern(field, npts)\n"
            "  ! compute inner product of results of adjoint kernel with "
            "the original inputs to the tangent-linear kernel\n"
            "  inner2 = 0.0\n"
            "  inner2 = inner2 + dot_product(field, field_input)\n"
            "  ! test the inner-product values for equality, allowing for "
            "the precision of the active variables\n"
            "  machinetol = spacing(max(abs(inner1), abs(inner2)))\n"
            in harness.lower())
    # Ideally we would test that the generated harness code compiles
    # but, since it depends on the TL and adjoint kernels, we can't
    # currently do that (see #284).
    # assert Compile(tmpdir).string_compiles(harness)


def test_generate_adjoint_test_no_extent(fortran_reader, fortran_writer):
    ''' Full test for generating test harness for the adjoint created
    from a supplied subroutine within a module. The subroutine has two
    array arguments which are of assumed size. '''
    tl_code = (
        "module my_mod\n"
        "  contains\n"
        "  subroutine kern(field1, field2)\n"
        "    real, intent(inout) :: field1(:), field2(:,:)\n"
        "    field1(:) = 0.0\n"
        "    field2(:,:) = 0.0\n"
        "  end subroutine kern\n"
        "end module my_mod\n"
    )
    tl_psyir = fortran_reader.psyir_from_source(tl_code)
    ad_psyir = generate_adjoint(tl_psyir, ["field1", "field2"])
    test_psyir = generate_adjoint_test(tl_psyir, ad_psyir,
                                       ["field1", "field2"])
    harness = fortran_writer(test_psyir)
    assert "integer, parameter :: array_extent = 20\n" in harness
    assert "real, dimension(array_extent) :: field1\n" in harness
    assert "real, dimension(array_extent) :: field1_input\n" in harness
    assert "real, dimension(array_extent,array_extent) :: field2\n" in harness
    assert ("real, dimension(array_extent,array_extent) :: field2_input\n" in
            harness)
    # Ideally we would test that the generated harness code compiles
    # but, since it depends on the TL and adjoint kernels, we can't
    # currently do that (see #284).
    # assert Compile(tmpdir).string_compiles(harness)


def test_add_precision_symbol():
    ''' Tests for the _add_precision_symbol() utility function. '''
    table = SymbolTable()
    sym = DataSymbol("i_def", INTEGER_TYPE)
    _add_precision_symbol(sym, table)
    # A local symbol should just be copied into the table.
    new_sym = table.lookup("i_def")
    assert new_sym is not sym
    # Calling _add_precision a second time should do nothing.
    _add_precision_symbol(sym, table)
    assert table.lookup("i_def") is new_sym
    # An imported symbol should have its originating Container copied
    # over too.
    csym = ContainerSymbol("some_mod")
    rdef = DataSymbol("r_def", INTEGER_TYPE, interface=ImportInterface(csym))
    _add_precision_symbol(rdef, table)
    csym_copy = table.lookup("some_mod")
    assert isinstance(csym_copy, ContainerSymbol)
    assert csym_copy is not csym
    rdef_copy = table.lookup("r_def")
    assert rdef_copy.interface.container_symbol is csym_copy
    # A precision symbol must be either local or imported
    arg_sym = DataSymbol("wrong", INTEGER_TYPE, interface=ArgumentInterface())
    table.specify_argument_list([arg_sym])
    with pytest.raises(NotImplementedError) as err:
        _add_precision_symbol(arg_sym, table)
    assert ("One or more variables have a precision specified by symbol "
            "'wrong' which is not local or explicitly imported" in
            str(err.value))
    # A precision symbol must be a scalar integer or of unresolved/unsupported
    # type
    isym = DataSymbol("iwrong", REAL_TYPE)
    with pytest.raises(TypeError) as err:
        _add_precision_symbol(isym, table)
    assert ("integer type but 'iwrong' has type 'Scalar<REAL, UNDEFINED>'." in
            str(err.value))
    arr_sym = DataSymbol("iarray", ArrayType(INTEGER_TYPE, [10]))
    with pytest.raises(TypeError) as err:
        _add_precision_symbol(arr_sym, table)
    assert ("integer type but 'iarray' has type 'Array<Scalar<INTEGER, "
            "UNDEFINED>, shape=[10]>'." in str(err.value))
    def_sym = DataSymbol(
        "my_def", UnsupportedFortranType("integer, parameter :: my_def"))
    _add_precision_symbol(def_sym, table)
    assert table.lookup("my_def")
    odef_sym = DataSymbol("o_def", UnresolvedType(),
                          interface=ImportInterface(csym))
    _add_precision_symbol(odef_sym, table)
    assert table.lookup("o_def")


def test_generate_harness_extent_name_clash(fortran_reader, fortran_writer):
    ''' Test that we don't get a name clash when one of the kernel arguments
    matches the name we will give (internally) to the extent of the test
    arrays. '''
    tl_code = (
        "module my_mod\n"
        "  contains\n"
        "  subroutine kern(field1, array_extent)\n"
        "    integer, intent(in) :: array_extent\n"
        "    real, intent(inout) :: field1(array_extent)\n"
        "    field1(:) = 0.0\n"
        "  end subroutine kern\n"
        "end module my_mod\n"
    )
    tl_psyir = fortran_reader.psyir_from_source(tl_code)
    ad_psyir = generate_adjoint(tl_psyir, ["field1"])
    test_psyir = generate_adjoint_test(tl_psyir, ad_psyir, ["field1"])
    prog = test_psyir.walk(Routine)[0]
    assert "array_extent" in prog.symbol_table
    assert "array_extent_1" in prog.symbol_table
    harness = fortran_writer(test_psyir)
    # The re-named argument should be used to dimension the test arrays and
    # be passed to the kernels.
    assert "real, dimension(array_extent_1) :: field1" in harness
    assert "real, dimension(array_extent_1) :: field1_input" in harness
    assert "call kern(field1, array_extent_1)" in harness
    assert "call adj_kern(field1, array_extent_1)" in harness


def test_generate_harness_arg_name_clash(fortran_reader, fortran_writer):
    ''' Test that we don't get a name clash when one of the kernel field
    arguments matches the name we will give (internally) to the extent of the
    test arrays. '''
    tl_code = (
        "module my_mod\n"
        "  contains\n"
        "  subroutine kern(array_extent, extent)\n"
        "    integer, intent(in) :: extent\n"
        "    real, intent(inout) :: array_extent(extent)\n"
        "    array_extent(:) = 0.0\n"
        "  end subroutine kern\n"
        "end module my_mod\n"
    )
    tl_psyir = fortran_reader.psyir_from_source(tl_code)
    ad_psyir = generate_adjoint(tl_psyir, ["array_extent"])
    test_psyir = generate_adjoint_test(tl_psyir, ad_psyir, ["array_extent"])
    prog = test_psyir.walk(Routine)[0]
    assert "array_extent" in prog.symbol_table
    assert "array_extent_1" in prog.symbol_table
    harness = fortran_writer(test_psyir)
    # The re-named argument should be used to dimension the test arrays and
    # be passed to the kernels.
    assert "integer, parameter :: extent = array_extent" in harness
    assert "real, dimension(extent) :: array_extent_1" in harness
    assert "real, dimension(extent) :: array_extent_1_input" in harness
    assert "call kern(array_extent_1, extent)" in harness
    assert "call adj_kern(array_extent_1, extent)" in harness


def test_generate_harness_routine_name_clash(fortran_reader, fortran_writer):
    ''' Test that we don't get a name clash when one of the kernel arguments
    matches the name we will give (internally) to the extent of the test
    arrays. '''
    tl_code = (
        "module my_mod\n"
        "  contains\n"
        "  subroutine array_extent(field1, extent)\n"
        "    integer, intent(in) :: extent\n"
        "    real, intent(inout) :: field1(extent)\n"
        "    field1(:) = 0.0\n"
        "  end subroutine array_extent\n"
        "end module my_mod\n"
    )
    tl_psyir = fortran_reader.psyir_from_source(tl_code)
    ad_psyir = generate_adjoint(tl_psyir, ["field1"])
    test_psyir = generate_adjoint_test(tl_psyir, ad_psyir, ["field1"])
    harness = fortran_writer(test_psyir)
    assert "integer, parameter :: array_extent_1 = 20" in harness
    assert "integer, parameter :: extent = array_extent_1" in harness


def test_generate_harness_kernel_arg_static_shape(fortran_reader,
                                                  fortran_writer):
    ''' Test that we generate the correct array extents when a kernel
    defines the extent of an argument with a literal value. '''
    tl_code = (
        "module my_mod\n"
        "  contains\n"
        "  subroutine array_extent(field1, npts)\n"
        "    integer, intent(in) :: npts\n"
        "    real, intent(inout) :: field1(10, npts)\n"
        "    field1(:,:) = 0.0\n"
        "  end subroutine array_extent\n"
        "end module my_mod\n"
    )
    tl_psyir = fortran_reader.psyir_from_source(tl_code)
    ad_psyir = generate_adjoint(tl_psyir, ["field1"])
    test_psyir = generate_adjoint_test(tl_psyir, ad_psyir, ["field1"])
    harness = fortran_writer(test_psyir)
    assert "integer, parameter :: npts = array_extent_1" in harness
    assert "real, dimension(10,npts) :: field1" in harness
    assert "real, dimension(10,npts) :: field1_input" in harness


def test_generate_harness_kernel_arg_shape_error(fortran_reader):
    ''' Test that we raise the expected error if the shape of a kernel
    argument is a variable that is not passed as an argument. '''
    tl_code = (
        "module my_mod\n"
        "  contains\n"
        "  subroutine array_extent(field1)\n"
        "    use some_mod, only: npts\n"
        "    real, intent(inout) :: field1(10, npts)\n"
        "    field1(:,:) = 0.0\n"
        "  end subroutine array_extent\n"
        "end module my_mod\n"
    )
    tl_psyir = fortran_reader.psyir_from_source(tl_code)
    ad_psyir = generate_adjoint(tl_psyir, ["field1"])
    with pytest.raises(NotImplementedError) as err:
        generate_adjoint_test(tl_psyir, ad_psyir, ["field1"])
    assert ("Found argument 'field1' to kernel 'array_extent' which has a "
            "reference to 'npts' in its shape. However, 'npts' is not passed "
            "as an argument. This is not supported." in str(err.value))


def test_generate_harness_kernel_arg_invalid_shape(fortran_reader):
    ''' Test that the test-harness generation raises the expected error if
    a kernel argument's shape is invalid. '''
    tl_code = (
        "module my_mod\n"
        "  contains\n"
        "  subroutine kernel(field1, npts)\n"
        "    integer, intent(in) :: npts\n"
        "    real, intent(inout) :: field1(10, npts)\n"
        "    field1(:,:) = 0.0\n"
        "  end subroutine kernel\n"
        "end module my_mod\n"
    )
    tl_psyir = fortran_reader.psyir_from_source(tl_code)
    ad_psyir = generate_adjoint(tl_psyir, ["field1"])
    # Get hold of the kernel argument
    kernel = tl_psyir.walk(Routine)[0]
    fld_arg = kernel.symbol_table.argument_list[0]
    # Break one of the bounds in the Range inside the argument shape by making
    # it into a UnaryOperation node.
    fld_arg.datatype._shape[0] = ArrayType.ArrayBounds(
        lower=IntrinsicCall.create(IntrinsicCall.Intrinsic.NINT,
                                   [Literal("1", INTEGER_TYPE)]),
        upper=fld_arg.datatype._shape[0].upper)
    with pytest.raises(NotImplementedError) as err:
        generate_adjoint_test(tl_psyir, ad_psyir, ["field1"])
    assert ("Found argument 'field1' to kernel 'kernel' which has an array "
            "bound specified by a 'IntrinsicCall' node. Only Literals or "
            "References are supported" in str(err.value))
    # Break the argument shape.
    fld_arg.datatype._shape = [1] + fld_arg.datatype._shape[1:]
    with pytest.raises(InternalError) as err:
        generate_adjoint_test(tl_psyir, ad_psyir, ["field1"])
    assert ("Argument 'field1' to kernel 'kernel' contains a 'int' in its "
            "shape definition but expected an ArrayType.Extent or "
            "ArrayType.ArrayBound" in str(err.value))


def test_generate_harness_kind_import(fortran_reader, fortran_writer):
    ''' Check that the test harness that is generated correctly declares
    any imported kind parameter that is used to define the precision of the
    active variable(s).
    '''
    tl_code = (
        "module my_mod\n"
        "  use kinds_mod, only: r_def, i_def\n"
        "  use precision_mod, only: i32\n"
        "  contains\n"
        "  subroutine kern(field, npts, iflag)\n"
        "    integer(kind=i_def), intent(in) :: npts\n"
        "    real(kind=r_def), intent(inout) :: field(npts)\n"
        "    integer(kind=i32), intent(in) :: iflag\n"
        "    field = 0.0\n"
        "  end subroutine kern\n"
        "end module my_mod\n"
    )
    tl_psyir = fortran_reader.psyir_from_source(tl_code)
    ad_psyir = generate_adjoint(tl_psyir, ["field"])
    test_psyir = generate_adjoint_test(tl_psyir, ad_psyir, ["field"])
    harness = fortran_writer(test_psyir)
    assert ("  real(kind=r_def), dimension(npts) :: field\n"
            "  real(kind=r_def), dimension(npts) :: field_input" in harness)
    assert "real(kind=r_def) :: inner1\n" in harness
    assert "use kinds_mod, only : i_def, r_def" in harness
    assert "use precision_mod, only : i32" in harness
    assert ("real(kind=r_def), parameter :: overall_tolerance = "
            "1500.0_r_def" in harness)


def test_generate_harness_constant_kind(fortran_reader, fortran_writer):
    ''' Check that the test harness that is generated correctly declares
    a constant, local kind parameter that is used to define the precision
    of the active variable(s).
    '''
    tl_code = (
        "module my_mod\n"
        "  integer, parameter :: r_def=8\n"
        "  contains\n"
        "  subroutine kern(field, npts)\n"
        "    integer, intent(in) :: npts\n"
        "    real(kind=r_def), intent(inout) :: field(npts)\n"
        "    field = 0.0\n"
        "  end subroutine kern\n"
        "end module my_mod\n"
    )
    tl_psyir = fortran_reader.psyir_from_source(tl_code)
    ad_psyir = generate_adjoint(tl_psyir, ["field"])
    test_psyir = generate_adjoint_test(tl_psyir, ad_psyir, ["field"])
    harness = fortran_writer(test_psyir)
    assert ("  use adj_my_mod, only : adj_kern\n"
            "  integer, parameter :: r_def = 8\n" in harness)
    assert ("  real(kind=r_def), dimension(npts) :: field\n"
            "  real(kind=r_def), dimension(npts) :: field_input" in harness)
    assert "real(kind=r_def) :: inner1\n" in harness
    assert ("real(kind=r_def), parameter :: overall_tolerance = "
            "1500.0_r_def" in harness)


def test_generate_harness_unknown_kind_error(fortran_reader):
    ''' Check that generate_adjoint_test() raises the expected error if the
    kind of the active variables is of an unsupported form.
    '''
    tl_code = (
        "module my_mod\n"
        "  use kinds_mod\n"
        "  contains\n"
        "  subroutine kern(field, npts)\n"
        "    integer, intent(in) :: npts\n"
        "    real(kind=r_def), intent(inout) :: field(npts)\n"
        "    field = 0.0\n"
        "  end subroutine kern\n"
        "end module my_mod\n"
    )
    tl_psyir = fortran_reader.psyir_from_source(tl_code)
    ad_psyir = generate_adjoint(tl_psyir, ["field"])
    with pytest.raises(NotImplementedError) as err:
        generate_adjoint_test(tl_psyir, ad_psyir, ["field"])
    assert ("One or more variables have a precision specified by "
            "symbol 'r_def' which is not local or explicitly imported" in
            str(err.value))


# _create_inner_product and _create_array_inner_product

def test_create_inner_product_errors():
    ''' Check that the _create_inner_product() utility raises the expected
    exceptions if given invalid inputs. '''
    table = SymbolTable()
    accum = DataSymbol("result", REAL_DOUBLE_TYPE)
    var1 = DataSymbol("var1", REAL_DOUBLE_TYPE)
    var2 = DataSymbol("var2", INTEGER_TYPE)
    with pytest.raises(TypeError) as err:
        _create_inner_product(accum, [(var1, var2)], table)
    assert ("Cannot compute inner product of Symbols 'var1' and 'var2' "
            "because they represent different datatypes (Scalar" in
            str(err.value))
    var3 = DataSymbol("var3", ArrayType(REAL_DOUBLE_TYPE, [10]))
    with pytest.raises(TypeError) as err:
        _create_inner_product(accum, [(var1, var3)], table)
    assert ("Cannot compute inner product of Symbols 'var1' and 'var3' "
            "because they represent different datatypes (Scalar" in
            str(err.value))
    var4 = DataSymbol("var4", ArrayType(REAL_TYPE, [10]))
    with pytest.raises(TypeError) as err:
        _create_inner_product(accum, [(var4, var3)], table)
    assert ("Cannot compute inner product of Symbols 'var4' and 'var3' "
            "because they represent different datatypes (Array" in
            str(err.value))
    var5 = DataSymbol("var5", ArrayType(REAL_TYPE, [10, 10]))
    with pytest.raises(TypeError) as err:
        _create_inner_product(accum, [(var4, var5)], table)
    assert ("Cannot compute inner product of Symbols 'var4' and 'var5' "
            "because they represent different datatypes (Array" in
            str(err.value))


def test_create_array_inner_product_errors():
    ''' Tests for the checks in _create_array_inner_product function. '''
    table = SymbolTable()
    accum = DataSymbol("result", REAL_DOUBLE_TYPE)
    array_type = ArrayType(INTEGER_TYPE, [10])
    var1 = DataSymbol("var1", INTEGER_TYPE)
    var2 = DataSymbol("var2", array_type)
    with pytest.raises(TypeError) as err:
        _create_array_inner_product(accum, var1, var2, table)
    assert ("Symbols 'var1' and 'var2' because they represent different "
            "datatypes" in str(err.value))
    var2 = DataSymbol("var2", INTEGER_TYPE)
    with pytest.raises(TypeError) as err:
        _create_array_inner_product(accum, var1, var2, table)
    assert ("Supplied Symbols must represent arrays but got 'Scalar<INTEGER, "
            "UNDEFINED>' for 'var1'" in str(err.value))


def test_create_inner_product_scalars(fortran_writer):
    ''' Test for utility that creates PSyIR for computing an
    inner product when given scalars. '''
    accum = DataSymbol("result", REAL_DOUBLE_TYPE)
    var1 = DataSymbol("var1", INTEGER_TYPE)
    var2 = DataSymbol("var2", INTEGER_TYPE)
    table = SymbolTable()
    nodes = _create_inner_product(accum, [(var1, var2)], table)
    assert len(nodes) == 2
    assert isinstance(nodes[0], Assignment)
    assert nodes[0].lhs.symbol is accum
    assert nodes[0].rhs.value == "0.0"
    assert isinstance(nodes[1], Assignment)
    assert nodes[1].lhs.symbol is accum
    assert isinstance(nodes[1].rhs, BinaryOperation)
    assert nodes[1].rhs.operator == BinaryOperation.Operator.ADD
    code = fortran_writer(nodes[1])
    assert "result = result + var1 * var2" in code


def test_create_inner_product_1d_arrays(fortran_writer):
    ''' Test for utility that creates PSyIR for computing an
    inner product when given rank-1 arrays. '''
    table = SymbolTable()
    accum = DataSymbol("result", REAL_DOUBLE_TYPE)
    array_type = ArrayType(INTEGER_TYPE, [10])
    var1 = DataSymbol("var1", array_type)
    var2 = DataSymbol("var2", array_type)
    table.add(var1)
    table.add(var2)
    nodes = _create_inner_product(accum, [(var1, var2)], table)
    assert len(nodes) == 2
    assert isinstance(nodes[0], Assignment)
    assert nodes[0].lhs.symbol is accum
    assert nodes[0].rhs.value == "0.0"
    assert isinstance(nodes[1], Assignment)
    assert nodes[1].lhs.symbol is accum
    assert isinstance(nodes[1].rhs, BinaryOperation)
    assert nodes[1].rhs.operator == BinaryOperation.Operator.ADD
    code = fortran_writer(nodes[1])
    assert "result = result + DOT_PRODUCT(var1, var2)" in code


def test_create_inner_product_arrays(fortran_writer):
    ''' Test for utility that creates PSyIR for computing an
    inner product when given arrays with rank > 1. '''
    table = SymbolTable()
    accum = DataSymbol("result", REAL_DOUBLE_TYPE)
    table.add(accum)
    array_type = ArrayType(INTEGER_TYPE, [10, 10, 10])
    var1 = DataSymbol("var1", array_type)
    var2 = DataSymbol("var2", array_type)
    table.add(var1)
    table.add(var2)
    nodes = _create_inner_product(accum, [(var1, var2)], table)
    assert len(nodes) == 2
    assert isinstance(nodes[0], Assignment)
    assert nodes[0].lhs.symbol is accum
    assert nodes[0].rhs.value == "0.0"
    assert isinstance(nodes[1], Assignment)
    assert nodes[1].lhs.symbol is accum
    assert isinstance(nodes[1].rhs, BinaryOperation)
    assert nodes[1].rhs.operator == BinaryOperation.Operator.ADD
    code = fortran_writer(nodes[1])
    assert "result = result + SUM(var1(:,:,:) * var2(:,:,:))" in code


def test_inner_product_scalars_and_arrays(fortran_writer):
    ''' Test for utility that creates PSyIR for computing an
    inner product when given arrays and scalars. '''
    table = SymbolTable()
    accum = DataSymbol("result", REAL_DOUBLE_TYPE)
    table.add(accum)
    array3d_type = ArrayType(INTEGER_TYPE, [10, 10, 10])
    vars3d = DataSymbol("var1", array3d_type), DataSymbol("var2", array3d_type)
    table.add(vars3d[0])
    table.add(vars3d[1])
    array1d_type = ArrayType(INTEGER_TYPE, [5])
    vecs = DataSymbol("vec1", array1d_type), DataSymbol("vec2", array1d_type)
    table.add(vecs[0])
    table.add(vecs[1])
    scals = DataSymbol("a1", REAL_TYPE), DataSymbol("a2", REAL_TYPE)
    table.add(scals[0])
    table.add(scals[1])
    nodes = _create_inner_product(accum, [vars3d, vecs, scals], table)
    assert len(nodes) == 4
    assert all(isinstance(node, Assignment) for node in nodes)
    assert fortran_writer(nodes[0]) == "result = 0.0\n"
    assert (fortran_writer(nodes[1]) ==
            "result = result + SUM(var1(:,:,:) * var2(:,:,:))\n")
    assert (fortran_writer(nodes[2]) ==
            "result = result + DOT_PRODUCT(vec1, vec2)\n")
    assert fortran_writer(nodes[3]) == "result = result + a1 * a2\n"
