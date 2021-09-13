# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2021, Science and Technology Facilities Council.
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

'''A module to perform pytest tests on the code in the tl2ad.py file
within the psyad directory.

'''
from __future__ import print_function, absolute_import
import logging
import pytest
import six

from psyclone.errors import InternalError
from psyclone.psyir.backend.fortran import FortranWriter
from psyclone.psyir.frontend.fortran import FortranReader
from psyclone.psyir.nodes import Container, FileContainer, Return, Routine, \
    Assignment, BinaryOperation
from psyclone.psyir.symbols import DataSymbol, SymbolTable, REAL_DOUBLE_TYPE, \
    INTEGER_TYPE, REAL_TYPE, ArrayType, RoutineSymbol, ImportInterface
from psyclone.psyad import generate_adjoint_str, generate_adjoint, \
    generate_adjoint_test
from psyclone.psyad.tl2ad import _find_container, _create_inner_product, \
    _create_array_inner_product


# 1: generate_adjoint_str function

# expected output
@pytest.mark.xfail(reason="issue #1235: caplog returns an empty string in "
                   "github actions.", strict=False)
def test_generate_adjoint_str(caplog):
    '''Test that the generate_adjoint_str() function works as expected
    including logging.

    '''
    tl_code = (
        "program test\n"
        "integer :: a\n"
        "a = 0.0\n"
        "end program test\n")
    expected = (
        "program test\n"
        "  integer :: a\n\n"
        "  a = 0.0\n\n"
        "end program test\n")

    with caplog.at_level(logging.INFO):
        result, test_harness = generate_adjoint_str(tl_code)

    assert caplog.text == ""
    assert expected in result
    assert test_harness is None

    with caplog.at_level(logging.DEBUG):
        result, test_harness = generate_adjoint_str(tl_code)

    assert "DEBUG    psyclone.psyad.tl2ad:tl2ad.py:58" in caplog.text
    assert tl_code in caplog.text
    assert "DEBUG    psyclone.psyad.tl2ad:tl2ad.py:74" in caplog.text
    assert expected in caplog.text
    assert expected in result
    assert test_harness is None


def test_find_container():
    ''' Tests for the internal, helper function _find_container(). '''
    assert _find_container(Return()) is None
    assert _find_container(FileContainer("test")) is None
    cont = Container("my_mod")
    assert _find_container(cont) is cont
    cont.addchild(FileContainer("test"))
    with pytest.raises(InternalError) as err:
        _find_container(cont)
    assert ("The supplied PSyIR contains two Containers but the innermost is "
            "a FileContainer. This should not be possible" in str(err.value))
    cont = Container("my_mod")
    cont.addchild(Container("another_mod"))
    with pytest.raises(NotImplementedError) as err:
        _find_container(cont)
    assert ("supplied PSyIR contains two Containers and the outermost one is "
            "not a FileContainer. This is not supported." in str(err.value))
    file_cont = FileContainer("test")
    cont = Container("my_mod")
    file_cont.addchild(cont)
    assert _find_container(file_cont) is cont
    file_cont.addchild(cont.copy())
    with pytest.raises(NotImplementedError) as err:
        _find_container(file_cont)
    assert ("The supplied PSyIR contains more than two Containers. This is "
            "not supported." in str(err.value))


# 2: generate_adjoint function
def test_generate_adjoint(fortran_reader):
    '''Test that the generate_adjoint() function works as expected.'''

    tl_fortran_str = (
        "program test\n"
        "integer :: a\n"
        "a = 0.0\n"
        "end program test\n")
    expected_ad_fortran_str = (
        "program test_adj\n"
        "  integer :: a\n\n"
        "  a = 0.0\n\n"
        "end program test_adj\n")
    tl_psyir = fortran_reader.psyir_from_source(tl_fortran_str)

    ad_psyir = generate_adjoint(tl_psyir)

    writer = FortranWriter()
    ad_fortran_str = writer(ad_psyir)
    assert expected_ad_fortran_str in ad_fortran_str


def test_generate_adjoint_errors():
    ''' Check that generate_adjoint() raises the expected exceptions when
    given invalid input. '''
    # Only a FileContainer
    psyir = FileContainer("test_file")
    with pytest.raises(InternalError) as err:
        generate_adjoint(psyir)
    assert ("The supplied PSyIR does not contain any routines." in
            str(err.value))
    with pytest.raises(InternalError) as err:
        generate_adjoint(Container.create("test_mod", SymbolTable(),
                                          [psyir.copy()]))
    assert ("The supplied PSyIR contains two Containers but the innermost is "
            "a FileContainer. This should not be possible" in str(err.value))
    # No kernel code
    cont = Container("test_mod")
    with pytest.raises(InternalError) as err:
        generate_adjoint(cont)
    assert ("The supplied PSyIR does not contain any routines." in
            str(err.value))
    # Only one routine is permitted
    cont.addchild(Routine.create("my_kern1", SymbolTable(), [Return()]))
    cont.addchild(Routine.create("my_kern2", SymbolTable(), [Return()]))
    with pytest.raises(NotImplementedError) as err:
        generate_adjoint(cont)
    assert ("The supplied Fortran must contain one and only one routine but "
            "found: ['my_kern1', 'my_kern2']" in str(err.value))


# generate_adjoint function logging
@pytest.mark.xfail(reason="issue #1235: caplog returns an empty string in "
                   "github actions.", strict=False)
def test_generate_adjoint_logging(caplog):
    '''Test that logging works as expected in the generate_adjoint()
    function.

    '''
    tl_fortran_str = (
        "program test\n"
        "integer :: a\n"
        "a = 0.0\n"
        "end program test\n")
    expected_ad_fortran_str = (
        "program test\n"
        "  integer :: a\n\n"
        "  a = 0.0\n\n"
        "end program test\n")
    reader = FortranReader()
    tl_psyir = reader.psyir_from_source(tl_fortran_str)

    with caplog.at_level(logging.INFO):
        ad_psyir = generate_adjoint(tl_psyir)
    assert caplog.text == ""

    writer = FortranWriter()
    ad_fortran_str = writer(ad_psyir)
    assert expected_ad_fortran_str in ad_fortran_str

    with caplog.at_level(logging.DEBUG):
        ad_psyir = generate_adjoint(tl_psyir)
    # Python2 and 3 report different line numbers
    if six.PY2:
        line_number = 96
    else:
        line_number = 95
    assert (
        "DEBUG    psyclone.psyad.tl2ad:tl2ad.py:{0} Translation from generic "
        "PSyIR to LFRic-specific PSyIR should be done now.".format(line_number)
        in caplog.text)
    assert (
        "DEBUG    psyclone.psyad.tl2ad:tl2ad.py:100 Transformation from TL to "
        "AD should be done now." in caplog.text)

    ad_fortran_str = writer(ad_psyir)
    assert expected_ad_fortran_str in ad_fortran_str


def test_generate_adjoint_str_generate_harness():
    ''' Test the create_test option to generate_adjoint_str(). '''
    tl_code = (
        "module my_mod\n"
        "  contains\n"
        "  subroutine kern(field)\n"
        "    real, intent(inout) :: field\n"
        "    field = 0.0\n"
        "  end subroutine kern\n"
        "end module my_mod\n"
    )
    result, harness = generate_adjoint_str(tl_code, create_test=True)
    assert "subroutine kern_adj(field)\n" in result
    assert ('''program adj_test
  use my_mod, only : kern
  use my_mod_adj, only : kern_adj
  integer, parameter :: array_extent = 20
  double precision :: inner1
  double precision :: inner2
  double precision :: abs_diff
  real :: field
  real :: field_input

  CALL random_number(field)
  field_input = field
  call kern(field)
  inner1 = 0.0
  inner1 = inner1 + field * field
  call kern_adj(field)
  inner2 = 0.0
  inner2 = inner2 + field * field_input
  abs_diff = ABS(inner1 - inner2)
  if (abs_diff > 1.0e-10) then
    WRITE(*, *) 'Test of adjoint of ''kern'' failed: diff = ', abs_diff
    return
  end if
  WRITE(*, *) 'Test of adjoint of ''kern'' passed: diff = ', abs_diff

end program adj_test''' in harness)


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
        _ = generate_adjoint_str(tl_code, create_test=True)
    assert "hello" in caplog.text


def test_create_inner_product_errors():
    ''' Check that the _create_inner_product() utility raises the expected
    exceptions if given invalid inputs. '''
    accum = DataSymbol("result", REAL_DOUBLE_TYPE)
    var1 = DataSymbol("var1", REAL_DOUBLE_TYPE)
    var2 = DataSymbol("var2", INTEGER_TYPE)
    with pytest.raises(TypeError) as err:
        _create_inner_product(accum, [(var1, var2)])
    assert ("Cannot compute inner product of Symbols 'var1' and 'var2' "
            "because they represent different datatypes (Scalar" in
            str(err.value))
    var3 = DataSymbol("var3", ArrayType(REAL_DOUBLE_TYPE, [10]))
    with pytest.raises(TypeError) as err:
        _create_inner_product(accum, [(var1, var3)])
    assert ("Cannot compute inner product of Symbols 'var1' and 'var3' "
            "because they represent different datatypes (Scalar" in
            str(err.value))
    var4 = DataSymbol("var4", ArrayType(REAL_TYPE, [10]))
    with pytest.raises(TypeError) as err:
        _create_inner_product(accum, [(var4, var3)])
    assert ("Cannot compute inner product of Symbols 'var4' and 'var3' "
            "because they represent different datatypes (Array" in
            str(err.value))
    var5 = DataSymbol("var5", ArrayType(REAL_TYPE, [10, 10]))
    with pytest.raises(TypeError) as err:
        _create_inner_product(accum, [(var4, var5)])
    assert ("Cannot compute inner product of Symbols 'var4' and 'var5' "
            "because they represent different datatypes (Array" in
            str(err.value))


def test_create_array_inner_product_errors():
    ''' Tests for the checks in _create_array_inner_product function. '''
    accum = DataSymbol("result", REAL_DOUBLE_TYPE)
    array_type = ArrayType(INTEGER_TYPE, [10])
    var1 = DataSymbol("var1", INTEGER_TYPE)
    var2 = DataSymbol("var2", array_type)
    with pytest.raises(TypeError) as err:
        _create_array_inner_product(accum, var1, var2)
    assert ("Symbols 'var1' and 'var2' because they represent different "
            "datatypes" in str(err.value))
    var2 = DataSymbol("var2", INTEGER_TYPE)
    with pytest.raises(TypeError) as err:
        _create_array_inner_product(accum, var1, var2)
    assert ("Supplied Symbols must represent arrays but got 'Scalar<INTEGER, "
            "UNDEFINED>' for 'var1'" in str(err.value))


def test_create_inner_product_scalars(fortran_writer):
    ''' Test for utility that creates PSyIR for computing an
    inner product when given scalars. '''
    accum = DataSymbol("result", REAL_DOUBLE_TYPE)
    var1 = DataSymbol("var1", INTEGER_TYPE)
    var2 = DataSymbol("var2", INTEGER_TYPE)
    nodes = _create_inner_product(accum, [(var1, var2)])
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
    accum = DataSymbol("result", REAL_DOUBLE_TYPE)
    array_type = ArrayType(INTEGER_TYPE, [10])
    var1 = DataSymbol("var1", array_type)
    var2 = DataSymbol("var2", array_type)
    nodes = _create_inner_product(accum, [(var1, var2)])
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
    accum = DataSymbol("result", REAL_DOUBLE_TYPE)
    array_type = ArrayType(INTEGER_TYPE, [10, 10, 10])
    var1 = DataSymbol("var1", array_type)
    var2 = DataSymbol("var2", array_type)
    nodes = _create_inner_product(accum, [(var1, var2)])
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
    accum = DataSymbol("result", REAL_DOUBLE_TYPE)
    array3d_type = ArrayType(INTEGER_TYPE, [10, 10, 10])
    vars3d = DataSymbol("var1", array3d_type), DataSymbol("var2", array3d_type)
    array1d_type = ArrayType(INTEGER_TYPE, [5])
    vecs = DataSymbol("vec1", array1d_type), DataSymbol("vec2", array1d_type)
    scals = DataSymbol("a1", REAL_TYPE), DataSymbol("a2", REAL_TYPE)
    nodes = _create_inner_product(accum, [vars3d, vecs, scals])
    assert len(nodes) == 4
    assert all([isinstance(node, Assignment) for node in nodes])
    assert fortran_writer(nodes[0]) == "result = 0.0\n"
    assert (fortran_writer(nodes[1]) ==
            "result = result + SUM(var1(:,:,:) * var2(:,:,:))\n")
    assert (fortran_writer(nodes[2]) ==
            "result = result + DOT_PRODUCT(vec1, vec2)\n")
    assert (fortran_writer(nodes[3]) == "result = result + a1 * a2\n")


def test_generate_adjoint_test_errors():
    ''' Check that generate_adjoint_test() raises the expected exceptions if
    the input is not valid for test-harness generation. '''
    with pytest.raises(NotImplementedError) as err:
        generate_adjoint_test(FileContainer("test_file"),
                              FileContainer("test_adj_file"))
    assert ("Generation of a test harness is only supported for a TL kernel "
            "implemented as a subroutine within a module but failed to find "
            "enclosing module." in str(err.value))
    cont = Container("test_mod")
    # Only one routine is permitted
    kern1 = Routine.create("my_kern1", SymbolTable(), [Return()])
    cont.addchild(kern1)
    cont.addchild(Routine.create("my_kern2", SymbolTable(), [Return()]))
    with pytest.raises(NotImplementedError) as err:
        generate_adjoint_test(cont, cont.copy())
    assert ("The supplied Fortran must contain one and only one subroutine "
            "but found: ['my_kern1', 'my_kern2']" in str(err.value))
    cont.pop_all_children()
    kern1._is_program = True
    cont.addchild(kern1)
    with pytest.raises(NotImplementedError) as err:
        generate_adjoint_test(cont, cont.copy())
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
    ad_psyir = generate_adjoint(tl_psyir)
    test_psyir = generate_adjoint_test(tl_psyir, ad_psyir)
    assert isinstance(test_psyir, Routine)
    assert test_psyir.is_program is True
    sym_table = test_psyir.symbol_table
    tl_kern = sym_table.lookup("kern")
    adj_kern = sym_table.lookup("kern_adj")
    assert isinstance(tl_kern, RoutineSymbol)
    assert isinstance(adj_kern, RoutineSymbol)
    assert isinstance(tl_kern.interface, ImportInterface)
    assert isinstance(adj_kern.interface, ImportInterface)
    assert tl_kern.interface.container_symbol.name == "my_mod"
    assert adj_kern.interface.container_symbol.name == "my_mod_adj"
    harness = fortran_writer(test_psyir)
    assert ("  real, dimension(npts) :: field\n"
            "  real, dimension(npts) :: field_input" in harness)
    assert ("  CALL random_number(field)\n"
            "  field_input = field\n"
            "  call kern(field, npts)\n"
            "  inner1 = 0.0\n"
            "  inner1 = inner1 + DOT_PRODUCT(field, field)\n"
            "  call kern_adj(field, npts)\n"
            "  inner2 = 0.0\n"
            "  inner2 = inner2 + DOT_PRODUCT(field, field_input)\n"
            "  abs_diff = ABS(inner1 - inner2)\n" in harness)
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
    ad_psyir = generate_adjoint(tl_psyir)
    test_psyir = generate_adjoint_test(tl_psyir, ad_psyir)
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
    ad_psyir = generate_adjoint(tl_psyir)
    test_psyir = generate_adjoint_test(tl_psyir, ad_psyir)
    prog = test_psyir.walk(Routine)[0]
    assert "array_extent" in prog.symbol_table
    assert "array_extent_1" in prog.symbol_table
    harness = fortran_writer(test_psyir)
    # The re-named argument should be used to dimension the test arrays and
    # be passed to the kernels.
    assert "real, dimension(array_extent_1) :: field1" in harness
    assert "real, dimension(array_extent_1) :: field1_input" in harness
    assert "call kern(field1, array_extent_1)" in harness
    assert "call kern_adj(field1, array_extent_1)" in harness


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
    ad_psyir = generate_adjoint(tl_psyir)
    test_psyir = generate_adjoint_test(tl_psyir, ad_psyir)
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
    assert "call kern_adj(array_extent_1, extent)" in harness


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
    ad_psyir = generate_adjoint(tl_psyir)
    test_psyir = generate_adjoint_test(tl_psyir, ad_psyir)
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
    ad_psyir = generate_adjoint(tl_psyir)
    test_psyir = generate_adjoint_test(tl_psyir, ad_psyir)
    harness = fortran_writer(test_psyir)
    assert "integer, parameter :: npts = array_extent_1" in harness
    assert "real, dimension(10,npts) :: field1" in harness
    assert "real, dimension(10,npts) :: field1_input" in harness


def test_generate_harness_kernel_arg_shape_error(fortran_reader,
                                                 fortran_writer):
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
    ad_psyir = generate_adjoint(tl_psyir)
    with pytest.raises(NotImplementedError) as err:
        generate_adjoint_test(tl_psyir, ad_psyir)
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
    ad_psyir = generate_adjoint(tl_psyir)
    # Get hold of the kernel argument
    kernel = tl_psyir.walk(Routine)[0]
    fld_arg = kernel.symbol_table.argument_list[0]
    # Break one of the bounds in the Range inside the argument shape by making
    # it into a Return node.
    fld_arg.datatype._shape[0] = ArrayType.ArrayBounds(
        lower=Return(), upper=fld_arg.datatype._shape[0].upper)
    with pytest.raises(NotImplementedError) as err:
        generate_adjoint_test(tl_psyir, ad_psyir)
    assert ("Found argument 'field1' to kernel 'kernel' which has an array "
            "bound specified by a 'Return' node. Only Literals or References "
            "are supported" in str(err.value))
    # Break the argument shape.
    fld_arg.datatype._shape = [1] + fld_arg.datatype._shape[1:]
    with pytest.raises(InternalError) as err:
        generate_adjoint_test(tl_psyir, ad_psyir)
    assert ("Argument 'field1' to kernel 'kernel' contains a 'int' in its "
            "shape definition but expected an ArrayType.Extent or "
            "ArrayType.ArrayBound" in str(err.value))
