# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017-2024, Science and Technology Facilities Council.
#
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
# Authors: R. W. Ford and A. R. Porter, STFC Daresbury Lab
# Modified: I. Kavcic and L. Turner, Met Office

''' Tests for the algorithm generation (re-writing) as implemented
    in alg_gen.py '''

import os
import pytest

from fparser.common.readfortran import FortranStringReader
from fparser.two import Fortran2003
from fparser.two.utils import walk

from psyclone import alg_gen
from psyclone.configuration import Config
from psyclone.generator import generate, GenerationError
from psyclone.errors import InternalError


@pytest.fixture(scope="module", autouse=True)
def setup():
    '''Make sure that all tests here use dynamo0.3 as API.'''
    Config.get().api = "dynamo0.3"
    yield
    Config._instance = None


BASE_PATH = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                         "test_files", "dynamo0p3")


def test_single_function_invoke():
    ''' single kernel specified in an invoke call'''
    alg, _ = generate(os.path.join(BASE_PATH, "1_single_invoke.f90"),
                      api="dynamo0.3")
    gen = str(alg).lower()
    # TODO issue #1618 Split test into two as while there are
    # different implementations we may or may not output a space
    # before the ':'
    assert "use single_invoke_psy, only" in gen
    assert ": invoke_0_testkern_type" in gen
    assert "call invoke_0_testkern_type(a, f1, f2, m1, m2)" in gen
    assert "use testkern_mod" not in gen


def test_single_function_named_invoke():
    ''' Test that we correctly handle a named invoke call '''
    alg, _ = generate(
        os.path.join(os.path.dirname(os.path.abspath(__file__)),
                     "test_files", "dynamo0p3",
                     "1.0.1_single_named_invoke.f90"),
        api="dynamo0.3")
    gen = str(alg).lower()
    # TODO issue #1618 Split test into two as while there are
    # different implementations we may or may not output a space
    # before the ':'
    assert "use single_invoke_psy, only" in gen
    assert ": invoke_important_invoke" in gen
    assert "call invoke_important_invoke(a, f1, f2, m1, m2)" in gen


def test_invoke_named_invoke():
    ''' Test that we correctly handle a named invoke call where the
    naming string already begins with "invoke_" '''
    alg, _ = generate(
        os.path.join(os.path.dirname(os.path.abspath(__file__)),
                     "test_files", "dynamo0p3",
                     "1.0.5_invoke_named_invoke.f90"),
        api="dynamo0.3")
    gen = str(alg).lower()
    # TODO issue #1618 Split test into two as while there are
    # different implementations we may or may not output a space
    # before the ':'
    assert "use single_invoke_psy, only" in gen
    assert ": invoke_important" in gen
    assert "call invoke_important(a, f1, f2, m1, m2)" in gen


def test_multi_kernel_named_invoke():
    ''' Test that we correctly handle a named invoke call that contains
    more than one kernel '''
    alg, _ = generate(
        os.path.join(os.path.dirname(os.path.abspath(__file__)),
                     "test_files", "dynamo0p3",
                     "4.9_named_multikernel_invokes.f90"),
        api="dynamo0.3")
    gen = str(alg).lower()
    # TODO issue #1618 Split test into two as while there are
    # different implementations we may or may not output a space
    # before the ':'
    assert "use multikernel_invokes_7_psy, only" in gen
    assert ": invoke_some_name" in gen
    assert (
        "call invoke_some_name(a, b, istp, rdt, d, e, ascalar, f, c, g, qr)"
        in gen)


def test_multi_position_named_invoke():
    ''' Test that we correctly handle a named invoke call that contains
    more than one kernel when the name= clause appears at different
    points in the Invoke argument list '''
    alg, _ = generate(
        os.path.join(os.path.dirname(os.path.abspath(__file__)),
                     "test_files", "dynamo0p3",
                     "4.10_multi_position_named_invokes.f90"),
        api="dynamo0.3")
    gen = str(alg).lower()

    # TODO issue #1618 Split test into two as while there are
    # different implementations we may or may not output a space
    # before the ':' and may have multiple only names.
    assert "use multikernel_invokes_7_psy, only" in gen
    assert ": invoke_name_first" in gen
    assert "invoke_name_middle" in gen
    assert "invoke_name_last" in gen
    assert ("call invoke_name_first(a, b, istp, rdt, d, e, ascalar, f, c, "
            "g, qr)") in gen
    assert ("call invoke_name_middle(a, b, istp, rdt, d, e, ascalar, f, c, "
            "g, qr)") in gen
    assert ("call invoke_name_last(a, b, istp, rdt, d, e, ascalar, f, c, "
            "g, qr)") in gen


def test_single_function_invoke_qr():
    ''' single function specified in an invoke call which requires a
    quadrature rule'''
    alg, _ = generate(os.path.join(BASE_PATH,
                                   "1.1.0_single_invoke_xyoz_qr.f90"),
                      api="dynamo0.3")
    gen = str(alg).lower()
    assert "use testkern_qr_mod" not in gen
    # TODO issue #1618 Split test into two as while there are
    # different implementations we may or may not output a space
    # before the ':'
    assert "use single_invoke_psy, only" in gen
    assert ": invoke_0_testkern_qr_type" in gen
    assert ("call invoke_0_testkern_qr_type(f1, f2, m1, a, m2, istp, qr)"
            in gen)


def test_multi_function_invoke():
    ''' two functions specified in an invoke call'''
    alg, _ = generate(os.path.join(BASE_PATH, "1.2_multi_invoke.f90"),
                      api="dynamo0.3")
    gen = str(alg).lower()
    # TODO issue #1618 Split test into two as while there are
    # different implementations we may or may not output a space
    # before the ':'
    assert "use multi_invoke_psy, only" in gen
    assert ": invoke_0" in gen
    assert "call invoke_0(a, f1, f2, m1, m2, f3)" in gen


def test_single_function_multi_invokes():
    ''' three invokes, each containing a single function '''
    alg, _ = generate(os.path.join(BASE_PATH, "3_multi_invokes.f90"),
                      api="dynamo0.3")
    gen = str(alg).lower()
    # Use statements for kernels should have been removed.
    assert "use testkern_mod" not in gen
    assert "use testkern_qr_mod" not in gen
    # TODO issue #1618 Split test into two as while there are
    # different implementations we may or may not output a space
    # before the ':' and may have multiple only names
    # Use statements for PSy-layer routines should have been added.
    assert "use multi_invokes_psy, only" in gen
    assert ": invoke_0_testkern_type" in gen
    assert "invoke_2_testkern_type" in gen
    assert "invoke_1_testkern_qr_type" in gen
    assert "call invoke_0_testkern_type(a, f1, f2, m1, m2)" in gen
    assert "call invoke_2_testkern_type(a, f1, f2, m1, m2)" in gen
    assert ("call invoke_1_testkern_qr_type(f1, f2, m1, a, m2, istp, qr)"
            in gen)


def test_named_multi_invokes():
    ''' Check that we generate correct code when we have more than one
    named invoke in an Algorithm file '''
    alg, _ = generate(
        os.path.join(BASE_PATH,
                     "3.2_multi_functions_multi_named_invokes.f90"),
        api="dynamo0.3")
    gen = str(alg).lower()
    # Use statements for kernels should have been removed.
    assert "use testkern_mod" not in gen
    assert "use testkern_qr_mod" not in gen
    # TODO issue #1618 Split test into two as while there are
    # different implementations we may or may not output a space
    # before the ':' and may have multiple only names.
    # Use statements for PSy-layer routines should have been added.
    assert "use multi_functions_multi_invokes_psy, only" in gen
    assert ": invoke_my_first" in gen
    assert "invoke_my_second" in gen
    assert "call invoke_my_first(a, f1, f2," in gen
    assert "call invoke_my_second(f1, f2, m1, a, m2" in gen


def test_multi_function_multi_invokes():
    ''' two invokes, each containing multiple functions '''
    alg, _ = generate(
        os.path.join(BASE_PATH, "3.1_multi_functions_multi_invokes.f90"),
        api="dynamo0.3")
    gen = str(alg).lower()
    # TODO issue #1618 Split test into two as while there are
    # different implementations we may or may not output a space
    # before the ':' and may have multiple only names.
    assert "use multi_functions_multi_invokes_psy, only" in gen
    assert ": invoke_0" in gen
    assert "invoke_1" in gen
    assert "call invoke_0(a, f1, f2, m1, m2, istp, qr)" in gen
    assert "call invoke_1(f1, f2, m1, a, m2, istp, qr)" in gen


def test_multi_function_invoke_qr():
    '''three functions specified in an invoke call, two of which which
    requires a quadrature rule'''
    alg, _ = generate(os.path.join(
        BASE_PATH, "1.3_multi_invoke_qr.f90"), api="dynamo0.3")
    gen = str(alg).lower()
    # Use statements for kernels should have been removed.
    assert "use testkern_qr_mod" not in gen
    assert "use testkern_mod" not in gen
    # TODO issue #1618 Split test into two as while there are
    # different implementations we may or may not output a space
    # before the ':'
    # Use statement for PSy-layer routines should have been added.
    assert "use multi_invoke_qr_psy, only" in gen
    assert ": invoke_0" in gen
    assert "call invoke_0(f1, f2, m1, a, m2, istp, m3, f3, qr)" in gen


def test_invoke_argnames():
    ''' invoke call arguments which are arrays '''
    alg, _ = generate(os.path.join(
        BASE_PATH, "5_alg_field_array.f90"), api="dynamo0.3")
    gen = str(alg).lower()
    # TODO issue #1618 Split test into two as while there are
    # different implementations we may or may not output a space
    # before the ':'
    assert "use single_function_psy, only" in gen
    assert ": invoke_0" in gen
    # TODO issue #1618 different implementations we may or may not
    # output a space after a ","
    assert ("call invoke_0(f0(1), f1(1, 1), f1(2, index), b(1), "
            "f1(index, index2(index3)), iflag(2), a(index1), "
            "iflag(index2(index3)), qr)" in gen or
            "call invoke_0(f0(1), f1(1,1), f1(2,index), b(1), "
            "f1(index,index2(index3)), iflag(2), a(index1), "
            "iflag(index2(index3)), qr)" in gen)


def test_multiple_qr_per_invoke():
    ''' Test that we handle an Invoke containing multiple kernel calls,
    each requiring quadrature. '''
    alg, _ = generate(os.path.join(
        BASE_PATH, "6_multiple_QR_per_invoke.f90"), api="dynamo0.3")
    gen = str(alg).lower()
    # TODO issue #1618 Split test into two as while there are
    # different implementations we may or may not output a space
    # before the ':'
    assert "use multi_qr_per_invoke_psy, only" in gen
    assert ": invoke_0" in gen
    assert ("call invoke_0(f1, f2, f3, ascalar, f4, iscalar, f0, qr0, qr1)"
            in gen)


def test_qr_argnames():
    ''' Check that we produce correct Algorithm code when the invoke passes
    qr arguments that are array elements. '''
    alg, _ = generate(os.path.join(BASE_PATH, "7_QR_field_array.f90"),
                      api="dynamo0.3")
    gen = str(alg).lower()
    # TODO issue #1618 Split test into two as while there are
    # different implementations we may or may not output a space
    # before the ':'
    assert "use qr_field_array_psy, only" in gen
    assert ": invoke_0" in gen
    # TODO issue #1618 different implementations we may or may not
    # output a space after a ","
    assert ("call invoke_0(f1, f2, f3, ascal, f4, l, f0, qr0(i, j), "
            "qr0(i, j + 1), qr1(i, k(l)))" in gen or
            "call invoke_0(f1, f2, f3, ascal, f4, l, f0, qr0(i,j), "
            "qr0(i,j + 1), qr1(i,k(l)))" in gen)


def test_deref_derived_type_args():
    ''' Test the case where a kernel argument is specified as both a
    component of a derived type and as the result of a call to a
    type-bound procedure '''
    alg, _ = generate(
        os.path.join(os.path.dirname(os.path.abspath(__file__)),
                     "test_files", "dynamo0p3",
                     "1.6.2_single_invoke_1_int_from_derived_type.f90"),
        api="dynamo0.3")
    gen = str(alg).lower()
    # TODO issue #1618 different implementations we may or may not
    # output a space before and after a "%"
    assert (
        "call invoke_0(f1, my_obj % iflag, f2, m1, m2, my_obj % get_flag(), "
        "my_obj % get_flag(switch), my_obj % get_flag(int_wrapper % data))"
        in gen or
        "call invoke_0(f1, my_obj%iflag, f2, m1, m2, my_obj % get_flag(), "
        "my_obj%get_flag(switch), my_obj%get_flag(int_wrapper%data))"
        in gen)


def test_multi_deref_derived_type_args():
    ''' Test the case where a given kernel argument is specified using
    different derived types in the same invoke. '''
    alg, _ = generate(
        os.path.join(os.path.dirname(os.path.abspath(__file__)),
                     "test_files", "dynamo0p3",
                     "1.6.3_single_invoke_multiple_derived_types.f90"),
        api="dynamo0.3")
    gen = str(alg).lower()
    # TODO issue #1618 different implementations we may or may not
    # output a space before and after a "%"
    assert (
        "call invoke_0(f1, obj_a % iflag, f2, m1, m2, obj_b % iflag, "
        "obj_a % obj_b % iflag, obj_b % obj_a % iflag)"
        in gen or
        "call invoke_0(f1, obj_a%iflag, f2, m1, m2, obj_b%iflag, "
        "obj_a%obj_b%iflag, obj_b%obj_a%iflag)"
        in gen)


def test_single_stencil():
    ''' test extent value is passed correctly from the algorithm layer '''
    path = os.path.join(BASE_PATH, "19.1_single_stencil.f90")
    alg, _ = generate(path, api="dynamo0.3")
    output = str(alg).lower()
    assert ("call invoke_0_testkern_stencil_type(f1, f2, f3, f4, "
            "f2_extent)" in output)


def test_single_stencil_broken():
    '''test we raise an exception when we do not pass a stencil argument
    '''
    path = os.path.join(BASE_PATH, "19.2_single_stencil_broken.f90")
    with pytest.raises(GenerationError) as excinfo:
        _, _ = generate(path, api="dynamo0.3")
    # TODO issue #1618 different error messages for the different versions.
    assert ("expected '5' arguments in the algorithm layer but found '4'"
            in str(excinfo.value) or "The invoke kernel functor "
            "'testkern_stencil_type' has 4 arguments, but the kernel "
            "metadata expects there to be 5 arguments." in str(excinfo.value))


def test_single_stencil_xory1d():
    '''test extent and dimension values are passed correctly from the
    algorithm layer when xory1d is specified'''
    path = os.path.join(BASE_PATH, "19.3_single_stencil_xory1d.f90")
    alg, _ = generate(path, api="dynamo0.3")
    output = str(alg).lower()
    assert ("call invoke_0_testkern_stencil_xory1d_type(f1, f2, "
            "f3, f4, f2_extent, f2_direction)") in output


def test_single_stencil_literal():
    ''' test extent value is passed correctly from the algorithm layer '''
    path = os.path.join(BASE_PATH, "19.4_single_stencil_literal.f90")
    alg, _ = generate(path, api="dynamo0.3")
    output = str(alg).lower()
    assert ("call invoke_0_testkern_stencil_type(f1, f2, f3, f4)"
            in output)


def test_single_stencil_xory1d_literal():
    '''test dimension value is recognised and not passed if either
    x_direction or y_direction'''
    path = os.path.join(
        BASE_PATH, "19.5_single_stencil_xory1d_literal.f90")
    alg, _ = generate(path, api="dynamo0.3")
    output = str(alg).lower()
    assert ("call invoke_0_testkern_stencil_xory1d_type(f1, f2, "
            "f3, f4)") in output


def test_multiple_stencils():
    '''more than one stencil in a kernel'''
    path = os.path.join(BASE_PATH, "19.7_multiple_stencils.f90")
    alg, _ = generate(path, api="dynamo0.3")
    output = str(alg).lower()
    assert ("call invoke_0_testkern_stencil_multi_type(f1, f2, "
            "f3, f4, f2_extent, f3_extent, f3_direction)") in output


def test_multiple_stencil_same_name_direction():
    ''' more than one stencil in a kernel with the same name for direction
    '''
    path = os.path.join(BASE_PATH, "19.9_multiple_stencils_same_name.f90")
    alg, _ = generate(path, api="dynamo0.3")
    output = str(alg).lower()
    assert ("call invoke_0_testkern_stencil_multi_2_type(f1, f2, "
            "f3, f4, extent, direction)") in output


def test_multiple_kernels_stencils():
    '''more than one kernel with stencils'''
    path = os.path.join(BASE_PATH, "19.10_multiple_kernels_stencils.f90")
    alg, _ = generate(path, api="dynamo0.3")
    output = str(alg).lower()
    # TODO issue #1618 Split test into two as while there are
    # different implementations we may or may not output a space
    # before the ':'
    assert "use multiple_stencil_psy, only" in output
    assert ": invoke_0" in output
    assert ("call invoke_0(f1, f2, f3, f4, f2_extent, f3_extent, extent, "
            "f3_direction, direction)") in output


def test_multiple_stencil_same_name_case():
    '''more than one stencil in a kernel with the same names but different
    case'''
    path = os.path.join(
        BASE_PATH, "19.11_multiple_stencils_mixed_case.f90")
    alg, _ = generate(path, api="dynamo0.3")
    output = str(alg).lower()
    assert ("call invoke_0_testkern_stencil_multi_2_type(f1, f2, "
            "f3, f4, extent, direction)") in output


def test_single_stencil_xory1d_scalar():
    '''test we raise an error if a value is passed for the direction
    argument'''
    path = os.path.join(BASE_PATH, "19.6_single_stencil_xory1d_value.f90")
    with pytest.raises(GenerationError) as excinfo:
        _, _ = generate(path, api="dynamo0.3")
    assert ("literal is not a valid value for a stencil direction"
            in str(excinfo.value))


def test_multiple_stencil_same_name():
    '''more than one stencil in a kernel with the same name for extent'''
    path = os.path.join(BASE_PATH, "19.8_multiple_stencils_same_name.f90")
    alg, _ = generate(path, api="dynamo0.3")
    output = str(alg).lower()
    assert ("call invoke_0_testkern_stencil_multi_type(f1, f2, "
            "f3, f4, extent, f3_direction)") in output


# Sample code for use in subsequent _adduse tests.
CODE = ("program test\n"
        "  integer :: i\n"
        "  i=0\n"
        "end program test\n")


# Utility function for parsing code, used in subsequent _adduse tests.
def get_parse_tree(code, parser):
    '''Utility function that takes Fortran code as a string and returns an
    fparser2 parse tree of the code. Pass in an instance of the parser
    so we don't create a new one each time this routine is called.

    :param str code: Fortran code in a string

    :param parser: An fparser2 program class.
    :type parser: :py:class:`fparser.two.Fortran2003.Program`

    :returns: parse tree of the supplied code.
    :rtype: :py:class:`fparser.two.utils.Base`

    '''
    reader = FortranStringReader(code)
    return parser(reader)


# Function _rm_kernel_use_stmts tests. These will be removed once the LFRic
# algorithm layer uses PSyIR (#1618).


def test_rm_kernel_use_stmts(parser):
    '''Tests for the _rm_kernel_use_stmts() method.'''
    code = ("program test\n"
            "  use my_kernel_mod, only: my_kernel_type\n"
            "  use kernel2_mod, only: kernel2_type, something_else\n"
            "contains\n"
            "  subroutine my_sub()\n"
            "    use a_kernel_mod, only: a_kernel_type\n"
            "  end subroutine my_sub\n"
            "end program test\n")
    parse_tree = get_parse_tree(code, parser)
    # An empty list of kernel names should be fine.
    alg_gen._rm_kernel_use_stmts([], parse_tree)
    gen = str(parse_tree).lower()
    assert "use my_kernel_mod, only: my_kernel_type" in gen
    assert "use kernel2_mod, only: kernel2_type, something_else" in gen
    assert "use a_kernel_mod, only: a_kernel_type" in gen
    # A kernel name that doesn't exist should be fine (because we need to
    # support builtins).
    alg_gen._rm_kernel_use_stmts(["my_builtin"], parse_tree)
    gen = str(parse_tree).lower()
    assert "use my_kernel_mod, only: my_kernel_type" in gen
    assert "use kernel2_mod, only: kernel2_type, something_else" in gen
    # Check that the use associated with a named kernel is removed.
    alg_gen._rm_kernel_use_stmts(["my_kernel_type"], parse_tree)
    gen = str(parse_tree).lower()
    assert "my_kernel_type" not in gen
    assert "use kernel2_mod, only: kernel2_type, something_else" in gen
    # Check that a use statement is not removed if it imports symbols other
    # than the named kernel.
    alg_gen._rm_kernel_use_stmts(["kernel2_type"], parse_tree)
    gen = str(parse_tree).lower()
    assert "use kernel2_mod, only: kernel2_type, something_else" in gen
    alg_gen._rm_kernel_use_stmts(["kernel2_type", "something_else"],
                                 parse_tree)
    # One Specification_Part should have been removed entirely.
    assert len(walk(parse_tree, Fortran2003.Specification_Part)) == 1
    gen = str(parse_tree).lower()
    assert "kernel2_type" not in gen
    assert "something_else" not in gen
    # Finally, check for the use in the nested subroutine.
    assert "use a_kernel_mod, only: a_kernel_type" in gen
    alg_gen._rm_kernel_use_stmts(["a_kernel_type"], parse_tree)
    assert not walk(parse_tree, Fortran2003.Specification_Part)
    gen = str(parse_tree).lower()
    assert "a_kernel_type" not in gen

# Function adduse tests. These will be removed once the LFRic algorithm
# layer uses PSyIR (#1618).


@pytest.mark.parametrize("location", [None, "lilliput"])
def test_adduse_invalid_location(location):
    '''Test that the expected exception is raised when the specified
    location is invalid.

    '''
    name = "my_use"
    with pytest.raises(GenerationError) as excinfo:
        alg_gen._adduse(location, name)
    assert ("Location argument must be a sub-class of fparser.two.utils.Base "
            "but got: " in str(excinfo.value))


def test_adduse_only_names1(parser):
    '''Test that the expected output is obtained in a Fortran program when
    variable/function names are provided for a use statement and only
    is True.

    '''
    parse_tree = get_parse_tree(CODE, parser)
    location = parse_tree.content[0].content[0]
    name = "my_use"

    alg_gen._adduse(location, name, only=True, funcnames=["a", "b", "c"])
    assert "PROGRAM test\n  USE my_use, ONLY: a, b, c\n  INTEGER :: i\n" \
        in str(parse_tree)


def test_adduse_only_names2(parser):
    '''Test that the expected output is obtained in a Fortran subroutine
    when variable/function names are provided for a use statement and
    only is True.

    '''
    parse_tree = get_parse_tree(
        "subroutine test()\n"
        "  integer :: i\n"
        "  i=0\n"
        "end subroutine test\n", parser)
    location = parse_tree.content[0].content[0]
    name = "my_use"

    alg_gen._adduse(location, name, only=True, funcnames=["a", "b", "c"])
    assert ("SUBROUTINE test\n  USE my_use, ONLY: a, b, c\n"
            "  INTEGER :: i\n") in str(parse_tree)


def test_adduse_only_names3(parser):
    '''Test that the expected output is obtained in a Fortran function
    when variable/function names are provided for a use statement and
    only is True.

    '''
    parse_tree = get_parse_tree(
        "integer function test()\n"
        "  integer :: i\n"
        "  return i\n"
        "end function test\n", parser)
    location = parse_tree.content[0].content[0]
    name = "my_use"

    alg_gen._adduse(location, name, only=True, funcnames=["a", "b", "c"])
    assert ("INTEGER FUNCTION test()\n  USE my_use, ONLY: a, b, c\n"
            "  INTEGER :: i\n") in str(parse_tree)


def test_adduse_only_nonames(parser):
    '''Test that the expected output is obtained when no variable/function
    names are provided for a use statement and only is True.

    '''
    parse_tree = get_parse_tree(CODE, parser)
    location = parse_tree.content[0].content[0]
    name = "my_use"

    alg_gen._adduse(location, name, only=True)
    assert "PROGRAM test\n  USE my_use, ONLY:\n  INTEGER :: i\n" \
        in str(parse_tree)


def test_adduse_noonly_names(parser):
    '''Test that the expected output is obtained when variable/function
    names are provided for a use statement and the value for the only
    argument is not provided.

    '''
    parse_tree = get_parse_tree(CODE, parser)
    location = parse_tree.content[0].content[0]
    name = "my_use"
    alg_gen._adduse(location, name, funcnames=["a", "b", "c"])
    assert ("PROGRAM test\n  USE my_use, ONLY: a, b, c\n"
            "  INTEGER :: i\n") in str(parse_tree)


def test_adduse_onlyfalse_names(parser):
    '''Test that an exception is raised when variable/function names are
    provided for a use statement and the value for the only argument
    is set to False.

    '''
    parse_tree = get_parse_tree(CODE, parser)
    location = parse_tree.content[0].content[0]
    name = "my_use"
    with pytest.raises(GenerationError) as excinfo:
        alg_gen._adduse(location, name, only=False, funcnames=["a", "b", "c"])
    assert ("If the 'funcnames' argument is provided and has content, "
            "then the 'only' argument must not be set to "
            "'False'.") in str(excinfo.value)


def test_adduse_noonly_nonames(parser):
    '''Test that the expected output is obtained when no variable/function
    names are provided for a use statement and only is not explicitly
    set.

    '''
    parse_tree = get_parse_tree(CODE, parser)
    location = parse_tree.content[0].content[0]
    name = "my_use"

    alg_gen._adduse(location, name)
    assert "PROGRAM test\n  USE my_use\n  INTEGER :: i\n" \
        in str(parse_tree)


def test_adduse_noprogparent(parser):
    '''Test that the expected exception is raised when the specified
    location has no parent that is one of main_program, module,
    subroutine or function.

    '''
    parse_tree = get_parse_tree(CODE, parser)
    # Choose the Program_Stmt node and then patch it so that it has
    # no parent
    location = parse_tree.content[0].content[0]
    location.parent = None
    name = "my_use"

    with pytest.raises(GenerationError) as excinfo:
        alg_gen._adduse(location, name)
    assert ("The specified location is invalid as it has no parent in the "
            "parse tree that is a program, module, subroutine or "
            "function.") in str(excinfo.value)


def test_adduse_unsupportedparent1(parser):
    '''Test that the expected exception is raised when the specified
    location has an ancestor that is a module.

    '''
    parse_tree = get_parse_tree(
        "module test\n"
        "  integer :: i\n"
        "end module test\n", parser)
    location = parse_tree.content[0].content[0]
    name = "my_use"

    with pytest.raises(NotImplementedError) as excinfo:
        alg_gen._adduse(location, name)
    assert ("Currently support is limited to program, subroutine and "
            "function.") in str(excinfo.value)


def test_adduse_nospec(parser):
    '''Test that the expected exception is raised when the ancestor (a
    program or a subroutine) does not have a specification part as its
    second child location has a parent that is a function. This is the
    case if a program has no content. This could be considered to be a
    bug but we'll treat it as a feature at the moment.

    '''
    parse_tree = get_parse_tree(
        "program test\n"
        "end program test\n", parser)
    location = parse_tree.content[0].content[0]
    name = "my_use"

    with pytest.raises(InternalError) as excinfo:
        alg_gen._adduse(location, name)
    assert ("The second child of the parent code (content[1]) is expected "
            "to be a specification part but found 'End_Program_Stmt"
            "('PROGRAM', Name('test'))'.") in str(excinfo.value)
