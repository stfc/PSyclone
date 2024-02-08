# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2019-2024, Science and Technology Facilities Council.
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
# Modified I. Kavcic, Met Office
# Modified C.M. Maynard, Met Office / University of Reading

'''A module to perform pytest unit tests on the parse/kernel.py
file. Some tests for this file are in parse_test.py. This file adds
tests for code that is not covered there.'''

import os
import pytest
from fparser.api import parse
from fparser import api as fpapi
from fparser.one.block_statements import BeginSource
from fparser.two import Fortran2003
from psyclone.domain.lfric.lfric_builtins import BUILTIN_MAP as builtins
from psyclone.domain.lfric.lfric_builtins import \
    BUILTIN_DEFINITIONS_FILE as fname
from psyclone.parse.kernel import KernelType, get_kernel_metadata, \
    get_kernel_interface, KernelProcedure, Descriptor, \
    BuiltInKernelTypeFactory, get_kernel_filepath, get_kernel_ast
from psyclone.parse.utils import ParseError
from psyclone.errors import InternalError

# pylint: disable=invalid-name

LFRIC_BASE_PATH = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                               os.path.pardir, "test_files", "dynamo0p3")
GOCEAN_BASE_PATH = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                                os.path.pardir, "test_files", "gocean1p0")

# Code fragment for testing standard kernel setup with
# a type-bound procedure.
CODE = (
    "module test_mod\n"
    "  type, extends(kernel_type) :: test_type\n"
    "    type(arg_type), dimension(1) :: meta_args =       &\n"
    "          (/ arg_type(gh_field, gh_real, gh_inc, w1) /)\n"
    "     integer :: operates_on = cell_column\n"
    "   contains\n"
    "     procedure, nopass :: code => test_code\n"
    "  end type test_type\n"
    "contains\n"
    "  subroutine test_code()\n"
    "  end subroutine test_code\n"
    "end module test_mod\n"
    )

# Code fragment for testing kernel which uses an interface
# instead of a type-bound procedure
CODE_INTERFACE = (
    "module test_mod\n"
    "  type, extends(kernel_type) :: test_type\n"
    "    type(arg_type), dimension(1) :: meta_args =       &\n"
    "          (/ arg_type(gh_field, gh_real, gh_inc, w1) /)\n"
    "     integer :: operates_on = cell_column\n"
    "   contains\n"
    "  end type test_type\n"
    "  interface test_code\n"
    "    module procedure sub_code\n"
    "  end interface test_code\n"
    "contains\n"
    "  subroutine sub_code()\n"
    "  end subroutine sub_code\n"
    "end module test_mod\n"

    )

# Code fragment for (failure) test for kernel with two
# interfaces and no type-bound procedure.
CODE_DOUBLE_INTERFACE = (
    "module test_mod\n"
    "  type, extends(kernel_type) :: test_type\n"
    "    type(arg_type), dimension(1) :: meta_args =       &\n"
    "          (/ arg_type(gh_field, gh_real, gh_inc, w1) /)\n"
    "     integer :: operates_on = cell_column\n"
    "   contains\n"
    "  end type test_type\n"
    "  interface test_code\n"
    "    module procedure sub_code\n"
    "  end interface test_code\n"
    "  interface test_code_again\n"
    "    module procedure more_code\n"
    "  end interface test_code_again\n"
    "contains\n"
    "  subroutine sub_code()\n"
    "  end subroutine sub_code\n"
    "end module test_mod\n"

    )

CODE_DOUBLE_PROCEDURE = (
    "module test_mod\n"
    "  type, extends(kernel_type) :: test_type\n"
    "    type(arg_type), dimension(1) :: meta_args =       &\n"
    "          (/ arg_type(gh_field, gh_real, gh_inc, w1) /)\n"
    "     integer :: operates_on = cell_column\n"
    "   contains\n"
    "  end type test_type\n"
    "  interface test_code\n"
    "    module procedure sub_code, more_code\n"
    "  end interface test_code\n"
    "contains\n"
    "  subroutine sub_code()\n"
    "  end subroutine sub_code\n"
    "  subroutine more_code()\n"
    "  end subroutine more_code\n"
    "end module test_mod\n"

    )


@pytest.fixture(scope="module", params=[CODE, CODE_INTERFACE],
                name="get_code_fragment")
def get_code_fragment_fixture(request):
    '''Fixture for testing two code versions.'''

    return request.param

# function get_kernel_filepath


def test_getkernelfilepath_nodir():
    '''Test that an appropriate exception is raised if the specified
    directory does not exist.

    '''
    with pytest.raises(ParseError) as excinfo:
        _ = get_kernel_filepath("test_mod", ["non/existent/file/path"], None)
    assert ("Supplied kernel search path does not exist or cannot be "
            "read") in str(excinfo.value)


def test_getkernelfilepath_multifile(tmpdir):
    '''Test that an appropriate exception is raised if more than one file
    matches when searching for kernels.

    '''
    filename = str(tmpdir.join("test_mod.f90"))
    ffile = open(filename, "w")
    ffile.write("")
    ffile.close()
    os.mkdir(str(tmpdir.join("tmp")))
    filename = str(tmpdir.join("tmp", "test_mod.f90"))
    ffile = open(filename, "w")
    ffile.write("")
    ffile.close()

    with pytest.raises(ParseError) as excinfo:
        _ = get_kernel_filepath("test_mod", [str(tmpdir)], None)
    assert ("More than one match for kernel file 'test_mod.[fF]90' "
            "found!") in str(excinfo.value)


def test_getkernelfilepath_nodir_supplied():
    '''Test that the directory of the algorithm file is searched if no
    directory is supplied.

    '''
    kern_module_name = "testkern_mod"
    alg_file_name = os.path.join(LFRIC_BASE_PATH, "1_single_invoke.f90")
    result = get_kernel_filepath(kern_module_name, [], alg_file_name)
    assert "testkern_mod.F90" in result


def test_getkernelfilepath_nomatch():
    '''Test that the expected exception is raised if the kernel file is
    not found in the supplied directory (or its descendents).

    '''
    kern_module_name = "testkern_mod"
    alg_file_name = os.path.join(LFRIC_BASE_PATH, "1_single_invoke.f90")
    with pytest.raises(ParseError) as info:
        get_kernel_filepath(
            kern_module_name, [GOCEAN_BASE_PATH], alg_file_name)
    assert ("Kernel file 'testkern_mod.[fF]90' not found in"
            in str(info.value))


def test_getkernelfilepath_multidir():
    '''Test that get_kernel_filepath works when multiple directories are
    supplied.

    '''
    kern_module_name = "testkern_mod"
    alg_file_name = os.path.join(LFRIC_BASE_PATH, "1_single_invoke.f90")
    result = get_kernel_filepath(
        kern_module_name, [GOCEAN_BASE_PATH, LFRIC_BASE_PATH], alg_file_name)
    assert "testkern_mod.F90" in result


def test_getkernelfilepath_caseinsensitive1(tmpdir):
    '''Test that a case insensitive match is performed when searching for
    kernels with a supplied kernel search path.

    '''
    os.mkdir(str(tmpdir.join("tmp")))
    filename = str(tmpdir.join("tmp", "test_mod.f90"))
    ffile = open(filename, "w")
    ffile.write("")
    ffile.close()
    result = get_kernel_filepath("TEST_MOD", [str(tmpdir)], None)
    assert "tmp" in result
    assert "test_mod.f90" in result


def test_getkernelfilepath_caseinsensitive2(tmpdir):
    '''Test that a case insensitive match is performed when searching for
    kernels without a supplied kernel search path.

    '''
    os.mkdir(str(tmpdir.join("tmp")))
    filename = str(tmpdir.join("tmp", "test_mod.f90"))
    ffile = open(filename, "w")
    ffile.write("")
    ffile.close()
    filename = str(tmpdir.join("tmp", "alg.f90"))
    ffile = open(filename, "w")
    ffile.write("")
    ffile.close()
    result = get_kernel_filepath("TEST_MOD", [], filename)
    assert "tmp" in result
    assert "test_mod.f90" in result

# function get_kernel_ast


def test_getkernelast_nodir():
    '''Test that the directory of the algorithm file is searched if no
    directory is supplied.

    '''
    kern_module_name = "testkern_mod"
    alg_file_name = os.path.join(LFRIC_BASE_PATH, "1_single_invoke.f90")
    result = get_kernel_ast(kern_module_name, alg_file_name, [], False)
    assert isinstance(result, BeginSource)


def test_getkernelast_nomatch():
    '''Test that the expected exception is raised if the kernel file is
    not found in the supplied directory (or its descendents).

    '''
    kern_module_name = "testkern_mod"
    alg_file_name = os.path.join(LFRIC_BASE_PATH, "1_single_invoke.f90")
    with pytest.raises(ParseError) as info:
        get_kernel_ast(
            kern_module_name, alg_file_name, [GOCEAN_BASE_PATH], False)
    assert ("Kernel file 'testkern_mod.[fF]90' not found in"
            in str(info.value))


def test_getkernelast_multidir():
    '''Test that get_kernel_ast works when multiple directories are
    supplied.

    '''
    kern_module_name = "testkern_mod"
    alg_file_name = os.path.join(LFRIC_BASE_PATH, "1_single_invoke.f90")
    result = get_kernel_ast(
        kern_module_name, alg_file_name, [GOCEAN_BASE_PATH, LFRIC_BASE_PATH],
        False)
    assert isinstance(result, BeginSource)

# function get_kernel_interface


def test_get_kernel_interface_no_match():
    '''Tests that get_kernel_interface() returns None when searching a
    parse tree that does not contain an interface.

    '''
    module_parse_tree = parse(CODE)
    kernel_type_name = "no_interface_found"
    meta1, meta2 = get_kernel_interface(kernel_type_name, module_parse_tree)
    assert meta1 is None
    assert meta2 is None


def test_get_kernel_interface_match_caseinsensitive():
    '''Tests that the interface name is case insensitive.'''

    module_parse_tree = parse(CODE_INTERFACE.replace("test_code", "TeST_CoDe"))
    kernel_type_name = "interface_found"
    meta1, meta2 = get_kernel_interface(kernel_type_name, module_parse_tree)
    assert meta1 == "test_code"
    assert meta2 is not None


def test_get_kernel_interface_match_no_name():
    '''Tests that the interface with no name returns None.'''

    module_parse_tree = parse(CODE_INTERFACE.replace("test_code", ""))
    kernel_type_name = "interface_withnoname"
    meta1, meta2 = get_kernel_interface(kernel_type_name, module_parse_tree)
    assert meta1 is None
    assert meta2 is None


def test_get_kernel_interface_match_correct():
    '''Tests that the get_kernel_interface has correct return when
    searching for an interface that defines more than one module
    procedure.

    '''
    module_parse_tree = parse(CODE_DOUBLE_PROCEDURE)
    kernel_type_name = "interface_procedures"
    meta1, meta2 = get_kernel_interface(kernel_type_name, module_parse_tree)
    assert meta1 == "test_code"
    assert meta2[0] == "sub_code"
    assert meta2[1] == "more_code"
    assert len(meta2) == 2


def test_two_module_procedures():
    '''Tests that 'None' is returned as the ast if there are more than one
    module procedure.

    '''
    kp = create_kernelprocedure(CODE_DOUBLE_PROCEDURE)
    assert kp.name == "test_code"
    assert kp.ast is None


def test_get_kernel_interface_double_interface():
    '''Tests that parse error occurs when the parse tree contains two
    interfaces.

    '''
    module_parse_tree = parse(CODE_DOUBLE_INTERFACE)
    kernel_type_name = "double_interface_kernel"
    with pytest.raises(ParseError) as excinfo:
        _, _ = get_kernel_interface(kernel_type_name, module_parse_tree)
    assert "Module containing kernel double_interface_kernel has more than "\
           "one interface, this is forbidden in the LFRic API."\
           in str(excinfo.value)


# function get_kernel_metadata
def test_get_kernel_metadata_no_match(get_code_fragment):
    '''Test that we get a ParseError when searching for a kernel that does
    not exist in the parse tree.

    '''
    module_parse_tree = parse(get_code_fragment)
    kernel_type_name = "no_matching_kernel"
    with pytest.raises(ParseError) as excinfo:
        get_kernel_metadata(
            kernel_type_name, module_parse_tree)
    assert ('Kernel type no_matching_kernel does not exist'
            in str(excinfo.value))


def test_get_kernel_metadata_match_case_insensitive(get_code_fragment):
    '''Test that searching for a kernel is not dependent upon the case of
    the name.

    '''
    module_parse_tree = parse(get_code_fragment)
    kernel_type_name = "TeSt_TyPe"
    meta = get_kernel_metadata(kernel_type_name, module_parse_tree)
    # Make sure we found it.
    assert meta is not None


# class BuiltInKernelTypeFactory():create test


def test_builtinfactory_metadataerror(monkeypatch):
    '''Test that an appropriate exception is raised if the builtin
    metadata cannot be parsed. This is difficult to trigger so use
    monkeypatch. We just need to make the call to the parse function
    raise an exception. This can be simply done by setting it to None
    (which gives a TypeError as it is not callable).

    '''
    monkeypatch.setattr(fpapi, "parse", None)
    factory = BuiltInKernelTypeFactory()
    with pytest.raises(ParseError) as excinfo:
        _ = factory.create(builtins.keys(), fname, "setval_c")
    assert "Failed to parse the meta-data for PSyclone built-ins" \
        in str(excinfo.value)

# class Descriptor() test


def test_descriptor_constructor():
    '''
    Test the constructor of the Descriptor() class.

    '''
    obj = Descriptor("gh_write", "w3", 4)
    assert isinstance(obj, Descriptor)
    assert obj.access == "gh_write"
    assert obj.function_space == "w3"
    assert obj.metadata_index == 4
    with pytest.raises(InternalError) as err:
        Descriptor("gh_write", "w2", -1)
    assert ("metadata index must be an integer and greater than or equal to "
            "zero but got: -1" in str(err.value))


def test_descriptor_repr():
    '''Test that the __repr__ method in Descriptor() behaves as expected.

    '''
    tmp = Descriptor("gh_inc", "w1", 2)
    assert repr(tmp) == "Descriptor(gh_inc, w1, 2)"


# class KernelProcedure() test utility
def create_kernelprocedure(code):
    '''Support function that attempts to create an instance of the
    'KernelProcedure' class. It is assumed that the name of the
    metadata is 'test_type' which matches the example code in the
    'CODE' variable. The code is passed in by argument as it might be
    necessary to modify it beforehand.

    :param str code: kernel code

    :returns: An instance of the KernelProcedure class
    :rtype: :py:class:`psyclone.parse.kernel.KernelProcedure`

    '''
    module_parse_tree = parse(code)
    kernel_type_name = "test_type"
    kernel_type_parse_tree = get_kernel_metadata(kernel_type_name,
                                                 module_parse_tree)
    return KernelProcedure(kernel_type_parse_tree, kernel_type_name,
                           module_parse_tree)

# class KernelProcedure():get_procedure tests


def test_kernelprocedure_notfound():
    '''Test that the appropriate exception is raised if the kernel
    subroutine specified in the kernel metadata does not exist in the
    module.

    '''
    my_code = CODE.replace("=> test_code", "=> non_existent_code")
    with pytest.raises(ParseError) as excinfo:
        _ = create_kernelprocedure(my_code)
    assert "Kernel subroutine 'non_existent_code' not found." \
        in str(excinfo.value)


def test_kernelinterface_notfound():
    '''Test that the appropriate exception is raised if the kernel
    subroutine specified in the kernel metadata does not exist in the
    module.

    '''
    with pytest.raises(ParseError) as excinfo:
        my_code = CODE_INTERFACE.replace(
            "module procedure sub_code",
            "module procedure sub_code, non_existent_code")
        _ = create_kernelprocedure(my_code)
    assert "Kernel subroutine 'non_existent_code' not found." \
        in str(excinfo.value)


# class KernelProcedure() tests
def test_kernelprocedure_repr(get_code_fragment):
    '''Test that the __repr__ method in KernelProcedure() behaves as
    expected.

    '''
    tmp = create_kernelprocedure(get_code_fragment)
    assert repr(tmp) == ("KernelProcedure(test_code)")


def test_kernelprocedure_str():
    '''Test that the __str__ method in KernelProcedure() behaves as
    expected.

    '''
    tmp = create_kernelprocedure(CODE)
    assert str(tmp) == ("    SUBROUTINE test_code()\n"
                        "    END SUBROUTINE test_code")

# class KernelType():getkerneldescriptors tests


def test_kerneltype_typename():
    '''Test that an exception is raised if the metadata type name is not
    what was expected.

    '''
    my_code = CODE.replace("meta_args", "invalid_name")
    parse_tree = parse(my_code)

    with pytest.raises(ParseError) as excinfo:
        _ = KernelType(parse_tree)
    assert "No variable named 'meta_args' found in the metadata for" \
        in str(excinfo.value)


def test_kerneltype_array():
    '''Test that an exception is raised if the metadata variable is not an
    array.

    '''
    my_code = CODE.replace(", dimension(1)", "")
    parse_tree = parse(my_code)

    with pytest.raises(ParseError) as excinfo:
        _ = KernelType(parse_tree)
    assert ("In kernel metadata 'test_type': 'meta_args' variable must be "
            "an array.") in str(excinfo.value)


def test_kerneltype_dimensions():
    '''Test that an exception is raised if the metadata variable is a
    multi-dimensional array.

    '''
    my_code = CODE.replace("dimension(1)", "dimension(1,1)")
    parse_tree = parse(my_code)

    with pytest.raises(ParseError) as excinfo:
        _ = KernelType(parse_tree)
    assert ("In kernel metadata 'test_type': 'meta_args' variable must be a "
            "1 dimensional array") in str(excinfo.value)


def test_kerneltype_brackets():
    '''Test that an exception is raised if the metadata is supplied within
    square brackets as this is not supported within the parser.

    '''
    my_code = CODE.replace("(/", "[")
    my_code = my_code.replace("/)", "]")
    parse_tree = parse(my_code)

    with pytest.raises(ParseError) as excinfo:
        _ = KernelType(parse_tree)
    assert ("Parser does not currently support '[...]' initialisation for "
            "'meta_args', please use '(/.../)' instead.") in str(excinfo.value)


def test_kerneltype_nargs():
    '''Test that an exception is raised if the number of arguments does
    not match the specified number of arguments in the kernel
    metadata.

    '''
    my_code = CODE.replace("dimension(1)", "dimension(2)")
    parse_tree = parse(my_code)

    with pytest.raises(ParseError) as excinfo:
        _ = KernelType(parse_tree)
    assert ("In the 'meta_args' metadata, the number of items in the array "
            "constructor (1) does not match the extent of the array (2)"
            in str(excinfo.value))


def test_kerneltype_repr():
    '''Test that the __repr__ method in KernelType() behaves as
    expected.

    '''
    parse_tree = parse(CODE)

    tmp = KernelType(parse_tree)
    assert repr(tmp) == "KernelType(test_type, cell_column)"


@pytest.mark.parametrize('operates', ["cell_column", "dof"])
def test_kerneltype_operates_on(operates):
    '''Test the parsing of the 'operates_on' metadata element.'''

    code = CODE.replace("cell_column", operates)
    parse_tree = parse(code)
    ktype = KernelType(parse_tree)
    assert ktype.iterates_over == operates
    # Check that the parsing is not case sensitive
    code = CODE.replace("cell_column", operates.upper())
    parse_tree = parse(code)
    ktype = KernelType(parse_tree)
    assert ktype.iterates_over == operates


def test_kerneltype_both_operates_on_iterates_over():
    '''Check that KernelType raises the expected error if the kernel
    metadata specifies *both* operates_on and iterates_over (the
    GOcean API uses iterates_over while LFRic uses operates_on).

    TODO #1204 this test can be removed once the check for this metadata
    has been moved into the API-specific subclasses.

    '''
    code = CODE.replace(
        "   contains\n",
        "     integer :: iterates_over = cell_column\n"
        "   contains\n")
    parse_tree = parse(code)
    with pytest.raises(ParseError) as err:
        KernelType(parse_tree)
    assert ("kernel 'test_type' contains both 'operates_on' and "
            "'iterates_over'" in str(err.value))


# Meta-data specifying quadrature
DIFF_BASIS = '''
module dummy_mod
  type, extends(kernel_type) :: dummy_type
     type(arg_type), meta_args(2) =                               &
          (/ arg_type(gh_field,    gh_real, gh_inc,       w0),    &
             arg_type(gh_operator, gh_real, gh_readwrite, w1, w1) &
           /)
     type(func_type), meta_funcs(2) =          &
          (/ func_type(w0, gh_diff_basis),     &
             func_type(w1, gh_basis)           &
           /)
     integer :: operates_on = cell_column
     integer :: gh_shape(2) = (/gh_quadrature_XYoZ, gh_quadrature_edge/)
   contains
     procedure, nopass :: code => dummy_code
  end type dummy_type
contains
  subroutine dummy_code()
  end subroutine dummy_code
end module dummy_mod
'''


def test_get_integer_variable():
    '''Test that the KernelType get_integer_variable method works as
    expected.

    '''
    parse_tree = parse(DIFF_BASIS)
    tmp = KernelType(parse_tree)
    # Check that we return None if the matched name is an array
    assert tmp.get_integer_variable("GH_SHAPE") is None
    assert tmp.get_integer_variable("gh_shape") is None

    new_code = DIFF_BASIS.replace(
        "integer :: gh_shape(2) = (/gh_quadrature_XYoZ, gh_quadrature_edge/)",
        "integer :: gh_shape = gh_quadrature_face")
    parse_tree = parse(new_code)
    tmp = KernelType(parse_tree)
    assert tmp.get_integer_variable("GH_SHAPE") == "gh_quadrature_face"
    assert tmp.get_integer_variable("Gh_Shape") == "gh_quadrature_face"


def test_get_integer_variable_err():
    '''Tests that we raise the expected error if the meta-data contains
    an integer literal instead of a name.

    '''
    mdata = DIFF_BASIS.replace("= cell_column", "= 1")
    ast = parse(mdata, ignore_comments=False)
    with pytest.raises(ParseError) as err:
        _ = KernelType(ast)
    assert ("RHS of assignment is not a variable name: 'operates_on = 1'" in
            str(err.value))


def test_get_integer_array():
    '''Test that the KernelType get_integer_array method works as
    expected.

    '''
    parse_tree = parse(DIFF_BASIS)
    tmp = KernelType(parse_tree)
    assert tmp.get_integer_array("gh_shape") == ['gh_quadrature_xyoz',
                                                 'gh_quadrature_edge']
    assert tmp.get_integer_array("GH_SHAPE") == ['gh_quadrature_xyoz',
                                                 'gh_quadrature_edge']
    new_code = DIFF_BASIS.replace(
        "(/gh_quadrature_XYoZ, gh_quadrature_edge/)",
        "[gh_quadrature_XYoZ, gh_quadrature_edge]")
    parse_tree = parse(new_code)
    tmp = KernelType(parse_tree)
    assert tmp.get_integer_array("GH_SHAPE") == ['gh_quadrature_xyoz',
                                                 'gh_quadrature_edge']

    new_code = DIFF_BASIS.replace("gh_shape(2)", "gh_shape(3)")
    parse_tree = parse(new_code)
    tmp = KernelType(parse_tree)
    with pytest.raises(ParseError) as err:
        tmp.get_integer_array("gh_shape")
    assert ("declared length of array 'gh_shape' is 3 but constructor only "
            "contains 2 names: 'gh_shape" in str(err.value))

    # Use variable name instead of integer to dimension array
    new_code = DIFF_BASIS.replace("gh_shape(2)", "gh_shape(npts)")
    parse_tree = parse(new_code)
    tmp = KernelType(parse_tree)
    with pytest.raises(ParseError) as err:
        tmp.get_integer_array("gh_shape")
    assert ("array extent must be specified using an integer literal but "
            "found 'npts' for array 'gh_shape'" in str(err.value))

    # Only 1D arrays are supported
    new_code = DIFF_BASIS.replace("gh_shape(2)", "gh_shape(2,2)")
    parse_tree = parse(new_code)
    tmp = KernelType(parse_tree)
    with pytest.raises(ParseError) as err:
        tmp.get_integer_array("gh_shape")
    assert ("array must be 1D but found an array with 2 dimensions for name "
            "'gh_shape'" in str(err.value))

    # Break RHS so that it is not an array constructor
    new_code = DIFF_BASIS.replace(
        "(/gh_quadrature_XYoZ, gh_quadrature_edge/)",
        "gh_quadrature_XYoZ")
    parse_tree = parse(new_code)
    tmp = KernelType(parse_tree)
    with pytest.raises(ParseError) as err:
        tmp.get_integer_array("gh_shape")
    assert "RHS of assignment is not an array constructor" in str(err.value)

    # Check that we return an empty list if the matched name is a scalar
    new_code = DIFF_BASIS.replace(
        "integer :: gh_shape(2) = (/gh_quadrature_XYoZ, gh_quadrature_edge/)",
        "integer :: gh_shape = gh_quadrature_face")
    parse_tree = parse(new_code)
    tmp = KernelType(parse_tree)
    assert tmp.get_integer_array("gh_shape") == []


def test_get_int_array_name_err(monkeypatch):
    '''Tests that we raise the correct error if there is something wrong
    with the Name in the assignment statement obtained from
    fparser2.

    '''
    # This is difficult as we have to break the result returned by fparser2.
    # We therefore create a valid KernelType object
    ast = parse(DIFF_BASIS, ignore_comments=False)
    ktype = KernelType(ast)
    # Next we create a valid fparser2 result
    my_assign = Fortran2003.Assignment_Stmt("my_array(2) = [1, 2]")
    # Break its `items` property by replacing the Name object with a string
    # (tuples are immutable so make a new one)
    broken_items = tuple(["invalid"] + list(my_assign.items[1:]))

    # Use monkeypatch to ensure that that the Assignment_Stmt that
    # is returned when we attempt to use fparser2 from within the
    # routine under test now has the broken tuple of items.

    def my_init(self, _):
        ''' dummy class '''
        self.items = broken_items
    monkeypatch.setattr(Fortran2003.Assignment_Stmt, "__init__", my_init)

    with pytest.raises(InternalError) as err:
        _ = ktype.get_integer_array("gh_evaluator_targets")
    assert ("Unsupported assignment statement: 'invalid = [1, 2]'"
            in str(err.value))


def test_get_int_array_constructor_err(monkeypatch):
    '''Check that we raise the appropriate error if we fail to parse the
    array constructor expression.

    '''

    # First create a valid KernelType object
    ast = parse(DIFF_BASIS, ignore_comments=False)
    ktype = KernelType(ast)
    # Create a valid fparser2 result
    assign = Fortran2003.Assignment_Stmt("gh_evaluator_targets(2) = [1, 2]")
    # Break the array constructor expression (tuples are immutable so make a
    # new one)
    assign.items[2].items[1].items = tuple(["hello", "goodbye"])

    # Use monkeypatch to ensure that that's the result that is returned
    # when we attempt to use fparser2 from within the routine under test

    def my_init(self, _):
        ''' dummy class '''
        self.items = assign.items
    monkeypatch.setattr(Fortran2003.Assignment_Stmt, "__init__", my_init)
    with pytest.raises(InternalError) as err:
        _ = ktype.get_integer_array("gh_evaluator_targets")
    assert ("Failed to parse array constructor: '[hello, goodbye]'"
            in str(err.value))


def test_get_int_array_section_subscript_err(monkeypatch):
    '''Check that we raise the appropriate error if the parse tree for
    the LHS of the array declaration is broken.

    '''
    # First create a valid KernelType object
    ast = parse(DIFF_BASIS, ignore_comments=False)
    ktype = KernelType(ast)
    # Create a valid fparser2 result
    assign = Fortran2003.Assignment_Stmt("gh_evaluator_targets(2) = [1, 2]")
    # Break the array constructor expression by replacing the
    # Section_Subscript_List with a str
    assign.children[0].items = (assign.children[0].items[0], "hello")

    # Use monkeypatch to ensure that that's the result that is returned
    # when we attempt to use fparser2 from within the routine under test

    def my_init(self, _):
        ''' dummy constructor '''
        self.items = assign.items
    monkeypatch.setattr(Fortran2003.Assignment_Stmt, "__init__", my_init)

    with pytest.raises(InternalError) as err:
        _ = ktype.get_integer_array("gh_evaluator_targets")
    assert ("expected array declaration to have a Section_Subscript_List but "
            "found" in str(err.value))
