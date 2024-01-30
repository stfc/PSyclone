# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2022-2024, Science and Technology Facilities Council
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
# Author R. W. Ford, STFC Daresbury Lab

'''Module containing tests for the MetaFuncsArgMetadata class.

'''
import pytest

from fparser.two import Fortran2003

from psyclone.domain.lfric.kernel import MetaFuncsArgMetadata
from psyclone.parse.utils import ParseError


def test_create():
    '''Test that an instance of MetaFuncsArgMetadata can be created
    successfully.

    '''
    funcs_arg = MetaFuncsArgMetadata("W0", basis_function=True)
    assert isinstance(funcs_arg, MetaFuncsArgMetadata)
    assert funcs_arg.function_space == "w0"


def test_init_values():
    '''Test MetaFuncsArgMetadata optional constructor arguments.'''

    with pytest.raises(ValueError) as info:
        _ = MetaFuncsArgMetadata("w0")
    assert ("At least one of basis_function or diff_basis_function must be "
            "set to True." in str(info.value))

    funcs_arg = MetaFuncsArgMetadata("w1", basis_function=True)
    assert funcs_arg.function_space == "w1"
    assert funcs_arg.basis_function is True
    assert funcs_arg.diff_basis_function is False

    funcs_arg = MetaFuncsArgMetadata("w2", diff_basis_function=True)
    assert funcs_arg.function_space == "w2"
    assert funcs_arg.basis_function is False
    assert funcs_arg.diff_basis_function is True


def test_create_from_fparser2_errors():
    '''Test that the create_from_fparser2 static method works as
    expected. Test that all relevant check and get methods are called
    by raising exceptions within them, as well as checking for valid
    input (with varying case).

    '''
    with pytest.raises(TypeError) as info:
        _ = MetaFuncsArgMetadata.create_from_fparser2("hello")
    assert ("Expected kernel metadata to be encoded as an fparser2 "
            "Part_Ref object but found type 'str' with value 'hello'."
            in str(info.value))

    fparser2_tree = MetaFuncsArgMetadata.create_fparser2(
        "hello(x)", Fortran2003.Part_Ref)

    with pytest.raises(ValueError) as info:
        _ = MetaFuncsArgMetadata.create_from_fparser2(fparser2_tree)
    assert ("Expected kernel metadata to have the name 'func_type' and be "
            "in the form 'func_type(...)', but found 'hello(x)'."
            in str(info.value))

    fparser2_tree = MetaFuncsArgMetadata.create_fparser2(
        "func_type(x)", Fortran2003.Part_Ref)
    with pytest.raises(ParseError) as info:
        _ = MetaFuncsArgMetadata.create_from_fparser2(fparser2_tree)
    assert ("There must be at least 2 arguments: a function_space and one "
            "of basis_function or diff_basis_function, but found '1' "
            "arguments." in str(info.value))

    fparser2_tree = MetaFuncsArgMetadata.create_fparser2(
        "func_type(x, y, z, a)", Fortran2003.Part_Ref)
    with pytest.raises(ParseError) as info:
        _ = MetaFuncsArgMetadata.create_from_fparser2(fparser2_tree)
    assert ("There must be at most 3 arguments: function_space, "
            "basis_function and diff_basis_function, but found '4'."
            in str(info.value))

    # the validity of the function space is checked in the constructor
    # - after the validity of the basis function values.
    fparser2_tree = MetaFuncsArgMetadata.create_fparser2(
        "func_type(invalid, x)", Fortran2003.Part_Ref)
    with pytest.raises(ValueError) as info:
        _ = MetaFuncsArgMetadata.create_from_fparser2(fparser2_tree)
    assert ("The 'basis or differential basis' metadata should be a "
            "recognised value (one of ['gh_basis', 'gh_diff_basis']) "
            "but found 'x'." in str(info.value))

    fparser2_tree = MetaFuncsArgMetadata.create_fparser2(
        "func_type(W0, invalid)", Fortran2003.Part_Ref)
    with pytest.raises(ValueError) as info:
        _ = MetaFuncsArgMetadata.create_from_fparser2(fparser2_tree)
    assert ("The 'basis or differential basis' metadata should be a "
            "recognised value (one of ['gh_basis', 'gh_diff_basis']) "
            "but found 'invalid'." in str(info.value))

    fparser2_tree = MetaFuncsArgMetadata.create_fparser2(
        "func_type(W0, GH_BASIS, invalid)", Fortran2003.Part_Ref)
    with pytest.raises(ValueError) as info:
        _ = MetaFuncsArgMetadata.create_from_fparser2(fparser2_tree)
    assert ("The 'basis or differential basis' metadata should be a "
            "recognised value (one of ['gh_basis', 'gh_diff_basis']) "
            "but found 'invalid'." in str(info.value))

    fparser2_tree = MetaFuncsArgMetadata.create_fparser2(
        "func_type(W0, GH_BASIS, gh_basis)", Fortran2003.Part_Ref)
    with pytest.raises(ParseError) as info:
        _ = MetaFuncsArgMetadata.create_from_fparser2(fparser2_tree)
    assert ("The same basis or differential basis function value should not "
            "be repeated, but found 'GH_BASIS' twice." in str(info.value))


@pytest.mark.parametrize("args, basis_function, diff_basis_function",
                         [("GH_basis", True, False),
                          ("GH_basis, GH_diff_basis", True, True),
                          ("GH_diff_basis", False, True),
                          ("GH_diff_basis, GH_basis", True, True)])
def test_create_from_fparser2(args, basis_function, diff_basis_function):
    '''Test that the create_from_fparser2 static method works as
    expected. Test the optional values. Also check with varying case.

    '''
    fparser2_tree = MetaFuncsArgMetadata.create_fparser2(
        f"func_type(w0, {args})", Fortran2003.Part_Ref)
    funcs_arg = MetaFuncsArgMetadata.create_from_fparser2(fparser2_tree)
    assert funcs_arg.basis_function == basis_function
    assert funcs_arg.diff_basis_function == diff_basis_function


@pytest.mark.parametrize("args", ["gh_basis", "gh_basis, gh_diff_basis",
                                  "gh_diff_basis", "gh_basis"])
def test_fortran_string(args):
    '''Test that the fortran_string method works as expected with the
    different combinations of the optional arguments.

    '''
    fortran_string = f"func_type(w0, {args})"
    funcs_arg = MetaFuncsArgMetadata.create_from_fortran_string(fortran_string)
    result = funcs_arg.fortran_string()
    assert result == fortran_string


def test_function_space_setter_getter():
    '''Test that the function space setter and getter work as expected,
    including raising an exception if the value is invalid.

    '''
    funcs_arg = MetaFuncsArgMetadata("w0", basis_function=True)
    with pytest.raises(ValueError) as info:
        funcs_arg.function_space = "invalid"
    assert ("The 'function_space' metadata should be a recognised value (one "
            "of ['w3', 'wtheta', 'w2v', 'w2vtrace', 'w2broken', 'w0', 'w1', "
            "'w2', 'w2trace', 'w2h', 'w2htrace', 'any_w2', 'wchi', "
            "'any_space_1', 'any_space_2', 'any_space_3', 'any_space_4', "
            "'any_space_5', 'any_space_6', 'any_space_7', 'any_space_8', "
            "'any_space_9', 'any_space_10', 'any_discontinuous_space_1', "
            "'any_discontinuous_space_2', 'any_discontinuous_space_3', "
            "'any_discontinuous_space_4', 'any_discontinuous_space_5', "
            "'any_discontinuous_space_6', 'any_discontinuous_space_7', "
            "'any_discontinuous_space_8', 'any_discontinuous_space_9', "
            "'any_discontinuous_space_10']) but found 'invalid'."
            in str(info.value))
    funcs_arg.function_space = "W1"
    assert funcs_arg.function_space == "w1"
    # any_space function spaces are also allowed
    funcs_arg.function_space = "any_space_1"
    assert funcs_arg.function_space == "any_space_1"


def test_basis_function_setter_getter():
    '''Test that the basis function setter and getter work as expected,
    including raising any exceptions.

    '''
    funcs_arg = MetaFuncsArgMetadata("w0", basis_function=True)
    with pytest.raises(TypeError) as info:
        funcs_arg.basis_function = "invalid"
    assert ("The basis_function argument should be a boolean but found "
            "'str'." in str(info.value))

    with pytest.raises(ValueError) as info:
        funcs_arg.basis_function = False
    assert ("At least one of basis_function or diff_basis_function must "
            "be set to True." in str(info.value))

    funcs_arg = MetaFuncsArgMetadata("w0", diff_basis_function=True)
    assert funcs_arg.basis_function is False
    funcs_arg.basis_function = True
    assert funcs_arg.basis_function is True


def test_diff_basis_function_setter_getter():
    '''Test that the differential basis function setter and getter work as
    expected, including raising any exceptions.

    '''
    funcs_arg = MetaFuncsArgMetadata("w0", diff_basis_function=True)
    with pytest.raises(TypeError) as info:
        funcs_arg.diff_basis_function = "invalid"
    assert ("The diff_basis_function argument should be a boolean but "
            "found 'str'" in str(info.value))

    with pytest.raises(ValueError) as info:
        funcs_arg.diff_basis_function = False
    assert ("At least one of basis_function or diff_basis_function must be "
            "set to True." in str(info.value))

    funcs_arg = MetaFuncsArgMetadata("w0", basis_function=True)
    assert funcs_arg.diff_basis_function is False
    funcs_arg.diff_basis_function = True
    assert funcs_arg.diff_basis_function is True


def test_check_constraints():
    '''Test the internal check_constraints method works as expected.'''
    with pytest.raises(ValueError) as info:
        MetaFuncsArgMetadata._check_constraints(False, False)
    assert ("At least one of basis_function or diff_basis_function must be "
            "set to True." in str(info.value))
    MetaFuncsArgMetadata._check_constraints(True, False)
    MetaFuncsArgMetadata._check_constraints(False, True)
    MetaFuncsArgMetadata._check_constraints(True, True)
