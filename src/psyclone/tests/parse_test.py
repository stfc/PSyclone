# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017-2018, Science and Technology Facilities Council.
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

''' A module to perform pytest unit and functional tests on the parse
function. '''


from __future__ import absolute_import, print_function
import os
import pytest
from fparser import api as fpapi
from psyclone.parse import parse, ParseError
from psyclone.psyGen import InternalError


def test_default_api():
    ''' Check that parse() picks up the default API if none is specified
    by the caller. We do this simply by checking that it returns OK
    having parsed some dynamo0.3 code. '''
    _, invoke_info = parse(
        os.path.join(os.path.dirname(os.path.abspath(__file__)),
                     "test_files", "dynamo0p3", "1_single_invoke.f90"))
    assert len(list(invoke_info.calls.keys())) == 1


def test_dm_not_bool():
    ''' Check that we raise the correct error if the distributed_memory
    argument is not a bool '''
    with pytest.raises(ParseError) as err:
        _, __info = parse(
            os.path.join(os.path.dirname(os.path.abspath(__file__)),
                         "test_files", "dynamo0p3", "1_single_invoke.f90"),
            distributed_memory="a string")
    assert ("The distributed_memory flag in parse() must be set to 'True' "
            "or 'False'" in str(err))


def test_continuators_kernel():
    '''Tests that an input kernel file with long lines that already has
       continuators to make the code conform to the line length limit
       does not cause an error. '''
    _, _ = parse(os.path.join(os.path.dirname(os.path.abspath(__file__)),
                              "test_files", "dynamo0p3",
                              "1.1.0_single_invoke_xyoz_qr.f90"),
                 api="dynamo0.3", line_length=True)


def test_continuators_algorithm():
    '''Tests that an input algorithm file with long lines that already has
       continuators to make the code conform to the line length limit
       does not cause an error. '''
    _, _ = parse(os.path.join(os.path.dirname(os.path.abspath(__file__)),
                              "test_files", "dynamo0p3",
                              "13.2_alg_long_line_continuator.f90"),
                 api="dynamo0.3", line_length=True)


def test_get_builtin_defs_wrong_api():
    ''' Check that we raise an appropriate error if we call
    get_builtin_defs() with an invalid API '''
    import psyclone.parse as pparse
    with pytest.raises(ParseError) as excinfo:
        _, _ = pparse.get_builtin_defs('invalid_api')
    assert "check_api: Unsupported API 'invalid_api'" in str(excinfo.value)


def test_kerneltypefactory_wrong_api():
    ''' Check that we raise an appropriate error if we try to create
    a KernelTypeFactory with an invalid API '''
    from psyclone.parse import KernelTypeFactory
    with pytest.raises(ParseError) as excinfo:
        _ = KernelTypeFactory(api="invalid_api")
    assert "check_api: Unsupported API 'invalid_api'" in str(excinfo.value)


def test_kerneltypefactory_default_api():
    ''' Check that the KernelTypeFactory correctly defaults to using
    the default API '''
    from psyclone.parse import KernelTypeFactory
    from psyclone.configuration import Config
    _config = Config.get()
    factory = KernelTypeFactory(api="")
    assert factory._type == _config.default_api


def test_kerntypefactory_create_broken_type():
    ''' Check that we raise an error if the KernelTypeFactory.create()
    method encounters an unrecognised API. '''
    from psyclone.parse import KernelTypeFactory
    factory = KernelTypeFactory(api="")
    # Deliberately break the 'type' (API) of this factory
    factory._type = "invalid_api"
    test_builtin_name = "aX_plus_Y"
    with pytest.raises(ParseError) as excinfo:
        _ = factory.create(None, name=test_builtin_name.lower())
    assert ("KernelTypeFactory: Internal Error: Unsupported kernel type"
            in str(excinfo.value))


def test_broken_builtin_metadata():
    ''' Check that we raise an appropriate error if there is a problem
    with the meta-data describing the built-ins for a given API '''
    from psyclone import dynamo0p3_builtins
    # The file containing broken meta-data for the built-ins
    test_builtin_name = "aX_plus_Y"
    defs_file = os.path.join(
        os.path.dirname(os.path.abspath(__file__)),
        "test_files", "dynamo0p3", "broken_builtins_mod.f90")
    from psyclone.parse import BuiltInKernelTypeFactory
    factory = BuiltInKernelTypeFactory(api="dynamo0.3")
    with pytest.raises(ParseError) as excinfo:
        _ = factory.create(dynamo0p3_builtins.BUILTIN_MAP,
                           defs_file, name=test_builtin_name.lower())
    assert ("Failed to parse the meta-data for PSyclone built-ins in" in
            str(excinfo.value))


def test_unrecognised_builtin():
    ''' Check that we raise an error if we call the BuiltInKernelTypeFactory
    with an unrecognised built-in name '''
    from psyclone import dynamo0p3_builtins
    from psyclone.parse import BuiltInKernelTypeFactory
    factory = BuiltInKernelTypeFactory()
    with pytest.raises(ParseError) as excinfo:
        _ = factory.create(dynamo0p3_builtins.BUILTIN_MAP,
                           None,
                           name="not_a_builtin")
    assert ("unrecognised built-in name. Got 'not_a_builtin' but"
            in str(excinfo.value))


def test_builtin_with_use():
    ''' Check that we raise an error if we encounter a use statement for
    a built-in operation '''
    with pytest.raises(ParseError) as excinfo:
        _, _ = parse(
            os.path.join(os.path.dirname(os.path.abspath(__file__)),
                         "test_files", "dynamo0p3",
                         "15.12.2_builtin_with_use.f90"),
            api="dynamo0.3")
    assert ("A built-in cannot be named in a use statement but "
            "'setval_c' is used from module 'fake_builtin_mod' in "
            in str(excinfo.value))


def test_element_unpack():
    ''' Check that the unpack method of the Element class behaves as
    expected when passed a string '''
    from psyclone.parse import Element
    ele = Element()
    output = ele.unpack("andy")
    assert str(output) == "andy"


def test_too_many_names_invoke():
    ''' Test that we raise the expected error when the invoke contains
    more than one name=xxx argument. '''
    with pytest.raises(ParseError) as err:
        _, _ = parse(
            os.path.join(os.path.dirname(os.path.abspath(__file__)),
                         "test_files", "dynamo0p3",
                         "1.0.2_many_named_invoke.f90"),
            api="dynamo0.3")
    assert "An invoke must contain one or zero " in str(err)
    assert "1.0.2_many_named_invoke.f90" in str(err)


def test_wrong_named_invoke():
    ''' Test that we raise the expected error when the invoke contains
    a named argument where the argument is not called 'name' '''
    with pytest.raises(ParseError) as err:
        _, _ = parse(
            os.path.join(os.path.dirname(os.path.abspath(__file__)),
                         "test_files", "dynamo0p3",
                         "1.0.3_wrong_named_arg_invoke.f90"),
            api="dynamo0.3")
    print(str(err))
    assert (
        "The arguments to an invoke() must be either kernel calls or an "
        "(optional) name=" in str(err))


def test_wrong_type_named_invoke():
    ''' Test that we raise the expected error when the invoke contains
    a named argument but its value is not a string '''
    with pytest.raises(ParseError) as err:
        _, _ = parse(
            os.path.join(os.path.dirname(os.path.abspath(__file__)),
                         "test_files", "dynamo0p3",
                         "1.0.4_wrong_type_named_arg_invoke.f90"),
            api="dynamo0.3")
    assert ("The (optional) name of an invoke must be specified as a "
            "string" in str(err))
    assert "1.0.4_wrong_type_named_arg_invoke.f90" in str(err)


def test_invalid_named_invoke():
    ''' Test that we raise the expected error when the invoke contains
    a named argument but its value is not a valid Fortran name '''
    with pytest.raises(ParseError) as err:
        _, _ = parse(
            os.path.join(os.path.dirname(os.path.abspath(__file__)),
                         "test_files", "dynamo0p3",
                         "1.0.6_invoke_name_invalid_chars.f90"),
            api="dynamo0.3")
    assert ("The (optional) name of an invoke must be a string containing a "
            "valid Fortran name (with any spaces replaced by underscores) but "
            "got 'ja_ck(1)' " in str(err))
    assert "1.0.6_invoke_name_invalid_chars.f90" in str(err)


def test_duplicate_named_invoke():
    ''' Test that we raise the expected error when an algorithm file
    contains two invokes that are given the same name '''
    with pytest.raises(ParseError) as err:
        _, _ = parse(
            os.path.join(os.path.dirname(os.path.abspath(__file__)),
                         "test_files", "dynamo0p3",
                         "3.3_multi_functions_multi_invokes_name_clash.f90"),
            api="dynamo0.3")
    print(str(err))
    assert ("Found multiple named invoke()'s with the same name ('jack') "
            "when parsing " in str(err))
    assert "3.3_multi_functions_multi_invokes_name_clash.f90" in str(err)


def test_duplicate_named_invoke_case():
    ''' Test that we raise the expected error when an algorithm file
    contains two invokes that are given the same name but with different
    case. '''
    with pytest.raises(ParseError) as err:
        _, _ = parse(
            os.path.join(os.path.dirname(os.path.abspath(__file__)),
                         "test_files", "dynamo0p3",
                         "3.4_multi_invoke_name_clash_case_insensitive.f90"),
            api="dynamo0.3")
    assert ("Found multiple named invoke()'s with the same name ('jack') "
            "when parsing " in str(err))
    assert "3.4_multi_invoke_name_clash_case_insensitive.f90" in str(err)


def test_get_stencil():
    ''' Check that parse.get_stencil() raises the correct errors when
    passed various incorrect inputs. '''
    from psyclone.parse import get_stencil
    from psyclone.expression import ExpressionNode, FunctionVar
    enode = ExpressionNode(["1"])
    with pytest.raises(ParseError) as excinfo:
        _ = get_stencil(enode, ["cross"])
    assert ("Expecting format stencil(<type>[,<extent>]) but found the "
            "literal" in str(excinfo))
    node = FunctionVar(["stencil()"])
    with pytest.raises(ParseError) as excinfo:
        _ = get_stencil(node, ["cross"])
    assert ("Expecting format stencil(<type>[,<extent>]) but found stencil()"
            in str(excinfo))
    node = FunctionVar(["stencil", "cross"])
    # Deliberately break the args member of node in order to trigger an
    # internal error
    node.args = [True]
    with pytest.raises(ParseError) as excinfo:
        _ = get_stencil(node, ["cross"])
    assert ("expecting either FunctionVar or str from the expression analyser"
            in str(excinfo))


MDATA = '''
module testkern_eval_mod
  type, extends(kernel_type) :: testkern_eval_type
    type(arg_type) :: meta_args(2) = (/       &
         arg_type(GH_FIELD,   GH_WRITE,  W0), &
         arg_type(GH_FIELD,   GH_READ, W1)    &
         /)
    type(func_type) :: meta_funcs(2) = (/     &
         func_type(W0, GH_BASIS),             &
         func_type(W1, GH_DIFF_BASIS)         &
         /)
    integer :: gh_shape = gh_evaluator
    integer :: gh_evaluator_targets(2) = [W0, W1]
    integer :: iterates_over = cells
  contains
    procedure, nopass :: code => testkern_eval_code
  end type testkern_eval_type
contains
  subroutine testkern_eval_code()
  end subroutine testkern_eval_code
end module testkern_eval_mod
'''


def test_get_int():
    ''' Tests for the KernelType.get_integer(). method '''
    from psyclone.parse import KernelType
    ast = fpapi.parse(MDATA, ignore_comments=False)
    ktype = KernelType(ast)
    iter_val = ktype.get_integer_variable("iterates_over")
    assert iter_val == "cells"


def test_get_int_err():
    ''' Tests that we raise the expected error if the meta-data contains
    an integer literal instead of a name. '''
    from psyclone.parse import KernelType
    mdata = MDATA.replace("= cells", "= 1")
    ast = fpapi.parse(mdata, ignore_comments=False)
    with pytest.raises(ParseError) as err:
        _ = KernelType(ast)
    assert ("RHS of assignment is not a variable name: 'iterates_over = 1'" in
            str(err))


def test_get_int_array():
    ''' Tests for the KernelType.get_integer_array() method. '''
    from psyclone.parse import KernelType
    ast = fpapi.parse(MDATA, ignore_comments=False)
    ktype = KernelType(ast)
    targets = ktype.get_integer_array("gh_evaluator_targets")
    assert targets == ["w0", "w1"]
    mdata = MDATA.replace("[W0, W1]", "(/W0, W1/)")
    ast = fpapi.parse(mdata, ignore_comments=False)
    ktype = KernelType(ast)
    targets = ktype.get_integer_array("gh_evaluator_targets")
    assert targets == ["w0", "w1"]


def test_get_int_array_err1(monkeypatch):
    ''' Tests that we raise the correct error if there is something wrong
    with the assignment statement obtained from fparser2. '''
    from psyclone.parse import KernelType
    from fparser.two import Fortran2003
    # This is difficult as we have to break the result returned by fparser2.
    # We therefore create a valid KernelType object
    ast = fpapi.parse(MDATA, ignore_comments=False)
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
        self.items = broken_items
    monkeypatch.setattr(Fortran2003.Assignment_Stmt, "__init__", my_init)

    with pytest.raises(InternalError) as err:
        _ = ktype.get_integer_array("gh_evaluator_targets")
    assert "Unsupported assignment statement: 'invalid = [1, 2]'" in str(err)


def test_get_int_array_not_array():
    ''' Test that get_integer_array returns the expected error if the
    requested variable is not an array. '''
    from psyclone.parse import KernelType
    ast = fpapi.parse(MDATA, ignore_comments=False)
    ktype = KernelType(ast)
    # Erroneously call get_integer_array with the name of a scalar meta-data
    # entry
    with pytest.raises(ParseError) as err:
        _ = ktype.get_integer_array("iterates_over")
    assert ("RHS of assignment is not an array constructor: 'iterates_over = "
            "cells'" in str(err))


def test_get_int_array_err2(monkeypatch):
    ''' Check that we raise the appropriate error if we fail to parse the
    array constructor expression. '''
    from psyclone.parse import KernelType
    from fparser.two import Fortran2003
    # First create a valid KernelType object
    ast = fpapi.parse(MDATA, ignore_comments=False)
    ktype = KernelType(ast)
    # Create a valid fparser2 result
    assign = Fortran2003.Assignment_Stmt("gh_evaluator_targets(2) = [1, 2]")
    # Break the array constructor expression (tuples are immutable so make a
    # new one)
    assign.items[2].items[1].items = tuple(["hello", "goodbye"])

    # Use monkeypatch to ensure that that's the result that is returned
    # when we attempt to use fparser2 from within the routine under test

    def my_init(self, _):
        self.items = assign.items
    monkeypatch.setattr(Fortran2003.Assignment_Stmt, "__init__", my_init)

    with pytest.raises(InternalError) as err:
        _ = ktype.get_integer_array("gh_evaluator_targets")
    assert "Failed to parse array constructor: '[hello, goodbye]'" in str(err)
