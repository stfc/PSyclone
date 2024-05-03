# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017-2024, Science and Technology Facilities Council.
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


import os
import pytest
from fparser import api as fpapi
from psyclone.parse.algorithm import parse, ParseError
from psyclone.parse.kernel import KernelType, KernelTypeFactory, \
    BuiltInKernelTypeFactory
from psyclone.errors import InternalError

TEST_PATH = os.path.join(os.path.dirname(os.path.abspath(__file__)), "..",
                         "test_files", "dynamo0p3")


def test_default_api():
    ''' Check that parse() picks up the default API if none is specified
    by the caller. We do this simply by checking that it returns OK
    having parsed some dynamo0.3 code. '''
    _, invoke_info = parse(
        os.path.join(TEST_PATH, "1_single_invoke.f90"))
    assert len(invoke_info.calls) == 1


def test_continuators_kernel():
    '''Tests that an input kernel file with long lines that already has
       continuators to make the code conform to the line length limit
       does not cause an error. '''
    _, _ = parse(os.path.join(TEST_PATH, "1.1.0_single_invoke_xyoz_qr.f90"),
                 api="dynamo0.3", line_length=True)


def test_continuators_algorithm():
    '''Tests that an input algorithm file with long lines that already has
       continuators to make the code conform to the line length limit
       does not cause an error. '''
    _, _ = parse(os.path.join(TEST_PATH, "13.2_alg_long_line_continuator.f90"),
                 api="dynamo0.3", line_length=True)


def test_get_builtin_defs_wrong_api():
    ''' Check that we raise an appropriate error if we call
    get_builtin_defs() with an invalid API '''
    import psyclone.parse.algorithm as pparse
    with pytest.raises(ParseError) as excinfo:
        _, _ = pparse.get_builtin_defs('invalid_api')
    assert "check_api: Unsupported API 'invalid_api'" in str(excinfo.value)


def test_kerneltypefactory_wrong_api():
    ''' Check that we raise an appropriate error if we try to create
    a KernelTypeFactory with an invalid API '''
    with pytest.raises(ParseError) as excinfo:
        _ = KernelTypeFactory(api="invalid_api")
    assert "check_api: Unsupported API 'invalid_api'" in str(excinfo.value)


def test_kerneltypefactory_default_api():
    ''' Check that the KernelTypeFactory correctly defaults to using
    the default API '''
    from psyclone.configuration import Config
    _config = Config.get()
    factory = KernelTypeFactory(api="")
    assert factory._type == _config.default_api


def test_kerntypefactory_create_broken_type():
    ''' Check that we raise an error if the KernelTypeFactory.create()
    method encounters an unrecognised API. '''
    factory = KernelTypeFactory(api="")
    # Deliberately break the 'type' (API) of this factory
    factory._type = "invalid_api"
    test_builtin_name = "aX_plus_Y"
    with pytest.raises(ParseError) as excinfo:
        _ = factory.create(None, name=test_builtin_name.lower())
    assert ("KernelTypeFactory:create: Unsupported kernel type"
            in str(excinfo.value))


def test_broken_builtin_metadata():
    ''' Check that we raise an appropriate error if there is a problem
    with the meta-data describing the built-ins for a given API '''
    from psyclone.domain.lfric import lfric_builtins
    # The file containing broken meta-data for the built-ins
    test_builtin_name = "aX_plus_Y"
    defs_file = os.path.join(TEST_PATH, "broken_builtins_mod.f90")
    factory = BuiltInKernelTypeFactory(api="dynamo0.3")
    with pytest.raises(ParseError) as excinfo:
        _ = factory.create(lfric_builtins.BUILTIN_MAP,
                           defs_file, name=test_builtin_name.lower())
    assert ("Failed to parse the meta-data for PSyclone built-ins in" in
            str(excinfo.value))


def test_unrecognised_builtin():
    ''' Check that we raise an error if we call the BuiltInKernelTypeFactory
    with an unrecognised built-in name '''
    from psyclone.domain.lfric import lfric_builtins
    factory = BuiltInKernelTypeFactory()
    with pytest.raises(ParseError) as excinfo:
        _ = factory.create(lfric_builtins.BUILTIN_MAP,
                           None,
                           name="not_a_builtin")
    assert ("unrecognised built-in name. Got 'not_a_builtin' but"
            in str(excinfo.value))


def test_builtin_with_use():
    ''' Check that we raise an error if we encounter a use statement for
    a built-in operation '''
    with pytest.raises(ParseError) as excinfo:
        _, _ = parse(
            os.path.join(TEST_PATH, "15.12.2_builtin_with_use.f90"),
            api="dynamo0.3")
    assert ("A built-in cannot be named in a use statement but "
            "'setval_c' is used from module 'fake_builtin_mod' in "
            in str(excinfo.value))


def test_too_many_names_invoke():
    ''' Test that we raise the expected error when the invoke contains
    more than one name=xxx argument. '''
    with pytest.raises(ParseError) as err:
        _, _ = parse(
            os.path.join(TEST_PATH, "1.0.2_many_named_invoke.f90"),
            api="dynamo0.3")
    assert "An invoke must contain one or zero " in str(err.value)
    assert "1.0.2_many_named_invoke.f90" in str(err.value)


def test_wrong_named_invoke():
    ''' Test that we raise the expected error when the invoke contains
    a named argument where the argument is not called 'name' '''
    with pytest.raises(ParseError) as err:
        _, _ = parse(
            os.path.join(TEST_PATH, "1.0.3_wrong_named_arg_invoke.f90"),
            api="dynamo0.3")
    assert ("Expected named identifier to be 'name' but found "
            "'not_a_name'" in str(err.value))


def test_wrong_type_named_invoke():
    ''' Test that we raise the expected error when the invoke contains
    a named argument but its value is not a string '''
    with pytest.raises(ParseError) as err:
        _, _ = parse(
            os.path.join(TEST_PATH, "1.0.4_wrong_type_named_arg_invoke.f90"),
            api="dynamo0.3")
    assert ("The (optional) name of an invoke must be specified as a "
            "string" in str(err.value))
    assert "1.0.4_wrong_type_named_arg_invoke.f90" in str(err.value)


def test_invalid_named_invoke():
    ''' Test that we raise the expected error when the invoke contains
    a named argument but its value is not a valid Fortran name '''
    with pytest.raises(ParseError) as err:
        _, _ = parse(
            os.path.join(TEST_PATH, "1.0.6_invoke_name_invalid_chars.f90"),
            api="dynamo0.3")
    assert ("the (optional) name of an invoke must be a string containing a "
            "valid Fortran name (with no whitespace) but "
            "got 'jack(1)' " in str(err.value))
    assert "1.0.6_invoke_name_invalid_chars.f90" in str(err.value)


def test_duplicate_named_invoke():
    ''' Test that we raise the expected error when an algorithm file
    contains two invokes that are given the same name '''
    with pytest.raises(ParseError) as err:
        _, _ = parse(os.path.join(
            TEST_PATH, "3.3_multi_functions_multi_invokes_name_clash.f90"),
                     api="dynamo0.3")
    assert ("Found multiple named invoke()'s with the same label ('jack') "
            "when parsing " in str(err.value))
    assert "3.3_multi_functions_multi_invokes_name_clash.f90" in str(err.value)


def test_duplicate_named_invoke_case():
    ''' Test that we raise the expected error when an algorithm file
    contains two invokes that are given the same name but with different
    case. '''
    with pytest.raises(ParseError) as err:
        _, _ = parse(os.path.join(
            TEST_PATH, "3.4_multi_invoke_name_clash_case_insensitive.f90"),
                     api="dynamo0.3")
    assert ("Found multiple named invoke()'s with the same label ('jack') "
            "when parsing " in str(err.value))
    assert "3.4_multi_invoke_name_clash_case_insensitive.f90" in str(err.value)


def test_get_stencil():
    ''' Check that parse.get_stencil() raises the correct errors when
    passed various incorrect inputs. '''
    from psyclone.parse.kernel import get_stencil
    from psyclone.expression import ExpressionNode, FunctionVar
    enode = ExpressionNode(["1"])
    with pytest.raises(ParseError) as excinfo:
        _ = get_stencil(enode, ["cross"])
    assert ("Expecting format stencil(<type>[,<extent>]) but found the "
            "literal" in str(excinfo.value))
    node = FunctionVar(["stencil()"])
    with pytest.raises(ParseError) as excinfo:
        _ = get_stencil(node, ["cross"])
    assert ("Expecting format stencil(<type>[,<extent>]) but found stencil()"
            in str(excinfo.value))
    node = FunctionVar(["stencil", "cross"])
    # Deliberately break the args member of node in order to trigger an
    # internal error
    node.args = [True]
    with pytest.raises(ParseError) as excinfo:
        _ = get_stencil(node, ["cross"])
    assert ("expecting either FunctionVar or str from the expression analyser"
            in str(excinfo.value))


MDATA = '''
module testkern_eval_mod
  type, extends(kernel_type) :: testkern_eval_type
    type(arg_type) :: meta_args(2) = (/            &
         arg_type(GH_FIELD, GH_REAL, GH_INC,  W0), &
         arg_type(GH_FIELD, GH_REAL, GH_READ, W1)  &
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


def test_kernel_binding_not_code():
    ''' Check that we raise the expected error when Kernel meta-data uses
    a specific binding but does not have 'code' as the generic name. '''
    mdata = MDATA.replace("code => test", "my_code => test")
    ast = fpapi.parse(mdata)
    with pytest.raises(ParseError) as err:
        _ = KernelType(ast)
    assert ("binds to a specific procedure but does not use 'code' as the "
            "generic name" in str(err.value))


def test_kernel_binding_missing():
    ''' Check that we raise the correct error when the Kernel meta-data is
    missing the type-bound procedure giving the name of the subroutine. '''
    mdata = MDATA.replace(
        "contains\n    procedure, nopass :: code => testkern_eval_code\n", "")
    ast = fpapi.parse(mdata)
    with pytest.raises(ParseError) as err:
        _ = KernelType(ast)
    assert ("Kernel type testkern_eval_type does not bind a specific "
            "procedure" in str(err.value))


def test_empty_kernel_name(monkeypatch):
    ''' Check that we raise the correct error when we get a blank string for
    the name of the Kernel subroutine. '''
    import fparser
    mdata = MDATA.replace("procedure, nopass :: code => testkern_eval_code",
                          "procedure, nopass :: testkern_eval_code")
    ast = fpapi.parse(mdata)
    # Break the AST
    for statement, _ in fpapi.walk(ast, -1):
        if isinstance(statement, fparser.one.statements.SpecificBinding):
            monkeypatch.setattr(statement, "name", "")
            break
    with pytest.raises(InternalError) as err:
        _ = KernelType(ast)
    assert ("Empty Kernel name returned for Kernel type testkern_eval_type"
            in str(err.value))
