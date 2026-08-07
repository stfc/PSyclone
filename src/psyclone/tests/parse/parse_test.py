# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017-2026, Science and Technology Facilities Council.
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
from psyclone.parse.algorithm import parse, ParseError
from psyclone.parse.kernel import KernelTypeFactory, BuiltInKernelTypeFactory

TEST_PATH = os.path.join(os.path.dirname(os.path.abspath(__file__)), "..",
                         "test_files", "lfric")


def test_continuators_kernel():
    '''Tests that an input kernel file with long lines that already has
       continuators to make the code conform to the line length limit
       does not cause an error. '''
    _, _ = parse(os.path.join(TEST_PATH, "1.1.0_single_invoke_xyoz_qr.f90"),
                 api="lfric", line_length=True)


def test_continuators_algorithm():
    '''Tests that an input algorithm file with long lines that already has
       continuators to make the code conform to the line length limit
       does not cause an error. '''
    _, _ = parse(os.path.join(TEST_PATH, "13.2_alg_long_line_continuator.f90"),
                 api="lfric", line_length=True)


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


def test_kerntypefactory_create_broken_type():
    ''' Check that we raise an error if the KernelTypeFactory.create()
    method encounters an unrecognised API. '''
    factory = KernelTypeFactory(api="lfric")
    # Deliberately break the 'type' (API) of this factory
    factory._type = "invalid_api"
    test_builtin_name = "aX_plus_Y"
    with pytest.raises(ParseError) as excinfo:
        _ = factory.create(None, name=test_builtin_name.lower())
    assert ("KernelTypeFactory:create: Unsupported PSyIR-first kernel type"
            in str(excinfo.value))


def test_broken_builtin_metadata():
    ''' Check that we raise an appropriate error if there is a problem
    with the meta-data describing the built-ins for a given API '''
    from psyclone.domain.lfric import lfric_builtins
    # The file containing broken meta-data for the built-ins
    test_builtin_name = "aX_plus_Y"
    defs_file = os.path.join(TEST_PATH, "broken_builtins_mod.f90")
    factory = BuiltInKernelTypeFactory(api="lfric")
    with pytest.raises(ParseError) as excinfo:
        _ = factory.create(lfric_builtins.BUILTIN_MAP,
                           defs_file, name=test_builtin_name.lower())
    assert ("Failed to parse the meta-data for PSyclone built-ins in" in
            str(excinfo.value))


def test_unrecognised_builtin():
    ''' Check that we raise an error if we call the BuiltInKernelTypeFactory
    with an unrecognised built-in name '''
    from psyclone.domain.lfric import lfric_builtins
    factory = BuiltInKernelTypeFactory("lfric")
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
            api="lfric")
    assert ("A built-in cannot be named in a use statement but "
            "'setval_c' is used from module 'fake_builtin_mod' in "
            in str(excinfo.value))


def test_too_many_names_invoke():
    ''' Test that we raise the expected error when the invoke contains
    more than one name=xxx argument. '''
    with pytest.raises(ParseError) as err:
        _, _ = parse(
            os.path.join(TEST_PATH, "1.0.2_many_named_invoke.f90"),
            api="lfric")
    assert "An invoke must contain one or zero " in str(err.value)
    assert "1.0.2_many_named_invoke.f90" in str(err.value)


def test_wrong_named_invoke():
    ''' Test that we raise the expected error when the invoke contains
    a named argument where the argument is not called 'name' '''
    with pytest.raises(ParseError) as err:
        _, _ = parse(
            os.path.join(TEST_PATH, "1.0.3_wrong_named_arg_invoke.f90"),
            api="lfric")
    assert ("Expected named identifier to be 'name' but found "
            "'not_a_name'" in str(err.value))


def test_wrong_type_named_invoke():
    ''' Test that we raise the expected error when the invoke contains
    a named argument but its value is not a string '''
    with pytest.raises(ParseError) as err:
        _, _ = parse(
            os.path.join(TEST_PATH, "1.0.4_wrong_type_named_arg_invoke.f90"),
            api="lfric")
    assert ("The (optional) name of an invoke must be specified as a "
            "string" in str(err.value))
    assert "1.0.4_wrong_type_named_arg_invoke.f90" in str(err.value)


def test_invalid_named_invoke():
    ''' Test that we raise the expected error when the invoke contains
    a named argument but its value is not a valid Fortran name '''
    with pytest.raises(ParseError) as err:
        _, _ = parse(
            os.path.join(TEST_PATH, "1.0.6_invoke_name_invalid_chars.f90"),
            api="lfric")
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
                     api="lfric")
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
                     api="lfric")
    assert ("Found multiple named invoke()'s with the same label ('jack') "
            "when parsing " in str(err.value))
    assert "3.4_multi_invoke_name_clash_case_insensitive.f90" in str(err.value)
