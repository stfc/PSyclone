# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2018, Science and Technology Facilities Council
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
# ----------------------------------------------------------------------------
# Author R. W. Ford, STFC Daresbury Lab

'''Stencil tests for PSy-layer code generation that are specific to the
GOcean 1.0 API.'''

from __future__ import absolute_import
import os
import pytest
from psyclone.parse import parse
from psyclone.psyGen import PSyFactory
from psyclone.generator import GenerationError, ParseError

from psyclone.gocean1p0 import GOStencil
from psyclone import expression as expr

API = "gocean1.0"
BASE_PATH = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                         "test_files", "gocean1p0")

# Section 1
# Tests for the case where an object of type GOStencil has not been
# initialised (i.e. the load method has not been called)


def create_stencil(stencil_string):
    '''This is a small helper function that creates a stencil and
    initialises it with the data in the given in the string. It does
    not do any exception handling, so any exceptions from parsing
    or loading the data will be passed on.
    :param str stencil_string: A valid stencil definition or stencil name.
    :returns A new GOStencil instance initialied with the given data.
    :rtype: :py:class:`psyclone.gocean1p0.GOStencil`
    '''
    stencil = GOStencil()
    parsed_stencil = expr.FORT_EXPRESSION.parseString(stencil_string)[0]
    stencil.load(parsed_stencil, "kernel_stencil")
    return stencil


def test_not_initialised():
    '''A GOStencil object can be created in isolation and then have its
    stencil information initialised using the load() method. If a
    GOStencil object's stencil information has not been initialised
    then asking for stencil information using the has_stencil, name
    and depth methods should return an exception. This test checks
    that an exception is raised as expected.

    '''
    stencil = GOStencil()

    with pytest.raises(GenerationError) as excinfo:
        _ = stencil.has_stencil
    assert "ensure the load() method is called" in str(excinfo.value)

    with pytest.raises(GenerationError) as excinfo:
        _ = stencil.name
    assert "ensure the load() method is called" in str(excinfo.value)

    with pytest.raises(GenerationError) as excinfo:
        _ = stencil.depth(0, 0)
    assert "ensure the load() method is called" in str(excinfo.value)

# Section 2
# Tests for the case where the load method in an object of type
# GOStencil is provided with invalid stencil information


def test_stencil_invalid_format_1():
    '''Check all the ways in which the 'stencil(...) format can be
    invalid

    '''
    stencil = GOStencil()

    # this should cause a general unexpected format error
    with pytest.raises(ParseError) as excinfo:
        stencil.load(None, "kernel_stencil")
    assert "expected either a name or the format 'go_stencil(...)" \
        in str(excinfo.value)

    # this should cause a general unexpected format error
    with pytest.raises(ParseError) as excinfo:
        stencil.load(stencil, "kernel_stencil")
    assert "expected either a name or the format 'go_stencil(...)" \
        in str(excinfo.value)

    # this should cause a general unexpected format error
    with pytest.raises(ParseError) as excinfo:
        create_stencil("(a)")
    assert "expected either a name or the format 'go_stencil(...)" \
        in str(excinfo.value)

    # this should cause an unsupported name error
    with pytest.raises(ParseError) as excinfo:
        create_stencil("random_name")
    assert ("argument is 'random_name' but must be one of ['go_pointwise'] or "
            "go_stencil(...)") in str(excinfo.value)

    # this should cause an unsupported name error
    with pytest.raises(ParseError) as excinfo:
        create_stencil("stenci(a)")
    assert "argument is 'stenci' but must be 'go_stencil(...)" \
        in str(excinfo.value)


def test_stencil_invalid_format_2():
    '''Check all the ways in which the arguments in the 'go_stencil(...)
    format can be invalid

    '''
    with pytest.raises(ParseError) as excinfo:
        create_stencil("go_stencil(a)")
    assert "format 'go_stencil(...)', has 1 arguments but should have 3" \

    # this should cause a not-a-number error
    with pytest.raises(ParseError) as excinfo:
        create_stencil("go_stencil(a,b,c)")
    assert "Argument index 0 should be a number but found 'a'" \
        in str(excinfo.value)

    # this should also cause a not-a-number error
    with pytest.raises(ParseError) as excinfo:
        create_stencil("go_stencil(000,x00,000)")
    assert "index 1 should be a number but found 'x00'" \
        in str(excinfo.value)

    # this should cause a not-3-numbers error
    with pytest.raises(ParseError) as excinfo:
        create_stencil("go_stencil(012,345,67)")
    assert "index 2 should consist of 3 digits but found 2" \
        in str(excinfo.value)

    # this should cause an invalid-middle-number error
    with pytest.raises(ParseError) as excinfo:
        create_stencil("go_stencil(012,345,678)")
    assert "index 1 position 1 should be a number from 0-1 but found 4" \
        in str(excinfo.value)

    # this should cause a zero-size-stencil error
    with pytest.raises(ParseError) as excinfo:
        create_stencil("go_stencil(000,010,000)")
    assert ("A zero sized stencil has been specified. This should be "
            "specified with the 'go_pointwise' keyword") in str(excinfo.value)

    # lastly, this should work as it is valid
    create_stencil("go_stencil(000,011,000)")

# Section 3 Test that GOStencil method arguments cause the object to
# raise an exception if they are invalid


def test_stencil_depth_args():
    '''Check that invalid index values for the depth method in an instance
    of the GOStencil class cause an exception to be raised.

    '''
    stencil = create_stencil("go_stencil(010,111,010)")
    for i, j in [(-2, 0), (2, 0), (0, -2), (0, 2)]:
        with pytest.raises(GenerationError) as excinfo:
            stencil.depth(i, j)
        assert "must be between -1 and 1 but found ({0},{1})".format(i, j) \
            in str(excinfo.value)

# Section 4
# Test that the GOStencil object captures valid stencil information correctly


def test_stencil_case_1():
    '''test that the metadata name 'go_stencil' can be provided in lower, or
    upper case.

    '''
    stencil = create_stencil("go_StEnCiL(000,011,000)")
    assert stencil.has_stencil
    for idx2 in range(-1, 2):
        for idx1 in range(-1, 2):
            if idx1 in [0, 1] and idx2 == 0:
                expected_depth = 1
            else:
                expected_depth = 0
            assert stencil.depth(idx1, idx2) == expected_depth


def test_stencil_case_2():
    '''test that the metadata name 'go_pointwise' can be provided in lower,
    or upper case

    '''

    stencil = create_stencil("go_pOiNtWiSe")
    assert not stencil.has_stencil
    assert stencil.name == "go_pointwise"


def test_stencil_information():
    '''Test that the GOStencil class provides the expected stencil
    information. This exercises the "pointwise" name and the stencil
    description

    '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "test28_invoke_kernel_stencil.f90"),
                           api=API)
    psy = PSyFactory(API).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    kernel = schedule.children[0].children[0].children[0]

    # args 1 and 3 specify pointwise as a stencil access
    for idx in [0, 2]:
        pointwise_arg = kernel.args[idx]
        assert pointwise_arg.stencil
        assert not pointwise_arg.stencil.has_stencil
        assert pointwise_arg.stencil.name == "go_pointwise"

    # arg 4 provides grid information so knows nothing about stencils
    grid_arg = kernel.args[3]
    with pytest.raises(AttributeError) as excinfo:
        _ = grid_arg.stencil
    assert "object has no attribute 'stencil'" in str(excinfo.value)

    # arg 2 has a stencil
    stencil_arg = kernel.args[1]
    assert stencil_arg.stencil.has_stencil
    for idx2 in range(-1, 2):
        for idx1 in range(-1, 2):
            if idx1 in [0, 1] and idx2 == 0:
                expected_depth = 1
            else:
                expected_depth = 0
            assert stencil_arg.stencil.depth(idx1, idx2) == expected_depth


def test_copy():
    '''Test that the stencil copy operation does indeed create a stand
    alone copy of the original and does not contain a reference to the
    original stencil data structure.'''

    stencil = create_stencil("go_stencil(000,111,000)")

    stencil_copy = stencil.copy()
    assert stencil_copy is not stencil
    assert stencil_copy.name == stencil.name
    assert stencil_copy._initialised == stencil._initialised
    assert stencil_copy.has_stencil == stencil.has_stencil
    assert stencil_copy._stencil == stencil._stencil

    # Even though stencil_copy and stencil are different objects (see assert
    # above), they could still share a sub-list object of stencil. So to be
    # very thorough, modify each stencil element in the copy and make sure
    # the original is not affected.
    for i in range(0, 3):
        for j in range(0, 3):
            stencil_copy._stencil[i][j] = (i+1)*100+j+1
            assert stencil_copy.depth(i-1, j-1) != stencil.depth(i-1, j-1)


def test_merge():
    '''This functions tests if merging two stencils works as expected.
    '''

    # The test values test all combinations of a value in stencil1
    # being smaller, equal and bigger than the corresponding value
    # in stencil2.

    stencil1 = create_stencil("go_stencil(123, 405, 321)")
    stencil2 = create_stencil("go_stencil(321, 504, 123)")
    stencil1.merge(stencil2)

    # The internal representation of the stencil is a list of colummns (while
    # in the string that is parsed it is a list of rows). To be independent
    # of the internal representation, we don't compare the lists directly,
    # but parse the expected merged stencil:
    stencil_result = create_stencil("go_stencil(323, 505, 323)")
    assert stencil1._stencil == stencil_result._stencil

    # Test the handling of pointwise stencils (i.e. no stencil)
    stencil1 = create_stencil("go_pointwise")
    stencil2 = create_stencil("go_stencil(321, 504, 123)")
    assert not stencil1.has_stencil
    stencil1.merge(stencil2)
    assert stencil1.has_stencil
