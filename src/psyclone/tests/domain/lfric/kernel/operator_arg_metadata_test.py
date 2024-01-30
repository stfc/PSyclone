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

'''Module containing tests for the OperatorArgMetadata class.

'''
import pytest

from fparser.two import Fortran2003

from psyclone.domain.lfric.kernel import OperatorArgMetadata


def test_create():
    '''Test that an instance of OperatorArgMetadata can be created
    successfully.

    '''
    operator_arg = OperatorArgMetadata("GH_REAL", "GH_READ", "W0", "W1")
    assert isinstance(operator_arg, OperatorArgMetadata)
    assert operator_arg.form == "gh_operator"
    assert operator_arg._datatype == "gh_real"
    assert operator_arg._access == "gh_read"
    assert operator_arg._function_space_to == "w0"
    assert operator_arg._function_space_from == "w1"


def test_init_invalid():
    '''Test that invalid function spaces supplied to the constructor raise
    the expected exceptions.

    '''
    with pytest.raises(ValueError) as info:
        _ = OperatorArgMetadata("GH_REAL", "GH_READ", "invalid", "W1")
    assert ("The 'function_space_to' metadata should be a recognised value "
            "(one of ['w3', 'wtheta', 'w2v', 'w2vtrace', 'w2broken', 'w0', "
            "'w1', 'w2', 'w2trace', 'w2h', 'w2htrace', 'any_w2', 'wchi', "
            "'any_space_1', 'any_space_2', 'any_space_3', 'any_space_4', "
            "'any_space_5', 'any_space_6', 'any_space_7', 'any_space_8', "
            "'any_space_9', 'any_space_10', 'any_discontinuous_space_1', "
            "'any_discontinuous_space_2', 'any_discontinuous_space_3', "
            "'any_discontinuous_space_4', 'any_discontinuous_space_5', "
            "'any_discontinuous_space_6', 'any_discontinuous_space_7', "
            "'any_discontinuous_space_8', 'any_discontinuous_space_9', "
            "'any_discontinuous_space_10']) but found 'invalid'."
            in str(info.value))

    with pytest.raises(ValueError) as info:
        _ = OperatorArgMetadata("GH_REAL", "GH_READ", "W0", "invalid")
    assert ("The 'function_space_from' metadata should be a recognised value "
            "(one of ['w3', 'wtheta', 'w2v', 'w2vtrace', 'w2broken', 'w0', "
            "'w1', 'w2', 'w2trace', 'w2h', 'w2htrace', 'any_w2', 'wchi', "
            "'any_space_1', 'any_space_2', 'any_space_3', 'any_space_4', "
            "'any_space_5', 'any_space_6', 'any_space_7', 'any_space_8', "
            "'any_space_9', 'any_space_10', 'any_discontinuous_space_1', "
            "'any_discontinuous_space_2', 'any_discontinuous_space_3', "
            "'any_discontinuous_space_4', 'any_discontinuous_space_5', "
            "'any_discontinuous_space_6', 'any_discontinuous_space_7', "
            "'any_discontinuous_space_8', 'any_discontinuous_space_9', "
            "'any_discontinuous_space_10']) but found 'invalid'."
            in str(info.value))


def test_get_metadata():
    '''Test that the get_metadata class method works as expected. Test
    that all relevant check and get methods are called by raising
    exceptions within them, as well as checking for valid input.

    '''
    fparser2_tree = OperatorArgMetadata.create_fparser2(
        "arg_type(GH_OPERATOR, GH_REAL, GH_READ, W0, W1)",
        Fortran2003.Part_Ref)
    datatype, access, function_space_to, function_space_from = \
        OperatorArgMetadata._get_metadata(fparser2_tree)
    assert datatype == "GH_REAL"
    assert access == "GH_READ"
    assert function_space_to == "W0"
    assert function_space_from == "W1"


def test_fortran_string():
    '''Test that the fortran_string method works as expected.'''

    fortran_string = "arg_type(GH_OPERATOR, GH_REAL, GH_READ, W0, W1)"
    operator_arg = OperatorArgMetadata.create_from_fortran_string(
        fortran_string)
    result = operator_arg.fortran_string()
    assert result == fortran_string.lower()


def test_check_datatype():
    '''Test the check_datatype method works as expected.'''
    OperatorArgMetadata.check_datatype("GH_REAL")
    with pytest.raises(ValueError) as info:
        OperatorArgMetadata.check_datatype("invalid")
    assert ("The 'datatype descriptor' metadata should be a recognised value "
            "(one of ['gh_real']) but found 'invalid'." in str(info.value))


def test_check_access():
    '''Test the check_access method works as expected.'''
    OperatorArgMetadata.check_access("GH_READ")
    with pytest.raises(ValueError) as info:
        OperatorArgMetadata.check_access("invalid")
    assert ("The 'access descriptor' metadata should be a recognised value "
            "(one of ['gh_read', 'gh_write', 'gh_readwrite']) but found "
            "'invalid'." in str(info.value))


def test_setter_getter():
    '''Test that the two function space setters and getters work as
    expected, including raising exceptions if values are invalid.

    '''
    operator_arg = OperatorArgMetadata("GH_REAL", "GH_READ", "W0", "W1")

    with pytest.raises(ValueError) as info:
        operator_arg.function_space_to = "invalid"
    assert ("The 'function_space_to' metadata should be a recognised value "
            "(one of ['w3', 'wtheta', 'w2v', 'w2vtrace', 'w2broken', 'w0', "
            "'w1', 'w2', 'w2trace', 'w2h', 'w2htrace', 'any_w2', 'wchi', "
            "'any_space_1', 'any_space_2', 'any_space_3', 'any_space_4', "
            "'any_space_5', 'any_space_6', 'any_space_7', 'any_space_8', "
            "'any_space_9', 'any_space_10', 'any_discontinuous_space_1', "
            "'any_discontinuous_space_2', 'any_discontinuous_space_3', "
            "'any_discontinuous_space_4', 'any_discontinuous_space_5', "
            "'any_discontinuous_space_6', 'any_discontinuous_space_7', "
            "'any_discontinuous_space_8', 'any_discontinuous_space_9', "
            "'any_discontinuous_space_10']) but found 'invalid'."
            in str(info.value))

    operator_arg.function_space_to = "w2"
    assert operator_arg.function_space_to == "w2"
    operator_arg.function_space_to = "W2"
    assert operator_arg.function_space_to == "w2"

    with pytest.raises(ValueError) as info:
        operator_arg.function_space_from = "invalid"
    assert ("The 'function_space_from' metadata should be a recognised value "
            "(one of ['w3', 'wtheta', 'w2v', 'w2vtrace', 'w2broken', 'w0', "
            "'w1', 'w2', 'w2trace', 'w2h', 'w2htrace', 'any_w2', 'wchi', "
            "'any_space_1', 'any_space_2', 'any_space_3', 'any_space_4', "
            "'any_space_5', 'any_space_6', 'any_space_7', 'any_space_8', "
            "'any_space_9', 'any_space_10', 'any_discontinuous_space_1', "
            "'any_discontinuous_space_2', 'any_discontinuous_space_3', "
            "'any_discontinuous_space_4', 'any_discontinuous_space_5', "
            "'any_discontinuous_space_6', 'any_discontinuous_space_7', "
            "'any_discontinuous_space_8', 'any_discontinuous_space_9', "
            "'any_discontinuous_space_10']) but found 'invalid'."
            in str(info.value))

    operator_arg.function_space_from = "w3"
    assert operator_arg.function_space_from == "w3"
    operator_arg.function_space_from = "W3"
    assert operator_arg.function_space_from == "w3"
