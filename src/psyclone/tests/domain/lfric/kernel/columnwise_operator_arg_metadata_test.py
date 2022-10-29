# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2022, Science and Technology Facilities Council
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

'''Module containing tests for the ColumnwiseOperatorArgMetadata class.

'''
import pytest

from fparser.two import Fortran2003

from psyclone.domain.lfric.kernel.columnwise_operator_arg_metadata import \
    ColumnwiseOperatorArgMetadata


def test_create():
    '''Test that an instance of ColumnwiseOperatorArgMetadata can be
    created successfully.

    '''
    operator_arg = ColumnwiseOperatorArgMetadata(
        "GH_REAL", "GH_READ", "W0", "W1")
    assert isinstance(operator_arg, ColumnwiseOperatorArgMetadata)
    assert operator_arg.form == "GH_COLUMNWISE_OPERATOR"
    assert operator_arg._datatype == "GH_REAL"
    assert operator_arg._access == "GH_READ"
    assert operator_arg._function_space_to == "W0"
    assert operator_arg._function_space_from == "W1"


def test_create_from_fparser2():
    '''Test that the create_from_fparser2 static method works as
    expected. Test that all relevant check and get methods are called
    by raising exceptions within them, as well as checking for valid
    input.

    '''
    with pytest.raises(TypeError) as info:
        _ = ColumnwiseOperatorArgMetadata.create_from_fparser2("hello")
    assert ("Expected kernel metadata to be encoded as an fparser2 "
            "Part_Ref object but found type 'str' with value 'hello'."
            in str(info.value))

    fparser2_tree = ColumnwiseOperatorArgMetadata.create_fparser2(
        "hello(x)", Fortran2003.Part_Ref)
    with pytest.raises(ValueError) as info:
        _ = ColumnwiseOperatorArgMetadata.create_from_fparser2(fparser2_tree)
    assert ("Expected kernel metadata to have the name 'arg_type' "
            "and be in the form 'arg_type(...)', but found 'hello(x)'."
            in str(info.value))

    fparser2_tree = ColumnwiseOperatorArgMetadata.create_fparser2(
        "arg_type(x)", Fortran2003.Part_Ref)
    with pytest.raises(ValueError) as info:
        _ = ColumnwiseOperatorArgMetadata.create_from_fparser2(fparser2_tree)
    assert ("Expected kernel metadata to have 5 arguments, but "
            "found 1 in 'arg_type(x)'." in str(info.value))

    fparser2_tree = ColumnwiseOperatorArgMetadata.create_fparser2(
        "arg_type(GH_UNWISE_OPERATOR, GH_REAL, GH_READ, W0, W1)",
        Fortran2003.Part_Ref)
    with pytest.raises(ValueError) as info:
        _ = ColumnwiseOperatorArgMetadata.create_from_fparser2(fparser2_tree)
    assert ("ColumnwiseOperators should have GH_COLUMNWISE_OPERATOR as their "
            "first metadata argument, but found 'GH_UNWISE_OPERATOR'."
            in str(info.value))

    fparser2_tree = ColumnwiseOperatorArgMetadata.create_fparser2(
        "arg_type(GH_COLUMNWISE_OPERATOR, GH_UNREAL, GH_READ, W0, W1)",
        Fortran2003.Part_Ref)
    with pytest.raises(ValueError) as info:
        _ = ColumnwiseOperatorArgMetadata.create_from_fparser2(fparser2_tree)
    assert ("At argument index '1' for metadata 'arg_type("
            "GH_COLUMNWISE_OPERATOR, GH_UNREAL, GH_READ, W0, W1)'. The "
            "datatype descriptor value should be one of ['gh_real'], but "
            "found 'GH_UNREAL'." in str(info.value))

    fparser2_tree = ColumnwiseOperatorArgMetadata.create_fparser2(
        "arg_type(GH_COLUMNWISE_OPERATOR, GH_REAL, GH_ERROR, W0, W1)",
        Fortran2003.Part_Ref)
    with pytest.raises(ValueError) as info:
        _ = ColumnwiseOperatorArgMetadata.create_from_fparser2(fparser2_tree)
    assert ("At argument index '2' for metadata 'arg_type("
            "GH_COLUMNWISE_OPERATOR, GH_REAL, GH_ERROR, W0, W1)'. The access "
            "descriptor value should be one of ['gh_read', 'gh_write', "
            "'gh_readwrite'], but found 'GH_ERROR'." in str(info.value))

    fparser2_tree = ColumnwiseOperatorArgMetadata.create_fparser2(
        "arg_type(GH_COLUMNWISE_OPERATOR, GH_REAL, GH_READ, XX, W1)",
        Fortran2003.Part_Ref)
    with pytest.raises(ValueError) as info:
        _ = ColumnwiseOperatorArgMetadata.create_from_fparser2(fparser2_tree)
    assert ("At argument index '3' for metadata 'arg_type("
            "GH_COLUMNWISE_OPERATOR, GH_REAL, GH_READ, XX, W1)'. The "
            "function_space_to value should be one of ['w3', 'wtheta', "
            "'w2v', 'w2vtrace', 'w2broken', 'w0', 'w1', 'w2', 'w2trace', "
            "'w2h', 'w2htrace', 'any_w2', 'wchi'], but found 'XX'."
            in str(info.value))

    fparser2_tree = ColumnwiseOperatorArgMetadata.create_fparser2(
        "arg_type(GH_COLUMNWISE_OPERATOR, GH_REAL, GH_READ, W0, YY)",
        Fortran2003.Part_Ref)
    with pytest.raises(ValueError) as info:
        _ = ColumnwiseOperatorArgMetadata.create_from_fparser2(fparser2_tree)
    assert ("At argument index '4' for metadata 'arg_type("
            "GH_COLUMNWISE_OPERATOR, GH_REAL, GH_READ, W0, YY)'. The "
            "function_space_from value should be one of ['w3', 'wtheta', "
            "'w2v', 'w2vtrace', 'w2broken', 'w0', 'w1', 'w2', 'w2trace', "
            "'w2h', 'w2htrace', 'any_w2', 'wchi'], but found 'YY'."
            in str(info.value))

    fparser2_tree = ColumnwiseOperatorArgMetadata.create_fparser2(
        "arg_type(GH_COLUMNWISE_OPERATOR, GH_REAL, GH_READ, W0, W1)",
        Fortran2003.Part_Ref)
    operator_arg = ColumnwiseOperatorArgMetadata.create_from_fparser2(
        fparser2_tree)
    assert operator_arg.form == "GH_COLUMNWISE_OPERATOR"
    assert operator_arg._datatype == "GH_REAL"
    assert operator_arg._access == "GH_READ"
    assert operator_arg._function_space_to == "W0"
    assert operator_arg._function_space_from == "W1"
