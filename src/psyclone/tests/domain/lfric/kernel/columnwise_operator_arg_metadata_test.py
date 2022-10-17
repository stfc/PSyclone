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

from psyclone.domain.lfric.kernel.columnwise_operator_arg_metadata import \
    ColumnwiseOperatorArgMetadata


def test_init_noargs():
    '''Test that an OperatorArg instance can be created successfully when no
    arguments are provided.

    '''
    operator_arg = ColumnwiseOperatorArgMetadata()
    assert isinstance(operator_arg, ColumnwiseOperatorArgMetadata)
    assert operator_arg.form == "GH_COLUMNWISE_OPERATOR"
    assert operator_arg._datatype is None
    assert operator_arg._access is None
    assert operator_arg._function_space_to is None
    assert operator_arg._function_space_from is None


def test_init_args():
    '''Test that valid initial values provided when constructing an
    instance of OperatorArg are stored as expected.

    '''
    operator_arg = ColumnwiseOperatorArgMetadata(
        "GH_REAL", "GH_READ", "W0", "W1")
    assert operator_arg.form == "GH_COLUMNWISE_OPERATOR"
    assert operator_arg._datatype == "GH_REAL"
    assert operator_arg._access == "GH_READ"
    assert operator_arg._function_space_to == "W0"
    assert operator_arg._function_space_from == "W1"


def test_create_from_fortran_string():
    '''Test that the create_from_fortran_string static method works as
    expected. Test for exceptions as well as valid input.

    '''
    with pytest.raises(ValueError) as info:
        _ = ColumnwiseOperatorArgMetadata.create_from_fortran_string(
            "not valid")
    assert ("Expected kernel metadata to be a Fortran Part_Ref, with "
            "the form 'arg_type(...)' but found 'not valid'."
            in str(info.value))

    fortran_string = (
        "arg_type(GH_COLUMNWISE_OPERATOR, GH_REAL, GH_READ, W0, W1)")
    operator_arg = ColumnwiseOperatorArgMetadata.create_from_fortran_string(
        fortran_string)
    assert operator_arg.form == "GH_COLUMNWISE_OPERATOR"
    assert operator_arg._datatype == "GH_REAL"
    assert operator_arg._access == "GH_READ"
    assert operator_arg._function_space_to == "W0"
    assert operator_arg._function_space_from == "W1"


def test_create_from_fparser2():
    '''Test that the create_from_fparser2 static method works as
    expected. Test for exceptions as well as valid input.

    '''
    fparser2_tree = ColumnwiseOperatorArgMetadata.create_fparser2(
        "arg_type(GH_UNWISE_OPERATOR, GH_REAL, GH_READ, W0, W1)")
    with pytest.raises(ValueError) as info:
        _ = ColumnwiseOperatorArgMetadata.create_from_fparser2(fparser2_tree)
    assert ("ColumnwiseOperators should have GH_COLUMNWISE_OPERATOR as their "
            "first metadata argument, but found 'GH_UNWISE_OPERATOR'."
            in str(info.value))

    fparser2_tree = ColumnwiseOperatorArgMetadata.create_fparser2(
        "arg_type(GH_COLUMNWISE_OPERATOR, GH_REAL, GH_READ, W0, W1)")
    operator_arg = ColumnwiseOperatorArgMetadata.create_from_fparser2(
        fparser2_tree)
    assert operator_arg.form == "GH_COLUMNWISE_OPERATOR"
    assert operator_arg._datatype == "GH_REAL"
    assert operator_arg._access == "GH_READ"
    assert operator_arg._function_space_to == "W0"
    assert operator_arg._function_space_from == "W1"
