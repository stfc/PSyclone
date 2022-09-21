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

'''Module containing tests for the ColumnwiseOperatorArg class.

'''
import pytest

from fparser.common.readfortran import FortranStringReader
from fparser.two import Fortran2003
from fparser.two.parser import ParserFactory

from psyclone.domain.lfric.kernel.columnwise_operator_arg import \
    ColumnwiseOperatorArg


def test_init_noargs():
    '''Test that an OperatorArg instance can be created successfully when no
    arguments are provided.

    '''
    operator_arg = ColumnwiseOperatorArg()
    assert isinstance(operator_arg, ColumnwiseOperatorArg)
    assert operator_arg.form == "GH_COLUMNWISE_OPERATOR"
    assert operator_arg._datatype is None
    assert operator_arg._access is None
    assert operator_arg._function_space1 is None
    assert operator_arg._function_space2 is None


def test_init_args():
    '''Test that valid initial values provided when constructing an
    instance of OperatorArg are stored as expected.

    '''
    operator_arg = ColumnwiseOperatorArg("GH_REAL", "GH_READ", "W0", "W1")
    assert operator_arg.form == "GH_COLUMNWISE_OPERATOR"
    assert operator_arg._datatype == "GH_REAL"
    assert operator_arg._access == "GH_READ"
    assert operator_arg._function_space1 == "W0"
    assert operator_arg._function_space2 == "W1"


def test_create_from_fortran_string():
    '''Test that the create_from_fortran_string static method works as
    expected. Test for exceptions as well as valid input.

    '''
    with pytest.raises(ValueError) as info:
        _ = ColumnwiseOperatorArg.create_from_fortran_string("not valid")
    assert ("Expected kernel metadata to be a Fortran Part_Ref, with "
            "the form 'arg_type(...)' but found 'not valid'."
            in str(info.value))

    fortran_string = "arg_type(GH_OPERATOR, GH_REAL, GH_READ, W0, W1)"
    operator_arg = ColumnwiseOperatorArg.create_from_fortran_string(
        fortran_string)
    assert operator_arg.form == "GH_COLUMNWISE_OPERATOR"
    assert operator_arg._datatype == "GH_REAL"
    assert operator_arg._access == "GH_READ"
    assert operator_arg._function_space1 == "W0"
    assert operator_arg._function_space2 == "W1"


def create_part_ref(fortran_string):
    '''Utility method to create an fparser2 Part_Ref instance from a
    Fortran string.

    :param str fortran_string: the Fortran string to convert.

    :returns: the fparser2 Part_Ref representation of the Fortran string.
    :rtype: :py:class:`fparser.two.Fortran2003.Part_Ref`

    '''
    _ = ParserFactory().create(std="f2003")
    reader = FortranStringReader(fortran_string)
    return Fortran2003.Part_Ref(reader)


def test_create_from_fparser2():
    '''Test that the create_from_fparser2 static method works as
    expected. Test for exceptions as well as valid input.

    '''
    part_ref = create_part_ref(
        "arg_type(GH_COLUMNWISE_OPERATOR, GH_REAL, GH_READ, W0, W1)")
    operator_arg = ColumnwiseOperatorArg.create_from_fparser2(part_ref)
    assert operator_arg.form == "GH_COLUMNWISE_OPERATOR"
    assert operator_arg._datatype == "GH_REAL"
    assert operator_arg._access == "GH_READ"
    assert operator_arg._function_space1 == "W0"
    assert operator_arg._function_space2 == "W1"
