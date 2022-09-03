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

'''Module containing tests for the FieldVectorArg class.

'''
import pytest

from fparser.common.readfortran import FortranStringReader
from fparser.two import Fortran2003
from fparser.two.parser import ParserFactory

from psyclone.domain.lfric.kernel.field_vector_arg import FieldVectorArg


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


def test_init_noargs():
    '''Test that a FieldVectorArg instance can be created successfully when no
    arguments are provided.

    '''
    field_vector_arg = FieldVectorArg()
    assert isinstance(field_vector_arg, FieldVectorArg)
    assert field_vector_arg._form == "GH_FIELD"
    assert field_vector_arg._datatype is None
    assert field_vector_arg._access is None
    assert field_vector_arg._function_space is None
    assert field_vector_arg._vector_length is None


def test_init_invalid():
    '''Test that appropriate exceptions are raised if invalid initial
    values are provided when constructing an instance of the FieldVectorArg
    class. Only test one of the arguments for the base class.

    '''
    with pytest.raises(ValueError) as info:
        _ = FieldVectorArg(datatype="invalid")
    assert ("The second metadata entry for an argument should be a "
            "recognised datatype descriptor (one of ['gh_real', "
            "'gh_integer']), but found 'invalid'." in str(info.value))

    with pytest.raises(TypeError) as info:
        _ = FieldVectorArg(vector_length=1)
    assert ("The vector size should be a string but found int."
            in str(info.value))

    with pytest.raises(ValueError) as info:
        _ = FieldVectorArg(vector_length="invalid")
    assert ("invalid literal for int() with base 10: 'invalid'"
            in str(info.value))

    with pytest.raises(ValueError) as info:
        _ = FieldVectorArg(vector_length="0")
    assert ("The vector size should be an integer greater than 1 but found 0."
            in str(info.value))


def test_init_args():
    '''Test that valid initial values provided when constructing an
    instance of FieldArg are stored as expected.

    '''
    field_vector_arg = FieldVectorArg("GH_REAL", "GH_READ", "W0", "2")
    assert field_vector_arg._form == "GH_FIELD"
    assert field_vector_arg._datatype == "GH_REAL"
    assert field_vector_arg._access == "GH_READ"
    assert field_vector_arg._function_space == "W0"
    assert field_vector_arg._vector_length == "2"


def test_create_from_fortran_string():
    '''Test that the create_from_fortran_string static method works as
    expected. Test for exceptions as well as valid input.

    '''
    with pytest.raises(ValueError) as info:
        _ = FieldVectorArg.create_from_fortran_string("not valid")
    assert ("Expected kernel metadata to be a Fortran Part_Ref, with "
            "the form 'arg_type(...)' but found 'not valid'."
            in str(info.value))

    fortran_string = "arg_type(GH_FIELD*3, GH_REAL, GH_READ, W0)"
    field_arg = FieldVectorArg.create_from_fortran_string(fortran_string)
    assert field_arg._form == "GH_FIELD"
    assert field_arg._datatype == "GH_REAL"
    assert field_arg._access == "GH_READ"
    assert field_arg._function_space == "W0"
    assert field_arg._vector_length == "3"


def test_create_from_psyir():
    '''Test that the create_from_psyir static method works as
    expected. Test for exceptions as well as valid input.

    '''
    with pytest.raises(TypeError) as info:
        _ = FieldVectorArg.create_from_psyir("hello")
    assert ("Expected kernel metadata to be encoded as a Fortran "
            "Part_Ref object but found type 'str' with value 'hello'."
            in str(info.value))

    part_ref = create_part_ref("arg_type(GH_FIELD, GH_REAL, GH_READ, W0)")
    with pytest.raises(TypeError) as info:
        _ = FieldVectorArg.create_from_psyir(part_ref)
    assert("Expecting the first argument to be in the form "
           "'form*vector_length' but found 'GH_FIELD'." in str(info.value))

    part_ref = create_part_ref("arg_type(GH_FIELD*3, GH_REAL, GH_READ, W0)")
    field_vector_arg = FieldVectorArg.create_from_psyir(part_ref)
    assert field_vector_arg._form == "GH_FIELD"
    assert field_vector_arg._datatype == "GH_REAL"
    assert field_vector_arg._access == "GH_READ"
    assert field_vector_arg._function_space == "W0"
    assert field_vector_arg._vector_length == "3"


def test_fortran_string():
    '''Test that the fortran_string method works as expected, including
    raise an exception if all of the required properties have not been
    set '''
    fortran_string = "arg_type(GH_FIELD*3, GH_REAL, GH_READ, W0)"
    field_vector_arg = FieldVectorArg.create_from_fortran_string(
        fortran_string)
    result = field_vector_arg.fortran_string()
    assert result == fortran_string

    field_vector_arg = FieldVectorArg()
    with pytest.raises(ValueError) as info:
        _ = field_vector_arg.fortran_string()
    assert ("Values for datatype, access, function_space and vector_length "
            "must be provided before calling the fortran_string method, but "
            "found 'None', 'None', 'None' and 'None'." in str(info.value))


def test_setter_getter():
    '''Test that the setters and getters work as expected, including
    raising exceptions if values are invalid. Only test some of the
    base class values.

    '''
    field_arg = FieldVectorArg()
    assert field_arg.form == "GH_FIELD"

    assert field_arg.datatype is None
    with pytest.raises(ValueError) as info:
        field_arg.datatype = "invalid"
    assert ("The second metadata entry for an argument should be a "
            "recognised datatype descriptor (one of ['gh_real', "
            "'gh_integer']), but found 'invalid'." in str(info.value))

    field_arg.datatype = "gh_integer"
    assert field_arg.datatype == "gh_integer"
    field_arg.datatype = "GH_INTEGER"
    assert field_arg.datatype == "GH_INTEGER"

    assert field_arg.vector_length is None
    with pytest.raises(ValueError) as info:
        field_arg.vector_length = "invalid"
    assert ("invalid literal for int() with base 10: 'invalid'"
            in str(info.value))

    with pytest.raises(ValueError) as info:
        field_arg.vector_length = "1"
    assert ("The vector size should be an integer greater than 1 but found 1."
            in str(info.value))

    field_arg.vector_length = "3"
    assert field_arg.vector_length == "3"
