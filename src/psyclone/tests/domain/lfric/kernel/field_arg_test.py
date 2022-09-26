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

'''Module containing tests for the FieldArg class.

'''
import pytest

from fparser.common.readfortran import FortranStringReader
from fparser.two import Fortran2003
from fparser.two.parser import ParserFactory

from psyclone.domain.lfric.kernel.field_arg import FieldArg


def test_init_noargs():
    '''Test that a FieldArg instance can be created successfully when no
    arguments are provided.

    '''
    field_arg = FieldArg()
    assert isinstance(field_arg, FieldArg)
    assert field_arg.form == "GH_FIELD"
    assert field_arg._datatype is None
    assert field_arg._access is None
    assert field_arg._function_space is None


def test_init_invalid():
    '''Test that appropriate exceptions are raised if invalid initial
    values are provided when constructing an instance of the FieldArg
    class.

    '''
    with pytest.raises(ValueError) as info:
        _ = FieldArg(datatype="invalid")
    assert ("The second metadata entry for an argument should be a "
            "recognised datatype descriptor (one of ['gh_real', "
            "'gh_integer']), but found 'invalid'." in str(info.value))

    with pytest.raises(ValueError) as info:
        _ = FieldArg(access="invalid")
    assert ("The third metadata entry for an argument should be a "
            "recognised access descriptor (one of ['gh_read', 'gh_write', "
            "'gh_inc', 'gh_readinc']), but found 'invalid'."
            in str(info.value))

    with pytest.raises(ValueError) as info:
        _ = FieldArg(function_space="invalid")
    assert ("The fourth metadata entry for an argument should be a "
            "recognised function space (one of ['w3', 'wtheta', 'w2v', "
            "'w2vtrace', 'w2broken', 'w0', 'w1', 'w2', 'w2trace', 'w2h', "
            "'w2htrace', 'any_w2', 'wchi', 'any_space_1', 'any_space_2', "
            "'any_space_3', 'any_space_4', 'any_space_5', 'any_space_6', "
            "'any_space_7', 'any_space_8', 'any_space_9', 'any_space_10', "
            "'any_discontinuous_space_1', 'any_discontinuous_space_2', "
            "'any_discontinuous_space_3', 'any_discontinuous_space_4', "
            "'any_discontinuous_space_5', 'any_discontinuous_space_6', "
            "'any_discontinuous_space_7', 'any_discontinuous_space_8', "
            "'any_discontinuous_space_9', 'any_discontinuous_space_10']), "
            "but found 'invalid'." in str(info.value))


def test_init_args():
    '''Test that valid initial values provided when constructing an
    instance of FieldArg are stored as expected.

    '''
    field_arg = FieldArg("GH_REAL", "GH_READ", "W0")
    assert field_arg.form == "GH_FIELD"
    assert field_arg._datatype == "GH_REAL"
    assert field_arg._access == "GH_READ"
    assert field_arg._function_space == "W0"


def test_create_from_fortran_string():
    '''Test that the create_from_fortran_string static method works as
    expected. Test for exceptions as well as valid input.

    '''
    with pytest.raises(ValueError) as info:
        _ = FieldArg.create_from_fortran_string("not valid")
    assert ("Expected kernel metadata to be a Fortran Part_Ref, with "
            "the form 'arg_type(...)' but found 'not valid'."
            in str(info.value))

    fortran_string = "arg_type(GH_FIELD, GH_REAL, GH_READ, W0)"
    field_arg = FieldArg.create_from_fortran_string(fortran_string)
    assert field_arg.form == "GH_FIELD"
    assert field_arg._datatype == "GH_REAL"
    assert field_arg._access == "GH_READ"
    assert field_arg._function_space == "W0"


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
    with pytest.raises(TypeError) as info:
        _ = FieldArg.create_from_fparser2("hello")
    assert ("Expected kernel metadata to be encoded as a Fortran "
            "Part_Ref object but found type 'str' with value 'hello'."
            in str(info.value))

    part_ref = create_part_ref("hello(x)")
    with pytest.raises(ValueError) as info:
        _ = FieldArg.create_from_fparser2(part_ref)
    assert ("Expected kernel metadata to have the name 'arg_type' "
            "and be in the form 'arg_type(...)', but found 'hello(x)'."
            in str(info.value))

    part_ref = create_part_ref("arg_type(x)")
    with pytest.raises(ValueError) as info:
        _ = FieldArg.create_from_fparser2(part_ref)
    assert ("Expected kernel metadata to have 4 arguments, but "
            "found 1 in 'arg_type(x)'." in str(info.value))

    part_ref = create_part_ref("arg_type(GH_FIELD, GH_REAL, GH_READ, W0)")
    field_arg = FieldArg.create_from_fparser2(part_ref)
    assert field_arg.form == "GH_FIELD"
    assert field_arg._datatype == "GH_REAL"
    assert field_arg._access == "GH_READ"
    assert field_arg._function_space == "W0"


def test_fortran_string():
    '''Test that the fortran_string method works as expected, including
    raise an exception if all of the required properties have not been
    set '''
    fortran_string = "arg_type(GH_FIELD, GH_REAL, GH_READ, W0)"
    field_arg = FieldArg.create_from_fortran_string(fortran_string)
    result = field_arg.fortran_string()
    assert result == fortran_string

    field_arg = FieldArg()
    with pytest.raises(ValueError) as info:
        _ = field_arg.fortran_string()
    assert ("Values for datatype, access and function_space must be provided "
            "before calling the fortran_string method, but found 'None', "
            "'None' and 'None', respectively." in str(info.value))


def test_setter_getter():
    '''Test that the setters and getters work as expected, including
    raising exceptions if values are invalid. '''
    field_arg = FieldArg()
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

    assert field_arg.access is None
    with pytest.raises(ValueError) as info:
        field_arg.access = "invalid"
    assert ("The third metadata entry for an argument should be a "
            "recognised access descriptor (one of ['gh_read', 'gh_write', "
            "'gh_inc', 'gh_readinc']), but found 'invalid'."
            in str(info.value))

    field_arg.access = "gh_read"
    assert field_arg.access == "gh_read"
    field_arg.access = "GH_READ"
    assert field_arg.access == "GH_READ"

    assert field_arg.function_space is None
    with pytest.raises(ValueError) as info:
        field_arg.function_space = "invalid"
    assert ("The fourth metadata entry for an argument should be a "
            "recognised function space (one of ['w3', 'wtheta', 'w2v', "
            "'w2vtrace', 'w2broken', 'w0', 'w1', 'w2', 'w2trace', 'w2h', "
            "'w2htrace', 'any_w2', 'wchi', 'any_space_1', 'any_space_2', "
            "'any_space_3', 'any_space_4', 'any_space_5', 'any_space_6', "
            "'any_space_7', 'any_space_8', 'any_space_9', 'any_space_10', "
            "'any_discontinuous_space_1', 'any_discontinuous_space_2', "
            "'any_discontinuous_space_3', 'any_discontinuous_space_4', "
            "'any_discontinuous_space_5', 'any_discontinuous_space_6', "
            "'any_discontinuous_space_7', 'any_discontinuous_space_8', "
            "'any_discontinuous_space_9', 'any_discontinuous_space_10']), "
            "but found 'invalid'." in str(info.value))
    field_arg.function_space = "w0"
    assert field_arg.function_space == "w0"
    field_arg.function_space = "W0"
    assert field_arg.function_space == "W0"
