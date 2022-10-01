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

'''Module containing tests for the InterGridVectorArg class.

'''
import pytest

from fparser.common.readfortran import FortranStringReader
from fparser.two import Fortran2003
from fparser.two.parser import ParserFactory

from psyclone.domain.lfric.kernel.inter_grid_vector_arg import \
    InterGridVectorArg


def test_init_noargs():
    '''Test that an InterGridVectorArg instance can be created
    successfully when no arguments are provided.

    '''
    inter_grid_arg = InterGridVectorArg()
    assert isinstance(inter_grid_arg, InterGridVectorArg)
    assert inter_grid_arg.form == "GH_FIELD"
    assert inter_grid_arg._datatype is None
    assert inter_grid_arg._access is None
    assert inter_grid_arg._function_space is None
    assert inter_grid_arg._mesh_arg is None
    assert inter_grid_arg._vector_length is None


def test_init_invalid():
    '''Test that appropriate exceptions are raised if invalid initial
    values are provided when constructing an instance of the
    InterGridVectorArg class.

    '''
    with pytest.raises(ValueError) as info:
        _ = InterGridVectorArg(datatype="invalid")
    assert ("The datatype descriptor metadata for a field should be one of "
            "['gh_real', 'gh_integer'], but found 'invalid'."
            in str(info.value))

    with pytest.raises(ValueError) as info:
        _ = InterGridVectorArg(access="invalid")
    assert ("The access descriptor metadata for a field should be one of "
            "['gh_read', 'gh_write', 'gh_inc', 'gh_readinc'], but found "
            "'invalid'." in str(info.value))

    with pytest.raises(ValueError) as info:
        _ = InterGridVectorArg(function_space="invalid")
    assert ("The function space metadata should be one of ['w3', 'wtheta', "
            "'w2v', 'w2vtrace', 'w2broken', 'w0', 'w1', 'w2', 'w2trace', "
            "'w2h', 'w2htrace', 'any_w2', 'wchi', 'any_space_1', "
            "'any_space_2', 'any_space_3', 'any_space_4', 'any_space_5', "
            "'any_space_6', 'any_space_7', 'any_space_8', 'any_space_9', "
            "'any_space_10', 'any_discontinuous_space_1', "
            "'any_discontinuous_space_2', 'any_discontinuous_space_3', "
            "'any_discontinuous_space_4', 'any_discontinuous_space_5', "
            "'any_discontinuous_space_6', 'any_discontinuous_space_7', "
            "'any_discontinuous_space_8', 'any_discontinuous_space_9', "
            "'any_discontinuous_space_10'], but found 'invalid'."
            in str(info.value))

    with pytest.raises(ValueError) as info:
        _ = InterGridVectorArg(mesh_arg="invalid")
    assert ("The mesh_arg metadata for a mesh should be one of ['gh_coarse', "
            "'gh_fine'], but found 'invalid'." in str(info.value))


def test_init_args():
    '''Test that valid initial values provided when constructing an
    instance of InterGridVectorArg are stored as expected.

    '''
    inter_grid_arg = InterGridVectorArg(
        "GH_REAL", "GH_READ", "W0", "GH_FINE", "3")
    assert inter_grid_arg.form == "GH_FIELD"
    assert inter_grid_arg._datatype == "GH_REAL"
    assert inter_grid_arg._access == "GH_READ"
    assert inter_grid_arg._function_space == "W0"
    assert inter_grid_arg._mesh_arg == "GH_FINE"
    assert inter_grid_arg._vector_length == "3"


def test_create_from_fortran_string():
    '''Test that the create_from_fortran_string static method works as
    expected. Test for exceptions as well as valid input.

    '''
    with pytest.raises(ValueError) as info:
        _ = InterGridVectorArg.create_from_fortran_string("not valid")
    assert ("Expected kernel metadata to be a Fortran Structure_Constructor, "
            "with the form 'arg_type(...)' but found 'not valid'."
            in str(info.value))

    fortran_string = ("arg_type(GH_FIELD*3, GH_REAL, GH_READ, W0, "
                      "gh_mesh=GH_COARSE)")
    inter_grid_arg = InterGridVectorArg.create_from_fortran_string(
        fortran_string)
    assert inter_grid_arg.form == "GH_FIELD"
    assert inter_grid_arg._datatype == "GH_REAL"
    assert inter_grid_arg._access == "GH_READ"
    assert inter_grid_arg._function_space == "W0"
    assert inter_grid_arg._mesh_arg == "GH_COARSE"
    assert inter_grid_arg._vector_length == "3"


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
        _ = InterGridVectorArg.create_from_fparser2("hello")
    assert ("Expected kernel metadata to be encoded as an fparser2 "
            "Structure_Constructor object but found type 'str' with value "
            "'hello'." in str(info.value))

    with pytest.raises(ValueError) as info:
        _ = InterGridVectorArg.create_from_fortran_string("hello(x)")
    assert ("Expected kernel metadata to have the name 'arg_type' "
            "and be in the form 'arg_type(...)', but found 'hello(x)'."
            in str(info.value))

    with pytest.raises(ValueError) as info:
        _ = InterGridVectorArg.create_from_fortran_string("arg_type(x)")
    assert ("Expected kernel metadata to have 5 arguments, but "
            "found 1 in 'arg_type(x)'." in str(info.value))

    with pytest.raises(TypeError) as info:
        _ = InterGridVectorArg.create_from_fortran_string(
            "arg_type(GH_FIELD, GH_REAL, GH_READ, W0, mesh_arg=GH_COARSE)")
    assert ("The vector length metadata should be in the form "
            "'form*vector_length' but found 'GH_FIELD'." in str(info.value))

    inter_grid_arg = InterGridVectorArg.create_from_fortran_string(
        "arg_type(GH_FIELD*3, GH_REAL, GH_READ, W0, mesh_arg=GH_COARSE)")
    assert inter_grid_arg.form == "GH_FIELD"
    assert inter_grid_arg._datatype == "GH_REAL"
    assert inter_grid_arg._access == "GH_READ"
    assert inter_grid_arg._function_space == "W0"
    assert inter_grid_arg._mesh_arg == "GH_COARSE"
    assert inter_grid_arg._vector_length == "3"


def test_fortran_string():
    '''Test that the fortran_string method works as expected, including
    raise an exception if all of the required properties have not been
    set '''
    fortran_string = ("arg_type(GH_FIELD*3, GH_REAL, GH_READ, W0, "
                      "mesh_arg=GH_FINE)")
    inter_grid_arg = InterGridVectorArg.create_from_fortran_string(
        fortran_string)
    result = inter_grid_arg.fortran_string()
    assert result == fortran_string

    inter_grid_arg = InterGridVectorArg()
    with pytest.raises(ValueError) as info:
        _ = inter_grid_arg.fortran_string()
    assert ("Values for datatype, access, function_space, mesh_arg and "
            "vector_length must be provided before calling the "
            "fortran_string method, but found 'None', 'None', 'None', "
            "'None' and 'None', respectively." in str(info.value))


def test_setter_getter():
    '''Test that the setters and getters work as expected, including
    raising exceptions if values are invalid. '''
    inter_grid_arg = InterGridVectorArg()
    assert inter_grid_arg.form == "GH_FIELD"

    with pytest.raises(ValueError) as info:
        inter_grid_arg.mesh_arg = "invalid"
    assert ("The mesh_arg metadata for a mesh should be one of ['gh_coarse', "
            "'gh_fine'], but found 'invalid'." in str(info.value))

    inter_grid_arg.mesh_arg = "GH_COARSE"
    assert inter_grid_arg.mesh_arg == "GH_COARSE"
    inter_grid_arg.mesh_arg = "GH_FINE"
    assert inter_grid_arg.mesh_arg == "GH_FINE"

    assert inter_grid_arg.vector_length is None
    with pytest.raises(TypeError) as info:
        inter_grid_arg.vector_length = 3
    assert ("The vector size should be a string but found int."
            in str(info.value))
    with pytest.raises(ValueError) as info:
        inter_grid_arg.vector_length = "0"
    assert ("The vector size should be an integer greater than 1 but found 0."
            in str(info.value))
    inter_grid_arg.vector_length = "3"
    assert inter_grid_arg.vector_length == "3"
