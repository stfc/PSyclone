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

'''Module containing tests for the InterGridVectorArgMetadata class.

'''
import pytest

from fparser.two import Fortran2003

from psyclone.domain.lfric.kernel import InterGridVectorArgMetadata


@pytest.mark.parametrize(
    "datatype, access, function_space, mesh, vector_length, stencil", [
        ("GH_REAL", "GH_READ", "W0", "GH_FINE", "3", "REGION"),
        ("gh_real", "gh_read", "w0", "gh_fine", "3", "region")])
def test_create(datatype, access, function_space, mesh, vector_length,
                stencil):
    '''Test that an instance of InterGridVectorArgMetadata can be created
    successfully. Test that the input is case insensitive and for
    optional stencil information.

    '''
    inter_grid_arg = InterGridVectorArgMetadata(
        datatype, access, function_space, mesh, vector_length)
    assert isinstance(inter_grid_arg, InterGridVectorArgMetadata)
    assert inter_grid_arg.form == "gh_field"
    assert inter_grid_arg._datatype == "gh_real"
    assert inter_grid_arg._access == "gh_read"
    assert inter_grid_arg._function_space == "w0"
    assert inter_grid_arg._mesh_arg == "gh_fine"
    assert inter_grid_arg._vector_length == "3"
    assert inter_grid_arg._stencil is None

    inter_grid_arg = InterGridVectorArgMetadata(
        datatype, access, function_space, mesh, vector_length, stencil=stencil)
    assert inter_grid_arg._stencil == "region"


def test_init_invalid():
    '''Test that an invalid vector length supplied to the constructor
    raises the expected exception.

    '''
    with pytest.raises(TypeError) as info:
        _ = InterGridVectorArgMetadata(
            "GH_REAL", "GH_READ", "W0", "GH_FINE", 3)
    assert ("The vector size should be a string but found int."
            in str(info.value))


def test_get_metadata():
    '''Test that the get_metadata class method works as expected.

    '''
    metadata = "arg_type(GH_FIELD*3, GH_REAL, GH_READ, W0, mesh_arg=GH_COARSE)"
    fparser2_tree = InterGridVectorArgMetadata.create_fparser2(
        metadata, encoding=Fortran2003.Structure_Constructor)
    datatype, access, function_space, mesh_arg, vector_length, stencil = \
        InterGridVectorArgMetadata._get_metadata(fparser2_tree)
    assert datatype == "GH_REAL"
    assert access == "GH_READ"
    assert function_space == "W0"
    assert mesh_arg == "GH_COARSE"
    assert vector_length == "3"
    assert stencil is None


def test_get_metadata_stencil():
    '''Test that the get_metadata class method works as expected when an
    optional stencil value is provided.

    '''
    metadata = ("arg_type(GH_FIELD*3, GH_REAL, GH_READ, W0, stencil(xory1d), "
                "mesh_arg=GH_COARSE)")
    fparser2_tree = InterGridVectorArgMetadata.create_fparser2(
        metadata, encoding=Fortran2003.Structure_Constructor)
    datatype, access, function_space, mesh_arg, vector_length, stencil = \
        InterGridVectorArgMetadata._get_metadata(fparser2_tree)
    assert datatype == "GH_REAL"
    assert access == "GH_READ"
    assert function_space == "W0"
    assert mesh_arg == "GH_COARSE"
    assert vector_length == "3"
    assert stencil == "xory1d"


@pytest.mark.parametrize("fortran_string", [
    "arg_type(GH_FIELD*3, GH_REAL, GH_READ, W0, mesh_arg=GH_FINE)",
    "arg_type(GH_FIELD*3, GH_REAL, GH_READ, W0, STENCIL(X1D), "
    "mesh_arg=GH_FINE)"])
def test_fortran_string(fortran_string):
    '''Test that the fortran_string method works as expected. Test with
    and without a stencil.

    '''
    inter_grid_arg = InterGridVectorArgMetadata.create_from_fortran_string(
        fortran_string)
    result = inter_grid_arg.fortran_string()
    assert result == fortran_string.lower()


def test_vector_length_setter_getter():
    '''Test that the vector length setter and getter work as expected,
    including raising an exception if the value is invalid.

    '''
    inter_grid_arg = InterGridVectorArgMetadata(
        "GH_REAL", "GH_READ", "W0", "GH_FINE", "3")

    with pytest.raises(ValueError) as info:
        inter_grid_arg.vector_length = "invalid"
    assert ("The vector size should be a string containing an integer, "
            "but found 'invalid'." in str(info.value))

    with pytest.raises(ValueError) as info:
        inter_grid_arg.vector_length = "1"
    assert ("The vector size should be an integer greater than 1 but found 1."
            in str(info.value))

    inter_grid_arg.vector_length = "3"
    assert inter_grid_arg.vector_length == "3"
