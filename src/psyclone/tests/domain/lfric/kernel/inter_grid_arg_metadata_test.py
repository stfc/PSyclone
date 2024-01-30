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

'''Module containing tests for the InterGridArgMetadata class.

'''
import pytest

from fparser.two import Fortran2003

from psyclone.domain.lfric.kernel import InterGridArgMetadata


@pytest.mark.parametrize("datatype, access, function_space, mesh, stencil", [
    ("GH_REAL", "GH_READ", "W0", "GH_FINE", "REGION"),
    ("gh_real", "gh_read", "w0", "gh_fine", "region")])
def test_create(datatype, access, function_space, mesh, stencil):
    '''Test that an instance of InterGridArgMetadata can be created
    successfully. Test that the input is case insensitive and for
    optional stencil information.

    '''
    inter_grid_arg = InterGridArgMetadata(
        datatype, access, function_space, mesh)
    assert isinstance(inter_grid_arg, InterGridArgMetadata)
    assert inter_grid_arg.form == "gh_field"
    assert inter_grid_arg._datatype == "gh_real"
    assert inter_grid_arg._access == "gh_read"
    assert inter_grid_arg._function_space == "w0"
    assert inter_grid_arg._mesh_arg == "gh_fine"
    assert inter_grid_arg._stencil is None

    inter_grid_arg = InterGridArgMetadata(
        datatype, access, function_space, mesh, stencil=stencil)
    assert inter_grid_arg._stencil == "region"


def test_init_invalid():
    '''Test that an invalid mesh supplied to the constructor
    raises the expected exception.

    '''
    with pytest.raises(ValueError) as info:
        _ = InterGridArgMetadata("GH_REAL", "GH_READ", "W0", "invalid")
    assert ("The 'mesh_arg' metadata should be a recognised value (one of "
            "['gh_coarse', 'gh_fine']) but found 'invalid'."
            in str(info.value))


def test_get_metadata():
    '''Test that the get_metadata class method works as expected.

    '''
    metadata = "arg_type(GH_FIELD, GH_REAL, GH_READ, W0, mesh_arg=GH_COARSE)"
    fparser2_tree = InterGridArgMetadata.create_fparser2(
        metadata, encoding=Fortran2003.Structure_Constructor)
    datatype, access, function_space, mesh_arg, stencil = \
        InterGridArgMetadata._get_metadata(fparser2_tree)
    assert datatype == "GH_REAL"
    assert access == "GH_READ"
    assert function_space == "W0"
    assert mesh_arg == "GH_COARSE"
    assert stencil is None


def test_get_metadata_stencil():
    '''Test that the get_metadata class method works as expected when an
    optional stencil value is provided.

    '''
    metadata = ("arg_type(GH_FIELD, GH_REAL, GH_READ, W0, stencil(region), "
                "mesh_arg=GH_COARSE)")
    fparser2_tree = InterGridArgMetadata.create_fparser2(
        metadata, encoding=Fortran2003.Structure_Constructor)
    datatype, access, function_space, mesh_arg, stencil = \
        InterGridArgMetadata._get_metadata(fparser2_tree)
    assert datatype == "GH_REAL"
    assert access == "GH_READ"
    assert function_space == "W0"
    assert mesh_arg == "GH_COARSE"
    assert stencil == "region"


def test_get_mesh_arg():
    '''Test that the get_mesh_arg method works as expected. Also check
    that it raises the expected error when the metadata is invalid.

    '''
    fparser2_tree = InterGridArgMetadata.create_fparser2(
        "arg_type(GH_FIELD, GH_REAL, GH_READ, W0, mesh_arg=GH_COARSE)",
        encoding=Fortran2003.Structure_Constructor)
    mesh_arg = InterGridArgMetadata.get_mesh_arg(fparser2_tree, 4)
    assert mesh_arg == "GH_COARSE"

    # Test when metadata is not in the expected 'mesh_arg = value'
    # form. For simplicity, just choose the wrong argument index for
    # the existing valid metadata.
    with pytest.raises(ValueError) as info:
        _ = InterGridArgMetadata.get_mesh_arg(fparser2_tree, 3)
    assert ("At argument index 3 for metadata 'arg_type(GH_FIELD, GH_REAL, "
            "GH_READ, W0, mesh_arg = GH_COARSE)' expected the metadata to be "
            "in the form 'mesh_arg=value' but found 'W0'." in str(info.value))

    fparser2_tree = InterGridArgMetadata.create_fparser2(
        "arg_type(GH_FIELD, GH_REAL, GH_READ, W0, invalid=GH_COARSE)",
        encoding=Fortran2003.Structure_Constructor)
    with pytest.raises(ValueError) as info:
        _ = InterGridArgMetadata.get_mesh_arg(fparser2_tree, 4)
    assert ("At argument index 4 for metadata 'arg_type(GH_FIELD, GH_REAL, "
            "GH_READ, W0, invalid = GH_COARSE)' expected the left hand side "
            "to be 'mesh_arg' but found 'invalid'." in str(info.value))


@pytest.mark.parametrize("fortran_string", [
    "arg_type(GH_FIELD, GH_REAL, GH_READ, W0, mesh_arg=GH_FINE)",
    "arg_type(GH_FIELD, GH_REAL, GH_READ, W0, STENCIL(X1D), "
    "mesh_arg=GH_FINE)"])
def test_fortran_string(fortran_string):
    '''Test that the fortran_string method works as expected. Test with
    and without a stencil.

    '''
    inter_grid_arg = InterGridArgMetadata.create_from_fortran_string(
        fortran_string)
    result = inter_grid_arg.fortran_string()
    assert result == fortran_string.lower()


def test_mesh_arg_setter_getter():
    '''Test that the mesh_arg setter and getter work as expected,
    including raising an exception if value is invalid.

    '''
    inter_grid_arg = InterGridArgMetadata(
        "GH_REAL", "GH_READ", "W0", "GH_FINE")

    with pytest.raises(ValueError) as info:
        inter_grid_arg.mesh_arg = "invalid"
    assert ("The 'mesh_arg' metadata should be a recognised value (one of "
            "['gh_coarse', 'gh_fine']) but found 'invalid'."
            in str(info.value))

    inter_grid_arg.mesh_arg = "GH_COARSE"
    assert inter_grid_arg.mesh_arg == "gh_coarse"
    inter_grid_arg.mesh_arg = "GH_FINE"
    assert inter_grid_arg.mesh_arg == "gh_fine"
