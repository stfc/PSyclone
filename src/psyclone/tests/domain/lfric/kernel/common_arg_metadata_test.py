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

'''Module containing tests for the CommonArgMetadata class.

'''
import pytest

from fparser.two import Fortran2003

from psyclone.domain.lfric.kernel.common_arg_metadata import CommonArgMetadata
from psyclone.domain.lfric.kernel.inter_grid_arg_metadata import \
    InterGridArgMetadata
from psyclone.domain.lfric.kernel.meta_mesh_arg_metadata import \
    MetaMeshArgMetadata


def test_init():
    '''Test that the CommonArgMetadata class can be created.'''

    common_arg_metadata = CommonArgMetadata()
    assert isinstance(common_arg_metadata, CommonArgMetadata)


def test_create_from_fortran_string():
    '''Test the create_from_fortran_string() method. Test with two
    subclasses that cover the two classes of fparser2 that are
    required as part of this method a) Part_Ref and b)
    Structure_Constructor.

    '''
    # Makes use of Fortran2003.Part_Ref.
    meta = MetaMeshArgMetadata.create_from_fortran_string(
        "mesh_data_type(adjacent_face)")
    assert isinstance(meta, MetaMeshArgMetadata)
    assert meta.mesh == "adjacent_face"

    # Makes use of Fortran2003.Structure_Constructor.
    meta = InterGridArgMetadata.create_from_fortran_string(
        "arg_type(gh_field, gh_real, gh_read, w0, mesh_arg=gh_fine)")
    assert isinstance(meta, InterGridArgMetadata)
    assert meta.datatype == "gh_real"


def test_check_value():
    '''Test that the check_value method works as expected.'''

    CommonArgMetadata.check_value("hello", "metadata", ["hello"])

    with pytest.raises(TypeError) as info:
        CommonArgMetadata.check_value(None, "metadata", ["bonjour"])
    assert ("The metadata value should be of type str, but found 'NoneType'."
            in str(info.value))

    with pytest.raises(ValueError) as info:
        CommonArgMetadata.check_value("hello", "metadata", ["bonjour"])
    assert ("The metadata value should be one of ['bonjour'], but found "
            "'hello'." in str(info.value))


def test_check_nargs():
    '''Test that the check_nargs method works as expected.'''
    fparser2_tree = CommonArgMetadata.create_fparser2(
        "args(one, two, three)", Fortran2003.Part_Ref)
    CommonArgMetadata.check_nargs(fparser2_tree, 3)
    with pytest.raises(ValueError) as info:
        CommonArgMetadata.check_nargs(fparser2_tree, 2)
    assert ("Expected kernel metadata to have 2 arguments, but found 3 in "
            "'args(one, two, three)'." in str(info.value))


def test_check_fparser2():
    '''Test that the check_fparser2 method in the CommonArgMetadata class
    works as expected.

    '''
    with pytest.raises(TypeError) as info:
        _ = CommonArgMetadata.check_fparser2(None, None)
    assert ("Expected kernel metadata to be encoded as an fparser2 Part_Ref "
            "object but found type 'NoneType' with value 'None'."
            in str(info.value))

    fparser_tree = CommonArgMetadata.create_fparser2(
        "braz_type(GH_FIELD, GH_REAL, GH_READ)", Fortran2003.Part_Ref)
    with pytest.raises(ValueError) as info:
        _ = CommonArgMetadata.check_fparser2(fparser_tree, "arg_type")
    assert ("Expected kernel metadata to have the name 'arg_type' and be in "
            "the form 'arg_type(...)', but found 'braz_type(GH_FIELD, "
            "GH_REAL, GH_READ)'." in str(info.value))


def test_get_nargs():
    '''Test that the get_nargs method works as expected.'''
    fparser2_tree = CommonArgMetadata.create_fparser2(
        "args(one, two, three)", Fortran2003.Part_Ref)
    assert CommonArgMetadata.get_nargs(fparser2_tree) == 3


def test_get_arg():
    '''Test that the get_arg method in the CommonArgMetadata class works
    as expected.

    '''
    fparser_tree = CommonArgMetadata.create_fparser2(
        "arg_type(GH_FIELD, GH_REAL, GH_READ)", Fortran2003.Part_Ref)
    assert CommonArgMetadata.get_arg(fparser_tree, 0) == "GH_FIELD"
    assert CommonArgMetadata.get_arg(fparser_tree, 1) == "GH_REAL"
    assert CommonArgMetadata.get_arg(fparser_tree, 2) == "GH_READ"
