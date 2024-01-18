# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2023-2024, Science and Technology Facilities Council.
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
# Author: R. W. Ford, STFC Daresbury Lab

'''This module implements tests for the ArgIndexToMetadataIndex
class.

'''
from psyclone.domain.lfric import ArgIndexToMetadataIndex
from psyclone.domain.lfric.kernel import (
    ScalarArgMetadata, FieldArgMetadata, FieldVectorArgMetadata,
    OperatorArgMetadata, ColumnwiseOperatorArgMetadata, LFRicKernelMetadata)


def call_method(method_name, meta_arg, metadata):
    '''Utility method to initialise the required class variables and then
    call the method specified in the method_name argument.

    :param str method_name: the name of the method to call.
    :param meta_arg: the meta_arg metadata to pass to the call.
    :type meta_arg: subclass of \
        :py:class:`psyclone.domain.lfric.kernel.CommonArgMetadata`
    :param metadata: the full metadata description.
    :type metadata: \
        :py:class:`psyclone.domain.lfric.kernel.LFRicKernelMetadata`

    :returns: an ArgIndexToMetadataIndex class
    :rtype: :py:class:`psyclone.domain.lfric.ArgIndexToMetadataIndex`

    '''
    metadata.validate()
    cls = ArgIndexToMetadataIndex
    cls._initialise(None)
    cls._metadata = metadata
    getattr(cls, method_name)(meta_arg)
    return cls


def test_initialise():
    '''Test the _initialise method.'''
    cls = ArgIndexToMetadataIndex
    cls._index = 2
    cls._info = None
    cls._initialise(None)
    assert cls._index == 0
    assert isinstance(cls._info, dict)
    assert not cls._info


def test_scalar():
    '''Test the _scalar method. Note, we need at least one field/operator
    for the metadata to be valid.

    '''
    meta_arg = ScalarArgMetadata("GH_REAL", "GH_READ")
    meta_args = [
        FieldArgMetadata("GH_REAL", "GH_INC", "W0"),
        meta_arg]
    metadata = LFRicKernelMetadata(
        operates_on="cell_column", meta_args=meta_args)
    cls = call_method("_scalar", meta_arg, metadata)
    assert len(cls._info) == 1
    # pylint: disable=unsubscriptable-object
    assert cls._info[0] == 1
    assert cls._index == 1


def test_field():
    '''Test the _field method.'''
    meta_arg = FieldArgMetadata("GH_REAL", "GH_INC", "W0")
    metadata = LFRicKernelMetadata(
        operates_on="cell_column", meta_args=[meta_arg])
    cls = call_method("_field", meta_arg, metadata)
    assert len(cls._info) == 1
    # pylint: disable=unsubscriptable-object
    assert cls._info[0] == 0
    assert cls._index == 1


def test_field_vector():
    '''Test the _field_vector method.'''
    meta_arg = FieldVectorArgMetadata("GH_REAL", "GH_INC", "W0", "3")
    metadata = LFRicKernelMetadata(
        operates_on="cell_column", meta_args=[meta_arg])
    cls = call_method("_field_vector", meta_arg, metadata)
    assert len(cls._info) == 3
    # pylint: disable=unsubscriptable-object
    assert cls._info[0] == 0
    assert cls._info[1] == 0
    assert cls._info[2] == 0
    assert cls._index == 3


def test_operator():
    '''Test the _operator method.'''
    meta_arg = OperatorArgMetadata("GH_REAL", "GH_WRITE", "W0", "W1")
    metadata = LFRicKernelMetadata(
        operates_on="cell_column", meta_args=[meta_arg])
    cls = call_method("_operator", meta_arg, metadata)
    assert len(cls._info) == 1
    # pylint: disable=unsubscriptable-object
    assert cls._info[1] == 0
    assert cls._index == 2


def test_cma_operator():
    '''Test the _cma_operator method.'''
    meta_arg = ColumnwiseOperatorArgMetadata("GH_REAL", "GH_WRITE", "W0", "W1")
    metadata = LFRicKernelMetadata(
        operates_on="cell_column", meta_args=[meta_arg])
    cls = call_method("_cma_operator", meta_arg, metadata)
    assert len(cls._info) == 1
    # pylint: disable=unsubscriptable-object
    assert cls._info[0] == 0
    assert cls._index == 8


def test_add_arg():
    '''Test the _add_arg utility method.'''
    field_meta_arg = FieldArgMetadata("GH_REAL", "GH_WRITE", "W0")
    scalar_meta_arg = ScalarArgMetadata("GH_REAL", "GH_READ")
    metadata = LFRicKernelMetadata(
        operates_on="cell_column", meta_args=[field_meta_arg, scalar_meta_arg])
    cls = ArgIndexToMetadataIndex
    cls._initialise(None)
    cls._metadata = metadata
    # call add_arg
    cls._add_arg(field_meta_arg)
    assert len(cls._info) == 1
    # pylint: disable=unsubscriptable-object
    assert cls._info[0] == 0
    assert cls._index == 1
    # call add_arg again
    cls._add_arg(scalar_meta_arg)
    assert len(cls._info) == 2
    assert cls._info[0] == 0
    assert cls._info[1] == 1
    assert cls._index == 2


def test_remaining_methods():
    '''Test the remaining methods. Each of these increments the _index
    variable by one, apart from _cell_map which increments it by four.

    '''
    cls = ArgIndexToMetadataIndex
    cls._initialise(None)
    # Check the methods that increase _index by 1 and have no arguments.
    count = 0
    for method_name in (
            ["_cell_position", "_mesh_height", "_mesh_ncell2d_no_halos",
             "_mesh_ncell2d"]):
        getattr(cls, method_name)()
        count += 1
        assert cls._index == count
    # Check the one method that increases _index by 4 and has no arguments.
    cls._cell_map()
    count += 4
    assert cls._index == count
    # Check the methods that increase _index by 1 and have an argument.
    for method_name in (
            ["_stencil_cross2d_extent", "_stencil_cross2d2d_max_extent",
             "_stencil_extent", "_stencil_xory1d_direction",
             "_stencil_cross2d", "_stencil"]):
        getattr(cls, method_name)(None)
        count += 1
        assert cls._index == count
