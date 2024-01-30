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

'''Module containing tests for the MetaFuncsMetadata class.

'''
import pytest

from fparser.two import Fortran2003

from psyclone.domain.lfric.kernel import (
    MetaFuncsMetadata, MetaFuncsArgMetadata)


def test_init():
    '''Test that an instance of MetaFuncsMetadata can be created
    and that its initial values are stored as expected.

    '''
    meta_funcs_arg = MetaFuncsArgMetadata("w0", basis_function=True)
    values = [meta_funcs_arg]
    metadata = MetaFuncsMetadata(values)
    assert isinstance(metadata, MetaFuncsMetadata)
    assert isinstance(metadata._meta_funcs_args, list)
    assert len(metadata._meta_funcs_args) == 1
    assert metadata._meta_funcs_args[0] is meta_funcs_arg


def test_init_error():
    '''Test that invalid input to the constructor causes the expected
    exception to be raised.

    '''
    with pytest.raises(TypeError) as info:
        _ = MetaFuncsMetadata(None)
    assert ("MetaFuncsMetadata values should be provided as a list but found "
            "'NoneType'." in str(info.value))


def test_fortran_string():
    '''Test that the fortran_string method works as expected.'''
    meta_funcs_arg = MetaFuncsArgMetadata("w0", basis_function=True)
    values = [meta_funcs_arg]
    metadata = MetaFuncsMetadata(values)
    fortran_string = metadata.fortran_string()
    expected = (
        f"type(FUNC_TYPE) :: META_FUNCS(1) = (/ &\n"
        f"    {meta_funcs_arg.fortran_string()}/)\n")
    assert fortran_string == expected


def test_create_from_fortran_string():
    '''Test that the create_from_fortran_string method works as
    expected.

    '''
    meta_funcs_arg1 = MetaFuncsArgMetadata("w0", basis_function=True)
    meta_funcs_arg2 = MetaFuncsArgMetadata(
        "w2", basis_function=True, diff_basis_function=True)
    values = [meta_funcs_arg1, meta_funcs_arg2]
    values_list_str = [value.fortran_string() for value in values]
    values_str = ", ".join(values_list_str)
    fortran_string = (f"type(func_type) :: meta_funcs({len(values)}) = "
                      f"(/{values_str}/)\n")
    metadata = MetaFuncsMetadata.create_from_fortran_string(
        fortran_string)
    assert isinstance(metadata.meta_funcs_args, list)
    assert len(metadata.meta_funcs_args) == 2
    assert metadata.meta_funcs_args[0].fortran_string() == \
        meta_funcs_arg1.fortran_string()
    assert metadata.meta_funcs_args[1].fortran_string() == \
        meta_funcs_arg2.fortran_string()


def test_create_from_fparser2():
    '''Test that the create_from_fparser2 method works as expected.'''

    fortran_string = (
        "type(FUNC_TYPE) :: META_FUNCS(2) = (/ &\n"
        "    func_type(w0, gh_basis), &\n"
        "    func_type(w3, gh_diff_basis)/)\n")
    fparser2_tree = MetaFuncsMetadata.create_fparser2(
        fortran_string, Fortran2003.Data_Component_Def_Stmt)
    metadata = MetaFuncsMetadata.create_from_fparser2(fparser2_tree)
    assert isinstance(metadata, MetaFuncsMetadata)
    assert metadata.fortran_string() == fortran_string


def test_setter_getter():
    '''Test that the setters and getters work as expected.'''
    values = [MetaFuncsArgMetadata("w0", basis_function=True),
              MetaFuncsArgMetadata("w2", diff_basis_function=True)]
    metadata = MetaFuncsMetadata(values)
    assert metadata.meta_funcs_args == values
    # Check that the getter makes a copy of the list
    assert metadata.meta_funcs_args is not metadata._meta_funcs_args

    metadata.meta_funcs_values = values
    assert metadata._meta_funcs_args == values
    # Check that the setter makes a copy of the list
    assert metadata._meta_funcs_args is not values


def test_setter_errors():
    '''Test that the setter raises the expected exceptions.'''

    values = [MetaFuncsArgMetadata("w0", basis_function=True),
              MetaFuncsArgMetadata("w2", diff_basis_function=True)]
    metadata = MetaFuncsMetadata(values)

    with pytest.raises(TypeError) as info:
        metadata.meta_funcs_args = "invalid"
    assert ("MetaFuncsMetadata values should be provided as a list but found "
            "'str'." in str(info.value))

    with pytest.raises(TypeError) as info:
        metadata.meta_funcs_args = []
    assert ("The MetaFuncsMetadata list should contain at least one entry, "
            "but it is empty." in str(info.value))

    with pytest.raises(TypeError) as info:
        metadata.meta_funcs_args = [None]
    assert ("The MetaFuncsMetadata list should be a list containing objects "
            "of type MetaFuncsArgMetadata but found 'None', which is of "
            "type 'NoneType'." in str(info.value))

    with pytest.raises(TypeError) as info:
        metadata.meta_funcs_args = ["invalid"]
    assert ("The MetaFuncsMetadata list should be a list containing objects "
            "of type MetaFuncsArgMetadata but found 'invalid', which is of "
            "type 'str'." in str(info.value))
