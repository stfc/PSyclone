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

'''Module containing tests for the CommonMetaArgMetadata class.

'''
import pytest

from fparser.two import Fortran2003

from psyclone.domain.lfric.kernel.common_meta_arg_metadata import \
    CommonMetaArgMetadata
from psyclone.errors import InternalError


# pylint: disable=abstract-class-instantiated
def test_init_error():
    '''Test that a CommonMetaArgMetadata instance can't be created as it
    is abstract.

    '''
    with pytest.raises(TypeError) as info:
        _ = CommonMetaArgMetadata(None, None)
    assert ("Can't instantiate abstract class CommonMetaArgMetadata with "
            "abstract methods check_access, check_datatype" in str(info.value))


class CheckArg(CommonMetaArgMetadata):
    '''A utility class that allows the abstract CommonMetaArgMetadata
    class to be tested.

    '''
    form = "sluglike"
    form_arg_index = 0
    vector_length_arg_index = 0
    datatype_arg_index = 1
    access_arg_index = 2
    function_space_arg_index = 3

    @staticmethod
    def check_datatype(value):
        '''A concrete implementation of the abstract method in the
        CommonMetaArgMetadata class.

        :param str value: the value being passed into this method.

        '''

    @staticmethod
    def check_access(value):
        '''A concrete implementation of the abstract method in the
        CommonMetaArgMetadata class.

        :param str value: the value being passed into this method.

        '''

    @staticmethod
    def create_from_fparser2(fparser2_tree):
        '''A concrete implementation of the abstract method in the
        CommonMetadata class.

        :param fparser2_tree: fparser2 tree containing the metadata \
            for a scalar argument.
        :type fparser2_tree: :py:class:`fparser.two.Fortran2003.Base`

        '''


def test_init():

    '''Test that the CommonMetaArgMetadata class can be created when a
    concrete class subclasses it. Also check that the
    CommonMetaArgMetadata class stores the datatype and access
    arguments supplied to it correctly.

    '''
    dummy = CheckArg("datatype", "access")
    assert dummy._datatype == "datatype"
    assert dummy._access == "access"


def test_check_first_arg():
    '''Check that the check_first_arg method in the CommonMetaArgMetadata
    class works as expected.

    '''
    fparser2_tree = CheckArg.create_fparser2(
        "arg_type(first_arg)", Fortran2003.Part_Ref)
    with pytest.raises(ValueError) as info:
        CheckArg.check_first_arg(fparser2_tree, "MyTest")
        assert ("MyTests should have sluglike as their first metadata "
                "argument, but found 'first_arg'." in str(info.value))

    fparser2_tree = CheckArg.create_fparser2(
        "arg_type(first_arg*3)", Fortran2003.Part_Ref)
    with pytest.raises(ValueError) as info:
        CheckArg.check_first_arg(fparser2_tree, "MyTest", vector=True)
        assert ("MyTests should have sluglike as their first metadata "
                "argument, but found 'first_arg'." in str(info.value))

    fparser2_tree = CheckArg.create_fparser2(
        "arg_type(sluglike)", Fortran2003.Part_Ref)
    CheckArg.check_first_arg(fparser2_tree, "MyTest")


def test_check_remaining_args():
    '''Check that the check_remaining_args method in the CommonMetaArgMetadata
    class works as expected.

    '''
    class DummyArg(CheckArg):
        '''Utility class used to test the abstract CommonMetaArgMetadata class
        (via the CheckArg class).

        :param str message: the message to use for an exception.

        :raises ValueError: when instantiated.

        '''
        datatype_arg_index = 1
        access_arg_index = 2
        function_space_arg_index = 3
        mesh_arg_index = 4
        function_space_to_arg_index = 5
        function_space_from_arg_index = 6

        def __init__(self, message):
            super().__init__("datatype", "access")
            raise ValueError(message)

    for index, message in [(1, "datatype descriptor error"),
                           (2, "access descriptor error"),
                           (3, "function space error"),
                           (4, "mesh_arg error"),
                           (5, "function_space_to error"),
                           (6, "function_space_from error")]:
        with pytest.raises(ValueError) as info:
            DummyArg.check_remaining_args("dummy", message)
            assert (f"At argument index '{index}' for metadata 'dummy'. "
                    f"{message}")

    with pytest.raises(InternalError) as info:
        DummyArg.check_remaining_args("dummy", "Unrecognised error")
    assert ("Unexpected error message found 'Unrecognised error'"
            in str(info.value))


def test_get_type_and_access():
    '''Test that the get_type_and_access method in the
    CommonMetaArgMetadata class works as expected.

    '''
    fparser_tree = CheckArg.create_fparser2(
        "arg_type(GH_FIELD, GH_REAL, GH_READ)", Fortran2003.Part_Ref)
    datatype, access = CheckArg.get_type_and_access(fparser_tree)
    assert datatype == "GH_REAL"
    assert access == "GH_READ"


def test_get_type_access_and_fs():
    '''Test that the get_type_access_and_fs method in the
    CommonMetaArgMetadata class works as expected.

    '''
    fparser_tree = CheckArg.create_fparser2(
        "arg_type(GH_FIELD, GH_REAL, GH_READ, W0)", Fortran2003.Part_Ref)
    datatype, access, function_space = CheckArg.get_type_access_and_fs(
        fparser_tree)
    assert datatype == "GH_REAL"
    assert access == "GH_READ"
    assert function_space == "W0"


def test_get_and_check_vector_length():
    '''Test that the get_and_check_vector_length method in the
    CommonMetaArgMetadata class works as expected.

    '''
    fparser_tree = CheckArg.create_fparser2(
        "arg_type(GH_FIELD, GH_REAL, GH_READ, W0)", Fortran2003.Part_Ref)
    with pytest.raises(TypeError) as info:
        _ = CheckArg.get_and_check_vector_length(fparser_tree)
    assert ("The vector length metadata should be in the form "
            "'form*vector_length' but found 'GH_FIELD'."
            in str(info.value))

    fparser_tree = CheckArg.create_fparser2(
        "arg_type(GH_FIELD*3, GH_REAL, GH_READ, W0)", Fortran2003.Part_Ref)
    vector_length = CheckArg.get_and_check_vector_length(fparser_tree)
    assert vector_length == "3"


def test_setter_getter():
    '''Test that the setters and getters in the CommonMetaArgMetadata
    class work as expected.

    '''
    dummy = CheckArg("invalid", "invalid")

    dummy.datatype = "cheese"
    assert dummy._datatype == "cheese"
    assert dummy.datatype == "cheese"

    dummy.access = "crackers"
    assert dummy._access == "crackers"
    assert dummy.access == "crackers"
