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

'''Module containing tests for the CommonMetaArgMetadata class.

'''
import pytest

from fparser.two import Fortran2003

from psyclone.domain.lfric.kernel import (
    CommonMetaArgMetadata, ScalarArgMetadata)
from psyclone.errors import InternalError


# pylint: disable=abstract-class-instantiated
def test_init_error():
    '''Test that a CommonMetaArgMetadata instance can't be created as it
    is abstract.

    '''
    with pytest.raises(TypeError) as info:
        _ = CommonMetaArgMetadata(None, None)
    # Python >= 3.12 tweaks the error message to mention
    # the lack of an implementation and to quote the method names.
    # We split the check to accomodate for this.
    assert ("Can't instantiate abstract class CommonMetaArgMetadata with"
            in str(info.value))
    assert ("abstract methods" in str(info.value))
    assert ("_get_metadata" in str(info.value))
    assert ("check_access" in str(info.value))
    assert ("check_datatype" in str(info.value))
# pylint: enable=abstract-class-instantiated


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
    check_name = "check-arg"

    @classmethod
    def _get_metadata(cls, fparser2_tree):
        '''A concrete implementation of the abstract method in the
        CommonMetaArgMetadata class.

        :param fparser2_tree: fparser2 tree containing the metadata \
            for this argument.
        :type fparser2_tree: :py:class:`fparser.two.Fortran2003.Part_Ref` | \
            :py:class:`fparser.two.Fortran2003.Structure_Constructor`

        :raises NotImplementedError: if this test class method is called.

        '''
        raise NotImplementedError("CheckArg._get_metadata() called.")

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


def test_init():

    '''Test that the CommonMetaArgMetadata class can be created when a
    concrete class subclasses it. Also check that the
    CommonMetaArgMetadata class stores the datatype and access
    arguments supplied to it correctly.

    '''
    dummy = CheckArg("datatype", "access")
    assert dummy._datatype == "datatype"
    assert dummy._access == "access"


def test_create_from_fparser2():
    '''Check that the create_from_fparser2 method works as expected. The
    ScalarArg subclass is used to help perform the checks.

    '''
    # _get_metadata called
    with pytest.raises(TypeError) as info:
        ScalarArgMetadata.create_from_fparser2(None)
    assert ("Expected kernel metadata to be encoded as an fparser2 Part_Ref "
            "object but found type 'NoneType' with value 'None'."
            in str(info.value))

    # check_remaining_args called
    fparser2_tree = ScalarArgMetadata.create_fparser2(
        "hello(x)", Fortran2003.Part_Ref)

    with pytest.raises(ValueError) as info:
        _ = ScalarArgMetadata.create_from_fparser2(fparser2_tree)
    assert ("Expected kernel metadata to have the name 'arg_type' "
            "and be in the form 'arg_type(...)', but found 'hello(x)'."
            in str(info.value))

    # expected class returned
    fparser2_tree = ScalarArgMetadata.create_fparser2(
        "arg_type(GH_SCALAR, GH_REAL, GH_READ)", Fortran2003.Part_Ref)
    obj = ScalarArgMetadata.create_from_fparser2(fparser2_tree)
    assert isinstance(obj, ScalarArgMetadata)
    assert obj.form == "gh_scalar"
    assert obj._datatype == "gh_real"
    assert obj._access == "gh_read"


def test_check_first_arg():
    '''Check that the check_first_arg method in the CommonMetaArgMetadata
    class works as expected.

    '''
    fparser2_tree = CheckArg.create_fparser2(
        "arg_type(first_arg)", Fortran2003.Part_Ref)
    with pytest.raises(ValueError) as info:
        CheckArg.check_first_arg(fparser2_tree)
        assert ("check-arg should have 'sluglike' as their first metadata "
                "argument, but found 'first_arg'." in str(info.value))

    class CheckArgVec(CheckArg):
        '''A utility vector class.'''
        vector = True

    fparser2_tree = CheckArgVec.create_fparser2(
        "arg_type(sluglike*3)", Fortran2003.Part_Ref)
    CheckArgVec.check_first_arg(fparser2_tree)

    fparser2_tree = CheckArg.create_fparser2(
        "arg_type(sluglike)", Fortran2003.Part_Ref)
    CheckArg.check_first_arg(fparser2_tree)


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


def test_check_nargs():
    '''Check that the check_nargs method behaves as expected.'''

    fparser2_tree = CheckArg.create_fparser2(
        "arg_type(GH_FIELD, GH_REAL, GH_READ, W0)", Fortran2003.Part_Ref)
    with pytest.raises(ValueError) as info:
        CheckArg.check_nargs(fparser2_tree)
    assert ("Expected kernel metadata to have 1 arguments, but found 4 in "
            "'arg_type(GH_FIELD, GH_REAL, GH_READ, W0)'." in str(info.value))
    fparser2_tree = CheckArg.create_fparser2(
        "arg_type(GH_FIELD)", Fortran2003.Part_Ref)
    CheckArg.check_nargs(fparser2_tree)


def test_get_vector_length():
    '''Test that the get_vector_length method in the
    CommonMetaArgMetadata class works as expected.

    '''
    fparser_tree = CheckArg.create_fparser2(
        "arg_type(GH_FIELD, GH_REAL, GH_READ, W0)", Fortran2003.Part_Ref)
    with pytest.raises(TypeError) as info:
        _ = CheckArg.get_vector_length(fparser_tree)
    assert ("The vector length metadata should be in the form "
            "'form*vector_length' but found 'GH_FIELD'."
            in str(info.value))

    fparser_tree = CheckArg.create_fparser2(
        "arg_type(GH_FIELD*3, GH_REAL, GH_READ, W0)", Fortran2003.Part_Ref)
    vector_length = CheckArg.get_vector_length(fparser_tree)
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

    # pylint: disable=super-init-not-called
    class CheckArg2(CheckArg):
        '''Utility class to help check the calling of check_datatype and
        check_access from the relevant setters.

        '''
        def __init__(self):
            self._datatype = None
            self._access = None

        @staticmethod
        def check_datatype(value):
            '''Raises an exception so that the calling of this method can be
            tested.

            :param str value: an input value.

            :raises NotImplementedError: if this method is called.

            '''
            raise NotImplementedError(f"check_datatype({value})")

        @staticmethod
        def check_access(value):
            '''Raises an exception so that the calling of this method can be
            tested.

            :param str value: an input value.

            :raises NotImplementedError: if this method is called.

            '''
            raise NotImplementedError(f"check_access({value})")

    dummy = CheckArg2()
    with pytest.raises(NotImplementedError) as info:
        dummy.datatype = None
    assert "check_datatype(None)" in str(info.value)
    with pytest.raises(NotImplementedError) as info:
        dummy.access = None
    assert "check_access(None)" in str(info.value)
