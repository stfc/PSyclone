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

'''Module containing tests for the CommonArg class.

'''
import pytest

from fparser.two import Fortran2003

from psyclone.domain.lfric.kernel.common_arg import CommonArg
from psyclone.errors import InternalError


# pylint: disable=abstract-class-instantiated
def test_init_error():
    '''Test that a CommonArg instance can't be created as it is abstract.

    '''
    with pytest.raises(TypeError) as info:
        _ = CommonArg()
    assert ("Can't instantiate abstract class CommonArg with abstract "
            "methods check_access, check_datatype" in str(info.value))


class CheckArg(CommonArg):
    '''A utility class that allow the abstract CommonArg class to be
    tested.

    '''
    form = "sluglike"
    form_arg_index = 0

    @staticmethod
    def check_datatype(value):
        '''A concrete implementation of the abstract method in the CommonArg
        class.

        :param str value: the value being passed into this method.

        '''

    @staticmethod
    def check_access(value):
        '''A concrete implementation of the abstract method in the CommonArg
        class.

        :param str value: the value being passed into this method.

        '''


def test_init():
    '''Test that the CommonArg class can be created when a concrete class
    subclasses it. Also check that the CommonArg class stores the
    datatype and access arguments supplied to it correctly and the
    associated setter and getter methods work as expected.

    '''
    dummy = CheckArg()
    assert dummy._datatype is None
    assert dummy._access is None

    dummy = CheckArg(datatype="hello", access="there")
    assert dummy._datatype == "hello"
    assert dummy.datatype == "hello"
    assert dummy._access == "there"
    assert dummy.access == "there"


def test_create_fparser2():
    '''Test that the create_fparser2 method in the CommonArg class works
    as expected.

    '''
    dummy = CheckArg()
    fortran_string = "arg_type(GH_FIELD, GH_REAL, GH_READ)"
    result = dummy.create_fparser2(fortran_string)
    assert isinstance(result, Fortran2003.Part_Ref)

    fortran_string = ("arg_type(GH_FIELD, GH_REAL, GH_READ, W0, "
                      "gh_mesh=GH_COARSE)")
    result = dummy.create_fparser2(
        fortran_string, encoding=Fortran2003.Structure_Constructor)
    assert isinstance(result, Fortran2003.Structure_Constructor)

    with pytest.raises(ValueError) as info:
        _ = dummy.create_fparser2("#!$%")
    assert ("Expected kernel metadata to be a Fortran Part_Ref, with the "
            "form 'arg_type(...)' but found '#!$%'." in str(info.value))


def test_check_fparser2():
    '''Test that the check_fparser2 method in the CommonArg class works
    as expected.

    '''
    dummy = CheckArg()
    with pytest.raises(TypeError) as info:
        _ = dummy.check_fparser2(None)
    assert ("Expected kernel metadata to be encoded as an fparser2 Part_Ref "
            "object but found type 'NoneType' with value 'None'."
            in str(info.value))

    fparser_tree = dummy.create_fparser2(
        "braz_type(GH_FIELD, GH_REAL, GH_READ)")
    with pytest.raises(ValueError) as info:
        _ = dummy.check_fparser2(fparser_tree)
    assert ("Expected kernel metadata to have the name 'arg_type' and be in "
            "the form 'arg_type(...)', but found 'braz_type(GH_FIELD, "
            "GH_REAL, GH_READ)'." in str(info.value))

    fparser_tree = dummy.create_fparser2(
        "arg_type(GH_FIELD, GH_REAL, GH_READ)")
    with pytest.raises(ValueError) as info:
        _ = dummy.check_fparser2(fparser_tree)
    assert ("Expected kernel metadata to have 4 arguments, but found 3 in "
            "'arg_type(GH_FIELD, GH_REAL, GH_READ)'." in str(info.value))


def test_check_first_arg():
    '''Check that the check_first_arg method in the CommonArg class works
    as expected

    '''
    fparser2_tree = CheckArg.create_fparser2("arg_type(first_arg)")
    with pytest.raises(ValueError) as info:
        CheckArg.check_first_arg(fparser2_tree, "MyTest")
        assert ("MyTests should have sluglike as their first metadata "
                "argument, but found 'first_arg'." in str(info.value))

    fparser2_tree = CheckArg.create_fparser2("arg_type(first_arg*3)")
    with pytest.raises(ValueError) as info:
        CheckArg.check_first_arg(fparser2_tree, "MyTest", vector=True)
        assert ("MyTests should have sluglike as their first metadata "
                "argument, but found 'first_arg'." in str(info.value))

    fparser2_tree = CheckArg.create_fparser2("arg_type(sluglike)")
    CheckArg.check_first_arg(fparser2_tree, "MyTest")


def test_check_remaining_args():
    '''Check that the check_remaining_args method in the CommonArg class
    works as expected

    '''
    class DummyArg(CheckArg):
        ''' xxx '''
        datatype_arg_index = 1
        access_arg_index = 2
        function_space_arg_index = 3
        mesh_arg_index = 4
        function_space_to_arg_index = 5
        function_space_from_arg_index = 6

        def __init__(self, message):
            ''' xxx '''
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


def test_get_arg():
    '''Test that the get_arg method in the CommonArg class works
    as expected.

    '''
    dummy = CheckArg()
    fparser_tree = dummy.create_fparser2(
        "arg_type(GH_FIELD, GH_REAL, GH_READ)")
    assert dummy.get_arg(fparser_tree, 0) == "GH_FIELD"
    assert dummy.get_arg(fparser_tree, 1) == "GH_REAL"
    assert dummy.get_arg(fparser_tree, 2) == "GH_READ"


def test_get_type_and_access():
    '''Test that the get_type_and_access method in the CommonArg class
    works as expected.

    '''
    dummy = CheckArg()
    fparser_tree = dummy.create_fparser2(
        "arg_type(GH_FIELD, GH_REAL, GH_READ)")
    datatype, access = dummy.get_type_and_access(fparser_tree, 1, 2)
    assert datatype == "GH_REAL"
    assert access == "GH_READ"


def test_get_type_access_and_fs():
    '''Test that the get_type_access_and_fs method in the CommonArg class
    works as expected.

    '''
    dummy = CheckArg()
    fparser_tree = dummy.create_fparser2(
        "arg_type(GH_FIELD, GH_REAL, GH_READ, W0)")
    datatype, access, function_space = dummy.get_type_access_and_fs(
        fparser_tree, 1, 2, 3)
    assert datatype == "GH_REAL"
    assert access == "GH_READ"
    assert function_space == "W0"


def test_get_and_check_vector_length():
    '''Test that the get_and_check_vector_length method in the CommonArg
    class works as expected.

    '''
    dummy = CheckArg()
    fparser_tree = dummy.create_fparser2(
        "arg_type(GH_FIELD, GH_REAL, GH_READ, W0)")
    with pytest.raises(TypeError) as info:
        _ = dummy.get_and_check_vector_length(fparser_tree, 0)
    assert ("The vector length metadata should be in the form "
            "'form*vector_length' but found 'GH_FIELD'."
            in str(info.value))

    fparser_tree = dummy.create_fparser2(
        "arg_type(GH_FIELD*3, GH_REAL, GH_READ, W0)")
    vector_length = dummy.get_and_check_vector_length(fparser_tree, 0)
    assert vector_length == "3"


def test_check_methods():
    '''Check that the check_datatype and check_access methods get called
    from the datatype and access setter methods respectively.

    '''
    class Test(CommonArg):
        '''A utility class that allow the abstract CommonArg class to be
        tested.

        '''

        @staticmethod
        def check_datatype(value):
            '''A concrete implementation of the abstract method in the
            CommonArg class.

            :param str value: the value being passed into this method.

            :raises NotImplementedError: so we can check that the \
                method is called.

            '''
            raise NotImplementedError("check_datatype")

        @staticmethod
        def check_access(value):
            '''A concrete implementation of the abstract method in the
            CommonArg class.

            :param str value: the value being passed into this method.

            :raises NotImplementedError: so we can check that the \
                method is called.

            '''
            raise NotImplementedError("check_access")

    test = Test()

    # The setter calls the datatype setter which should call the
    # check_datatype routine. The check_datatype routine has been set
    # to raise an exception.
    with pytest.raises(NotImplementedError) as info:
        test.datatype = "dummy"
    assert "check_datatype" in str(info.value)

    # The setter calls the access setter which should call the
    # check_access routine. The check_access routine has been set
    # to raise an exception.
    with pytest.raises(NotImplementedError) as info:
        test.access = "dummy"
    assert "check_access" in str(info.value)
