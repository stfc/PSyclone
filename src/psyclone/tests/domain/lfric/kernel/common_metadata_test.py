# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2022-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

'''Module containing tests for the CommonMetadata class.

'''
import pytest

from fparser.two import Fortran2003

from psyclone.domain.lfric.kernel import (
    CommonMetadata, MetaMeshArgMetadata, LFRicKernelMetadata)


def test_init():
    '''Test that a CommonMetadata instance can't be created as it is
    abstract.

    '''
    # pylint: disable=abstract-class-instantiated
    with pytest.raises(TypeError) as info:
        _ = CommonMetadata()
    # Python >= 3.9 spots that 'method' should be singular. Prior to this it
    # was plural. Python >= 3.12 tweaks the error message yet again to mention
    # the lack of an implementation and to quote the method name.
    # We split the check to accommodate for this.
    assert ("Can't instantiate abstract class CommonMetadata with"
            in str(info.value))
    assert ("abstract method" in str(info.value))
    assert ("create_from_fparser2" in str(info.value))
    # pylint: enable=abstract-class-instantiated


def test_check_fparser2():
    '''Test that the check_fparser2 method in the CommonMetadata class
    works as expected.

    '''
    fortran_string = "program test\nend program"
    fparser2_tree = CommonMetadata.create_fparser2(
        fortran_string, Fortran2003.Program)
    _ = CommonMetadata.check_fparser2(fparser2_tree, Fortran2003.Program)

    with pytest.raises(TypeError) as info:
        _ = CommonMetadata.check_fparser2("invalid", Fortran2003.Program)
    assert ("Expected kernel metadata to be encoded as an fparser2 Program "
            "object but found type 'str' with value 'invalid'."
            in str(info.value))


def test_validate_scalar_value():
    '''Test that the validate_scalar_value method behaves as
    expected.

    '''
    with pytest.raises(TypeError) as info:
        CommonMetadata.validate_scalar_value(None, None, None)
    assert ("The 'None' value should be of type str, but found 'NoneType'."
            in str(info.value))
    with pytest.raises(ValueError) as info:
        CommonMetadata.validate_scalar_value(
            "invalid", ["value1", "value2"], "my_metadata")
    assert ("The 'my_metadata' metadata should be a recognised value (one of "
            "['value1', 'value2']) but found 'invalid'." in str(info.value))
    CommonMetadata.validate_scalar_value(
            "Value2", ["value1", "value2"], "")


def test_create_fparser2():
    '''Test that the create_fparser2 method in the CommonMetadata class
    works as expected.

    '''
    encoding = Fortran2003.Part_Ref
    fortran_string = "arg_type(GH_FIELD, GH_REAL, GH_READ)"
    result = CommonMetadata.create_fparser2(fortran_string, encoding)
    assert isinstance(result, encoding)

    with pytest.raises(ValueError) as info:
        _ = CommonMetadata.create_fparser2("#!$%", encoding)
    assert ("Expected kernel metadata to be a Fortran Part_Ref, but found "
            "'#!$%'." in str(info.value))

    with pytest.raises(ValueError) as info:
        _ = LFRicKernelMetadata.create_fparser2(
            "hello", Fortran2003.Derived_Type_Def)
    assert ("Expected kernel metadata to be a Fortran Derived_Type_Def, "
            "but found 'hello'." in str(info.value))


def test_create_from_fortran_string():
    '''Test the create_from_fortran_string() method. Test with an example
    subclass (MetaMeshArgMetadata).

    '''
    # Makes use of Fortran2003.Part_Ref.
    meta = MetaMeshArgMetadata.create_from_fortran_string(
        "mesh_data_type(adjacent_face)")
    assert isinstance(meta, MetaMeshArgMetadata)
    assert meta.mesh == "adjacent_face"
