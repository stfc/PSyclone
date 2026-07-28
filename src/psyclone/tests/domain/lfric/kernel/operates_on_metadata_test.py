# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2022-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

'''Module containing tests for the OperatesOnMetadata class.

'''
import pytest

from fparser.two import Fortran2003

from psyclone.domain.lfric.kernel import OperatesOnMetadata


def test_init():
    '''Test that an instance of OperatesOnMetadata can be created and that its
    initial values are stored as expected.

    '''
    value = "domain"
    operates_on_metadata = OperatesOnMetadata(value)
    assert isinstance(operates_on_metadata, OperatesOnMetadata)
    assert operates_on_metadata._operates_on == value


def test_init_error():
    '''Test that invalid input to the constructor causes the expected
    exception to be raised.

    '''
    with pytest.raises(TypeError) as info:
        _ = OperatesOnMetadata(None)
    assert ("The 'OPERATES_ON' value should be of type str, but found "
            "'NoneType'." in str(info.value))


def test_fortran_string():
    '''Test that the fortran_string method works as expected.'''

    operates_on_metadata = OperatesOnMetadata("DOMAIN")
    fortran_string = operates_on_metadata.fortran_string()
    expected = ("INTEGER :: OPERATES_ON = domain\n")
    assert fortran_string == expected


def test_create_from_fparser2():
    '''Test that the create_from_fparser2 method works as expected.'''
    fortran_string = "integer :: operates_on = cell_column"
    fparser2_tree = OperatesOnMetadata.create_fparser2(
        fortran_string, Fortran2003.Data_Component_Def_Stmt)
    operates_on_metadata = OperatesOnMetadata.create_from_fparser2(
        fparser2_tree)
    assert isinstance(operates_on_metadata, OperatesOnMetadata)
    assert operates_on_metadata.operates_on == "cell_column"


@pytest.mark.parametrize("value", ["domain", "cell_column", "DOMAIN",
                                   "halo_cell_column", "dof",
                                   "owned_dof", "owned_cell_column",
                                   "owned_and_halo_cell_column"])
def test_setter_getter(value):
    '''Test that the setters and getters work as expected.'''
    operates_on_metadata = OperatesOnMetadata(value)
    assert operates_on_metadata.operates_on == value.lower()


def test_setter_errors():
    '''Test that the setter raises the expected exceptions.'''

    operates_on_metadata = OperatesOnMetadata("cell_column")

    with pytest.raises(TypeError) as info:
        operates_on_metadata.operates_on = None
    assert ("The 'OPERATES_ON' value should be of type str, but found "
            "'NoneType'." in str(info.value))

    with pytest.raises(ValueError) as info:
        operates_on_metadata.operates_on = "invalid"
    assert ("The 'OPERATES_ON' metadata should be a recognised value (one of "
            "['domain', 'dof', 'owned_dof', 'cell_column', "
            "'owned_cell_column', 'halo_cell_column', "
            "'owned_and_halo_cell_column']) "
            "but found 'invalid'." in str(info.value))
