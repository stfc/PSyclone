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


def test_create_from_fortran_string():
    '''Test that the create_from_fortran_string method works as
    expected.

    '''
    fortran_string = "integer :: operates_on = cell_column"
    operates_on_metadata = OperatesOnMetadata.create_from_fortran_string(
        fortran_string)
    assert operates_on_metadata.operates_on == "cell_column"


def test_create_from_fparser2():
    '''Test that the create_from_fparser2 method works as expected.'''
    fortran_string = "integer :: operates_on = cell_column"
    fparser2_tree = OperatesOnMetadata.create_fparser2(
        fortran_string, Fortran2003.Data_Component_Def_Stmt)
    operates_on_metadata = OperatesOnMetadata.create_from_fparser2(
        fparser2_tree)
    assert isinstance(operates_on_metadata, OperatesOnMetadata)
    assert operates_on_metadata.operates_on == "cell_column"


def test_setter_getter():
    '''Test that the setters and getters work as expected.'''
    value = "domain"
    operates_on_metadata = OperatesOnMetadata(value)
    assert operates_on_metadata.operates_on == value

    value = "cell_column"
    operates_on_metadata.operates_on = value
    assert operates_on_metadata._operates_on == value

    value = "DOMAIN"
    operates_on_metadata.operates_on = value
    assert operates_on_metadata._operates_on == value.lower()


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
            "['cell_column', 'domain', 'dof']) but found 'invalid'."
            in str(info.value))
