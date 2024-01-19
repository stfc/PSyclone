# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017-2024, Science and Technology Facilities Council.
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
# Authors R. W. Ford, A. R. Porter and S. Siso, STFC Daresbury Lab
#         I. Kavcic, Met Office
#         J. Henrichs, Bureau of Meteorology
# -----------------------------------------------------------------------------

''' Perform py.test tests on the psygen.psyir.symbols.containersymbols file '''

from __future__ import absolute_import
import os
import pytest
from psyclone.psyir.symbols import SymbolError, Symbol
from psyclone.psyir.symbols.containersymbol import ContainerSymbol, \
    ContainerSymbolInterface, FortranModuleInterface
from psyclone.psyir.nodes import Container
from psyclone.configuration import Config


def create_dummy_module(path, filename="dummy_module.f90"):
    '''Utility to generate a simple Fortran module file with the given path
    and filename. The filename must be 'dummy_module.[f|F]90' (default) to
    match the module name, but other names can also be given for testing
    purposes'''

    source = '''
    module ignore_me
    end module ignore_me
    subroutine ignore_me_too()
    end subroutine ignore_me_too
    module Dummy_Module

        integer :: a
        real :: b
        real, parameter :: c = 3.14

    end module dummy_module
    program also_ignore_me
    end program also_ignore_me
    '''
    with open(os.path.join(path, filename), "w") as mfile:
        mfile.write(source)


def test_containersymbol_initialisation():
    '''Test that a ContainerSymbol instance can be created when valid
    arguments are given, otherwise raise relevant exceptions.'''

    sym = ContainerSymbol("my_mod")
    assert isinstance(sym, ContainerSymbol)
    assert sym.name == "my_mod"
    assert sym.wildcard_import is False
    # References are not followed/evaluated until explicitly requested
    assert not sym._reference
    # Right now the FortranModuleInterface is assigned by default
    # because it is the only one. This may change in the future
    assert isinstance(sym._interface, FortranModuleInterface)

    # Test wildcard_import argument
    sym2 = ContainerSymbol("my_mod", wildcard_import=True)
    assert sym2.wildcard_import is True

    with pytest.raises(TypeError) as error:
        sym = ContainerSymbol(None)
    assert "ContainerSymbol 'name' attribute should be of type 'str'" \
        in str(error.value)

    with pytest.raises(TypeError) as error:
        sym = ContainerSymbol("name", interface="interface")
    assert ("A ContainerSymbol interface must be of type "
            "'FortranModuleInterface' but found 'str' for Container 'name'."
            in str(error.value))


def test_containersymbol_specialise_and_process_arguments():
    ''' Tests that a ContainerSymbol created from a specialisation instead of
    the constructor deals with the arguments as expected.'''
    sym1 = Symbol("symbol1")
    sym1.specialise(ContainerSymbol)
    assert isinstance(sym1, ContainerSymbol)

    # It has a wildcard_import and it default to False
    assert sym1.wildcard_import is False
    # It could use the container import infrastructure (in this case it fails
    # because it does not exist)
    with pytest.raises(SymbolError) as error:
        _ = sym1.container
    assert "not found" in str(error.value)

    # Now with a wildcard_import argument
    sym2 = Symbol("symbol1")
    sym2.specialise(ContainerSymbol, wildcard_import=True)
    assert isinstance(sym1, ContainerSymbol)
    assert sym2.wildcard_import is True


def test_containersymbol_can_be_copied():
    '''Test that a ContainerSymbol instance can be copied. '''
    symbol = ContainerSymbol("my_mod")
    symbol.wildcard_import = True
    new_symbol = symbol.copy()

    assert new_symbol is not symbol
    assert new_symbol.name == "my_mod"
    assert isinstance(new_symbol._interface, FortranModuleInterface)
    # Disable false positive no-member pylint error
    # pylint: disable=no-member
    assert new_symbol.wildcard_import is True


def test_containersymbol_str():
    '''Test that a ContainerSymbol instance can be stringified'''

    sym = ContainerSymbol("my_mod")
    assert str(sym) == "my_mod: ContainerSymbol<not linked>"

    sym._reference = Container("my_mod")
    assert str(sym) == "my_mod: ContainerSymbol<linked>"


def test_containersymbol_resolve_external_container(monkeypatch):
    '''Test that a ContainerSymbol uses its interface import_container method
    the first time its associated container reference is needed'''

    sym = ContainerSymbol("my_mod")

    monkeypatch.setattr(sym._interface, "import_container",
                        lambda x: "MockContainer")

    # At the beginning the reference is never resolved (lazy evaluation)
    assert not sym._reference

    # When container is invoked the reference is resolved
    assert sym.container == "MockContainer"
    assert sym._reference == "MockContainer"

    # Check that subsequent invocations do not update the container reference
    monkeypatch.setattr(sym._interface, "import_container",
                        staticmethod(lambda x: "OtherContainer"))
    assert sym.container == "MockContainer"


def test_containersymbol_generic_interface():
    '''Check ContainerSymbolInterface abstract methods '''

    abstractinterface = ContainerSymbolInterface

    with pytest.raises(NotImplementedError) as error:
        abstractinterface.import_container("name")
    assert "Abstract method" in str(error.value)


def test_containersymbol_fortranmodule_interface(monkeypatch, tmpdir):
    '''Check that the FortranModuleInterface imports Fortran modules
    as containers or produces the appropriate errors'''

    fminterface = FortranModuleInterface
    path = str(tmpdir)

    # Try with a non-existent module and no include path
    monkeypatch.setattr(Config.get(), "_include_paths", [])
    with pytest.raises(SymbolError) as error:
        fminterface.import_container("fake_module")
    assert ("Module 'fake_module' (expected to be found in "
            "'fake_module.[f|F]90') not found in any of the include_paths "
            "directories []." in str(error.value))

    # Try with a non-existent module and an existing directory
    monkeypatch.setattr(Config.get(), '_include_paths', [path])
    with pytest.raises(SymbolError) as error:
        fminterface.import_container("fake_module")
    assert ("Module 'fake_module' (expected to be found in "
            "'fake_module.[f|F]90') not found in any of the include_paths "
            "directories " in str(error.value))

    # Try importing an existing Fortran module
    create_dummy_module(path)
    container = fminterface.import_container("dummy_module")
    assert isinstance(container, Container)
    assert container.name.lower() == "dummy_module"

    # Import the wrong module, additionally it tests that the uppercase
    # F90 extension is also being imported as it does not produce a file
    # not found error.
    create_dummy_module(path, "different_name_module.F90")
    with pytest.raises(ValueError) as error:
        container = fminterface.import_container("different_name_module")
    assert ("Error importing the Fortran module 'different_name_module' "
            "into a PSyIR container. The file with filename "
            "'different_name_module.F90' does not contain the expected "
            "module." in str(error.value))


def test_containersymbol_wildcard_import():
    ''' Check the setter and getter for the wildcard_import property. '''
    csym = ContainerSymbol("my_mod")
    assert not csym.wildcard_import
    csym.wildcard_import = True
    assert csym.wildcard_import
    with pytest.raises(TypeError) as err:
        csym.wildcard_import = "false"
    assert "wildcard_import must be a bool but got" in str(err.value)
