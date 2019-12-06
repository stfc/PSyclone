# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017-2019, Science and Technology Facilities Council.
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
from psyclone.psyir.symbols import SymbolError
from psyclone.psyir.symbols.containersymbol import ContainerSymbol, \
    ContainerSymbolInterface, FortranModuleInterface
from psyclone.psyir.symbols.datasymbol import DataSymbol
from psyclone.psyir.symbols.datatypes import DataType

from psyclone.psyGen import Container
from psyclone.configuration import Config


def create_dummy_module(path, filename="dummy_module.f90"):
    '''Utility to generate a simple Fortran module file with the given path
    and filename. The filename must be 'dummy_module.[f|F]90' (default) to
    match the module name, but other names can also be given for testing
    purposes'''

    source = '''
    module dummy_module

        integer :: a
        real :: b
        real, parameter :: c = 3.14

    end module dummy_module
    '''
    with open(os.path.join(path, filename), "w") as mfile:
        mfile.write(source)


def test_containersymbol_initialisation():
    '''Test that a ContainerSymbol instance can be created when valid
    arguments are given, otherwise raise relevant exceptions.'''

    sym = ContainerSymbol("my_mod")
    assert isinstance(sym, ContainerSymbol)
    assert sym.name == "my_mod"
    assert not sym._reference  # References are not evaluated until told
    # Right now the FortranModuleInterface is assigned by default
    # because it is the only one. This may change in the future
    assert sym._interface == FortranModuleInterface
    # Upon creation we have no associated datasymbols
    assert sym._datasymbols == set()

    with pytest.raises(TypeError) as error:
        sym = ContainerSymbol(None)
    assert "ContainerSymbol name attribute should be of type 'str'" \
        in str(error.value)


def test_containersymbol_str():
    '''Test that a ContainerSymbol instance can be stringified'''

    sym = ContainerSymbol("my_mod")
    assert str(sym) == "my_mod: <not linked>"

    sym._reference = Container("my_mod")
    assert str(sym) == "my_mod: <linked>"


def test_containersymbol_resolve_external_container(monkeypatch):
    '''Test that a ContainerSymbol uses its interface import_container method
    the first time its associated container reference is needed'''

    sym = ContainerSymbol("my_mod")

    monkeypatch.setattr(sym._interface, "import_container",
                        staticmethod(lambda x: "MockContainer"))

    # At the beggining the reference is never resolved (lazy evaluation)
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

    # Try with a non-existant module and no include path
    monkeypatch.setattr(Config.get(), "_include_paths", [])
    with pytest.raises(SymbolError) as error:
        fminterface.import_container("fake_module")
    assert ("Module 'fake_module' (expected to be found in "
            "'fake_module.[f|F]90') not found in any of the include_paths "
            "directories []." in str(error.value))

    # Try with a non-existant module and an existing directory
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
    assert container.name == "dummy_module"

    # Import the wrong module, additionally it tests that the uppercase
    # F90 extension is also being imported as it does not produce a file
    # not found error.
    create_dummy_module(path, "different_name_module.F90")
    with pytest.raises(ValueError) as error:
        container = fminterface.import_container("different_name_module")
    assert ("Error importing the Fortran module 'different_name_module' into "
            "a PSyIR container. The imported module has the unexpected name: "
            "'dummy_module'." in str(error.value))


def test_containersymbol_importlist():
    ''' Check the setters and getters for the imported-symbol list. '''
    csym = ContainerSymbol("my_mod")
    with pytest.raises(TypeError) as err:
        csym.add_symbol_import(csym)
    assert ("Expected an argument of type DataSymbol but got: "
            "'ContainerSymbol'" in str(err.value))
    assert csym.imported_symbols == []
    assert not csym.has_wildcard_import
    dsym1 = DataSymbol("var1", DataType.INTEGER)
    csym.add_symbol_import(dsym1)
    assert csym.imported_symbols == [dsym1]
    dsym2 = DataSymbol("var2", DataType.REAL)
    csym.add_symbol_import(dsym2)
    assert dsym2 in csym.imported_symbols
    # Try adding the same symbol again
    csym.add_symbol_import(dsym2)
    assert csym.imported_symbols.count(dsym2) == 1
    assert len(csym.imported_symbols) == 2
    assert not csym.has_wildcard_import
    csym.add_wildcard_import()
    assert csym.has_wildcard_import
