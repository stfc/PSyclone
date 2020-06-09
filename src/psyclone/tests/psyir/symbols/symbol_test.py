# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2020, Science and Technology Facilities Council.
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
# Author S. Siso, STFC Daresbury Lab
# Modified by R. W. Ford, STFC Daresbury Lab
# -----------------------------------------------------------------------------

''' Perform py.test tests on the psygen.psyir.symbols.symbol module '''

from __future__ import absolute_import
import pytest
from psyclone.psyir.symbols import Symbol, LocalInterface, GlobalInterface, \
    ArgumentInterface, UnresolvedInterface, ContainerSymbol
from psyclone.psyir.symbols.symbol import SymbolInterface


def test_symbol_initialisation():
    '''Test that a Symbol instance can be created when valid arguments are
    given, otherwise raise relevant exceptions. Also tests the
    internal Visibility class, the name, visibility and interface properties.

    '''
    sym = Symbol("sym1")
    assert isinstance(sym, Symbol)
    assert sym.name == "sym1"
    assert sym.visibility == Symbol.DEFAULT_VISIBILITY
    assert isinstance(sym.interface, LocalInterface)
    # Check that the default visibility is public
    assert Symbol.DEFAULT_VISIBILITY == Symbol.Visibility.PUBLIC

    sym = Symbol("sym2", Symbol.Visibility.PRIVATE)
    assert sym.visibility == Symbol.Visibility.PRIVATE

    sym = Symbol("sym3", interface=UnresolvedInterface())
    assert isinstance(sym.interface, UnresolvedInterface)

    with pytest.raises(TypeError) as error:
        sym = Symbol(None)
    assert ("Symbol 'name' attribute should be of type 'str'"
            in str(error.value))

    with pytest.raises(TypeError) as error:
        Symbol('sym1', visibility="hello")
    assert ("Symbol 'visibility' attribute should be of type "
            "psyir.symbols.Symbol.Visibility but" in str(error.value))

    with pytest.raises(TypeError) as error:
        Symbol('sym1', interface="hello")
    assert ("Symbol 'interface' attribute should be of type "
            "psyir.symbols.symbol.SymbolInterface but 'str' found."
            in str(error.value))


def test_symbol_interface_setter():
    '''Test that the Symbol interface setter behaves as expected. Also use
    this to test the is_local, is_global and is_argument and
    unresolved_interface properties

    '''
    symbol = Symbol('sym1')
    assert symbol.is_local
    assert not symbol.is_global
    assert not symbol.is_argument
    assert not symbol.unresolved_interface

    symbol.interface = GlobalInterface(ContainerSymbol("my_mod"))
    assert not symbol.is_local
    assert symbol.is_global
    assert not symbol.is_argument
    assert not symbol.unresolved_interface

    symbol.interface = ArgumentInterface()
    assert not symbol.is_local
    assert not symbol.is_global
    assert symbol.is_argument
    assert not symbol.unresolved_interface

    symbol.interface = UnresolvedInterface()
    assert not symbol.is_local
    assert not symbol.is_global
    assert not symbol.is_argument
    assert symbol.unresolved_interface


def test_symbol_str():
    '''Test that a Symbol instance can be stringified'''

    sym = Symbol("my_symbol")
    assert str(sym) == "my_symbol"


# SymbolError is declared but not used in symbol.py. There are many
# other files that use SymbolError and have associated tests so it is
# not tested here.


def test_symbolinterface():
    '''Test we can create a SymbolInterface instance. This does nothing so
    needs no further testing.

    '''
    _ = SymbolInterface()


def test_localinterface():
    '''Test we can create a LocalInterface instance and check its __str__
    value

    '''
    interface = LocalInterface()
    assert str(interface) == "Local"


def test_unresolvedinterface():
    '''Test we can create an UnresolvedInterface instance and check its
    __str__ value

    '''
    interface = UnresolvedInterface()
    assert str(interface) == "Unresolved"


def test_globalinterface():
    '''Test that we can create a global interface successfully, that is
    raises the expected exception if the container_symbol attribute is
    of the wrong type, that the container symbol property and str
    method work as expected.

    '''
    container_symbol = ContainerSymbol("my_mod")
    global_interface = GlobalInterface(container_symbol)
    assert global_interface.container_symbol is container_symbol
    assert str(global_interface) == "Global(container='my_mod')"

    with pytest.raises(TypeError) as info:
        _ = GlobalInterface("hello")
    assert ("Global container_symbol parameter must be of type "
            "ContainerSymbol, but found 'str'." in str(info.value))


def test_argumentinterface_init():
    '''Check that the ArgumentInterface can be created successfully and
    has the expected values. Also checks the access property and that
    an exception is raised if the supplied access value is the wrong
    type.

    '''
    argument_interface = ArgumentInterface()
    assert argument_interface._access == ArgumentInterface.Access.UNKNOWN
    assert argument_interface.access == argument_interface._access
    assert argument_interface._pass_by_value is False

    argument_interface = ArgumentInterface(ArgumentInterface.Access.READ)
    assert argument_interface._access == ArgumentInterface.Access.READ
    assert argument_interface.access == argument_interface._access

    with pytest.raises(TypeError) as info:
        _ = ArgumentInterface("hello")
    assert ("SymbolInterface.access must be an 'ArgumentInterface.Access' but "
            "got 'str'." in str(info.value))
    with pytest.raises(TypeError) as info:
        argument_interface.access = "hello"
    assert ("SymbolInterface.access must be an 'ArgumentInterface.Access' but "
            "got 'str'." in str(info.value))


@pytest.mark.parametrize("access", [ArgumentInterface.Access.READ,
                                    ArgumentInterface.Access.WRITE,
                                    ArgumentInterface.Access.READWRITE,
                                    ArgumentInterface.Access.UNKNOWN])
def test_argumentinterface_access_values(access):
    '''Check that all the ArgumentInterface access values are supported.

    '''
    argument_interface = ArgumentInterface()
    argument_interface.access = access
    assert argument_interface.access == access


def test_argumentinterface_str():
    '''Test that an ArgumentInterface instance can be stringified'''

    argument_interface = ArgumentInterface()
    assert str(argument_interface) == "Argument(pass-by-value=False)"
