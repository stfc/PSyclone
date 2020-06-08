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
# Authors S. Siso, STFC Daresbury Lab
# -----------------------------------------------------------------------------

''' Perform py.test tests on the psygen.psyir.symbols.symbol module '''

from __future__ import absolute_import
import pytest
from psyclone.psyir.symbols import Symbol


def test_symbol_initialisation():
    '''Test that a Symbol instance can be created when valid
    arguments are given, otherwise raise relevant exceptions.'''

    sym = Symbol("sym1")
    assert isinstance(sym, Symbol)
    assert sym.name == "sym1"
    assert sym.visibility == Symbol.DEFAULT_VISIBILITY
    # Check that the default visibility is public
    assert Symbol.DEFAULT_VISIBILITY == Symbol.Visibility.PUBLIC

    sym = Symbol("sym2", Symbol.Visibility.PRIVATE)
    assert sym.visibility == Symbol.Visibility.PRIVATE

    with pytest.raises(TypeError) as error:
        sym = Symbol(None)
    assert ("Symbol 'name' attribute should be of type 'str'"
            in str(error.value))

    with pytest.raises(TypeError) as error:
        Symbol('sym1', visibility="hello")
    assert ("Symbol 'visibility' attribute should be of type "
            "psyir.symbols.Symbol.Visibility but" in str(error.value))


def test_symbol_str():
    '''Test that a Symbol instance can be stringified'''

    sym = Symbol("my_symbol")
    assert str(sym) == "my_symbol"


#def test_datasymbol_invalid_interface():
#    ''' Check that the DataSymbol.interface setter rejects the supplied value
#    if it is not a DataSymbolInterface. '''
#    sym = DataSymbol("some_var", REAL_SINGLE_TYPE)
#    with pytest.raises(TypeError) as err:
#        sym.interface = "invalid interface spec"
#    assert "interface to a Symbol must be a SymbolInterface but" \
#        in str(err.value)


#def test_datasymbol_interface():
#    ''' Check the interface getter on a DataSymbol. '''
#    my_mod = ContainerSymbol("my_mod")
#    symbol = DataSymbol("some_var", REAL_SINGLE_TYPE,
#                        interface=GlobalInterface(my_mod))
#    assert symbol.interface.container_symbol.name == "my_mod"


#def test_datasymbol_interface_setter():
#    ''' Check the interface setter on a DataSymbol. '''
#    my_mod = ContainerSymbol("my_mod")
#    symbol = DataSymbol("some_var", REAL_SINGLE_TYPE,
#                        interface=GlobalInterface(my_mod))
#    assert symbol.interface.container_symbol.name == "my_mod"
#
#    with pytest.raises(TypeError) as err:
#        symbol.interface = "not valid"
#    assert ("interface to a Symbol must be a SymbolInterface but "
#            "got 'str'" in str(err.value))


#def test_datasymbol_interface_access():
#    ''' Tests for the DataSymbolInterface.access setter. '''
#    symbol = DataSymbol("some_var", REAL_SINGLE_TYPE,
#                        interface=ArgumentInterface())
#    symbol.interface.access = ArgumentInterface.Access.READ
#    assert symbol.interface.access == ArgumentInterface.Access.READ
#    # Force the error by supplying a string instead of a SymbolAccess type.
#    with pytest.raises(TypeError) as err:
#        symbol.interface.access = "read"
#    assert "must be a 'ArgumentInterface.Access' but got " in str(err.value)


#def test_datasymbol_argument_str():
#    ''' Check the __str__ method of the ArgumentInterface class. '''
#    # An ArgumentInterface represents a routine argument by default.
#    interface = ArgumentInterface()
#    assert str(interface) == "Argument(pass-by-value=False)"


#def test_fortranglobal_str():
#    ''' Test the __str__ method of GlobalInterface. '''
#    # If it's not an argument then we have nothing else to say about it (since
#    # other options are language specific and are implemented in sub-classes).
#    my_mod = ContainerSymbol("my_mod")
#    interface = GlobalInterface(my_mod)
#    assert str(interface) == "Global(container='my_mod')"


#def test_global_modname():
#    ''' Test the GlobalInterface.module_name setter error conditions. '''
#    with pytest.raises(TypeError) as err:
#        _ = GlobalInterface(None)
#    assert ("Global container_symbol parameter must be of type"
#            " ContainerSymbol, but found ") in str(err.value)


#def test_datasymbol_unresolved_interface():
#    '''Test that unresolved_interface returns the expected value.'''
#    data_symbol = DataSymbol("a", REAL4_TYPE)
#    assert not data_symbol.unresolved_interface
#    data_symbol = DataSymbol("a", REAL4_TYPE, interface=UnresolvedInterface())
#    assert data_symbol.unresolved_interface
