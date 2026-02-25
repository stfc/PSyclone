# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2020-2026, Science and Technology Facilities Council.
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
#             A. R. Porter, STFC Daresbury Lab
# Modified by J. Henrichs, Bureau of Meteorology
# -----------------------------------------------------------------------------

'''Perform py.test tests on the psyclone.psyir.symbols.symbol module.

Note that SymbolError is declared but not used in symbol.py. There are
many other files that use SymbolError and have associated tests so it
is not tested here.

'''


import pytest

from psyclone.core import Signature, AccessSequence, AccessType
from psyclone.errors import InternalError
from psyclone.psyir.nodes import Container, Literal, KernelSchedule, Reference
from psyclone.psyir.symbols import (
    ArgumentInterface, ContainerSymbol,
    DataSymbol, ImportInterface, DefaultModuleInterface, StaticInterface,
    INTEGER_SINGLE_TYPE, AutomaticInterface, CommonBlockInterface,
    NoType, RoutineSymbol, Symbol, SymbolError, UnknownInterface,
    SymbolTable, UnresolvedInterface)


def test_symbol_initialisation():
    '''Test that a Symbol instance can be created when valid arguments are
    given, otherwise raise relevant exceptions. Also tests the
    internal Visibility class, the name, visibility and interface properties.

    '''
    sym = Symbol("sym1")
    assert isinstance(sym, Symbol)
    assert sym.name == "sym1"
    assert sym.visibility == Symbol.DEFAULT_VISIBILITY
    assert isinstance(sym.interface, AutomaticInterface)
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
    assert ("The interface to a Symbol must be a SymbolInterface but got "
            "'str'" in str(error.value))


def test_symbol_interface_setter_and_is_properties():
    '''Test that the Symbol interface setter behaves as expected,
    including raising an exception if the input is of the wrong
    type. Also use this to test the is_automatic, is_import and
    is_argument, is_unresolved, is_modulevar, is_static,
    is_commonblock, is_unknown_interface properties.

    '''
    symbol = Symbol('sym1')
    assert symbol.is_automatic
    assert not symbol.is_import
    assert not symbol.is_argument
    assert not symbol.is_unresolved
    assert not symbol.is_modulevar
    assert not symbol.is_static
    assert not symbol.is_commonblock
    assert not symbol.is_unknown_interface

    symbol.interface = ImportInterface(ContainerSymbol("my_mod"))
    assert not symbol.is_automatic
    assert symbol.is_import
    assert not symbol.is_argument
    assert not symbol.is_unresolved
    assert not symbol.is_modulevar
    assert not symbol.is_static
    assert not symbol.is_commonblock
    assert not symbol.is_unknown_interface

    symbol.interface = ArgumentInterface()
    assert not symbol.is_automatic
    assert not symbol.is_import
    assert symbol.is_argument
    assert not symbol.is_unresolved
    assert not symbol.is_modulevar
    assert not symbol.is_static
    assert not symbol.is_commonblock
    assert not symbol.is_unknown_interface

    symbol.interface = UnresolvedInterface()
    assert not symbol.is_automatic
    assert not symbol.is_import
    assert not symbol.is_argument
    assert symbol.is_unresolved
    assert not symbol.is_modulevar
    assert not symbol.is_static
    assert not symbol.is_commonblock
    assert not symbol.is_unknown_interface

    symbol.interface = DefaultModuleInterface()
    assert not symbol.is_automatic
    assert not symbol.is_import
    assert not symbol.is_argument
    assert not symbol.is_unresolved
    assert symbol.is_modulevar
    assert not symbol.is_static
    assert not symbol.is_commonblock
    assert not symbol.is_unknown_interface

    symbol.interface = StaticInterface()
    assert not symbol.is_automatic
    assert not symbol.is_import
    assert not symbol.is_argument
    assert not symbol.is_unresolved
    assert not symbol.is_modulevar
    assert symbol.is_static
    assert not symbol.is_commonblock
    assert not symbol.is_unknown_interface

    symbol.interface = CommonBlockInterface()
    assert not symbol.is_automatic
    assert not symbol.is_import
    assert not symbol.is_argument
    assert not symbol.is_unresolved
    assert not symbol.is_modulevar
    assert not symbol.is_static
    assert symbol.is_commonblock
    assert not symbol.is_unknown_interface

    symbol.interface = UnknownInterface()
    assert not symbol.is_automatic
    assert not symbol.is_import
    assert not symbol.is_argument
    assert not symbol.is_unresolved
    assert not symbol.is_modulevar
    assert not symbol.is_static
    assert not symbol.is_commonblock
    assert symbol.is_unknown_interface

    with pytest.raises(TypeError) as info:
        symbol.interface = "hello"
    assert ("The interface to a Symbol must be a SymbolInterface but got "
            "'str'" in str(info.value))


def test_symbol_str():
    '''Test that a Symbol instance can be stringified'''

    sym = Symbol("my_symbol")
    assert str(sym) == "my_symbol: Symbol<Automatic>"


def test_find_symbol_table():
    ''' Test the find_symbol_table() method. '''
    sym = Symbol("a_var")
    with pytest.raises(TypeError) as err:
        sym.find_symbol_table("3")
    assert ("expected to be passed an instance of psyir.nodes.Node but got "
            "'str'" in str(err.value))
    # Search for a SymbolTable with only one level of hierarchy
    sched = KernelSchedule.create("dummy")
    table = sched.symbol_table
    table.add(sym)
    assert sym.find_symbol_table(sched) is table
    # Create a Container so that we have two levels of hierarchy
    ctable = SymbolTable()
    sym2 = Symbol("b_var")
    ctable.add(sym2)
    _ = Container.create("test", ctable, [sched])
    assert sym2.find_symbol_table(sched) is ctable
    # A Symbol that isn't in any table
    sym3 = Symbol("missing")
    assert sym3.find_symbol_table(sched) is None
    # When there is no SymbolTable associated with the PSyIR node
    orphan = Literal("1", INTEGER_SINGLE_TYPE)
    assert sym3.find_symbol_table(orphan) is None


def test_symbol_copy():
    '''
    Test the Symbol.copy() method.
    '''
    csym = ContainerSymbol("some_mod")
    asym = Symbol("a", visibility=Symbol.Visibility.PRIVATE,
                  interface=ImportInterface(csym))
    new_sym = asym.copy()
    assert new_sym is not asym
    assert new_sym.name == asym.name
    assert isinstance(new_sym.interface, ImportInterface)
    assert new_sym.interface.container_symbol is csym
    assert new_sym.visibility == asym.visibility
    # Check that we can modify the interface of the new symbol without
    # affecting the original.
    new_sym.interface.container_symbol = ContainerSymbol("other_mod")
    assert asym.interface.container_symbol is csym


def test_symbol_copy_properties():
    ''' Test the copy_properties() method. '''
    csym = ContainerSymbol("some_mod")
    sym = Symbol("a", visibility=Symbol.Visibility.PRIVATE,
                 interface=ImportInterface(csym))
    new_sym = Symbol("b")
    # First, exclude the interface from the update.
    new_sym.copy_properties(sym, exclude_interface=True)
    assert isinstance(new_sym.interface, AutomaticInterface)
    # Name and visibility should also be unchanged
    assert new_sym.name == "b"
    assert new_sym.visibility == Symbol.Visibility.PUBLIC
    # Repeat but include the interface in the update.
    new_sym.copy_properties(sym)
    # Name and visibility should be unchanged
    assert new_sym.name == "b"
    assert new_sym.visibility == Symbol.Visibility.PUBLIC
    # Interface should have been updated
    assert new_sym.interface == sym.interface

    with pytest.raises(TypeError) as err:
        new_sym.copy_properties("hello")
    assert ("Argument should be of type 'Symbol' but found 'str'" in
            str(err.value))


def test_symbol_specialise():
    '''Test the Symbol.specialise() method.'''
    # pylint: disable = unidiomatic-typecheck
    asym = Symbol("a")
    assert type(asym) is Symbol
    assert str(asym) == "a: Symbol<Automatic>"
    asym.specialise(RoutineSymbol)
    assert type(asym) is RoutineSymbol
    assert (str(asym) == "a: RoutineSymbol<NoType, pure=unknown, "
            "elemental=unknown>")


@pytest.mark.parametrize("test_class", [Symbol, RoutineSymbol])
@pytest.mark.parametrize("arg", [str, Symbol])
def test_symbol_specialise_class_error(test_class, arg):
    '''Test the Symbol.specialise() method raises the expected
    exception if the supplied argument is a class that is not a
    subclass of the instance that calls specialise().

    '''
    asym = test_class("a")
    with pytest.raises(TypeError) as info:
        asym.specialise(arg)
    assert (f"The specialise method in 'a', an instance of "
            f"'{test_class.__name__}', expects the subclass argument to be a "
            f"subclass of '{test_class.__name__}', but found "
            f"'{arg.__name__}'." in str(info.value))


@pytest.mark.parametrize("test_class", [Symbol, RoutineSymbol])
def test_symbol_specialise_instance_error(test_class):
    '''Test the Symbol.specialise() method raises the expected exception
    if the supplied argument is not a class. Check using Symbol and a
    subclass of Symbol.

    '''
    asym = test_class("a")
    with pytest.raises(TypeError) as info:
        asym.specialise(None)
    assert ("The specialise method in 'a' expects the subclass "
            "argument to be a class."
            in str(info.value))


def test_get_external_symbol(monkeypatch):
    ''' Test the get_external_symbol() method. '''
    asym = Symbol("a")
    with pytest.raises(NotImplementedError) as err:
        asym.get_external_symbol()
    assert ("trying to resolve symbol 'a' properties, the lazy evaluation "
            "of 'Automatic' interfaces is not supported" in str(err.value))
    other_container = ContainerSymbol("some_mod")
    ctable = SymbolTable()
    ctable.add(other_container)
    # Create a Symbol that is imported from the "some_mod" Container
    bsym = Symbol("b", interface=ImportInterface(other_container))
    ctable.add(bsym)
    _ = Container.create("test", ctable, [KernelSchedule.create("dummy")])
    # Monkeypatch the container's FortranModuleInterface so that it always
    # appears to be unable to find the "some_mod" module

    def fake_import(name):
        raise SymbolError("Oh dear")
    monkeypatch.setattr(other_container._interface, "get_container",
                        fake_import)
    with pytest.raises(SymbolError) as err:
        bsym.get_external_symbol()
    assert ("trying to resolve the properties of symbol 'b' in module "
            "'some_mod': PSyclone SymbolTable error: Oh dear" in
            str(err.value))
    # Monkeypatch to pretend that we fail to get a Container object.
    monkeypatch.setattr(other_container, "find_container_psyir", lambda: None)
    with pytest.raises(SymbolError) as err:
        bsym.get_external_symbol()
    assert ("trying to resolve the properties of symbol 'b' in module "
            "'some_mod': PSyclone SymbolTable error: Error trying to resolve "
            "the properties of symbol 'b'. The interface points to module "
            "'some_mod' but could not obtain its PSyIR." in str(err.value))


def test_get_external_symbol_missing(monkeypatch):
    '''
    Test that get_external_symbol() raises the expected error when the
    requested symbol cannot be found in the Container from which it is
    imported.

    '''
    # Create a Container for the 'some_mod' module and attach this to
    # the ContainerSymbol
    ctable2 = SymbolTable()
    some_mod = Container.create("some_mod", ctable2,
                                [KernelSchedule.create("dummy2")])
    other_container = ContainerSymbol("some_mod")
    other_container._reference = some_mod
    # Create a Symbol that is imported from the "some_mod" Container
    bsym = Symbol("b", interface=ImportInterface(other_container))
    # Currently the Container does not contain an entry for 'b'
    with pytest.raises(SymbolError) as err:
        bsym.get_external_symbol()
    assert ("trying to resolve the properties of symbol 'b'. The interface "
            "points to module 'some_mod' but could not find the definition of "
            "'b' in that module." in str(err.value))
    # Add an entry for 'b' to the Container's symbol table
    ctable2.add(DataSymbol("b", INTEGER_SINGLE_TYPE))
    new_sym = bsym.resolve_type()
    assert isinstance(new_sym, DataSymbol)
    assert new_sym.datatype == INTEGER_SINGLE_TYPE


def test_symbol_resolve_type(monkeypatch):
    ''' Test the resolve_type method. '''
    # resolve_type() for a local symbol has nothing to do so should
    # just return itself.
    asym = Symbol("a")
    assert asym.resolve_type() is asym
    # Now test for a symbol that is imported from another Container
    other_container = ContainerSymbol("some_mod")
    bsym = Symbol("b", visibility=Symbol.Visibility.PRIVATE,
                  interface=ImportInterface(other_container))
    # Monkeypatch the get_external_symbol() method so that it just returns
    # a new DataSymbol
    monkeypatch.setattr(bsym, "get_external_symbol",
                        lambda: DataSymbol("b", INTEGER_SINGLE_TYPE))
    new_sym = bsym.resolve_type()
    # The symbol should be the same instance as before but with properties and
    # type obtained from the other table.
    assert new_sym is bsym
    assert isinstance(new_sym, DataSymbol)
    assert new_sym.datatype == INTEGER_SINGLE_TYPE
    assert new_sym.visibility == Symbol.Visibility.PRIVATE
    assert new_sym.is_import
    # Repeat the test but get_external_symbol() just returns
    # a new bare Symbol (e.g. because it is a transitive import)
    bsym = Symbol("b", visibility=Symbol.Visibility.PRIVATE,
                  interface=ImportInterface(other_container))
    monkeypatch.setattr(bsym, "get_external_symbol",
                        lambda: Symbol("b"))
    with pytest.raises(SymbolError) as err:
        new_sym = bsym.resolve_type()
    assert ("The external symbol 'b' was found but it does not have a "
            "type. Maybe it is a transitive (indirect) import which is "
            "currently not resolvable." in str(err.value))

    # Repeat but test when the imported Symbol is a parameter.
    csym = Symbol("c", visibility=Symbol.Visibility.PRIVATE,
                  interface=ImportInterface(other_container))
    # Monkeypatch the get_external_symbol() method so that it just returns
    # a new DataSymbol
    monkeypatch.setattr(
        csym, "get_external_symbol",
        lambda: DataSymbol("c", INTEGER_SINGLE_TYPE,
                           is_constant=True,
                           initial_value=Literal("1", INTEGER_SINGLE_TYPE)))
    new_sym = csym.resolve_type()
    assert new_sym is csym
    assert new_sym.datatype == INTEGER_SINGLE_TYPE
    assert new_sym.is_import
    assert new_sym.is_constant
    assert new_sym.initial_value == Literal("1", INTEGER_SINGLE_TYPE)
    # Repeat but test when the import turns out to be a RoutineSymbol.
    dsym = Symbol("d", visibility=Symbol.Visibility.PRIVATE,
                  interface=ImportInterface(other_container))
    monkeypatch.setattr(
        dsym, "get_external_symbol",
        lambda: RoutineSymbol("d", NoType()))
    new_sym = dsym.resolve_type()
    assert new_sym is dsym
    assert isinstance(dsym, RoutineSymbol)


def test_symbol_array_handling():
    '''Verifies the handling of arrays together with access information.

    '''
    # Make sure that a generic `Symbol` (no datatype) does not claim to
    # be an array
    asym = Symbol("a")
    assert asym.is_array is False

    # A generic symbol (no datatype) without an explicit array access
    # expression is not considered to have array access.
    assert not asym.is_array_access()

    # Specifying an index variable without any access information is an error.
    with pytest.raises(InternalError) as err:
        asym.is_array_access("i")
    assert ("index variable 'i' specified, but no access information given"
            in str(err.value))
    # Supply some access information.
    svinfo = AccessSequence(Signature("i"))
    svinfo.add_access(AccessType.READ, Reference(asym))
    assert not asym.is_array_access("i", svinfo)


@pytest.mark.parametrize("table", [None, SymbolTable()])
def test_symbol_replace_symbols_using(table):
    '''Test the replace_symbols_using() method in Symbol.'''
    interf = DefaultModuleInterface()
    asym = Symbol("a", interface=interf)
    if table is not None:
        # No symbols in table and nothing to update.
        asym.replace_symbols_using(table)
    assert asym.interface is interf
    cont = ContainerSymbol("genesis")
    binterf = ImportInterface(cont, orig_name="e")
    bsym = Symbol("b", interface=binterf)
    # No symbols in table.
    if table is not None:
        bsym.replace_symbols_using(table)
    assert bsym.interface is binterf
    assert bsym.interface.container_symbol is cont
    # Add a new ContainerSymbol to the table.
    cont2 = cont.copy()
    if table is not None:
        table.add(cont2)
        bsym.replace_symbols_using(table)
    else:
        bsym.replace_symbols_using(cont2)
    assert bsym.interface is not binterf
    assert bsym.interface.container_symbol is cont2


def test_symbol_get_all_accessed_symbols():
    '''Test that the get_all_accessed_symbols() method of a Symbol does not add
    any accesses.'''
    interf = DefaultModuleInterface()
    asym = Symbol("a", interface=interf)
    assert not asym.get_all_accessed_symbols()
