# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2021, Science and Technology Facilities Council.
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
# Author: A. R. Porter, STFC Daresbury Lab
# -----------------------------------------------------------------------------

''' Performs py.test tests for the _find_or_create_imported_symbol routine
of the fparser2 frontend. '''


import os
import pytest
from fparser.common.readfortran import FortranFileReader
from psyclone.psyGen import PSyFactory, Kern
from psyclone.psyir.frontend.fparser2 import _find_or_create_imported_symbol
from psyclone.psyir.nodes import Reference, Container, Assignment, Literal, \
    KernelSchedule, BinaryOperation
from psyclone.psyir.symbols import Symbol, DataSymbol, SymbolError, \
    SymbolTable, REAL_TYPE, ContainerSymbol, ScalarType, UnresolvedInterface, \
    RoutineSymbol, NoType
from psyclone.tests.utilities import get_invoke


def test_find_or_create_imported_symbol():
    '''Test that the find_or_create_imported_symbol method in a Node instance
    returns the associated symbol if there is one and raises an exception if
    not. Also test for an incorrect scope argument.'''

    _, invoke = get_invoke("single_invoke_kern_with_global.f90",
                           api="gocean1.0", idx=0)
    sched = invoke.schedule
    kernels = sched.walk(Kern)
    kernel_schedule = kernels[0].get_kernel_schedule()
    references = kernel_schedule.walk(Reference)

    # Symbol in KernelSchedule SymbolTable
    field_old = references[0]
    assert field_old.name == "field_old"
    assert isinstance(field_old.symbol, DataSymbol)
    assert field_old.symbol in kernel_schedule.symbol_table.symbols

    # Symbol in KernelSchedule SymbolTable with KernelSchedule scope
    assert isinstance(_find_or_create_imported_symbol(
        field_old, field_old.name, scope_limit=kernel_schedule), DataSymbol)
    assert field_old.symbol.name == field_old.name

    # Symbol in KernelSchedule SymbolTable with parent scope, so
    # the symbol should not be found as we limit the scope to the
    # immediate parent of the reference
    with pytest.raises(SymbolError) as excinfo:
        _ = _find_or_create_imported_symbol(field_old, field_old.name,
                                            scope_limit=field_old.parent)
    assert "No Symbol found for name 'field_old'." in str(excinfo.value)

    # Symbol in Container SymbolTable
    alpha = references[6]
    assert alpha.name == "alpha"
    assert isinstance(_find_or_create_imported_symbol(alpha, alpha.name),
                      DataSymbol)
    container = kernel_schedule.ancestor(Container)
    assert isinstance(container, Container)
    assert (_find_or_create_imported_symbol(alpha, alpha.name) in
            container.symbol_table.symbols)

    # Symbol in Container SymbolTable with KernelSchedule scope, so
    # the symbol should not be found as we limit the scope to the
    # kernel so do not search the container symbol table.
    with pytest.raises(SymbolError) as excinfo:
        _ = _find_or_create_imported_symbol(alpha, alpha.name,
                                            scope_limit=kernel_schedule)
    assert "No Symbol found for name 'alpha'." in str(excinfo.value)

    # Symbol in Container SymbolTable with Container scope
    assert (_find_or_create_imported_symbol(
        alpha, alpha.name, scope_limit=container).name == alpha.name)

    # Test _find_or_create_impored_symbol with invalid location
    with pytest.raises(TypeError) as excinfo:
        _ = _find_or_create_imported_symbol("hello", alpha.name)
    assert ("The location argument 'hello' provided to "
            "_find_or_create_imported_symbol() is not of type `Node`."
            in str(excinfo.value))

    # Test _find_or_create_impored_symbol with invalid scope type
    with pytest.raises(TypeError) as excinfo:
        _ = _find_or_create_imported_symbol(alpha, alpha.name,
                                            scope_limit="hello")
    assert ("The scope_limit argument 'hello' provided to "
            "_find_or_create_imported_symbol() is not of type `Node`."
            in str(excinfo.value))

    # find_or_create_symbol method with invalid scope location
    with pytest.raises(ValueError) as excinfo:
        _ = _find_or_create_imported_symbol(alpha, alpha.name,
                                            scope_limit=alpha)
    assert ("The scope_limit node 'Reference[name:'alpha']' provided to "
            "_find_or_create_imported_symbol() is not an ancestor of this "
            "node 'Reference[name:'alpha']'." in str(excinfo.value))

    # With a visibility parameter
    sym = _find_or_create_imported_symbol(alpha, "very_private",
                                          visibility=Symbol.Visibility.PRIVATE)
    assert sym.name == "very_private"
    assert sym.visibility == Symbol.Visibility.PRIVATE
    assert sym is container.symbol_table.lookup("very_private",
                                                scope_limit=container)


def test_find_or_create_imported_symbol_2():
    ''' Check that the _find_or_create_imported_symbol() method creates new
    symbols when appropriate. '''
    # Create some suitable PSyIR from scratch
    symbol_table = SymbolTable()
    symbol_table.add(DataSymbol("tmp", REAL_TYPE))
    kernel1 = KernelSchedule.create("mod_1", SymbolTable(), [])
    container = Container.create("container_name", symbol_table,
                                 [kernel1])
    xvar = DataSymbol("x", REAL_TYPE)
    xref = Reference(xvar)
    assign = Assignment.create(xref, Literal("1.0", REAL_TYPE))
    kernel1.addchild(assign)
    # We have no wildcard imports so there can be no symbol named 'undefined'
    with pytest.raises(SymbolError) as err:
        _ = _find_or_create_imported_symbol(assign, "undefined")
    assert "No Symbol found for name 'undefined'" in str(err.value)
    # We should be able to find the 'tmp' symbol in the parent Container
    sym = _find_or_create_imported_symbol(assign, "tmp")
    assert sym.datatype.intrinsic == ScalarType.Intrinsic.REAL
    # Add a wildcard import to the SymbolTable of the KernelSchedule
    new_container = ContainerSymbol("some_mod")
    new_container.wildcard_import = True
    kernel1.symbol_table.add(new_container)
    # Symbol not in any container but we do have wildcard imports so we
    # get a new symbol back
    new_symbol = _find_or_create_imported_symbol(assign, "undefined")
    assert new_symbol.name == "undefined"
    assert isinstance(new_symbol.interface, UnresolvedInterface)
    # pylint: disable=unidiomatic-typecheck
    assert type(new_symbol) == Symbol
    assert "undefined" not in container.symbol_table
    assert kernel1.symbol_table.lookup("undefined") is new_symbol


def test_nemo_find_container_symbol(parser):
    ''' Check that find_or_create_symbol() works for the NEMO API when the
    searched-for symbol is declared in the parent module. '''
    reader = FortranFileReader(
        os.path.join(
            os.path.dirname(os.path.dirname(os.path.dirname(
                os.path.abspath(__file__)))),
            "test_files", "gocean1p0", "kernel_with_global_mod.f90"))
    prog = parser(reader)
    psy = PSyFactory("nemo", distributed_memory=False).create(prog)
    # Get a node from the schedule
    bops = psy._invokes.invoke_list[0].schedule.walk(BinaryOperation)
    # Use it as the starting point for the search
    symbol = _find_or_create_imported_symbol(bops[0], "alpha")
    assert symbol.datatype.intrinsic == ScalarType.Intrinsic.REAL


def test_find_or_create_change_symbol_type():
    ''' Check that the _find_or_create_imported_symbol routine correctly
    updates the class of the located symbol if it is not an instance of
    the requested symbol type.
    '''
    # pylint: disable=unidiomatic-typecheck
    # Create some suitable PSyIR from scratch
    symbol_table = SymbolTable()
    tmp_sym = symbol_table.new_symbol("tmp")
    sub_sym = symbol_table.new_symbol("my_sub")
    kernel1 = KernelSchedule.create("mod_1", SymbolTable(), [])
    _ = Container.create("container_name", symbol_table, [kernel1])
    assign = Assignment.create(Reference(tmp_sym), Literal("1.0", REAL_TYPE))
    kernel1.addchild(assign)
    # Search for the 'tmp' symbol
    sym = _find_or_create_imported_symbol(assign, "tmp")
    assert sym is tmp_sym
    assert type(sym) == Symbol
    # Repeat but this time specify that we're expecting a DataSymbol
    sym = _find_or_create_imported_symbol(assign, "tmp",
                                          symbol_type=DataSymbol,
                                          datatype=REAL_TYPE)
    assert sym is tmp_sym
    assert type(sym) == DataSymbol
    assert sym.datatype == REAL_TYPE
    # Search for 'my_sub' and specify that it should be a RoutineSymbol
    sym2 = _find_or_create_imported_symbol(assign, "my_sub",
                                           symbol_type=RoutineSymbol)
    assert sym2 is sub_sym
    assert type(sym2) == RoutineSymbol
    assert isinstance(sym2.datatype, NoType)
