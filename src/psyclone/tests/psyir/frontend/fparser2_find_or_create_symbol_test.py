# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2021-2024, Science and Technology Facilities Council.
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
# Modified: R. W. Ford, STFC Daresbury Lab
# -----------------------------------------------------------------------------

''' Performs py.test tests for the _find_or_create_unresolved_symbol routine
of the fparser2 frontend. '''


import os
import pytest
from fparser.common.readfortran import FortranFileReader
from psyclone.psyGen import PSyFactory, Kern
from psyclone.psyir.frontend.fparser2 import _find_or_create_unresolved_symbol
from psyclone.psyir.nodes import (
    Reference, Container, Assignment, Literal, KernelSchedule,
    BinaryOperation)
from psyclone.psyir.symbols import (
    Symbol, DataSymbol, SymbolTable, REAL_TYPE, ScalarType,
    UnresolvedInterface, RoutineSymbol, NoType)
from psyclone.tests.utilities import get_invoke


def test_find_or_create_unresolved_symbol():
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
    assert isinstance(_find_or_create_unresolved_symbol(
        field_old, field_old.name, scope_limit=kernel_schedule), DataSymbol)
    assert field_old.symbol.name == field_old.name

    # Symbol in Container SymbolTable
    alpha = references[6]
    assert alpha.name == "alpha"
    assert isinstance(_find_or_create_unresolved_symbol(alpha, alpha.name),
                      DataSymbol)
    container = kernel_schedule.ancestor(Container)
    assert isinstance(container, Container)
    assert (_find_or_create_unresolved_symbol(alpha, alpha.name) in
            container.symbol_table.symbols)

    # Symbol in Container SymbolTable with Container scope
    assert (_find_or_create_unresolved_symbol(
        alpha, alpha.name, scope_limit=container).name == alpha.name)

    # Test _find_or_create_unresolved_symbol with invalid location
    with pytest.raises(TypeError) as excinfo:
        _ = _find_or_create_unresolved_symbol("hello", alpha.name)
    assert ("The location argument 'hello' provided to "
            "_find_or_create_unresolved_symbol() is not of type `Node`."
            in str(excinfo.value))

    # Test _find_or_create_unresolved_symbol with invalid scope type
    with pytest.raises(TypeError) as excinfo:
        _ = _find_or_create_unresolved_symbol(alpha, alpha.name,
                                              scope_limit="hello")
    assert ("The scope_limit argument 'hello' provided to "
            "_find_or_create_unresolved_symbol() is not of type `Node`."
            in str(excinfo.value))

    # find_or_create_symbol method with invalid scope location
    with pytest.raises(ValueError) as excinfo:
        _ = _find_or_create_unresolved_symbol(alpha, alpha.name,
                                              scope_limit=alpha)
    assert ("The scope_limit node 'Reference[name:'alpha']' provided to "
            "_find_or_create_unresolved_symbol() is not an ancestor of this "
            "node 'Reference[name:'alpha']'." in str(excinfo.value))

    # With a visibility parameter
    sym = _find_or_create_unresolved_symbol(
        alpha, "very_private", visibility=Symbol.Visibility.PRIVATE)
    assert sym.name == "very_private"
    assert sym.visibility == Symbol.Visibility.PRIVATE
    assert sym is container.children[0].symbol_table.lookup(
        "very_private", scope_limit=container)


def test_find_or_create_unresolved_symbol_2():
    ''' Check that the _find_or_create_unresolved_symbol() method creates new
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
    # OK to add
    orig_symbol = _find_or_create_unresolved_symbol(assign, "undefined")
    # We should be able to find the 'tmp' symbol in the parent Container
    sym = _find_or_create_unresolved_symbol(assign, "tmp")
    assert sym.datatype.intrinsic == ScalarType.Intrinsic.REAL
    # OK to find
    new_symbol = _find_or_create_unresolved_symbol(assign, "undefined")
    assert orig_symbol is new_symbol
    assert new_symbol.name == "undefined"
    assert isinstance(new_symbol.interface, UnresolvedInterface)
    # pylint: disable=unidiomatic-typecheck
    assert type(new_symbol) is Symbol
    assert "undefined" not in container.symbol_table
    assert kernel1.symbol_table.lookup("undefined") is new_symbol


def test_find_or_create_unresolved_scope_limit():
    '''Test that the implemented works as expected when scope_limit is
    provided.

    '''
    kernel = KernelSchedule.create("mod_1", SymbolTable(), [])
    container = Container.create("container_name", SymbolTable(),
                                 [kernel])
    xvar = DataSymbol("x", REAL_TYPE)
    xref = Reference(xvar)
    assign = Assignment.create(xref, Literal("1.0", REAL_TYPE))
    kernel.addchild(assign)

    # Add x to the kernel symbol table as no symbol exists
    xsymbol = _find_or_create_unresolved_symbol(assign, "x")
    assert "x" in kernel.symbol_table
    xsymbol = kernel.symbol_table.lookup("x")
    # pylint: disable=unidiomatic-typecheck
    assert type(xsymbol) is Symbol

    # Move x to the container symbol table
    kernel.symbol_table.remove(xsymbol)
    container.symbol_table.add(xsymbol)
    assert "x" in container.symbol_table
    assert "x" not in kernel.symbol_table

    # Add "x" ("x_1") to the kernel symbol table as we limit the scope to
    # kernel. However, to avoid conficts with the pre-existing
    # variables "x_1" in created.
    xsymbol = _find_or_create_unresolved_symbol(
        assign, "x", scope_limit=kernel)
    assert "x" in container.symbol_table
    assert "x_1" in kernel.symbol_table
    assert (container.symbol_table.lookup("x") !=
            kernel.symbol_table.lookup("x_1"))
    assert kernel.symbol_table.lookup("x_1") == xsymbol


def test_nemo_find_container_symbol(parser):
    '''Check that find_or_create_symbol() works for the NEMO API when the
    searched-for symbol is declared in the parent module.

    '''
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
    symbol = _find_or_create_unresolved_symbol(bops[0], "alpha")
    assert symbol.datatype.intrinsic == ScalarType.Intrinsic.REAL


def test_find_or_create_change_symbol_type():
    ''' Check that the _find_or_create_unresolved_symbol routine correctly
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
    sym = _find_or_create_unresolved_symbol(assign, "tmp")
    assert sym is tmp_sym
    assert type(sym) is Symbol
    # Repeat but this time specify that we're expecting a DataSymbol
    sym = _find_or_create_unresolved_symbol(assign, "tmp",
                                            symbol_type=DataSymbol,
                                            datatype=REAL_TYPE)
    assert sym is tmp_sym
    assert type(sym) is DataSymbol
    assert sym.datatype == REAL_TYPE
    # Search for 'my_sub' and specify that it should be a RoutineSymbol
    sym2 = _find_or_create_unresolved_symbol(assign, "my_sub",
                                             symbol_type=RoutineSymbol)
    assert sym2 is sub_sym
    assert type(sym2) is RoutineSymbol
    assert isinstance(sym2.datatype, NoType)


@pytest.mark.parametrize("visibility", [
    Symbol.Visibility.PUBLIC, Symbol.Visibility.PRIVATE])
def test_visibility_interface(fortran_reader, visibility):
    '''Test that PSyclone successfully parses public/private symbols where
    their declaration is hidden in an abstract interface. This support
    is implemented in _find_or_create_unresolved_symbol.

    '''
    code = (
        f"module test\n"
        f"  abstract interface\n"
        f"     subroutine update_interface()\n"
        f"     end subroutine update_interface\n"
        f"  end interface\n"
        f"  {visibility.name} :: update_interface\n"
        f"contains\n"
        f"  subroutine alg()\n"
        f"  end subroutine alg\n"
        f"end module test\n")
    psyir = fortran_reader.psyir_from_source(code)
    symbol_table = psyir.children[0].symbol_table
    assert symbol_table.lookup("_psyclone_internal_interface")
    symbol = symbol_table.lookup("update_interface")
    assert symbol.visibility is visibility
