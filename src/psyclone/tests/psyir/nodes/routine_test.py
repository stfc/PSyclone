# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2020-2025, Science and Technology Facilities Council.
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
# Modified: S. Siso, STFC Daresbury Lab
# Modified: J. Henrichs, Bureau of Meteorology
# Modified: A. B. G. Chalk, STFC Daresbury Lab
# -----------------------------------------------------------------------------

''' This module contains the pytest tests for the Routine class. '''

import pytest

from psyclone.errors import GenerationError
from psyclone.psyGen import CodedKern
from psyclone.psyir.nodes import (Assignment, Call, CodeBlock, Container,
                                  Literal, Reference, Routine, ScopingNode)
from psyclone.psyir.symbols import (
    ContainerSymbol, DataSymbol, ImportInterface, REAL_TYPE,
    Symbol, SymbolError, SymbolTable, RoutineSymbol)
from psyclone.tests.utilities import check_links, get_invoke


def test_routine_constructor():
    ''' Check the constructor and associated type checking. '''
    symbol = RoutineSymbol("hello")
    with pytest.raises(TypeError) as err:
        Routine(1)
    assert ("Routine argument 'symbol' must be present and must be a "
            "RoutineSymbol but got 'int'" in str(err.value))
    with pytest.raises(TypeError) as err:
        Routine(symbol, is_program=1)
    assert "'is_program' must be a bool" in str(err.value)
    node = Routine(symbol)
    assert node.name == "hello"
    assert isinstance(node._symbol, RoutineSymbol)
    assert node._symbol.name == "hello"


def test_routine_properties():
    ''' Check the various properties of the Routine class. '''
    node1 = Routine.create("hello")
    assert node1.dag_name == "routine_hello_0"
    assert node1.return_symbol is None
    assert node1.is_program is False
    assert node1.name == "hello"
    # Give the Routine a child to get full coverage of __str__ method
    node1.addchild(Assignment())
    assert "Routine[name:'hello']:\nAssignment" in str(node1)

    node2 = Routine.create("bonjour")
    assert node2.is_program is False

    node3 = Routine.create("gutentag", is_program=True)
    assert node3.is_program

    with pytest.raises(TypeError) as excinfo:
        node3.symbol = "123"
    assert ("Routine symbol must be a RoutineSymbol but got 'str'"
            in str(excinfo.value))
    node3.symbol = RoutineSymbol("ave")
    assert node3.symbol.name == "ave"


def test_routine_name_setter():
    ''' Check the name setter property of the Routine class updates its
    name and its associated Routine symbol. '''

    node = Routine.create("hello")
    # The constructor has an implicit name setter
    # Check the associated RoutineSymbol has been created
    assert "hello" == node._symbol.name
    assert isinstance(node._symbol, RoutineSymbol)
    # Check with an incorrect value type
    with pytest.raises(TypeError) as err:
        node.name = 3
    assert "must be a str but got" in str(err.value)

    # Perform a successful name change
    assert node.name == "hello"
    node.name = "goodbye"
    assert node.name == "goodbye"
    # Check that the previous symbol has been deleted and the new one created
    assert "hello" not in node.symbol_table

    # Repeat when there is no RoutineSymbol present at all.
    node.symbol_table._symbols.pop("goodbye")
    node.name = "hello_again"
    assert node.name == "hello_again"

    # Repeat when the Routine has a parent.
    cntr = Container("greeting")
    cntr.addchild(node)
    node.name = "ave"
    assert node.name == "ave"


def test_routine_return_symbol_setter():
    ''' Check that the return_symbol setter works correctly and rejects invalid
    values.

    '''
    node = Routine.create("hello")
    assert node.return_symbol is None
    with pytest.raises(TypeError) as err:
        node.return_symbol = "wrong"
    assert ("Routine return-symbol should be a DataSymbol but found 'str'" in
            str(err.value))
    sym = DataSymbol("result", REAL_TYPE)
    with pytest.raises(KeyError) as err:
        node.return_symbol = sym
    assert ("For a symbol to be a return-symbol, it must be present in the "
            "symbol table of the Routine but \'result\' is not." in
            str(err.value))
    node.symbol_table.add(sym)
    node.return_symbol = sym
    assert node.return_symbol is sym


def test_routine_create_invalid():
    '''Test that the create method in the Routine class raises the
    expected exceptions if the provided input is invalid.

    '''
    symbol_table = SymbolTable()
    symbol = DataSymbol("x", REAL_TYPE)
    symbol_table.add(symbol)
    children = [Assignment.create(Reference(symbol),
                                  Literal("1", REAL_TYPE))]

    # name is not a string.
    with pytest.raises(TypeError) as excinfo:
        _ = Routine.create(1, symbol_table, children)
    assert ("name argument in create method of Routine class "
            "should be a string but found 'int'.") in str(excinfo.value)

    # symbol_table not a SymbolTable.
    with pytest.raises(TypeError) as excinfo:
        _ = Routine.create("mod_name", "invalid", children)
    assert ("symbol_table argument in create method of Routine class "
            "should be a SymbolTable but found 'str'.") in str(excinfo.value)

    # children not a list.
    with pytest.raises(TypeError) as excinfo:
        _ = Routine.create("mod_name", symbol_table, "invalid")
    assert ("children argument in create method of Routine class "
            "should be a list but found 'str'." in str(excinfo.value))

    # contents of children list are not Node.
    with pytest.raises(TypeError) as excinfo:
        _ = Routine.create("mod_name", symbol_table, ["invalid"])
    assert (
        "child of children argument in create method of Routine class "
        "should be a PSyIR Node but found 'str'." in str(excinfo.value))


def test_routine_create():
    '''Test that the create method correctly creates a Routine instance. '''
    symbol_table = SymbolTable()
    symbol = DataSymbol("tmp", REAL_TYPE)
    symbol_table.add(symbol)
    assignment = Assignment.create(Reference(symbol),
                                   Literal("0.0", REAL_TYPE))
    kschedule = Routine.create("mod_name", symbol_table, [assignment],
                               is_program=True, return_symbol_name=symbol.name)
    assert isinstance(kschedule, Routine)
    check_links(kschedule, [assignment])
    assert kschedule.symbol_table is symbol_table
    assert symbol_table.node is kschedule
    assert kschedule.is_program
    assert kschedule.return_symbol is symbol


def test_routine_equality(monkeypatch):
    ''' Test the __eq__ method for Routines.'''
    # In this test we disable the routine parent (ScopingNode) __eq__
    # test, which is already tested in the appropriate test file.
    monkeypatch.setattr(ScopingNode, "__eq__", lambda x, y: True)

    symbol_table = SymbolTable()
    symbol = DataSymbol("tmp", REAL_TYPE)
    symbol_table.add(symbol)
    assignment = Assignment.create(Reference(symbol),
                                   Literal("0.0", REAL_TYPE))
    assignment2 = Assignment.create(Reference(symbol),
                                    Literal("0.0", REAL_TYPE))

    ksched1 = Routine.create("mod_name", symbol_table, [assignment],
                             is_program=True, return_symbol_name=symbol.name)
    ksched2 = Routine.create("mod_name", symbol_table.deep_copy(),
                             [assignment2], is_program=True)
    # In this case we still need to associate to the same symbol table because
    # the return symbol must point to the exact same instance.
    ksched2._symbol_table = symbol_table
    ksched2.return_symbol = symbol
    assert ksched1 == ksched2

    # Test non-equality if different names.
    assignment2.detach()
    ksched3 = Routine.create("mod_name", symbol_table.deep_copy(),
                             [assignment2], is_program=True,
                             return_symbol_name=symbol.name)
    # Workaround for the routine name
    ksched3.name = "mod_name2"

    assert ksched1 != ksched3

    # Reset the name so we can create more routines
    ksched3.name = "mod_name"

    # Test non-equality if different is_program status
    assignment2.detach()
    ksched4 = Routine.create("mod_name", symbol_table.deep_copy(),
                             [assignment2], is_program=False,
                             return_symbol_name=symbol.name)
    assert ksched1 != ksched4

    # Test non-equality if different return symbols
    assignment2.detach()
    ksched5 = Routine.create("mod_name", symbol_table.deep_copy(),
                             [assignment2], is_program=True,
                             return_symbol_name=None)
    assert ksched1 != ksched5


def test_routine_copy():
    '''Test that the copy method correctly creates an equivalent Routine
    instance. '''
    # Create a function
    symbol_table = SymbolTable()
    routine = Routine.create("my_func", symbol_table, [])
    symbol = DataSymbol("my_result", REAL_TYPE)
    routine.symbol_table.add(symbol)
    routine.return_symbol = symbol

    # After a copy the symbol tables are separate and the return symbol
    # references a internal copy of the symbol
    routine2 = routine.copy()
    assert routine2.symbol_table is not routine.symbol_table
    assert routine2.symbol_table.node is routine2
    assert routine2.return_symbol in routine2.symbol_table.symbols
    assert routine2.return_symbol not in routine.symbol_table.symbols


def test_routine_copy_in_container(fortran_reader):
    '''
    Test the copying of a Routine when it is inside a Container and has
    references to Symbols declared in that Container.
    '''
    psyir = fortran_reader.psyir_from_source('''
    module my_mod
      use other_mod, only: trouble
      implicit none

    contains
      subroutine my_sub()
        trouble = trouble + 1
      end subroutine my_sub
    end module my_mod
    ''')
    rt0 = psyir.walk(Routine)[0]
    rt1 = rt0.copy()
    assert rt1.parent is None
    # The copy of just the routine (and not its parent container) leaves
    # it with dangling References - i.e. References to Symbols that aren't in
    # any table.
    assert "trouble" not in rt1.symbol_table
    ref = rt1.walk(Reference)[0]
    assert ref.symbol.name == "trouble"
    # TODO #723 - these dangling References should be handled when the Routine
    # is attached to a new parent.


def test_routine_replace_with(fortran_reader):
    '''Test that the replace_with method correctly replaces the Routine
    with another Routine. '''

    code = '''subroutine a()
    end subroutine a'''
    psyir = fortran_reader.psyir_from_source(code)
    rout = psyir.walk(Routine)[0]

    with pytest.raises(TypeError) as excinfo:
        rout.replace_with(123)
    assert ("The argument node in method replace_with in the Routine "
            "class should be a Routine but found 'int'." in str(excinfo.value))

    rout2 = Routine.create("a")
    with pytest.raises(GenerationError) as excinfo:
        rout2.replace_with(rout)
    assert ("This node should have a parent if its replace_with method is "
            "called." in str(excinfo.value))

    code2 = '''subroutine a()
    end subroutine a'''
    psyir2 = fortran_reader.psyir_from_source(code2)
    rout2 = psyir2.walk(Routine)[0]

    with pytest.raises(GenerationError) as excinfo:
        rout.replace_with(rout2)
    assert ("The parent of argument node in method replace_with in the "
            "Routine class should be None but found 'FileContainer'."
            in str(excinfo.value))

    rout2 = Routine.create("a")
    with pytest.raises(GenerationError) as excinfo:
        rout.replace_with(rout2)
    assert ("The symbol of argument node in method replace_with in the "
            "Routine class should be the same as the Routine being "
            "replaced." in str(excinfo.value))

    rout2 = Routine(symbol=rout._symbol)

    rout.replace_with(rout2)
    assert rout.parent is None
    assert rout2.parent is psyir


def test_routine_update_parent_symbol_table_illegal_parent(fortran_reader):
    ''' Test the cases where we're attempting to add a routine into
     a scope we aren't allowed. '''
    code = '''module my_mod
    contains
        subroutine routine()
        end subroutine routine
    end module my_mod'''
    psyir = fortran_reader.psyir_from_source(code)
    module = psyir.walk(Container)[1]
    alt_routine = Routine.create("routine")
    with pytest.raises(GenerationError) as excinfo:
        module.addchild(alt_routine)
    assert ("Can't add routine 'routine' into a scope that already contains "
            "a resolved symbol with the same name." in str(excinfo.value))

    alt_routine = Routine(module.symbol_table.lookup("routine"))
    with pytest.raises(GenerationError) as excinfo:
        module.addchild(alt_routine)
    assert ("Can't add routine 'routine' into a scope that already contains "
            "a Routine with that name." in str(excinfo.value))

    # Need a CodeBlock routine
    code = '''module my_mod
    contains
        recursive subroutine routine()
        end subroutine routine
    end module my_mod'''
    psyir = fortran_reader.psyir_from_source(code)
    module = psyir.walk(Container)[1]
    assert isinstance(module.children[0], CodeBlock)
    alt_routine = Routine(module.symbol_table.lookup("routine"))
    with pytest.raises(GenerationError) as excinfo:
        module.addchild(alt_routine)
    assert ("Can't add routine 'routine' into a scope that already contains "
            "a CodeBlock representing a routine with that name."
            in str(excinfo.value))


def test_routine_update_parent_symbol_table():
    ''' Test the update_parent_symbol_table function of the Routine class.
    Some of the tests here are accessed through addchild of a container. '''
    routine = Routine.create("test")

    symbol_table = SymbolTable()
    container = Container.create("my_container", symbol_table, [])

    assert routine.symbol_table.lookup("test") is not None
    container.addchild(routine)
    assert container.symbol_table.lookup("test").is_modulevar
    # Routine's symbol table should no longer contain the RoutineSymbol
    with pytest.raises(KeyError):
        routine.symbol_table.lookup("test", scope_limit=routine)
    assert container.symbol_table.lookup("test") is routine.symbol

    # Test the update_parent_symbol_table mimicing using replace_with.
    routine2 = Routine(routine.symbol)
    routine.detach()
    container.symbol_table.add(routine.symbol)
    routine2.update_parent_symbol_table(container)
    # This all should work correctly, add routine2 as a child of container
    # so we can test the last section
    container.addchild(routine2)
    # Test the removal of a parent section of update_parent_symbol_table
    with pytest.raises(KeyError):
        routine2.symbol_table.lookup("test", scope_limit=routine2)
    routine2.update_parent_symbol_table(None)
    assert (routine2.symbol_table.lookup("test", scope_limit=routine2) is
            routine.symbol)


def test_routine_update_parent_symbol_table_when_referenced(fortran_reader):
    ''' Test the update_parent_symbol_table function of the Routine class when
    the target RoutineSymbol cannot be removed because of other references to
    it.

    '''
    code = '''\
    module my_mod
      interface do_it
        module procedure :: do_64, do_32
      end interface
    contains
      subroutine do_64(var)
        real*8 :: var
      end subroutine do_64
      subroutine do_32(var)
        real*4 :: var
      end subroutine do_32
    end module my_mod'''
    psyir = fortran_reader.psyir_from_source(code)
    cntr = psyir.children[0]
    do_64_sym = cntr.symbol_table.lookup("do_64")
    assert do_64_sym.is_modulevar
    do_64 = cntr.children[0]
    do_64.detach()
    # The RoutineSymbol must still be in the symbol table because it is
    # referred to by the Interface.
    assert "do_64" in cntr.symbol_table
    assert do_64_sym.is_unresolved
    cntr.addchild(do_64)
    assert do_64_sym.is_modulevar


def test_routine_update_parent_symbol_table_missing_symbol(fortran_reader):
    '''
    Test that update_parent_symbol_table() works correctly when the symbol
    representing the Routine is missing from the parent symbol table.

    TODO #2702 - the code that this test covers is marked with this TODO.
    '''
    code = '''\
    module my_mod
    contains
      subroutine do_64(var)
        real*8 :: var
      end subroutine do_64
    end module my_mod'''
    psyir = fortran_reader.psyir_from_source(code)
    cntr = psyir.children[0]
    # Remove the RoutineSymbol from the parent Container.
    cntr.symbol_table._symbols.pop("do_64")
    do_64 = cntr.children[0]
    # Then detach the routine.
    do_64.detach()
    assert len(cntr.symbol_table._symbols.keys()) == 0


def test_check_outer_scope_accesses(config_instance):
    '''
    Tests for the check_outer_scope_accesses() method.
    '''
    _, invoke = get_invoke("single_invoke_three_kernels_with_use.f90",
                           "gocean", idx=0, dist_mem=False)
    schedule = invoke.schedule
    kcall = schedule.walk(CodedKern)[1]
    config_instance.include_paths = []
    # Multiple wildcard imports are handled by bringing them into the routine
    # and so aren't a problem.
    kcall.get_kernel_schedule().check_outer_scope_accesses(kcall, "Kernel")
    # Now try where there's only a single wildcard import so we know the origin
    # of the symbol.
    kcall0 = schedule.walk(CodedKern)[0]
    ksched = kcall0.get_kernel_schedule()
    ctable = ksched.ancestor(Container).symbol_table
    # To do this, we manually remove all ContainerSymbols apart from the one
    # from which 'go_wp' is imported.
    for sym in ctable.wildcard_imports():
        if sym.name != "kind_params_mod":
            ctable._symbols.pop(sym.name)

    ksched.check_outer_scope_accesses(kcall0, "Kernel")
    table = ksched.symbol_table
    assert (table.lookup("go_wp").interface.container_symbol.name ==
            "kind_params_mod")


def test_check_outer_scope_accesses_import_clash(fortran_reader):
    '''
    Check that check_outer_scope_accesses() spots a clash with an imported
    Symbol.

    '''
    psyir = fortran_reader.psyir_from_source('''\
    module my_mod
      use my_kernel_mod, only: a_routine
    contains
      subroutine call_it()

        if(.TRUE.)then
          call a_routine()
        end if
      end subroutine call_it
    end module my_mod
    ''')
    rt_psyir = fortran_reader.psyir_from_source('''\
    subroutine a_routine()
      use other_mod, only: a_clash
    end subroutine a_routine
    ''')
    kern_call = psyir.walk(Call)[0]
    csym = ContainerSymbol("money")
    kern_call.scope.symbol_table.add(csym)
    kern_call.scope.symbol_table.add(Symbol("a_clash",
                                            interface=ImportInterface(csym)))
    sched = rt_psyir.children[0]
    with pytest.raises(SymbolError) as err:
        sched.check_outer_scope_accesses(kern_call, "Call")
    assert ("One or more symbols from routine 'a_routine' cannot be added to "
            "the table at the call site" in str(err.value))
    assert ("This table has an import of 'a_clash' via interface" in
            str(err.value))


def test_outer_scope_accesses_unresolved(fortran_reader):
    '''
    Test that check_outer_scope_accesses() raises the expected errors for
    symbols that aren't found or are unresolved.

    '''
    psyir = fortran_reader.psyir_from_source('''\
    module my_mod
      use another_mod
    contains
      subroutine call_it()
        write(*,*) unresolved()
        call a_routine()
      end subroutine call_it
    end module my_mod
    ''')
    rt0 = psyir.children[0].children[0]
    sym = rt0.symbol_table.lookup("a_routine")
    assert sym.is_unresolved
    call = Call.create(RoutineSymbol("a_routine"), [])
    # The access to 'unresolved' is in a CodeBlock and we don't have a
    # Symbol for it.
    with pytest.raises(SymbolError) as err:
        rt0.check_outer_scope_accesses(call, "call")
    assert ("'call_it' contains accesses to 'unresolved' but the origin of "
            "this" in str(err.value))
    # Remove the CodeBlock and repeat.
    rt0.children[0].detach()
    rt0.check_outer_scope_accesses(call, "call")
    # The interface should have been updated as there's only 1 wildcard import
    assert sym.interface.container_symbol.name == "another_mod"


def test_outer_scope_accesses_multi_wildcards(fortran_reader):
    '''
    Test that check_outer_scope_accesses() raises the expected errors when it's
    not known which wildcard import is bringing a symbol into scope..

    '''
    psyir = fortran_reader.psyir_from_source('''\
    module my_mod
      use another_mod
      use this_one
    contains
      subroutine call_it(vaar)
        real, intent(inout) :: vaar
        call a_routine()
        vaar = 1.0_r_def
      end subroutine call_it
    end module my_mod
    ''')
    rt0 = psyir.children[0].children[0]
    call = Call.create(RoutineSymbol("a_routine"), [])
    # By default we allow unresolved Symbols.
    rt0.check_outer_scope_accesses(call, "call")
    # But not if we disable that.
    with pytest.raises(SymbolError) as err:
        rt0.check_outer_scope_accesses(call, "call", permit_unresolved=False)
    assert ("'call_it' contains accesses to 'a_routine' which is unresolved. "
            "It is being brought into" in str(err.value))
    # Remove the call.
    rt0.children[0].detach()
    # Now the kind parameter 'r_def' should be flagged.
    with pytest.raises(SymbolError) as err:
        rt0.check_outer_scope_accesses(call, "call", permit_unresolved=False)
    assert ("'call_it' contains accesses to 'r_def' which is unresolved. It "
            "is being brought into scope from one of ['another_mod', "
            "'this_one']" in str(err.value))
    # But not if we ignore non-data accesses.
    rt0.check_outer_scope_accesses(call, "call", permit_unresolved=False,
                                   ignore_non_data_accesses=True)


def test_outer_scope_accesses_module_data(fortran_reader):
    '''
    Test that check_outer_scope_accesses() raises the expected errors when a
    routine accesses module data.

    '''
    psyir = fortran_reader.psyir_from_source('''\
    module my_mod
      use kinds_mod
      real(kind=r_def) :: vaar
    contains
      subroutine call_it()
        call second()
      end subroutine call_it
      subroutine second()
        vaar = 2.0_r_def
      end subroutine second
    end module my_mod
    ''')
    rt1 = psyir.children[0].children[1]
    call = psyir.walk(Call)[0]
    # No error because `vaar` at the call site is the same Symbol that is
    # accessed within the called routine.
    rt1.check_outer_scope_accesses(call, "call")
    rt0 = psyir.children[0].children[0]
    # Add a new `vaar` Symbol at the call site that shadows the module
    # variable.
    rt0.symbol_table.new_symbol("vaar", shadowing=True)
    with pytest.raises(SymbolError) as err:
        rt1.check_outer_scope_accesses(call, "call")
    assert ("'second' contains accesses to 'vaar' which is declared in the "
            "callee module scope" in str(err.value))
