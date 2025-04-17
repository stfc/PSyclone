# -----------------------------------------------------------------------------
# Copyright (c) 2021-2023 Science and Technology Facilities Council.
# All rights reserved.
#
# Modifications made as part of the fparser project are distributed
# under the following license:
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are
# met:
#
# 1. Redistributions of source code must retain the above copyright
# notice, this list of conditions and the following disclaimer.
#
# 2. Redistributions in binary form must reproduce the above copyright
# notice, this list of conditions and the following disclaimer in the
# documentation and/or other materials provided with the distribution.
#
# 3. Neither the name of the copyright holder nor the names of its
# contributors may be used to endorse or promote products derived from
# this software without specific prior written permission.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
# "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
# LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
# A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
# HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
# SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
# LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
# DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
# THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
# (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
# OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
# -----------------------------------------------------------------------------

""" Module containing tests for the symbol-table functionality
 of fparser2. """

import pytest
from fparser.api import get_reader
from fparser.two import Fortran2003
from fparser.two.parser import ParserFactory
from fparser.two.symbol_table import SymbolTable, SYMBOL_TABLES, SymbolTableError


def test_basic_table():
    """Check the basic functionality of a symbol table."""
    table = SymbolTable("BAsic")
    # Name of table is not case sensitive
    assert table.name == "basic"
    assert table.parent is None
    assert table.children == []
    assert table.node is None
    # Consistency checking is disabled by default
    assert table._checking_enabled is False
    with pytest.raises(KeyError) as err:
        table.lookup("missing")
    assert "Failed to find symbol named 'missing'" in str(err.value)
    # Add a symbol and check that its naming is not case sensitive
    table.add_data_symbol("Var", "integer")
    sym = table.lookup("var")
    assert sym.name == "var"
    assert table.lookup("VAR") is sym
    # Check that we can enable consistency checking
    table2 = SymbolTable("table2", checking_enabled=True)
    assert table2._checking_enabled is True
    # Check that we can supply an associated parse tree node.
    with pytest.raises(TypeError) as err:
        SymbolTable("table3", node="oops")
    assert (
        "The 'node' argument to the SymbolTable constructor must be a "
        "valid parse tree node (instance of utils.Base) but got 'str'" in str(err.value)
    )
    table3 = SymbolTable("table3", node=Fortran2003.Return_Stmt("return"))
    assert isinstance(table3.node, Fortran2003.Return_Stmt)


def test_add_data_symbol():
    """Test that the add_data_symbol() method behaves as expected when
    validation is enabled."""
    table = SymbolTable("basic", checking_enabled=True)
    table.add_data_symbol("var", "integer")
    sym = table.lookup("var")
    assert sym.primitive_type == "integer"
    with pytest.raises(SymbolTableError) as err:
        table.add_data_symbol("var", "real")
    assert (
        "Symbol table already contains a symbol for a variable with name "
        "'var'" in str(err.value)
    )
    with pytest.raises(TypeError) as err:
        table.add_data_symbol(table, "real")
    assert "name of the symbol must be a str but got 'SymbolTable'" in str(err.value)
    with pytest.raises(TypeError) as err:
        table.add_data_symbol("var2", table)
    assert (
        "primitive type of the symbol must be specified as a str but got "
        "'SymbolTable'" in str(err.value)
    )
    # Check a clash with a USE statement - both the module name and the
    # name of imported variables
    table.add_use_symbols("mod1", only_list=[("var3", None)])
    with pytest.raises(SymbolTableError) as err:
        table.add_data_symbol("mod1", "real")
    assert "table already contains a use of a module with name 'mod1'" in str(err.value)
    with pytest.raises(SymbolTableError) as err:
        table.add_data_symbol("var3", "real")
    assert (
        "table already contains a use of a symbol named 'var3' from "
        "module 'mod1'" in str(err.value)
    )


def test_add_data_symbols_no_checks():
    """Check that we can disable the checks in the
    add_data_symbol() method."""
    table = SymbolTable("basic", checking_enabled=False)
    table.add_data_symbol("var", "integer")
    table.add_data_symbol("var", "real")
    sym = table.lookup("var")
    assert sym.primitive_type == "real"
    table.add_use_symbols("mod1", [("var3", None)])
    table.add_data_symbol("mod1", "real")
    table.add_use_symbols("mod2", [("var3", None)])
    table.add_data_symbol("var3", "real")
    assert table.lookup("var3").primitive_type == "real"


def test_add_use_symbols():
    """Test that the add_use_symbols() method behaves as expected."""
    table = SymbolTable("basic")
    # A use without an 'only' clause
    table.add_use_symbols("mod1")
    assert table._modules["mod1"].only_list is None
    assert table._modules["mod1"].rename_list is None
    assert table._modules["mod1"].wildcard_import is True
    # Fortran permits other use statements for the same module
    table.add_use_symbols("mod1", only_list=[("var", None)])
    # Since we already have a wildcard import that should remain true while
    # we now also capture those symbols that are explicitly imported.
    assert table._modules["mod1"].only_list == ["var"]
    assert table._modules["mod1"].wildcard_import
    table.add_use_symbols("mod2", only_list=[("iVar", None)])
    assert table._modules["mod2"].only_list == ["ivar"]
    table.add_use_symbols("mod2", only_list=[("jvar", None)])
    assert sorted(table._modules["mod2"].only_list) == ["ivar", "jvar"]


def test_add_use_symbols_errors():
    """Test the various checks on the supplied parameters to
    add_use_symbols()."""
    table = SymbolTable("basic")
    with pytest.raises(TypeError) as err:
        table.add_use_symbols(table)
    assert "name of the module must be a str but got 'SymbolTable'" in str(err.value)
    with pytest.raises(TypeError) as err:
        table.add_use_symbols("mod3", only_list="hello")
    assert "If present, the only_list must be a list but got 'str'" in str(err.value)
    with pytest.raises(TypeError) as err:
        table.add_use_symbols("mod3", only_list=[("hello", None, None)])
    assert (
        "If present, the only_list must be a list of 2-tuples but got: "
        "[('hello', None, None)]" in str(err.value)
    )


def test_str_method():
    """Test the str property of the SymbolTable class."""
    table = SymbolTable("basic")
    assert "Symbol Table 'basic'\nSymbols:\nUsed modules:\n" in str(table)
    table.add_data_symbol("var", "integer")
    assert "Symbol Table 'basic'\nSymbols:\nvar\nUsed modules:\n" in str(table)
    table.add_use_symbols("some_mod")
    assert "Symbol Table 'basic'\nSymbols:\nvar\nUsed modules:\nsome_mod\n" in str(
        table
    )


def test_del_child():
    """Checks for the del_child method."""
    table = SymbolTable("BASIC")
    inner_table = SymbolTable("func1", parent=table)
    table.add_child(inner_table)
    with pytest.raises(KeyError) as err:
        table.del_child("missing")
    assert "Symbol table 'basic' does not contain a table named 'missing'" in str(
        err.value
    )
    table.del_child("func1")
    assert table.children == []


def test_parent_child():
    """Test the parent/child-related properties."""
    table = SymbolTable("BASIC")
    with pytest.raises(TypeError) as err:
        table.add_child("wrong")
    assert "Expected a SymbolTable instance but got 'str'" in str(err.value)
    inner_table = SymbolTable("func1", parent=table)
    table.add_child(inner_table)
    assert table.children == [inner_table]
    assert inner_table.parent is table
    with pytest.raises(TypeError) as err:
        inner_table.parent = "wrong"
    assert (
        "Unless it is None, the parent of a SymbolTable must also be a "
        "SymbolTable but got 'str'" in str(err.value)
    )


def test_root_property():
    """Test the `root` property of the SymbolTable."""
    table = SymbolTable("BASIC")
    inner_table = SymbolTable("func1", parent=table)
    table.add_child(inner_table)
    inner_inner_table = SymbolTable("func2", parent=inner_table)
    assert inner_inner_table.root is table
    assert inner_table.root is table
    assert table.root is table


def test_module_use(f2003_parser):
    """Check that a USE of a module is captured in the symbol table."""
    _ = f2003_parser(
        get_reader(
            """\
PROGRAM a_prog
  use some_mod
END PROGRAM a_prog
    """
        )
    )
    tables = SYMBOL_TABLES
    table = tables.lookup("a_prog")
    assert isinstance(table, SymbolTable)
    assert table.parent is None
    assert "some_mod" in table._modules


def test_module_use_with_only(f2003_parser):
    """Check that USE statements with an ONLY: clause are correctly captured
    in the symbol table."""
    _ = f2003_parser(
        get_reader(
            """\
PROGRAM a_prog
  use some_mod, only:
  use mod2, only: this_one, that_one
END PROGRAM a_prog
    """
        )
    )
    tables = SYMBOL_TABLES
    table = tables.lookup("a_prog")
    assert isinstance(table, SymbolTable)
    assert table.parent is None
    assert "some_mod" in table._modules
    assert table._modules["some_mod"].only_list == []
    assert "mod2" in table._modules
    assert sorted(table._modules["mod2"].only_list) == ["that_one", "this_one"]
    sym = table.lookup("this_one")
    assert sym.name == "this_one"
    assert sym.primitive_type == "unknown"
    sym = table.lookup("that_one")
    assert sym.name == "that_one"
    assert sym.primitive_type == "unknown"


def test_module_use_with_rename(f2003_parser):
    """Check that USE statements with renamed imported symbols are correctly
    captured in the symbol table and do not clash."""
    _ = f2003_parser(
        get_reader(
            """\
PROGRAM a_prog
  use mod2, only: this_one => that_one
  use mod3, local => other
  integer :: that_one
  logical :: other
END PROGRAM a_prog
    """
        )
    )
    tables = SYMBOL_TABLES
    table = tables.lookup("a_prog")
    assert isinstance(table, SymbolTable)
    assert table.parent is None
    assert "mod2" in table._modules
    mod2 = table._modules["mod2"]
    assert mod2.only_list == ["this_one"]
    assert mod2.get_declared_name("this_one") == "that_one"
    sym = table.lookup("that_one")
    assert sym.primitive_type == "integer"
    mod3 = table._modules["mod3"]
    assert mod3.rename_list == ["local"]
    assert mod3.get_declared_name("local") == "other"
    assert table.lookup("other").primitive_type == "logical"


def test_wildcard_module_search(f2003_parser):
    """Test the wildcard_imports method of the SymbolTable."""
    _ = f2003_parser(
        get_reader(
            """\
module my_mod
  use other_mod, only: b
  use medium_mod
  use some_mod
  real :: a
contains
  subroutine sub
    use big_mod
    use medium_mod, only: c
    use pointless_mod, only:
  end subroutine sub
end module my_mod
    """
        )
    )
    # Module symbol table should have two modules listed with wildcard imports.
    mod_table = SYMBOL_TABLES.lookup("my_mod")
    assert mod_table.wildcard_imports == ["medium_mod", "some_mod"]
    # Move down to the subroutine and check that we recurse upwards.
    sub_table = mod_table.children[0]
    assert sub_table.wildcard_imports == ["big_mod", "medium_mod", "some_mod"]
    # Repeat for a program containing a subroutine containing a function.
    _ = f2003_parser(
        get_reader(
            """\
program my_prog
  use pointless_mod, only: dee
  use medium_mod
  use no_really_mod
  real :: a
  call sub()
contains
  subroutine sub
    use big_mod
    use no_really_mod, only: avon
    use medium_mod, only: c
    use pointless_mod, only:
    write(*,*) "yes", my_fn()
  contains
    integer function my_fn()
      use no_really_mod, only: afon
      use tiny_mod
      my_fn = 1 + afon
    end function my_fn
  end subroutine sub
end program my_prog
    """
        )
    )
    prog_table = SYMBOL_TABLES.lookup("my_prog")
    assert prog_table.wildcard_imports == ["medium_mod", "no_really_mod"]
    sub_table = prog_table.children[0]
    assert sub_table.wildcard_imports == ["big_mod", "medium_mod", "no_really_mod"]
    fn_table = sub_table.children[0]
    assert fn_table.wildcard_imports == [
        "big_mod",
        "medium_mod",
        "no_really_mod",
        "tiny_mod",
    ]


def test_module_definition(f2003_parser):
    """Check that a SymbolTable is created for a module and populated with
    the symbols it defines."""
    _ = f2003_parser(
        get_reader(
            """\
module my_mod
  use some_mod
  real :: a
end module my_mod
    """
        )
    )
    tables = SYMBOL_TABLES
    assert list(tables._symbol_tables.keys()) == ["my_mod"]
    table = tables.lookup("my_mod")
    assert isinstance(table, SymbolTable)
    assert isinstance(table.node, Fortran2003.Module_Stmt)
    assert "some_mod" in table._modules
    assert "a" in table._data_symbols
    sym = table.lookup("a")
    assert sym.name == "a"
    assert sym.primitive_type == "real"


def test_routine_in_module(f2003_parser):
    """Check that we get two, nested symbol tables when a module contains
    a subroutine."""
    _ = f2003_parser(
        get_reader(
            """\
module my_mod
  use some_mod
  real :: a
contains
  subroutine my_sub()
  end subroutine my_sub
end module my_mod
    """
        )
    )
    tables = SYMBOL_TABLES
    assert list(tables._symbol_tables.keys()) == ["my_mod"]
    table = tables.lookup("my_mod")
    assert isinstance(table.node, Fortran2003.Module_Stmt)
    assert len(table.children) == 1
    assert table.children[0].name == "my_sub"
    assert table.children[0].parent is table
    # Check that the search for a symbol moves up to the parent scope
    sym = table.children[0].lookup("a")
    assert sym.name == "a"
    assert sym.primitive_type == "real"


def test_routine_in_prog(f2003_parser):
    """Check that we get two, nested symbol tables when a program contains
    a subroutine."""
    _ = f2003_parser(
        get_reader(
            """\
program my_prog
  use some_mod
  real :: a
contains
  subroutine my_sub()
    real :: b
  end subroutine my_sub
end program my_prog
    """
        )
    )
    tables = SYMBOL_TABLES
    assert list(tables._symbol_tables.keys()) == ["my_prog"]
    table = SYMBOL_TABLES.lookup("my_prog")
    assert len(table.children) == 1
    assert isinstance(table.node, Fortran2003.Program_Stmt)
    assert table.children[0].name == "my_sub"
    assert isinstance(table.children[0].node, Fortran2003.Subroutine_Stmt)
    assert table.children[0]._data_symbols["b"].name == "b"
    assert table.children[0].parent is table


def test_all_symbols_resolved(f2003_parser):
    """Tests for the all_symbols_resolved() method."""
    code = """\
program my_prog
  use some_mod
  real :: a
contains
  subroutine my_sub()
    real :: b
  end subroutine my_sub
end program my_prog
    """
    _ = f2003_parser(get_reader(code))
    table = SYMBOL_TABLES.lookup("my_prog").children[0]
    assert table.all_symbols_resolved is False
    new_code = code.replace("some_mod\n", "some_mod, only: igor\n")
    SYMBOL_TABLES.clear()
    _ = f2003_parser(get_reader(new_code))
    table = SYMBOL_TABLES.lookup("my_prog").children[0]
    assert table.all_symbols_resolved is True


def test_all_symbols_resolved_submodule():
    """Tests for the all_symbols_resolved() method returns False when the table
    is within a Fortran2008 submodule (since a submodule has access to the
    scope of its parent module)."""
    code = """\
submodule (other_mod) my_mod
  real :: a
contains
  subroutine my_sub()
    real :: b
  end subroutine my_sub
end submodule my_mod
    """
    _ = ParserFactory().create(std="f2008")(get_reader(code))
    table = SYMBOL_TABLES.lookup("my_mod").children[0]
    assert table.all_symbols_resolved is False
