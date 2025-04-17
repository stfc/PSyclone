# Copyright (c) 2018-2022 Science and Technology Facilities Council.

# All rights reserved.

# Modifications made as part of the fparser project are distributed
# under the following license:

# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are
# met:

# 1. Redistributions of source code must retain the above copyright
# notice, this list of conditions and the following disclaimer.

# 2. Redistributions in binary form must reproduce the above copyright
# notice, this list of conditions and the following disclaimer in the
# documentation and/or other materials provided with the distribution.

# 3. Neither the name of the copyright holder nor the names of its
# contributors may be used to endorse or promote products derived from
# this software without specific prior written permission.

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

"""Test Fortran 2003 rule R1109 : This file tests the support for the
Use statement.

"""

import pytest
from fparser.api import get_reader
from fparser.two.Fortran2003 import Use_Stmt
from fparser.two.symbol_table import SYMBOL_TABLES, ModuleUse
from fparser.two.utils import NoMatchError, InternalError


@pytest.fixture(autouse=True)
@pytest.mark.usefixtures("f2003_create")
def use_f2003():
    """
    A pytest fixture that just sets things up so that the f2003_create
    fixture is used automatically for every test in this file.
    """


# match() use ...


# match() 'use x'. Use both string and reader input here, but from
# here on we will just use string input as that is what is passed to
# the match() method
def test_use():
    """Check that a basic use is parsed correctly. Input separately as a
    string and as a reader object

    """

    def check_use(reader):
        """Internal helper function to avoid code replication."""
        ast = Use_Stmt(reader)
        assert "USE my_model" in str(ast)
        assert repr(ast) == "Use_Stmt(None, None, Name('my_model'), '', None)"

    line = "use my_model"
    check_use(line)
    reader = get_reader(line)
    check_use(reader)


# match() 'use :: x'
def test_use_colons():
    """Check that a basic use with '::' is parsed correctly."""
    line = "use :: my_model"
    ast = Use_Stmt(line)
    assert "USE :: my_model" in str(ast)
    assert repr(ast) == "Use_Stmt(None, '::', Name('my_model'), '', None)"


# match() 'use, nature :: x'
def test_use_nature():
    """Check that a use with a 'nature' specification is parsed correctly."""
    line = "use, intrinsic :: my_model"
    ast = Use_Stmt(line)
    assert "USE, INTRINSIC :: my_model" in str(ast)
    assert repr(ast) == (
        "Use_Stmt(Module_Nature('INTRINSIC'), '::', Name('my_model'), " "'', None)"
    )


# match() 'use x, rename'
@pytest.mark.usefixtures("fake_symbol_table")
def test_use_rename():
    """Check that a use with a rename clause is parsed correctly."""
    line = "use my_module, new_name=>name"
    ast = Use_Stmt(line)
    assert "USE my_module, new_name => name" in str(ast)
    assert repr(ast) == (
        "Use_Stmt(None, None, Name('my_module'), ',', Rename_List(',', "
        "(Rename(None, Name('new_name'), Name('name')),)))"
    )
    table = SYMBOL_TABLES.current_scope
    assert "my_module" in table._modules
    use = table._modules["my_module"]
    # We only store the *local* names of the symbols in rename_list
    assert use.rename_list == ["new_name"]
    # The original name of the symbol in the module being imported is stored
    # in a separate map.
    assert use._local_to_module_map["new_name"] == "name"


def test_use_operator_rename():
    """
    Check that a use with a rename clause for an operator is parsed correctly.

    """
    line = "use my_module, operator(.new.) => operator(.old.)"
    ast = Use_Stmt(line)
    assert repr(ast) == (
        "Use_Stmt(None, None, Name('my_module'), ',', Rename_List(',', "
        "(Rename('OPERATOR', Defined_Op('.NEW.'), Defined_Op('.OLD.')),)))"
    )
    SYMBOL_TABLES.enter_scope("test_scope")
    ast = Use_Stmt(line)
    # Check that the symbol table functionality copes (by ignoring operators).
    table = SYMBOL_TABLES.current_scope
    use = table._modules["my_module"]
    # Operators are not currently captured in the SymbolTable (TODO #379)
    assert use.rename_list is None
    SYMBOL_TABLES.exit_scope()


# match() 'use x, only: y'
def test_use_only():
    """Check that a use statement is parsed correctly when there is an
    only clause. Test both with and without a scoping region.

    """
    line = "use my_model, only: name"
    ast = Use_Stmt(line)
    assert "USE my_model, ONLY: name" in str(ast)
    assert repr(ast) == (
        "Use_Stmt(None, None, Name('my_model'), ', ONLY:', Only_List(',', "
        "(Name('name'),)))"
    )
    # Repeat when there is a scoping region.
    SYMBOL_TABLES.enter_scope("test_scope")
    ast = Use_Stmt(line)
    table = SYMBOL_TABLES.current_scope
    assert "my_model" in table._modules
    use = table._modules["my_model"]
    assert use.name == "my_model"
    assert use.only_list == ["name"]
    assert use.rename_list is None
    SYMBOL_TABLES.exit_scope()


# match() 'use x, only:'
def test_use_only_empty():
    """Check that a use statement is parsed correctly when there is an
    only clause without any content. Test both with and without a scoping region.

    """
    line = "use my_model, only:"
    ast = Use_Stmt(line)
    assert "USE my_model, ONLY:" in str(ast)
    assert repr(ast) == ("Use_Stmt(None, None, Name('my_model'), ', ONLY:', None)")
    # Repeat when there is a scoping region.
    SYMBOL_TABLES.enter_scope("test_scope")
    ast = Use_Stmt(line)
    table = SYMBOL_TABLES.current_scope
    assert "my_model" in table._modules
    use = table._modules["my_model"]
    assert isinstance(use, ModuleUse)
    assert use.only_list == []
    assert use.rename_list is None
    SYMBOL_TABLES.exit_scope()


# match() 'use x, only: b => c'
def test_use_only_plus_rename():
    """Check that a use statement with an only clause and some variable
    renaming is parsed correctly.

    """
    line = "use my_model, only: a, b=>c"
    ast = Use_Stmt(line)
    assert repr(ast) == (
        "Use_Stmt(None, None, Name('my_model'), "
        "', ONLY:', Only_List(',', (Name('a'), "
        "Rename(None, Name('b'), Name('c')))))"
    )
    # Repeat when there is a scoping region.
    SYMBOL_TABLES.enter_scope("test_scope")
    ast = Use_Stmt(line)
    table = SYMBOL_TABLES.current_scope
    assert "my_model" in table._modules
    use = table._modules["my_model"]
    assert isinstance(use, ModuleUse)
    assert use.symbol_names == ["a", "b"]
    assert sorted(use.only_list) == ["a", "b"]
    assert use.rename_list is None
    assert use._local_to_module_map["b"] == "c"
    SYMBOL_TABLES.exit_scope()


# match() 'use x, only: operator(-)'
def test_use_only_operator():
    """
    Check that a 'use, only' that imports an operator is parsed correctly.

    """
    line = "use my_mod, only: operator(-), operator(.yes.)"
    ast = Use_Stmt(line)
    assert repr(ast) == (
        "Use_Stmt(None, None, Name('my_mod'), ', ONLY:', Only_List(',', "
        "(Generic_Spec('OPERATOR', Extended_Intrinsic_Op('-')),"
        " Generic_Spec('OPERATOR', Defined_Op('.YES.')))))"
    )
    # Repeat when there is a scoping region.
    SYMBOL_TABLES.enter_scope("test_scope")
    ast = Use_Stmt(line)
    table = SYMBOL_TABLES.current_scope
    use = table._modules["my_mod"]
    # Operators are not currently captured in the symbol table.
    # TODO #379.
    assert use.only_list == []
    SYMBOL_TABLES.exit_scope()


@pytest.mark.parametrize("lhs, rhs", [("-", "+"), (".in.", ".out.")])
def test_use_only_renamed_operator(lhs, rhs):
    """
    Check that a 'use, only' that imports and locally renames an operator is
    correctly parsed. We test both for an extended intrinsic operator and a
    defined operator.

    """
    line = f"use my_mod, only: operator({lhs}) => operator({rhs})"
    ast = Use_Stmt(line)
    if lhs == "-":
        assert repr(ast) == (
            "Use_Stmt(None, None, Name('my_mod'), ', ONLY:', Only_List(',', "
            "(Generic_Spec('OPERATOR', Extended_Intrinsic_Op('-) => "
            "operator(+')),)))"
        )
    else:
        assert repr(ast) == (
            "Use_Stmt(None, None, Name('my_mod'), ', ONLY:', Only_List(',', "
            "(Rename('OPERATOR', Defined_Op('.IN.'), Defined_Op('.OUT.')),)))"
        )

    # Repeat when there is a scoping region.
    SYMBOL_TABLES.enter_scope("test_scope")
    ast = Use_Stmt(line)
    table = SYMBOL_TABLES.current_scope
    use = table._modules["my_mod"]
    # Operators are not currently captured in the symbol table.
    # TODO #379.
    assert use.only_list == []
    SYMBOL_TABLES.exit_scope()


# match() '  use  ,  nature  ::  x  ,  name=>new_name'
def test_use_spaces_1():
    """Check that a use statement with spaces works correctly with
    renaming.

    """
    line = "  Use  ,  intrinsic  ::  my_model  ,  name=>new_name  "
    ast = Use_Stmt(line)
    assert "USE, INTRINSIC :: my_model, name => new_name" in str(ast)
    assert repr(ast) == (
        "Use_Stmt(Module_Nature('INTRINSIC'), '::', Name('my_model'), ',', "
        "Rename_List(',', (Rename(None, Name('name'), Name('new_name')),)))"
    )


# match() '  use  ,  nature  ::  x  ,  only  :  name'
def test_use_spaces_2():
    """Check that a use statement with spaces works correctly with an only
    clause.

    """
    line = "  use  ,  intrinsic  ::  my_model  ,  only  :  name  "
    ast = Use_Stmt(line)
    assert "USE, INTRINSIC :: my_model, ONLY: name" in str(ast)
    assert (
        repr(ast) == "Use_Stmt(Module_Nature('INTRINSIC'), '::', Name('my_model'), ', "
        "ONLY:', Only_List(',', (Name('name'),)))"
    )


# match() mixed case
def test_use_mixed_case():
    """Check that a use statement with mixed case keywords ('use' and
    'only') works as expected.

    """
    line = "UsE my_model, OnLy: name"
    ast = Use_Stmt(line)
    assert "USE my_model, ONLY: name" in str(ast)
    assert (
        repr(ast) == "Use_Stmt(None, None, Name('my_model'), ', ONLY:', Only_List(',', "
        "(Name('name'),)))"
    )


# match() Syntax errors


def test_syntaxerror():
    """Test that NoMatchError is raised for various syntax errors."""
    for line in [
        "us",
        "ust",
        "use",
        "usemy_model",
        "use, ",
        "use, ::",
        "use, intrinsic",
        "use, intrinsic::",
        "use, intrinsic my_module",
        "use,",
        "use my_model,",
        "use my_model, only",
        "use my_model, only ;",
        "use my_model, only name",
    ]:
        with pytest.raises(NoMatchError) as excinfo:
            _ = Use_Stmt(line)
        assert f"Use_Stmt: '{line}'" in str(excinfo.value)


# match() Internal errors


def test_use_internal_error1():
    """Check that an internal error is raised if the length of the Items
    list is not 5 as the str() method assumes that it is.

    """
    line = "use my_model"
    ast = Use_Stmt(line)
    ast.items = (None, None, None, None)
    with pytest.raises(InternalError) as excinfo:
        str(ast)
    assert "should be of size 5 but found '4'" in str(excinfo.value)


def test_use_internal_error2():
    """Check that an internal error is raised if the module name (entry 2
    of Items) is empty or None as the str() method assumes that it is
    a string with content.

    """
    line = "use my_model"
    ast = Use_Stmt(line)
    for content in [None, ""]:
        ast.items = (None, None, content, None, None)
        with pytest.raises(InternalError) as excinfo:
            str(ast)
        assert "entry 2 should be a module name but it is empty" in str(excinfo.value)


def test_use_internal_error3():
    """Check that an internal error is raised if entry 3 of Items is
    'None' as the str() method assumes it is a (potentially empty)
    string.

    """
    line = "use my_model"
    ast = Use_Stmt(line)
    ast.items = (None, None, "my_module", None, None)
    with pytest.raises(InternalError) as excinfo:
        str(ast)
    assert "entry 3 should be a string but found 'None'" in str(excinfo.value)


@pytest.mark.usefixtures("fake_symbol_table")
def test_use_internal_error_only_list(monkeypatch):
    """Check that an internal error is raised if an Only_List contains an
    unexpected entry.

    """
    # First get a valid parse tree and then break it.
    line = "use my_model, only: var"
    ast = Use_Stmt(line)
    only_list = ast.children[4]
    monkeypatch.setattr(only_list, "items", ("wrong",))
    # Monkeypatch the underlying _match method to return this broken tree.
    monkeypatch.setattr(Use_Stmt, "_match", lambda x: ast.children)
    with pytest.raises(InternalError) as err:
        Use_Stmt(line)
    assert (
        "An Only_List can contain only Name, Rename or Generic_Spec entries but "
        "found 'str' when matching 'use my_model, only: var'" in str(err.value)
    )
