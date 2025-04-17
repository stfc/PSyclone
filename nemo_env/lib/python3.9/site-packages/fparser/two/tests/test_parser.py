# Copyright (c) 2018-2022 Science and Technology Facilities Council.
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

""" Module containing tests for the parser file """

import pytest
from fparser.two.parser import ParserFactory
from fparser.common.readfortran import FortranStringReader
from fparser.two.utils import FortranSyntaxError, StmtBase
from fparser.two.symbol_table import SYMBOL_TABLES
from fparser.two import Fortran2003, Fortran2008


def test_parserfactory_std():
    """Test ParserFactory std argument options [none, f2003, f2008 and
    invalid]. Also tests that the SYMBOL_TABLES instance has been initialised
    correctly and that previous calls to the create method in the ParserFactory
    class do not affect current calls.

    """
    fstring = "submodule (x) y\n" "end\n"
    parser = ParserFactory().create()
    reader = FortranStringReader(fstring)
    with pytest.raises(FortranSyntaxError) as excinfo:
        _ = parser(reader)
    assert "at line 1\n>>>submodule (x) y\n" in str(excinfo.value)

    parser = ParserFactory().create(std="f2003")
    reader = FortranStringReader(fstring)
    with pytest.raises(FortranSyntaxError) as excinfo:
        _ = parser(reader)
    assert "at line 1\n>>>submodule (x) y\n" in str(excinfo.value)

    parser = ParserFactory().create(std="f2008")
    reader = FortranStringReader(fstring)
    ast = parser(reader)
    code = str(ast)
    assert "SUBMODULE (x) y\nEND" in code
    # Submodule_Stmt should now be included in the list of classes that define
    # scoping regions.
    assert "y" in SYMBOL_TABLES._symbol_tables

    # Repeat f2003 example to make sure that a previously valid (f2008)
    # match does not affect the current (f2003) invalid match.
    parser = ParserFactory().create(std="f2003")
    reader = FortranStringReader(fstring)
    with pytest.raises(FortranSyntaxError) as excinfo:
        _ = parser(reader)
    assert "at line 1\n>>>submodule (x) y\n" in str(excinfo.value)
    # The previous symbol table entries should have been removed when
    # creating the new parser.
    assert "y" not in SYMBOL_TABLES._symbol_tables

    with pytest.raises(ValueError) as excinfo:
        parser = ParserFactory().create(std="invalid")
        assert "is an invalid standard" in str(excinfo.value)


def _cmp_tree_types_rec(
    node1: Fortran2003.Program, node2: Fortran2003.Program, depth: int = 0
):
    """Helper function to recursively check for deepcopied programs

    :param node1: First AST tree to check
    :type node1: Fortran2003.Program
    :param node2: Second AST tree to check
    :type node2: Fortran2003.Program
    :param depth: Depth useful later on for debugging reasons,
        defaults to 0
    :type depth: int, optional
    """

    # Make sure that both trees are the same
    assert type(node1) is type(
        node2
    ), f"Nodes have different types: '{type(node1)}' and '{type(node2)}"

    if node1 is None:
        # Just return for None objects
        return

    if type(node1) is str:
        # WARNING: Different string objects with the same can have the same id.
        # Therefore, we can't compare with 'is' or with 'id(.) == id(.)'.
        # We can just compare the both strings have the same content.
        # See https://stackoverflow.com/questions/20753364/why-does-creating-multiple-objects-without-naming-them-result-in-them-having-the
        assert node1 == node2
        return

    else:
        # Make sure that we're working on a copy rather than the same object
        assert node1 is not node2, "Nodes refer to the same object"

    # Continue recursive traversal of ast
    for child1, child2 in zip(node1.children, node2.children):
        _cmp_tree_types_rec(child1, child2, depth + 1)


_f90_source_test = """
module andy
implicit none

   real :: apple = 1.0
   real, parameter :: pi = 3.14

contains
   subroutine sergi()
      print *, "Pi = ", pi
      print *,  "apple = ", apple
   end subroutine

end module andy


program awesome
    use andy
    implicit none

   real :: x
   integer :: i

   x = 2.2
   i = 7

   call sergi()

   print *, "apple pie: ", apple, pi
   print *, "i: ", i

end program awesome

"""


def test_deepcopy():
    """
    Test that we can deepcopy a parsed fparser tree.
    """

    parser = ParserFactory().create(std="f2008")
    reader = FortranStringReader(_f90_source_test)
    ast = parser(reader)

    import copy

    new_ast = copy.deepcopy(ast)

    _cmp_tree_types_rec(new_ast, ast)


def test_pickle():
    """
    Test that we can pickle and unpickle a parsed fparser tree.
    """

    parser = ParserFactory().create(std="f2008")
    reader = FortranStringReader(_f90_source_test)
    ast = parser(reader)

    import pickle

    s = pickle.dumps(ast)
    new_ast = pickle.loads(s)

    _cmp_tree_types_rec(new_ast, ast)
