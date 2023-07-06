# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2019-2020, Science and Technology Facilities Council.
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
# Author A. Porter and A. B. G. Chalk, STFC Daresbury Laboratory

''' Module containing pytest tests for the handling of some select case
construction for the Fparser->PSyIR frontend.'''

from __future__ import absolute_import
import pytest

from fparser.common.readfortran import FortranStringReader
from fparser.two.Fortran2003 import (
    Assignment_Stmt, Execution_Part, Name)

from psyclone.errors import InternalError
from psyclone.psyir.frontend.fparser2 import Fparser2Reader
from psyclone.psyir.nodes import (
    Schedule, CodeBlock, Assignment, BinaryOperation, IfBlock)
from psyclone.psyir.symbols import (
    DataSymbol, INTEGER_TYPE, Symbol)


@pytest.mark.usefixtures("disable_declaration_check", "f2008_parser")
def test_handling_case_construct():
    ''' Test that fparser2 Case_Construct is converted to the expected PSyIR
    tree structure.

    TODO #754 fix test so that 'disable_declaration_check' fixture is not
    required.
    '''
    reader = FortranStringReader(
        '''SELECT CASE (selector)
            CASE (label1)
                branch1 = 1
            CASE (label2)
                branch2 = 1
            END SELECT''')
    fparser2case_construct = Execution_Part.match(reader)[0][0]

    fake_parent = Schedule()
    processor = Fparser2Reader()
    processor.process_nodes(fake_parent, [fparser2case_construct])

    # Check a new node was properly generated and connected to parent
    assert len(fake_parent.children) == 1
    ifnode = fake_parent.children[0]
    assert isinstance(ifnode, IfBlock)
    assert ifnode.if_body.ast is fparser2case_construct.content[2]
    assert ifnode.if_body.ast_end is fparser2case_construct.content[2]
    assert 'was_case' in ifnode.annotations
    assert ifnode.condition.children[0].name == 'selector'
    assert ifnode.condition.children[1].name == 'label1'
    assert ifnode.if_body[0].children[0].name == 'branch1'
    assert isinstance(ifnode.else_body[0], IfBlock)
    assert ifnode.else_body[0].condition.children[1].name == 'label2'
    assert ifnode.else_body[0].if_body[0].children[0].name == 'branch2'
    assert ifnode.else_body[0].ast is \
        fparser2case_construct.content[4]
    assert ifnode.else_body[0].children[1].ast is \
        fparser2case_construct.content[4]
    assert ifnode.else_body[0].children[1].ast_end is \
        fparser2case_construct.content[4]
    assert len(ifnode.else_body[0].children) == 2  # SELECT CASE ends here


@pytest.mark.usefixtures("f2008_parser")
def test_case_default():
    ''' Check that the fparser2Reader handles SELECT blocks with
    a default clause.

    '''
    case_clauses = ["CASE default\nbranch3 = 1\nbranch3 = branch3 * 2\n",
                    "CASE (label1)\nbranch1 = 1\n",
                    "CASE (label2)\nbranch2 = 1\n"]

    # Create the symbols that the frontend will expect to already be
    # present in the symbol table.
    symbols = []
    for idx in [1, 2, 3]:
        symbols.append(DataSymbol(f"branch{idx}", INTEGER_TYPE))
    for var_name in ["selector", "label1", "label2"]:
        symbols.append(DataSymbol(var_name, INTEGER_TYPE))

    # Loop over the 3 possible locations for the 'default' clause
    for idx1, idx2, idx3 in [(0, 1, 2), (1, 0, 2), (1, 2, 0)]:
        fortran_text = (
            f"SELECT CASE (selector)\n"
            f"{case_clauses[idx1]}{case_clauses[idx2]}{case_clauses[idx3]}"
            f"END SELECT\n")
        reader = FortranStringReader(fortran_text)
        fparser2case_construct = Execution_Part.match(reader)[0][0]

        fake_parent = Schedule()
        # Ensure we have the necessary symbols in the symbol table.
        for sym in symbols:
            fake_parent.symbol_table.add(sym)

        processor = Fparser2Reader()
        processor.process_nodes(fake_parent, [fparser2case_construct])
        assigns = fake_parent.walk(Assignment)
        # Check that the assignment to 'branch 3' (in the default clause) is
        # the deepest in the tree
        assert "branch3" in str(assigns[2])
        assert isinstance(assigns[2].ast, Assignment_Stmt)
        assert isinstance(assigns[2].parent, Schedule)
        assert isinstance(assigns[2].parent.ast, Assignment_Stmt)
        assert "branch3 * 2" in str(assigns[2].parent.ast_end)
        assert isinstance(assigns[2].parent.parent, IfBlock)
        # Check that the if-body of the parent IfBlock also contains
        # an Assignment
        assert isinstance(assigns[2].parent.parent.children[1], Schedule)
        assert isinstance(assigns[2].parent.parent.children[1].children[0],
                          Assignment)


@pytest.mark.usefixtures("disable_declaration_check", "f2008_parser")
def test_handling_case_list():
    ''' Test that the Case_Construct handler correctly processes CASE
    statements involving a list of conditions.

    TODO #754 fix test so that 'disable_declaration_check' fixture is not
    required.
    '''
    reader = FortranStringReader(
        '''SELECT CASE (my_var)
            CASE (label2, label3)
                branch2 = 1
            END SELECT''')
    fparser2case_construct = Execution_Part.match(reader)[0][0]

    fake_parent = Schedule()
    processor = Fparser2Reader()
    processor.process_nodes(fake_parent, [fparser2case_construct])
    assert len(fake_parent.children) == 1
    ifnode = fake_parent.children[0]
    assert isinstance(ifnode, IfBlock)
    assert isinstance(ifnode.condition, BinaryOperation)
    assert ifnode.condition.operator == BinaryOperation.Operator.OR
    eqnode = ifnode.condition.children[0]
    assert eqnode.operator == BinaryOperation.Operator.EQ
    assert "my_var" in str(eqnode.children[0])
    assert "label2" in str(eqnode.children[1])
    eqnode = ifnode.children[0].children[1]
    assert eqnode.operator == BinaryOperation.Operator.EQ
    assert "my_var" in str(eqnode.children[0])
    assert "label3" in str(eqnode.children[1])

    assert "Reference[name:'branch2']" in str(ifnode.if_body[0].lhs)


@pytest.mark.usefixtures("disable_declaration_check", "f2008_parser")
def test_handling_case_range():
    ''' Test that the Case_Construct handler correctly processes CASE
    statements involving a range.

    TODO #754 fix test so that 'disable_declaration_check' fixture is not
    required.
    '''
    reader = FortranStringReader(
        '''SELECT CASE (my_var)
            CASE (label4:label5)
                branch3 = 1
            END SELECT''')
    fparser2case_construct = Execution_Part.match(reader)[0][0]

    fake_parent = Schedule()
    processor = Fparser2Reader()
    processor.process_nodes(fake_parent, [fparser2case_construct])
    assert len(fake_parent.children) == 1
    ifnode = fake_parent.children[0]
    assert isinstance(ifnode, IfBlock)
    assert isinstance(ifnode.children[0], BinaryOperation)
    assert ifnode.condition.operator == BinaryOperation.Operator.AND
    assert ifnode.condition.children[0].operator == BinaryOperation.Operator.GE
    assert ifnode.condition.children[1].operator == BinaryOperation.Operator.LE
    assert "branch3" in str(ifnode.if_body[0].lhs)


@pytest.mark.usefixtures("disable_declaration_check", "f2008_parser")
def test_handling_case_range_list():
    ''' Test that the Case_Construct handler correctly processes CASE
    statements involving a list of ranges.

    TODO #754 fix test so that 'disable_declaration_check' fixture is not
    required.
    '''
    reader = FortranStringReader(
        '''SELECT CASE (my_var)
            CASE (:label1, label5:, label6)
                branch4 = 1
            END SELECT''')
    # We should end up with:
    #    my_var <= label1 OR my_var >= label5 OR my_var == label6
    fparser2case_construct = Execution_Part.match(reader)[0][0]

    fake_parent = Schedule()
    processor = Fparser2Reader()
    processor.process_nodes(fake_parent, [fparser2case_construct])
    assert len(fake_parent.children) == 1
    ifnode = fake_parent.children[0]
    assert isinstance(ifnode, IfBlock)
    assert isinstance(ifnode.condition, BinaryOperation)
    assert ifnode.condition.operator == BinaryOperation.Operator.OR
    assert ifnode.condition.children[0].operator == BinaryOperation.Operator.LE
    assert "label1" in str(ifnode.condition.children[0].children[1])
    orop = ifnode.condition.children[1]
    assert orop.operator == BinaryOperation.Operator.OR
    assert orop.children[0].operator == BinaryOperation.Operator.GE
    assert "label5" in str(orop.children[0].children[1])
    assert orop.children[1].operator == BinaryOperation.Operator.EQ
    assert "label6" in str(orop.children[1].children[1])
    assert "branch4" in str(ifnode.if_body[0].lhs)


@pytest.mark.usefixtures("parser")
def test_handling_labelled_case_construct():
    ''' Test that a labelled case construct results in a CodeBlock. '''
    reader = FortranStringReader(
        '''999 SELECT CASE (selector)
            CASE (pick_me)
                branch3 = 1
            END SELECT''')
    fparser2case_construct = Execution_Part.match(reader)[0][0]

    fake_parent = Schedule()
    fake_parent.symbol_table.new_symbol("selector", symbol_type=DataSymbol,
                                        datatype=INTEGER_TYPE)
    fake_parent.symbol_table.new_symbol("pick_me", symbol_type=DataSymbol,
                                        datatype=INTEGER_TYPE)
    fake_parent.symbol_table.new_symbol("branch3", symbol_type=DataSymbol,
                                        datatype=INTEGER_TYPE)
    processor = Fparser2Reader()
    processor.process_nodes(fake_parent, [fparser2case_construct])
    assert len(fake_parent.children) == 1
    assert isinstance(fake_parent.children[0], CodeBlock)


@pytest.mark.usefixtures("f2008_parser")
def test_case_default_only():
    ''' Check that we handle a select case that contains only a
    default clause and is thus redundant. The PSyIR should represent
    only the code that is within the default case.

    '''
    fake_parent = Schedule()
    fake_parent.symbol_table.add(Symbol("a"))
    processor = Fparser2Reader()
    reader = FortranStringReader(
        '''SELECT CASE ( jprstlib )
           CASE DEFAULT
             WRITE(numout,*) 'open ice restart NetCDF file: '
             a = 1
           END SELECT''')
    exe_part = Execution_Part.match(reader)
    processor.process_nodes(fake_parent, exe_part[0])
    # We should have no IfBlock in the resulting PSyIR
    assert len(fake_parent.children) == 2
    assert isinstance(fake_parent.children[0], CodeBlock)
    assert isinstance(fake_parent.children[1], Assignment)


@pytest.mark.usefixtures("disable_declaration_check", "f2008_parser")
def test_handling_invalid_case_construct():
    ''' Test that the Case_Construct handler raises the proper errors when
    it parses invalid or unsupported fparser2 trees.

    TODO #754 fix test so that 'disable_declaration_check' fixture is not
    required.
    '''
    # CASE (default) is just a regular symbol named default
    reader = FortranStringReader(
        '''SELECT CASE (selector)
            CASE (default)
                branch3 = 1
            END SELECT''')
    fparser2case_construct = Execution_Part.match(reader)[0][0]

    fake_parent = Schedule()
    processor = Fparser2Reader()
    processor.process_nodes(fake_parent, [fparser2case_construct])
    assert isinstance(fake_parent.children[0], IfBlock)

    # Test with no opening Select_Case_Stmt
    reader = FortranStringReader(
        '''SELECT CASE (selector)
            CASE (label1)
                branch1 = 1
            CASE (label2)
                branch2 = 1
            END SELECT''')
    fparser2case_construct = Execution_Part.match(reader)[0][0]
    del fparser2case_construct.content[0]
    with pytest.raises(InternalError) as error:
        processor.process_nodes(fake_parent, [fparser2case_construct])
    assert "Failed to find opening case statement in:" in str(error.value)

    # Test with no closing End_Select_Stmt
    reader = FortranStringReader(
        '''SELECT CASE (selector)
            CASE (label1)
                branch1 = 1
            CASE (label2)
                branch2 = 1
            END SELECT''')
    fparser2case_construct = Execution_Part.match(reader)[0][0]
    del fparser2case_construct.content[-1]
    with pytest.raises(InternalError) as error:
        processor.process_nodes(fake_parent, [fparser2case_construct])
    assert "Failed to find closing case statement in:" in str(error.value)

    # Test when one clause is not of the expected type
    reader = FortranStringReader(
        '''SELECT CASE (selector)
            CASE (label1)
                branch1 = 1
            CASE (label2)
                branch2 = 1
            END SELECT''')
    fparser2case_construct = Execution_Part.match(reader)[0][0]
    fparser2case_construct.content[1].items = (Name("Fake"), None)
    with pytest.raises(InternalError) as error:
        processor.process_nodes(fake_parent, [fparser2case_construct])
    assert "to be a Case_Selector but got" in str(error.value)


def test_logical_literal_case(fortran_reader, fortran_writer):
    '''Test that a select case statement comparing to logical literals
    results in if statements with .eqv. checks.'''
    code = '''subroutine test_subroutine(a)
    logical :: a

    SELECT CASE(A)
    CASE(.FALSE.)
        print *, "Not hello"
    CASE(.TRUE.)
        print *, "hello"
    END SELECT
  end subroutine'''

    psyir = fortran_reader.psyir_from_source(code)
    pass_through = fortran_writer(psyir)
    assert "a == .true." not in pass_through
    assert "a .EQV. .true." in pass_through

    # Test with an unknown comparison to a literal.
    code = '''subroutine test_subroutine()
    use mymod, only: a

    SELECT CASE(A)
    CASE(.FALSE.)
        print *, "Not hello"
    CASE(.TRUE.)
        print *, "hello"
    END SELECT
  end subroutine'''

    psyir = fortran_reader.psyir_from_source(code)
    pass_through = fortran_writer(psyir)
    assert "a == .true." not in pass_through
    assert "a .EQV. .true." in pass_through


def test_logical_reference_case(fortran_reader, fortran_writer):
    '''Test that a select case statement comparing to logical references
    results in if statements with .eqv. checks.'''
    code = '''subroutine test_subroutine(a, b, c)
    logical :: a, b, c

    SELECT CASE(A)
    CASE(b)
        print *, "Not hello"
    CASE(c)
        print *, "hello"
    END SELECT
  end subroutine'''

    psyir = fortran_reader.psyir_from_source(code)
    pass_through = fortran_writer(psyir)
    assert "a == b" not in pass_through
    assert "a .EQV. b" in pass_through
    assert "a .EQV. c" in pass_through


def test_nonlogical_literal_case(fortran_reader, fortran_writer):
    '''Test that a select case statement comparing to non-logical literals
    results in if statements with == checks.'''
    code = '''subroutine test_subroutine(a)
    integer :: a

    SELECT CASE(A)
    CASE(1)
        print *, "Not hello"
    CASE(2)
        print *, "hello"
    END SELECT
  end subroutine'''

    psyir = fortran_reader.psyir_from_source(code)
    pass_through = fortran_writer(psyir)
    assert "a == 1" in pass_through
    assert "a == 2" in pass_through


def test_nonlogical_reference_case(fortran_reader, fortran_writer):
    '''Test that a select case statement comparing to non-logical references
    results in if statements with == checks.'''
    code = '''subroutine test_subroutine(a, b, c)
    integer :: a, b, c

    SELECT CASE(A)
    CASE(b)
        print *, "Not hello"
    CASE(c)
        print *, "hello"
    END SELECT
  end subroutine'''

    psyir = fortran_reader.psyir_from_source(code)
    pass_through = fortran_writer(psyir)
    assert "a == b" in pass_through
    assert "a == c" in pass_through


def test_expression_case(fortran_reader):
    '''Test that a select case statement comparing two expressions
    results in a code block.'''
    code = '''subroutine test_subroutine(a, b, c)
    integer :: a, b, c

    SELECT CASE(a*a)
    CASE(b-c)
        print *, "Not hello"
    CASE(c-b)
        print *, "hello"
    END SELECT
  end subroutine'''

    psyir = fortran_reader.psyir_from_source(code)
    assert isinstance(psyir.children[0].children[0], CodeBlock)


def test_unknown_types_case(fortran_reader):
    '''Test that a select case statement comparing two unknown types
    results in a code block.'''
    code = '''subroutine test_subroutine()
    use my_mod, only : a, b, c

    SELECT CASE(a)
    CASE(b)
        print *, "Not hello"
    CASE(c)
        print *, "hello"
    END SELECT
  end subroutine'''

    psyir = fortran_reader.psyir_from_source(code)
    assert isinstance(psyir.children[0].children[0], CodeBlock)
