# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2019-2024, Science and Technology Facilities Council.
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
# Author A. Porter, A. B. G. Chalk and S. Siso, STFC Daresbury Laboratory

''' Module containing pytest tests for the handling of some select case
construction for the Fparser->PSyIR frontend.'''

import pytest

from fparser.common.readfortran import FortranStringReader
from fparser.two.Fortran2003 import (
    Assignment_Stmt, Execution_Part, Name)

from psyclone.errors import InternalError
from psyclone.psyir.frontend.fparser2 import (
    Fparser2Reader, _find_or_create_psyclone_internal_cmp)
from psyclone.psyir.nodes import (
    Schedule, CodeBlock, Assignment, BinaryOperation, IfBlock, Routine, Return,
    Container)
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

    # Test with an unresolved comparison to a literal.
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


def has_cmp_interface(code):
    ''' Utility function that asserts that the psyclone_internal_cmp
    generic interface and its 3 implementations are part of a given code.
    '''

    # Check that the generic interface in in the code
    assert '''interface psyclone_internal_cmp
  procedure psyclone_cmp_int
  procedure psyclone_cmp_logical
  procedure psyclone_cmp_char
end interface psyclone_internal_cmp''' in code

    # Check that the integer implementation is in the code
    assert '''function psyclone_cmp_int(op1, op2)
    integer, intent(in) :: op1
    integer, intent(in) :: op2
    logical :: psyclone_cmp_int

    psyclone_cmp_int = op1 == op2

  end function psyclone_cmp_int''' in code

    # Check that the char implementation is in the code
    assert '''function psyclone_cmp_char(op1, op2)
    CHARACTER(LEN = *), INTENT(IN) :: op1
    CHARACTER(LEN = *), INTENT(IN) :: op2
    logical :: psyclone_cmp_char

    psyclone_cmp_char = op1 == op2

  end function psyclone_cmp_char''' in code

    # Check that the logical implementation is in the code
    assert '''function psyclone_cmp_logical(op1, op2)
    logical, intent(in) :: op1
    logical, intent(in) :: op2
    logical :: psyclone_cmp_logical

    psyclone_cmp_logical = op1 .EQV. op2

  end function psyclone_cmp_logical''' in code


def test_find_or_create_psyclone_internal_cmp(fortran_writer):
    '''Test that the find_or_create_psyclone_internal_cmp returns the expected
    symbol and creates the interface if it does not exist. '''
    subroutine = Routine("mysub", children=[Return()])
    node_in_subroutine = subroutine.children[0]

    # If it is not inside a Container it producess a NotImplementedError
    with pytest.raises(NotImplementedError) as error:
        _ = _find_or_create_psyclone_internal_cmp(node_in_subroutine)
    assert ("Could not find the generic comparison interface and the scope "
            "does not have an ancestor container in which to add it."
            in str(error.value))

    container = Container("test", children=[subroutine])
    symbol = _find_or_create_psyclone_internal_cmp(node_in_subroutine)

    # Check that the interface and 3 additional functions have been added to
    # the container
    assert "psyclone_internal_cmp" in container.symbol_table
    assert symbol is container.symbol_table.lookup_with_tag(
            "psyclone_internal_cmp")
    assert symbol.visibility == Symbol.Visibility.PRIVATE
    assert len(container.children) == 4
    assert (container.symbol_table.lookup("psyclone_cmp_int").visibility
            == Symbol.Visibility.PRIVATE)
    assert (container.symbol_table.lookup("psyclone_cmp_logical").visibility
            == Symbol.Visibility.PRIVATE)
    assert (container.symbol_table.lookup("psyclone_cmp_char").visibility
            == Symbol.Visibility.PRIVATE)

    # Check the generated code matches the expected code
    has_cmp_interface(fortran_writer(container))

    # If called again, the same symbol is retrived and no extra code is added
    another_symbol = _find_or_create_psyclone_internal_cmp(node_in_subroutine)
    assert symbol is another_symbol
    assert len(container.children) == 4

    # Test what happens if there are name clashes for the interface and
    # supporting functions, an easy way to do it is remove the existing tag
    # so the existing symbols remain but are not considered the ones we are
    # looking for
    del container.symbol_table.tags_dict['psyclone_internal_cmp']
    another_symbol = _find_or_create_psyclone_internal_cmp(node_in_subroutine)
    assert another_symbol is not symbol
    assert another_symbol.name == "psyclone_internal_cmp_1"
    assert len(container.children) == 7  # 3 more functions added
    assert "psyclone_internal_cmp_1" in container.symbol_table

    # Check that the interface new names are internally consistent
    assert '''interface psyclone_internal_cmp_1
  procedure psyclone_cmp_int_1
  procedure psyclone_cmp_logical_1
  procedure psyclone_cmp_char_1
end interface psyclone_internal_cmp_1''' in fortran_writer(container)

    # And that from now on the tag refers to the new symbol
    assert container.symbol_table.lookup_with_tag(
            "psyclone_internal_cmp").name == "psyclone_internal_cmp_1"


def test_expression_case(fortran_reader, fortran_writer):
    '''Test that a select case statement comparing two expressions
    does not use the generic comparison interface if the types can be
    determined.

    '''
    code = '''
    module test
        contains
        subroutine test_subroutine(a, b, c)
            integer :: a, b, c

            SELECT CASE(a*a)
            CASE(b-c)
                print *, "Not hello"
            CASE(c-b)
                print *, "hello"
            END SELECT
        end subroutine
    end module test
    '''

    psyir = fortran_reader.psyir_from_source(code)
    output = fortran_writer(psyir)

    # Check that the cannonicalised comparisons do not use the interface method
    assert "if (a * a == b - c) then" in output
    assert "if (a * a == c - b) then" in output
    assert "interface psyclone_internal_cmp" not in output


def test_unresolved_types_case(fortran_reader, fortran_writer):
    '''Test that a select case statement comparing two unresolved types
    is using the generic comparison interface'''
    code = '''
    module test
        contains
        subroutine test_subroutine()
            use my_mod, only : a, b, c

            SELECT CASE(a)
            CASE(b)
                print *, "Not hello"
            CASE(c)
                print *, "hello"
            END SELECT
      end subroutine test_subroutine
    end module test
    '''

    psyir = fortran_reader.psyir_from_source(code)
    output = fortran_writer(psyir)

    # Check that the interface implementation has been inserted
    has_cmp_interface(output)

    # Check that the canonicalised comparisons use the interface method
    assert "if (psyclone_internal_cmp(a, b)) then" in output
    assert "if (psyclone_internal_cmp(a, c)) then" in output


def test_unresolved_types_case_without_module(fortran_reader):
    '''Test that a select case statement comparing two unresolved types in a
    situation wihtout an ancestor module, it will generate a CodeBlock'''
    code = '''
    subroutine test_subroutine()
        use my_mod, only : a, b, c

        SELECT CASE(a)
        CASE(b)
            print *, "Not hello"
        CASE(c)
            print *, "hello"
        END SELECT
    end subroutine test_subroutine
    '''
    psyir = fortran_reader.psyir_from_source(code)
    assert isinstance(psyir.children[0].children[0], CodeBlock)


def test_derived_types_case(fortran_reader, fortran_writer):
    '''Test that a select case statement comparing two derived types accessors
    generate the appropriate code'''

    # When the datatype information is known
    code = '''
    module test
        type :: my_type
            integer :: field
        end type
        contains
        subroutine test_subroutine()
            type(my_type) :: a
            SELECT CASE(a%field)
            CASE(1)
                print *, "Not hello"
            CASE(2)
                print *, "hello"
            END SELECT
      end subroutine test_subroutine
    end module test
    '''

    psyir = fortran_reader.psyir_from_source(code)
    output = fortran_writer(psyir)
    assert "if (a%field == 1) then" in output
    assert "if (a%field == 2) then" in output

    # And then the datatype information is unresolved
    code = '''
    module test
        contains
        subroutine test_subroutine()
            use my_mod, only : a, b, c, i

            SELECT CASE(a%b(i)%c)
            CASE(b%d)
                print *, "Not hello"
            CASE(c%a)
                print *, "hello"
            END SELECT
      end subroutine test_subroutine
    end module test
    '''

    psyir = fortran_reader.psyir_from_source(code)
    output = fortran_writer(psyir)

    # Check that the interface implementation has been inserted
    has_cmp_interface(output)

    # Check that the canonicalised comparisons use the interface method
    assert "if (psyclone_internal_cmp(a%b(i)%c, b%d)) then" in output
    assert "if (psyclone_internal_cmp(a%b(i)%c, c%a)) then" in output
