# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2021-2025, Science and Technology Facilities Council.
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
# Author: S. Siso, STFC Daresbury Lab
# Modifications: R. W. Ford and A. R. Porter, STFC Daresbury Lab
# -----------------------------------------------------------------------------

''' Performs py.test tests on the Fortran PSyIR front-end '''

import pytest
from fparser.two import Fortran2003
from psyclone.psyir.frontend.fortran import FortranReader
from psyclone.psyir.frontend.fparser2 import Fparser2Reader
from psyclone.psyir.nodes import (
    Routine, FileContainer, UnaryOperation, BinaryOperation, Literal,
    Assignment, CodeBlock, IntrinsicCall, Loop)
from psyclone.psyir.commentable_mixin import CommentableMixin
from psyclone.psyir.symbols import (
    SymbolTable, DataSymbol, ScalarType, UnresolvedType)


# The 'contiguous' keyword is just valid with Fortran 2008
ONLY_2008_CODE = '''
subroutine sub(a)
    integer, dimension(:), contiguous, intent(inout) :: a
end subroutine sub
'''

CODE = '''
subroutine sub(a)
    integer, dimension(:), intent(inout) :: a
    a = a + 1
end subroutine sub
'''

FIXED_FORM_CODE = '''
      subroutine insert_node (lstr, node, nnodes, ierr)
      integer*4 lstr, ierr, i,j,k, lsffx, digits, power, ndots, idot(3)
     &                                        , node,  nnodes
      end
'''

CODE_WITH_COMMENTS_AND_DIRECTIVES = '''
subroutine my_sub
    integer :: a, b

    ! Comment on do loop
    !$omp parallel do
    do a = 1, 10
        ! Comment on assignment
        b = a
    end do
    !$omp end parallel do
end subroutine my_sub
'''


def test_fortran_reader_constructor():
    ''' Test that the constructor initialises the _parser and _processor
    attributes. '''
    freader = FortranReader()
    assert freader._parser is Fortran2003.Program
    assert isinstance(freader._processor, Fparser2Reader)

    # Check that the initialised parser can parse Fortran 2008 standard,
    # the return value of this function is tested in the following tests
    freader.psyir_from_source(ONLY_2008_CODE)


def test_fortran_psyir_from_source(fortran_reader):
    ''' Test that the psyir_from_source method parses to PSyIR
    the specified source code. '''
    file_container = fortran_reader.psyir_from_source(CODE)
    assert isinstance(file_container, FileContainer)
    subroutine = file_container.children[0]
    assert isinstance(subroutine, Routine)


def test_fortran_psyir_from_source_fixed_form():
    '''
    Test we parse also fixed-form fortran code when enabling the right
    option.
    '''
    fortran_reader = FortranReader(free_form=False)
    file_container = fortran_reader.psyir_from_source(FIXED_FORM_CODE)
    assert isinstance(file_container, FileContainer)
    subroutine = file_container.children[0]
    assert isinstance(subroutine, Routine)


def test_fortran_psyir_from_source_invalid_cpp_directives(fortran_reader):
    '''
    Test that we cleanly catch and report the problem when the provided
    source contains CPP directives which result in fparser failing.

    '''
    code = '''\
    program problems
      integer i, n_tracers, nmodes
      logical :: mode_names(20)
    DO i=1,n_tracers
    #if defined(NAG_FORTRAN) && (NAG_FORTRAN == 7000000)
    tracer_belongs = .FALSE.
    DO j=1,nmodes
      IF ( mode_names(j) ) THEN
        tracer_belongs = .TRUE.
      END IF
    END DO
    IF ( tracer_belongs ) THEN
    #else
    IF ( ANY(mode_names(:) == all_tracers_names(i)(1:7)) ) THEN
    #endif
    END IF
    END DO
    end program problems
    '''
    with pytest.raises(ValueError) as err:
        _ = fortran_reader.psyir_from_source(code)
    assert ("Is the input valid Fortran (note that CPP directives must be "
            "handled by a pre-processor)?" in str(err.value))


def test_fortran_psyir_from_expression(fortran_reader):
    ''' Test that the psyir_from_expression method generates the
    expected PSyIR. '''
    sched = Routine.create("malachi")
    table = sched.symbol_table
    psyir = fortran_reader.psyir_from_expression("3.0", table)
    assert isinstance(psyir, Literal)
    assert psyir.value == "3.0"

    psyir = fortran_reader.psyir_from_expression("-3.0 + 1.0", table)
    assert isinstance(psyir, BinaryOperation)
    assert psyir.operator == BinaryOperation.Operator.ADD
    assert isinstance(psyir.children[0], UnaryOperation)
    assert psyir.children[0].operator == UnaryOperation.Operator.MINUS
    assert isinstance(psyir.children[0].children[0], Literal)
    assert psyir.children[0].children[0].value == "3.0"

    psyir = fortran_reader.psyir_from_expression("ABS(-3.0)", table)
    assert isinstance(psyir, IntrinsicCall)
    assert psyir.intrinsic == IntrinsicCall.Intrinsic.ABS
    assert isinstance(psyir.arguments[0], UnaryOperation)
    assert psyir.arguments[0].operator == UnaryOperation.Operator.MINUS
    assert isinstance(psyir.arguments[0].children[0], Literal)

    psyir = fortran_reader.psyir_from_expression("3.0_r_def", table)
    assert isinstance(psyir, Literal)
    assert isinstance(psyir.datatype, ScalarType)
    assert isinstance(psyir.datatype.precision, DataSymbol)
    symbol = table.lookup("r_def")
    assert isinstance(symbol, DataSymbol)
    assert psyir.datatype.precision is symbol

    psyir = fortran_reader.psyir_from_expression("3.0 + a", table)
    assert isinstance(psyir, BinaryOperation)
    a_symbol = psyir.children[1].symbol
    a_symbol_table = table.lookup("a")
    assert a_symbol is a_symbol_table


def test_fortran_psyir_from_expression_invalid(fortran_reader):
    ''' Test that the psyir_from_expression method raises the expected
    error when given something that is not an expression. '''
    # No supplied table
    with pytest.raises(TypeError) as err:
        fortran_reader.psyir_from_expression("3.0 + sin(a)", 3)
    assert ("Must be supplied with a valid SymbolTable but got 'int'" in
            str(err.value))
    with pytest.raises(TypeError) as err:
        fortran_reader.psyir_from_expression("3.0 + sin(a)", "wrong")
    assert ("Must be supplied with a valid SymbolTable but got 'str'" in
            str(err.value))
    # OK
    fortran_reader.psyir_from_expression("return")
    with pytest.raises(ValueError) as err:
        fortran_reader.psyir_from_expression("a = b")
    assert "not represent a Fortran expression: 'a = b'" in str(err.value)
    with pytest.raises(ValueError) as err:
        fortran_reader.psyir_from_expression("if(3 == 2)then")
    assert ("not represent a Fortran expression: 'if(3 == 2)then'" in
            str(err.value))
    with pytest.raises(ValueError) as err:
        fortran_reader.psyir_from_expression("this is not Fortran")
    assert ("not represent a Fortran expression: 'this is not Fortran'" in
            str(err.value))


def test_psyir_from_statement(fortran_reader):
    ''' Check the correct operation of the psyir_from_statement() method. '''
    table = SymbolTable()
    table.new_symbol("a", symbol_type=DataSymbol, datatype=UnresolvedType())
    table.new_symbol("b", symbol_type=DataSymbol, datatype=UnresolvedType())
    psyir = fortran_reader.psyir_from_statement("a=b", table)
    assert isinstance(psyir, Assignment)
    psyir = fortran_reader.psyir_from_statement("write(*,*) a", table.detach())
    assert isinstance(psyir, CodeBlock)
    assert psyir.structure == CodeBlock.Structure.STATEMENT


def test_psyir_from_statement_invalid(fortran_reader):
    ''' Test that the psyir_from_statement method raises the expected error
    when given something that is not a statement. '''
    with pytest.raises(TypeError) as err:
        fortran_reader.psyir_from_statement("blah", 3)
    assert ("Must be supplied with a valid SymbolTable but got 'int'"
            in str(err.value))
    with pytest.raises(ValueError) as err:
        fortran_reader.psyir_from_statement("blah")
    assert ("Supplied source does not represent a Fortran statement: 'blah'"
            in str(err.value))
    # OK
    fortran_reader.psyir_from_statement("a=b")


def test_fortran_psyir_from_file(fortran_reader, tmpdir_factory):
    ''' Test that the psyir_from_file method reads and parses to PSyIR
    the specified file. '''
    filename = str(tmpdir_factory.mktemp('frontend_test').join("testfile.f90"))
    with open(filename, "w", encoding='utf-8') as wfile:
        wfile.write(CODE)

    # Check with a proper file
    file_container = fortran_reader.psyir_from_file(filename)
    assert isinstance(file_container, FileContainer)
    subroutine = file_container.children[0]
    assert isinstance(subroutine, Routine)

    # Check with an empty file
    filename = str(tmpdir_factory.mktemp('frontend_test').join("empty.f90"))
    with open(filename, "w", encoding='utf-8') as wfile:
        wfile.write("")
    file_container = fortran_reader.psyir_from_file(filename)
    assert isinstance(file_container, FileContainer)
    assert file_container.name == "empty.f90"

    # Check with a file containing invalid Fortran
    filename = str(tmpdir_factory.mktemp('frontend_test').join("wrong.f90"))
    with open(filename, "w", encoding='utf-8') as wfile:
        wfile.write("this is not Fortran")
    with pytest.raises(ValueError) as err:
        file_container = fortran_reader.psyir_from_file(filename)
    assert "Failed to parse source in file" in str(err.value)

    # Check with a file that doesn't exist
    filename = str(tmpdir_factory.mktemp('frontend_test').join("Idontexist"))
    with pytest.raises(IOError) as err:
        fortran_reader.psyir_from_file(filename)
    assert "No such file or directory: '" + str(filename) in str(err.value)

    # Check that directives and comments are ignored by default
    filename = str(tmpdir_factory.mktemp('frontend_test').join("comments.f90"))
    with open(filename, "w", encoding='utf-8') as wfile:
        wfile.write(CODE_WITH_COMMENTS_AND_DIRECTIVES)
    file_container = fortran_reader.psyir_from_file(filename)
    assert isinstance(file_container, FileContainer)
    for node in file_container.walk(CommentableMixin):
        assert node.preceding_comment == ""
        assert node.inline_comment == ""

    # Check that comments can be preserved, and that directives are still
    # ignored by default
    fortran_reader = FortranReader(ignore_comments=False)
    file_container = fortran_reader.psyir_from_file(filename)
    assert isinstance(file_container, FileContainer)
    for node in file_container.walk(CommentableMixin):
        if isinstance(node, Loop):
            assert node.preceding_comment == "Comment on do loop"
        elif isinstance(node, Assignment):
            assert node.preceding_comment == "Comment on assignment"
        else:
            assert node.preceding_comment == ""

    # Check that directives can be preserved
    fortran_reader = FortranReader(ignore_comments=False,
                                   ignore_directives=False)
    file_container = fortran_reader.psyir_from_file(filename)
    assert isinstance(file_container, FileContainer)
    assignment = file_container.walk(Assignment)[0]
    assert assignment.preceding_comment == "Comment on assignment"
    # When keeping directives a comment before the directive
    # goes on the directive.
    par_direc = file_container.walk(CodeBlock)[0]
    assert par_direc.preceding_comment == "Comment on do loop"
    for node in file_container.walk(CommentableMixin):
        if node not in (assignment, par_direc):
            assert node.preceding_comment == ""

    # Check that the following combination raises an error
    with pytest.raises(ValueError) as err:
        FortranReader(ignore_comments=True, ignore_directives=False)
    msg = (
        "Setting ignore_directives to False in the FortranReader will"
        " only have an effect if ignore_comments is also set to False."
    )
    assert msg in str(err.value)
