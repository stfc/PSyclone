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
# Author: J. Henrichs, Bureau of Meteorology
# Modified: R. W. Ford, STFC Daresbury Lab


''' Module containing py.test tests for the symbolic maths class.'''

import pytest
from sympy import solvers, Symbol

from psyclone.core.symbolic_maths import SymbolicMaths
from psyclone.psyir.backend.sympy_writer import SymPyWriter
from psyclone.psyir.nodes import Assignment


def test_sym_maths_get():
    '''Makes sure that the getter works as expected, especially
    that sympy can be imported.'''

    sym_maths = SymbolicMaths.get()
    assert sym_maths is not None

    # Make sure we get the indeed the same instance:
    sym_maths2 = SymbolicMaths.get()
    assert sym_maths is sym_maths2

    assert sym_maths.equal(None, 1) is False
    assert sym_maths.equal(2, None) is False


@pytest.mark.parametrize("expressions", [(".true.", ".TRUE."),
                                         (".false.", ".FALSE."),
                                         ])
def test_math_logicals(fortran_reader, expressions):
    '''Test that the sympy based comparison handles logical constants
    as expected.
    '''
    # A dummy program to easily create the PSyIR for the
    # expressions we need. We just take the RHS of the assignments
    source = f'''program test_prog
                logical :: x
                x = {expressions[0]}
                x = {expressions[1]}
                end program test_prog
                '''
    psyir = fortran_reader.psyir_from_source(source)
    schedule = psyir.children[0]

    sym_maths = SymbolicMaths.get()
    assert sym_maths.equal(schedule[0].rhs, schedule[1].rhs) is True


@pytest.mark.parametrize("expressions", [("i", "i"),
                                         ("2", "1+1"),
                                         ("123_4", "123_8"),
                                         ("123_4", "120+3"),
                                         ("123_xx", "123"),
                                         ("1.23E5", "123000"),
                                         ("1.23D5", "123000"),
                                         ("1.0E+3", "1000"),
                                         ("1.0", "1"),
                                         ("0.01E-3", "0.00001"),
                                         ("3.14e-2", "0.0314"),
                                         ("2.0", "1.1+0.9"),
                                         ("2", "1+7*i-3-4*i-3*i+4"),
                                         ("i+j", "j+i"),
                                         ("i+j+k", "i+k+j"),
                                         ("i+i", "2*i"),
                                         ("i+j-2*k+3*j-2*i", "-i+4*j-2*k")
                                         ])
def test_symbolic_math_equal(fortran_reader, expressions):
    '''Test that the sympy based comparison handles complex
    expressions that are equal.

    '''
    # A dummy program to easily create the PSyIR for the
    # expressions we need. We just take the RHS of the assignments
    source = f'''program test_prog
                use some_mod
                integer :: i, j, k, x
                type(my_mod_type) :: a, b
                x = {expressions[0]}
                x = {expressions[1]}
                end program test_prog
                '''
    psyir = fortran_reader.psyir_from_source(source)
    schedule = psyir.children[0]

    sym_maths = SymbolicMaths.get()
    assert sym_maths.equal(schedule[0].rhs, schedule[1].rhs) is True


@pytest.mark.parametrize("expressions", [("a%b", "a%b"),
                                         ("a%b(i)", "a%b(i)"),
                                         ("a%b(2*i)", "a%b(3*i-i)"),
                                         ("a%b(i-1)%c(j+1)",
                                          "a%b(-1+i)%c(1+j)"),
                                         ("c(i,j)%b(i,j)", "c(i,j)%b(i,j)"),
                                         ("c(i+k,j-1-2*j)%b(2*i-i,j+3*k)",
                                          "c(k+i,-1-j)%b(i,3*k+j)"),
                                         ("a%b%c%d", "a%b%c%d")
                                         ])
def test_symbolic_math_equal_structures(fortran_reader, expressions):
    '''Test that the sympy based comparison handles structures as expected.

    '''
    # A dummy program to easily create the PSyIR for the
    # expressions we need. We just take the RHS of the assignments
    source = f'''program test_prog
                use some_mod
                integer :: i, j, k
                type(my_mod_type) :: a, b, c(:,:)
                x = {expressions[0]}
                x = {expressions[1]}
                end program test_prog
                '''
    psyir = fortran_reader.psyir_from_source(source)
    schedule = psyir.children[0]

    sym_maths = SymbolicMaths.get()
    assert sym_maths.equal(schedule[0].rhs, schedule[1].rhs) is True


@pytest.mark.parametrize("expressions", [("i", "0"),
                                         ("i", "j"),
                                         ("2", "1+1-1"),
                                         ("i+j", "j+i+1"),
                                         ("i-j", "j-i"),
                                         ("max(1, 2)", "max(1, 2, 3)")
                                         ])
def test_symbolic_math_not_equal(fortran_reader, expressions):
    '''Test that the sympy based comparison handles complex
    expressions that are not equal.

    '''
    # A dummy program to easily create the PSyIR for the
    # expressions we need. We just take the RHS of the assignments
    source = f'''program test_prog
                use some_mod
                integer :: i, j, k, x
                type(my_mod_type) :: a, b
                x = {expressions[0]}
                x = {expressions[1]}
                end program test_prog
                '''
    psyir = fortran_reader.psyir_from_source(source)
    schedule = psyir.children[0]

    sym_maths = SymbolicMaths.get()
    assert sym_maths.equal(schedule[0].rhs, schedule[1].rhs) is False


@pytest.mark.parametrize("expressions", [("a%b", "a%c"),
                                         ("a%b(i)", "a%b(i+1)"),
                                         ("a%b(i)%c(k)", "a%b(i+1)%c(k)"),
                                         ("a%b(i)%c(k)", "a%b(i)%c(k+1)"),
                                         ("a%b(i+1)%c(k)", "a%b(i)%c(k+1)"),
                                         ])
def test_symbolic_math_not_equal_structures(fortran_reader, expressions):
    '''Test that the sympy based comparison handles complex
    expressions that are not equal.

    '''
    # A dummy program to easily create the PSyIR for the
    # expressions we need. We just take the RHS of the assignments
    source = f'''program test_prog
                use some_mod
                integer :: i, j, k, x
                type(my_mod_type) :: a, b
                x = {expressions[0]}
                x = {expressions[1]}
                end program test_prog
                '''
    psyir = fortran_reader.psyir_from_source(source)
    schedule = psyir.children[0]

    sym_maths = SymbolicMaths.get()

    assert sym_maths.equal(schedule[0].rhs, schedule[1].rhs) is False


@pytest.mark.parametrize("exp1, exp2, result", [("i", "0", False),
                                                ("i", "j", False),
                                                ("2", "1+1-1", True),
                                                ("2", "1+1", False),
                                                ("i", "i+1", True),
                                                ("i+j", "j+i+1", True),
                                                ("i-j", "j-i", False),
                                                ("max(1, 2)",
                                                 "max(1, 2, 3)", True),
                                                ("a(:)", "b(:)", False),
                                                ])
def test_symbolic_math_never_equal(fortran_reader, exp1, exp2, result):
    '''Test that the sympy based comparison handles complex
    expressions that are tested for never equal.

    '''
    # A dummy program to easily create the PSyIR for the
    # expressions we need. We just take the RHS of the assignments
    source = f'''program test_prog
                use some_mod
                integer :: i, j, k, x
                type(my_mod_type) :: a, b
                x = {exp1}
                x = {exp2}
                end program test_prog
                '''
    psyir = fortran_reader.psyir_from_source(source)
    schedule = psyir.children[0]

    sym_maths = SymbolicMaths.get()
    assert sym_maths.never_equal(schedule[0].rhs, schedule[1].rhs) is result


def test_symbolic_maths_never_equal_error(fortran_reader):
    '''Test the never_equal method with an invalid SymPy expression, to make
    sure it hides any exception. We use an array assignment using (/ ... /),
    which is not valid in SymPy.'''
    source = (
        "program test_prog\n"
        "  integer :: a(2)\n"
        "  a(:) = (/1, 2/)\n"
        "end program test_prog\n")
    psyir = fortran_reader.psyir_from_source(source)
    assignment = psyir.children[0][0]
    sym_maths = SymbolicMaths.get()
    assert sym_maths.never_equal(assignment.lhs, assignment.rhs) is False


@pytest.mark.parametrize("exp1, exp2, positive, result",
                         [("i", "j", False, SymbolicMaths.Fuzzy.MAYBE),
                          ("i+1", "i", False, SymbolicMaths.Fuzzy.TRUE),
                          ("i+j", "i", False, SymbolicMaths.Fuzzy.MAYBE),
                          ("i+j", "i", True, SymbolicMaths.Fuzzy.TRUE),
                          ("2*i", "i", True, SymbolicMaths.Fuzzy.TRUE),
                          ("i", "2*i", True, SymbolicMaths.Fuzzy.FALSE),
                          ("i", "i+1", False, SymbolicMaths.Fuzzy.FALSE)])
def test_symbolic_maths_greater_than(
        fortran_reader, exp1, exp2, positive, result):
    '''
    Tests for the greater_than() method.
    '''
    sym_maths = SymbolicMaths.get()
    ir1 = fortran_reader.psyir_from_expression(exp1)
    ir2 = fortran_reader.psyir_from_expression(exp2)
    assert sym_maths.greater_than(ir1, ir2,
                                  all_variables_positive=positive) == result


@pytest.mark.parametrize("exp1, exp2, positive, result",
                         [("i", "j", False, SymbolicMaths.Fuzzy.MAYBE),
                          ("i+1", "i", False, SymbolicMaths.Fuzzy.FALSE),
                          ("i+j", "i", False, SymbolicMaths.Fuzzy.MAYBE),
                          ("i+j", "i", True, SymbolicMaths.Fuzzy.FALSE),
                          ("2*i", "i", True, SymbolicMaths.Fuzzy.FALSE),
                          ("i", "2*i", True, SymbolicMaths.Fuzzy.TRUE),
                          ("i", "i+1", False, SymbolicMaths.Fuzzy.TRUE)])
def test_symbolic_maths_less_than(
        fortran_reader, exp1, exp2, positive, result):
    '''
    Tests for the greater_than() method.
    '''
    sym_maths = SymbolicMaths.get()
    ir1 = fortran_reader.psyir_from_expression(exp1)
    ir2 = fortran_reader.psyir_from_expression(exp2)
    assert sym_maths.less_than(ir1, ir2,
                               all_variables_positive=positive) == result


@pytest.mark.parametrize("exp1, exp2, result", [("i", "2*i+1", set([-1])),
                                                # Infinite solutions (i is any
                                                # integer) are returned as
                                                # string "independent"
                                                ("i", "i", "independent"),
                                                # Indirect addressing cannot be
                                                # resolved, sympy returns a
                                                # ConditionSet, which must be
                                                # returned as 'independent'
                                                ("ind(i)", "ind(i+1)",
                                                 "independent"),
                                                # This returns a SymPy Image
                                                # object:
                                                ("EXP(i)", "1",
                                                 "independent"),
                                                # This returns a SymPy Union
                                                ("i*(exp(i)-i)", "0",
                                                 "independent"),
                                                ("i*i", "2*i-1", set([1])),
                                                ("i*i", "4", set([2, -2])),
                                                ("2*i", "2*i+1", set()),
                                                ])
def test_symbolic_math_solve(fortran_reader, exp1, exp2, result):
    '''Test that the sympy based comparison handles complex
    expressions that are not equal.

    '''
    # A dummy program to easily create the PSyIR for the
    # expressions we need. We just take the RHS of the assignments
    source = f'''program test_prog
                use some_mod
                integer :: i, j, k, x, ind(10)
                type(my_mod_type) :: a, b
                x = {exp1}
                x = {exp2}
                end program test_prog
                '''
    psyir = fortran_reader.psyir_from_source(source)
    schedule = psyir.children[0]

    sym_maths = SymbolicMaths.get()
    writer = SymPyWriter()
    sympy_expressions = writer([schedule[0].rhs, schedule[1].rhs])
    symbol_map = writer.type_map
    # Get the symbol used for 'i', so we can solve for 'i'
    i = symbol_map["i"]
    solution = sym_maths.solve_equal_for(sympy_expressions[0],
                                         sympy_expressions[1], i)
    assert solution == result


def test_solve_equal_for_error(monkeypatch):
    '''Test that an unexpected SymPy result type raises the expected error. '''

    sym_maths = SymbolicMaths.get()
    # Monkeypatch SymPy's solveset to return a plain Python integer:
    monkeypatch.setattr(solvers, "solveset", lambda _x, _y: 1)
    x_sym = Symbol("X")
    with pytest.raises(ValueError) as err:
        sym_maths.solve_equal_for(x_sym, x_sym, x_sym)
    assert "Unexpected solution '1'' of type '<class 'int'>'" in str(err.value)


@pytest.mark.parametrize("expressions", [("max(3, 2, 1)", "max(1, 2, 3)"),
                                         ("max(1, 3)", "3"),
                                         ("max(1, 3)", "max(1, 2, 3)"),
                                         ("min(3, 2, 1)", "min(1, 2, 3)"),
                                         ("min(1, 3)", "min(1, 2, 3)"),
                                         ("min(1, 2, 3)", "1"),
                                         ("MOD(7,2)", "1"),
                                         ("MOD(i,j)", "mod(2+i-2, j)"),
                                         ("FLOOR(1.1)", "1"),
                                         ("FLOOR(-1.1)", "-2")
                                         ])
def test_symbolic_math_functions_with_constants(fortran_reader, expressions):
    '''Test that recognised functions with constant values as arguments are
    handled correctly."

    '''
    # A dummy program to easily create the PSyIR for the
    # expressions we need. We just take the RHS of the assignments
    source = f'''program test_prog
                 use some_mod
                 integer :: i, j, k, x
                 type(my_mod_type) :: a, b
                 x = {expressions[0]}
                 x = {expressions[1]}
                 end program test_prog
             '''

    psyir = fortran_reader.psyir_from_source(source)
    schedule = psyir.children[0]
    sym_maths = SymbolicMaths.get()
    assert sym_maths.equal(schedule[0].rhs, schedule[1].rhs) is True


@pytest.mark.parametrize("expressions", [("field(1+i)", "field(i+1)"),
                                         ("lambda", "lambda"),
                                         ("lambda(1+i)", "lambda(i+1)"),
                                         ("a%field(b+1)", "a%field(1+b)"),
                                         ("a%b%c(a_b+1)", "a%b%c(1+a_b)"),
                                         ("a%field(field+1)",
                                          "a%field(1+field)"),
                                         ("b+a%b(a%c,a%c,a%c)",
                                          "b+a%b(a%c,a%c,a%c)")
                                         ])
def test_symbolic_math_use_reserved_names(fortran_reader, expressions):
    '''Test that reserved names are handled as expected. The SymPy parser
    uses 'eval' internally, so if a Fortran variable name should be the
    same as a SymPy function (e.g. 'field'), parsing will fail. Similarly,
    a Python reserved name (like 'lambda') would cause a parsing error.

    '''
    # A dummy program to easily create the PSyIR for the
    # expressions we need. We just take the RHS of the assignments
    source = f'''program test_prog
                 use some_mod
                 integer :: field(10)
                 integer :: i, x
                 type(my_mod_type) :: a, b
                 x = {expressions[0]}
                 x = {expressions[1]}
                 end program test_prog
             '''
    psyir = fortran_reader.psyir_from_source(source)
    schedule = psyir.children[0]
    sym_maths = SymbolicMaths.get()
    assert sym_maths.equal(schedule[0].rhs, schedule[1].rhs) is True


@pytest.mark.parametrize("expressions", [("field(:)", "field(::)", True),
                                         ("field(1:2:3)",
                                          "field(1:2:3)", True),
                                         ("field(1:2:1)",
                                          "field(1:2)", True),
                                         ("field(1:2:3)",
                                          "field(2:2:3)", False),
                                         ("field(1:2:3)",
                                          "field(1:3:3)", False),
                                         ("field(1:2:3)",
                                          "field(1:2:4)", False),
                                         ])
def test_symbolic_math_use_range(fortran_reader, expressions):
    '''Test that ranges are handled correctly. A `Range` is converted
    to a SymPy three-tuple (start, stop, step), which means all components
    need to be handled individually

    '''
    # A dummy program to easily create the PSyIR for the
    # expressions we need. We just take the RHS of the assignments
    source = f'''program test_prog
                 use some_mod
                 integer :: field(10), i
                 type(my_mod_type) :: a, b
                 x = {expressions[0]}
                 x = {expressions[1]}
                 end program test_prog
             '''
    psyir = fortran_reader.psyir_from_source(source)
    schedule = psyir.children[0]
    sym_maths = SymbolicMaths.get()
    # The child of the ArrayReference is the Range
    assert sym_maths.equal(schedule[0].rhs.children[0],
                           schedule[1].rhs.children[0]) is expressions[2]


@pytest.mark.parametrize("expr,expected", [
    ("lambda + 1", "lambda + 1"),
    ("1.0", "1.0"),
    ("a", "a"),
    ("a*b+c", "a * b + c"),
    ("c+a*b", "a * b + c"),
    ("(a*b)+c", "a * b + c"),
    ("a*(b+c)", "a * b + a * c"),
    ("a*((b+c)/d)", "a * b / d + a * c / d"),
    ("a(i)*((b(i,j)+c(j))/d)",
     "a(i) * b(i,j) / d + a(i) * c(j) / d"),
    # 'a' is unresolved so we don't know from the first occurrence whether or
    # not it is a scalar.
    ("a / a(i)", "a / a(i)"),
    ("norm_u(idx+iw2) * u_e(idx + (LBOUND(u_e,dim=1)-iw2v), df2)",
     "norm_u(idx + iw2) * u_e(idx - iw2v + LBOUND(u_e, 1),df2)")])
def test_symbolic_maths_expand(fortran_reader, fortran_writer, expr, expected):
    '''Test the expand method works as expected.'''
    # A dummy program to easily create the PSyIR for the
    # expression we need. We just take the RHS of the assignment
    source = (
        f"program test_prog\n"
        f"  use some_mod\n"
        f"  x = {expr}\n"
        f"end program test_prog\n")
    psyir = fortran_reader.psyir_from_source(source)
    sym_maths = SymbolicMaths.get()
    sym_maths.expand(psyir.children[0][0].rhs)
    result = fortran_writer(psyir.children[0][0].rhs)
    assert result == expected


def test_expand_with_intrinsic(fortran_reader, fortran_writer):
    '''
    Test that calling the `expand` method does not alter array accesses
    that are passed as arguments to other routines - in this case the
    LBOUND intrinsic.

    '''
    source = '''
  subroutine apply_mixed_operator_code(ncell1, nlayers, ndf_w2, ndf_w2h, &
                                  undf_w2v, undf_w2, lhs_w, norm_u, mu_cd)
    use kinds_mod, only: i_def, r_solver
    integer, intent(in) :: nlayers, ndf_w2, ndf_w2h, ncell1
    integer, intent(in) :: undf_w2v, undf_w2
    real(kind=r_solver), dimension(undf_w2v), intent(inout) :: lhs_w
    real(kind=r_solver), dimension(undf_w2), intent(in) :: norm_u
    real(kind=r_solver), dimension(ncell1,ndf_w2,ndf_w2), intent(in) :: mu_cd
    real(kind=r_solver), dimension(0:nlayers - 1,ndf_w2) :: u_e
    real(kind=r_solver), dimension(0:nlayers) :: t_col
    integer(kind=i_def) :: idx_10, df, df2, ij, iwt, iw2, iw2h, iw2v
    lhs_w(idx_10) = norm_u(idx_10 + (iw2 - iw2v)) * &
        mu_cd(idx_10 + (ij - iw2v), ndf_w2h + df ,df2) * &
        u_e(idx_10 + (LBOUND(u_e, dim=1) - iw2v), df2)
  end subroutine apply_mixed_operator_code'''
    psyir = fortran_reader.psyir_from_source(source)
    sym_maths = SymbolicMaths.get()
    rhs = psyir.walk(Assignment)[0].rhs
    sym_maths.expand(rhs)
    result = fortran_writer(psyir).lower()
    # Check that the 'u_e' argument remains unchanged.
    assert "lbound(u_e, 1),df2)" in result


def test_symbolic_maths_expand_function(fortran_reader, fortran_writer):
    '''Test the expand method works as expected when one of the
    Symbols in the expression corresponds to an actual function.

    '''
    source = (
        "module test\n"
        "  implicit none\n"
        "  contains\n"
        "  subroutine use_a\n"
        "    use some_mod, only: b, c, d\n"
        "    integer :: i, j, x\n"
        "    x = a(i)*((b(i,j)+c(j))/d)\n"
        "  end subroutine\n"
        "  function a(i)\n"
        "     integer, intent(in) :: i\n"
        "     integer :: a\n"
        "     a = i\n"
        "  end function\n"
        "end module\n")
    psyir = fortran_reader.psyir_from_source(source)
    sym_maths = SymbolicMaths.get()
    assigns = psyir.walk(Assignment)
    sym_maths.expand(assigns[0].rhs)
    result = fortran_writer(psyir).lower()
    assert "a(i) * b(i,j) / d +" in result


def test_symbolic_maths_expand_function_no_arg(fortran_reader, fortran_writer):
    '''Test the expand method works as expected when one of the Symbols in the
    expression corresponds to a function call with no arguments.

    '''
    source = (
        "module test\n"
        "  implicit none\n"
        "  contains\n"
        "  subroutine use_a\n"
        "    use some_mod, only: b, c, d\n"
        "    integer :: x, i, j\n"
        "    x = a()*((b(i,j)+c(j))/d)\n"
        "  end subroutine\n"
        "  function a()\n"
        "     integer :: a\n"
        "     a = 10\n"
        "  end function\n"
        "end module\n")
    psyir = fortran_reader.psyir_from_source(source)
    sym_maths = SymbolicMaths.get()
    assigns = psyir.walk(Assignment)
    sym_maths.expand(assigns[0].rhs)
    result = fortran_writer(psyir).lower()
    assert "x = a() * b(i,j) / d + a() *" in result


def test_symbolic_maths_array_and_array_index(fortran_reader):
    '''Test having an expression that uses a whole array and
    the same array with an index, e.g. : `a(i) + a`.
    '''
    source = '''program test_prog
          use some_mod
          real :: x, y, a(10)
          x = a(i)
          y = a
        end program test_prog'''
    psyir = fortran_reader.psyir_from_source(source)
    assigns = psyir.walk(Assignment)
    sym_maths = SymbolicMaths.get()
    assert not sym_maths.equal(assigns[0].rhs, assigns[1].rhs)

    assert sym_maths.equal(assigns[0].rhs, assigns[0].rhs)

    assert sym_maths.equal(assigns[1].rhs, assigns[1].rhs)


@pytest.mark.parametrize(
    "expressions",
    [(".false. .and. .false.", "False"),
     (".false. .and. .true.", "False"),
     (".true. .and. .false.", "False"),
     (".true. .and. .true.", "True"),
     (".false. .or. .false.", "False"),
     (".false. .or. .true.", "True"),
     (".true. .or. .false.", "True"),
     (".true. .or. .true.", "True"),
     (".false. .eqv. .false.", "True"),
     (".false. .eqv. .true.", "False"),
     (".true. .eqv. .false.", "False"),
     (".true. .eqv. .true.", "True"),
     (".false. .neqv. .false.", "False"),
     (".false. .neqv. .true.", "True"),
     (".true. .neqv. .false.", "True"),
     (".true. .neqv. .true.", "False"),
     (" .false. .and. ((3 -2 + 4 - 5) .eq. 0 .and. .false.)", False),
     ])
def test_sym_writer_boolean_expr(fortran_reader, expressions):
    '''Test that booleans are written in the way that SymPy accepts.
    '''
    # A dummy program to easily create the PSyIR for the
    # expressions we need. We just take the RHS of the assignments
    source = f'''program test_prog
                logical :: bool_expr
                bool_expr = {expressions[0]}
                bool_expr = {expressions[1]}
                end program test_prog '''

    psyir = fortran_reader.psyir_from_source(source)
    lit0 = psyir.children[0].children[0].rhs
    lit1 = psyir.children[0].children[1].rhs
    sympy_writer = SymPyWriter()

    sympy_expr = sympy_writer(lit0)
    assert sympy_expr == sympy_writer(lit1)


@pytest.mark.parametrize(
    "expressions",
    [(".true. .and. .false.", False),
     (".true. .and. .true.", True),
     (".false. .or. .true.", True),
     ("3 .eq. 3", True),
     (" ((3 -2 + 4 - 5) .eq. 0 .and. .false.) .or. .true.", True),
     (" ((3 -2 + 4 - 5) .eq. 0 .and. .true.)", True),
     (" (3 -2 + 4 - 5) .eq. 0 .and. .false. .and. .true.", False),
     (" ((3 -2 + 4 - 5) .eq. 0 .and. .false.) .and. .true.", False),
     (" .false. .and. ((3 -2 + 4 - 5) .eq. 0 .and. .false.)", False),
     ("  (((3 -2 + 4 - 5) .eq. 0) .and. .false.)", False),
     ])
def test_sym_writer_boolean_expr_add_test(fortran_reader, expressions):
    '''Test that booleans are written in the way that SymPy accepts.
    '''
    # A dummy program to easily create the PSyIR for the
    # expressions we need. We just take the RHS of the assignments
    source = f'''program test_prog
    logical :: bool_expr
    bool_expr = {expressions[0]}
    end program test_prog '''

    psyir = fortran_reader.psyir_from_source(source)
    lit = psyir.children[0].children[0].rhs
    sympy_writer = SymPyWriter()
    sympy_expr = sympy_writer(lit)
    assert sympy_expr == expressions[1]
