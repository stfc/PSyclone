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
# Author: J. Henrichs, Bureau of Meteorology
# Modified: R. W. Ford, STFC Daresbury Lab
#           A. R. Porter, STFC Daresbury Lab
#           S. Siso, STFC Daresbury Lab

''' Module containing py.test tests the SymPy writer.'''

import pytest

from sympy import Function, Symbol
from sympy.parsing.sympy_parser import parse_expr

from psyclone.psyir.frontend.sympy_reader import SymPyReader
from psyclone.psyir.backend.sympy_writer import SymPyWriter
from psyclone.psyir.backend.visitor import VisitorError
from psyclone.psyir.nodes import Literal
from psyclone.psyir.symbols import (ArrayType, BOOLEAN_TYPE, CHARACTER_TYPE,
                                    INTEGER_TYPE)


def test_sym_writer_constructor():
    '''Test that the constructor accepts an optional dictionary.
    '''
    sympy_writer = SymPyWriter()
    assert sympy_writer._sympy_type_map == {}
    assert sympy_writer._DISABLE_LOWERING is True

    # __getitem_ can never be called anyway, since both the SymPy
    # writer and the visitor base class implement __call__. In order
    # to test the exception, we need to call it explicitly:
    with pytest.raises(NotImplementedError) as err:
        # pylint: disable=unnecessary-dunder-call
        sympy_writer.__getitem__(None)
    assert ("__getitem__ for a SymPyWriter should never be called."
            in str(err.value))


def test_sym_writer_lowering_disabled(monkeypatch):
    '''Test that by default this Writer does not attempt to lower higher
    abstraction nodes into language level nodes. We also test that with
    _DISABLE_LOWERING set to False the same situation would produce an error.
    '''
    def error(_):
        ''' Produce an error '''
        raise NotImplementedError()

    monkeypatch.setattr(Literal, "lower_to_language_level", error)
    lit = Literal("true", BOOLEAN_TYPE)
    sympy_writer = SymPyWriter()
    sympy_writer(lit)  # No error should be raised here

    # Without disabling lowering it would fail with a VisitorError
    sympy_writer._DISABLE_LOWERING = False
    with pytest.raises(VisitorError) as err:
        sympy_writer(lit)
    assert "Failed to lower 'Literal" in str(err.value)


def test_sym_writer_boolean():
    '''Test that booleans are written in the way that SymPy accepts.
    '''
    sympy_writer = SymPyWriter()
    lit = Literal("true", BOOLEAN_TYPE)
    assert sympy_writer._to_str(lit) == "True"
    lit = Literal("false", BOOLEAN_TYPE)
    assert sympy_writer._to_str(lit) == "False"


def test_sym_writer_character():
    '''Test that characters are rejected.
    '''
    sympy_writer = SymPyWriter()
    lit = Literal("bla", CHARACTER_TYPE)

    with pytest.raises(TypeError) as err:
        sympy_writer(lit)

    assert "SymPy cannot handle strings like 'bla'." in str(err.value)


@pytest.mark.parametrize("expressions", [("2", "2"),
                                         ("123_4", "123"),
                                         ("456_8", "456"),
                                         ("123_xx", "123")
                                         ])
def test_sym_writer_int_constants(fortran_reader, expressions):
    '''Test that integer constants are handled, including precision
    specifications (either as int or as a name).
    '''
    # A dummy program to easily create the PSyIR for the
    # expressions we need. We just take the RHS of the assignments
    source = f'''program test_prog
                use some_mod
                integer :: x
                x = {expressions[0]}
                end program test_prog '''
    psyir = fortran_reader.psyir_from_source(source)
    # psyir is a FileContainer, its first child the program, and its
    # first child the assignment, of which we take the right hand side
    lit = psyir.children[0].children[0].rhs

    sympy_writer = SymPyWriter()
    assert sympy_writer._to_str(lit) == expressions[1]


@pytest.mark.parametrize("expressions", [("3.1415926535897932384626",
                                          "3.1415926535897932384626"),
                                         ("1.23E5", "1.23e5"),
                                         ("1.23D5", "1.23e5"),
                                         ("1.0E+3", "1.0e+3"),
                                         ("1.0", "1.0"),
                                         ("0.01E-3", "0.01e-3"),
                                         ("3.14e-2", "3.14e-2")
                                         ])
def test_sym_writer_real_constants(fortran_reader, expressions):
    '''Test that real constants are handled, including precision
    specifications (either as int or as a name).
    '''
    # A dummy program to easily create the PSyIR for the
    # expressions we need. We just take the RHS of the assignments
    source = f'''program test_prog
                integer :: x
                x = {expressions[0]}
                end program test_prog '''

    psyir = fortran_reader.psyir_from_source(source)
    lit = psyir.children[0].children[0].rhs
    assert SymPyWriter()._to_str(lit) == expressions[1]


@pytest.mark.parametrize("expressions", [("MAX(1,2)", "Max(1, 2)"),
                                         ("MIN(1,2)", "Min(1, 2)"),
                                         ("MOD(1,2)", "Mod(1, 2)"),
                                         ("EXP(1)", "exp(1)"),
                                         ("LBOUND(1,2)", "LBOUND(1, 2)")
                                         ])
def test_sym_writer_functions(fortran_reader, expressions):
    '''Test that certain standard functions are recognised and
    converted into the equivalent SymPy syntax (capitalised first
    letter). Note LBOUND is not defined in SymPy, and as such is
    not converted.

    '''
    # A dummy program to easily create the PSyIR for the
    # expressions we need. We just take the RHS of the assignments
    source = f'''program test_prog
                integer :: x
                x = {expressions[0]}
                end program test_prog '''

    psyir = fortran_reader.psyir_from_source(source)
    function = psyir.children[0].children[0].rhs
    assert SymPyWriter()._to_str(function) == expressions[1]


@pytest.mark.parametrize("expr, sym_map", [("i", {'i': Symbol('i')}),
                                           ("f(1)", {'f': Function('f')}),
                                           ("f(:)", {'f': Function('f')}),
                                           ("a%b", {'a': Symbol('a'),
                                                    'a_b': Symbol('a_b')}),
                                           ("a%b(1)", {'a': Symbol('a'),
                                                       'a_b': Function('a_b')})
                                           ])
def test_sympy_writer_create_type_map(expr, sym_map, fortran_reader):
    '''Tests that the static create_type_map creates a dictionary
    with correctly declared references (and not any member names,
    which will be added later).
    '''

    # A dummy program to easily create the PSyIR for the
    # expressions we need. We just take the RHS of the assignments
    source = f'''program test_prog
                use my_mod
                type(my_type) :: a, b(10)
                integer :: i, f(10)
                x = {expr}
                end program test_prog '''

    psyir = fortran_reader.psyir_from_source(source)
    expr = psyir.children[0].children[0].rhs
    sympy_writer = SymPyWriter()
    _ = sympy_writer(expr)
    assert sympy_writer._sympy_type_map.keys() == sym_map.keys()


@pytest.mark.parametrize("expressions", [("a%x", "a_x"),
                                         ("b(i)%x", "b_x(i,i,1)"),
                                         ("a%x(i)", "a_x(i,i,1)"),
                                         ("b(j)%x(i)", "b_x(j,j,1,i,i,1)"),
                                         ("b(i)%c(b_c)",
                                          "b_c_1(i,i,1,b_c,b_c,1)"),
                                         ("a_c + a%c(i)",
                                          "a_c + a_c_1(i,i,1)"),
                                         ("b(b_c)%c(i)",
                                          "b_c_1(b_c,b_c,1,i,i,1)"),
                                         ("b(b_c)%c(i)",
                                          "b_c_1(b_c,b_c,1,i,i,1)"),
                                         ("a_b_c + a_b_c_1 + a%b%c",
                                          "a_b_c + a_b_c_1 + a_b_c_2"),
                                         ])
def test_sym_writer_rename_members(fortran_reader, expressions):
    '''Test that members are converted and get a unique name that
    does not clash with any other variable used in the expression.

    '''
    # A dummy program to easily create the PSyIR for the
    # expressions we need. We just take the RHS of the assignments
    source = f'''program test_prog
                use my_mod
                type(my_type) :: a, b(10)
                integer :: i, j, x, a_c, b_c, a_b_c, a_b_c_1
                x = {expressions[0]}
                end program test_prog '''

    psyir = fortran_reader.psyir_from_source(source)
    expr = psyir.children[0].children[0].rhs
    assert SymPyWriter()._to_str(expr) == expressions[1]


@pytest.mark.parametrize("expr, sym_map", [("a%x", {"a": Symbol("a"),
                                                    "a_x": Symbol("a_x")}),
                                           ("a%x(i)", {"a": Symbol("a"),
                                                       "a_x": Function("a_x"),
                                                       "i": Symbol("i")}),
                                           ("b(i)%x(i)",
                                            {"b": Function("b"),
                                             "b_x": Function("b_x"),
                                             "i": Symbol("i")}),
                                           ("b(b_c)%c(i)",
                                            {"b": Function("b"),
                                             "b_c": Symbol("b_c"),
                                             "b_c_1": Function("b_c_1"),
                                             "i": Symbol("i")}),
                                           ])
def test_sym_writer_symbol_types(fortran_reader, expr, sym_map):
    '''Tests that arrays are detected as SymPy functions, and scalars
    as SymPy symbols. The expressions parameter contains as first
    element the expression to parse, and as second element the
    expected mapping of names to SymPy functions or symbols.

    '''
    # A dummy program to easily create the PSyIR for the
    # expressions we need. We just take the RHS of the assignments
    source = f'''program test_prog
                use my_mod
                type(my_type) :: a, b(10)
                integer :: i, j, x, a_c, b_c, a_b_c, a_b_c_1
                x = {expr}
                end program test_prog '''

    psyir = fortran_reader.psyir_from_source(source)
    expr = psyir.children[0].children[0].rhs
    sympy_writer = SymPyWriter()
    _ = sympy_writer(expr)
    assert sympy_writer.type_map.keys() == sym_map.keys()


@pytest.mark.parametrize("expr, sym_map", [("i", {'i': Symbol('i')}),
                                           ("f(1)", {'f': Function('f')}),
                                           ("a%b", {'a': Symbol('a'),
                                                    'a_b': Symbol('a_b')}),
                                           ("a%b(1)",
                                            {'a': Symbol('a'),
                                             'a_b': Function('a_b')})
                                           ])
def test_sympy_writer_get_symbol_and_map(expr, sym_map, fortran_reader):
    '''Tests that `get_sympy_expressions_and_symbol_map` function
    returns the right symbol map.
    '''

    # A dummy program to easily create the PSyIR for the
    # expressions we need. We just take the RHS of the assignments
    source = f'''program test_prog
                use my_mod
                type(my_type) :: a, b(10)
                integer :: i, f(10), x
                x = {expr}
                end program test_prog '''

    psyir = fortran_reader.psyir_from_source(source)
    expr = psyir.children[0].children[0].rhs

    writer = SymPyWriter()
    # Ignore the converted expressions here, they are tested elsewhere
    _ = writer([expr])
    assert writer._sympy_type_map.keys() == sym_map.keys()


def test_sym_writer_convert_to_sympy_expressions(fortran_reader):
    '''Tests that convenience function `convert_to_sympy_expressions`
    works as expected.

    '''
    # A dummy program to easily create the PSyIR for the
    # expressions we need. We just take the RHS of the assignments
    source = '''program test_prog
                use my_mod
                type(my_type) :: a, b(10)
                integer :: i, j, x, a_c, b_c, a_b_c, a_b_c_1
                x = a%b + a%c(1) + i
                x = a_b + j
                end program test_prog '''

    psyir = fortran_reader.psyir_from_source(source)
    exp1 = psyir.children[0].children[0].rhs
    exp2 = psyir.children[0].children[1].rhs
    sympy_writer = SymPyWriter()
    sympy_list = sympy_writer([exp1, exp2])

    expr = parse_expr("a_b_1 + a_c(1,1,1) + i", sympy_writer.type_map)
    assert sympy_list[0] == expr
    assert sympy_list[1] == parse_expr("a_b + j", sympy_writer.type_map)


def test_sym_writer_parse_errors(fortran_reader):
    '''Tests that unsupported syntax (e.g. array ranges) raise the
    expected VisitorError

    '''
    # A dummy program to easily create the PSyIR for the
    # expressions we need. We just take the RHS of the assignments
    source = '''program test_prog
                real :: x, a(10), b(10)
                x = a(:) /= b(:)
                end program test_prog '''

    psyir = fortran_reader.psyir_from_source(source)
    exp1 = psyir.children[0].children[0].rhs

    with pytest.raises(VisitorError) as err:
        _ = SymPyWriter(exp1)

    assert ("Visitor Error: Invalid SymPy expression: "
            "'a(sympy_lower,sympy_upper,1) /= b(sympy_lower,sympy_upper,1)'"
            in str(err.value))


@pytest.mark.parametrize("expressions", [("b(i)", "b(i,i,1)"),
                                         ("b(:)",
                                          "b(sympy_lower,sympy_upper,1)"),
                                         ("b(::)",
                                          "b(sympy_lower,sympy_upper,1)"),
                                         ("b(5::)", "b(5,sympy_upper,1)"),
                                         ("b(:5:)", "b(sympy_lower,5,1)"),
                                         ("b(::5)",
                                          "b(sympy_lower,sympy_upper,5)"),
                                         ("b(i::)", "b(i,sympy_upper,1)"),
                                         ("b(:i:)", "b(sympy_lower,i,1)"),
                                         ("b(::i)",
                                          "b(sympy_lower,sympy_upper,i)"),
                                         ("b(i:5:)", "b(i,5,1)"),
                                         ("b(i:j:)", "b(i,j,1)"),
                                         ("b(i::j)", "b(i,sympy_upper,j)"),
                                         ("b(:i:j)", "b(sympy_lower,i,j)"),
                                         ("b(i:j:k)", "b(i,j,k)"),
                                         ("b", "b(sympy_lower,sympy_upper,1)"),
                                         ("c(i,j)", "c(i,i,1,j,j,1)"),
                                         ("c(::,::)",
                                          "c(sympy_lower,sympy_upper,1,"
                                          "sympy_lower,sympy_upper,1)"),
                                         ("c", "c(sympy_lower,sympy_upper,1,"
                                               "sympy_lower,sympy_upper,1)"),
                                         ("b(i)%x", "b_x(i,i,1)"),
                                         ("b(i)%x(j)", "b_x(i,i,1,j,j,1)"),
                                         ("c(i,j)%x", "c_x(i,i,1,j,j,1)"),
                                         ("c(i,j)%x(j)",
                                          "c_x(i,i,1,j,j,1,j,j,1)"),
                                         ("c(i,j)%d%e",
                                          "c_d_e(i,i,1,j,j,1)"),
                                         ("c(i,j)%d%f(i)",
                                          "c_d_f(i,i,1,j,j,1,i,i,1)"),
                                         ("c(i::k,j)%d%f(i:j:k)",
                                          "c_d_f(i,sympy_upper,k,j,j,1,"
                                          "i,j,k)"),
                                         # Check name clashes, if a user
                                         # variable is the same as the names
                                         # for upper/lower bound
                                         ("sympy_upper(:)",
                                          "sympy_upper(sympy_lower,"
                                          "sympy_upper_1,1)"),
                                         ("sympy_lower(:)",
                                          "sympy_lower(sympy_lower_1,"
                                          "sympy_upper,1)"),
                                         # The +sympy_upper at the end is
                                         # an array expression, so it gets
                                         # indices added!
                                         ("sympy_lower(:)+sympy_upper",
                                          "sympy_lower(sympy_lower_1,"
                                          "sympy_upper_1,1) + sympy_upper"
                                          "(sympy_lower_1,sympy_upper_1,1)"),
                                         ])
def test_sym_writer_array_expressions(fortran_reader, expressions):
    '''Test that array expressions (including ones using user-defined
    types) are converted correctly. A Fortran range is converted into
    three arguments for the SymPy function used: lower bound, upper bound,
    step. If the bounds are not given, +/- sympy_upper(inity) is used. E.g.:
    `a(:)` --> `a(sympy_lower,sympy_upper,1)`. And to keep the number of
    arguments the same, an array index access like `b(i,j)` is converted to:
    `b(i,i,1, j,j,1)`.

    '''
    # A dummy program to easily create the PSyIR for the
    # expressions we need. We just take the RHS of the assignments
    source = f'''program test_prog
                use my_mod
                integer :: sympy_upper(10), sympy_lower(10), x
                type(my_type) :: a, b(10), c(10, 10)
                x = {expressions[0]}
                end program test_prog '''

    psyir = fortran_reader.psyir_from_source(source)
    expr = psyir.children[0].children[0].rhs
    sympy_writer = SymPyWriter()
    out = sympy_writer._to_str([expr])
    assert out[0] == expressions[1]


def test_gen_indices():
    '''This test covers other datatypes that might be passed to `gen_indices`.
    '''

    sympy_writer = SymPyWriter()
    # Test using array bounds and DEFERRED:
    arr_bounds = ArrayType.ArrayBounds(Literal("2", INTEGER_TYPE),
                                       Literal("5", INTEGER_TYPE))
    gen_ind = sympy_writer.gen_indices([arr_bounds, ArrayType.Extent.DEFERRED])
    assert gen_ind == ["2", "5", "1", "sympy_lower", "sympy_upper", "1"]

    # Test invalid type:
    with pytest.raises(NotImplementedError) as err:
        _ = sympy_writer.gen_indices([None])
    assert "unsupported gen_indices index 'None'" in str(err.value)


@pytest.mark.parametrize("fortran_expr,sympy_str",
                         [("a%b", "a_b"),
                          # Handle name clash:
                          ("a%c + a_c", "a_c_1 + a_c"),
                          ("a%b(i)", "a_b(i,i,1)"),
                          ("b(i)%b", "b_b(i,i,1)"),
                          ("b(:)%b(i) + b(1)%c",
                           "b_b(sympy_lower,sympy_upper,1,i,i,1) + "
                           "b_c(1,1,1)"),
                          ("b(i)%b(j)", "b_b(i,i,1,j,j,1)"),
                          ("a%b(i)%c", "a_b_c(i,i,1)"),
                          ("a%b%c(i)", "a_b_c(i,i,1)"),
                          ("a%b%c(2 * i - 1)", "a_b_c(2 * i - 1,2 * i - 1,1)")
                          ])
def test_sympy_writer_user_types(fortran_reader, fortran_writer,
                                 fortran_expr, sympy_str):
    '''Test handling of user-defined types, e.g. conversion of
    ``a(i)%b(j)`` to ``a_b(i,i,1,j,j,1)``. Each Fortran expression
    ``fortran_expr`` is first converted to a string ``sympy_str`` to be
    parsed by SymPy. The sympy expression is then converted back to PSyIR.
    This string must be the same as the original ``fortran_expr``.

    '''
    source = f'''program test_prog
                use my_mod
                type(my_mod_type) :: a, b(1)
                x = {fortran_expr}
                end program test_prog'''

    psyir = fortran_reader.psyir_from_source(source)
    # Get the actual fortran expression requested:
    psyir_expr = psyir.children[0].children[0].rhs

    # Convert the PSyIR to a SymPy string:
    sympy_writer = SymPyWriter()
    out = sympy_writer._to_str([psyir_expr])
    # Make sure we get the expected string as output:
    assert out[0] == sympy_str

    # Second part of the test: convert the PSyIR to a SymPy expression
    # (not only a string):
    sympy_exp = sympy_writer(psyir_expr)

    symbol_table = psyir.children[0].symbol_table
    sympy_reader = SymPyReader(sympy_writer)
    new_psyir = sympy_reader.psyir_from_expression(sympy_exp, symbol_table)
    assert fortran_writer(new_psyir) == fortran_expr


@pytest.mark.parametrize("expression", ["def", "if", "raise", "del",
                                        "import", "return", "elif", "in",
                                        "try", "and", "else", "is", "while",
                                        "as", "except", "lambda", "with",
                                        "assert", "finally", "nonlocal",
                                        "yield", "break", "for", "not",
                                        "class", "from", "or", "continue",
                                        "global", "pass"])
def test_sym_writer_reserved_names(fortran_reader, expression):
    '''Test that reserved names are properly renamed. In this example,
    all reserved named will get a ``_1`` appended.
    '''
    # A dummy program to easily create the PSyIR for the
    # expressions we need. We just take the RHS of the assignments
    source = f'''program test_prog
                use some_mod
                integer :: x
                x = {expression}
                end program test_prog '''
    psyir = fortran_reader.psyir_from_source(source)
    # psyir is a FileContainer, its first child the program, and its
    # first child the assignment, of which we take the right hand side
    psyir_expr = psyir.children[0].children[0].rhs

    # Make sure that the symbols are renamed in the string representation
    # of the PSyIR - the symbol table will make the symbols unique by
    # appending ``_1``
    sympy_writer = SymPyWriter()
    assert sympy_writer._to_str(psyir_expr) == f"{expression}_1"

    # The SymPy representation will contain e.g. the symbol 'lambda_1' after
    # renaming, but when the expression is converted to a string, it should
    # use 'lambda' as string representation for the symbol 'lambda_1'.
    # Explicit:
    #  >>> lambda_1 = sp.Symbol("lambda")
    #  >>> print(lambda_1)
    # lambda

    sympy_exp = sympy_writer(psyir_expr)
    assert str(sympy_exp) == expression
