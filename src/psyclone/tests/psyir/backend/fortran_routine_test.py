# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2019-2026, Science and Technology Facilities Council.
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
# Author R. W. Ford, STFC Daresbury Lab
# Modified by A. R. Porter and S. Siso, STFC Daresbury Lab
# Modified by A. B. G. Chalk, STFC Daresbury Lab
# -----------------------------------------------------------------------------

'''Performs pytest tests on the Routine node handler in the
   psyclone.psyir.backend.fortran module.'''

import pytest

from psyclone.psyir import nodes, symbols
from psyclone.psyir.nodes import Routine
from psyclone.tests.utilities import Compile


def test_fw_routine(fortran_reader, fortran_writer, monkeypatch, tmpdir):
    '''Check the FortranWriter class outputs correct code when a routine node
    is found. Also tests that an exception is raised if routine.name does not
    have a value.

    '''
    code = (
        "module test\n"
        "contains\n"
        "subroutine tmp(a, b, c)\n"
        "  use iso_c_binding, only : c_int\n"
        "  real, intent(out) :: a(:)\n"
        "  real, intent(in) :: b(:)\n"
        "  integer, intent(in) :: c\n"
        "  if(c > 3) then\n"
        "  a = b/c\n"
        "  else\n"
        "  a = b/c\n"
        "  endif\n"
        "end subroutine tmp\n"
        "end module test")
    psyir = fortran_reader.psyir_from_source(code)
    schedule = psyir.children[0].children[0]
    # Generate Fortran from the PSyIR schedule
    result = fortran_writer(schedule)

    expected = (
        "subroutine tmp(a, b, c)\n"
        "  use iso_c_binding, only : c_int\n"
        "  real, dimension(:), intent(out) :: a\n"
        "  real, dimension(:), intent(in) :: b\n"
        "  integer, intent(in) :: c\n"
        "\n"
        "  if (c > 3) then\n"
        "    a = b / c\n"
        "  else\n"
        "    a = b / c\n"
        "  end if\n"
        "\n"
        "end subroutine tmp\n")
    assert expected in result
    assert Compile(tmpdir).string_compiles(result)

    # Add distinctly named symbols in the routine sub-scopes
    sub_scopes = schedule.walk(nodes.Schedule)[1:]
    sub_scopes[0].symbol_table.new_symbol(
        "symbol1", symbol_type=symbols.DataSymbol,
        datatype=symbols.INTEGER_TYPE)
    sub_scopes[1].symbol_table.new_symbol(
        "symbol2", symbol_type=symbols.DataSymbol,
        datatype=symbols.INTEGER_TYPE)
    # They should be promoted to the routine-scope level
    result = fortran_writer(schedule)
    assert (
        "  integer, intent(in) :: c\n"
        "  integer :: symbol1\n"
        "  integer :: symbol2\n") in result
    assert Compile(tmpdir).string_compiles(result)

    # Add symbols that will result in name clashes to sibling scopes
    sub_scopes = schedule.walk(nodes.Schedule)[1:]
    sub_scopes[0].symbol_table.new_symbol(
        "symbol2", symbol_type=symbols.DataSymbol,
        datatype=symbols.INTEGER_TYPE)
    sub_scopes[1].symbol_table.new_symbol(
        "symbol1", symbol_type=symbols.DataSymbol,
        datatype=symbols.INTEGER_TYPE)
    # Since the scopes are siblings they are allowed the same name
    assert "symbol1" in sub_scopes[0].symbol_table
    assert "symbol2" in sub_scopes[0].symbol_table
    assert "symbol1" in sub_scopes[1].symbol_table
    assert "symbol2" in sub_scopes[1].symbol_table
    # But the back-end will promote them to routine-scope level with different
    # names
    result = fortran_writer(schedule)
    assert (
        "  integer, intent(in) :: c\n"
        "  integer :: symbol1\n"
        "  integer :: symbol2\n"
        "  integer :: symbol2_1\n"
        "  integer :: symbol1_1\n"
        "\n") in result
    assert Compile(tmpdir).string_compiles(result)


def test_fw_routine_nameclash(fortran_writer):
    ''' Test that any name clashes are handled when merging symbol tables. '''
    sym1 = symbols.DataSymbol("var1", symbols.INTEGER_TYPE)
    sym2 = symbols.DataSymbol("var1", symbols.INTEGER_TYPE)
    assign1 = nodes.Assignment.create(nodes.Reference(sym1),
                                      nodes.Literal("1", symbols.INTEGER_TYPE))
    assign2 = nodes.Assignment.create(nodes.Reference(sym2),
                                      nodes.Literal("2", symbols.INTEGER_TYPE))
    ifblock = nodes.IfBlock.create(nodes.Literal("true", symbols.BOOLEAN_TYPE),
                                   [assign1], [assign2])
    # Place the symbols for the two variables in the tables associated with
    # the two branches of the IfBlock. These then represent *different*
    # variables, despite having the same name.
    ifblock.if_body.symbol_table.add(sym1)
    ifblock.else_body.symbol_table.add(sym2)
    routine = nodes.Routine.create("my_sub", symbols.SymbolTable(), [ifblock])
    result = fortran_writer(routine)
    assert ("  integer :: var1\n"
            "  integer :: var1_1\n"
            "\n"
            "  if (.true.) then\n"
            "    var1 = 1\n"
            "  else\n"
            "    var1_1 = 2\n"
            "  end if" in result)
    # Add a symbol to the local scope of the else that will clash with
    # the name generated with reference to the routine scope.
    ifblock.else_body.symbol_table.add(
        symbols.DataSymbol("var1_1", symbols.INTEGER_TYPE))
    result = fortran_writer(routine)
    assert ("  integer :: var1\n"
            "  integer :: var1_2\n"
            "  integer :: var1_1\n"
            "\n"
            "  if (.true.) then\n"
            "    var1 = 1\n"
            "  else\n"
            "    var1_2 = 2\n"
            "  end if" in result)
    # Add a symbol to the routine scope that will clash with the first name
    # generated with reference to the else scope.
    routine.symbol_table.add(symbols.DataSymbol("var1_2",
                                                symbols.INTEGER_TYPE))
    result = fortran_writer(routine)
    assert ("  integer :: var1_2\n"
            "  integer :: var1\n"
            "  integer :: var1_3\n"
            "  integer :: var1_1\n"
            "\n"
            "  if (.true.) then\n"
            "    var1 = 1\n"
            "  else\n"
            "    var1_3 = 2\n"
            "  end if" in result)


def test_fw_routine_program(fortran_reader, fortran_writer, tmpdir):
    '''Check the FortranWriter class outputs correct code when a routine node
    is found with is_program set to True i.e. it should be output as a program.

    '''
    code = (
        "program test\n"
        "  real :: a\n"
        "  a = 0.0\n"
        "end program test")
    psyir = fortran_reader.psyir_from_source(code)

    # Generate Fortran from PSyIR
    result = fortran_writer(psyir)

    assert (
        "program test\n"
        "  real :: a\n\n"
        "  a = 0.0\n\n"
        "end program test\n" in result)
    assert Compile(tmpdir).string_compiles(result)


@pytest.mark.parametrize("result_decl", ["real :: val",
                                         "real, volatile :: val"])
def test_fw_routine_function(fortran_reader, fortran_writer, tmpdir,
                             result_decl):
    ''' Check that the FortranWriter outputs a function when a routine node
    is found with return_symbol set. We check for both supported and
    unsupported (e.g. with the 'volatile' attribute) forms of declaration for
    that symbol.

    '''
    code = (f"module test\n"
            f"implicit none\n"
            f"real :: a\n"
            f"contains\n"
            f"function tmp(b) result(val)\n"
            f"  {result_decl}\n"
            f"  real :: b\n"
            f"  val = a + b\n"
            f"end function tmp\n"
            f"end module test")
    container = fortran_reader.psyir_from_source(code)
    # Generate Fortran from PSyIR
    result = fortran_writer(container)
    assert (
        f"  contains\n"
        f"  function tmp(b) result(val)\n"
        f"    {result_decl}\n"
        f"    real :: b\n\n"
        f"    val = a + b\n\n"
        f"  end function tmp\n" in result.lower())
    assert Compile(tmpdir).string_compiles(result)


def test_fw_routine_function_no_result(fortran_reader, fortran_writer, tmpdir):
    ''' Check that no `result(xxx)` clause is added to the output function
    definition if the name of the return symbol matches the name of the
    function but has different capitalisation.

    '''
    code = ("module test\n"
            "implicit none\n"
            "real :: a\n"
            "contains\n"
            "function tmp(b)\n"
            "  real, intent(in) :: b\n"
            "  real :: TMP\n"
            "  tmp = a + b\n"
            "end function tmp\n"
            "end module test")
    container = fortran_reader.psyir_from_source(code)
    # Generate Fortran from PSyIR
    result = fortran_writer(container)
    assert "  function tmp(b)\n" in result
    assert Compile(tmpdir).string_compiles(result)


def test_fw_routine_flatten_tables(fortran_reader, fortran_writer):
    '''
    Check that module use statements in nested symbol tables are handled
    correctly in the presence of name clashes.

    '''
    code = ("module test\n"
            "implicit none\n"
            "contains\n"
            "subroutine sub(b)\n"
            "  use some_mod, only: joe\n"
            "  real, intent(inout) :: b\n"
            "  real :: the_clash, strummer\n"
            "  integer :: ii\n"
            "  do ii = 1, 10\n"
            "    b = ii + b\n"
            "  end do\n"
            "end subroutine sub\n"
            "end module test")
    container = fortran_reader.psyir_from_source(code)
    # Find the loop as its body has a symbol table.
    loop = container.walk(nodes.Loop)[0]
    table = loop.loop_body.symbol_table
    # Add an import to this symbol table that will clash with symbols already
    # declared in the routine table.
    csym = symbols.ContainerSymbol("the_clash")
    ssym = symbols.DataSymbol("strummer", datatype=symbols.UnresolvedType(),
                              interface=symbols.ImportInterface(csym))
    # Add a variable to this table that will clash with a Container symbol
    # in the routine table.
    jsym = symbols.DataSymbol("joe", symbols.INTEGER_TYPE)
    table.add(csym)
    table.add(ssym)
    table.add(jsym)
    code = fortran_writer(container)
    # Check the resulting code has the correct module use statements.
    assert ("  subroutine sub(b)\n"
            "    use some_mod, only : joe\n"
            "    use the_clash, only : strummer\n" in code)
    # While the clashing variables have been renamed.
    assert "real :: the_clash_1" in code
    assert "real :: strummer_1" in code
    assert "integer :: joe_1" in code
    # We can't test for compilation because of the `use` statements.


def test_fw_routine_flatten_tables_unresolved_sym(fortran_reader,
                                                  fortran_writer):
    '''
    Check that the flattening process works correctly when nested scopes
    contain the same, unresolved symbol being brought into scope at the
    module level.

    '''
    code = ("module test\n"
            "use some_mod\n"
            "implicit none\n"
            "contains\n"
            "subroutine sub(b)\n"
            "  real, intent(inout) :: b\n"
            "  integer :: ii\n"
            "  do ii = 1, 10\n"
            "    b = ii + b\n"
            "    call iom_put(ii)\n"
            "  end do\n"
            "  call iom_put(b)\n"
            "end subroutine sub\n"
            "end module test")
    container = fortran_reader.psyir_from_source(code)
    output = fortran_writer(container)
    # We should still have the wildcard import.
    assert "use some_mod\n" in output
    # The calls to iom_put() should be unaffected.
    assert "call iom_put(ii)" in output
    assert "call iom_put(b)" in output


def test_fw_routine_prefixes(fortran_reader, fortran_writer):
    '''
        Test the pure, impure and elemental routine prefixes.
    '''
    code = '''module test
    contains
    elemental subroutine sub()
    end subroutine sub
    end module test'''
    container = fortran_reader.psyir_from_source(code)
    output = fortran_writer(container)
    routine = container.walk(Routine)[0]
    rsym = routine._symbol
    assert rsym.is_elemental
    assert rsym.is_pure is None
    assert "elemental" in output
    # elemental => pure unless impure specified.
    assert "impure" not in output

    code = '''module test
    contains
    pure subroutine sub()
    end subroutine sub
    end module test'''
    container = fortran_reader.psyir_from_source(code)
    output = fortran_writer(container)
    routine = container.walk(Routine)[0]
    rsym = routine._symbol
    assert not rsym.is_elemental
    assert rsym.is_pure
    assert "pure" in output

    code = '''module test
    contains
    impure elemental subroutine sub()
    end subroutine sub
    end module test'''
    container = fortran_reader.psyir_from_source(code)
    output = fortran_writer(container)
    routine = container.walk(Routine)[0]
    rsym = routine._symbol
    assert rsym.is_elemental
    assert not rsym.is_pure
    assert "impure elemental" in output

    # Test this also works for functions
    code = '''module test
    contains
    elemental function sub(val1, val2)
        real :: sub
        real :: val1
        real :: val2
        sub = val1 - val2
    end function
    end module
    '''
    container = fortran_reader.psyir_from_source(code)
    routine = container.walk(Routine)[0]
    rsym = routine.parent.scope.symbol_table.lookup(routine.name)
    assert rsym.is_elemental
    output = fortran_writer(container)
    assert "elemental function" in output

    code = '''module test
    contains
    impure elemental function sub(val1, val2)
        real :: sub
        real :: val1
        real :: val2
        sub = val1 - val2
    end function
    end module
    '''
    container = fortran_reader.psyir_from_source(code)
    routine = container.walk(Routine)[0]
    rsym = routine.parent.scope.symbol_table.lookup(routine.name)
    assert rsym.is_elemental
    assert not rsym.is_pure
    output = fortran_writer(container)
    assert "impure elemental function" in output

    code = '''module test
    contains
    real impure elemental function sub(val1, val2)
        real :: val1, val2
        sub = val1 - val2
    end function
    end module
    '''
    container = fortran_reader.psyir_from_source(code)
    routine = container.walk(Routine)[0]
    assert rsym.is_elemental
    assert not rsym.is_pure
    output = fortran_writer(container)
    # PSyclone doesn't output with the type as part of the function
    # declaration
    assert "impure elemental function" in output


def test_fw_routine_prefixes_nomodule(fortran_reader, fortran_writer):
    '''
        Test for routine prefixes outside of a module.
    '''
    code = '''impure elemental subroutine sub()
    end subroutine sub'''
    container = fortran_reader.psyir_from_source(code)
    output = fortran_writer(container)
    assert "impure elemental subroutine sub" in output


def test_fw_standalone_routine(fortran_reader, fortran_writer):
    '''
    Test for a routine that is not part of a PSyIR tree.
    '''
    code = '''subroutine sub()
    integer :: a
    a = 1
    end subroutine sub'''
    container = fortran_reader.psyir_from_source(code)
    routine = container.children[0]
    routine.detach()
    output = fortran_writer(routine)
    correct = '''subroutine sub()
  integer :: a

  a = 1

end subroutine sub
'''
    assert correct == output
