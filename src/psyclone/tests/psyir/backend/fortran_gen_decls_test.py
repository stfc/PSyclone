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
# Authors: R. W. Ford, A. R. Porter and S. Siso, STFC Daresbury Lab
# -----------------------------------------------------------------------------

''' Performs pytest tests on the 'gen_decls()' method of the fortran
    PSyIR backend. '''

import pytest
from psyclone.errors import InternalError
from psyclone.psyir.backend.visitor import VisitorError
from psyclone.psyir.nodes import (
    BinaryOperation, Container, IntrinsicCall, Literal, Reference, Routine,
    Return)
from psyclone.psyir.symbols import (
    ArrayType, DataSymbol, DataTypeSymbol, ContainerSymbol,
    GenericInterfaceSymbol,
    RoutineSymbol, ScalarType, Symbol, SymbolTable, UnresolvedType,
    StructureType, ImportInterface, UnresolvedInterface, ArgumentInterface,
    INTEGER_TYPE, REAL_TYPE, StaticInterface, PreprocessorInterface,
    CHARACTER_TYPE)


def test_gen_param_decls_dependencies(fortran_writer):
    ''' Test that dependencies between parameter declarations are handled. '''
    symbol_table = SymbolTable()
    rlg_sym = DataSymbol("rlg", INTEGER_TYPE, is_constant=True,
                         initial_value=Literal("8", INTEGER_TYPE))
    wp_sym = DataSymbol("wp", INTEGER_TYPE, is_constant=True,
                        initial_value=Reference(rlg_sym))
    var_sym = DataSymbol("var", INTEGER_TYPE, is_constant=True,
                         initial_value=BinaryOperation.create(
                             BinaryOperation.Operator.ADD,
                             Reference(rlg_sym), Reference(wp_sym)))
    symbol_table.add(var_sym)
    symbol_table.add(wp_sym)
    symbol_table.add(rlg_sym)
    result = fortran_writer._gen_parameter_decls(symbol_table)
    assert (result == "integer, parameter :: rlg = 8\n"
                      "integer, parameter :: wp = rlg\n"
                      "integer, parameter :: var = rlg + wp\n")
    # A Symbol can depend upon itself. We can't currently create such a Symbol
    # directly (because we need the symbol to exist in order to make a
    # Reference to it).
    circ_sym = DataSymbol("circle", INTEGER_TYPE, is_constant=True,
                          initial_value=IntrinsicCall.create(
                              IntrinsicCall.Intrinsic.HUGE,
                              [Reference(var_sym)]))
    # Now that we have the Symbol, update the initial value to refer to it.
    circ_sym.initial_value.arguments[0].replace_with(Reference(circ_sym))
    symbol_table.add(circ_sym)
    result = fortran_writer._gen_parameter_decls(symbol_table)
    assert (result == "integer, parameter :: rlg = 8\n"
                      "integer, parameter :: wp = rlg\n"
                      "integer, parameter :: var = rlg + wp\n"
                      "integer, parameter :: circle = HUGE(circle)\n")

    # Check that an (invalid, obviously) circular dependency is handled.
    # Replace "rlg" with a new one that depends on "wp".
    del symbol_table._symbols[rlg_sym.name]
    rlg_sym = DataSymbol("rlg", INTEGER_TYPE, is_constant=True,
                         initial_value=Reference(wp_sym))
    symbol_table.add(rlg_sym)
    with pytest.raises(VisitorError) as err:
        fortran_writer._gen_parameter_decls(symbol_table)
    assert ("Unable to satisfy dependencies for the declarations of ['var', "
            "'wp', 'rlg']" in str(err.value))


def test_gen_param_decls_imported_dep(fortran_reader, fortran_writer):
    ''' Check that the dependency handling doesn't generate a false positive
    for a dependence on an imported symbol. '''
    code = ("program my_prog\n"
            "  use my_kinds_mod\n"
            "  use other_kinds_mod, only: rdef\n"
            "  integer, parameter :: fbdp = wp\n"
            "  integer, parameter :: obdp = rdef\n"
            "end program my_prog\n")
    psyir = fortran_reader.psyir_from_source(code)
    table = psyir.walk(Routine)[0].symbol_table
    result = fortran_writer._gen_parameter_decls(table)
    assert result == ("integer, parameter :: fbdp = wp\n"
                      "integer, parameter :: obdp = rdef\n")


def test_gen_param_decls_kind_dep(fortran_writer):
    ''' Check that symbols defining precision are accounted for when
    allowing for dependencies between parameter declarations. '''
    table = SymbolTable()
    rdef_sym = DataSymbol("r_def", INTEGER_TYPE, is_constant=True,
                          initial_value=Literal("4", INTEGER_TYPE))
    wp_sym = DataSymbol("wp", INTEGER_TYPE, is_constant=True,
                        initial_value=Reference(rdef_sym))
    rdef_type = ScalarType(ScalarType.Intrinsic.REAL, Reference(wp_sym))
    var_sym = DataSymbol("var", rdef_type, is_constant=True,
                         initial_value=Literal("1.0", rdef_type))
    var2_sym = DataSymbol("var2", REAL_TYPE, is_constant=True,
                          initial_value=Literal("1.0", rdef_type))
    table.add(var2_sym)
    table.add(var_sym)
    table.add(wp_sym)
    table.add(rdef_sym)
    result = fortran_writer._gen_parameter_decls(table)
    assert result == ("integer, parameter :: r_def = 4\n"
                      "integer, parameter :: wp = r_def\n"
                      "real, parameter :: var2 = 1.0_wp\n"
                      "real(kind=wp), parameter :: var = 1.0_wp\n")


def test_gen_param_decls_case_insensitive(fortran_reader,
                                          fortran_writer):
    '''
    Checks that _gen_parameter_decls is not case sensitive. We have to
    use the fortran frontend to exercise this.

    '''
    psyir = fortran_reader.psyir_from_source('''\
module my_mod
  implicit none
  integer(kind=AN_int), parameter :: a_second_int = 5_i_def
  integer, parameter :: an_int=6
  Integer, Parameter :: InterpolationLevels(nfieldnames3d) = &
                  [ 2, 0, Huge(InterpolationLevels)/3, 0 ]
  integer, parameter :: nFieldNames3d = 4
  integer, parameter :: I_DEF = 4
end module my_mod''')
    container = psyir.walk(Container)[1]
    result = fortran_writer._gen_parameter_decls(container.symbol_table)
    assert result == (
        "integer, parameter :: an_int = 6\n"
        "integer, parameter :: i_def = 4\n"
        "integer(kind=an_int), parameter :: a_second_int = 5_i_def\n"
        "integer, parameter :: nfieldnames3d = 4\n"
        "integer, dimension(nfieldnames3d), parameter :: "
        "interpolationlevels = [2, 0, HUGE(InterpolationLevels) / 3, 0]\n")


def test_gen_decls(fortran_writer):
    '''Check the FortranWriter class gen_decls method produces the
    expected declarations. Also check that an exception is raised if
    an 'argument' symbol exists in the supplied symbol table and the
    optional argument 'is_module_scope' is set to True.

    '''
    symbol_table = SymbolTable()
    symbol_table.add(ContainerSymbol("my_module"))
    use_statement = DataSymbol("my_use", UnresolvedType(),
                               interface=ImportInterface(
                                   symbol_table.lookup("my_module")))
    symbol_table.add(use_statement)
    local_variable = DataSymbol("local", INTEGER_TYPE)
    symbol_table.add(local_variable)
    dtype = StructureType.create([
        ("flag", INTEGER_TYPE, Symbol.Visibility.PUBLIC, None)])
    dtype_variable = DataTypeSymbol("field", dtype)
    symbol_table.add(dtype_variable)
    grid_type = DataTypeSymbol("grid_type", UnresolvedType(),
                               interface=ImportInterface(
                                   symbol_table.lookup("my_module")))
    symbol_table.add(grid_type)
    grid_variable = DataSymbol("grid", grid_type)
    symbol_table.add(grid_variable)
    symbol_table.add(DataSymbol("rlg", INTEGER_TYPE, is_constant=True,
                                initial_value=Literal("8", INTEGER_TYPE)))
    result = fortran_writer.gen_decls(symbol_table)
    # If derived type declaration is not inside a module then its components
    # cannot have accessibility attributes.
    assert (result == "integer :: local\n"
                      "type :: field\n"
                      "  integer :: flag\n"
                      "end type field\n"
                      "type(grid_type) :: grid\n"
                      "integer, parameter :: rlg = 8\n")
    # Repeat but specify that these declarations are within a module.
    result = fortran_writer.gen_decls(symbol_table, is_module_scope=True)
    assert (result == 
                      "type, public :: field\n"
                      "  integer, public :: flag\n"
                      "end type field\n"
                      "integer, public :: local\n"
                      "type(grid_type), public :: grid\n"
                      "integer, parameter, public :: rlg = 8\n")
    # Add a Symbol with an argument interface.
    argument_variable = DataSymbol("arg", INTEGER_TYPE,
                                   interface=ArgumentInterface())
    symbol_table.add(argument_variable)
    result = fortran_writer.gen_decls(symbol_table)
    assert (result == "integer, parameter :: rlg = 8\n"
                      "integer :: arg\n"
                      "type :: field\n"
                      "  integer :: flag\n"
                      "end type field\n"
                      "integer :: local\n"
                      "type(grid_type) :: grid\n")
    result = fortran_writer.gen_decls(symbol_table)

    # Add a Symbol with PreprocessorInterface which has to be ignored by
    # the gen_decl method (as no declarations is needed)
    preprocessor_variable = DataSymbol("__LINE__", CHARACTER_TYPE,
                                       interface=PreprocessorInterface())
    symbol_table.add(preprocessor_variable)
    result = fortran_writer.gen_decls(symbol_table)
    assert (result == "integer, parameter :: rlg = 8\n"
                      "integer :: arg\n"
                      "type :: field\n"
                      "  integer :: flag\n"
                      "end type field\n"
                      "integer :: local\n"
                      "type(grid_type) :: grid\n")

    # We can't have an argument if these declarations are in a module.
    with pytest.raises(VisitorError) as excinfo:
        _ = fortran_writer.gen_decls(symbol_table, is_module_scope=True)
    assert ("Arguments are not allowed in this context but this symbol table "
            "contains argument(s): '['arg']'." in str(excinfo.value))

    # Add a symbol with a deferred (unknown) interface
    symbol_table.add(DataSymbol("unknown", INTEGER_TYPE,
                                interface=UnresolvedInterface()))
    with pytest.raises(VisitorError) as excinfo:
        _ = fortran_writer.gen_decls(symbol_table)
    assert ("The following symbols are not explicitly declared or imported "
            "from a module and there are no wildcard "
            "imports which could be bringing them into scope: "
            "'unknown'" in str(excinfo.value))


def test_gen_decls_array(fortran_writer):
    '''
    Test that various forms of array declaration are created correctly.
    '''
    atype = ArrayType(REAL_TYPE, [3, 5])
    symbol_table = SymbolTable()
    symbol_table.add(DataSymbol("simple", atype))
    result = fortran_writer.gen_decls(symbol_table)
    assert "real, dimension(3,5) :: simple" in result
    # With range
    sym = symbol_table.new_symbol("upper", symbol_type=DataSymbol,
                                  datatype=INTEGER_TYPE)
    atype = ArrayType(REAL_TYPE, [(3, 5), (-1, Reference(sym))])
    symbol_table.add(DataSymbol("simple2", atype))
    result = fortran_writer.gen_decls(symbol_table)
    assert "real, dimension(3:5,-1:upper) :: simple2" in result
    # Only an explicit lower bound.
    atype = ArrayType(REAL_TYPE, [(3, ArrayType.Extent.ATTRIBUTE)])
    symbol_table.add(DataSymbol("simple3", atype))
    result = fortran_writer.gen_decls(symbol_table)
    assert "real, dimension(3:) :: simple3" in result
    # With default lower bound.
    atype = ArrayType(REAL_TYPE, [(1, ArrayType.Extent.ATTRIBUTE)])
    symbol_table.add(DataSymbol("simple4", atype))
    result = fortran_writer.gen_decls(symbol_table)
    assert "real, dimension(:) :: simple4" in result


def test_gen_decls_nested_scope(fortran_writer):
    ''' Test that gen_decls() correctly checks for potential wildcard imports
    of an unresolved symbol in an outer scope.

    '''
    inner_table = SymbolTable()
    inner_table.add(DataSymbol("unknown1", INTEGER_TYPE,
                               interface=UnresolvedInterface()))
    routine = Routine.create("my_func", inner_table, [Return()])
    cont_table = SymbolTable()
    _ = Container.create("my_mod", cont_table, [routine])

    cont_table.add(ContainerSymbol("my_module"))
    # Innermost symbol table contains "unknown1" and there's no way it can
    # be brought into scope
    with pytest.raises(VisitorError) as err:
        fortran_writer.gen_decls(inner_table)
    assert ("symbols are not explicitly declared or imported from a module "
            "and there are no wildcard imports which "
            "could be bringing them into scope: 'unknown1'" in str(err.value))
    # Add a ContainerSymbol with a wildcard import in the outermost scope
    csym = ContainerSymbol("other_mod")
    csym.wildcard_import = True
    cont_table.add(csym)
    # The inner symbol table contains a symbol with an unresolved interface
    # but nothing that requires an actual declaration
    result = fortran_writer.gen_decls(inner_table)
    assert result == ""
    # Move the wildcard import into the innermost table
    cont_table.remove(csym)
    inner_table.add(csym)
    result = fortran_writer.gen_decls(inner_table)
    assert result == ""


def test_gen_decls_routine_unresolved(fortran_writer):
    '''Test that the gen_decls method accepts routine symbols with
    unresolved interfaces.

    '''
    symbol_table = SymbolTable()
    # Check that a RoutineSymbol representing an intrinsic is OK
    symbol_table.add(RoutineSymbol("nint", interface=UnresolvedInterface()))
    result = fortran_writer.gen_decls(symbol_table)
    assert result == ""

    # Check that a RoutineSymbol representing a user-defined Routine is OK
    symbol_table.add(RoutineSymbol("my_sub", interface=UnresolvedInterface()))
    result = fortran_writer.gen_decls(symbol_table)
    assert result == ""

    # Check that a routine 'symbol' resulting from a call to a type-
    # bound procedure is quietly ignored.
    symbol_table.add(RoutineSymbol("grid%init",
                                   interface=UnresolvedInterface()))
    result = fortran_writer.gen_decls(symbol_table)
    assert result == ""


def test_gen_decls_name_too_long(fortran_writer):
    '''Test that the expected error is raised when the supplied table
    contains a Symbol with a name longer than the internal limit.
    '''
    table = SymbolTable()
    bad_name = 100*"a"
    table.add(DataSymbol(bad_name, INTEGER_TYPE))
    with pytest.raises(VisitorError) as err:
        fortran_writer.gen_decls(table)
    assert (f"Found a symbol '{bad_name}' with a name greater than "
            f"{fortran_writer.MAX_VARIABLE_NAME_LENGTH} characters in length"
            in str(err.value))


def test_gen_decls_routine_wrong_interface(fortran_writer):
    '''Test that the gen_decls method raises an exception if the interface
    of a routine symbol is of the wrong type.

    '''
    symbol_table = SymbolTable()
    # Add a routine symbol with an unsupported ArgumentInterface
    rsym = RoutineSymbol("arg_sub", interface=ArgumentInterface())
    symbol_table.add(rsym)
    with pytest.raises(VisitorError) as info:
        _ = fortran_writer.gen_decls(symbol_table)
    assert (" Routine symbol 'arg_sub' has 'Argument(Access.UNKNOWN)'. This "
            "is not supported by the Fortran back-end." in str(info.value))


def test_gen_decls_static_variables(fortran_writer):
    '''Test that the gen_decls and gen_vardecl methods add the appropriate
    Fortran attributes to static variables.

    '''
    symbol_table = SymbolTable()
    sym = DataSymbol("v1", datatype=INTEGER_TYPE, interface=StaticInterface())
    symbol_table.add(sym)
    assert "integer, save :: v1" in fortran_writer.gen_decls(symbol_table)
    assert "integer, save :: v1" in fortran_writer.gen_vardecl(sym)
    sym.initial_value = 1
    sym.is_constant = True
    assert "parameter :: v1 = 1" in fortran_writer.gen_vardecl(sym)


def test_gen_decls_comments(fortran_writer):
    '''Test that the gen_vardecl method adds comments to the Fortran code
    when the symbol has a description.

    '''
    sym = DataSymbol("v1", datatype=INTEGER_TYPE,
                     initial_value=Literal("1", INTEGER_TYPE),
                     is_constant=True)
    sym.preceding_comment = "Preceding comment"
    sym.inline_comment = "Inline comment"
    result = fortran_writer.gen_vardecl(sym)
    expected = ("! Preceding comment\n"
                "integer, parameter :: v1 = 1 ! Inline comment")
    assert expected in result

    sym2 = DataSymbol("v2", datatype=INTEGER_TYPE,
                      initial_value=Literal("2", INTEGER_TYPE),
                      is_constant=True)
    sym2.preceding_comment = "Preceding comment\nwith newline"
    sym2.inline_comment = "Inline comment"
    result = fortran_writer.gen_vardecl(sym2)
    expected = ("! Preceding comment\n"
                "! with newline\n"
                "integer, parameter :: v2 = 2 ! Inline comment")
    assert expected in result


@pytest.mark.parametrize("visibility", ["public", "private"])
def test_visibility_abstract_interface(fortran_reader, fortran_writer,
                                       visibility):
    '''Test that PSyclone's Fortran backend successfully writes out
    public/private clauses and symbols when the symbol's declaration
    is hidden in an abstract interface.

    '''
    code = (
        f"module test\n"
        f"  abstract interface\n"
        f"     subroutine update_interface()\n"
        f"     end subroutine update_interface\n"
        f"  end interface\n"
        f"  {visibility} :: update_interface\n"
        f"contains\n"
        f"  subroutine alg()\n"
        f"  end subroutine alg\n"
        f"end module test\n")
    psyir = fortran_reader.psyir_from_source(code)
    result = fortran_writer(psyir)
    # The default visibility is PUBLIC so it is always output by
    # the backend.
    assert "public\n" in result
    if visibility == "public":
        # The generic PUBLIC visibility covers all symbols so we do
        # not need to output "public :: update_interface".
        assert "public :: update_interface" not in result
    if visibility == "private":
        assert "private :: update_interface" in result


def test_procedure_interface(fortran_writer):
    '''Test that the Fortran backend correctly recreates an interface
    declaration from a GenericInterfaceSymbol.
    '''
    symbol_table = SymbolTable()
    isub = GenericInterfaceSymbol("subx", [(RoutineSymbol("sub1"), False),
                                           (RoutineSymbol("sub2"), True)])
    symbol_table.add(isub)
    out = fortran_writer.gen_decls(symbol_table)
    assert "interface subx" in out
    assert "procedure :: sub1" in out
    assert "module procedure :: sub2" in out
    assert "end interface subx" in out


def test_gen_interfacedecl(fortran_writer):
    '''
    Test the gen_interfacedecl() method directly. That it raises the expected
    error if not supplied with a GenericInterfaceSymbol but otherwise generates
    correct Fortran.

    '''
    with pytest.raises(InternalError) as err:
        fortran_writer.gen_interfacedecl("not a symbol")
    assert ("gen_interfacedecl only supports 'GenericInterfaceSymbol's but "
            "got 'str'" in str(err.value))
    isub = GenericInterfaceSymbol("subx", [(RoutineSymbol("sub1"), False),
                                           (RoutineSymbol("sub2"), True)])
    out = fortran_writer.gen_interfacedecl(isub)
    assert (out == '''interface subx
  module procedure :: sub2
  procedure :: sub1
end interface subx
''')
