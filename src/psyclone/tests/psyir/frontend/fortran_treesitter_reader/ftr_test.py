# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

''' Performs tests on the treesitter PSyIR front-end '''
import logging
import pytest

from tree_sitter import Node as TSNode

from psyclone.psyir.frontend.fortran_treesitter_reader import \
    FortranTreeSitterReader
from psyclone.psyir import nodes as psyir_nodes, symbols as psyir_symbols
from psyclone.tests.utilities import min_version_3_10

# TODO #3416: Skip treesitter tests below 3.10 as they're unsupported by
# treesitter.
pytestmark = min_version_3_10


def test_constructor():
    ''' Test the constructor and its arguments '''
    processor = FortranTreeSitterReader()

    # Check default arguments
    assert processor._ignore_directives is True
    assert processor._resolve_modules is False
    assert processor._last_comments_as_codeblocks is False

    # Currently arguments are ignored (they are just accepted for compatibility
    # with the fparser2 interface)
    processor = FortranTreeSitterReader(
        ignore_directives=False,
        resolve_modules=True,
        last_comments_as_codeblocks=True,
    )
    assert processor._ignore_directives is False
    assert processor._resolve_modules is True
    assert processor._last_comments_as_codeblocks is True

    # TODO #3038 Typecheck arguments


def test_generate_parse_tree(tmpdir_factory, caplog):
    '''
    Test that generate_parse_tree returns treesitter trees or appropriate
    error messages.
    '''
    processor = FortranTreeSitterReader()

    # Valid code returns a treesitter Node
    valid_code = """
        program test
        end program test
    """
    ptree = processor.generate_parse_tree_from_source(valid_code)
    assert isinstance(ptree, TSNode)

    # Invalid code raises a Value error with a relevant error message
    invalid_code = """
        program test
            syntax error
        end program test
    """
    with pytest.raises(ValueError) as err:
        _ = processor.generate_parse_tree_from_source(invalid_code)
    assert "Syntax Error found at line" in str(err.value)

    # Test providing a source file
    filename = str(tmpdir_factory.mktemp('ts_test').join("testfile.f90"))
    with open(filename, "w", encoding='utf-8') as wfile:
        wfile.write(valid_code)
    ptree = processor.generate_parse_tree_from_file(filename)
    assert isinstance(ptree, TSNode)

    # Test providing a source file with a non utf-8 encoding
    valid_code = valid_code + "\n! Comment with character \xfc"
    filename = str(tmpdir_factory.mktemp('ts_test').join("testfile2.f90"))
    with open(filename, "w", encoding='cp1252') as wfile:
        wfile.write(valid_code)

    with caplog.at_level(logging.WARNING,
                         "psyclone.psyir.frontend.fortran_treesitter_reader"):
        ptree = processor.generate_parse_tree_from_file(filename)
    assert ("Skipped bad character in input file, 'utf-8' codec can't "
            "decode byte 0xfc in position 77" in caplog.text)
    assert isinstance(ptree, TSNode)


def test_generate_psyir():
    '''
    Test that generate_psyir transforms treesitter parse trees to
    PSyIR nodes.
    '''
    processor = FortranTreeSitterReader()

    valid_code = """
    module test
        implicit none
        contains
        subroutine mysub()
        end subroutine
    end module test
    """
    ptree = processor.generate_parse_tree_from_source(valid_code)
    root = processor.generate_psyir(ptree)

    assert isinstance(root, psyir_nodes.FileContainer)
    assert isinstance(root.children[0], psyir_nodes.Container)
    assert isinstance(root.children[0].children[0], psyir_nodes.Routine)
    assert root.children[0].children[0].name == "mysub"


def test_routine_host_association():
    '''Test that a contained routine resolves a symbol from its host.'''
    processor = FortranTreeSitterReader()
    valid_code = """
        module host
          implicit none
          integer :: count
        contains
          subroutine work()
            count = count + 1
          end subroutine work
        end module host
    """
    root = processor.generate_psyir(
        processor.generate_parse_tree_from_source(valid_code))
    container = root.children[0]
    routine = container.children[0]
    count = container.symbol_table.lookup("count")
    assert "count" not in routine.symbol_table
    assert [ref.symbol for ref in routine.walk(psyir_nodes.Reference)] == [
        count, count]


def test_subroutine():
    '''
    Test subroutine nodes.
    '''
    processor = FortranTreeSitterReader()

    valid_code = """
        subroutine mysub()
        end subroutine
        subroutine mysub2()
        end subroutine mysub2
    """
    ptree = processor.generate_parse_tree_from_source(valid_code)
    root = processor.generate_psyir(ptree)

    # Check the tree is as expected
    assert len(root.children) == 2
    assert isinstance(root.children[0], psyir_nodes.Routine)
    assert root.children[0].name == "mysub"
    assert isinstance(root.children[1], psyir_nodes.Routine)
    assert root.children[1].name == "mysub2"

    # Check that the symbols have been added to the symbol table
    assert len(root.symbol_table.symbols) == 2
    rsymbol1 = root.symbol_table.lookup("mysub")
    rsymbol2 = root.symbol_table.lookup("mysub2")
    assert root.children[0].symbol is rsymbol1
    assert root.children[1].symbol is rsymbol2
    assert isinstance(rsymbol1, psyir_symbols.RoutineSymbol)
    assert isinstance(rsymbol2, psyir_symbols.RoutineSymbol)


def test_declarations():
    '''
    Test subroutine nodes.
    '''
    processor = FortranTreeSitterReader()

    valid_code = """
        module test
            implicit none
            integer :: a
            real :: b
        end module
    """
    ptree = processor.generate_parse_tree_from_source(valid_code)
    root = processor.generate_psyir(ptree)
    module = root.children[0]

    # Declarations do not add children nodes
    assert len(module.children) == 0

    # Check that the symbols have been added to the symbol table
    assert len(module.symbol_table.symbols) == 2
    assert "a" in module.symbol_table
    assert "b" in module.symbol_table


@pytest.mark.parametrize("fortran_type,psyir_type", [
    ("integer", psyir_symbols.ScalarType.integer_type()),
    ("integer(kind=4)", psyir_symbols.ScalarType.integer_single_type()),
    ("integer(8)", psyir_symbols.ScalarType.integer_double_type()),
    ("real", psyir_symbols.ScalarType.real_type()),
    ("real(4)", psyir_symbols.ScalarType.real_single_type()),
    ("real(kind=8)", psyir_symbols.ScalarType.real_double_type()),
    ("logical", psyir_symbols.ScalarType.boolean_type()),
    ("character", psyir_symbols.ScalarType.character_type()),
])
def test_declarations_datatypes(fortran_type, psyir_type):
    '''
    Test subroutine nodes.
    '''
    processor = FortranTreeSitterReader()

    valid_code = f"""
        module test
            implicit none
            {fortran_type} :: a
        end module
    """
    ptree = processor.generate_parse_tree_from_source(valid_code)
    root = processor.generate_psyir(ptree)
    module = root.children[0]
    assert module.symbol_table.lookup("a").datatype == psyir_type, (
        f"{module.symbol_table.lookup('a').datatype} != {psyir_type}"
    )


@pytest.mark.parametrize("shape_string, extent", [
    ("(:)", psyir_symbols.ArrayType.Extent.ATTRIBUTE),
    ("(10)", "10"),
])
def test_declarations_arrays_datatypes(shape_string, extent):
    '''
    Test subroutine nodes.
    '''
    processor = FortranTreeSitterReader()

    valid_code = f"""
        module test
            implicit none
            integer(4), dimension{shape_string} :: a
        end module
    """
    ptree = processor.generate_parse_tree_from_source(valid_code)
    root = processor.generate_psyir(ptree)
    module = root.children[0]

    array_symbol = module.symbol_table.lookup("a")
    assert isinstance(array_symbol.datatype, psyir_symbols.ArrayType)
    assert (array_symbol.datatype.elemental_type ==
            psyir_symbols.ScalarType.integer_single_type())
    shape = array_symbol.datatype.shape[0]
    if isinstance(extent, str):
        assert shape.upper.value == extent
        assert shape.lower.value == "1"
    else:
        assert shape == extent


def test_program():
    '''Test a main-program unit.'''
    processor = FortranTreeSitterReader()
    valid_code = """
        program main
          implicit none
        end program main
    """
    root = processor.generate_psyir(
        processor.generate_parse_tree_from_source(valid_code))
    routine = root.children[0]
    assert isinstance(routine, psyir_nodes.Routine)
    assert routine.is_program
    assert routine.name == "main"


def test_use_rename():
    '''Test a renamed symbol in a USE ONLY statement.'''
    processor = FortranTreeSitterReader()
    valid_code = """
        program main
          use kinds, only: local_kind => remote_kind
          implicit none
        end program main
    """
    root = processor.generate_psyir(
        processor.generate_parse_tree_from_source(valid_code))
    table = root.children[0].symbol_table
    container = table.lookup("kinds")
    imported = table.lookup("local_kind")
    assert isinstance(container, psyir_symbols.ContainerSymbol)
    assert imported.interface.container_symbol is container
    assert imported.interface.orig_name == "remote_kind"


def test_symbolic_kind():
    '''Test a symbolic kind expression.'''
    processor = FortranTreeSitterReader()
    valid_code = """
        program main
          use kinds, only: local_kind
          implicit none
          integer(local_kind) :: value
        end program main
    """
    root = processor.generate_psyir(
        processor.generate_parse_tree_from_source(valid_code))
    table = root.children[0].symbol_table
    kind = table.lookup("local_kind")
    assert isinstance(kind, psyir_symbols.DataSymbol)
    assert table.lookup("value").datatype.precision.symbol is kind


def test_parameter_declaration():
    '''Test a named constant declaration.'''
    processor = FortranTreeSitterReader()
    valid_code = """
        program main
          implicit none
          integer, parameter :: count = 4
        end program main
    """
    root = processor.generate_psyir(
        processor.generate_parse_tree_from_source(valid_code))
    count = root.children[0].symbol_table.lookup("count")
    assert count.is_constant
    assert count.initial_value.value == "4"


def test_character_length():
    '''Test a character-length specification.'''
    processor = FortranTreeSitterReader()
    valid_code = """
        program main
          implicit none
          character(len=12) :: label
        end program main
    """
    root = processor.generate_psyir(
        processor.generate_parse_tree_from_source(valid_code))
    label = root.children[0].symbol_table.lookup("label")
    assert label.datatype.length.value == "12"


def test_allocatable_declaration():
    '''Test an allocatable-array declaration.'''
    processor = FortranTreeSitterReader()
    valid_code = """
        program main
          implicit none
          real, allocatable :: values(:)
        end program main
    """
    root = processor.generate_psyir(
        processor.generate_parse_tree_from_source(valid_code))
    values = root.children[0].symbol_table.lookup("values")
    assert values.datatype.shape == [
        psyir_symbols.ArrayType.Extent.DEFERRED]


def test_function_result():
    '''Test a named function-result symbol.'''
    processor = FortranTreeSitterReader()
    valid_code = """
        real function square(value) result(answer)
          real :: value
          answer = value * value
        end function square
    """
    root = processor.generate_psyir(
        processor.generate_parse_tree_from_source(valid_code))
    routine = root.children[0]
    assert routine.return_symbol.name == "answer"
    assert routine.return_symbol.datatype == \
        psyir_symbols.ScalarType.real_type()


def test_argument_order():
    '''Test the order of routine arguments.'''
    processor = FortranTreeSitterReader()
    valid_code = """
        subroutine update(first, second)
          real :: first, second
        end subroutine update
    """
    root = processor.generate_psyir(
        processor.generate_parse_tree_from_source(valid_code))
    table = root.children[0].symbol_table
    assert [symbol.name for symbol in table.argument_list] == [
        "first", "second"]


@pytest.mark.parametrize("intent,access", [
    ("in", psyir_symbols.ArgumentInterface.Access.READ),
    ("out", psyir_symbols.ArgumentInterface.Access.WRITE),
    ("inout", psyir_symbols.ArgumentInterface.Access.READWRITE),
])
def test_argument_intent(intent, access):
    '''Test an INTENT attribute on a routine argument.'''
    processor = FortranTreeSitterReader()
    valid_code = f"""
        subroutine update(value)
          real, intent({intent}) :: value
        end subroutine update
    """
    root = processor.generate_psyir(
        processor.generate_parse_tree_from_source(valid_code))
    value = root.children[0].symbol_table.lookup("value")
    assert value.interface.access == access


def test_pure_function():
    '''Test the PURE function qualifier.'''
    processor = FortranTreeSitterReader()
    valid_code = """
        pure real function identity(value)
          real :: value
          identity = value
        end function identity
    """
    root = processor.generate_psyir(
        processor.generate_parse_tree_from_source(valid_code))
    assert root.children[0].symbol.is_pure is True


def test_elemental_function():
    '''Test the ELEMENTAL function qualifier.'''
    processor = FortranTreeSitterReader()
    valid_code = """
        elemental real function identity(value)
          real :: value
          identity = value
        end function identity
    """
    root = processor.generate_psyir(
        processor.generate_parse_tree_from_source(valid_code))
    assert root.children[0].symbol.is_elemental is True


def test_unsupported_complex_datatype():
    '''Test the unsupported complex datatype.'''
    processor = FortranTreeSitterReader()
    valid_code = """
        module declarations
          implicit none
          complex :: coefficient
        end module declarations
    """
    root = processor.generate_psyir(
        processor.generate_parse_tree_from_source(valid_code))
    coefficient = root.children[0].symbol_table.lookup("coefficient")
    assert isinstance(coefficient.datatype,
                      psyir_symbols.UnsupportedFortranType)
    assert coefficient.datatype.declaration == "complex :: coefficient"


def test_unsupported_pointer_datatype():
    '''Test entity-specific unsupported pointer datatypes.'''
    processor = FortranTreeSitterReader()
    valid_code = """
        module declarations
          implicit none
          integer, pointer :: first, second
        end module declarations
    """
    root = processor.generate_psyir(
        processor.generate_parse_tree_from_source(valid_code))
    table = root.children[0].symbol_table
    for name in ("first", "second"):
        datatype = table.lookup(name).datatype
        assert isinstance(datatype, psyir_symbols.UnsupportedFortranType)
        assert datatype.declaration == f"integer, pointer :: {name}"


def test_unsupported_initialisation_is_entity_specific():
    '''Test that one unsupported initializer does not affect its sibling.'''
    processor = FortranTreeSitterReader()
    valid_code = """
        module declarations
          implicit none
          integer :: first = [(i, i=1,2)], second = 2
        end module declarations
    """
    root = processor.generate_psyir(
        processor.generate_parse_tree_from_source(valid_code))
    table = root.children[0].symbol_table
    first = table.lookup("first")
    second = table.lookup("second")
    assert isinstance(
        first.datatype, psyir_symbols.UnsupportedFortranType)
    assert first.datatype.declaration == \
        "integer :: first = [(i, i=1,2)]"
    assert isinstance(second.datatype, psyir_symbols.ScalarType)
    assert second.initial_value.value == "2"


def test_save_attribute():
    '''Test the SAVE declaration attribute.'''
    processor = FortranTreeSitterReader()
    valid_code = """
        module declarations
          implicit none
          double precision, save :: accumulator
        end module declarations
    """
    root = processor.generate_psyir(
        processor.generate_parse_tree_from_source(valid_code))
    accumulator = root.children[0].symbol_table.lookup("accumulator")
    assert accumulator.datatype == \
        psyir_symbols.ScalarType.real_double_type()
    assert isinstance(accumulator.interface, psyir_symbols.StaticInterface)


def test_logical_literal():
    '''Test a logical literal used as an initial value.'''
    processor = FortranTreeSitterReader()
    valid_code = """
        module declarations
          implicit none
          logical :: enabled = .true.
        end module declarations
    """
    root = processor.generate_psyir(
        processor.generate_parse_tree_from_source(valid_code))
    enabled = root.children[0].symbol_table.lookup("enabled")
    assert enabled.initial_value.value == "true"


def test_derived_type_definition():
    '''Test a simple derived-type definition.'''
    processor = FortranTreeSitterReader()
    valid_code = """
        module geometry
          implicit none
          type :: point
            real :: x
          end type point
        end module geometry
    """
    root = processor.generate_psyir(
        processor.generate_parse_tree_from_source(valid_code))
    point = root.children[0].symbol_table.lookup("point")
    assert isinstance(point, psyir_symbols.DataTypeSymbol)
    assert isinstance(point.datatype, psyir_symbols.StructureType)
    assert list(point.datatype.components) == ["x"]
    assert point.datatype.components["x"].datatype == \
        psyir_symbols.ScalarType.real_type()


def test_derived_type_component_host_association():
    '''Test that a component datatype resolves a kind from its host.'''
    processor = FortranTreeSitterReader()
    valid_code = """
        module geometry
          implicit none
          integer, parameter :: wp = 8
          type :: point
            real(kind=wp) :: x
          end type point
        end module geometry
    """
    root = processor.generate_psyir(
        processor.generate_parse_tree_from_source(valid_code))
    table = root.children[0].symbol_table
    wp = table.lookup("wp")
    point = table.lookup("point")
    x_type = point.datatype.components["x"].datatype
    assert x_type.precision.symbol is wp


def test_structure_reference():
    '''Test a scalar structure-component reference.'''
    processor = FortranTreeSitterReader()
    valid_code = """
        subroutine get_x(item, value)
          type(point) :: item
          real :: value
          value = item%x
        end subroutine get_x
    """
    root = processor.generate_psyir(
        processor.generate_parse_tree_from_source(valid_code))
    reference = root.children[0].children[0].rhs
    assert isinstance(reference, psyir_nodes.StructureReference)
    assert reference.symbol.name == "item"
    assert reference.member.name == "x"


def test_array_of_structures_reference():
    '''Test an indexed array-of-structures component reference.'''
    processor = FortranTreeSitterReader()
    valid_code = """
        subroutine get_value(items, value)
          type(point) :: items(2)
          real :: value
          value = items(1)%vector(2)
        end subroutine get_value
    """
    root = processor.generate_psyir(
        processor.generate_parse_tree_from_source(valid_code))
    reference = root.children[0].children[0].rhs
    assert isinstance(reference,
                      psyir_nodes.ArrayOfStructuresReference)
    assert reference.indices[0].value == "1"
    assert reference.member.name == "vector"
    assert reference.member.indices[0].value == "2"


def test_multidimensional_explicit_array_bounds():
    '''Test explicit lower bounds in a multidimensional shape.'''
    processor = FortranTreeSitterReader()
    valid_code = """
        module array_shapes
          implicit none
          real :: field(-2:10, 20)
        end module array_shapes
    """
    root = processor.generate_psyir(
        processor.generate_parse_tree_from_source(valid_code))
    datatype = root.children[0].symbol_table.lookup("field").datatype
    assert isinstance(datatype, psyir_symbols.ArrayType)
    assert len(datatype.shape) == 2
    assert datatype.shape[0].lower.operator == \
        psyir_nodes.UnaryOperation.Operator.MINUS
    assert datatype.shape[0].lower.children[0].value == "2"
    assert datatype.shape[0].upper.value == "10"
    assert datatype.shape[1].lower.value == "1"
    assert datatype.shape[1].upper.value == "20"


def test_unary_operation():
    '''Test a unary arithmetic operation.'''
    processor = FortranTreeSitterReader()
    valid_code = """
        subroutine operations(value)
          real :: value
          value = -value
        end subroutine operations
    """
    root = processor.generate_psyir(
        processor.generate_parse_tree_from_source(valid_code))
    expression = root.children[0].children[0].rhs
    assert expression.operator == psyir_nodes.UnaryOperation.Operator.MINUS


def test_binary_operation():
    '''Test a binary arithmetic operation.'''
    processor = FortranTreeSitterReader()
    valid_code = """
        subroutine operations(value)
          real :: value
          value = value + 1.0
        end subroutine operations
    """
    root = processor.generate_psyir(
        processor.generate_parse_tree_from_source(valid_code))
    expression = root.children[0].children[0].rhs
    assert expression.operator == psyir_nodes.BinaryOperation.Operator.ADD


def test_explicit_array_section():
    '''Test an array section with explicit start, stop and step.'''
    processor = FortranTreeSitterReader()
    valid_code = """
        subroutine section(array)
          real :: array(10)
          array(2:8:2) = 0.0
        end subroutine section
    """
    root = processor.generate_psyir(
        processor.generate_parse_tree_from_source(valid_code))
    section = root.children[0].children[0].lhs.indices[0]
    assert isinstance(section, psyir_nodes.Range)
    assert section.start.value == "2"
    assert section.stop.value == "8"
    assert section.step.value == "2"


def test_implicit_array_section_bounds():
    '''Test synthesized bounds for a whole-array section.'''
    processor = FortranTreeSitterReader()
    valid_code = """
        subroutine section(array)
          real :: array(10)
          array(:) = array(:)
        end subroutine section
    """
    root = processor.generate_psyir(
        processor.generate_parse_tree_from_source(valid_code))
    assignment = root.children[0].children[0]
    for reference in (assignment.lhs, assignment.rhs):
        section = reference.indices[0]
        assert section.start.intrinsic == \
            psyir_nodes.IntrinsicCall.Intrinsic.LBOUND
        assert section.stop.intrinsic == \
            psyir_nodes.IntrinsicCall.Intrinsic.UBOUND


def test_intrinsic_call():
    '''Test an intrinsic function call.'''
    processor = FortranTreeSitterReader()
    valid_code = """
        subroutine intrinsic(value)
          real :: value
          value = sin(value)
        end subroutine intrinsic
    """
    root = processor.generate_psyir(
        processor.generate_parse_tree_from_source(valid_code))
    call = root.children[0].children[0].rhs
    assert isinstance(call, psyir_nodes.IntrinsicCall)
    assert call.intrinsic == psyir_nodes.IntrinsicCall.Intrinsic.SIN


def test_named_call_argument():
    '''Test a named subroutine-call argument.'''
    processor = FortranTreeSitterReader()
    valid_code = """
        subroutine caller(value)
          integer :: value
          call update(value, result=value)
        end subroutine caller
    """
    root = processor.generate_psyir(
        processor.generate_parse_tree_from_source(valid_code))
    call = root.children[0].children[0]
    assert isinstance(call, psyir_nodes.Call)
    assert call.argument_names == [None, "result"]


def test_array_constructor():
    '''Test a simple array constructor.'''
    processor = FortranTreeSitterReader()
    valid_code = """
        subroutine constructor(values)
          real :: values(3)
          values = [1.0, 2.0, 3.0]
        end subroutine constructor
    """
    root = processor.generate_psyir(
        processor.generate_parse_tree_from_source(valid_code))
    constructor = root.children[0].children[0].rhs
    assert isinstance(constructor, psyir_nodes.ArrayConstructor)
    assert [element.value for element in constructor.children] == [
        "1.0", "2.0", "3.0"]


def test_pointer_assignment():
    '''Test a pointer assignment.'''
    processor = FortranTreeSitterReader()
    valid_code = """
        subroutine associate(target, pointer)
          integer, target :: target
          integer, pointer :: pointer
          pointer => target
        end subroutine associate
    """
    root = processor.generate_psyir(
        processor.generate_parse_tree_from_source(valid_code))
    assignment = root.children[0].children[0]
    assert isinstance(assignment, psyir_nodes.Assignment)
    assert assignment.is_pointer


def test_nullify_statement():
    '''Test a NULLIFY statement.'''
    processor = FortranTreeSitterReader()
    valid_code = """
        subroutine clear(pointer)
          integer, pointer :: pointer
          nullify(pointer)
        end subroutine clear
    """
    root = processor.generate_psyir(
        processor.generate_parse_tree_from_source(valid_code))
    nullify = root.children[0].children[0]
    assert nullify.intrinsic == psyir_nodes.IntrinsicCall.Intrinsic.NULLIFY


def test_if_construct():
    '''Test IF, ELSE IF and ELSE clauses.'''
    processor = FortranTreeSitterReader()
    valid_code = """
        subroutine conditional(value)
          integer :: value
          if (value < 0) then
            value = -value
          else if (value == 0) then
            value = 1
          else
            value = value + 1
          end if
        end subroutine conditional
    """
    root = processor.generate_psyir(
        processor.generate_parse_tree_from_source(valid_code))
    ifblock = root.children[0].children[0]
    assert isinstance(ifblock, psyir_nodes.IfBlock)
    nested = ifblock.else_body.children[0]
    assert "was_elseif" in nested.annotations
    assert nested.else_body.children[0].rhs.operator == \
        psyir_nodes.BinaryOperation.Operator.ADD


def test_counted_do_loop():
    '''Test a counted DO loop.'''
    processor = FortranTreeSitterReader()
    valid_code = """
        subroutine counted(limit)
          integer :: limit, index
          do index = 1, limit, 2
            limit = limit - 1
          end do
        end subroutine counted
    """
    root = processor.generate_psyir(
        processor.generate_parse_tree_from_source(valid_code))
    loop = root.children[0].children[0]
    assert isinstance(loop, psyir_nodes.Loop)
    assert loop.variable.name == "index"
    assert loop.start_expr.value == "1"
    assert loop.stop_expr.symbol.name == "limit"
    assert loop.step_expr.value == "2"


def test_do_while_loop():
    '''Test a DO WHILE loop.'''
    processor = FortranTreeSitterReader()
    valid_code = """
        subroutine conditional(value)
          integer :: value
          do while (value > 0)
            value = value - 1
          end do
        end subroutine conditional
    """
    root = processor.generate_psyir(
        processor.generate_parse_tree_from_source(valid_code))
    loop = root.children[0].children[0]
    assert isinstance(loop, psyir_nodes.WhileLoop)
    assert loop.condition.operator == \
        psyir_nodes.BinaryOperation.Operator.GT


def test_unconditional_do_loop():
    '''Test an unconditional DO loop.'''
    processor = FortranTreeSitterReader()
    valid_code = """
        subroutine unconditional()
          do
            return
          end do
        end subroutine unconditional
    """
    root = processor.generate_psyir(
        processor.generate_parse_tree_from_source(valid_code))
    loop = root.children[0].children[0]
    assert isinstance(loop, psyir_nodes.WhileLoop)
    assert "was_unconditional" in loop.annotations


def test_where_construct():
    '''Test a WHERE construct with ELSEWHERE.'''
    processor = FortranTreeSitterReader()
    valid_code = """
        subroutine mask(array)
          real :: array(10)
          where (array > 0.0)
            array = sqrt(array)
          elsewhere
            array = 0.0
          end where
        end subroutine mask
    """
    root = processor.generate_psyir(
        processor.generate_parse_tree_from_source(valid_code))
    where = root.children[0].children[0]
    assert isinstance(where, psyir_nodes.IfBlock)
    assert where.annotations == ["was_where"]
    assert where.else_body.children[0].rhs.value == "0.0"


def test_select_case_construct():
    '''Test SELECT CASE lowering.'''
    processor = FortranTreeSitterReader()
    valid_code = """
        subroutine select_value(selector)
          integer :: selector
          select case(selector)
          case(1)
            selector = 2
          case(3:5, 8)
            selector = 3
          case default
            selector = 0
          end select
        end subroutine select_value
    """
    root = processor.generate_psyir(
        processor.generate_parse_tree_from_source(valid_code))
    case = root.children[0].children[0]
    assert case.annotations == ["was_case"]
    assert case.condition.operator == \
        psyir_nodes.BinaryOperation.Operator.EQ
    second = case.else_body.children[0]
    assert second.condition.operator == psyir_nodes.BinaryOperation.Operator.OR
    assert second.else_body.children[0].rhs.value == "0"


def test_allocate_statement():
    '''Test an ALLOCATE statement.'''
    processor = FortranTreeSitterReader()
    valid_code = """
        subroutine allocate_array(array, extent, status)
          integer :: extent, status
          real, allocatable :: array(:)
          allocate(array(extent), stat=status)
        end subroutine allocate_array
    """
    root = processor.generate_psyir(
        processor.generate_parse_tree_from_source(valid_code))
    allocate = root.children[0].children[0]
    assert allocate.intrinsic == \
        psyir_nodes.IntrinsicCall.Intrinsic.ALLOCATE
    assert isinstance(allocate.arguments[0], psyir_nodes.ArrayReference)
    assert allocate.argument_names == [None, "stat"]


def test_deallocate_statement():
    '''Test a DEALLOCATE statement.'''
    processor = FortranTreeSitterReader()
    valid_code = """
        subroutine deallocate_array(array, status)
          integer :: status
          real, allocatable :: array(:)
          deallocate(array, stat=status)
        end subroutine deallocate_array
    """
    root = processor.generate_psyir(
        processor.generate_parse_tree_from_source(valid_code))
    deallocate = root.children[0].children[0]
    assert deallocate.intrinsic == \
        psyir_nodes.IntrinsicCall.Intrinsic.DEALLOCATE
    assert deallocate.argument_names == [None, "stat"]


def test_stop_codeblock():
    '''Test the localized fallback for a STOP statement.'''
    processor = FortranTreeSitterReader()
    valid_code = """
        subroutine stop_execution()
          stop 1
        end subroutine stop_execution
    """
    root = processor.generate_psyir(
        processor.generate_parse_tree_from_source(valid_code))
    codeblock = root.children[0].children[0]
    assert isinstance(codeblock, psyir_nodes.CodeBlock)
    assert "Unsupported 'stop_statement'" in codeblock.preceding_comment


def test_default_visibility():
    '''Test a module's default visibility.'''
    processor = FortranTreeSitterReader()
    valid_code = """
        module visibility
          implicit none
          private
        contains
          subroutine hidden()
          end subroutine hidden
        end module visibility
    """
    root = processor.generate_psyir(
        processor.generate_parse_tree_from_source(valid_code))
    container = root.children[0]
    assert container.symbol_table.default_visibility == \
        psyir_symbols.Symbol.Visibility.PRIVATE
    assert container.symbol_table.lookup("hidden").visibility == \
        psyir_symbols.Symbol.Visibility.PRIVATE


def test_named_visibility():
    '''Test name-specific visibility for a contained routine.'''
    processor = FortranTreeSitterReader()
    valid_code = """
        module visibility
          implicit none
          private
          public :: exposed
        contains
          subroutine exposed()
          end subroutine exposed
        end module visibility
    """
    root = processor.generate_psyir(
        processor.generate_parse_tree_from_source(valid_code))
    exposed = root.children[0].symbol_table.lookup("exposed")
    assert exposed.visibility == psyir_symbols.Symbol.Visibility.PUBLIC


def test_generic_interface():
    '''Test a named generic interface.'''
    processor = FortranTreeSitterReader()
    valid_code = """
        module dispatch
          implicit none
          interface apply
            module procedure apply_integer, apply_real
          end interface apply
        end module dispatch
    """
    root = processor.generate_psyir(
        processor.generate_parse_tree_from_source(valid_code))
    generic = root.children[0].symbol_table.lookup("apply")
    assert isinstance(generic, psyir_symbols.GenericInterfaceSymbol)
    assert [info.symbol.name for info in generic.routines] == [
        "apply_integer", "apply_real"]
    assert all(info.from_container for info in generic.routines)


def test_ignored_comment():
    '''Test that an ignored comment does not create a CodeBlock.'''
    processor = FortranTreeSitterReader()
    valid_code = """
        subroutine commented(value)
          integer :: value
          ! This comment must not create a CodeBlock.
          value = 1
        end subroutine commented
    """
    root = processor.generate_psyir(
        processor.generate_parse_tree_from_source(valid_code))
    routine = root.children[0]
    assert len(routine.children) == 1
    assert isinstance(routine.children[0], psyir_nodes.Assignment)


def test_implied_do_codeblock():
    '''Test the localized fallback for an implied-DO array constructor.'''
    processor = FortranTreeSitterReader()
    valid_code = """
        subroutine constructor(values)
          real :: values(3)
          integer :: index
          values = [(real(index), index=1,3)]
        end subroutine constructor
    """
    root = processor.generate_psyir(
        processor.generate_parse_tree_from_source(valid_code))
    codeblock = root.children[0].children[0]
    assert isinstance(codeblock, psyir_nodes.CodeBlock)
    assert ("Array constructors with implied-DO loops are not supported" in
            codeblock.preceding_comment)
