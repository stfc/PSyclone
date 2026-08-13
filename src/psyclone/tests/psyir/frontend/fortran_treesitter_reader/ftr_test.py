# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

''' Performs tests on the treesitter PSyIR front-end '''
import logging
from types import SimpleNamespace
import pytest

from tree_sitter import Node as TSNode

from psyclone.errors import InternalError
from psyclone.psyir.frontend import fortran_treesitter_reader as ftr
from psyclone.psyir.frontend.fortran_treesitter_reader import (
    FortranTreeSitterReader, _CommonDeclAttributes, _NodeExpectation)
from psyclone.psyir import nodes as psyir_nodes, symbols as psyir_symbols
from psyclone.tests.utilities import min_version_3_10

# TODO #3416: Skip treesitter tests below 3.10 as they're unsupported by
# treesitter.
pytestmark = min_version_3_10


def _first_tsnode(tsnode, node_type):
    '''
    :param tsnode: the tree-sitter tree to search.
    :param node_type: the type to search.

    :returns: the first tree-sitter node of the requested type.

    '''
    if tsnode.type == node_type:
        return tsnode
    for child in tsnode.children:
        result = _first_tsnode(child, node_type)
        if result:
            return result
    return None


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


def test_process_node_expectation_errors():
    '''Test defensive validation of dispatcher result expectations.'''
    valid_code = """
        subroutine assignment(first, second)
          integer :: first, second
          first = second
        end subroutine assignment
    """
    processor = FortranTreeSitterReader()
    parse_tree = processor.generate_parse_tree_from_source(valid_code)
    assignment = _first_tsnode(parse_tree, "assignment_statement")

    with pytest.raises(InternalError, match="Only one node was expected"):
        processor._process_nodes([], _NodeExpectation.ONE)
    with pytest.raises(InternalError, match="A DataNode was expected"):
        processor._process_nodes(assignment, _NodeExpectation.EXPRESSION)
    with pytest.raises(InternalError, match="Unsupported node expectation"):
        processor._process_nodes([], None)


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


def test_routines_nodes():
    ''' Test that routine nodes create a node and delcare a symbol  '''
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


def test_routine_symbol_association():
    '''Test that a contained routine resolves a symbol from its parent.'''
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


def test_function_return_type_variants():
    '''Test different types of expressing return types.'''
    valid_code = """
        real function signature_type()
        end function
        function inner_type()
          integer :: inner_type
        end function inner_type
        complex function unsupported()
        end function unsupported
    """
    processor = FortranTreeSitterReader()
    root = processor.generate_psyir(
        processor.generate_parse_tree_from_source(valid_code))

    assert root.children[0].return_symbol.name == "signature_type"
    assert (root.children[0].return_symbol.datatype ==
            psyir_symbols.ScalarType.real_type())

    assert root.children[1].return_symbol.name == "inner_type"
    assert (root.children[1].return_symbol.datatype ==
            psyir_symbols.ScalarType.integer_type())

    assert isinstance(root.children[2].return_symbol.datatype,
                      psyir_symbols.UnsupportedFortranType)


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


def test_declarations():
    ''' Test simple declarations '''
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


@pytest.mark.parametrize("qualifier", ["pointer", "protected"])
def test_unsupported_qualifier_datatype(qualifier):
    '''Test entity-specific unsupported declaration qualifiers, including
    those that are not in a predefined list.
    '''
    processor = FortranTreeSitterReader()
    valid_code = f"""
        module declarations
          implicit none
          integer, {qualifier} :: first, second
        end module declarations
    """
    root = processor.generate_psyir(
        processor.generate_parse_tree_from_source(valid_code))
    table = root.children[0].symbol_table
    for name in ("first", "second"):
        datatype = table.lookup(name).datatype
        assert isinstance(datatype, psyir_symbols.UnsupportedFortranType)
        assert datatype.declaration == f"integer, {qualifier} :: {name}"


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


def test_class_declaration_is_unsupported():
    '''Test that a polymorphic declaration uses the unsupported fallback.'''
    valid_code = """
        subroutine polymorphic(value)
          class(item) :: value
        end subroutine polymorphic
    """
    processor = FortranTreeSitterReader()
    root = processor.generate_psyir(
        processor.generate_parse_tree_from_source(valid_code))

    assert isinstance(root.children[0].symbol_table.lookup("value").datatype,
                      psyir_symbols.UnsupportedFortranType)


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
    assert isinstance(first.datatype, psyir_symbols.ScalarType)
    assert isinstance(first.initial_value, psyir_nodes.CodeBlock)
    assert first.initial_value.structure == \
        psyir_nodes.CodeBlock.Structure.EXPRESSION
    assert isinstance(second.datatype, psyir_symbols.ScalarType)
    assert second.initial_value.value == "2"


def test_unsupported_initializer_translation(monkeypatch):
    '''Test the declaration fallback if expression processing itself fails.'''
    valid_code = """
        module declarations
          integer :: value = 1
        end module declarations
    """
    processor = FortranTreeSitterReader()
    parse_tree = processor.generate_parse_tree_from_source(valid_code)
    original = processor._process_nodes

    def unsupported_number(tsnodes, expect):
        '''Reject the initializer while processing everything else normally.'''
        if (getattr(tsnodes, "type", None) == "number_literal" and
                expect is _NodeExpectation.EXPRESSION):
            raise NotImplementedError("unsupported test initializer")
        return original(tsnodes, expect)

    monkeypatch.setattr(processor, "_process_nodes", unsupported_number)
    root = processor.generate_psyir(parse_tree)

    assert isinstance(root.children[0].symbol_table.lookup("value").datatype,
                      psyir_symbols.UnsupportedFortranType)


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
    ''' Test base declaration datatypes '''
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
    assert module.symbol_table.lookup("a").datatype == psyir_type


def test_datatype_kind_variants():
    '''Test symbolic, literal and unsupported kinds.'''
    valid_code = """
        subroutine declarations()
          integer(named_kind) :: symbolic
          integer(16) :: wide
          integer(1 + 1) :: unsupported_kind
        end subroutine declarations
    """
    processor = FortranTreeSitterReader()
    root = processor.generate_psyir(
        processor.generate_parse_tree_from_source(valid_code))
    table = root.children[0].symbol_table

    assert isinstance(table.lookup("named_kind"), psyir_symbols.DataSymbol)
    assert table.lookup("wide").datatype.precision == 16
    assert isinstance(table.lookup("unsupported_kind").datatype,
                      psyir_symbols.UnsupportedFortranType)

    # Also check symbolic links are connected
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


def test_literal_kind_variants_and_string():
    '''Test literal kind suffixes and a character literal.'''
    valid_code = '''
        subroutine literals()
          integer, parameter :: wp = 16
          integer :: single = 1_4, double = 1_8
          integer :: explicit = 1_16, symbolic = 1_wp
          character(5) :: text = "hello"
        end subroutine literals
    '''
    processor = FortranTreeSitterReader()
    root = processor.generate_psyir(
        processor.generate_parse_tree_from_source(valid_code))
    table = root.children[0].symbol_table

    assert table.lookup("single").initial_value.datatype.precision == \
        psyir_symbols.ScalarType.Precision.SINGLE
    assert table.lookup("double").initial_value.datatype.precision == \
        psyir_symbols.ScalarType.Precision.DOUBLE
    assert table.lookup("explicit").initial_value.datatype.precision == 16
    precision = table.lookup("symbolic").initial_value.datatype.precision
    assert precision.symbol is table.lookup("wp")
    assert table.lookup("text").initial_value.value == "hello"


def test_new_literal_kind_symbol():
    '''Test that a named literal kind creates an unresolved kind symbol.'''
    valid_code = """
        subroutine literal_kind()
          integer :: value = 1_new_kind
        end subroutine literal_kind
    """
    processor = FortranTreeSitterReader()
    root = processor.generate_psyir(
        processor.generate_parse_tree_from_source(valid_code))
    table = root.children[0].symbol_table

    assert isinstance(table.lookup("new_kind"), psyir_symbols.DataSymbol)
    assert isinstance(table.lookup("new_kind").interface,
                      psyir_symbols.UnresolvedInterface)
    assert table.lookup("value").initial_value.datatype.precision.symbol is \
        table.lookup("new_kind")


@pytest.mark.parametrize("shape_string, extent", [
    ("(:)", psyir_symbols.ArrayType.Extent.ATTRIBUTE),
    ("(10)", "10"),
])
def test_declarations_arrays_datatypes(shape_string, extent):
    ''' Test array datatypes and its dimensions. '''
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


def test_multidimensional_and_lower_bounded_arrays():
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


def test_shared_dimension_is_given_to_all_entities():
    '''Test that a shared DIMENSION are copied for each declared entity.
    '''
    processor = FortranTreeSitterReader()
    valid_code = """
        module test
            implicit none
            integer, parameter :: extent = 10
            real, dimension(extent) :: first, second
        end module
    """
    root = processor.generate_psyir(
        processor.generate_parse_tree_from_source(valid_code))
    table = root.children[0].symbol_table
    first_type = table.lookup("first").datatype
    second_type = table.lookup("second").datatype

    # They are not the same object, but copies
    assert first_type is not second_type
    assert first_type.shape[0].upper is not second_type.shape[0].upper
    assert first_type.shape[0].upper.symbol is table.lookup("extent")
    assert second_type.shape[0].upper.symbol is table.lookup("extent")


def test_entity_dimension_overrides_shared_dimension():
    '''Test handling of an entity shape together with DIMENSION.'''
    processor = FortranTreeSitterReader()
    valid_code = """
        module test
            implicit none
            real, dimension(10) :: field(20)
        end module
    """
    root = processor.generate_psyir(
        processor.generate_parse_tree_from_source(valid_code))
    datatype = root.children[0].symbol_table.lookup("field").datatype

    assert isinstance(datatype, psyir_symbols.ArrayType)
    assert datatype.shape[0].upper.value == "20"


@pytest.mark.parametrize("valid_code", [
    """
        module declarations
          real, dimension(10) :: values
        end module declarations
    """,
    """
        module declarations
          real :: values(10)
        end module declarations
    """,
])
def test_unsupported_shape_translation(valid_code, monkeypatch):
    '''Test failure translating shared and entity-specific array shapes.'''
    processor = FortranTreeSitterReader()
    parse_tree = processor.generate_parse_tree_from_source(valid_code)

    def unsupported_shape(*_args, **_kwargs):
        '''Stand in for an unsupported bound expression.'''
        raise NotImplementedError("unsupported test shape")

    monkeypatch.setattr(processor, "_shape_from_node", unsupported_shape)
    root = processor.generate_psyir(parse_tree)

    assert isinstance(root.children[0].symbol_table.lookup("values").datatype,
                      psyir_symbols.UnsupportedFortranType)


def test_direct_shape_and_argument_helpers():
    '''Test defensive extent splitting and absent argument handling.'''
    valid_code = """
        subroutine shape(values)
          integer :: values(10)
        end subroutine shape
    """
    processor = FortranTreeSitterReader()
    parse_tree = processor.generate_parse_tree_from_source(valid_code)
    number = _first_tsnode(parse_tree, "number_literal")

    before, after, has_colon = processor._split_extent(number)
    assert not before
    assert not after
    assert not has_colon
    assert not processor._arguments(None)

    malformed = SimpleNamespace(
        type="extent_specifier", children=[number])
    assert processor._shape_from_node(
        SimpleNamespace(children=[malformed]))[0].value == "10"
    common = _CommonDeclAttributes(
        psyir_symbols.ScalarType.integer_type(),
        psyir_symbols.ArgumentInterface.Access.UNKNOWN,
        frozenset(), frozenset(), "integer")
    empty_initializer = SimpleNamespace(
        type="init_declarator", children=[
            SimpleNamespace(type="identifier", text=b"value"),
            SimpleNamespace(type="=", text=b"=")])
    datatype, initial = processor._declarator_datatype(
        empty_initializer, common)
    assert isinstance(datatype, psyir_symbols.ScalarType)
    assert initial is None

    with pytest.raises(NotImplementedError, match="Malformed array range"):
        processor._range(number, psyir_symbols.DataSymbol(
            "values", psyir_symbols.ArrayType(
                psyir_symbols.ScalarType.integer_type(), [10])), 1)
    with pytest.raises(NotImplementedError, match="Malformed allocation"):
        processor._allocation_extent(malformed)


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


def test_declaration_visibility_and_allocatable_scalar():
    '''Test declaration access attributes and unsupported scalar
    ALLOCATABLE.'''
    valid_code = """
        module declarations
          integer, public :: exposed
          integer, private :: hidden
        contains
          subroutine local()
            real, allocatable :: scalar
          end subroutine local
        end module declarations
    """
    processor = FortranTreeSitterReader()
    root = processor.generate_psyir(
        processor.generate_parse_tree_from_source(valid_code))
    container = root.children[0]

    assert container.symbol_table.lookup("exposed").visibility == \
        psyir_symbols.Symbol.Visibility.PUBLIC
    assert container.symbol_table.lookup("hidden").visibility == \
        psyir_symbols.Symbol.Visibility.PRIVATE
    assert isinstance(container.children[0].symbol_table.lookup(
        "scalar").datatype, psyir_symbols.UnsupportedFortranType)


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


def test_wildcard_and_defensive_import_branches():
    '''Test wildcard import and defensive malformed/identity import
    handling.'''
    valid_code = """
        module imports
          use wildcard_source
        end module imports
    """
    processor = FortranTreeSitterReader()
    root = processor.generate_psyir(
        processor.generate_parse_tree_from_source(valid_code))
    assert root.children[0].symbol_table.lookup(
        "wildcard_source").wildcard_import

    table = psyir_symbols.SymbolTable()
    processor._current_scope = table
    module_name = SimpleNamespace(
        type="module_name", children=[], text=b"source")
    malformed_rename = SimpleNamespace(
        type="rename", children=[SimpleNamespace(
            type="identifier", children=[], text=b"local")])
    included = SimpleNamespace(
        type="included_items", children=[malformed_rename])
    use_statement = SimpleNamespace(
        children=[module_name, included])
    processor._use_statement_handler(use_statement)
    container = table.lookup("source")

    processor._add_imported_symbol("source", "source", container)
    assert table.lookup("source") is container


def test_declaration_conflicts_with_import():
    '''Test that redeclaring an imported bare Symbol is localised.'''
    valid_code = """
        subroutine conflict()
          use other, only: value
          integer :: value
        end subroutine conflict
    """
    processor = FortranTreeSitterReader()
    root = processor.generate_psyir(
        processor.generate_parse_tree_from_source(valid_code))

    codeblock = root.children[0].children[0]
    assert isinstance(codeblock, psyir_nodes.CodeBlock)
    assert "already declared as a non-data symbol" in \
        codeblock.preceding_comment


def test_name_conflict():
    '''Test that name conflicts in different declarations on the same scope
    are invalid.'''
    valid_code = """
        module conflict
          use other
          integer :: other
        end module conflict
    """
    processor = FortranTreeSitterReader()

    message = "USE module 'other' conflicts with another symbol"
    with pytest.raises(ValueError, match=message):
        processor.generate_psyir(
            processor.generate_parse_tree_from_source(valid_code))


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


def test_unsupported_and_forward_declared_derived_types():
    '''Test unsupported type procedures and completion of a forward type.'''
    valid_code = """
        module types
          type :: with_procedure
          contains
            procedure :: method
          end type with_procedure
          type(forward) :: instance
          type, private :: forward
            integer :: value
          end type forward
        end module types
    """
    processor = FortranTreeSitterReader()
    root = processor.generate_psyir(
        processor.generate_parse_tree_from_source(valid_code))
    table = root.children[0].symbol_table

    assert isinstance(table.lookup("with_procedure").datatype,
                      psyir_symbols.UnsupportedFortranType)
    forward = table.lookup("forward")
    assert isinstance(forward.datatype, psyir_symbols.StructureType)
    assert forward.visibility == psyir_symbols.Symbol.Visibility.PRIVATE


def test_invalid_derived_type_component_falls_back():
    '''Test a component-name conflict makes the whole type unsupported.'''
    valid_code = """
        module types
          type :: invalid
            type(component_type) :: value
            integer :: component_type
          end type invalid
        end module types
    """
    processor = FortranTreeSitterReader()
    root = processor.generate_psyir(
        processor.generate_parse_tree_from_source(valid_code))

    assert isinstance(root.children[0].symbol_table.lookup("invalid").datatype,
                      psyir_symbols.UnsupportedFortranType)


def test_derived_type_name_conflict():
    '''Test a derived type whose name is already used by a data symbol.'''
    valid_code = """
        module conflict
          integer :: item
          type :: item
            integer :: value
          end type item
        end module conflict
    """
    processor = FortranTreeSitterReader()

    with pytest.raises(InternalError, match="No node was expected"):
        processor.generate_psyir(
            processor.generate_parse_tree_from_source(valid_code))


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


def test_interface_routine_symbol_are_consistent():
    '''Test that a routine declared by an interface is reused by its body.'''
    valid_code = """
        module routines
          interface generic
            module procedure implementation
          end interface generic
        contains
          pure integer function implementation()
            implementation = 1
          end function implementation
        end module routines
    """
    processor = FortranTreeSitterReader()
    root = processor.generate_psyir(
        processor.generate_parse_tree_from_source(valid_code))
    container = root.children[0]
    routine = container.children[0]

    assert routine.symbol is container.symbol_table.lookup("implementation")
    assert routine.symbol.is_pure
    assert routine.symbol.datatype == \
        psyir_symbols.ScalarType.integer_type()


@pytest.mark.parametrize("valid_code", [
    """
        module interfaces
          abstract interface
            subroutine method()
            end subroutine method
          end interface
        end module interfaces
    """,
    """
        module interfaces
          integer :: method
          interface generic
            module procedure method
          end interface generic
        end module interfaces
    """,
    """
        module interfaces
          interface generic
            subroutine method()
            end subroutine method
          end interface generic
        end module interfaces
    """,
])
def test_unsupported_interface_forms(valid_code):
    '''Test unsupported interface forms are detected in declaration context.'''
    processor = FortranTreeSitterReader()

    with pytest.raises(InternalError, match="No node was expected"):
        processor.generate_psyir(
            processor.generate_parse_tree_from_source(valid_code))


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


def test_unsupported_expression_codeblock():
    '''An unsupported expression becomes an expression CodeBlock without
    replacing its enclosing statement.
    '''
    processor = FortranTreeSitterReader()
    valid_code = '''
        subroutine concatenate(value)
          character(*) :: value
          value = value // "suffix"
        end subroutine concatenate
    '''
    root = processor.generate_psyir(
        processor.generate_parse_tree_from_source(valid_code))
    assignment = root.children[0].children[0]
    assert isinstance(assignment, psyir_nodes.Assignment)
    assert isinstance(assignment.rhs, psyir_nodes.CodeBlock)
    assert assignment.rhs.structure == \
        psyir_nodes.CodeBlock.Structure.EXPRESSION
    assert "Unsupported 'concatenation_expression'" in \
        assignment.rhs.preceding_comment


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
    assignment = root.children[0].children[0]
    assert isinstance(assignment, psyir_nodes.Assignment)
    codeblock = assignment.rhs
    assert isinstance(codeblock, psyir_nodes.CodeBlock)
    assert codeblock.structure == psyir_nodes.CodeBlock.Structure.EXPRESSION
    assert ("Array constructors with implied-DO loops are not supported" in
            codeblock.preceding_comment)


def test_unresolved_identifiers_and_call_forms():
    '''Test unresolved names, empty calls, ranges and invalid scalar calls.'''
    valid_code = """
        subroutine expressions(result, scalar)
          integer :: result, scalar
          result = unknown
          result = function_without_arguments()
          result = function_with_range(1:2)
          result = scalar(1)
        end subroutine expressions
    """
    processor = FortranTreeSitterReader()
    root = processor.generate_psyir(
        processor.generate_parse_tree_from_source(valid_code))
    routine = root.children[0]

    assert routine.children[0].rhs.symbol.name == "unknown"
    assert isinstance(routine.children[1].rhs, psyir_nodes.Call)
    assert routine.children[1].rhs.arguments == ()
    for assignment in routine.children[2:]:
        assert isinstance(assignment.rhs, psyir_nodes.CodeBlock)


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


def test_invalid_array_and_intrinsic_arguments():
    '''Test named array subscripts and an invalid intrinsic signature.'''
    valid_code = """
        subroutine expressions(array, result)
          integer :: array(10), result
          result = array(dim=1)
          result = sin()
        end subroutine expressions
    """
    processor = FortranTreeSitterReader()
    root = processor.generate_psyir(
        processor.generate_parse_tree_from_source(valid_code))

    assert all(isinstance(assignment.rhs, psyir_nodes.CodeBlock)
               for assignment in root.children[0].children)
    assert "Named subscripts" in \
        root.children[0].children[0].rhs.preceding_comment
    assert "Unsupported argument form" in \
        root.children[0].children[1].rhs.preceding_comment


def test_existing_routine_and_local_type_calls():
    '''Test calls resolved to an existing routine or local datatype name.'''
    valid_code = """
        module routines
          interface generic
            module procedure existing
          end interface generic
        contains
          integer function existing()
            existing = 1
          end function existing
          subroutine caller(result)
            integer :: result
            type(local_type) :: value
            result = existing()
            value = local_type(1)
          end subroutine caller
        end module routines
    """
    processor = FortranTreeSitterReader()
    root = processor.generate_psyir(
        processor.generate_parse_tree_from_source(valid_code))
    caller = root.children[0].children[1]

    assert caller.children[0].rhs.routine.symbol.name == "existing"
    assert isinstance(caller.symbol_table.lookup("local_type"),
                      psyir_symbols.DataTypeSymbol)
    assert isinstance(caller.children[1].rhs, psyir_nodes.Call)


def test_call_statement_edge_cases():
    '''Test imported-symbol specialisation and invalid call targets.'''
    valid_code = """
        subroutine calls(object)
          use procedures, only: imported
          type(item_type) :: object
          integer :: data
          call imported()
          call data()
          call object%method()
        end subroutine calls
    """
    processor = FortranTreeSitterReader()
    root = processor.generate_psyir(
        processor.generate_parse_tree_from_source(valid_code))
    routine = root.children[0]

    assert isinstance(routine.children[0], psyir_nodes.Call)
    assert isinstance(routine.symbol_table.lookup("imported"),
                      psyir_symbols.RoutineSymbol)
    assert all(isinstance(node, psyir_nodes.CodeBlock)
               for node in routine.children[1:])


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


def test_structure_reference_edge_cases():
    '''Test unknown, nested, named-index and non-data structure bases.'''
    valid_code = """
        module structures
          type :: item_type
            integer :: field
          end type item_type
        contains
          subroutine references(items, result)
            type(item_type) :: items(2)
            integer :: result
            result = unknown%field
            result = items(1)%vector(2)%field
            result = items(dim=1)%field
            result = item_type%field
          end subroutine references
        end module structures
    """
    processor = FortranTreeSitterReader()
    root = processor.generate_psyir(
        processor.generate_parse_tree_from_source(valid_code))
    statements = root.children[0].children[0].children

    assert isinstance(statements[0].rhs, psyir_nodes.StructureReference)
    assert statements[0].rhs.symbol.name == "unknown"
    assert isinstance(statements[1].rhs,
                      psyir_nodes.ArrayOfStructuresReference)
    assert statements[1].rhs.member.member.name == "field"
    assert isinstance(statements[2].rhs, psyir_nodes.CodeBlock)
    assert isinstance(statements[3].rhs, psyir_nodes.CodeBlock)


def test_named_trailing_structure_index():
    '''Test a named index on the final component of a structure access.'''
    valid_code = """
        subroutine structure(result)
          integer :: result
          result = item%values(dim=1)
        end subroutine structure
    """
    processor = FortranTreeSitterReader()
    root = processor.generate_psyir(
        processor.generate_parse_tree_from_source(valid_code))

    codeblock = root.children[0].children[0].rhs
    assert isinstance(codeblock, psyir_nodes.CodeBlock)
    assert "Unsupported structure member array access" in \
        codeblock.preceding_comment


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


def test_allocate_bounds_and_invalid_object():
    '''Test explicit allocation bounds, missing upper bound and an import.'''
    valid_code = """
        subroutine bounds(first, second, third)
          real, allocatable :: first(:), second(:), third(:)
          allocate(first(2:10), second(:10))
          allocate(third(2:))
        end subroutine bounds
        subroutine imported_object()
          use source, only: array
          allocate(array(2))
        end subroutine imported_object
    """
    processor = FortranTreeSitterReader()
    root = processor.generate_psyir(
        processor.generate_parse_tree_from_source(valid_code))
    allocation = root.children[0].children[0]

    assert allocation.arguments[0].indices[0].start.value == "2"
    assert allocation.arguments[0].indices[0].stop.value == "10"
    assert allocation.arguments[1].indices[0].start.value == "1"
    assert isinstance(root.children[0].children[1], psyir_nodes.CodeBlock)
    assert isinstance(root.children[1].children[0], psyir_nodes.CodeBlock)


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

    # Top if construct
    ifblock = root.children[0].children[0]
    assert isinstance(ifblock, psyir_nodes.IfBlock)

    # Elseif
    nested = ifblock.else_body.children[0]
    assert isinstance(nested, psyir_nodes.IfBlock)
    assert "was_elseif" in nested.annotations

    # Check final else body
    assert nested.else_body.children[0].rhs.operator == \
        psyir_nodes.BinaryOperation.Operator.ADD


def test_single_statement_if():
    '''Test the single-statement IF annotation.'''
    valid_code = """
        subroutine control(value)
          integer :: value
          if (value > 0) value = 1
        end subroutine control
    """
    processor = FortranTreeSitterReader()
    root = processor.generate_psyir(
        processor.generate_parse_tree_from_source(valid_code))
    routine = root.children[0]

    assert "was_single_stmt" in routine.children[0].annotations


def test_do_loop():
    '''Test a DO loop.'''
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


def test_do_variable_variants():
    '''Test implicit, imported and non-scalar DO variables.'''
    valid_code = """
        subroutine implicit_variable()
          do index = 1, 10
          end do
        end subroutine implicit_variable
        subroutine imported_variable()
          use source, only: index
          do index = 1, 10
          end do
        end subroutine imported_variable
        subroutine array_variable()
          integer :: index(2)
          do index = 1, 10
          end do
        end subroutine array_variable
    """
    processor = FortranTreeSitterReader()
    root = processor.generate_psyir(
        processor.generate_parse_tree_from_source(valid_code))

    implicit = root.children[0]
    assert isinstance(implicit.children[0], psyir_nodes.Loop)
    assert implicit.symbol_table.lookup("index").datatype == \
        psyir_symbols.ScalarType.integer_type()
    assert isinstance(root.children[1].children[0], psyir_nodes.CodeBlock)
    assert isinstance(root.children[2].children[0], psyir_nodes.CodeBlock)


def test_keyword_statement():
    '''Test that an unsupported CYCLE is represented by a CodeBlock.'''
    valid_code = """
        subroutine control()
          do
            cycle
          end do
        end subroutine control
    """
    processor = FortranTreeSitterReader()
    root = processor.generate_psyir(
        processor.generate_parse_tree_from_source(valid_code))
    routine = root.children[0]

    assert isinstance(routine.children[0].loop_body.children[0],
                      psyir_nodes.CodeBlock)


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


def test_select_case_expression_and_open_ranges():
    '''Test an expression selector and lower- or upper-open CASE ranges.'''
    valid_code = """
        subroutine selection(value)
          integer :: value
          select case(value + 1)
          case(:5)
            value = 1
          case(8:)
            value = 2
          end select
        end subroutine selection
    """
    processor = FortranTreeSitterReader()
    root = processor.generate_psyir(
        processor.generate_parse_tree_from_source(valid_code))
    first = root.children[0].children[0]
    second = first.else_body.children[0]

    assert first.condition.operator == psyir_nodes.BinaryOperation.Operator.LE
    assert second.condition.operator == \
        psyir_nodes.BinaryOperation.Operator.GE


def test_select_case_with_only_default_is_unsupported():
    '''Test SELECT CASE with no conditional case becomes a CodeBlock.'''
    valid_code = """
        subroutine selection(value)
          integer :: value
          select case(value)
          case default
            value = 1
            value = 2
          end select
        end subroutine selection
    """
    processor = FortranTreeSitterReader()
    root = processor.generate_psyir(
        processor.generate_parse_tree_from_source(valid_code))

    assert isinstance(root.children[0].children[0], psyir_nodes.CodeBlock)
    assert "only a default clause" in \
        root.children[0].children[0].preceding_comment


# pylint: disable=too-many-locals
def test_malformed_operation_and_statement_guards(monkeypatch):
    '''Test defensive guards for malformed expressions and statements.'''
    valid_code = """
        subroutine guards(array)
          real, allocatable :: array(:)
          allocate(array(10))
        end subroutine guards
    """
    processor = FortranTreeSitterReader()
    parse_tree = processor.generate_parse_tree_from_source(valid_code)
    number = _first_tsnode(parse_tree, "number_literal")

    bad_unary = SimpleNamespace(children=[
        SimpleNamespace(type="operator", text=b"?"), number])
    with pytest.raises(NotImplementedError, match="unary operator"):
        processor._operation(bad_unary)

    bad_binary = SimpleNamespace(children=[
        number, SimpleNamespace(type="operator", text=b"?"), number])
    with pytest.raises(NotImplementedError, match="binary operator"):
        processor._operation(bad_binary)
    with pytest.raises(NotImplementedError, match="operation structure"):
        processor._operation(SimpleNamespace(children=[]))

    with pytest.raises(NotImplementedError, match="assignment structure"):
        processor._assignment_statement_handler(
            SimpleNamespace(children=[]))
    with pytest.raises(NotImplementedError, match="bounds remapping"):
        processor._pointer_association_statement_handler(
            SimpleNamespace(children=[]))
    with pytest.raises(NotImplementedError, match="IF statement"):
        processor._if_statement_handler(SimpleNamespace(children=[]))

    statement = SimpleNamespace(
        type="do_statement",
        children=[SimpleNamespace(
            type="loop_control_expression", children=[])])
    loop = SimpleNamespace(type="do_loop", children=[statement])
    with pytest.raises(NotImplementedError, match="counted DO loop"):
        processor._do_loop_handler(loop)

    identifier = _first_tsnode(parse_tree, "identifier")
    selector = SimpleNamespace(type="selector", children=[identifier])
    malformed_case = SimpleNamespace(type="case_statement", children=[])
    select = SimpleNamespace(children=[selector, malformed_case])
    with pytest.raises(NotImplementedError, match="Malformed CASE"):
        processor._select_case_statement_handler(select)

    malformed_member = SimpleNamespace(
        type="derived_type_member_expression",
        children=[SimpleNamespace(
            type="identifier", children=[], text=b"item")])
    with pytest.raises(NotImplementedError, match="Malformed structure"):
        processor._decompose_structure(malformed_member)
    with pytest.raises(NotImplementedError, match="structure access base"):
        processor._decompose_structure(
            SimpleNamespace(type="number_literal", children=[]))

    allocate = _first_tsnode(parse_tree, "allocate_statement")

    def invalid_intrinsic(*_args, **_kwargs):
        '''Simulate rejection by PSyIR intrinsic signature validation.'''
        raise TypeError("invalid test operands")

    monkeypatch.setattr(psyir_nodes.IntrinsicCall, "create",
                        invalid_intrinsic)
    with pytest.raises(NotImplementedError, match="Unsupported operands"):
        processor._memory_statement(allocate)


def test_scope_and_handler_defensive_errors():
    '''Test scope ownership defensive checks.'''
    valid_code = """
        subroutine routine()
        end subroutine routine
        module types
          type :: item
            integer :: value
          end type item
        end module types
    """
    processor = FortranTreeSitterReader()
    parse_tree = processor.generate_parse_tree_from_source(valid_code)
    routine = _first_tsnode(parse_tree, "subroutine")
    derived = _first_tsnode(parse_tree, "derived_type_definition")

    duplicate = SimpleNamespace(children=[
        SimpleNamespace(type="name"), SimpleNamespace(type="name")])
    with pytest.raises(InternalError, match="Expected only 1"):
        ftr.child_of_type(duplicate, "name")

    parent = psyir_nodes.ScopingNode(symbol_table=psyir_symbols.SymbolTable())
    attached = psyir_nodes.ScopingNode(
        symbol_table=psyir_symbols.SymbolTable())
    attached._parent = parent
    with pytest.raises(InternalError, match="must be an orphan"):
        with processor._using_temporary_scope(parent, attached):
            pass
    attached._parent = None

    with pytest.raises(InternalError, match="Routine must be translated"):
        processor._procedure_handler(routine)
    with pytest.raises(InternalError, match="derived type must be translated"):
        processor._derived_type_definition_handler(derived)
