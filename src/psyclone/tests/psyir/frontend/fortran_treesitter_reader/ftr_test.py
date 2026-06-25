# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2026, Science and Technology Facilities Council.
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
# Authors: S. Siso, STFC Daresbury Lab
# -----------------------------------------------------------------------------

''' Performs tests on the treesitter PSyIR front-end '''
import logging
import pytest

from tree_sitter import Node as TSNode

from psyclone.psyir.frontend.fortran_treesitter_reader import \
    FortranTreeSitterReader
from psyclone.psyir import nodes as psyir_nodes, symbols as psyir_symbols
from psyclone.tests.utilities import min_version_3_10


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


# TODO #3416: Skip treesitter tests below 3.10 as they're unsupported by
# treesitter.
@min_version_3_10
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


# TODO #3416: Skip treesitter tests below 3.10 as they're unsupported by
# treesitter.
@min_version_3_10
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
    assert isinstance(root.children[0].children[0], psyir_nodes.CodeBlock)


# TODO #3416: Skip treesitter tests below 3.10 as they're unsupported by
# treesitter.
@min_version_3_10
def test_codeblock_generation_and_messages():
    '''
    Test that NotImplementedErrors are caught and converted to CodeBlocks
    with the appropriate associated comment
    '''
    processor = FortranTreeSitterReader()

    unsupported_code = """
    module test
        contains
        subroutine mysub()
        end subroutine
    end module test
    """
    ptree = processor.generate_parse_tree_from_source(unsupported_code)
    root = processor.generate_psyir(ptree)

    assert isinstance(root, psyir_nodes.FileContainer)
    assert isinstance(root.children[0], psyir_nodes.CodeBlock)
    expected = (
        "PSyclone CodeBlock (unsupported code) reason:\n"
        "- Modules that allow implicit variables are not supported"
    )
    assert root.children[0].preceding_comment == expected


@min_version_3_10
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


@min_version_3_10
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
    ("integer, dimension(:)", psyir_symbols.ScalarType.integer_type()),
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
        f"{module.symbol_table.lookup("a").datatype} != {psyir_type}"
    )
@pytest.mark.parametrize("shape_string, psyir_shape", [
    ("(:)", psyir_nodes.Literal("10", psyir_symbols.ScalarType.integer_type())),
])
def test_declarations_arrays_datatypes(shape_string, psyir_shape):
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
    assert isinstance(array_symbol.elemental_type, psyir_symbols.ScalarType.integer4_type())

    assert module.symbol_table.lookup("a").datatype == psyir_type, (
        f"{module.symbol_table.lookup("a").datatype} != {psyir_type}"
    )
