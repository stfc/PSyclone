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
from psyclone.psyir.nodes import FileContainer, CodeBlock, Container


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
    assert "Syntax Error found at line 2" in str(err.value)

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

    # Valid code returns a treesitter Node
    valid_code = """
    module test
        implicit none
        contains
        subroutine mysub()
        end subroutine
    end module test
    """
    ptree = processor.generate_parse_tree_from_source(valid_code)
    psyir = processor.generate_psyir(ptree)

    assert isinstance(psyir, FileContainer)
    assert isinstance(psyir.children[0], Container)
    assert isinstance(psyir.children[0].children[0], CodeBlock)


def test_codeblock_generation_and_messages():
    '''
    Test that NotImplementedErrors are catch and converted to CodeBlocks
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
    psyir = processor.generate_psyir(ptree)

    assert isinstance(psyir, FileContainer)
    assert isinstance(psyir.children[0], CodeBlock)
    expected = (
        "PSyclone CodeBlock (unsupported code) reason:\n"
        "- Modules that allow implicit variables are not supported"
    )
    assert psyir.children[0].preceding_comment == expected

    unsupported_code = """
    module test
        implicit none
        integer :: a
        contains
        subroutine mysub()
        end subroutine
    end module test
    """
    ptree = processor.generate_parse_tree_from_source(unsupported_code)
    psyir = processor.generate_psyir(ptree)

    assert isinstance(psyir, FileContainer)
    assert isinstance(psyir.children[0], CodeBlock)
    expected = (
        "PSyclone CodeBlock (unsupported code) reason:\n"
        "- Module has an unsupported 'variable_declaration' node"
    )
    assert psyir.children[0].preceding_comment == expected
