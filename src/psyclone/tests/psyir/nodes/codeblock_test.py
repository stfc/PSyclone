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
# Authors R. W. Ford, A. R. Porter and S. Siso, STFC Daresbury Lab
#         I. Kavcic, Met Office
#         J. Henrichs, Bureau of Meteorology
# -----------------------------------------------------------------------------

''' Performs py.test tests on the CodeBlock PSyIR node. '''

from __future__ import absolute_import
import pytest
from fparser.common.readfortran import FortranStringReader
from psyclone.psyir.nodes import CodeBlock
from psyclone.psyir.nodes.node import colored
from psyclone.errors import GenerationError


def test_codeblock_node_str():
    ''' Check the node_str method of the Code Block class.'''
    cblock = CodeBlock([], "dummy")
    coloredtext = colored("CodeBlock", CodeBlock._colour)
    output = cblock.node_str()
    assert coloredtext+"[" in output
    assert "]" in output


def test_codeblock_can_be_printed():
    '''Test that a CodeBlock instance can always be printed (i.e. is
    initialised fully)'''
    cblock = CodeBlock([], "dummy")
    assert "CodeBlock[" in str(cblock)
    assert "]" in str(cblock)


def test_codeblock_getastnodes():
    '''Test that the get_ast_nodes method of a CodeBlock instance returns
    a copy of the list of nodes from the original AST that are associated with
    this code block.

    For simplicity we use a list of strings rather than an AST.

    '''
    original = ["hello", "there"]
    cblock = CodeBlock(original, CodeBlock.Structure.EXPRESSION)
    result = cblock.get_ast_nodes
    assert result == original
    # Check that the list is a copy not a reference.
    assert result is not original


@pytest.mark.parametrize("structure", [CodeBlock.Structure.STATEMENT,
                                       CodeBlock.Structure.EXPRESSION])
def test_codeblock_structure(structure):
    '''Check that the structure property in the CodeBlock class is set to
    the provided value.

    '''
    cblock = CodeBlock([], structure)
    assert cblock.structure == structure


def test_codeblock_children_validation():
    '''Test that children added to CodeBlock are validated. CodeBlock does
    not accept any children.

    '''
    cblock = CodeBlock([], "dummy")
    with pytest.raises(GenerationError) as excinfo:
        cblock.addchild(CodeBlock([], "dummy2"))
    assert ("Item 'CodeBlock' can't be child 0 of 'CodeBlock'. CodeBlock is a"
            " LeafNode and doesn't accept children.") in str(excinfo.value)


def test_codeblock_get_symbol_names(parser):
    '''Test that the get_symbol_names methods returns the names of the symbols
    used inside the CodeBlock.'''
    reader = FortranStringReader('''
    subroutine mytest
        a = b + sqrt(c)
    end subroutine mytest''')
    prog = parser(reader)
    block = CodeBlock(prog.children, CodeBlock.Structure.STATEMENT)
    assert "a" in block.get_symbol_names()
    assert "b" in block.get_symbol_names()
    assert "c" in block.get_symbol_names()
    assert "mytest" in block.get_symbol_names()
    assert "subroutine" not in block.get_symbol_names()
    assert "sqrt" not in block.get_symbol_names()


def test_codeblock_equality(parser):
    '''Test the __eq__ method of the Codeblock class.'''
    reader = FortranStringReader('''
    subroutine mytest
        a = b + sqrt(c)
    end subroutine mytest''')
    prog = parser(reader)
    block = CodeBlock(prog.children, CodeBlock.Structure.STATEMENT)
    block2 = CodeBlock(prog.children, CodeBlock.Structure.STATEMENT)
    block3 = CodeBlock(prog.children, CodeBlock.Structure.EXPRESSION)
    assert block == block2
    assert block != block3
    reader = FortranStringReader('''
    subroutine mytest
        a = b + c
    end subroutine mytest''')
    prog = parser(reader)
    block4 = CodeBlock(prog.children, CodeBlock.Structure.STATEMENT)
    assert block != block4
