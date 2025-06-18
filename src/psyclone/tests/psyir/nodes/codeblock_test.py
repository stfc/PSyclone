# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2019-2025, Science and Technology Facilities Council.
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

import pytest
from fparser.common.readfortran import FortranStringReader
from psyclone.psyir.frontend.fortran import FortranReader
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
    used inside the CodeBlock. This is slightly subtle as we have to avoid
    any labels on loop and branching statements.'''
    reader = FortranStringReader('''
    subroutine mytest
      myloop: DO i = 1, 10
        a = b + sqrt(c)
        myifblock: IF(this_is_true)THEN
          EXIT myloop
        ELSE IF(that_is_true)THEN myifblock
          write(*,*) "Bye"
        ELSE myifblock
          write(*,*) "hello"
        END IF myifblock
      END DO myloop
    end subroutine mytest''')
    prog = parser(reader)
    block = CodeBlock(prog.children, CodeBlock.Structure.STATEMENT)
    sym_names = block.get_symbol_names()
    assert "a" in sym_names
    assert "b" in sym_names
    assert "c" in sym_names
    assert "mytest" in sym_names
    assert "subroutine" not in sym_names
    assert "sqrt" not in sym_names
    assert "myloop" not in sym_names
    assert "myifblock" not in sym_names
    assert "this_is_true" in sym_names
    assert "that_is_true" in sym_names


def test_codeblock_get_symbol_names_comments_and_directives():
    '''
    Test that Codeblock.get_symbol_names returns any symbols in directives.
    '''
    code = """
    subroutine mytest
    integer :: i

    ! Here is a comment
    !$omp dir private(i)
    i = i + 1
    end subroutine"""

    reader = FortranReader(ignore_comments=False,
                           ignore_directives=False)
    psyir = reader.psyir_from_source(code)
    block = psyir.walk(CodeBlock)[0]
    sym_names = block.get_symbol_names()
    assert "i" in sym_names
    assert "omp" not in sym_names
    assert "dir" not in sym_names
    assert "private" not in sym_names
    assert "Here" not in sym_names
    assert "is" not in sym_names
    assert "a" not in sym_names
    assert "comment" not in sym_names


def test_codeblock_ref_accesses(parser):
    '''Test that the reference_accesses() method works as expected.

    TODO #2863 - accesses within a CodeBlock should really be marked as
    AccessType.UNKNOWN but are currently always READWRITE. Also, calls to
    Fortran intrinsics are not captured.

    '''
    reader = FortranStringReader('''
    subroutine mytest
      that_is_true = .TRUE._bool_kind
      hello_str = char_kind_"hello"
      my_cmplx = (1.0_c_def, 1.0_b_def)
      myloop: DO i = 1, 10_i_def
        a = b + sqrt(c) + 1.0_r_def
        myifblock: IF(this_is_true)THEN
          EXIT myloop
        ELSE IF(that_is_true)THEN myifblock
          call my_routine()
          write(*,*) "Bye"
        ELSE myifblock
          write(*,*) "hello"
        END IF myifblock
      END DO myloop
    end subroutine mytest''')
    prog = parser(reader)
    block = CodeBlock(prog.children, CodeBlock.Structure.STATEMENT)
    vam = block.reference_accesses()
    all_sigs = vam.all_signatures
    all_names = [sig.var_name for sig in all_sigs]
    assert "a" in all_names
    assert "i" in all_names
    # Check that the various precision symbols are included.
    assert "i_def" in all_names
    assert "r_def" in all_names
    assert "bool_kind" in all_names
    assert "char_kind" in all_names
    assert "c_def" in all_names
    assert "b_def" in all_names
    # The target of a CALL is included.
    assert "my_routine" in all_names
    # All signatures should be marked as READWRITE access.
    assert all(vam.has_read_write(sig) for sig in all_sigs)


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
