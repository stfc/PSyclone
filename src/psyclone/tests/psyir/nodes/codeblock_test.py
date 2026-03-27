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
# Authors R. W. Ford, A. R. Porter and S. Siso, STFC Daresbury Lab
#         I. Kavcic, Met Office
#         J. Henrichs, Bureau of Meteorology
# -----------------------------------------------------------------------------

''' Performs py.test tests on the CodeBlock PSyIR node. '''

import pytest
from fparser.common.readfortran import FortranStringReader
from psyclone.configuration import Config
from psyclone.psyir.frontend.fortran import FortranReader
from psyclone.psyir.frontend.fparser2 import Fparser2Reader
from psyclone.psyir.frontend.fortran_treesitter_reader import \
    FortranTreeSitterReader
from psyclone.psyir.nodes.codeblock import (
    CodeBlock, Fparser2CodeBlock, TreeSitterCodeBlock
)
from psyclone.psyir.nodes.node import colored
from psyclone.errors import GenerationError, InternalError


def test_codeblock_create():
    ''' Check the create method of the Code Block class.'''

    # The generic create works like a factory that creates the appropriate
    # CodeBlock subclass looking at the selected parser by default is fparser2
    cb = CodeBlock.create("3 + 3", partial_code="expression")
    assert isinstance(cb, Fparser2CodeBlock)
    assert "3 + 3" in cb.get_fortran_lines()

    cb = CodeBlock.create("a => b", partial_code="pointer_assignment")
    assert isinstance(cb, Fparser2CodeBlock)
    assert "a => b" in cb.get_fortran_lines()

    # Use the treesitter frontend (the frontend doesn't support partial
    # expressions yet, but we should get the appropriate error)
    Config.get()._frontend = "treesitter"
    with pytest.raises(ValueError) as err:
        cb = CodeBlock.create("3 + 3", partial_code="expression")
    assert "Syntax Error found at line 1: 3 + 3" in str(err.value)
    with pytest.raises(ValueError) as err:
        cb = CodeBlock.create("a => b", partial_code="pointer_assignment")
    assert "Syntax Error found at line 1: a => b" in str(err.value)

    # Use a different fronted value
    Config.get()._frontend = "newfrontend"
    with pytest.raises(InternalError) as err:
        cb = CodeBlock.create("3 + 3", partial_code="expression")
    assert ("The 'newfrontend' frontend does not have an associated CodeBlock "
            "subclass" in str(err.value))


def test_codeblock_node_str():
    ''' Check the node_str method of the Code Block class.'''
    cblock = CodeBlock([], CodeBlock.Structure.EXPRESSION)
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


def test_codeblock_constructor_and_getastnodes():
    '''Test that the parse_tree_nodes method of a CodeBlock instance returns
    a copy of the list of nodes from the original AST that are associated with
    this code block.

    For simplicity we use a list of strings rather than an AST.

    '''
    original = ["hello", "there"]
    cblock = CodeBlock(original, CodeBlock.Structure.EXPRESSION)
    result = cblock.parse_tree_nodes
    assert result == original
    # Check that the list is a copy not a reference.
    assert result is not original

    # If only one element is provided, this is added to a list
    original = 3
    cblock = CodeBlock(original, CodeBlock.Structure.EXPRESSION)
    assert cblock.parse_tree_nodes == [3]


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


def test_abstract_methods():
    ''' Test that the abstract methods of CodeBlock raise a NotImplementedError
    (to simplify other tests they still work when there is no associated parse
    tree) '''
    # If there is no associated parse_tree, the methods return a falsy value
    cblock = CodeBlock([], "dummy")
    assert not cblock.get_symbol_names()
    assert not cblock.has_potential_control_flow_jump()
    assert not cblock.get_fortran_lines()

    # But if there is one, the node will need to be subclassed to properly
    # interpret the meaning of the ast
    cblock._parse_tree_nodes = ["something"]
    with pytest.raises(NotImplementedError) as err:
        _ = cblock.get_symbol_names()
    assert "Use appropriate CodeBlock subclass" in str(err.value)
    with pytest.raises(NotImplementedError) as err:
        _ = cblock.has_potential_control_flow_jump()
    assert "Use appropriate CodeBlock subclass" in str(err.value)
    with pytest.raises(NotImplementedError) as err:
        _ = cblock.get_fortran_lines()
    assert "Use appropriate CodeBlock subclass" in str(err.value)


def test_codeblock_get_fortran_lines():
    '''
    Test the get_fortran_lines method for fparser and treesiteer codeblocks.

    (These should be the same to guarantee identical outcomes with both
    frontends)
    '''
    code = "\nsubroutine mytest\nend subroutine"
    tree = Fparser2Reader().generate_parse_tree_from_source(code)
    block = Fparser2CodeBlock(tree.children, CodeBlock.Structure.STATEMENT)
    assert isinstance(block.get_fortran_lines(), list)
    assert "subroutine mytest" in block.get_fortran_lines()
    assert "end subroutine" in block.get_fortran_lines()

    tree = FortranTreeSitterReader().generate_parse_tree_from_source(code)
    block = TreeSitterCodeBlock(tree, CodeBlock.Structure.STATEMENT)
    assert isinstance(block.get_fortran_lines(), list)
    assert "subroutine mytest" in block.get_fortran_lines()
    assert "end subroutine" in block.get_fortran_lines()


def test_codeblock_get_symbol_names():
    '''Test that the get_symbol_names methods returns the names of the symbols
    used inside the CodeBlock. This is slightly subtle as we have to avoid
    any labels on loop and branching statements.'''
    prog = Fparser2Reader().generate_parse_tree_from_source('''
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
    block = Fparser2CodeBlock(prog.children, CodeBlock.Structure.STATEMENT)
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
    integer :: i, j, is

    !$ompx dir private(i)
    i = i + 1
    !dir$ omp private(j)
    i = j + 1
    ! Here is a comment
    end subroutine"""

    reader = FortranReader(ignore_comments=False,
                           ignore_directives=False,
                           last_comments_as_codeblocks=True)
    psyir = reader.psyir_from_source(code)
    block = psyir.walk(CodeBlock)
    sym_names = set(block[0].get_symbol_names()).union(
                    set(block[1].get_symbol_names()))
    assert "i" in sym_names
    assert "j" in sym_names
    assert "omp" not in sym_names
    assert "dir" not in sym_names
    assert "private" not in sym_names
    block = psyir.walk(CodeBlock)[1]
    sym_names = block.get_symbol_names()
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
    block = Fparser2CodeBlock(
        prog.children, CodeBlock.Structure.STATEMENT)
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


def test_codeblock_has_potential_control_flow_jump(fortran_reader):
    """Test the has_potential_control_flow_jump function of the CodeBlock
    class."""

    code = """subroutine test()
    integer :: i
    GOTO 1234
    i = 1
    write(*,*) "Hello"
    do i = 1, 100
        EXIT
    end do
1234 i = 3
    end subroutine"""
    psyir = fortran_reader.psyir_from_source(code)
    codeblocks = psyir.walk(CodeBlock)

    # GOTO statement
    assert codeblocks[0].has_potential_control_flow_jump()
    # Write statement
    assert not codeblocks[1].has_potential_control_flow_jump()
    # Exit statement
    assert codeblocks[2].has_potential_control_flow_jump()
    # labelled statement
    assert codeblocks[3].has_potential_control_flow_jump()
