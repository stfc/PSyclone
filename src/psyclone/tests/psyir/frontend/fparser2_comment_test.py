# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2021-2026, Science and Technology Facilities Council.
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
# Author Julien Remy, Université Grenoble Alpes & Inria

"""Performs pytest tests on the support for comments in the fparser2
PSyIR front-end"""

import pytest

from fparser.two import Fortran2003

from psyclone.psyir.frontend.fortran import FortranReader
from psyclone.psyir.nodes import (
    Container,
    Routine,
    Assignment,
    Loop,
    IfBlock,
    Call,
    CodeBlock,
)
from psyclone.psyir.commentable_mixin import CommentableMixin
from psyclone.psyir.symbols import DataTypeSymbol, StructureType

from psyclone.psyir.backend.fortran import FortranWriter

# Test code
CODE = """
! Comment on module 'test_mod'
! and second line
module test_mod
  implicit none
  ! Comment on derived type 'my_type'
  type :: my_type
    ! Comment on component 'i'
    ! and second line
    integer :: i ! Inline comment on 'integer :: i'
    ! Comment on component 'j'
    integer :: j
  end type my_type ! Inline comment on 'end type my_type'
  ! Comment on derived type 'my_type2'
  type :: my_type2
  end type my_type2 ! Inline comment on 'end type my_type2'
contains
  ! Comment on a subroutine
  subroutine test_sub()
    ! Comment on variable 'a'
    ! and second line
    integer :: a
    ! Comment on variable 'i'
    integer :: i
    ! Comment on variable 'j'
    integer :: j
    ! Comment on assignment 'a = 1'
    ! and second line
    a = 1
    ! Comment on call 'call test_sub()'
    call test_sub()
    ! Comment on if block 'if (a == 1) then'
    if (a == 1) then
      ! Comment on assignment 'a = 2'
      a = 2
    ! Comment on elseif block 'elseif (a == 2) then' => CodeBlock
    elseif (a == 2) then
      ! Comment on assignment 'a = 3'
      a = 3
    ! Comment on else block 'else' => CodeBlock
    else
      ! Comment on assignment 'a = 4'
      a = 4
    ! Comment on 'end if' => CodeBlock
    end if ! Inline comment on 'end if'
    ! Comment on loop 'do i = 1, 10'
    do i = 1, 10
      ! Comment on assignment 'a = 5'
      a = 5
      ! Comment on loop 'do j = 1, 10'
        do j = 1, 10
          ! Comment on assignment 'a = 6'
          a = 6
          ! Comment at end of loop on j => CodeBlock
        end do ! Inline comment on 'end do j = 1, 10'
        ! Comment at end of loop on i => CodeBlock
    end do ! Inline comment on 'end do i = 1, 10'
    ! Comment on 'do while (a < 10)'
    do while (a < 10)
      ! Comment on assignment 'a = 7'
      a = 7 ! Inline comment on 'a = 7'
      ! Comment at end of while loop => CodeBlock
    end do ! Inline comment on 'end do while (a < 10)'
    ! Comment at end of subroutine => CodeBlock
  end subroutine test_sub ! Inline comment on 'end subroutine test_sub'
! Comment at end of module => CodeBlock
end module test_mod
"""


def test_no_comments():
    """Test that the FortranReader is without comments by default"""
    reader = FortranReader()
    psyir = reader.psyir_from_source(CODE)

    module = psyir.children[0]
    assert isinstance(module, Container)
    assert module.name == "test_mod"
    assert module.preceding_comment == ""

    my_type_sym = module.symbol_table.lookup("my_type")
    assert isinstance(my_type_sym, DataTypeSymbol)
    assert my_type_sym.preceding_comment == ""

    assert isinstance(my_type_sym.datatype, StructureType)
    for component in my_type_sym.datatype.components.values():
        assert component.preceding_comment == ""

    routine = module.walk(Routine)[0]
    assert routine.name == "test_sub"
    assert routine.preceding_comment == ""
    for symbol in routine.symbol_table.symbols:
        assert symbol.preceding_comment == ""
    commentable_nodes = routine.walk(CommentableMixin)
    assert len(commentable_nodes) != 0
    for node in commentable_nodes:
        assert node.preceding_comment == ""

    assert len(routine.walk(CodeBlock)) == 0


@pytest.mark.parametrize("last_comments_as_codeblocks", [True, False])
def test_comments_and_codeblocks(last_comments_as_codeblocks):
    """Test that the FortranReader is able to read comments"""
    reader = FortranReader(
        ignore_comments=False,
        last_comments_as_codeblocks=last_comments_as_codeblocks)
    psyir = reader.psyir_from_source(CODE)

    module = psyir.children[0]
    assert (
        module.preceding_comment
        == "Comment on module 'test_mod'\nand second line"
    )
    if last_comments_as_codeblocks:
        assert isinstance(module.children[-1], CodeBlock)
        assert isinstance(module.children[-1].get_ast_nodes()[0],
                          Fortran2003.Comment)
        assert (module.children[-1].get_ast_nodes()[0].tostr()
                == "! Comment at end of module => CodeBlock")
    else:
        assert not isinstance(module.children[-1], CodeBlock)

    my_type_sym = module.symbol_table.lookup("my_type")
    assert my_type_sym.preceding_comment == "Comment on derived type 'my_type'"
    assert my_type_sym.inline_comment == "Inline comment on 'end type my_type'"

    assert isinstance(my_type_sym.datatype, StructureType)
    for i, component in enumerate(my_type_sym.datatype.components.values()):
        if i == 0:
            assert (
                component.preceding_comment
                == "Comment on component 'i'\nand second line"
            )
            assert (
                component.inline_comment == "Inline comment on 'integer :: i'"
            )
        else:
            assert component.preceding_comment == "Comment on component 'j'"
            assert component.inline_comment == ""

    my_type2_sym = module.symbol_table.lookup("my_type2")
    assert (
        my_type2_sym.preceding_comment == "Comment on derived type 'my_type2'"
    )
    assert (
        my_type2_sym.inline_comment == "Inline comment on 'end type my_type2'"
    )

    routine = module.walk(Routine)[0]
    assert routine.preceding_comment == "Comment on a subroutine"
    assert (
        routine.inline_comment == "Inline comment on 'end subroutine test_sub'"
    )
    last_child = routine.children[-1]
    if last_comments_as_codeblocks:
        assert isinstance(last_child, CodeBlock)
        assert isinstance(last_child.get_ast_nodes()[0], Fortran2003.Comment)
        assert (
            last_child.get_ast_nodes()[0].tostr()
            == "! Comment at end of subroutine => CodeBlock"
        )
    else:
        assert not isinstance(last_child, CodeBlock)

    for i, symbol in enumerate(routine.symbol_table.symbols):
        if i == 0:
            assert (
                symbol.preceding_comment
                == "Comment on variable 'a'\nand second line"
            )
        else:
            assert (
                symbol.preceding_comment
                == f"Comment on variable '{symbol.name}'"
            )

    for i, assignment in enumerate(routine.walk(Assignment)):
        if i == 0:
            assert (
                assignment.preceding_comment
                == "Comment on assignment 'a = 1'\nand second line"
            )
        else:
            assert (
                assignment.preceding_comment
                == f"Comment on assignment 'a = {i+1}'"
            )

    call = routine.walk(Call)[0]
    assert call.preceding_comment == "Comment on call 'call test_sub()'"

    ifblock = routine.walk(IfBlock)[0]
    assert (
        ifblock.preceding_comment == "Comment on if block 'if (a == 1) then'"
    )
    last_child = ifblock.if_body.children[-1]
    if last_comments_as_codeblocks:
        assert isinstance(last_child, CodeBlock)
        assert isinstance(last_child.get_ast_nodes()[0], Fortran2003.Comment)
        assert (
            last_child.get_ast_nodes()[0].tostr()
            == "! Comment on elseif block 'elseif (a == 2) then' => CodeBlock"
        )
    else:
        assert not isinstance(last_child, CodeBlock)
    ifblock2 = ifblock.else_body.children[0]
    last_child = ifblock2.if_body.children[-1]
    if last_comments_as_codeblocks:
        assert isinstance(last_child, CodeBlock)
        assert isinstance(last_child.get_ast_nodes()[0], Fortran2003.Comment)
        assert (
            last_child.get_ast_nodes()[0].tostr()
            == "! Comment on else block 'else' => CodeBlock"
        )
    else:
        assert not isinstance(last_child, CodeBlock)
    last_child = ifblock2.else_body.children[-1]
    if last_comments_as_codeblocks:
        assert isinstance(last_child, CodeBlock)
        assert isinstance(last_child.get_ast_nodes()[0], Fortran2003.Comment)
        assert (last_child.get_ast_nodes()[0].tostr() ==
                "! Comment on 'end if' => CodeBlock")
    else:
        assert not isinstance(last_child, CodeBlock)

    loops = routine.walk(Loop)
    loop_i = loops[0]
    assert loop_i.variable.name == "i"
    assert loop_i.preceding_comment == "Comment on loop 'do i = 1, 10'"
    assert loop_i.inline_comment == "Inline comment on 'end do i = 1, 10'"
    last_child = loop_i.loop_body.children[-1]
    if last_comments_as_codeblocks:
        assert isinstance(last_child, CodeBlock)
        assert isinstance(last_child.get_ast_nodes()[0], Fortran2003.Comment)
        assert (
            last_child.get_ast_nodes()[0].tostr()
            == "! Comment at end of loop on i => CodeBlock"
        )
    else:
        assert not isinstance(last_child, CodeBlock)

    loop_j = loops[1]
    assert loop_j.variable.name == "j"
    assert loop_j.preceding_comment == "Comment on loop 'do j = 1, 10'"
    assert loop_j.inline_comment == "Inline comment on 'end do j = 1, 10'"
    last_child = loop_j.loop_body.children[-1]
    if last_comments_as_codeblocks:
        assert isinstance(last_child, CodeBlock)
        assert isinstance(last_child.get_ast_nodes()[0], Fortran2003.Comment)
        assert (
            last_child.get_ast_nodes()[0].tostr()
            == "! Comment at end of loop on j => CodeBlock"
        )
    else:
        assert not isinstance(last_child, CodeBlock)


EXPECTED_WITH_COMMENTS_AND_CODEBLOCKS = """! Comment on module 'test_mod'
! and second line
module test_mod
  implicit none
  ! Comment on derived type 'my_type'
  type, public :: my_type
    ! Comment on component 'i'
    ! and second line
    integer, public :: i ! Inline comment on 'integer :: i'
    ! Comment on component 'j'
    integer, public :: j
  end type my_type ! Inline comment on 'end type my_type'
  ! Comment on derived type 'my_type2'
  type, public :: my_type2
  end type my_type2 ! Inline comment on 'end type my_type2'
  public

  contains
  ! Comment on a subroutine
  subroutine test_sub()
    ! Comment on variable 'a'
    ! and second line
    integer :: a
    ! Comment on variable 'i'
    integer :: i
    ! Comment on variable 'j'
    integer :: j

    ! Comment on assignment 'a = 1'
    ! and second line
    a = 1

    ! Comment on call 'call test_sub()'
    call test_sub()

    ! Comment on if block 'if (a == 1) then'
    if (a == 1) then
      ! Comment on assignment 'a = 2'
      a = 2
      ! Comment on elseif block 'elseif (a == 2) then' => CodeBlock
    else
      if (a == 2) then
        ! Comment on assignment 'a = 3'
        a = 3
        ! Comment on else block 'else' => CodeBlock
      else
        ! Comment on assignment 'a = 4'
        a = 4
        ! Comment on 'end if' => CodeBlock
      end if
    end if  ! Inline comment on 'end if'

    ! Comment on loop 'do i = 1, 10'
    do i = 1, 10, 1
      ! Comment on assignment 'a = 5'
      a = 5

      ! Comment on loop 'do j = 1, 10'
      do j = 1, 10, 1
        ! Comment on assignment 'a = 6'
        a = 6
        ! Comment at end of loop on j => CodeBlock
      enddo  ! Inline comment on 'end do j = 1, 10'
      ! Comment at end of loop on i => CodeBlock
    enddo  ! Inline comment on 'end do i = 1, 10'

    ! Comment on 'do while (a < 10)'
    do while (a < 10)
      ! Comment on assignment 'a = 7'
      a = 7  ! Inline comment on 'a = 7'
      ! Comment at end of while loop => CodeBlock
    end do  ! Inline comment on 'end do while (a < 10)'
    ! Comment at end of subroutine => CodeBlock

  end subroutine test_sub  ! Inline comment on 'end subroutine test_sub'
  ! Comment at end of module => CodeBlock

end module test_mod
"""

EXPECTED_WITH_COMMENTS_AND_NO_CODEBLOCKS = """! Comment on module 'test_mod'
! and second line
module test_mod
  implicit none
  ! Comment on derived type 'my_type'
  type, public :: my_type
    ! Comment on component 'i'
    ! and second line
    integer, public :: i ! Inline comment on 'integer :: i'
    ! Comment on component 'j'
    integer, public :: j
  end type my_type ! Inline comment on 'end type my_type'
  ! Comment on derived type 'my_type2'
  type, public :: my_type2
  end type my_type2 ! Inline comment on 'end type my_type2'
  public

  contains
  ! Comment on a subroutine
  subroutine test_sub()
    ! Comment on variable 'a'
    ! and second line
    integer :: a
    ! Comment on variable 'i'
    integer :: i
    ! Comment on variable 'j'
    integer :: j

    ! Comment on assignment 'a = 1'
    ! and second line
    a = 1

    ! Comment on call 'call test_sub()'
    call test_sub()

    ! Comment on if block 'if (a == 1) then'
    if (a == 1) then
      ! Comment on assignment 'a = 2'
      a = 2
    else
      if (a == 2) then
        ! Comment on assignment 'a = 3'
        a = 3
      else
        ! Comment on assignment 'a = 4'
        a = 4
      end if
    end if  ! Inline comment on 'end if'

    ! Comment on loop 'do i = 1, 10'
    do i = 1, 10, 1
      ! Comment on assignment 'a = 5'
      a = 5

      ! Comment on loop 'do j = 1, 10'
      do j = 1, 10, 1
        ! Comment on assignment 'a = 6'
        a = 6
      enddo  ! Inline comment on 'end do j = 1, 10'
    enddo  ! Inline comment on 'end do i = 1, 10'

    ! Comment on 'do while (a < 10)'
    do while (a < 10)
      ! Comment on assignment 'a = 7'
      a = 7  ! Inline comment on 'a = 7'
    end do  ! Inline comment on 'end do while (a < 10)'

  end subroutine test_sub  ! Inline comment on 'end subroutine test_sub'

end module test_mod
"""


@pytest.mark.parametrize("last_comments_as_codeblocks", [True, False])
def test_write_comments(last_comments_as_codeblocks):
    """Test that the comments are written back to the code"""
    reader = FortranReader(
        ignore_comments=False,
        last_comments_as_codeblocks=last_comments_as_codeblocks
    )
    writer = FortranWriter()
    psyir = reader.psyir_from_source(CODE)
    generated_code = writer(psyir)
    if last_comments_as_codeblocks:
        assert generated_code == EXPECTED_WITH_COMMENTS_AND_CODEBLOCKS
    else:
        assert generated_code == EXPECTED_WITH_COMMENTS_AND_NO_CODEBLOCKS


CODE_WITH_DIRECTIVE = """
subroutine test_sub()
  integer :: a
  integer :: i
  ! Comment on loop 'do i = 1, 10'
  !dir$ somedir
  do i = 1, 10
    a = 1
  end do
end subroutine test_sub
"""


def test_no_directives():
    """Test that the FortranReader is without directives by default"""
    reader = FortranReader(ignore_comments=False)
    psyir = reader.psyir_from_source(CODE_WITH_DIRECTIVE)

    loop = psyir.walk(Loop)[0]
    assert loop.preceding_comment == "Comment on loop 'do i = 1, 10'"


def test_directives():
    """Test that the FortranReader is able to read directives"""
    reader = FortranReader(ignore_comments=False, ignore_directives=False)
    psyir = reader.psyir_from_source(CODE_WITH_DIRECTIVE)

    loop = psyir.walk(Loop)[0]
    directive = loop.preceding(reverse=True)[0]
    assert isinstance(directive, CodeBlock)
    assert (directive.debug_string() ==
            "! Comment on loop 'do i = 1, 10'\n!dir$ somedir\n")


EXPECTED_WITH_DIRECTIVES = """subroutine test_sub()
  integer :: a
  integer :: i

  ! Comment on loop 'do i = 1, 10'
  !dir$ somedir
  do i = 1, 10, 1
    a = 1
  enddo

end subroutine test_sub
"""


def test_write_directives():
    """Test that the directives are written back to the code"""
    reader = FortranReader(ignore_comments=False, ignore_directives=False)
    writer = FortranWriter()
    psyir = reader.psyir_from_source(CODE_WITH_DIRECTIVE)
    generated_code = writer(psyir)
    assert generated_code == EXPECTED_WITH_DIRECTIVES


CODE_WITH_INLINE_COMMENT = """
subroutine test_sub()
  integer :: a ! Inline comment on 'integer :: a'
  ! Preceding comment on 'i = 1'
  integer :: i ! Inline comment on 'integer :: i'
  ! Preceding comment on 'a = 1'
  a = 1 ! Inline comment on 'a = 1'
  ! Preceding comment on 'i = 1'
  i = 1 ! Inline comment on 'i = 1'

  a = & ! First line of inline comment
    i & ! Second line of inline comment
    + 1 ! Third line of inline comment
end subroutine test_sub
"""


def test_inline_comment():
    """Test that the FortranReader is able to read inline comments"""
    reader = FortranReader(ignore_comments=False)
    psyir = reader.psyir_from_source(CODE_WITH_INLINE_COMMENT)

    routine = psyir.walk(Routine)[0]
    sym_a = routine.symbol_table.lookup("a")
    assert sym_a.preceding_comment == ""
    assert sym_a.inline_comment == "Inline comment on 'integer :: a'"
    sym_i = routine.symbol_table.lookup("i")
    assert sym_i.preceding_comment == "Preceding comment on 'i = 1'"
    assert sym_i.inline_comment == "Inline comment on 'integer :: i'"

    assignment = routine.walk(Assignment)[0]
    assert "a = 1" in assignment.debug_string()
    assert assignment.preceding_comment == "Preceding comment on 'a = 1'"
    assert assignment.inline_comment == "Inline comment on 'a = 1'"

    assignment = routine.walk(Assignment)[1]
    assert "i = 1" in assignment.debug_string()
    assert assignment.preceding_comment == "Preceding comment on 'i = 1'"
    assert assignment.inline_comment == "Inline comment on 'i = 1'"

    # When processing
    # a = & ! First line of inline comment
    # i & ! Second line of inline comment
    # + 1 ! Third line of inline comment
    # only the third comment is kept as inline comment
    assignment = routine.walk(Assignment)[2]
    assert "a = i + 1" in assignment.debug_string()
    assert assignment.preceding_comment == ""
    assert assignment.inline_comment == "Third line of inline comment"


def test_lost_program_comments():
    """Test that the FortranReader doesn't lose comments after the
    declarations when reading a Program."""
    reader = FortranReader(ignore_comments=False)
    code = """program a
    integer :: i ! inline here

    ! Comment here
    i = 1
    end program"""
    psyir = reader.psyir_from_source(code)
    assert (psyir.children[0].symbol_table.lookup("i").inline_comment ==
            "inline here")
    assignment = psyir.walk(Assignment)[0]
    assert assignment.preceding_comment == "Comment here"


@pytest.mark.parametrize("directive", ["$omp target",
                                       "$acc kernels",
                                       "dir$ vector",
                                       "DIR$ VECTOR",
                                       "$pos dir"])
def test_directives_not_comments(directive):
    """Test that the FortranReader doesn't keep directives when only
    comments are requested."""
    code = f"""module A
  implicit none
  integer, public :: a
  public

  contains
  subroutine test()

    !$ a = 0 +     &
    !$&  0
    !{directive}
    a = 1

  end subroutine test

end module A"""
    reader = FortranReader(ignore_comments=False)
    psyir = reader.psyir_from_source(code)
    assert directive not in psyir.debug_string()
