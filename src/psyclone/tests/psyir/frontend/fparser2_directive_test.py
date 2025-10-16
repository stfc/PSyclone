# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2021-2025, Science and Technology Facilities Council.
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
# Author A. B. G. Chalk, STFC Daresbury Lab

"""Performs pytest tests on the support for directives in the fparser2
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


def test_directive_after_decls():
    """Test that the FortranReader correctly finds a directive immediately
    after the declarations"""

    code = """subroutine x()
    integer :: i
    !$omp barrier
    i = 3
    end subroutine"""
    reader = FortranReader(ignore_comments=False, ignore_directives=False)
    psyir = reader.psyir_from_source(code)
    routine = psyir.children[0]
    # The directive is a codeblock
    assert isinstance(routine.children[0], CodeBlock)
    assert routine.children[0].debug_string() == "!$omp barrier\n"


def test_directive_in_decls():
    """Test that the FortranReader can handle a directive inside the
    declarations"""
    code = """subroutine x()
    !$omp firstprivate
    integer, dimension(100) :: i !dir$ aligned
    i = 1
    end subroutine x"""
    reader = FortranReader(ignore_comments=False, ignore_directives=False)
    psyir = reader.psyir_from_source(code)
    routine = psyir.children[0]
    out = routine.debug_string()
    assert """!$omp firstprivate
integer, dimension(100) :: i  !dir$ aligned""" in out


def test_directive_at_end():
    """Test that the FortranReader stores a directive after all
    other code in a subroutine."""

    code = """subroutine x
    integer :: i
    i = i + 1
    !$omp barrier
    end subroutine"""
    reader = FortranReader(ignore_comments=False, ignore_directives=False)
    psyir = reader.psyir_from_source(code)
    routine = psyir.children[0]
    # The directive is a codeblock
    assert isinstance(routine.children[-1], CodeBlock)
    assert routine.children[-1].debug_string() == "!$omp barrier\n"


