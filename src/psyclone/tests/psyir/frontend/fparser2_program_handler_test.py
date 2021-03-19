# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2021, Science and Technology Facilities Council.
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
# Author R. W. Ford, STFC Daresbury Lab

'''Module containing pytest tests for the _program_handler method in
the class Fparser2Reader. This handler deals with the translation of
the fparser2 Program construct to PSyIR.'''

from __future__ import absolute_import
import pytest

from fparser.common.readfortran import FortranStringReader
from psyclone.errors import GenerationError
from psyclone.psyir.nodes import Container
from psyclone.psyir.frontend.fparser2 import Fparser2Reader
from psyclone.psyir.backend.fortran import FortranWriter


def test_program_handler(parser):
    '''Test that program_handler passes valid Fortran on to other handlers
    correctly.

    '''
    code = (
        "module a\n"
        "end module\n")
    expected = (
        "module a\n"
        "  implicit none\n\n"
        "  contains\n\n"
        "end module a\n")
    processor = Fparser2Reader()
    reader = FortranStringReader(code)
    parse_tree = parser(reader)
    psyir = processor._program_handler(parse_tree, None)
    # Check PSyIR nodes are being created
    assert isinstance(psyir, Container)
    assert psyir.parent is None
    writer = FortranWriter()
    result = writer(psyir)
    assert result == expected


def test_program_handler_error(parser):
    '''Test that the expected exception is raised when more than one
    module/subroutine/program/function etc. is found in the fparser2
    tree.

    '''
    code = "module a\nend module\nmodule b\nend module b\n"
    processor = Fparser2Reader()
    reader = FortranStringReader(code)
    parse_tree = parser(reader)
    with pytest.raises(GenerationError) as info:
        processor._program_handler(parse_tree, None)
    assert ("The PSyIR is currently limited to a single top level "
            "module/subroutine/program/function, but 2 were found."
            in str(info.value))
