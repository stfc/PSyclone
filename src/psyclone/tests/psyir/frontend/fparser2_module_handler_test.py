# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2021-2024, Science and Technology Facilities Council.
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

'''Module containing pytest tests for the _module_handler method in
the class Fparser2Reader. This handler deals with the translation of
the fparser2 Module construct to PSyIR.'''

import pytest

from fparser.common.readfortran import FortranStringReader
from psyclone.psyir.nodes import Container
from psyclone.psyir.frontend.fparser2 import Fparser2Reader
from psyclone.psyir.backend.fortran import FortranWriter

# module no declarations
MODULE1_IN = (
    "module a\n"
    "end module\n")
MODULE1_OUT = (
    "module a\n"
    "  implicit none\n"
    "  public\n\n"
    "  contains\n\n"
    "end module a\n")
# module with symbols/declarations
MODULE2_IN = (
    "module a\n"
    "use my_mod, only : b\n"
    "real :: c\n"
    "end module\n")
MODULE2_OUT = (
    "module a\n"
    "  use my_mod, only : b\n"
    "  implicit none\n"
    "  real, public :: c\n"
    "  public\n\n"
    "  contains\n\n"
    "end module a\n")
# module with subprograms
MODULE3_IN = (
    "module a\n"
    "contains\n"
    "subroutine sub1(a)\n"
    "real :: a\n"
    "end subroutine\n"
    "subroutine sub2\n"
    "end subroutine\n"
    "end module\n")
MODULE3_OUT = (
    "module a\n"
    "  implicit none\n"
    "  public\n\n"
    "  contains\n"
    "  subroutine sub1(a)\n"
    "    real :: a\n\n\n"
    "  end subroutine sub1\n"
    "  subroutine sub2()\n\n\n"
    "  end subroutine sub2\n\n"
    "end module a\n")


@pytest.mark.parametrize("code,expected",
                         [(MODULE1_IN, MODULE1_OUT),
                          (MODULE2_IN, MODULE2_OUT),
                          (MODULE3_IN, MODULE3_OUT)])
def test_module_handler(parser, code, expected):
    '''Test that module_handler handles valid Fortran modules.
    '''
    processor = Fparser2Reader()
    reader = FortranStringReader(code)
    parse_tree = parser(reader)
    module = parse_tree.children[0]
    psyir = processor._module_handler(module, None)
    # Check the expected PSyIR nodes are being created
    assert isinstance(psyir, Container)
    assert psyir.parent is None
    writer = FortranWriter()
    result = writer(psyir)
    assert expected == result
