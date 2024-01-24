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
# Author A. R. Porter, STFC Daresbury Lab

'''Module containing pytest tests for the handling of labelled format
statements.'''

from __future__ import absolute_import
from fparser.two import Fortran2003
from psyclone.psyir.nodes import Container, Routine, CodeBlock


def test_format_handler(fortran_reader):
    '''Test that a labelled format statement is correctly captured by a
    CodeBlock. '''
    code = '''
program my_test
  implicit none
  integer :: val

  write(*, 111) "This is just a test"

  val = 1

 111 format("(A)")

end program my_test'''
    psyir = fortran_reader.psyir_from_source(code)
    # Check the expected PSyIR nodes are being created
    assert isinstance(psyir, Container)
    assert psyir.parent is None
    prog = psyir.walk(Routine)[0]
    assert len(prog.children) == 3
    assert isinstance(prog.children[0], CodeBlock)
    cbnode = prog.children[2]
    assert isinstance(cbnode, CodeBlock)
    assert isinstance(cbnode._fp2_nodes[0], Fortran2003.Format_Stmt)
    assert cbnode._fp2_nodes[0].item.label == 111
