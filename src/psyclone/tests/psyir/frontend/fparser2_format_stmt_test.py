# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2021-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

'''Module containing pytest tests for the handling of labelled format
statements.'''

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
    assert isinstance(cbnode.parse_tree_nodes[0], Fortran2003.Format_Stmt)
    assert cbnode.parse_tree_nodes[0].item.label == 111
