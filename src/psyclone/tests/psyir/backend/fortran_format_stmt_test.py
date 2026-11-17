# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2021-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

'''Module containing pytest tests for the handling of CodeBlocks containing
Fortran Format statements in the backend.'''

from psyclone.psyir.nodes import Routine


def test_format_codeblock_handler(fortran_reader, fortran_writer):
    '''Test that a CodeBlock containing a labelled format statement is
    correctly output by the Fortran backend. '''
    code = '''
program my_test
  implicit none
  integer :: val

  write(*, 111) "This is just a test"

  val = 1

 111 format("(A)")

end program my_test'''
    psyir = fortran_reader.psyir_from_source(code)
    prog = psyir.walk(Routine)[0]
    assert len(prog.children) == 3
    cbnode = prog.children[2]
    result = fortran_writer(cbnode)
    assert '111 FORMAT("(A)")' in result
