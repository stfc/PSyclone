# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2023-2024, Science and Technology Facilities Council.
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
# Author: S. Siso, STFC Daresbury Lab
# -----------------------------------------------------------------------------

'''Performs pytest tests on PSyIR Fortran Backend for CommonBlocks '''

from psyclone.psyir.nodes import Routine
from psyclone.tests.utilities import Compile


def test_fw_common_blocks(fortran_reader, fortran_writer, tmpdir):
    '''Test that declarations with common blocks are maintained in the
    generated Fortran.

    '''
    # Generate PSyIR from Fortran code.
    code = (
        "module test\n"
        "  contains\n"
        "  subroutine sub()\n"
        "    integer :: a, b, c\n"
        "    real :: d, e, f\n"
        "    common /name1/ a, b\n"
        "    common /name1/ c /name2/ d\n"
        "    common e, f\n"
        "  end subroutine sub\n"
        "end module test\n")
    psyir = fortran_reader.psyir_from_source(code)
    routine = psyir.walk(Routine)[0]

    assert routine.symbol_table.lookup("a").is_commonblock  # Sanity check

    code = fortran_writer(routine)
    assert code == (
        "subroutine sub()\n"
        "  integer :: a\n"
        "  integer :: b\n"
        "  integer :: c\n"
        "  real :: d\n"
        "  real :: e\n"
        "  real :: f\n"
        "  COMMON /name1/ a, b\n"
        "  COMMON /name1/ c /name2/ d\n"
        "  COMMON // e, f\n\n\n"
        "end subroutine sub\n")
    assert Compile(tmpdir).string_compiles(fortran_writer(psyir))
