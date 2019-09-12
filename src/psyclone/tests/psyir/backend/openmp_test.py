# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2019, Science and Technology Facilities Council.
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
# Author J. Henrichs, Bureau of Meteorology
# -----------------------------------------------------------------------------

'''Performs pytest tests on the psyclond.psyir.backend.fortran module'''

from __future__ import absolute_import

from psyclone.psyir.backend.fortran import FortranWriter

# TODO - that code should probably moved into test_utils
from fortran_test import create_schedule


def test_omp_parallel():
    '''Tests if an OpenMP parallel directive is handled correctly.
    '''
    # Generate fparser2 parse tree from Fortran code.
    code = (
        "module test\n"
        "contains\n"
        "subroutine tmp()\n"
        "  integer :: i, sum\n"
        "  sum = 0\n"
        "  do i = 1, 20, 2\n"
        "    sum = sum + i\n"
        "  end do\n"
        "end subroutine tmp\n"
        "end module test")
    schedule = create_schedule(code)
    from psyclone.transformations import OMPParallelTrans

    # Now apply a parallel transform
    omp_par = OMPParallelTrans()
    omp_par.apply(schedule[1])

    fvisitor = FortranWriter()
    result = fvisitor(schedule)
    assert "!$omp parallel" in result
    assert "!$omp end parallel" in result

    from psyclone.psyir.backend.c import CWriter
    cvisitor = CWriter()
    result = cvisitor(schedule[1])
    # Maybe better tests ... beginning of line, right order
    # (not } before #pragma)
    assert "#pragma omp parallel\n{\n" in result
    assert "}\n" in result
