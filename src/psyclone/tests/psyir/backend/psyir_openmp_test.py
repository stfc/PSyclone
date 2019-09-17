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

import re

from psyclone.psyir.backend.c import CWriter
from psyclone.psyir.backend.fortran import FortranWriter
from psyclone.tests.psyclone_test_utils import create_schedule


# ----------------------------------------------------------------------------
def test_nemo_omp_parallel():
    '''Tests if an OpenMP parallel directive in NEMO is handled correctly.
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
    # Convert to code, and remove all new lines, to make
    # regex matching easier
    result = fvisitor(schedule).replace("\n", "")

    assert re.search(r"!\$omp parallel.*" +
                     r"!\$omp end parallel", result) is not None

    cvisitor = CWriter()
    # Remove newlines for easier RE matching
    result = cvisitor(schedule[1]).replace("\n", "")
    assert re.search(r"#pragma omp parallel.*{.*}", result) is not None


# ----------------------------------------------------------------------------
def test_gocean_omp_parallel():
    '''Test that an OMP PARALLEL directive in a 'classical' API (gocean here)
    is created correctly.
    '''

    from psyclone.transformations import OMPParallelTrans
    from psyclone.tests.psyclone_test_utils import get_invoke

    _, invoke = get_invoke("single_invoke.f90", "gocean1.0",
                           idx=0)
    schedule = invoke.schedule

    omp = OMPParallelTrans()
    omp_sched, _ = omp.apply(schedule.children[0])

    invoke.schedule = omp_sched

    fvisitor = FortranWriter()
    # TODO: Directly access the omp node, since higher level
    # node types are not yet supported.
    result = fvisitor(omp_sched[0]).replace("\n", "")

    assert re.search(r"!\$omp parallel.*" +
                     r"!\$omp end parallel", result) is not None

    cvisitor = CWriter()
    # Remove newlines for easier RE matching
    result = cvisitor(omp_sched[0]).replace("\n", "")
    assert re.search(r"#pragma omp parallel.*{.*}", result) is not None


# ----------------------------------------------------------------------------
def test_nemo_omp_do():
    '''Tests if an OpenMP parallel directive in NEMO is handled correctly.
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
    from psyclone.transformations import OMPLoopTrans

    # Now apply a parallel transform
    omp_loop = OMPLoopTrans()
    omp_loop.apply(schedule[1])

    fvisitor = FortranWriter()
    # Convert to code, and remove all new lines, to make
    # regex matching easier
    result = fvisitor(schedule).replace("\n", "")

    assert re.search(r"!\$omp do.*" +
                     r"!\$omp end do", result) is not None

    cvisitor = CWriter()
    # Remove newlines for easier RE matching
    result = cvisitor(schedule[1]).replace("\n", "")
    assert re.search(r"#pragma omp do.*{.*}", result) is not None
