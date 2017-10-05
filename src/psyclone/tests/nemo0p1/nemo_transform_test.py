# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017, Science and Technology Facilities Council
# (c) The copyright relating to this work is owned jointly by the Crown,
# Met Office and NERC 2016.
# However, it has been created with the help of the GungHo Consortium,
# whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
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
# Authors: R. Ford and A. R. Porter, STFC Daresbury Lab

''' Module containing py.test tests for the transformation of
    the PSy representation of NEMO code '''

import os
import fparser
import pytest
from psyclone.parse import parse, ParseError
from psyclone.psyGen import PSyFactory, TransInfo
from psyclone import nemo0p1

# Constants
API = "nemo0.1"
# Location of the Fortran files associated with these tests
BASE_PATH = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                         "test_files")


def test_explicit_gen():
    ''' Check code generation for a single explicit loop containing
    a kernel '''
    ast, invoke_info = parse(os.path.join(BASE_PATH, "explicit_do.f90"),
                             api=API, line_length=False)
    psy = PSyFactory(API, distributed_memory=False).create(invoke_info)
    schedule = psy.invokes.get('explicit_do').schedule
    tinfo = TransInfo()
    omp_trans = tinfo.get_trans_name('OMPParallelLoopTrans')

    for loop in schedule.loops():
        kernel = loop.kernel
        if kernel:
            if kernel.type == "3D" and loop.loop_type == "levels":
                schedule, _ = omp_trans.apply(loop)
            elif kernel.type == "2D" and loop.loop_type == "lat":
                schedule, _ = omp_trans.apply(loop)
    schedule.view()
    gen_code = str(psy.gen)
    print gen_code
    expected = (
        "PROGRAM explicit_do\n"
        "  IMPLICIT NONE\n"
        "  INTEGER :: ji, jj, jk\n"
        "  INTEGER :: jpi, jpj, jpk\n"
        "  REAL, DIMENSION(jpi, jpj, jpk) :: umask\n"
        "  !$OMP PARALLEL DO\n"
        "  DO , jk = 1, jpk\n"
        "    DO , jj = 1, jpj\n"
        "      DO , ji = 1, jpi\n"
        "        umask(ji, jj, jk) = ji * jj * jk / r\n"
        "      END DO\n"
        "    END DO\n"
        "  END DO\n"
        "  !$OMP END PARALLEL DO\n"
        "END PROGRAM explicit_do")
    assert expected in gen_code
