#!/usr/bin/env python
# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2018-2021, Science and Technology Facilities Council.
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
# Authors: R. W. Ford, A. R. Porter and S. Siso, STFC Daresbury Lab

'''A simple test script showing the introduction of OpenMP with PSyclone.
In order to use it you must first install PSyclone. See README.md in the
top-level psyclone directory.

Once you have psyclone installed, this script may be run by doing (you may
need to make it executable first with chmod u+x ./runme_openmp.py):

 >>> ./runme_openmp.py

This should generate a lot of output, ending with generated
Fortran.
'''

from __future__ import print_function
from psyclone.parse.algorithm import parse
from psyclone.psyir.nodes import Assignment
from psyclone.psyGen import PSyFactory
from psyclone.transformations import OMPParallelLoopTrans
from psyclone.domain.nemo.transformations import NemoAllArrayRange2LoopTrans

if __name__ == "__main__":
    from psyclone.nemo import NemoKern
    API = "nemo"
    _, INVOKEINFO = parse("../code/traldf_iso.F90", api=API)
    PSY = PSyFactory(API).create(INVOKEINFO)
    print(PSY.gen)

    print("Invokes found:")
    print(PSY.invokes.names)

    SCHED = PSY.invokes.get('tra_ldf_iso').schedule
    SCHED.view()

    OMP_TRANS = OMPParallelLoopTrans()
    LOOP_TRANS = NemoAllArrayRange2LoopTrans()

    # Transform implicit loops (arrays with ranges) so that they are
    # explicit loops. This then means we can parallelise them with
    # OpenMP below.
    for assign in SCHED.walk(Assignment):
        if assign.is_array_range:
            LOOP_TRANS.apply(assign)

    for loop in SCHED.loops():
        # TODO loop.kernel method needs extending to cope with
        # multiple kernels
        kernels = loop.walk(NemoKern)
        if kernels and loop.loop_type == "levels":
            OMP_TRANS.apply(loop)

    SCHED.view()

    PSY.invokes.get('tra_ldf_iso').schedule = SCHED
    print(PSY.gen)
