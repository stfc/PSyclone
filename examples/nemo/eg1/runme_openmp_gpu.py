#!/usr/bin/env python
# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2021, Science and Technology Facilities Council
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
# Authors: S. Siso, STFC Daresbury Lab

'''A simple test script showing the introduction of OpenMP for GPU with
PSyclone. In order to use it you must first install PSyclone. See README.md
in the top-level psyclone directory.

Once you have psyclone installed, this script may be run by doing:

 >>> python runme_openmp_gpu.py

This should generate the Fortran with OpenMP target and loop directives.
'''

from __future__ import print_function
from psyclone.parse.algorithm import parse
from psyclone.psyGen import PSyFactory, TransInfo
from psyclone.psyir.nodes import Loop, Assignment, CodeBlock
from psyclone.domain.nemo.transformations import NemoAllArrayRange2LoopTrans

API = "nemo"
USE_GPU = True  # Enable for generating OpenMP target directives


def main():
    ''' Parser the Nemo tra_adv file and transform it to OpenMP for GPU '''
    _, invokeinfo = parse("../code/tra_adv.F90", api=API)
    psy = PSyFactory(API).create(invokeinfo)
    sched = psy.invokes.get('tra_adv').schedule

    omp_loop_trans = TransInfo().get_trans_name('OMPLoopTrans')
    omp_loop_trans.omp_worksharing = False
    omp_target_trans = TransInfo().get_trans_name('OMPTargetTrans')

    # Convert all array implicit loops to explicit loops
    explicit_loops = NemoAllArrayRange2LoopTrans()
    for assignment in sched.walk(Assignment):
        try:
            explicit_loops.apply(assignment)
        except KeyError:
            # The transformation just works if jpi and jpj are
            # already declared in the file. Ignore other cases.
            pass

    # Add the OpenMP directives in each loop
    for loop in sched.walk(Loop):
        if loop.loop_type == "levels" and not loop.loop_body.walk(CodeBlock):

            if USE_GPU:
                omp_target_trans.apply(loop)

            omp_loop_trans.apply(loop)

            num_nested_loops = 0
            next_loop = loop
            while isinstance(next_loop, Loop):
                if len(next_loop.loop_body.children) > 1:
                    break
                next_loop = next_loop.loop_body.children[0]
                num_nested_loops += 1

            if num_nested_loops > 0:
                loop.parent.parent.collapse = num_nested_loops

    print(psy.gen)


if __name__ == "__main__":
    main()
