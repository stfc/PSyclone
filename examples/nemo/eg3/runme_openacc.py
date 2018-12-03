#!/usr/bin/env python
# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2018, Science and Technology Facilities Council
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
# Authors: R. W. Ford and A. R. Porter, STFC Daresbury Lab

'''A simple test script showing the introduction of the OpenACC
kernels, loop and parallel directives with PSyclone.  In order to use
it you must first install PSyclone. See README.md in the top-level
psyclone directory.

Once you have psyclone installed, this script may be run by doing (you may
need to make it executable first with chmod u+x ./runme_openacc.py):

 >>> ./runme_openacc.py

This should generate a lot of output, ending with generated
Fortran.

'''

from __future__ import print_function
from psyclone.parse import parse
from psyclone.psyGen import PSyFactory, TransInfo
from psyclone.nemo import NemoKern, NemoLoop


def trans(psy):
    '''A PSyclone-script compliant function. Demonstrates the application
    of the Kernels, Parallel and Loop OpenACC directives to the
    'tra_adv' code.

    '''
    print("Invokes found:")
    print(psy.invokes.names)

    sched = psy.invokes.get('tra_adv').schedule
    sched.view()

    trans_info = TransInfo()
    print(trans_info.list)

    acc_trans = trans_info.get_trans_name('ACCKernelsTrans')

    sched, _ = acc_trans.apply(sched.children)

    sched.view()

    acc_trans = trans_info.get_trans_name('ACCLoopTrans')

    # Add loop directives over latitude and collapse when they are
    # doubly nested with longitude inner. Default to independent. We
    # need to extend our dependence analysis to perform checks.
    count = 0
    for loop in sched.loops():
        kernels = loop.walk(loop.children, NemoKern)
        if kernels and loop.loop_type == "lat":
            count += 1
            if count == 14:
                # BUG: puts ACC declaration in the wrong place as the
                # loop structures are the same.
                continue
            child = loop.children[0]
            if isinstance(child, NemoLoop) and child.loop_type == "lon":
                sched, _ = acc_trans.apply(loop, collapse=2)
            else:
                sched, _ = acc_trans.apply(loop)

    sched.view()

    acc_trans = trans_info.get_trans_name('ACCParallelTrans')

    for loop in sched.loops():
        kernels = loop.walk(loop.children, NemoKern)
        if kernels and loop.loop_type == "levels":
            sched, _ = acc_trans.apply(loop)

    sched.view()

    psy.invokes.get('tra_adv').schedule = sched
    print(psy.gen)


if __name__ == "__main__":
    API = "nemo"
    _, INVOKEINFO = parse("../code/tra_adv.F90", api=API)
    PSY = PSyFactory(API).create(INVOKEINFO)
    print(PSY.gen)
    trans(PSY)
