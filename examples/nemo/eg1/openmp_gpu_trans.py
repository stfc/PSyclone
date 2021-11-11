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

''' PSyclone transformation script showing the introduction of OpenMP for GPU
directives into Nemo code. '''

from __future__ import print_function
from psyclone.psyGen import TransInfo
from psyclone.psyir.nodes import Loop, Assignment
from psyclone.domain.nemo.transformations import NemoAllArrayRange2LoopTrans
from psyclone.transformations import TransformationError

USE_GPU = True  # Enable for generating OpenMP target directives


def trans(psy):
    ''' Add OpenMP Target and Loop directives to Nemo loops over levels
    in the provided PSy-layer.

    :param psy: the PSy object which this script will transform.
    :type psy: :py:class:`psyclone.psyGen.PSy`
    :returns: the transformed PSy object.
    :rtype: :py:class:`psyclone.psyGen.PSy`

    '''
    omp_target_trans = TransInfo().get_trans_name('OMPTargetTrans')
    omp_loop_trans = TransInfo().get_trans_name('OMPLoopTrans')
    # Disabling worksharing will produce the 'loop' directive which is better
    # suited to map the work into the GPU
    omp_loop_trans.omp_worksharing = False

    print("Invokes found:")
    for invoke in psy.invokes.invoke_list:
        print(invoke.name)

        # Convert all array implicit loops to explicit loops
        explicit_loops = NemoAllArrayRange2LoopTrans()
        for assignment in invoke.schedule.walk(Assignment):
            explicit_loops.apply(assignment)

        # Add the OpenMP directives in each loop
        for loop in invoke.schedule.walk(Loop):
            if loop.loop_type == "levels":
                try:
                    if USE_GPU:
                        omp_target_trans.apply(loop)

                    omp_loop_trans.apply(loop)
                except TransformationError:
                    # This loop can not be transformed, proceed to next loop
                    continue

                num_nested_loops = 0
                next_loop = loop
                while isinstance(next_loop, Loop):
                    num_nested_loops += 1
                    if len(next_loop.loop_body.children) > 1:
                        break
                    next_loop = next_loop.loop_body.children[0]

                if num_nested_loops > 1:
                    loop.parent.parent.collapse = num_nested_loops

        return psy
