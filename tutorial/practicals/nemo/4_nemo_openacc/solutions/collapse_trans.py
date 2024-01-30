# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2020-2024, Science and Technology Facilities Council.
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
# Author: A. R. Porter, STFC Daresbury Lab
# Modified: R. W. Ford, STFC Daresbury Lab

'''A transformation script that adds a KERNELS region plus LOOP COLLAPSE
directives to the tracer-advection mini-app.  In order to use it you
must first install PSyclone. See README.md in the top-level psyclone
directory.

Once you have psyclone installed, this may be used by doing:

 $ psyclone -api nemo -s ./kernels_trans.py some_source_file.f90

This should produce a lot of output, ending with generated
Fortran. Note that the Fortran source files provided to PSyclone must
have already been preprocessed (if required).

'''

from __future__ import print_function
from psyclone.psyir.nodes import Loop
from psyclone.transformations import (ACCKernelsTrans, ACCDataTrans,
                                      ACCLoopTrans, TransformationError)


# Get the PSyclone transformations we will use
ACC_DATA_TRANS = ACCDataTrans()
ACC_KERNELS_TRANS = ACCKernelsTrans()
ACC_LOOP_TRANS = ACCLoopTrans()


def trans(psy):
    '''A PSyclone-script compliant transformation function that is
    bespoke for the tracer-advection mini-app. It encloses the
    body of the iteration loop within a KERNELS region and then
    applies COLLAPSE(2) to every latitude-longitude loop nest
    within that.

    :param psy: The PSy layer object to apply transformations to.
    :type psy: :py:class:`psyclone.psyGen.PSy`

    :returns: the transformed PSy layer object.
    :rtype: :py:class:`psyclone.psyGen.PSy`

    '''
    # Get the Schedule of the target routine
    sched = psy.invokes.get('tra_adv').schedule

    # Find the outer, 'iteration' loop
    tloop = None
    for node in sched.children:
        if isinstance(node, Loop) and node.loop_type == "tracers":
            tloop = node
            break
    ACC_KERNELS_TRANS.apply(tloop.loop_body)

    loops = tloop.walk(Loop)
    for loop in loops:
        if loop.loop_type == "lat":
            try:
                ACC_LOOP_TRANS.apply(loop, options={"collapse": 2})
            except TransformationError:
                pass

    # Finally, enclose the whole of the 'iteration' loop within
    # a data region
    ACC_DATA_TRANS.apply(tloop)

    print(sched.view())
