# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2021-2024, Science and Technology Facilities Council
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
# Authors: R. W. Ford and A. R. Porter, STFC Daresbury Laboratory
# Modified by J. Henrichs, Bureau of Meteorology

'''File containing a PSyclone transformation script for the dynamo0p3
API to apply colouring and then OpenMP parallelisation to an
invoke. This script can be applied via the -s option in the psyclone
command.

'''
from __future__ import print_function
from psyclone.transformations import DynamoOMPParallelLoopTrans, \
    TransformationError, Dynamo0p3ColourTrans, OMPParallelTrans, \
    Dynamo0p3OMPLoopTrans
from psyclone.psyGen import Loop
from psyclone.domain.lfric import LFRicConstants


def trans(psy):
    '''PSyclone transformation script for the dynamo0p3 API that applies
    loop colouring and OpenMP parallel loop parallelisation. It also
    outputs a textual representation of the transformated PSyIR.

    :param psy: a PSyclone PSy object which captures the algorithm and \
        kernel information required by PSyclone.
    :type psy: subclass of :py:class:`psyclone.psyGen.PSy`

    '''
    otrans = DynamoOMPParallelLoopTrans()
    ctrans = Dynamo0p3ColourTrans()
    ptrans = OMPParallelTrans()
    ltrans = Dynamo0p3OMPLoopTrans()
    const = LFRicConstants()

    for invoke in psy.invokes.invoke_list:
        schedule = invoke.schedule

        # Colour any loops that need colouring
        for loop in schedule.walk(Loop):
            if (loop.field_space.orig_name not in
                    const.VALID_DISCONTINUOUS_NAMES and
                    loop.iteration_space == "cell_column"):
                ctrans.apply(loop)

        # Add OpenMP parallel do directives to the loops
        for loop in schedule.walk(Loop):
            try:
                # Make sure reductions are reproducible
                if loop.reductions():
                    ptrans.apply(loop)
                    ltrans.apply(loop, {"reprod": True})
                else:
                    otrans.apply(loop)
            except TransformationError as info:
                print(str(info.value))

        # take a look at what we've done
        print(schedule.view())

        return psy
