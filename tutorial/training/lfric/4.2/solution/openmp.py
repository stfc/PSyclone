# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2024, Science and Technology Facilities Council.
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
# Author: J. Henrichs, Bureau of Meteorology

'''Python script intended to be passed to PSyclone's generate()
function via the -s option. It adds a generic OpenMP parallelisation
to the code.
'''

from psyclone.domain.common.transformations import KernelModuleInlineTrans
from psyclone.transformations import (OMPParallelTrans, OMPLoopTrans,
                                      TransformationError)
from psyclone.domain.lfric import LFRicKern, LFRicLoop
from psyclone.domain.lfric.transformations import LFRicLoopFuseTrans


def trans(psy):
    '''
    Take the supplied psy object, and fuse the first two loops

    :param psy: the PSy layer to transform.
    :type psy: :py:class:`psyclone.psyGen.PSy`

    :returns: the transformed PSy object.
    :rtype: :py:class:`psyclone.psyGen.PSy`

    '''
    omp_parallel = OMPParallelTrans()
    omp_loop = OMPLoopTrans()
    inline = KernelModuleInlineTrans()

    for invoke in psy.invokes.invoke_list:
        schedule = invoke.schedule

        # Module inline all kernels to help with inlining.
        for kern in schedule.walk(LFRicKern):
            inline.apply(kern)

        all_loops = list(schedule.walk(LFRicLoop))
        # We can't fuse the two builtins, and PSyclone will raise
        # an exception if you try. There is an option you can use
        # to overwrite PSyclone's warning, but it would create
        # incorrect core here. The field on w0 has 96 elements
        # (count the dots - 4x4x6), while the one on w3 one has
        # only 3*3*5 = 45. So the loops do not have the same loop
        # boundaries!
        fuse = LFRicLoopFuseTrans()
        try:
            fuse.apply(all_loops[0], all_loops[1])
        except IndexError:
            # We have two invokes, the second one has only one loop
            print(f"There is only one loop in {str(invoke.name)}")
        except TransformationError:
            print(f"Loops 0 and 1 cannot be fused in {type(invoke.name)}")

        # Add omp parallel around all loops
        omp_parallel.apply(all_loops)

        # And add omp do around all inner loop
        for loop in all_loops:
            omp_loop.apply(loop)

        print(schedule.view())
