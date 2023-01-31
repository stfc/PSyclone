# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017-2022, Science and Technology Facilities Council.
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
# Authors: R. W. Ford and A. R. Porter, STFC Daresbury Laboratory.

''' Module implementing a `trans` method for use as a PSyclone transformation
    script. This example performs loop fusion.
'''

from __future__ import print_function
from psyclone.domain.lfric.transformations import LFRicLoopFuseTrans


def trans(psy):
    '''
    PSyclone transformation routine. This is an example which performs loop
    fusion for the Built-in 'setval_c' kernels in the first 'invoke'. For the
    sake of this example we use the 'same_space' option to tell the
    transformation that this is safe to do.

    :param psy: the PSy object that PSyclone has constructed for the \
                'invoke'(s) found in the Algorithm file.
    :type psy: :py:class:`psyclone.dynamo0p3.DynamoPSy`

    :returns: the transformed PSy object.
    :rtype: :py:class:`psyclone.dynamo0p3.DynamoPSy`

    '''
    print(psy.gen)

    print(psy.invokes.names)

    schedule = psy.invokes.get('invoke_0').schedule
    print(schedule.view())

    lftrans = LFRicLoopFuseTrans()

    # Since the arguments to the 'setval_c' built-in are on 'ANY_SPACE', we
    # assert that the various loops over degrees of freedom are of
    # the same extent and may safely be fused. (This is not actually true
    # for this particular example but we do this for the purposes of
    # illustration.)
    lftrans.apply(schedule[0], schedule[1], {"same_space": True})
    lftrans.apply(schedule[0], schedule[1], {"same_space": True})
    lftrans.apply(schedule[0], schedule[1], {"same_space": True})

    print(schedule.view())

    return psy
