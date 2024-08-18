# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2019-2024, Science and Technology Facilities Council.
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
# Author: A. R. Porter and S. Siso, STFC Daresbury Lab
# Modified: R. W. Ford, STFC Daresbury Lab

'''Python script intended to be passed to PSyclone's generate()
function via the -s option. Transforms the invoke with the addition of
OpenACC directives and then encloses the whole in a profiling region. '''

from __future__ import print_function
from acc_transform import trans as acc_trans
from psyclone.psyir.transformations import ProfileTrans


def trans(psy):
    '''
    Take the supplied psy object, add OpenACC directives and then enclose
    the whole schedule within a profiling region.

    :param psy: the PSy layer to transform.
    :type psy: :py:class:`psyclone.gocean1p0.GOPSy`

    :returns: the transformed PSy object.
    :rtype: :py:class:`psyclone.gocean1p0.GOPSy`

    '''
    proftrans = ProfileTrans()

    # Use the trans() routine in acc_transform.py to add the OpenACC directives
    psy = acc_trans(psy)

    invoke = psy.invokes.get('invoke_0_inc_field')
    schedule = invoke.schedule

    # Enclose everything in a profiling region
    proftrans.apply(schedule.children)
    print(schedule.view())
    return psy
