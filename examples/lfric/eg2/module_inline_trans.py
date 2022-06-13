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
# Author R. W. Ford, STFC Daresbury Laboratory
# Modified by S. Siso, and A. R. Porter, STFC Daresbury Laboratory

''' Example transformation script showing the use of the module-inline
    transformation for the LFRic domain. '''

from __future__ import print_function
from psyclone.transformations import KernelModuleInlineTrans
from psyclone.psyGen import Kern


def trans(psy):
    '''
    PSyclone transformation routine. This is an example which module-inlines
    the kernel used in the second 'invoke' in the supplied PSy object.

    :param psy: the PSy object that PSyclone has constructed for the \
                'invoke'(s) found in the Algorithm file.
    :type psy: :py:class:`psyclone.dynamo0p3.DynamoPSy`

    :returns: the transformed PSy object.
    :rtype: :py:class:`psyclone.dynamo0p3.DynamoPSy`

    '''
    invokes = psy.invokes
    print(psy.invokes.names)
    invoke = invokes.get("invoke_1")
    schedule = invoke.schedule
    print(schedule.view())
    # Find the kernel we want to inline.
    kern = schedule.walk(Kern)[0]
    # Setting module inline directly.
    kern.module_inline = True
    print(schedule.view())
    # Unsetting module inline via a transformation.
    inline_trans = KernelModuleInlineTrans()
    inline_trans.apply(kern, {"inline": False})
    print(schedule.view())
    # Setting module inline via a transformation.
    inline_trans.apply(kern)
    print(schedule.view())

    return psy
