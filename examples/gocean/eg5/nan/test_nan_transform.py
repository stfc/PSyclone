# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2021-2022, Science and Technology Facilities Council.
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
# Modified: R. W. Ford, STFC Daresbury Lab

'''Python script intended to be passed to PSyclone's generate()
function via the -s option. It adds kernel NAN-verification to
the invokes. This then creates code that, at runtime, verifies that
all input and output parameters of a region are a valid number, i.e.
not infinity or NAN.
'''

from __future__ import print_function

from psyclone.psyir.transformations import NanTestTrans


def trans(psy):
    '''
    Take the supplied psy object, and add verification to both
    invokes that read only parameters are not modified.

    :param psy: the PSy layer to transform.
    :type psy: :py:class:`psyclone.gocean1p0.GOPSy`

    :returns: the transformed PSy object.
    :rtype: :py:class:`psyclone.gocean1p0.GOPSy`

    '''
    nan_test = NanTestTrans()

    invoke = psy.invokes.get("invoke_0")
    schedule = invoke.schedule

    # You could just apply the transform for all elements of
    # psy.invokes.invoke_list. But in this case we also
    # want to give the regions a friendlier name:
    nan_test.apply(schedule.children, {"region_name": ("main", "init")})

    invoke = psy.invokes.get("invoke_1_update_field")
    schedule = invoke.schedule

    # Enclose everything in a nan_test region
    nan_test.apply(schedule.children, {"region_name": ("main", "update")})

    # print(schedule.view())
    return psy
