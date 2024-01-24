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
# Author: J. Henrichs, Bureau of Meteorology
# Modified by D. Sergeev, University of Exeter
# Modified by R. W. Ford, STFC Daresbury Lab

'''Python script intended to be passed to PSyclone's generate()
function via the -s option. It adds kernel extraction code to
all invokes.
'''

from __future__ import print_function

from psyclone.domain.lfric.transformations import LFRicExtractTrans


def trans(psy):
    '''
    Take the supplied psy object, and add kernel extraction code.

    :param psy: the PSy layer to transform.
    :type psy: :py:class:`psyclone.psyGen.PSy`

    :returns: the transformed PSy object.
    :rtype: :py:class:`psyclone.psyGen.PSy`

    '''
    extract = LFRicExtractTrans()

    for invoke_name in psy.invokes.names:

        invoke = psy.invokes.get(invoke_name)

        # Now get the schedule, to which we want to apply the transformation
        schedule = invoke.schedule

        # Apply the transformation
        extract.apply(schedule, {"region_name": ("time_evolution",
                                                 str(invoke_name))})

        # Just as feedback: show the modified schedule, which should have
        # a new node at the top:
        print(schedule.view())

    return psy
