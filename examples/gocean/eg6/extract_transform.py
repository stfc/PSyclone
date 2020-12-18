# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2020, Science and Technology Facilities Council.
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
function via the -s option. It adds kernel extraction code to
the invokes. When the transformed program is compiled and run, it
will create one NetCDF file for each of the two invokes. A separate
driver program is also created for each invoke which can read the
created NetCDF files, execute the invokes and then compare the results.
At this stage it does not compile (TODO: #644), and the comparison is
missing (TODO: #647)
'''

from __future__ import print_function


def trans(psy):
    '''
    Take the supplied psy object, and add kernel extraction code.

    :param psy: the PSy layer to transform.
    :type psy: :py:class:`psyclone.gocean1p0.GOPSy`

    :returns: the transformed PSy object.
    :rtype: :py:class:`psyclone.gocean1p0.GOPSy`

    '''
    from psyclone.domain.gocean.transformations import GOceanExtractTrans
    extract = GOceanExtractTrans()

    invoke = psy.invokes.get("invoke_0")
    schedule = invoke.schedule
    _, _ = extract.apply(schedule.children,
                         {"create_driver": True,
                          "region_name": ("main", "init")})

    invoke = psy.invokes.get("invoke_1_update_field")
    schedule = invoke.schedule

    # Enclose everything in a extract region
    newschedule, _ = extract.apply(schedule.children,
                                   {"create_driver": True,
                                    "region_name": ("main", "update")})

    invoke.schedule = newschedule
    newschedule.view()
    return psy
