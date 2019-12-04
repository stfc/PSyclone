# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017-2018, Science and Technology Facilities Council
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
# Authors: R. W. Ford and A. R. Porter, STFC Daresbury Lab

'''A simple test script showing basic usage of the PSyclone API.
In order to use it you must first install PSyclone like so:

 >>> pip install --user psyclone

(or see the Getting Going section in ../../psyclone.pdf). Once PSyclone
is installed this script may be run by doing:

 >>> python runme.py

This should generate a lot of output, ending with a view of the
Schedules:

 >>> ...
 >>> Schedule[invoke='invoke_0']
 >>>    Loop[type='outer',field_space='cu',it_space='internal_pts']
 >>>        Loop[type='inner',field_space='cu',it_space='internal_pts']
 >>>            CodedKern compute_cu_code(cu_fld,p_fld,u_fld)
 >>>    Loop[type='outer',field_space='cv',it_space='internal_pts']
 >>>        Loop[type='inner',field_space='cv',it_space='internal_pts']
 >>>            CodedKern compute_cv_code(cv_fld,p_fld,v_fld)
 >>>...

'''

from __future__ import print_function
from psyclone.parse.algorithm import parse
from psyclone.psyGen import PSyFactory

API = "gocean1.0"
_, INVOKEINFO = parse("shallow_alg.f90", api=API)
PSY = PSyFactory(API).create(INVOKEINFO)

# Print the 'vanilla' generated Fortran
print(PSY.gen)

# Print a list of all of the invokes found
print(PSY.invokes.names)

# Print the Schedule of each of these Invokes
SCHEDULE = PSY.invokes.get('invoke_0').schedule
SCHEDULE.view()

SCHEDULE = PSY.invokes.get('invoke_1').schedule
SCHEDULE.view()

SCHEDULE = PSY.invokes.get('invoke_2').schedule
SCHEDULE.view()
