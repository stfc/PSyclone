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
# Author R. Ford STFC Daresbury Lab

'''Example script showing how to apply OpenMP transformations to
dynamo code'''

from __future__ import print_function
from psyclone.parse.algorithm import parse
from psyclone.psyGen import PSyFactory
from psyclone.psyGen import TransInfo
API = "dynamo0.1"
_, INVOKEINFO = parse("dynamo_algorithm_mod.F90", api=API)
PSY = PSyFactory(API).create(INVOKEINFO)
print(PSY.gen)

print(PSY.invokes.names)

TRANS = TransInfo()
print(TRANS.list)

LOOP_FUSE = TRANS.get_trans_name('LoopFuse')
OMP_PAR = TRANS.get_trans_name('OMPParallelLoopTrans')

SCHEDULE = PSY.invokes.get('invoke_0').schedule
SCHEDULE.view()

FUSE_SCHEDULE, _ = LOOP_FUSE.apply(SCHEDULE.children[0], SCHEDULE.children[1])
FUSE_SCHEDULE.view()
OMP_SCHEDULE, _ = OMP_PAR.apply(FUSE_SCHEDULE.children[0])
OMP_SCHEDULE.view()

PSY.invokes.get('invoke_0').schedule = OMP_SCHEDULE

SCHEDULE = PSY.invokes.get('invoke_1_v2_kernel_type').schedule
SCHEDULE.view()

OMP_SCHEDULE, _ = OMP_PAR.apply(SCHEDULE.children[0])
OMP_SCHEDULE.view()

PSY.invokes.get('invoke_1_v2_kernel_type').schedule = OMP_SCHEDULE

SCHEDULE = PSY.invokes.get('invoke_2_v1_kernel_type').schedule
SCHEDULE.view()

OMP_SCHEDULE, _ = OMP_PAR.apply(SCHEDULE.children[0])
OMP_SCHEDULE.view()

PSY.invokes.get('invoke_2_v1_kernel_type').schedule = OMP_SCHEDULE

print(PSY.gen)
