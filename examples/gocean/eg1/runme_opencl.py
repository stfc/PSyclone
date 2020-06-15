# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2020, Science and Technology Facilities Council
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
# Authors: S. Siso, STFC Daresbury Lab

'''A simple test script showing the introduction of OpenCL with PSyclone.
In order to use it you must first install PSyclone like so:

 >>> pip install --user psyclone

(or see the Getting Going section in ../../psyclone.pdf.) Once PSyclone is
installed, this script may be run by doing:

 >>> python runme_opencl.py

This should generate a lot of output, ending with the generated
Fortran invoke subroutines using FortCl to interface with the OpenCL
runtime. Additionally it should create a .cl file for each of the kernels.

'''

from __future__ import print_function
from psyclone.parse.algorithm import parse
from psyclone.psyGen import PSyFactory, TransInfo

API = "gocean1.0"
_, INVOKEINFO = parse("shallow_alg.f90", api=API)
PSY = PSyFactory(API).create(INVOKEINFO)
print(PSY.gen)

TRANS_INFO = TransInfo()
print(TRANS_INFO.list)
GLOBAL_TRANS = TRANS_INFO.get_trans_name('KernelGlobalsToArguments')
CL_TRANS = TRANS_INFO.get_trans_name('OCLTrans')

for invoke in PSY.invokes.invoke_list:
    print("Converting to OpenCL the invoke: " + invoke.name)
    schedule = invoke.schedule

    # Skip invoke_2
    if invoke.name == "invoke_2":
        continue

    # Remove the globals from inside each kernel
    for kern in schedule.kernels():
        print("Remove glovals from kernel: " + kern.name)
        GLOBAL_TRANS.apply(kern)

    # Transform invoke to OpenCL
    CL_TRANS.apply(schedule)

print(PSY.gen)
