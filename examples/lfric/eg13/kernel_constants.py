# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2019, Science and Technology Facilities Council
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
# Author R. W. Ford STFC Daresbury Lab.


'''An example PSyclone transformation script which makes ndofs, nqp*
and nlevels constant in all LFRic kernels called from within invokes
in the supplied algorithm code. This is achieved by applying the
DynKernelConstTrans transformation.

In the case where a space is defined as "any_space" in a kernel, the
associated ndofs value will not be modified (as the actual value could
change from one call to the next).

The DynKernelConstTrans transformation is work in progress and the
current version is limited to printing out the arguments that would be
transformed and the values they would take.

This script can be applied via the '-s' option when running PSyclone:

$ psyclone -s ./kernel_constants.py \
../code/gw_mixed_schur_preconditioner_alg_mod.x90 \
-oalg alg.f90 -opsy psy.f90

'''

from __future__ import print_function
from psyclone.transformations import Dynamo0p3KernelConstTrans, \
    TransformationError

# The number of layers to use when modifying a kernel to make the
# associated kernel value constant (rather than passing it in by
# argument).
NUMBER_OF_LAYERS = 20
# The element order to use when modifying a kernel to make the
# associated degrees of freedom values constant (rather than passing
# them in by argument).
ELEMENT_ORDER = 0
# Whether or not to make the number of quadrature points constant in a
# kernel (rather than passing them in by argument).
CONSTANT_QUADRATURE = True


def trans(psy):
    '''PSyclone transformation script for the Dynamo0.3 API to make the
    kernel values of ndofs, nlayers and nquadrature-point sizes constant.

    '''
    const_trans = Dynamo0p3KernelConstTrans()

    for invoke in psy.invokes.invoke_list:
        print("invoke '{0}'".format(invoke.name))
        schedule = invoke.schedule
        for kernel in schedule.coded_kernels():
            print("  kernel '{0}'".format(kernel.name.lower()))
            try:
                const_trans.apply(kernel,
                                  {"number_of_layers": NUMBER_OF_LAYERS,
                                   "element_order": ELEMENT_ORDER,
                                   "quadrature": CONSTANT_QUADRATURE})
            except TransformationError:
                print("    Failed to modify kernel '{0}'".format(kernel.name))

    return psy
