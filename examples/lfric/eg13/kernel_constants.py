# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2019-2026, Science and Technology Facilities Council
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
# Authors: R. W. Ford, N. Nobre and S. Siso, STFC Daresbury Lab
# Modified by: J. Dendy, Met Office


'''An example PSyclone transformation script which makes ndofs, nqp*
and nlevels constant in all LFRic kernels called from within invokes
in the supplied algorithm code. This is achieved by applying the
LFRicKernelConstTrans transformation.

In the case where a space is defined as "any_space" in a kernel, the
associated ndofs value will not be modified (as the actual value could
change from one call to the next).

This script can be applied via the '-s' option when running PSyclone:

$ psyclone -api lfric -s ./kernel_constants.py \
../code/gw_mixed_schur_preconditioner_alg_mod.x90 \
-oalg alg.f90 -opsy psy.f90

'''

from psyclone.domain.common.transformations import KernelModuleInlineTrans
from psyclone.psyir.nodes import FileContainer
from psyclone.transformations import LFRicKernelConstTrans

# The number of layers to use when modifying a kernel to make the
# associated kernel value constant (rather than passing it in by
# argument).
NUMBER_OF_LAYERS = 20
# The element orders to use when modifying a kernel to make the
# associated degrees of freedom values constant (rather than passing
# them in by argument).
ELEMENT_ORDER_H = 0
ELEMENT_ORDER_V = 0
# Whether or not to make the number of quadrature points constant in a
# kernel (rather than passing them in by argument).
CONSTANT_QUADRATURE = True


def trans(psyir: FileContainer):
    '''PSyclone transformation script for the LFRic API to make the
    kernel values of ndofs, nlayers and nquadrature-point sizes constant.

    :param psyir: the PSyIR of the PSy-layer.

    '''
    mod_inline_trans = KernelModuleInlineTrans()
    const_trans = LFRicKernelConstTrans()

    for kernel in psyir.coded_kernels():
        print(f"  kernel '{kernel.name.lower()}'")
        mod_inline_trans.apply(kernel)
        const_trans.apply(kernel,
                          {"number_of_layers": NUMBER_OF_LAYERS,
                           "element_order_h": ELEMENT_ORDER_H,
                           "element_order_v": ELEMENT_ORDER_V,
                           "quadrature": CONSTANT_QUADRATURE})
