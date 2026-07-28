# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2019-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------


'''An example PSyclone transformation script which makes ndofs, nqp*
and nlevels constant in all LFRic kernels called from within invokes
in the supplied algorithm code. This is achieved by applying the
LFRicKernelConstTrans transformation.

In the case where a space is defined as "any_space" in a kernel, the
associated ndofs value will not be modified (as the actual value could
change from one call to the next).

The LFRicKernelConstTrans transformation is work in progress and the
current version is limited to printing out the arguments that would be
transformed and the values they would take.

This script can be applied via the '-s' option when running PSyclone:

$ psyclone -api lfric -s ./kernel_constants.py \
../code/gw_mixed_schur_preconditioner_alg_mod.x90 \
-oalg alg.f90 -opsy psy.f90

'''

from psyclone.transformations import LFRicKernelConstTrans, \
    TransformationError

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


def trans(psyir):
    '''PSyclone transformation script for the LFRic API to make the
    kernel values of ndofs, nlayers and nquadrature-point sizes constant.

    :param psyir: the PSyIR of the PSy-layer.
    :type psyir: :py:class:`psyclone.psyir.nodes.FileContainer`

    '''
    const_trans = LFRicKernelConstTrans()

    for kernel in psyir.coded_kernels():
        print(f"  kernel '{kernel.name.lower()}'")
        try:
            const_trans.apply(kernel,
                              {"number_of_layers": NUMBER_OF_LAYERS,
                               "element_order_h": ELEMENT_ORDER_H,
                               "element_order_v": ELEMENT_ORDER_V,
                               "quadrature": CONSTANT_QUADRATURE})
        except TransformationError:
            print(f"    Failed to modify kernel '{kernel.name}'")
