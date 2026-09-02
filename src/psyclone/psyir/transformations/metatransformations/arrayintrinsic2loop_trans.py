# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

'''This module contains the ArrayIntrinsic2LoopTrans metatransformation.'''

from psyclone.psyir.nodes import IntrinsicCall
from psyclone.psyir.transformations.intrinsics.maxval2loop_trans\
    import Maxval2LoopTrans
from psyclone.psyir.transformations.intrinsics.minval2loop_trans\
    import Minval2LoopTrans
from psyclone.psyir.transformations.intrinsics.product2loop_trans\
    import Product2LoopTrans
from psyclone.psyir.transformations.intrinsics.sum2loop_trans\
    import Sum2LoopTrans
from psyclone.psyir.transformations.metatransformations.intrinsic2code_trans\
    import Intrinsic2CodeTrans
from psyclone.utils import transformation_documentation_wrapper


@transformation_documentation_wrapper
class ArrayIntrinsic2LoopTrans(Intrinsic2CodeTrans):
    '''This metatransformation applies any of the Intrinsic2Loop
    transformations to the provided input. The available transformations are
    Maxval2LoopTrans, Sum2LoopTrans, Minval2LoopTrans, or Product2LoopTrans.
    '''
    _SUB_TRANSFORMATIONS = [Maxval2LoopTrans, Sum2LoopTrans,
                            Minval2LoopTrans, Product2LoopTrans]

    # Create a map of intrinsic names to the appropriate Intrinsic2Code
    # transformation. This should be in the same order as the
    # _SUB_TRANSFORMATIONS else the _split_kwargs on this Transformation
    # may not work correctly.
    intrinsic_to_trans = {
        IntrinsicCall.Intrinsic.MAXVAL: Maxval2LoopTrans,
        IntrinsicCall.Intrinsic.SUM: Sum2LoopTrans,
        IntrinsicCall.Intrinsic.MINVAL: Minval2LoopTrans,
        IntrinsicCall.Intrinsic.PRODUCT: Product2LoopTrans}

    def apply(self, node: IntrinsicCall, **kwargs) -> None:
        '''
        Applies the appropriate Intrinsic2Loop transformation to the provided
        input node.

        :param node: the IntrinsicCall to be transformed.
        '''
        # The apply function is required for the docstring wrapper to work
        # correctly.
        super().apply(node, **kwargs)
