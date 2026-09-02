# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

'''This module contains the Intrinsic2CodeTrans metatransformation.'''

from typing import Any
import logging

from psyclone.psyGen import Transformation
from psyclone.psyir.nodes import IntrinsicCall
from psyclone.psyir.transformations.intrinsics.abs2code_trans\
    import Abs2CodeTrans
from psyclone.psyir.transformations.intrinsics.dotproduct2code_trans\
    import DotProduct2CodeTrans
from psyclone.psyir.transformations.intrinsics.matmul2code_trans\
    import Matmul2CodeTrans
from psyclone.psyir.transformations.intrinsics.max2code_trans\
    import Max2CodeTrans
from psyclone.psyir.transformations.intrinsics.maxval2loop_trans\
    import Maxval2LoopTrans
from psyclone.psyir.transformations.intrinsics.min2code_trans\
    import Min2CodeTrans
from psyclone.psyir.transformations.intrinsics.minval2loop_trans\
    import Minval2LoopTrans
from psyclone.psyir.transformations.intrinsics.product2loop_trans\
    import Product2LoopTrans
from psyclone.psyir.transformations.intrinsics.sign2code_trans\
    import Sign2CodeTrans
from psyclone.psyir.transformations.intrinsics.sum2loop_trans\
    import Sum2LoopTrans
from psyclone.utils import transformation_documentation_wrapper


@transformation_documentation_wrapper
class Intrinsic2CodeTrans(Transformation):
    '''This metatransformation applies any of the Intrinsic2Code
    transformations to the provided input.
    '''
    _SUB_TRANSFORMATIONS = [Maxval2LoopTrans, Sum2LoopTrans,
                            Minval2LoopTrans, Product2LoopTrans,
                            DotProduct2CodeTrans, Abs2CodeTrans,
                            Max2CodeTrans, Min2CodeTrans,
                            Sign2CodeTrans, Matmul2CodeTrans]

    # Create a map of intrinsic names to the appropriate Intrinsic2Code
    # transformation. This should be in the same order as the
    # _SUB_TRANSFORMATIONS else the _split_kwargs on this Transformation
    # may not work correctly.
    intrinsic_to_trans = {
        IntrinsicCall.Intrinsic.MAXVAL: Maxval2LoopTrans,
        IntrinsicCall.Intrinsic.SUM: Sum2LoopTrans,
        IntrinsicCall.Intrinsic.MINVAL: Minval2LoopTrans,
        IntrinsicCall.Intrinsic.PRODUCT: Product2LoopTrans,
        IntrinsicCall.Intrinsic.DOT_PRODUCT: DotProduct2CodeTrans,
        IntrinsicCall.Intrinsic.ABS: Abs2CodeTrans,
        IntrinsicCall.Intrinsic.MAX: Max2CodeTrans,
        IntrinsicCall.Intrinsic.MIN: Min2CodeTrans,
        IntrinsicCall.Intrinsic.SIGN: Sign2CodeTrans,
        IntrinsicCall.Intrinsic.MATMUL: Matmul2CodeTrans}

    def validate(self, node: IntrinsicCall, **kwargs) -> None:
        '''
        Validates the input options.

        :param node: the IntrinsicCall to be transformed.

        :raises TypeError: if the input node is not an IntrinsicCall.
        '''
        # Validate the provided options are allowed and typed correctly.
        self.validate_options(**kwargs)

        if not isinstance(node, IntrinsicCall):
            raise TypeError(
                f"Input node to {self.name} must be an IntrinsicCall but "
                f"received '{type(node).__name__}'."
            )

    def _split_kwargs(self, **kwargs) -> \
            tuple[dict[str, Any],
                  dict[IntrinsicCall.Intrinsic, dict[str, Any]]]:
        '''
        :returns: the kwargs for this transformation and the kwargs dict for
           the sub transformations indexed by appropriate Intrinsic.
        '''
        # The split_kwargs function returns a tuple containing the
        # kwargs for this transformation as the first entry and then
        # the kwargs for the SUB_TRANSFORAMTIONS in order as the following
        # entries.
        split_kwargs = self.split_kwargs(**kwargs)
        local_kwargs = split_kwargs[0]
        sub_kwargs = {}
        for i, intrinsic in enumerate(self.intrinsic_to_trans):
            sub_kwargs[intrinsic] = split_kwargs[i+1]
        return local_kwargs, sub_kwargs

    def apply(self, node: IntrinsicCall, **kwargs) -> None:
        '''
        Applies the appropriate Intrinsic2Code transformation to the provided
        input node.

        :param node: the IntrinsicCall to be transformed.
        '''
        # Split the options for the subtransformations. The options are
        # returned in the order of the _SUB_TRANSFORMATIONS list.
        kwargs_dict = {}
        local_kwargs, kwargs_dict = self._split_kwargs(**kwargs)

        self.validate(node, **local_kwargs)

        # If the intrinsic is one of the supported intrinsics then
        # apply the relevant transformation.
        if node.intrinsic in self.intrinsic_to_trans:
            self.intrinsic_to_trans[node.intrinsic]().apply(
                node, **kwargs_dict[node.intrinsic]
            )
        else:
            # Setup the logger.
            logger = logging.getLogger(__name__)
            supported_intrinsics = []
            if logger.isEnabledFor(logging.INFO):
                supported_intrinsics = [intrinsic.name for intrinsic in
                                        self.intrinsic_to_trans.keys()]
            logger.info(
                f"Input node was intrinsic of type '{node.intrinsic.name}' "
                f"which is not transformed by {self.name}. Supported "
                f"intrinsics are {supported_intrinsics}."
            )


__all__ = ["Intrinsic2CodeTrans"]
