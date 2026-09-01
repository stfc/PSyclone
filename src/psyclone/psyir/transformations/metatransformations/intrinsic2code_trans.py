# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

'''This module contains the Intrinsic2CodeTrans metatransformation.'''

from psyclone.psyGen import Transformation
from psyclone.psyir.nodes import IntrinsicCall
from psyclone.psyir.transformations.intrinsics.maxval2loop_trans\
    import Maxval2LoopTrans
from psyclone.psyir.transformations.intrinsics.minval2loop_trans\
    import Minval2LoopTrans
from psyclone.psyir.transformations.intrinsics.sum2loop_trans\
    import Sum2LoopTrans
from psyclone.psyir.transformations.intrinsics.product2loop_trans\
    import Product2LoopTrans
from psyclone.utils import transformation_documentation_wrapper


@transformation_documentation_wrapper
class Intrinsic2CodeTrans(Transformation):
    '''This metatransformation applies any of the Intrinsic2Code
    transformations to the provided input. The available transformations are
    Maxval2LoopTrans, Sum2LoopTrans, Minval2LoopTrans, or Product2LoopTrans.

    '''
    _SUB_TRANSFORMATIONS = [Maxval2LoopTrans, Sum2LoopTrans,
                            Minval2LoopTrans, Product2LoopTrans]

    # Create a map of intrinsic names to the appropriate Intrinsic2Code
    # transformation.
    intrinsic_to_trans = {"MAXVAL": Maxval2LoopTrans,
                          "SUM": Sum2LoopTrans,
                          "MINVAL": Minval2LoopTrans,
                          "PRODUCT": Product2LoopTrans}

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

    def apply(self, node: IntrinsicCall, **kwargs) -> None:
        '''
        Applies the appropriate Intrinsic2Code transformation to the provided
        input node.

        :param node: the IntrinsicCall to be transformed.
        '''
        # Split the options for the subtransformations. The options are
        # returned in the order of the _SUB_TRANSFORMATIONS list.
        kwargs_dict = {}
        local_kwargs, kwargs_dict["MAXVAL"], kwargs_dict["SUM"], \
            kwargs_dict["MINVAL"], kwargs_dict["PRODUCT"] = \
            self.split_kwargs(**kwargs)

        self.validate(node, **local_kwargs)

        # If the intrinsic is one of the supported intrinsics then
        # apply the relevant transformation.
        if node.intrinsic.name in Intrinsic2CodeTrans.intrinsic_to_trans:
            Intrinsic2CodeTrans.intrinsic_to_trans[node.intrinsic.name]().\
                apply(node, **kwargs_dict[node.intrinsic.name])
