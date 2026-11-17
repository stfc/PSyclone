# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2020-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

'''Module providing a transformation from a PSyIR MIN intrinsic to
PSyIR code. This could be useful if the MIN intrinsic is not supported
by the back-end or if the performance of the inline code is better
than the intrinsic.

'''

from psyclone.psyir.nodes import BinaryOperation, IntrinsicCall
from psyclone.psyir.transformations.intrinsics.minormax2code_trans import \
        MinOrMax2CodeTrans
from psyclone.utils import transformation_documentation_wrapper


@transformation_documentation_wrapper
class Min2CodeTrans(MinOrMax2CodeTrans):
    '''Provides a transformation from a PSyIR MIN Intrinsic node to
    equivalent code in a PSyIR tree. Validity checks are also
    performed (by a parent class).

    The transformation replaces

    .. code-block:: python

        R = MIN(A, B, C ...)

    with the following logic:

    .. code-block:: python

        R = A
        if B < R:
            R = B
        if C < R:
            R = C
        ...

    '''
    def __init__(self):
        super().__init__()
        self._intrinsic = IntrinsicCall.Intrinsic.MIN
        self._compare_operator = BinaryOperation.Operator.LT

    def apply(self, node, options=None, **kwargs):
        '''
        Applies the Min2CodeTrans to the provided node.


        :param node: a MIN intrinsic.
        :type node: :py:class:`psyclone.psyir.nodes.IntrinsicCall`
        :param options: a dictionary with options for transformations.
        :type options: Optional[Dict[str, Any]]
        '''
        super().apply(node, options=options, **kwargs)


# For AutoAPI auto-documentation generation.
__all__ = ["Min2CodeTrans"]
