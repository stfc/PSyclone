# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2023-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

'''Transform a PSyclone GOcean algorithm-layer-specific invoke call
into a call to the corresponding PSy-layer routine.

'''

from psyclone.domain.common.transformations import AlgInvoke2PSyCallTrans


class GOceanAlgInvoke2PSyCallTrans(AlgInvoke2PSyCallTrans):
    '''Transforms a GOceanAlgorithmInvokeCall into a standard Call to a
    generated PSy-layer routine.

    This transformation would normally be written as a lowering method
    on a GOceanAlgorithmInvokeCall. However, we don't always want to
    lower the code as we want the flexibility to also be able to
    output algorithm-layer code containing invoke's. We therefore need
    to selectively apply the lowering, which is naturally written as a
    transformation.

    '''
    def get_arguments(self, node, options=None):
        '''Creates the GOcean processed (lowered) argument list from the
        argument lists of the kernels within the invoke call and the
        kernel metadata.

        :param node: a GOcean algorithm invoke call.
        :type node: :py:class:`psyclone.domain.common.algorithm.psyir.\
            AlgorithmInvokeCall`
        :param options: a dictionary with options for transformations.
        :type options: Optional[Dict[str, Any]]

        :returns: the processed (lowered) argument list.
        :rtype: List[:py:class:`psyclone.psyir.nodes.Node`]

        '''
        arguments = []
        for kern in node.arguments:
            for arg in kern.children:
                self._add_arg(arg, arguments)
        return arguments
