# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2021-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

'''This module contains the LFRic Algorithm Invoke-call class.

'''
from psyclone.domain.common.algorithm import AlgorithmInvokeCall
from psyclone.domain.lfric.algorithm.psyir.lfric_kernel_functor import (
    LFRicFunctor, LFRicBuiltinFunctor)
from psyclone.psyir.nodes import Reference


class LFRicAlgorithmInvokeCall(AlgorithmInvokeCall):
    '''An invoke call from the LFRic Algorithm layer.'''

    _children_valid_format = "Reference, [LFRicFunctor]*"
    _text_name = "LFRicAlgorithmInvokeCall"

    @staticmethod
    def _validate_child(position, child):
        '''
        :param int position: the position to be validated.
        :param child: a child to be validated.
        :type child: :py:class:`psyclone.psyir.nodes.Node`

        :returns: whether the given child and position are valid for this node.
        :rtype: bool

        '''
        if position == 0:
            return isinstance(child, Reference)
        return isinstance(child, LFRicFunctor)

    @staticmethod
    def _def_container_root_name(node):
        '''
        :returns: the root name to use for the container.
        :rtype: str
        '''
        return f"{node.name}_psy"

    def _def_routine_root_name(self):
        '''
        :returns: the proposed processed routine name for this invoke.
        :rtype: str

        '''
        if (len(self.arguments) == 1 and
                isinstance(self.arguments[0], LFRicBuiltinFunctor)):
            # By default the name of the kernel is added if there is
            # only one functor. However we don't add this in LFRic if
            # the functor is a builtin.
            return f"invoke_{self._index}"
        return super()._def_routine_root_name()


# For AutoAPI documentation generation.
__all__ = ['LFRicAlgorithmInvokeCall']
