# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2021-2023, Science and Technology Facilities Council.
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
# Authors: R. W. Ford and A. R. Porter, STFC Daresbury Lab.

'''This module contains the LFRic Algorithm Invoke-call class.

'''
from psyclone.domain.common.algorithm import AlgorithmInvokeCall
from psyclone.domain.lfric.algorithm.psyir.lfric_kernel_functor import (
    LFRicFunctor, LFRicBuiltinFunctor)


class LFRicAlgorithmInvokeCall(AlgorithmInvokeCall):
    '''An invoke call from the LFRic Algorithm layer.'''

    _children_valid_format = "[LFRicFunctor]*"
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
        if (len(self.children) == 1 and
                isinstance(self.children[0], LFRicBuiltinFunctor)):
            # By default the name of the kernel is added if there is
            # only one functor. However we don't add this in LFRic if
            # the functor is a builtin.
            return f"invoke_{self._index}"
        return super()._def_routine_root_name()


# For AutoAPI documentation generation.
__all__ = ['LFRicAlgorithmInvokeCall']
