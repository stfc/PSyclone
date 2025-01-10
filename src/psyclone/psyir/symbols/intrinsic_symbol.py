# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2020-2025, Science and Technology Facilities Council.
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
# Authors R. W. Ford, S. Siso and N. Nobre, STFC Daresbury Lab
# -----------------------------------------------------------------------------

''' This module contains the IntrinsicSymbol.'''

from psyclone.psyir.symbols.routinesymbol import RoutineSymbol


class IntrinsicSymbol(RoutineSymbol):
    '''Symbol identifying a callable intrinsic routine.

    :param str name: name of the symbol.
    :param intrinsic: the intrinsic enum describing this Symbol.
    :type intrinsic: :py:class:`psyclone.psyir.nodes.IntrinsicCall.Intrinsic`
    :param kwargs: additional keyword arguments provided by
                   :py:class:`psyclone.psyir.symbols.TypedSymbol`
    :type kwargs: unwrapped dict.

    # TODO #2541: Currently name and the intrinsic should match, we really
    # just need the name, and make all the Intrinsic singature information
    # live inside the IntrinsicSymbol class.

    '''
    def __init__(self, name, intrinsic, **kwargs):
        super().__init__(name, **kwargs)
        self._intrinsic = intrinsic

    @property
    def intrinsic(self):
        '''
        :returns: the intrinsic enum describing this Symbol.
        :rtype: :py:class:`psyclone.psyir.nodes.IntrinsicCall.Intrinsic`
        '''
        return self._intrinsic


# For Sphinx AutoAPI documentation generation
__all__ = ["IntrinsicSymbol"]
