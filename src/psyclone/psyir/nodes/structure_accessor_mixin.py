
# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2024-2025, Science and Technology Facilities Council.
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
# Author: S. Siso STFC Daresbury Lab
# -----------------------------------------------------------------------------

''' This module contains the implementation of the StructureAccessor Mixin. '''

import abc
from psyclone.psyir.nodes.member import Member
from psyclone.errors import InternalError


class StructureAccessorMixin(metaclass=abc.ABCMeta):
    '''
    Abstract class used to add functionality common to Nodes that represent
    Structure accesses. These all have a "member" child at position 0.

    '''
    @property
    def member(self):
        '''
        :returns: the PSyIR child representing the accessor component.
        :rtype: :py:class:`psyclone.psyir.nodes.Member`

        :raises InternalError: if the first child of this node is not an
                               instance of Member.
        '''
        if not self.children or not isinstance(self.children[0], Member):
            raise InternalError(
                f"{type(self).__name__} malformed or incomplete. It must have "
                f"a first child that must be a (sub-class of) Member, but "
                f"found: {self.children}")
        return self.children[0]
