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
# Authors: A. R. Porter and S. Siso, STFC Daresbury Lab
# Modified J. G. Wallwork, University of Cambridge
# -----------------------------------------------------------------------------

''' This module contains the DataNode abstract node implementation.'''

from psyclone.psyir.nodes.node import Node


class DataNode(Node):
    '''
    Abstract node representing a general PSyIR expression that represents a
    value, which has a datatype.

    '''
    @property
    def datatype(self):
        '''
        :returns: the data-type of this Node. Currently this base
            implementation just returns UnresolvedType(). If a sub-class can do
            better then it must override this method.
        :rtype: :py:class:`psyclone.psyir.symbols.UnresolvedType`
        '''
        # pylint: disable=import-outside-toplevel
        from psyclone.psyir.symbols.datatypes import UnresolvedType
        return UnresolvedType()

    def is_character(self, unknown_as=None):
        '''
        :param unknown_as: Determines behaviour in the case where it cannot be
            determined whether the DataNode is a character. Defaults to None,
            in which case an exception is raised.
        :type unknown_as: Optional[bool]

        :returns: True if this DataNode is a character, otherwise False.
        :rtype: bool

        :raises ValueError: if the intrinsic type cannot be determined.

        '''
        # pylint: disable=import-outside-toplevel
        from psyclone.psyir.symbols.datatypes import ScalarType
        if not hasattr(self.datatype, "intrinsic"):
            if unknown_as is None:
                raise ValueError(
                    "is_character could not resolve whether the expression"
                    f" '{self.debug_string()}' operates on characters."
                )
            return unknown_as
        return (
            self.datatype.intrinsic == ScalarType.Intrinsic.CHARACTER
        )
