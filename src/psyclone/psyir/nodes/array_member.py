# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2020-2021, Science and Technology Facilities Council.
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
# Author: A. R. Porter and N. Nobre, STFC Daresbury Lab
# -----------------------------------------------------------------------------

''' This module contains the implementation of the ArrayMember node.'''

from __future__ import absolute_import
from psyclone.psyir.nodes.member import Member
from psyclone.psyir.nodes.array_mixin import ArrayMixin
from psyclone.errors import GenerationError


class ArrayMember(ArrayMixin, Member):
    '''
    Node representing an access to the element(s) of an array that is a
    member of a structure. Must have one or more children which give the
    array-index expressions for the array access.

    '''
    # Textual description of the node.
    _children_valid_format = "[DataNode | Range]+"
    _text_name = "ArrayMember"

    @staticmethod
    def create(member_name, indices):
        '''
        Construct an ArrayMember instance describing an array access to a
        member of a structure.

        e.g. for the Fortran `grid%subdomains(1,2)`, `subdomains` must be an
        array and we are accessing element (1,2) of it. We would therefore
        create the ArrayMember for this access by calling:

        >>> amem = ArrayMember.create("subdomains",
                                      [Literal("1", INTEGER_TYPE),
                                       Literal("2", INTEGER_TYPE)])

        :param str member_name: the name of the member of the structure that \
            is being accessed.
        :param indices: the array-index expressions.
        :type indices: list of :py:class:`psyclone.psyir.nodes.DataNode` or
            :py:class:`psyclone.psyir.nodes.Range`

        :raises GenerationError: if the supplied `indices` argument is not \
                                 a list.
        '''
        if not isinstance(indices, list):
            raise GenerationError(
                f"indices argument in create method of ArrayMember class "
                f"should be a list but found '{type(indices).__name__}'.")

        obj = ArrayMember(member_name)
        # Add any array-index expressions as children
        for child in indices:
            obj.addchild(child)
        return obj


# For AutoAPI documentation generation
__all__ = ['ArrayMember']
