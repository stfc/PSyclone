# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2020, Science and Technology Facilities Council.
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
# Author: A. R. Porter, STFC Daresbury Lab
# -----------------------------------------------------------------------------

''' This module contains the implementation of the ArrayMember node.'''

from __future__ import absolute_import
from psyclone.psyir.nodes.member import Member
from psyclone.psyir.nodes.array_mixin import ArrayMixin
from psyclone.psyir.nodes.datanode import DataNode
from psyclone.psyir.nodes.ranges import Range


class ArrayMember(Member, ArrayMixin):
    '''
    Node representing an access to the element(s) of an array that is a
    member of a structure.

    '''
    # Textual description of the node.
    _children_valid_format = "[DataNode | Range]+"
    _text_name = "ArrayMember"

    @staticmethod
    def create(member_name, indices, struct_type=None, parent=None):
        '''
        Construct an ArrayMember instance.

        :param struct_type: the datatype of the parent structure containing \
                            the member that is being accessed.
        :type struct_type: :py:class:`psyclone.psyir.symbols.StructureType` \
            or :py:class:`psyclone.psyir.symbols.TypeSymbol`
        :param str member_name: the name of the member of the structure that \
            is being accessed.
        :param indices: the array-index expressions.
        :type indices: list of :py:class:`psyclone.psyir.nodes.DataNode`
        :param parent: the parent of this node in the PSyIR tree.
        :type parent: subclass of :py:class:`psyclone.psyir.nodes.Node`

        '''
        return ArrayMember._create_array_of_structs(
            member_name, indices=indices, struct_type=struct_type,
            parent=parent)

    @staticmethod
    def _validate_child(position, child):
        '''
        :param int position: the position to be validated.
        :param child: a child to be validated.
        :type child: :py:class:`psyclone.psyir.nodes.Node`

        :return: whether the given child and position are valid for this node.
        :rtype: bool

        '''
        # pylint: disable=unused-argument
        return isinstance(child, (DataNode, Range))


# For AutoAPI documentation generation
__all__ = ['ArrayMember']
