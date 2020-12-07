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

''' This module contains the implementation of the ArrayMemberReference
    node.'''

from __future__ import absolute_import
from psyclone.psyir.nodes.member_reference import MemberReference
from psyclone.psyir.nodes.array_node import ArrayNode
from psyclone.psyir.nodes.datanode import DataNode
from psyclone.psyir.nodes.ranges import Range


class ArrayMemberReference(MemberReference, ArrayNode):
    '''
    Node representing a reference to the element(s) of an array that is a
    member of a structure.

    :param struct_type: the datatype of the structure containing the member \
                        that is being referenced.
    :type struct_type: :py:class:`psyclone.psyir.symbols.StructureType` or \
                       :py:class:`psyclone.psyir.symbols.TypeSymbol`
    :param str member: the member of the 'struct_type' structure that is \
                       being referenced.
    :param parent: the parent of this node in the PSyIR tree.
    :type parent: :py:class:`psyclone.psyir.nodes.StructureReference` or \
                  :py:class:`psyclone.psyir.nodes.MemberReference`
    :param children: PSyIR nodes representing any array-index expressions.
    :type children: list of :py:class:`psyclone.psyir.nodes.Node` or NoneType

    '''
    # Textual description of the node.
    _children_valid_format = "[DataNode | Range]*"
    _text_name = "ArrayMemberReference"

    def __init__(self, struct_type, member_name, parent=None, indices=None):
        MemberReference.__init__(self, struct_type, member_name, parent=parent)
        # The MemberReference node is a leaf so now call the ArrayNode
        # constructor to set up the children (representing any array-index
        # expressions).
        if indices:
            ArrayNode.__init__(self, parent=parent, children=indices)

    @staticmethod
    def create(struct_type, member_name, parent=None, indices=None):
        ''' '''
        amref = ArrayMemberReference(struct_type, member_name, parent=parent)
        # Children represent the array-index expressions
        if indices:
            for child in indices:
                amref.addchild(child)
                child.parent = amref
        return amref

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
__all__ = ['ArrayMemberReference']
