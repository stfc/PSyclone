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

''' This module contains the implementation of the MemberReference node.'''

from __future__ import absolute_import
from psyclone.psyir.nodes.structure_member_reference import \
    StructureMemberReference
from psyclone.psyir.nodes.array_node import ArrayNode


class ArrayStructureMemberReference(StructureMemberReference, ArrayNode):
    '''
    Node representing a reference to an element of an array of structures
    within a structure. As it is an array of structures,
    its first child is a reference to a member of that structure (or None)
    and any subsequent children give the array-index expressions.

    :param struct_type:  the datatype of the structure containing the member \
                        that is being referenced.
    :type struct_type: :py:class:`psyclone.psyir.symbols.StructureType` or \
                       :py:class:`psyclone.psyir.symbols.TypeSymbol`
    :param str member_name: the name of the member of the structure that is \
        being referenced.
    :param parent: the parent of this node in the PSyIR tree.
    :type parent: subclass of :py:class:`psyclone.psyir.nodes.Node`

    '''
    # Textual description of the node.
    _children_valid_format = "None | MemberReference, [DataNode | Range]*"
    _text_name = "ArrayStructureMemberReference"

    @staticmethod
    def create(struct_type, member_name, parent=None, indices=None):
        '''
        Create a reference to one or more elements of an array of
        structures that is itself a member of a structure.

        :param struct_type:  the datatype of the structure containing the \
            member that is being referenced.
        :type struct_type: :py:class:`psyclone.psyir.symbols.StructureType` \
            or :py:class:`psyclone.psyir.symbols.TypeSymbol`
        :param str member_name: the name of the member of the structure that \
            is being referenced.
        :param parent: the parent of this node in the PSyIR tree.
        :type parent: subclass of :py:class:`psyclone.psyir.nodes.Node`
        :param indices: the array-index expressions.
        :type indices: list of :py:class:`psyclone.psyir.nodes.DataNode`

        :returns: a new ArrayStructureMemberReference object.
        :rtype: :py:class:`psyclone.psyir.nodes.ArrayStructureMemberReference`

        '''
        asmr = ArrayStructureMemberReference(struct_type, member_name,
                                             parent=parent)
        # The first child will be a reference to a member of this structure
        asmr.addchild(None)
        # Subsequent children represent the array-index expressions
        for child in indices:
            asmr.addchild(child)
            child.parent = asmr
        return asmr

    @staticmethod
    def _validate_child(position, child):
        '''
        :param int position: the position to be validated.
        :param child: a child to be validated.
        :type child: :py:class:`psyclone.psyir.nodes.Node`

        :return: whether the given child and position are valid for this node.
        :rtype: bool

        '''
        from psyclone.psyir.nodes import MemberReference, DataNode, Range
        if position == 0:
            # The first child must either be a MemberReference or None.
            if child is None:
                return True
            return isinstance(child, MemberReference)
        return isinstance(child, (DataNode, Range))


# For AutoAPI automatic documentation generation
__all__ = ['ArrayStructureMemberReference']
