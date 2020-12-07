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
from psyclone.psyir.symbols.typesymbol import TypeSymbol
from psyclone.psyir.symbols.datatypes import StructureType


class ArrayStructureMemberReference(ArrayNode, StructureMemberReference):
    '''
    Node representing a reference to an element of an array of derived types
    within a structure (derived type). As it is an array of derived types,
    its first child is a reference to a member of that derived type (or None)
    and subsequent children give the array-index expressions.

    :param target:
    :param member:
    :param parent:
    :param children:

    '''
    # Textual description of the node.
    _children_valid_format = "MemberReference [DataNode | Range]*"
    _text_name = "ArrayStructureMemberReference"

    def __init__(self, target, member, parent=None, children=None):
        # Avoid circular dependency
        from psyclone.psyir.nodes.structure_reference import StructureReference

        if not isinstance(target, (StructureType, TypeSymbol)):
            raise TypeError(
                "In MemberReference initialisation expecting a ComponentType "
                "but found '{0}'.".format(type(target).__name__))
        if parent and not isinstance(parent,
                                     (StructureReference, MemberReference)):
            raise TypeError(
                "The parent of a MemberReference must be either a "
                "StructureReference or another MemberReference but found "
                "'{0}'.".format(type(parent).__name))
        super(MemberReference, self).__init__(parent=parent)

        if isinstance(target, StructureType):
            # Store the component that this member points to
            self._component = target.components[member]
        elif isinstance(target, TypeSymbol):
            if isinstance(target.datatype, StructureType):
                self._component = target.datatype.components[member]
            else:
                # We only have a symbol for this structure type
                raise NotImplementedError("Huh")

        # The first child of this node will either be a MemberReference
        # (to a component of this derived type) or None.
        self._children = [None]
        for child in children:
            self._children.append(child)
            child.parent = self

    @staticmethod
    def _validate_child(position, child):
        '''
        :param int position: the position to be validated.
        :param child: a child to be validated.
        :type child: :py:class:`psyclone.psyir.nodes.Node`

        :return: whether the given child and position are valid for this node.
        :rtype: bool

        '''
        from psyclone.psyir.nodes import DataNode, Range
        # pylint: disable=unused-argument
        if position == 0:
            # The first child must either be a MemberReference or None.
            if child:
                return isinstance(child, MemberReference)
            return True
        return isinstance(child, (DataNode, Range))

    @property
    def component(self):
        return self._component


__all__ = ['ArrayStructureMemberReference']
