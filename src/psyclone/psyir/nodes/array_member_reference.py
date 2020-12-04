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
from psyclone.psyir.symbols import TypeSymbol
from psyclone.psyir.symbols.datatypes import StructureType


class ArrayMemberReference(ArrayNode, MemberReference):
    '''
    Node representing a reference to the element(s) of an array member of a
    structure (derived type).

    :param target:
    :type target:
    :param str member:
    :param parent:
    :type parent:
    :param children:
    :type children:

    '''
    # Textual description of the node.
    _children_valid_format = "[DataNode | Range]*"
    _text_name = "ArrayMemberReference"

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
                "The parent of a MemberReference must be a StructureReference "
                "but found '{0}'.".format(type(parent).__name))
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
        self._children = []
        if children:
            self._children = children
        for child in self._children:
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
        return isinstance(child, (DataNode, Range))


__all__ = ['ArrayMemberReference']
