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

''' This module contains the implementation of the ArrayStructureReference
node. '''

from __future__ import absolute_import
from psyclone.psyir.nodes.array_node import ArrayNode
from psyclone.psyir.nodes.member_reference import MemberReference
from psyclone.psyir.nodes.structure_reference import StructureReference
from psyclone.psyir.nodes.datanode import DataNode
from psyclone.psyir import symbols


class ArrayStructureReference(StructureReference, ArrayNode):
    '''
    Node representing a reference to one or more elements of an array of
    structures (derived types).

    '''
    # Textual description of the node.
    _children_valid_format = "[MemberReference | None] [DataNode]*"
    _text_name = "ArrayStructureReference"

    @staticmethod
    def _validate_child(position, child):
        '''
        :param int position: the position to be validated.
        :param child: a child to be validated.
        :type child: :py:class:`psyclone.psyir.nodes.Node`

        :return: whether the given child and position are valid for this node.
        :rtype: bool

        '''
        if position == 0:
            if child is None:
                return True
            return isinstance(child, MemberReference)
        else:
            return isinstance(child, DataNode)

    @staticmethod
    def create(symbol, members, children):
        '''
        Create an ArrayStructureReference instance given a symbol and a
        list of components. e.g. for "field%bundle(2)%flag" this
        list would be [("bundle", [Literal("2", INTEGER4_TYPE)]), "flag"].

        :param symbol: the symbol that this reference is to.
        :type symbol: :py:class:`psyclone.psyir.symbols.DataSymbol`
        :param members: the component(s) of the structure that make up \
            the full reference. Any components that are array references must \
            provide the name of the array and a list of DataNodes describing \
            which part of it is accessed.
        :type members: list of str or 2-tuples containing (str, \
            list of nodes describing array access)
        :param children: a list of Nodes describing the array indices.
        :type children: list of :py:class:`psyclone.psyir.nodes.Node`

        :returns: an ArrayReference instance.
        :rtype: :py:class:`psyclone.psyir.nodes.ArrayReference`

        :raises GenerationError: if the arguments to the create method \
            are not of the expected type.

        '''
        if not isinstance(symbol.datatype, symbols.ArrayType):
            raise TypeError()

        # First use the StructureReference _create class method to create a
        # reference to the base structure of the array.
        # pylint: disable=protected_access
        ref = ArrayStructureReference._create(
            symbol, symbol.datatype.intrinsic, members)

        # Then add the array-index expressions. We don't validate the children
        # as that is handled in _validate_child.
        for child in children:
            ref.addchild(child)
            child.parent = ref
        return ref

    def __str__(self):
        result = super(ArrayStructureReference, self).__str__() + "\n"
        for entity in self._children:
            result += str(entity) + "\n"
        return result


# For AutoAPI documentation generation
__all__ = ['StructureReference']
