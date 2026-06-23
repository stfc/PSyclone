# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2026, Science and Technology Facilities Council.
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
# Authors M. Naylor, University of Cambridge, UK
# -----------------------------------------------------------------------------

''' This module contains the ArrayConstructor node implementation.'''

from __future__ import annotations
from typing import Union, TYPE_CHECKING
from psyclone.psyir.symbols import (
    UnresolvedType, DataType, ScalarType, ArrayType, DataTypeSymbol)
from psyclone.psyir.nodes.datanode import DataNode
if TYPE_CHECKING:
    from psyclone.psyir.nodes import Node


class ArrayConstructor(DataNode):
    '''
    Node representing an array constructor.
    '''

    # Textual description of the node.
    _children_valid_format = "[DataNode]*"
    _text_name = "ArrayConstructor"
    _colour = "yellow"

    def __init__(self, **kwargs):
        super().__init__(**kwargs)

    @staticmethod
    def create(elems: list[DataNode]) -> ArrayConstructor:
        '''Create an ArrayConstructor instance representing an array
        with the given elements.

        :param elems: the elements of the array being constructed.
        :returns: an ArrayConstructor instance.

        :raises GenerationError: if the arguments are not of the \
            expected type.
        '''
        array_cons = ArrayConstructor()
        for elem in elems:
            array_cons.children.append(elem)
        return array_cons

    @staticmethod
    def _validate_child(position: int, child: Node) -> bool:
        '''
        :param position: the position to be validated.
        :param child: a child to be validated.

        :return: whether the given child and position are valid for this node.
        '''
        return isinstance(child, DataNode)

    @property
    def datatype(self) -> Union[DataType, DataTypeSymbol]:
        '''
        :returns: the type of this array constructor.
        '''
        # The result of an array constructor is always a rank-1 array.
        # We look through the children to find the array-element type.
        elem_type = UnresolvedType()
        for child in self.children:
            if isinstance(child.datatype, ArrayType):
                elem_type = child.datatype.elemental_type
                break
            elif isinstance(child.datatype, ScalarType):
                elem_type = child.datatype
                break
            elif isinstance(child.datatype, DataTypeSymbol):
                elem_type = child.datatype
                break
        return ArrayType(elem_type, [ArrayType.Extent.ATTRIBUTE])

    def node_str(self, colour: bool = True) -> str:
        '''
        Construct a text representation of this node, optionally containing
        colour control codes.

        :param colour: whether or not to include colour control codes.

        :returns: description of this PSyIR node.
        '''
        return f"{self.coloured_name(colour)}[]"


# For AutoAPI documentation generation
__all__ = ['ArrayConstructor']
