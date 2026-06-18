# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017-2026, Science and Technology Facilities Council.
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

from typing import Union
from psyclone.core import VariablesAccessMap
from psyclone.psyir.symbols import (
    Symbol, UnresolvedType, ScalarType, ArrayType, DataTypeSymbol)
from psyclone.psyir.nodes.datanode import DataNode


class ArrayConstructor(DataNode):
    '''
    Node representing an array constructor.
    '''

    # Textual description of the node.
    _children_valid_format = "[DataNode]*"
    _text_name = "ArrayConstructor"
    _colour = "yellow"

    def __init__(self,
                 type_spec: Union[ScalarType, DataTypeSymbol] = None,
                 **kwargs):
        super().__init__(**kwargs)
        self._type_spec = type_spec

    @staticmethod
    def create(*elems: DataNode):
        '''Create and ArrayConstructor instance representing an array
        with the given elements.

        :param elems: the elements of the array being constructed.
        :returns: an ArrayConstructor instance.
        :rtype: :py:class:`psyclone.psyir.nodes.IfBlock`

        :raises GenerationError: if the arguments are not of the \
            expected type.
        '''
        array_cons = ArrayConstructor()
        for elem in elems:
            array_cons.children.append(elem)
        return array_cons

    @staticmethod
    def _validate_child(position, child):
        '''
        :param int position: the position to be validated.
        :param child: a child to be validated.
        :type child: :py:class:`psyclone.psyir.nodes.Node`

        :return: whether the given child and position are valid for this node.
        :rtype: bool
        '''
        return isinstance(child, DataNode)

    @property
    def datatype(self):
        '''
        :returns: the type of this array constructor.
        :rtype: :py:class:`psyclone.psyir.symbols.DataType`
        '''
        # The result of an array constructor is always a rank-1 array.
        # First look at the type-spec to determine the array-element type.
        elem_type = UnresolvedType()
        if isinstance(self._type_spec, ScalarType):
            elem_type = self._type_spec
        elif isinstance(self._type_spec, DataTypeSymbol):
            elem_type = self._type_spec
        else:
            # Alternatively, look through the children to find the
            # array-element type.
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

    def node_str(self, colour=True):
        '''
        Construct a text representation of this node, optionally containing
        colour control codes.

        :param bool colour: whether or not to include colour control codes.

        :returns: description of this PSyIR node.
        :rtype: str
        '''
        string = f"{self.coloured_name(colour)}["
        if self._type_spec:
            string += f"type_spec={self._type_spec}"
        string += "]"
        return string

    def get_all_accessed_symbols(self) -> set[Symbol]:
        '''
        :returns: a set of all the symbols accessed inside this
            ArrayConstructor.
        '''
        syms = super().get_all_accessed_symbols()
        if self._type_spec:
            syms.update(self._type_spec.get_all_accessed_symbols())
        return syms

    def reference_accesses(self) -> VariablesAccessMap:
        '''
        :returns: a map of all the symbol accessed inside this node, the
            keys are Signatures (unique identifiers to a symbol and its
            structure accessors) and the values are AccessSequence
            (a sequence of AccessTypes).
        '''
        return super().reference_accesses()

    def replace_symbols_using(self, table_or_symbol):
        '''
        Replace any Symbols referred to by this object with those in the
        supplied SymbolTable (or just the supplied Symbol instance) if they
        have matching names. If there is no match for a given Symbol then it
        is left unchanged.

        :param table_or_symbol: the symbol table from which to get replacement
            symbols or a single, replacement Symbol.
        :type table_or_symbol: :py:class:`psyclone.psyir.symbols.SymbolTable` |
            :py:class:`psyclone.psyir.symbols.Symbol`

        '''
        super().replace_symbols_using(table_or_symbol)
        if self._type_spec:
            self._type_spec.replace_symbols_using(table_or_symbol)


# For AutoAPI documentation generation
__all__ = ['ArrayConstructor']
