# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

''' This module contains the ComplexLiteral node implementation.'''

from typing import Union, Optional
from psyclone.core import VariablesAccessMap, Signature, AccessType
from psyclone.psyir.nodes import Node
from psyclone.psyir.nodes.datanode import DataNode
from psyclone.psyir.nodes.literal import Literal
from psyclone.psyir.nodes.reference import Reference
from psyclone.psyir.symbols.symbol import Symbol
from psyclone.psyir.symbols.symbol_table import SymbolTable
from psyclone.psyir.symbols.datatypes import ScalarType, ArrayType, DataType


class ComplexLiteral(DataNode):
    '''
    Complex literals are not a good fit for the existing Literal class
    because they are a composite structure and can contain named references to
    named constants, not just numbers represented in textual form. It is more
    natural to represent one as a node with two children, the real part and
    the imaginary part, each of which can be a real-valued Literal or
    a Reference.
    '''

    # Textual description of the node.
    _children_valid_format = ("[Literal|Reference], [Literal|Reference]")
    _text_name = "ComplexLiteral"
    _colour = "yellow"

    def __init__(self, parent: Node = None):
        super().__init__(parent=parent)

    @staticmethod
    def create(re_part: Union[Literal, Reference],
               im_part: Union[Literal, Reference],
               parent: Optional[Node] = None):
        '''Create a ComplexLiteral with given real and imaginary parts.
        :param precision: the precision of this literal.
        :param parent: the parent node of this ComplexLiteral in the PSyIR.
        '''
        lit = ComplexLiteral(parent=parent)
        lit.children.extend([re_part, im_part])
        return lit

    @staticmethod
    def _validate_child(position: int, child: Node) -> bool:
        '''
        :param position: the position to be validated.
        :param child: a child to be validated.
        :return: whether the given child and position are valid for this node.
        '''
        if position < 0 or position >= 2:
            return False
        if not isinstance(child, (Literal, Reference)):
            return False
        if (isinstance(child.datatype, (ScalarType, ArrayType)) and
                child.datatype.intrinsic != ScalarType.Intrinsic.REAL and
                child.datatype.intrinsic != ScalarType.Intrinsic.INTEGER
                ):
            return False
        return True

    @property
    def re_part(self) -> Union[Literal, Reference]:
        '''Return the real part'''
        return self.children[0]

    @property
    def im_part(self) -> Union[Literal, Reference]:
        '''Return the imaginary part'''
        return self.children[1]

    @property
    def datatype(self) -> DataType:
        from psyclone.psyir.tools.type_info_computation \
            import compute_precision
        precisions = []
        for child in self.children:
            if isinstance(child.datatype, (ScalarType, ArrayType)):
                precisions.append(child.datatype.precision)
        precision = compute_precision(precisions)
        return ScalarType(ScalarType.Intrinsic.COMPLEX, precision)

    def node_str(self, colour: bool = True) -> str:
        '''
        Construct a text representation of this node, optionally containing
        colour control codes.
        '''
        return (f"{self.coloured_name(colour)}"
                f"[datatype: {self.datatype}]")

    def get_all_accessed_symbols(self) -> set[Symbol]:
        '''
        :returns: a set of all the symbols accessed inside this ComplexLiteral.
        '''
        symbols = super().get_all_accessed_symbols()
        dt = self.datatype
        if isinstance(dt.precision, DataNode):
            symbols.update(self.datatype.get_all_accessed_symbols())
        return symbols

    def reference_accesses(self) -> VariablesAccessMap:
        '''
        :returns: a map of all the symbol accessed inside this node, the
            keys are Signatures (unique identifiers to a symbol and its
            structure accessors) and the values are AccessSequence
            (a sequence of AccessTypes).

        '''
        access_info = VariablesAccessMap()
        # Any references must be references to named constants
        for (sig, seq) in super().reference_accesses().items():
            for info in seq:
                access_info.add_access(sig, AccessType.CONSTANT, info.node)
        # Add any precision symbols
        dt = self.datatype
        if isinstance(dt.precision, DataNode):
            precision_symbols = dt.get_all_accessed_symbols()
            for symbol in precision_symbols:
                access_info.add_access(
                    Signature(symbol.name), AccessType.CONSTANT, dt.precision)
        return access_info

    def replace_symbols_using(self,
                              table_or_symbol: Union[SymbolTable, Symbol]):
        '''
        Replace any Symbols referred to by this object with those in the
        supplied SymbolTable (or just the supplied Symbol instance) if they
        have matching names. If there is no match for a given Symbol then it
        is left unchanged.

        :param table_or_symbol: the symbol table from which to get replacement
            symbols or a single, replacement Symbol.

        '''
        self.datatype.replace_symbols_using(table_or_symbol)
        super().replace_symbols_using(table_or_symbol)


# For AutoAPI documentation generation
__all__ = ['ComplexLiteral']
