# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2020-2024, Science and Technology Facilities Council.
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
# Authors R. W. Ford, A. R. Porter and S. Siso, STFC Daresbury Lab
#         I. Kavcic, Met Office
#         J. Henrichs, Bureau of Meteorology
# -----------------------------------------------------------------------------

''' This module contains the implementation of the ArrayReference node. '''

from psyclone.errors import GenerationError
from psyclone.psyir.nodes.array_mixin import ArrayMixin
from psyclone.psyir.nodes.literal import Literal
from psyclone.psyir.nodes.intrinsic_call import IntrinsicCall
from psyclone.psyir.nodes.ranges import Range
from psyclone.psyir.nodes.reference import Reference
from psyclone.psyir.symbols import (DataSymbol, UnresolvedType,
                                    UnsupportedFortranType, UnsupportedType,
                                    DataTypeSymbol, ScalarType, ArrayType,
                                    INTEGER_TYPE, Symbol)


class ArrayReference(ArrayMixin, Reference):
    '''
    Node representing a reference to an element or elements of an Array.
    The array-index expressions are stored as the children of this node.

    '''
    # Textual description of the node.
    _children_valid_format = "[DataNode | Range]+"
    _text_name = "ArrayReference"

    @staticmethod
    def create(symbol, indices):
        '''Create an ArrayReference instance given a symbol and a list of Node
        array indices. The special value ":" can be used as an index to
        create the corresponding PSyIR Range that represents ":".

        :param symbol: the symbol that this array is associated with.
        :type symbol: :py:class:`psyclone.psyir.symbols.DataSymbol`
        :param indices: a list of Nodes or ":" describing the array indices.
        :type indices: List[Union[:py:class:`psyclone.psyir.nodes.Node`,":"]]

        :returns: an ArrayReference instance.
        :rtype: :py:class:`psyclone.psyir.nodes.ArrayReference`

        :raises GenerationError: if the arguments to the create method \
            are not of the expected type.

        '''
        if not isinstance(symbol, DataSymbol):
            raise GenerationError(
                f"symbol argument in create method of ArrayReference class "
                f"should be a DataSymbol but found '{type(symbol).__name__}'.")
        if not isinstance(indices, list):
            raise GenerationError(
                f"indices argument in create method of ArrayReference class "
                f"should be a list but found '{type(indices).__name__}'.")
        if not symbol.is_array:
            # Unresolved and Unsupported types may still be arrays
            if not isinstance(symbol.datatype, (UnresolvedType,
                                                UnsupportedType)):
                raise GenerationError(
                    f"expecting the symbol '{symbol.name}' to be an array, but"
                    f" found '{symbol.datatype}'.")
        elif len(symbol.shape) != len(indices):
            raise GenerationError(
                f"the symbol '{symbol.name}' should have the same number of "
                f"dimensions as indices (provided in the 'indices' argument). "
                f"Expecting '{len(indices)}' but found '{len(symbol.shape)}'.")

        array = ArrayReference(symbol)
        for ind, child in enumerate(indices):
            if child == ":":
                lbound = IntrinsicCall.create(
                    IntrinsicCall.Intrinsic.LBOUND,
                    [Reference(symbol),
                     ("dim", Literal(f"{ind+1}", INTEGER_TYPE))])
                ubound = IntrinsicCall.create(
                    IntrinsicCall.Intrinsic.UBOUND,
                    [Reference(symbol),
                     ("dim", Literal(f"{ind+1}", INTEGER_TYPE))])
                my_range = Range.create(lbound, ubound)
                array.addchild(my_range)
            else:
                array.addchild(child)
        return array

    def __str__(self):
        result = super().__str__() + "\n"
        for entity in self._children:
            result += str(entity) + "\n"
        return result

    @property
    def datatype(self):
        '''
        :returns: the datatype of the accessed array element(s).
        :rtype: :py:class:`psyclone.psyir.symbols.DataType`
        '''
        shape = self._get_effective_shape()
        if shape:
            if type(self.symbol) is Symbol:
                # We don't have any information on the shape of the original
                # declaration.
                orig_shape = None
            elif isinstance(self.symbol.datatype, ArrayType):
                # We have full type information so we know the shape of the
                # original declaration.
                orig_shape = self.symbol.datatype.shape
            elif (isinstance(self.symbol.datatype, UnsupportedFortranType) and
                  self.symbol.datatype.partial_datatype):
                # We have partial type information so we also know the shape
                # of the original declaration.
                orig_shape = self.symbol.datatype.partial_datatype.shape
            else:
                # We don't have any information on the shape of the original
                # declaration.
                orig_shape = None
            if (orig_shape is not None and len(shape) == len(orig_shape) and
                    all(self.is_full_range(idx) for idx in range(len(shape)))):
                # Although this access has a shape, it is in fact for the
                # whole array and therefore the type of the result is just
                # that of the base symbol. (This wouldn't be true for a
                # StructureReference but they have their own implementation
                # of this method.)
                return self.symbol.datatype
            if type(self.symbol) is Symbol or isinstance(self.symbol.datatype,
                                                         UnsupportedType):
                # Even if an Unsupported(Fortran)Type has partial type
                # information, we can't easily use it here because we'd need
                # to re-write the original Fortran declaration stored in the
                # type. We could manipulate the shape in the fparser2 parse
                # tree if need be but, at this point, we wouldn't know what
                # the variable name should be (TODO #2137).
                base_type = UnresolvedType()
            else:
                base_type = self.symbol.datatype
            # TODO #1857 - passing base_type as an instance of ArrayType
            # only works because the ArrayType constructor just pulls out
            # the intrinsic and precision properties of the type.
            return ArrayType(base_type, shape)

        # Otherwise, we're accessing a single element of the array.
        if type(self.symbol) is Symbol:
            return UnresolvedType()
        if isinstance(self.symbol.datatype, UnsupportedType):
            if (isinstance(self.symbol.datatype, UnsupportedFortranType) and
                    self.symbol.datatype.partial_datatype):
                precision = self.symbol.datatype.partial_datatype.precision
                intrinsic = self.symbol.datatype.partial_datatype.intrinsic
                return ScalarType(intrinsic, precision)
            # Since we're accessing a single element of an array of
            # UnsupportedType we have to create a new UnsupportedFortranType.
            # Ideally we would re-write the original Fortran
            # declaration stored in the type. We could remove the
            # shape in the fparser2 parse tree but, at this point, we
            # wouldn't know what the variable name should be (TODO #2137).
            return UnresolvedType()
        if isinstance(self.symbol.datatype.intrinsic, DataTypeSymbol):
            return self.symbol.datatype.intrinsic
        # TODO #1857: Really we should just be able to return
        # self.symbol.datatype here but currently arrays of scalars are
        # handled in a different way to all other types of array.
        return ScalarType(self.symbol.datatype.intrinsic,
                          self.symbol.datatype.precision)


# For AutoAPI documentation generation
__all__ = ['ArrayReference']
