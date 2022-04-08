# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2020-2022, Science and Technology Facilities Council.
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

from __future__ import absolute_import
from psyclone.psyir.nodes.array_mixin import ArrayMixin
from psyclone.psyir.nodes.reference import Reference
from psyclone.psyir.symbols import DataSymbol, DeferredType, UnknownType
from psyclone.errors import GenerationError


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
        array indices.

        :param symbol: the symbol that this array is associated with.
        :type symbol: :py:class:`psyclone.psyir.symbols.DataSymbol`
        :param indices: a list of Nodes describing the array indices.
        :type indices: list of :py:class:`psyclone.psyir.nodes.Node`

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
            # Deferred and Unknown types may still be arrays
            if not isinstance(symbol.datatype, (DeferredType, UnknownType)):
                raise GenerationError(
                    f"expecting the symbol '{symbol.name}' to be an array, but"
                    f" found '{symbol.datatype}'.")
        if symbol.is_array:
            if len(symbol.shape) != len(indices):
                raise GenerationError(
                    f"the symbol '{symbol.name}' should have the same number "
                    f"of dimensions as indices (provided in the 'indices' "
                    f"argument). Expecting '{len(indices)}' but found "
                    f"'{len(symbol.shape)}'.")

        array = ArrayReference(symbol)
        for child in indices:
            array.addchild(child)
        return array

    def __str__(self):
        result = super(ArrayReference, self).__str__() + "\n"
        for entity in self._children:
            result += str(entity) + "\n"
        return result


# For AutoAPI documentation generation
__all__ = ['ArrayReference']
