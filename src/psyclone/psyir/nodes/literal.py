# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017-2021, Science and Technology Facilities Council.
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
# Authors R. W. Ford, A. R. Porter, S. Siso and N. Nobre, STFC Daresbury Lab
#         I. Kavcic, Met Office
#         J. Henrichs, Bureau of Meteorology
# -----------------------------------------------------------------------------

''' This module contains the Literal node implementation.'''

from __future__ import absolute_import

import re
import six

from psyclone.psyir.nodes.datanode import DataNode
from psyclone.psyir.symbols import ScalarType, ArrayType


class Literal(DataNode):
    '''
    Node representing a Literal. The value and datatype properties of
    this node are immutable.

    If the node represents "real" data and the value is expressed with
    an exponent (e.g. 3.2e4 or 0.1E-3) then the stored value always uses
    a lower case "e".

    :param str value: the value of the literal.
    :param datatype: the datatype of this literal.
    :type datatype: :py:class:`psyclone.psyir.symbols.DataType`
    :param parent: the parent node of this Literal in the PSyIR.
    :type parent: :py:class:`psyclone.psyir.nodes.Node`

    :raises TypeError: if the datatype is not an instance of \
        :py:class:`psyclone.psyir.symbols.DataType`.
    :raises ValueError: if the datatype is not one of self.VALID_DATA_TYPES.
    :raises TypeError: if the supplied value is not a string.
    :raises ValueError: if the supplied value is an empty string and the \
        Literal is not a CHARACTER.
    :raises ValueError: if the Literal is a BOOLEAN and the value is not \
        'true' or 'false'.
    :raises ValueError: if the Literal is a REAL but does not conform to \
        the supported format defined by the `_real_value` property.

    '''
    # Textual description of the node.
    _children_valid_format = "<LeafNode>"
    _text_name = "Literal"
    _colour = "yellow"
    _real_value = r'^[+-]?[0-9]+(\.[0-9]*)?([eE][+-]?[0-9]+)?$'

    def __init__(self, value, datatype, parent=None):
        super(Literal, self).__init__(parent=parent)

        # Checks for the datatype
        if not isinstance(datatype, (ScalarType, ArrayType)):
            raise TypeError(
                f"The datatype of a Literal must be an instance of "
                f"psyir.symbols.ScalarType or psyir.symbols.ArrayType "
                f"but found '{type(datatype).__name__}'")

        if not isinstance(value, six.string_types):
            raise TypeError(
                f"Literals must be supplied with a value encoded as a string "
                f"but found '{type(value).__name__}'")

        if not value and datatype.intrinsic != ScalarType.Intrinsic.CHARACTER:
            raise ValueError("A non-character literal value cannot be empty.")

        if (isinstance(datatype, ScalarType) and
                datatype.intrinsic == ScalarType.Intrinsic.BOOLEAN and
                value not in ("true", "false")):
            raise ValueError(
                f"A scalar boolean literal can only be: 'true' or "
                f"'false' but found '{value}'.")

        if datatype.intrinsic == ScalarType.Intrinsic.REAL:
            if not re.match(Literal._real_value, value):
                raise ValueError(
                    f"A scalar real literal value must conform to the "
                    f"supported format ('{Literal._real_value}') but found "
                    f"'{value}'.")
            # Ensure we always store any exponent with a lowercase 'e'
            self._value = value.replace("E", "e", 1)
        else:
            self._value = value
        self._datatype = datatype

    def __eq__(self, other):
        '''Checks the equality of this Literal with other. Literals are
        equal if they are the same type, and have the same datatype and
        value string (for now only compared with ==).

        :param object other: the object to check equality to.

        :returns: whether other is equal to self.
        :rtype: bool
        '''
        is_eq = super().__eq__(other)
        is_eq = is_eq and self.datatype == other.datatype
        is_eq = is_eq and self.value == other.value
        return is_eq

    @property
    def datatype(self):
        '''
        :returns: the type of this Literal.
        :rtype: :py:class:`psyclone.psyir.symbols.DataType`
        '''
        return self._datatype

    @property
    def value(self):
        '''
        :returns: String representing the literal value.
        :rtype: str
        '''
        return self._value

    def node_str(self, colour=True):
        '''
        Construct a text representation of this node, optionally containing
        colour control codes.

        :param bool colour: whether or not to include colour control codes.

        :returns: description of this PSyIR node.
        :rtype: str
        '''
        return f"{self.coloured_name(colour)}"\
               f"[value:'{self._value}', {self.datatype}]"
