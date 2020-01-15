# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017-2020, Science and Technology Facilities Council.
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

''' This module contains the literal node implementation'''

import six
from psyclone.psyir.nodes import Node
from psyclone.psyir.symbols import DataType


class Literal(Node):
    '''
    Node representing a Literal. The value and datatype properties of
    this node are immutable.

    :param str value: the value of the literal.
    :param datatype: the datatype of this literal.
    :type datatype: :py:class:`psyclone.psyir.symbols.DataType`
    :param parent: the parent node of this Literal in the PSyIR.
    :type parent: :py:class:`psyclone.psyGen.Node`

    :raises TypeError: if the datatype is not an instance of \
                       :py:class:`psyclone.psyir.symbols.DataType`.
    :raises ValueError: if the datatype is not one of \
                        self.VALID_DATA_TYPES.
    :raises TypeError: if the supplied value is not a string.
    :raises ValueError: if the Literal is a BOOLEAN and the value is not \
                        'true' or 'false'.
    '''
    # A Literal cannot have DEFERRED type
    VALID_DATA_TYPES = [DataType.INTEGER, DataType.REAL,
                        DataType.CHARACTER, DataType.BOOLEAN]

    def __init__(self, value, datatype, parent=None):
        super(Literal, self).__init__(parent=parent)

        # Checks for the datatype
        if not isinstance(datatype, DataType):
            raise TypeError("The datatype of a Literal must be an instance of"
                            " psyir.symbols.DataType but got '{0}'".format(
                                type(datatype).__name__))
        if datatype not in self.VALID_DATA_TYPES:
            raise ValueError("The datatype of a Literal must be one of {0} "
                             "but got '{1}'".format(self.VALID_DATA_TYPES,
                                                    datatype))
        if not isinstance(value, six.string_types):
            raise TypeError("Literals must be supplied with "
                            "a value encoded as a string but got: {0}".
                            format(type(value).__name__))

        if datatype is DataType.BOOLEAN:
            if value not in ("true", "false"):
                raise ValueError(
                    "A DataType.BOOLEAN Literal can only be: 'true' or "
                    "'false' but got '{0}' instead.".format(value))

        self._datatype = datatype
        self._value = value

    @property
    def datatype(self):
        '''
        :returns: the type of this Literal.
        :rtype: :py:class:`psyclone.psyGen.DataType`
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
        return "{0}[value:'{1}', {2}]".format(
            self.coloured_name(colour),
            self._value, str(self.datatype))

    def math_equal(self, other):
        ''':param other: the node to compare self with.
        :type other: py:class:`psyclone.psyGen.Node`

        :return: if the self has the same results as other.
        :type: bool
        '''

        return self.value == other.value



