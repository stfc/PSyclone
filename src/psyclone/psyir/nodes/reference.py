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
# Authors R. W. Ford, A. R. Porter and S. Siso, STFC Daresbury Lab
#         I. Kavcic, Met Office
#         J. Henrichs, Bureau of Meteorology
# -----------------------------------------------------------------------------

''' This module contains the implementation of the Reference node.'''

from __future__ import absolute_import
from psyclone.psyir.nodes.datanode import DataNode
from psyclone.psyir.nodes.operation import Operation, BinaryOperation
from psyclone.core import AccessType, Signature
from psyclone.psyir.symbols import Symbol


class Reference(DataNode):
    '''
    Node representing a Reference Expression.

    :param symbol: the symbol being referenced.
    :type symbol: :py:class:`psyclone.psyir.symbols.Symbol`
    :param parent: the parent node of this Reference in the PSyIR.
    :type parent: :py:class:`psyclone.psyir.nodes.Node` or NoneType

    '''
    # Textual description of the node.
    _children_valid_format = "<LeafNode>"
    _text_name = "Reference"
    _colour = "yellow"

    def __init__(self, symbol, parent=None):
        super(Reference, self).__init__(parent=parent)
        self.symbol = symbol

    @property
    def symbol(self):
        ''' Return the referenced symbol.

        :returns: the referenced symbol.
        :rtype: :py:class:`psyclone.psyir.symbols.Symbol`

        '''
        return self._symbol

    @symbol.setter
    def symbol(self, symbol):
        '''
        :param symbol: the new symbol being referenced.
        :type symbol: :py:class:`psyclone.psyir.symbols.Symbol`

        :raises TypeError: if the symbol argument is not of type Symbol.

        '''
        if not isinstance(symbol, Symbol):
            raise TypeError(
                "The Reference symbol setter expects a PSyIR Symbol object "
                "but found '{0}'.".format(type(symbol).__name__))
        self._symbol = symbol

    @property
    def name(self):
        ''' Return the name of the referenced symbol.

        :returns: Name of the referenced symbol.
        :rtype: str

        '''
        return self._symbol.name

    def node_str(self, colour=True):
        ''' Create a text description of this node in the schedule, optionally
        including control codes for colour.

        :param bool colour: whether or not to include colour control codes.

        :return: text description of this node.
        :rtype: str
        '''
        return self.coloured_name(colour) + "[name:'" + self.name + "']"

    def __str__(self):
        return self.node_str(False)

    def math_equal(self, other):
        ''':param other: the node to compare self with.
        :type other: py:class:`psyclone.psyir.nodes.Node`

        :returns: True if the self has the same results as other.
        :rtype: bool
        '''
        if not super(Reference, self).math_equal(other):
            return False
        return self.name == other.name

    def get_signature_and_indices(self):
        ''':returns: the Signature of this reference, and \
            an empty list of lists as 'indices' since this reference does \
            not represent an array access.
        :rtype: tuple(:py:class:`psyclone.core.Signature`, list of \
            list of indices)
        '''
        return (Signature(self.name), [[]])

    def reference_accesses(self, var_accesses):
        '''Get all variable access information from this node, i.e.
        it sets this variable to be read. It relies on
        `get_signature_and_indices` and will correctly handle
        array expressions.

        :param var_accesses: VariablesAccessInfo instance that stores the \
            information about variable accesses.
        :type var_accesses: \
            :py:class:`psyclone.core.access_info.VariablesAccessInfo`

        '''
        if (self.parent and isinstance(self.parent, Operation) and
                self.parent.operator in [BinaryOperation.Operator.LBOUND,
                                         BinaryOperation.Operator.UBOUND] and
                self.parent.children.index(self) == 0):
            # This reference is the first argument to a lbound or
            # ubound intrinsic. These intrinsics do not access the
            # array elements, they determine the array
            # bounds. Therefore there is no data dependence.
            return
        sig, all_indices = self.get_signature_and_indices()
        for indices in all_indices:
            for index in indices:
                index.reference_accesses(var_accesses)
        var_accesses.add_access(sig, AccessType.READ, self, all_indices)


# For AutoAPI documentation generation
__all__ = ['Reference']
