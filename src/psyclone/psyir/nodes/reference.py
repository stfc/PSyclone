# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017-2023, Science and Technology Facilities Council.
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
# Modified A. B. G. Chalk and N. Nobre, STFC Daresbury Lab
# -----------------------------------------------------------------------------

''' This module contains the implementation of the Reference node.'''

from psyclone.core import AccessType, Signature
# We cannot import from 'nodes' directly due to circular import
from psyclone.psyir.nodes.datanode import DataNode
from psyclone.psyir.symbols import Symbol
from psyclone.psyir.symbols.datatypes import DeferredType


class Reference(DataNode):
    '''
    Node representing a Reference Expression.

    :param symbol: the symbol being referenced.
    :type symbol: :py:class:`psyclone.psyir.symbols.Symbol`
    :param kwargs: additional keyword arguments provided to the super class.
    :type kwargs: unwrapped dict.

    '''
    # Textual description of the node.
    _children_valid_format = "<LeafNode>"
    _text_name = "Reference"
    _colour = "yellow"

    def __init__(self, symbol, **kwargs):
        super().__init__(**kwargs)
        self.symbol = symbol

    def __eq__(self, other):
        '''
        Checks equivalence of two References. References are considered
        equivalent if they are the same type of Reference and their symbol
        name is the same.

        :param object other: the object to check equality to.

        :returns: whether other is equal to self.
        :rtype: bool
        '''
        is_eq = super().__eq__(other)
        # TODO #1698. Is reference equality enough comparing the symbols by
        # name? (Currently it is needed because symbol equality is not fully
        # implemented)
        is_eq = is_eq and (self.symbol.name == other.symbol.name)
        return is_eq

    @property
    def is_array(self):
        ''':returns: if this reference is an array. Note that if an array
        expression is used, it will be a Reference in the PSyIR, but if the
        symbol has been resolved, the symbol will be queried to determine
        whether it is an array or not.

        :rtype: bool

        '''
        try:
            # The standard symbol raises a ValueError if is_array
            # is called - which indicates that we don't know if this
            # symbol is an array or not.
            return self.symbol.is_array
        except ValueError:
            return False

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
                f"The {type(self).__name__} symbol setter expects a PSyIR "
                f"Symbol object but found '{type(symbol).__name__}'.")
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
        return f"{self.coloured_name(colour)}[name:'{self.name}']"

    def __str__(self):
        return self.node_str(False)

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
            :py:class:`psyclone.core.VariablesAccessInfo`

        '''
        sig, all_indices = self.get_signature_and_indices()
        for indices in all_indices:
            for index in indices:
                index.reference_accesses(var_accesses)
        var_accesses.add_access(sig, AccessType.READ, self, all_indices)

    @property
    def datatype(self):
        '''
        :returns: the datatype of this reference.
        :rtype: :py:class:`psyclone.psyir.symbols.DataType`
        '''
        if type(self.symbol) is Symbol:
            # We don't even have a DataSymbol
            return DeferredType()
        return self.symbol.datatype


# For AutoAPI documentation generation
__all__ = ['Reference']
