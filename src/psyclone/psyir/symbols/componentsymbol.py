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

''' This module contains the ComponentSymbol. '''

from __future__ import absolute_import
from psyclone.psyir.symbols.datasymbol import DataSymbol
from psyclone.psyir.symbols.symbol import Symbol


class ComponentSymbol(DataSymbol):
    '''
    Symbol representing a component of a user-defined type.

    :param str name: the name of this symbol.
    :param datatype: the type represented by this symbol.
    :type datatype: :py:class:`psyclone.psyir.symbols.DataType`
    :param parent_symbol: the Symbol of which this is a component.
    :type parent_symbol: :py:class:`psyclone.psyir.symbols.Symbol`
    :param visibility: the visibility of this symbol.
    :type visibility: :py:class:`psyclone.psyir.symbols.Symbol.Visibility`
    :param interface: the interface to this symbol.
    :type interface: :py:class:`psyclone.psyir.symbols.SymbolInterface`

    '''
    def __init__(self, name, datatype, parent_symbol,
                 visibility=Symbol.DEFAULT_VISIBILITY,
                 constant_value=None, interface=None):
        super(ComponentSymbol, self).__init__(name, datatype,
                                              visibility=visibility,
                                              constant_value=constant_value,
                                              interface=interface)
        if not isinstance(parent_symbol, Symbol):
            raise TypeError(
                "The parent_symbol of a ComponentSymbol must be of type "
                "Symbol but got: {0}".format(type(parent_symbol).__name__))
        self._parent_symbol = parent_symbol


# For Sphinx auto-api
__all__ = ['ComponentSymbol']
