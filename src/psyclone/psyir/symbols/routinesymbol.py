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
# Authors R. W. Ford and S. Siso, STFC Daresbury Lab
# -----------------------------------------------------------------------------

''' This module contains the RoutineSymbol.'''

from __future__ import absolute_import
from psyclone.psyir.symbols.datatypes import NoType
from psyclone.psyir.symbols.symbol import Symbol
from psyclone.psyir.symbols.typed_symbol import TypedSymbol


class RoutineSymbol(TypedSymbol):
    '''Symbol identifying a callable routine.

    :param str name: name of the routine.
    :param datatype: data type returned by the routine. Defaults to NoType.
    :type datatype: :py:class:`psyclone.psyir.symbols.DataType` or `NoneType`
    :param visibility: the visibility of the symbol.
    :type visibility: :py:class:`psyclone.psyir.symbols.Symbol.Visibility`
    :param interface: optional object describing the interface to this \
        symbol (i.e. whether it is local or accessed from some Container) \
        Defaults to :py:class:`psyclone.psyir.symbols.LocalInterface`.
    :type interface: :py:class:`psyclone.psyir.symbols.symbol.SymbolInterface`

    '''
    def __init__(self, name, datatype=None,
                 visibility=Symbol.DEFAULT_VISIBILITY,
                 interface=None):
        # We override the constructor purely to add a default datatype
        local_datatype = datatype
        if local_datatype is None:
            local_datatype = NoType()

        super(RoutineSymbol, self).__init__(name, local_datatype,
                                            visibility=visibility,
                                            interface=interface)

    def __str__(self):
        # This implementation could be moved to TypedSymbol but it is kept
        # here to enable us to keep TypedSymbol abstract.
        return "{0} : {1} <{2}>".format(self.name, type(self).__name__,
                                        str(self.datatype))


# For Sphinx AutoAPI documentation generation
__all__ = ["RoutineSymbol"]
