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
# Authors R. W. Ford, S. Siso and N. Nobre, STFC Daresbury Lab
# -----------------------------------------------------------------------------

''' This module contains the RoutineSymbol.'''

from __future__ import absolute_import
from psyclone.psyir.symbols.datatypes import NoType
from psyclone.psyir.symbols.typed_symbol import TypedSymbol


class RoutineSymbol(TypedSymbol):
    '''Symbol identifying a callable routine.

    :param str name: name of the symbol.
    :param datatype: data type of the symbol. Default to NoType().
    :type datatype: :py:class:`psyclone.psyir.symbols.DataType`
    :param kwargs: additional keyword arguments provided by \
                   :py:class:`psyclone.psyir.symbols.TypedSymbol`
    :type kwargs: unwrapped dict.

    '''
    def __init__(self, name, datatype=None, **kwargs):
        # In general all arguments are processed by the _process_arguments
        # but in the 'datatype' case it must be done here because it is a
        # mandatory argument for the super constructor. There is equivalent
        # logic in the _process_argument for when the RoutineSymbol is
        # specialised instead of constructed.
        if datatype is None:
            datatype = NoType()
        super(RoutineSymbol, self).__init__(name, datatype)
        self._process_arguments(**kwargs)

    def _process_arguments(self, **kwargs):
        ''' Process the arguments for the constructor and the specialise
        methods. In this case it provides a default NoType datatype is
        none is found or provided.

        :param kwargs: keyword arguments which can be:\n
            the arguments in :py:class:`psyclone.psyir.symbols.TypedSymbol`
        :type kwargs: unwrapped dict.

        '''
        if "datatype" not in kwargs and \
           (not hasattr(self, '_datatype') or self.datatype is None):
            kwargs["datatype"] = NoType()
        super(RoutineSymbol, self)._process_arguments(**kwargs)

    def __str__(self):
        # This implementation could be moved to TypedSymbol but it is kept
        # here to enable us to keep TypedSymbol abstract.
        return f"{self.name}: {type(self).__name__}<{self.datatype}>"


# For Sphinx AutoAPI documentation generation
__all__ = ["RoutineSymbol"]
