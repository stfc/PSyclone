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

''' This module contains the TypeSymbol. '''

from __future__ import absolute_import
from psyclone.psyir.symbols.symbol import Symbol


class TypeSymbol(Symbol):
    '''
    Symbol identifying a user-defined type.

    TODO
    '''
    def __init__(self, name, datatype,
                 visibility=Symbol.DEFAULT_VISIBILITY,
                 interface=None):
        super(TypeSymbol, self).__init__(name, visibility, interface)

        # The following attributes have setter methods (with error checking)
        self._datatype = None
        self.datatype = datatype

    def __str__(self):
        return "{0} : {1}".format(self.name, type(self).__name__)

    @property
    def datatype(self):
        '''
        :returns: datatype that this TypeSymbol represents.
        :rtype: :py:class:`psyclone.psyir.symbols.StructureType`
        '''
        return self._datatype

    @datatype.setter
    def datatype(self, value):
        ''' Setter for TypeSymbol datatype.

        :param value: new value for datatype.
        :type value: :py:class:`psyclone.psyir.symbols.StructureType`

        :raises TypeError: if value is not of the correct type.

        '''
        from psyclone.psyir.symbols import StructureType, DeferredType
        if not isinstance(value, (StructureType, DeferredType)):
            raise TypeError(
                "The datatype of a TypeSymbol must be specified using a "
                "StructureType but got: '{0}'".format(type(value).__name__))
        self._datatype = value


# For automatic documentation generation
__all__ = ['TypeSymbol']
