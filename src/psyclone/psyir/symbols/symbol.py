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

''' This module contains the generic Symbol and the SymbolError.'''

from enum import Enum
import six


class SymbolError(Exception):
    '''
    PSyclone-specific exception for use with errors relating to the Symbol and
    SymbolTable in the PSyIR.

    :param str value: the message associated with the error.
    '''
    def __init__(self, value):
        Exception.__init__(self, value)
        self.value = "PSyclone SymbolTable error: "+value

    def __str__(self):
        return str(self.value)


class Symbol(object):
    '''
    Generic Symbol item for the Symbol Table. It always has a fixed name label
    that matches with the key in the SymbolTables that contain the symbol.
    If the symbol is not public then it is only visible to those nodes that
    are descendents of the Node to which its containing Symbol Table belongs.

    :param str name: name of the symbol.
    :param visibility: the visibility of the symbol.
    :type visibility: :py:class:`psyclone.psyir.symbols.Symbol.Visibility`

    :raises TypeError: if the name is not a str or visibility is not an \
                       instance of Visibility.
    '''

    class Visibility(Enum):
        ''' Enumeration of the different visibility attributes supported in
        the PSyIR. If no visibility information is supplied for a Symbol then
        it is given the DEFAULT_VISIBILITY value.

        PUBLIC: the symbol is visible in any scoping region that has access to
                the SymbolTable containing it.
        PRIVATE: the symbol is only visibile inside the scoping region that
                 contains the SymbolTable to which it belongs.
        '''
        PUBLIC = 1
        PRIVATE = 2

    # The visibility given to any PSyIR symbols that are created without being
    # given an explicit visibility.
    DEFAULT_VISIBILITY = Visibility.PUBLIC

    def __init__(self, name, visibility=DEFAULT_VISIBILITY):

        if not isinstance(name, six.string_types):
            raise TypeError(
                "{0} 'name' attribute should be of type 'str'"
                " but '{1}' found.".format(type(self).__name__, type(name)))
        if not isinstance(visibility, Symbol.Visibility):
            raise TypeError(
                "{0} 'visibility' attribute should be of type "
                "psyir.symbols.Symbol.Visibility but '{1}' found.".format(
                    type(self).__name__, type(visibility).__name__))
        self._name = name
        self._visibility = visibility

    @property
    def name(self):
        '''
        :returns: name of the Symbol.
        :rtype: str
        '''
        return self._name

    @property
    def visibility(self):
        '''
        :returns: the visibility of this Symbol.
        :rtype: :py:class:`psyclone.psyir.symbol.Symbol.Visibility`
        '''
        return self._visibility

    def __str__(self):
        return self.name
