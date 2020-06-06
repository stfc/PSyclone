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
# Author R. W. Ford STFC Daresbury Lab
# -----------------------------------------------------------------------------

''' This module contains the Call node implementation.'''

from __future__ import absolute_import
from psyclone.psyir.nodes import Statement, Literal, Reference, Container
from psyclone.psyir.symbols import RoutineSymbol


class Call(Statement):
    '''Node representing a Call.

    :param routine: the routine that this call calls.
    :type routine: py:class:`psyclone.psyir.symbols.RoutineSymbol`

    list of Reference arguments

    Node parent

    '''
    # Textual description of the node.
    _children_valid_format = "<LeafNode>"
    _text_name = "Call"
    _colour_key = "Call"

    def __init__(self, routine, arguments, parent=None):
        super(Call, self).__init__(parent=parent)

        if not isinstance(routine, RoutineSymbol):
            raise TypeError(
                "Call routine argument should be a RoutineSymbol but found "
                "'{0}'.".format(type(routine).__name__))
        if not isinstance(arguments, list):
            raise TypeError(
                "Call arguments argument should be a list but found '{0}'."
                "".format(type(arguments).__name__))
        if not all([isinstance(arg, Reference) for arg in arguments]):
            raise TypeError(
                "Call arguments argument list entries should all be "
                "references but at least one is not.")

        self._routine = routine
        self._arguments = arguments[:]

    @property
    def routine(self):
        '''
        :returns: the routine that this call calls.
        :rtype: py:class:`psyclone.psyir.symbols.RoutineSymbol`
        '''
        return self._routine

    @property
    def arguments(self):
        ''' xxx '''
        return self._arguments

    def node_str(self, colour=True):
        '''
        Construct a text representation of this node, optionally containing
        colour control codes.

        :param bool colour: whether or not to include colour control codes.

        :returns: description of this PSyIR node.
        :rtype: str
        '''
        return "{0}[name='{1}', args={2}]".format(
            self.coloured_name(colour), self.routine.name,
            [argument.name for argument in self.arguments])
