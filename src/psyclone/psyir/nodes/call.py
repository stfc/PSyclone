# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2020-2021, Science and Technology Facilities Council.
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
# Authors R. W. Ford and S. Siso STFC Daresbury Lab
# -----------------------------------------------------------------------------

''' This module contains the Call node implementation.'''

from __future__ import absolute_import
from psyclone.psyir.nodes.statement import Statement
from psyclone.psyir.nodes.datanode import DataNode
from psyclone.psyir.symbols import RoutineSymbol
from psyclone.errors import GenerationError


class Call(Statement, DataNode):
    ''' Node representing a Call. This can be found as a standalone statement
    or an expression.

    TODO #1437: The combined Statement and Expression implementation is simple
    but it has some shortcoming that may need to be addressed.

    :param routine: the routine that this call calls.
    :type routine: py:class:`psyclone.psyir.symbols.RoutineSymbol`
    :param parent: parent of this node in the PSyIR.
    :type parent: sub-class of :py:class:`psyclone.psyir.nodes.Node`

    :raises TypeError: if the routine argument is not a RoutineSymbol.

    '''
    # Textual description of the node.
    _children_valid_format = "[DataNode]*"
    _text_name = "Call"
    _colour = "cyan"

    def __init__(self, routine, parent=None):
        super(Call, self).__init__(parent=parent)

        if not isinstance(routine, RoutineSymbol):
            raise TypeError(
                "Call routine argument should be a RoutineSymbol but found "
                "'{0}'.".format(type(routine).__name__))

        self._routine = routine

    @classmethod
    def create(cls, routine, arguments):
        '''Create an instance of class cls given valid instances of a routine
        symbol, and a list of child nodes for its arguments.

        :param routine: the routine that class cls calls.
        :type routine: py:class:`psyclone.psyir.symbols.RoutineSymbol`
        :param arguments: the arguments to this routine. These are \
            added as child nodes.
        :type arguments: list of :py:class:`psyclone.psyir.nodes.DataNode`

        :raises GenerationError: if the routine argument is not a \
            RoutineSymbol.
        :raises GenerationError: if the arguments argument is not a \
            list.

        :returns: an instance of cls.
        :rtype: :py:class:`psyclone.psyir.nodes.Call` or a subclass thereof.

        '''
        if not isinstance(routine, RoutineSymbol):
            raise GenerationError(
                "Call create routine argument should be a RoutineSymbol but "
                "found '{0}'.".format(type(routine).__name__))
        if not isinstance(arguments, list):
            raise GenerationError(
                "Call create arguments argument should be a list but found "
                "'{0}'.".format(type(arguments).__name__))

        call = cls(routine)
        call.children = arguments
        return call

    @staticmethod
    def _validate_child(position, child):
        '''
        :param int position: the position to be validated.
        :param child: a child to be validated.
        :type child: :py:class:`psyclone.psyir.nodes.Node`

        :return: whether the given child and position are valid for this node.
        :rtype: bool

        '''
        return isinstance(child, DataNode)

    @property
    def routine(self):
        '''
        :returns: the routine symbol that this call calls.
        :rtype: py:class:`psyclone.psyir.symbols.RoutineSymbol`
        '''
        return self._routine

    def node_str(self, colour=True):
        '''
        Construct a text representation of this node, optionally containing
        colour control codes.

        :param bool colour: whether or not to include colour control codes.

        :returns: description of this PSyIR node.
        :rtype: str

        '''
        return "{0}[name='{1}']".format(
            self.coloured_name(colour), self.routine.name)

    def __str__(self):
        return self.node_str(False)
