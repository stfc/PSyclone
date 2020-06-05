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
from psyclone.psyir.nodes import Statement, Literal, Reference


class Call(Statement):
    '''Node representing a Call.

    :param str name: the name of the call.

    list of Reference arguments
    list of Reference returns

    :param calls: specifies the container containing the code that \
        this node calls or None if it has not been linked. If the \
        container is not available then the container name is stored.
    :type calls: :py:class:`psyclone.psyir.nodes.Container`, str or \
        NoneType

    Node parent

    '''
    # Textual description of the node.
    _children_valid_format = "<LeafNode>"
    _text_name = "Call"
    _colour_key = "Call"

    def __init__(self, name, arguments, returns=[], calls=None, parent=None):
        super(Call, self).__init__(parent=parent)

        if not isinstance(name, str):
            raise TypeError(
                "Call name argument should be a string but found '{0}'."
                .format(type(name).__name__))
        if not name:
            raise TypeError("Call name argument must not be empty.")
        if not isinstance(arguments, list):
            raise TypeError(
                "Call arguments argument should be a list but found '{0}'."
                "".format(type(arguments).__name__))
        if not all([isinstance(arg, Reference) for arg in arguments]):
            raise TypeError(
                "Call arguments argument list entries should all be "
                "references but at least one is not.")
        if not isinstance(returns, list):
            raise TypeError(
                "Call returns argument should be a list but found '{0}'."
                "".format(type(returns).__name__))
        if not all([isinstance(arg, Reference) for arg in returns]):
            raise TypeError(
                "Call returns argument list entries should all be "
                "references but at least one is not.")
        # TBD calls arg checking

        self._name = name
        self._arguments = arguments[:]
        self._returns = returns[:]
        self._calls = calls

    # TBD properties to access content
    @property
    def name(self):
        ''' xxx '''
        return self._name

    @property
    def arguments(self):
        ''' xxx '''
        return self._arguments

    @property
    def returns(self):
        ''' xxx '''
        return self._returns

    @property
    def calls(self):
        ''' xxx '''
        return self._calls

    def node_str(self, colour=True):
        '''
        Construct a text representation of this node, optionally containing
        colour control codes.

        :param bool colour: whether or not to include colour control codes.

        :returns: description of this PSyIR node.
        :rtype: str
        '''
        return "{0}[name='{1}', args={2}, returns={3}, calls={4}]".format(
            self.coloured_name(colour), self._name,
            [argument.name for argument in self.arguments],
            [returns.name for returns in self._returns],
            type(self.calls).__name__)
