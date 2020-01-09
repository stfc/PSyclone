# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2020, Science and Technology Facilities Council
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

''' Module containing definitions of the various Range nodes. '''

from psyclone.psyGen import Node, Literal, InternalError
from psyclone.psyir.symbols import DataType


class Range(Node):
    '''
    Represents an index range.

    '''


class EntireRange(Range):
    '''
    Represents an index range that encompasses the entire extent of a
    particular array dimension.

    '''


class ExplicitRange(Range):
    '''
    Represents an explicit index range with an (optional) increment. As such
    it must have at least two children representing the start and stop
    expressions. If no third child is provided then the increment defaults to
    an integer Literal with value 1.

    :param ast: the entry in the fparser2 parse tree representing the code \
                contained within this directive or None.
    :type ast: :py:class:`fparser.two.Fortran2003.Base` or NoneType
    :param parent: PSyIR node that is the parent of this Range or None.
    :type parent: :py:class:`psyclone.psyGen.Node` or NoneType

    '''
    def __init__(self, ast=None, parent=None, annotations=None):


        super(ExplicitRange, self).__init__(ast, parent=parent,
                                            annotations=annotations)

        if len(children) == 2:
            # No step was provided so we default to a value of 1
            self._children.append(Literal("1", DataType.INTEGER, parent=self))

    @staticmethod
    def create(start, stop, step=None, parent=None):
        '''
        TODO
        '''
        erange = ExplicitRange(parent=parent)
        erange.start = start
        start.parent = erange
        erange.stop = stop
        stop.parent = erange
        if step:
            erange.step = step
            step.parent = erange
        else:
            erange.step = Literal("1", DataType.INTEGER, parent=self)

    @property
    def start(self):
        '''
        :returns: the starting value of this range.
        :rtype: :py:class:`psyclone.psyGen.Node`
        '''
        return self._children[0]

    @start.setter
    def start(self, value):
        '''
        Sets the starting value of this explicit range.

        :param value:
        :type value:
        '''
        if not isinstance(value, Node):
            raise TypeError(
                "The start value of an ExplicitRange must be a sub-class of "
                "Node but got: {0}".format(type(value).__name__))
        # TODO check that Literal is integer if value is a Literal
        self._children[0] = value
        
    @property
    def stop(self):
        '''
        :returns: the end value of this range.
        :rtype: :py:class:`psyclone.psyGen.Node`
        '''
        return self._children[1]

    @property
    def step(self):
        '''
        :returns: the increment used in this range.
        :rtype: :py:class:`psyclone.psyGen.Node`
        '''
        return self._children[2]

    def node_str(self, colour=True):
        '''
        Returns the name of this node with (optional) control codes
        to generate coloured output in a terminal that supports it.

        :param bool colour: whether or not to include colour control codes.

        :returns: description of this node, possibly coloured.
        :rtype: str
        '''
        return ("{0}[start='{1}', stop='{2}', step='{3}']".
                format(colored("ExplicitRange", SCHEDULE_COLOUR_MAP["Range"]),
                       self.start, self.stop, self.step))
