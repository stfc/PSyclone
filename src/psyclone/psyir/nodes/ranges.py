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
# Author: A. R. Porter, STFC Daresbury Lab

''' Module containing the definition of the Range node. '''

from psyclone.psyir.nodes import Node, Literal
from psyclone.psyir.symbols import DataType, ScalarType, INTEGER_SINGLE_TYPE


class Range(Node):
    '''Represents an integer, index range with an increment. As such it
    has three children representing the start, stop and step
    expressions.

    :param parse_node: the entry in the fparser2 parse tree representing the \
                       code contained within this directive or None.
    :type parse_node: :py:class:`fparser.two.Fortran2003.Base` or NoneType
    :param parent: PSyIR node that is the parent of this Range or None.
    :type parent: :py:class:`psyclone.psyGen.Node` or NoneType
    :param annotations: Tags that provide additional information about \
        the node. The node should still be functionally correct when \
        ignoring these tags.
    :type annotations: list of str

    '''
    def __init__(self, parse_node=None, parent=None, annotations=None):

        super(Range, self).__init__(parse_node, parent=parent,
                                    annotations=annotations)
        # Initialise the list of children so that the start/stop/step setters
        # can be called in any order
        self._children = [None, None, None]

    @staticmethod
    def create(start, stop, step=None, parent=None):
        '''
        Create an internally-consistent Range object. If no step
        is provided then it defaults to an integer Literal with value 1.

        :param start: the PSyIR for the start value.
        :type start: :py:class:`psyclone.psyGen.Node`
        :param stop: the PSyIR for the stop value.
        :type stop: :py:class:`psyclone.psyGen.Node`
        :param step: the PSyIR for the increment/step or None.
        :type step: :py:class:`psyclone.psyGen.Node` or NoneType
        :param parent: the parent node of this Range in the PSyIR.
        :type parent: :py:class:`psyclone.psyGen.Node` or NoneType

        :returns: a fully-populated Range object.
        :rtype: :py:class:`psyclone.psyir.nodes.ranges.Range`

        '''
        erange = Range(parent=parent)
        erange.start = start
        start.parent = erange
        erange.stop = stop
        stop.parent = erange
        if step:
            erange.step = step
            step.parent = erange
        else:
            # No step supplied so default to a value of 1
            erange.step = Literal("1", INTEGER_SINGLE_TYPE, parent=erange)
        return erange

    @staticmethod
    def _check_valid_input(value, name):
        '''
        Perform checks that the supplied value is valid as a child of a
        Range node.

        :param object value: entity to check.
        :param str name: the name of the quantity for which this value has \
                         been supplied.

        :raises TypeError: if the supplied value is not a sub-class of Node.
        :raises TypeError: if the supplied value is a Literal but is not of \
                           INTEGER type.
        '''
        if not isinstance(value, Node):
            raise TypeError(
                "The {0} value of a Range must be a sub-class of "
                "Node but got: {1}".format(name, type(value).__name__))
        if (isinstance(value, Literal) and
                value.datatype.name != ScalarType.Name.INTEGER):
            raise TypeError(
                "If the {0} value of a Range is a Literal then it "
                "must be of type INTEGER but got {1}".format(
                    name, value.datatype))

    def _check_completeness(self):
        ''' Perform internal consistency checks for this Range.

        :raises InternalError: if there is not exactly three children.
        :raises InternalError: if any of the children are not sub-classes \
                               of Node.
        '''
        from psyclone.psyGen import InternalError

        if len(self._children) != 3:
            raise InternalError(
                "Malformed Range: should have three children but "
                "found {0}: {1}".format(len(self._children), self._children))

        if any(not isinstance(child, Node) for child in self._children):
            raise InternalError(
                "Malformed Range: all children must be sub-classes of "
                "Node but found: {0}".format(
                    [type(child).__name__ for child in self._children]))

    @property
    def start(self):
        '''
        Checks that this Range is valid and then returns the PSyIR
        for the starting value of the range.

        :returns: the starting value of this range.
        :rtype: :py:class:`psyclone.psyGen.Node`
        '''
        self._check_completeness()
        return self._children[0]

    @start.setter
    def start(self, value):
        '''
        Sets the start value/expression of this explicit range.

        :param value: the PSyIR node representing the starting value.
        :type value: :py:class:`psyclone.psyGen.Node`

        '''
        self._check_valid_input(value, "start")
        self._children[0] = value

    @property
    def stop(self):
        '''
        Checks that this Range is valid and then returns the end
        value/expression.

        :returns: the end value of this range.
        :rtype: :py:class:`psyclone.psyGen.Node`
        '''
        self._check_completeness()
        return self._children[1]

    @stop.setter
    def stop(self, value):
        ''' Set the stop value/expression of this Range.

        :param value: the PSyIR node representing the stop value.
        :type value: :py:class:`psyclone.psyGen.Node`
        '''
        self._check_valid_input(value, "stop")
        self._children[1] = value

    @property
    def step(self):
        '''
        Checks that this Range is valid and then returns the step
        (increment) value/expression.

        :returns: the increment used in this range.
        :rtype: :py:class:`psyclone.psyGen.Node`
        '''
        self._check_completeness()
        return self._children[2]

    @step.setter
    def step(self, value):
        ''' Set the step value/expression of this Range.

        :param value: the PSyIR node representing the step value.
        :type value: :py:class:`psyclone.psyGen.Node`
        '''
        self._check_valid_input(value, "step")
        self._children[2] = value

    def node_str(self, colour=True):
        ''' Checks that this Range is valid and then returns the name
        of this node with (optional) control codes to generate
        coloured output in a terminal that supports it.

        :param bool colour: whether or not to include colour control codes.

        :returns: description of this node, possibly coloured.
        :rtype: str
        '''
        self._check_completeness()
        return super(Range, self).node_str(colour)

    def __str__(self):
        return self.node_str(colour=False)
