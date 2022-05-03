# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2020-2021, Science and Technology Facilities Council
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
# Modified: R. W. Ford, S. Siso and N. Nobre, STFC Daresbury Lab

''' Module containing the definition of the Range node. '''

from psyclone.psyir.nodes.node import Node
from psyclone.psyir.nodes.literal import Literal
from psyclone.psyir.nodes.datanode import DataNode
from psyclone.psyir.symbols import ScalarType, INTEGER_TYPE


class Range(Node):
    '''The ``Range`` node is used to capture a range of integers via
    ``start``, ``stop`` and ``step`` expressions. For example,
    ``start=2``, ``stop=6`` and ``step=2`` indicates the values ``2``,
    ``4`` and ``6``.

    At the moment the only valid use of ``Range`` in the PSyIR is to
    describe a set of accesses to an Array dimension (so-called array
    notation in Fortran). Therefore, the parent of a ``Range`` node should
    only be an ``Array`` node.

    The ``Range`` node has three children nodes, the first child captures
    the start of the range, the second child captures the end of the range
    and the third captures the step within the range.

    The nodes for each of the children must return an integer. Potentially
    valid nodes are therefore ``Literal``, ``Reference``, ``Operation``
    and ``CodeBlock``.

    A common use case is to want to specify all the elements of a given
    array dimension without knowing the extent of that dimension. In the
    PSyIR this is achieved by using the ``LBOUND``, and ``UBOUND`` binary
    operators::

      >>> one = Literal("1", INTEGER_TYPE)
      >>> # Declare a 1D real array called 'a' with 10 elements
      >>> symbol = DataSymbol("a", ArrayType(REAL_TYPE, [10]))
      >>> # Return the lower bound of the first dimension of array 'a'
      >>> lbound = BinaryOperation.create(
              BinaryOperation.Operator.LBOUND,
              Reference(symbol), one)
      >>> # Return the upper bound of the first dimension of array 'a'
      >>> ubound = BinaryOperation.create(
              BinaryOperation.Operator.UBOUND,
              Reference(symbol), one)
      >>> # Step defaults to 1 so no need to include it when creating range
      >>> my_range = Range.create(lbound, ubound)
      >>> # Create an access to all elements in the 1st dimension of array 'a'
      >>> array_access = Array.create(symbol, [my_range])

    In Fortran the above access ``array_access`` can be represented by
    ``a(:)``. The Fortran front-ends and back-ends are aware of array
    notation. Therefore the Fortran frontend is able to convert array
    notation to PSyIR and the Fortran backend is able to convert PSyIR
    back to array notation.
    '''
    # Textual description of the node.
    _children_valid_format = "DataNode, DataNode, DataNode"
    _text_name = "Range"
    _colour = "white"

    @staticmethod
    def _validate_child(position, child):
        '''
        :param int position: the position to be validated.
        :param child: a child to be validated.
        :type child: :py:class:`psyclone.psyir.nodes.Node`

        :return: whether the given child and position are valid for this node.
        :rtype: bool

        '''
        return position < 3 and isinstance(child, DataNode)

    @staticmethod
    def create(start, stop, step=None):
        '''
        Create an internally-consistent Range object. If no step
        is provided then it defaults to an integer Literal with value 1.

        :param start: the PSyIR for the start value.
        :type start: :py:class:`psyclone.psyir.nodes.Node`
        :param stop: the PSyIR for the stop value.
        :type stop: :py:class:`psyclone.psyir.nodes.Node`
        :param step: the PSyIR for the increment/step or None.
        :type step: :py:class:`psyclone.psyir.nodes.Node` or NoneType
        :param parent: the parent node of this Range in the PSyIR.
        :type parent: :py:class:`psyclone.psyir.nodes.Node` or NoneType

        :returns: a fully-populated Range object.
        :rtype: :py:class:`psyclone.psyir.nodes.ranges.Range`

        '''
        erange = Range()
        erange.start = start
        erange.stop = stop
        if step:
            erange.step = step
        else:
            # No step supplied so default to a value of 1
            erange.step = Literal("1", INTEGER_TYPE)
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
                f"The {name} value of a Range must be a sub-class of "
                f"Node but got: {type(value).__name__}")
        if (isinstance(value, Literal) and
                value.datatype.intrinsic != ScalarType.Intrinsic.INTEGER):
            raise TypeError(
                f"If the {name} value of a Range is a Literal then it "
                f"must be of type INTEGER but got {value.datatype}")

    def _check_completeness(self):
        ''' Perform internal consistency checks for this Range.

        :raises InternalError: if there is not exactly three children.
        :raises InternalError: if any of the children are not sub-classes \
                               of Node.
        '''
        from psyclone.psyGen import InternalError

        if len(self._children) != 3:
            raise InternalError(
                f"Malformed Range: should have three children but "
                f"found {len(self._children)}: {self._children}")

    @property
    def start(self):
        '''
        Checks that this Range is valid and then returns the PSyIR
        for the starting value of the range.

        :returns: the starting value of this range.
        :rtype: :py:class:`psyclone.psyir.nodes.Node`
        '''
        self._check_completeness()
        return self._children[0]

    @start.setter
    def start(self, value):
        '''
        Sets the start value/expression of this explicit range.

        :param value: the PSyIR node representing the starting value.
        :type value: :py:class:`psyclone.psyir.nodes.Node`

        '''
        self._check_valid_input(value, "start")
        if not self.children:
            self.children.append(value)
        else:
            self.children[0] = value

    @property
    def stop(self):
        '''
        Checks that this Range is valid and then returns the end
        value/expression.

        :returns: the end value of this range.
        :rtype: :py:class:`psyclone.psyir.nodes.Node`
        '''
        self._check_completeness()
        return self.children[1]

    @stop.setter
    def stop(self, value):
        ''' Set the stop value/expression of this Range.

        :param value: the PSyIR node representing the stop value.
        :type value: :py:class:`psyclone.psyir.nodes.Node`
        '''
        self._check_valid_input(value, "stop")
        if not self.children:
            raise IndexError(
                f"The Stop value '{value}' can not be inserted into range "
                f"'{self}' before the Start value is provided.")
        if len(self.children) == 1:
            self.children.append(value)
        else:
            self.children[1] = value

    @property
    def step(self):
        '''
        Checks that this Range is valid and then returns the step
        (increment) value/expression.

        :returns: the increment used in this range.
        :rtype: :py:class:`psyclone.psyir.nodes.Node`
        '''
        self._check_completeness()
        return self.children[2]

    @step.setter
    def step(self, value):
        ''' Set the step value/expression of this Range.

        :param value: the PSyIR node representing the step value.
        :type value: :py:class:`psyclone.psyir.nodes.Node`
        '''
        self._check_valid_input(value, "step")
        if len(self.children) < 2:
            raise IndexError(
                f"The Step value '{value}' can not be inserted into range "
                f"'{self}' before the Start and Stop values are provided.")
        if len(self.children) == 2:
            self.children.append(value)
        else:
            self.children[2] = value

    def __str__(self):
        return self.node_str(colour=False)
