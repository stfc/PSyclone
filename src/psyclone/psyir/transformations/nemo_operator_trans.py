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
# Author: R. W. Ford, STFC Daresbury Lab

'''Module providing an abstract class which provides some generic
functionality required by transformations of PSyIR intrinsic operators
(such as MIN and MAX).

'''

import abc
import six
from psyclone.psyGen import Transformation, Assignment
from psyclone.psyir.transformations.transformation_error import TransformationError
from psyclone.psyir.symbols import SymbolTable


@six.add_metaclass(abc.ABCMeta)
class NemoOperatorTrans(Transformation):
    '''Provides NEMO-API-specific support for transformations from PSyIR
    intrinsic Operator nodes to equivalent PSyIR code in a PSyIR
    tree. Such transformations can be useful when the intrinsic is not
    supported by a particular backend or if it is more efficient to
    have explicit code.

    '''
    def __init__(self):
        super(NemoOperatorTrans, self).__init__()
        self._operator_name = None
        self._classes = None
        self._operators = None

    def __str__(self):
        return ("Convert the PSyIR {0} intrinsic to equivalent PSyIR "
                "code.".format(self._operator_name.upper()))

    @property
    def name(self):
        '''
        :returns: the name of the parent transformation as a string.
        :rtype:str

        '''
        return "Nemo{0}Trans".format(self._operator_name.title())

    def validate(self, node, symbol_table, options=None):
        '''Perform various checks to ensure that it is valid to apply
        an intrinsic transformation to the supplied Node.

        :param node: the node that is being checked.
        :type node: :py:class:`psyclone.psyGen.Operation`
        :param symbol_table: the symbol table that is being checked.
        :type symbol_table: :py:class:`psyclone.psyir.symbols.SymbolTable`
        :param options: a dictionary with options for transformations.
        :type options: dictionary of string:values or None

        :raises TransformationError: if the node argument is not the \
            expected type.
        :raises TransformationError: if the symbol_table argument is not a \
            :py:class:`psyclone.psyir.symbols.SymbolTable`.
        :raises TransformationError: if the API is not nemo.
        :raises TransformationError: if the Operation node does \
            not have an Assignment Node as an ancestor.

        '''
        # Check that the node is one of the expected types.
        if not isinstance(node, self._classes):
            raise TransformationError(
                "Error in {0} transformation. The supplied node argument is "
                "not a {1} operator, found '{2}'."
                "".format(self.name, self._operator_name,
                          type(node).__name__))
        if node.operator not in self._operators:
            raise TransformationError(
                "Error in {0} transformation. The supplied node operator is "
                "invalid, found '{1}'."
                "".format(self.name, str(node.operator)))
        # Check that symbol_table is a PSyIR symbol table
        if not isinstance(symbol_table, SymbolTable):
            raise TransformationError(
                "Error in {0} transformation. The supplied symbol_table "
                "argument is not an a SymbolTable, found '{1}'."
                "".format(self.name, type(symbol_table).__name__))
        # Check that this is the nemo API.
        from psyclone.configuration import Config
        if not Config.get().api == "nemo":
            raise TransformationError(
                "Error in {0} transformation. This transformation only "
                "works for the nemo API, but found '{1}'."
                "".format(self.name, Config.get().api))
        # Check that there is an Assignment node that is an ancestor
        # of this Operation.
        if not node.ancestor(Assignment):
            raise TransformationError(
                "Error in {0} transformation. This transformation requires "
                "the operator to be part of an assignment statement, "
                "but no such assignment was found.".format(self.name))

    @abc.abstractmethod
    def apply(self, node, options=None):
        '''Abstract method, see psyclone.psyGen.Transformations apply() for
        more details.'''
        # pylint: disable=no-self-use
        schedule = None
        memento = None
        return schedule, memento
