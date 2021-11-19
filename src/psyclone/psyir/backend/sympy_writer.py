# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2021, Science and Technology Facilities Council
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
# Author: J. Henrichs, Bureau of Meteorology


'''PSyIR backend to create expressions that are handled by sympy.
'''

# pylint: disable=too-many-lines
from __future__ import absolute_import

from psyclone.errors import InternalError
from psyclone.psyir.backend.fortran import FortranWriter
from psyclone.psyir.nodes import NaryOperation, BinaryOperation
from psyclone.psyir.symbols import ScalarType


class SymPyWriter(FortranWriter):
    '''Implements a PSyIR-to-sympy writer, used to create a representation
    of the PSyIR tree that can be understood by SymPy. Most Fortran
    expressions work as expected, this class implements special handling
    for constants (which can have a precision attached, e.g. 2_4) and some
    intrinsic functions (e.g. MAX, which SymPy expects to be Max).

    '''

    def __init__(self):
        super(SymPyWriter, self).__init__()
        self._intrinsic = set()
        self._op_to_str = {}
        for operator, op_str in [(NaryOperation.Operator.MAX, "Max"),
                                 (BinaryOperation.Operator.MAX, "Max"),
                                 (NaryOperation.Operator.MIN, "Min"),
                                 (BinaryOperation.Operator.MIN, "Min"),
                                 (BinaryOperation.Operator.REM, "Mod"),
                                 ]:
            self._intrinsic.add(op_str)
            self._op_to_str[operator] = op_str

    def literal_node(self, node):
        '''This method is called when a Literal instance is found in the PSyIR
        tree. For SymPy we need to handle booleans (which are expected to
        be capitalised: True). Real values work by just ignoring any precision
        information (e.g. 2_4, 3.1_wp). Character constants are supported
        and will raise an exception.

        :param node: a Literal PSyIR node.
        :type node: :py:class:`psyclone.psyir.nodes.Literal`

        :returns: the SymPy representation for the literal.
        :rtype: str

        :raises InternalError: if a character constant is found, which \
            is not supported with SymPy.

        '''
        if node.datatype.intrinsic == ScalarType.Intrinsic.BOOLEAN:
            # Booleans need to be converted to SymPy format
            return node.value.capitalize()

        if node.datatype.intrinsic == ScalarType.Intrinsic.CHARACTER:
            raise InternalError("SymPy cannot handle strings like '{0}'."
                                .format(node.value))
        # All real (single, double precision) and integer work by just
        # using the node value. Single and double precision both use
        # 'e' as specification, which SymPy accepts, and precision
        # information can be ignored.
        return node.value

    def get_operator(self, operator):
        '''Determine the Fortran operator that is equivalent to the provided
        PSyIR operator. This is achieved by reversing the Fparser2Reader
        maps that are used to convert from Fortran operator names to PSyIR
        operator names.

        :param operator: a PSyIR operator.
        :type operator: :py:class:`psyclone.psyir.nodes.Operation.Operator`

        :returns: the Fortran operator.
        :rtype: str

        :raises KeyError: if the supplied operator is not known.

        '''

        try:
            return self._op_to_str[operator]
        except KeyError:
            return super(SymPyWriter, self).get_operator(operator)

    def is_intrinsic(self, operator):
        '''Determine whether the supplied operator is an intrinsic
        function (i.e. needs to be used as `f(a,b)`) or not (i.e. used
        as `a + b`). This tests for known SymPy names of these functions
        (e.g. Max), and otherwise calls the function in the base class.

        :param str operator: the supplied operator.

        :returns: true if the supplied operator is an \
            intrinsic and false otherwise.

        '''
        if operator in self._intrinsic:
            return True

        return super(SymPyWriter, self).is_intrinsic(operator)
