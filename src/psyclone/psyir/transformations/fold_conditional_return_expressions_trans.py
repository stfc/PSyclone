# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2021, Science and Technology Facilities Council.
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
# Author S. Siso, STFC Daresbury Lab

'''This module contains the FoldConditionalReturnExpressionsTrans. '''

from psyclone.psyir.nodes import (Routine, IfBlock, Return,
    UnaryOperation)

class FoldConditionalReturnExpressionsTrans(Transformation):
    ''' Provides a ... '''

    def __str__(self):
        return "description"

    @property
    def name(self):
        '''Returns the name of this transformation as a string.'''
        return "FoldConditionalReturnExpressionsTrans"

    def validate(self, routine):
        '''Checks ...

        :raises TransformationError: if ....
         '''

        if not isinstance(routine, Routine):
            raise TransformationError("Not Routine")

    def apply(self, routine):
        self.validate(routine)

        def is_conditional_return(node):
            if not isinstance(node, IfBlock):
                return False
            if len(node.if_body.children) != 1:
                return False
            if node.else_body is not None:
                return False
            return isinstance(node.if_body[0], Return)

        remaining_statements = [line for line in routine]
        for statement in remaining_statements:
            if is_conditional_return(statement):
                # Reverse condition adding a NOT operator
                new_condition = UnaryOperation.create(
                    UnaryOperation.Operator.NOT,
                    statement.condition)
                statement.children[0] = new_condition
                new_condition._parent = statement

                # Remove return and add remaining of routine inside the
                # loop body
                del statement.if_body.children[0]
                start = statement.position
                end = len(statement.parent.children) - 1
                for index in range(end, start, -1):
                    #import pdb; pdb.set_trace()
                    move = statement.parent.children[index]
                    del statement.parent.children[index]
                    statement.if_body.children.insert(0, move)
                    move._parent = statement.if_body
