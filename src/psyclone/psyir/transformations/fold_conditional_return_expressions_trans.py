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
# Authors: S. Siso and N. Nobre, STFC Daresbury Lab

'''This module contains the FoldConditionalReturnExpressionsTrans. '''

from psyclone.psyir.nodes import (Routine, IfBlock, Return,
                                  UnaryOperation)
from psyclone.psyGen import Transformation
from psyclone.psyir.transformations.transformation_error import \
        TransformationError


class FoldConditionalReturnExpressionsTrans(Transformation):
    ''' Provides a transformation that folds conditional expressions with only
    a return statement inside so that the Return statement is moved to the end
    of the Routine and therefore it can be safely removed. This simplifies the
    control flow of the kernel to facilitate other transformations like kernel
    fusions. For example, the following code:

    .. code-block:: fortran

        subroutine test(i)
            if (i < 5) then
                return
            endif
            if (i > 10) then
                return
            endif
            ! CODE
        end subroutine

    will be transformed to:

    .. code-block:: fortran

        subroutine test(i)
            if (.not.(i < 5)) then
                if (.not.(i > 10)) then
                    ! CODE
                endif
            endif
        end subroutine

    '''

    def __str__(self):
        return ("Re-structure kernel statements to eliminate conditional "
                "Return expressions.")

    @property
    def name(self):
        '''Returns the name of this transformation as a string.'''
        return "FoldConditionalReturnExpressionsTrans"

    def validate(self, node, options=None):
        '''Ensure that it is valid to apply this transformation to the
        supplied node.

        :param node: the node to validate.
        :type node: :py:class:`psyclone.psyir.nodes.Routine`
        :param options: a dictionary with options for transformations.
        :type options: dict of string:values or None

        :raises TransformationError: if the node is not a Routine.

        '''
        if not isinstance(node, Routine):
            raise TransformationError(
                f"Error in {self.name} transformation. This transformation "
                f"can only be applied to 'Routine' nodes, but found "
                f"'{type(node).__name__}'.")

    def apply(self, node, options=None):
        '''Apply this transformation to the supplied node.

        :param node: the node to transform.
        :type node: :py:class:`psyclone.psyir.nodes.Routine`
        :param options: a dictionary with options for transformations.
        :type options: dict of string:values or None

        '''
        routine = node
        self.validate(routine, options)

        def is_conditional_return(node):
            '''
            :param node: node to evaluate.
            :type node: :py:class:`psyclone.psyir.nodes.Node`

            :returns: whether the given node represents a conditional return \
                      expression.
            '''
            if not isinstance(node, IfBlock):
                return False
            if node.else_body is not None:
                return False
            return isinstance(node.if_body[0], Return)

        for statement in routine[:]:
            if is_conditional_return(statement):
                # Reverse condition adding a NOT operator
                new_condition = UnaryOperation.create(
                    UnaryOperation.Operator.NOT,
                    statement.condition.copy())
                statement.children[0] = new_condition

                # Remove return statement (and any dead code inside the loop)
                statement.if_body.children = []
                # Then move any remaining statement after the conditional
                # statement inside the loop body
                while len(statement.parent.children) > statement.position + 1:
                    move = statement.parent.children.pop()
                    statement.if_body.children.insert(0, move)


# For Sphinx AutoAPI documentation generation
__all__ = ['FoldConditionalReturnExpressionsTrans']
