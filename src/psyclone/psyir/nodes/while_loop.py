# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2023-2024, Science and Technology Facilities Council.
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
# Authors: N. Nobre, STFC Daresbury Lab
# -----------------------------------------------------------------------------

''' This module contains the WhileLoop node implementation.'''

from psyclone.errors import InternalError, GenerationError
from psyclone.psyir.nodes.datanode import DataNode
from psyclone.psyir.nodes.schedule import Schedule
from psyclone.psyir.nodes.statement import Statement


class WhileLoop(Statement):
    '''
    Node representing a while loop within the PSyIR. It has two mandatory
    children: the first one represents the loop condition and the second one
    the loop body.

    '''
    # The valid annotations for this node:
    # 'was_unconditional' to tag do loops with no condition in Fortran.
    valid_annotations = ('was_unconditional',)
    # Textual description of the node.
    _children_valid_format = "DataNode, Schedule"
    _colour = "red"

    @staticmethod
    def _validate_child(position, child):
        '''
        :param int position: the position to be validated.
        :param child: a child to be validated.
        :type child: :py:class:`psyclone.psyir.nodes.Node`

        :return: whether the given child and position are valid for this node.
        :rtype: bool

        '''
        return (position == 0 and isinstance(child, DataNode)) or (
            position == 1 and isinstance(child, Schedule))

    @property
    def condition(self):
        ''' Return the PSyIR Node representing the conditional expression
        of this WhileLoop.

        :returns: WhileLoop conditional expression.
        :rtype: :py:class:`psyclone.psyir.nodes.Node`

        :raises InternalError: If the WhileLoop node does not have the \
            correct number of children.
        '''
        if len(self.children) < 2:
            raise InternalError(
                f"WhileLoop malformed or incomplete. It should have "
                f"2 children, but found {len(self.children)}.")
        return self._children[0]

    @property
    def loop_body(self):
        ''' Return the Schedule executed when the WhileLoop condition is True.

        :returns: Schedule to be executed when WhileLoop condition is True.
        :rtype: :py:class:`psyclone.psyir.nodes.Schedule`

        :raises InternalError: If the WhileLoop node does not have the \
            correct number of children.
        '''
        if len(self.children) < 2:
            raise InternalError(
                f"WhileLoop malformed or incomplete. It should have "
                f"2 children, but found {len(self.children)}.")

        return self._children[1]

    @staticmethod
    def create(loop_condition, loop_body):
        '''Create a WhileLoop instance given valid instances of a
        loop_condition and a loop_body.

        :param loop_condition: the PSyIR node containing the loop \
            condition of the while loop statement.
        :type loop_condition: :py:class:`psyclone.psyir.nodes.Node`
        :param loop_body: the PSyIR nodes representing the loop body of \
            the loop statement.
        :type loop_body: List[:py:class:`psyclone.psyir.nodes.Node`]

        :returns: a WhileLoop instance.
        :rtype: :py:class:`psyclone.psyir.nodes.WhileLoop`

        :raises GenerationError: if the arguments to the create method \
            are not of the expected type.

        '''
        if not isinstance(loop_body, list):
            raise GenerationError(
                f"loop_body argument in create method of WhileLoop class "
                f"should be a list but found '{type(loop_body).__name__}'.")

        loop_stmt = WhileLoop()
        loop_schedule = Schedule(parent=loop_stmt, children=loop_body)
        loop_stmt.children = [loop_condition, loop_schedule]
        return loop_stmt

    def __str__(self):
        name = self.__class__.__name__
        result = name + "[]\n"
        for entity in self._children:
            result += str(entity) + "\n"
        result += "End " + name
        return result

    def reference_accesses(self, var_accesses):
        '''Get all variable access information. It combines the data from
        the loop condition and the loop body.

        :param var_accesses: VariablesAccessInfo instance that stores the \
            information about variable accesses.
        :type var_accesses: :py:class:`psyclone.core.VariablesAccessInfo`
        '''

        # The first child is the loop condition - all variables are read-only
        self.condition.reference_accesses(var_accesses)
        var_accesses.next_location()
        self.loop_body.reference_accesses(var_accesses)
        var_accesses.next_location()
