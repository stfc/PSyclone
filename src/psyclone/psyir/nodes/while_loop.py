# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2023-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

''' This module contains the WhileLoop node implementation.'''

from psyclone.core import VariablesAccessMap
from psyclone.errors import InternalError, GenerationError
from psyclone.psyir.nodes.datanode import DataNode
from psyclone.psyir.nodes.node import Node
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

    def reference_accesses(self) -> VariablesAccessMap:
        '''
        :returns: a map of all the symbol accessed inside this node, the
            keys are Signatures (unique identifiers to a symbol and its
            structure accessors) and the values are AccessSequence
            (a sequence of AccessTypes).

        '''
        # The first child is the loop condition - all variables are read-only
        var_accesses = self.condition.reference_accesses()
        var_accesses.update(self.loop_body.reference_accesses())
        return var_accesses

    def next_accesses(self) -> list[Node]:
        '''
        :returns: the combined next_accesses for the children of this
            WhileLoop
        '''
        next_accesses = []
        var_accesses = self.condition.next_accesses()
        self._merge_accesses(next_accesses, var_accesses)
        for child in self.loop_body:
            self._merge_accesses(next_accesses, child.next_accesses())
        return next_accesses

    def previous_accesses(self) -> list[Node]:
        '''
        Abstract method for finding the previous_accesses of a statement.
        Subclasses should override this according to their own structure to
        return previous accesses to any References contained in the statement.
        '''
        prev_accesses = []
        var_accesses = self.condition.previous_accesses()
        self._merge_accesses(prev_accesses, var_accesses)
        for child in self.loop_body:
            self._merge_accesses(prev_accesses, child.previous_accesses())
        return prev_accesses
