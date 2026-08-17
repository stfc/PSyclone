# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

''' This module contains the IfBlock node implementation.'''

from psyclone.core import VariablesAccessMap
from psyclone.errors import InternalError, GenerationError
from psyclone.psyir.nodes.datanode import DataNode
from psyclone.psyir.nodes.node import Node
from psyclone.psyir.nodes.schedule import Schedule
from psyclone.psyir.nodes.statement import Statement


class IfBlock(Statement):
    '''
    Node representing an if-block within the PSyIR. It has two mandatory
    children: the first one represents the if-condition and the second one
    the if-body; and an optional third child representing the else-body.

    '''
    # The valid annotations for this If node:
    # 'was_elseif' to tag nested ifs originally written with the 'else if'
    # language syntactic construct;
    # 'was_single_stmt' to tag ifs with a 1-statement body which were
    # originally written in a single line;
    # 'was_case' to tag a conditional structure which was originally written
    # with the Fortran 'case' or C 'switch' syntactic constructs;
    # 'was_where' - a conditional structure originally implied by a Fortran
    # WHERE construct.
    # 'was_type_is' to tag a conditional structure originally
    # written with the Fortran 'select type' construct with a 'type
    # is' clause.
    # 'was_class_is' to tag a conditional structure originally
    # written with the Fortran 'select type' construct with a 'class
    # is' clause.
    valid_annotations = ('was_elseif', 'was_single_stmt', 'was_case',
                         'was_where', 'was_type_is', 'was_class_is')
    # Textual description of the node.
    _children_valid_format = "DataNode, Schedule [, Schedule]"
    _text_name = "If"
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
            position in (1, 2) and isinstance(child, Schedule))

    @property
    def condition(self):
        ''' Return the PSyIR Node representing the conditional expression
        of this IfBlock.

        :returns: IfBlock conditional expression.
        :rtype: :py:class:`psyclone.psyir.nodes.Node`
        :raises InternalError: If the IfBlock node does not have the correct \
            number of children.
        '''
        if len(self.children) < 2:
            raise InternalError(
                f"IfBlock malformed or incomplete. It should have at least 2 "
                f"children, but found {len(self.children)}.")
        return self._children[0]

    @property
    def if_body(self):
        ''' Return the Schedule executed when the IfBlock evaluates to True.

        :returns: Schedule to be executed when IfBlock evaluates to True.
        :rtype: :py:class:`psyclone.psyir.nodes.Schedule`
        :raises InternalError: If the IfBlock node does not have the correct \
            number of children.
        '''
        if len(self.children) < 2:
            raise InternalError(
                f"IfBlock malformed or incomplete. It should have at least 2 "
                f"children, but found {len(self.children)}.")

        return self._children[1]

    @property
    def else_body(self):
        ''' If available return the Schedule executed when the IfBlock
        evaluates to False, otherwise return None.

        :returns: Schedule to be executed when IfBlock evaluates \
            to False, if it doesn't exist returns None.
        :rtype: :py:class:`psyclone.psyir.nodes.Schedule` or NoneType
        '''
        if len(self._children) == 3:
            return self._children[2]
        return None

    @staticmethod
    def create(if_condition, if_body, else_body=None):
        '''Create an IfBlock instance given valid instances of an
        if_condition, an if_body and an optional else_body.

        :param if_condition: the PSyIR node containing the if \
            condition of the if block.
        :type if_condition: :py:class:`psyclone.psyir.nodes.Node`
        :param if_body: the PSyIR nodes representing the if body of \
            the if block.
        :type if_body: List[:py:class:`psyclone.psyir.nodes.Node`]
        :param else_body: PSyIR nodes representing the else body of the \
            if block or None if there is no else body (defaults to None).
        :type else_body: Optional[List[:py:class:`psyclone.psyir.nodes.Node`]]

        :returns: an IfBlock instance.
        :rtype: :py:class:`psyclone.psyir.nodes.IfBlock`

        :raises GenerationError: if the arguments to the create method \
            are not of the expected type.

        '''
        if not isinstance(if_body, list):
            raise GenerationError(
                f"if_body argument in create method of IfBlock class should "
                f"be a list but found '{type(if_body).__name__}'.")
        if else_body is not None and not isinstance(else_body, list):
            raise GenerationError(
                f"else_body argument in create method of IfBlock class should "
                f"be a list but found '{type(else_body).__name__}'.")

        if_stmt = IfBlock()
        if_schedule = Schedule(parent=if_stmt, children=if_body)
        if else_body is not None:
            else_schedule = Schedule(parent=if_stmt, children=else_body)
            if_stmt.children = [if_condition, if_schedule, else_schedule]
        else:
            if_stmt.children = [if_condition, if_schedule]
        return if_stmt

    def __str__(self):
        name = self._text_name
        result = name + "[]\n"
        for entity in self._children:
            result += str(entity)
        result += "End " + name
        return result

    def reference_accesses(self) -> VariablesAccessMap:
        '''
        :returns: a map of all the symbol accessed inside this node, the
            keys are Signatures (unique identifiers to a symbol and its
            structure accessors) and the values are AccessSequence
            (a sequence of AccessTypes).

        '''
        var_accesses = self.condition.reference_accesses()
        var_accesses.update(self.if_body.reference_accesses())

        if self.else_body:
            var_accesses.update(self.else_body.reference_accesses())
        return var_accesses


    def next_accesses(self) -> list[Node]:
        '''
        :returns: the combined next_accesses for the children of this IfBlock.
        '''
        next_accesses = []
        new_accesses = self.condition.next_accesses()
        self._merge_accesses(next_accesses, new_accesses)
        for child in self.if_body:
            self._merge_accesses(next_accesses, child.next_accesses())
        if self.else_body:
            for child in self.else_body:
                self._merge_accesses(next_accesses, child.next_accesses())

        # FIXME Should we sort the output in some way?
        return next_accesses

    def previous_accesses(self) -> list[Node]:
        '''
        :returns: the combined previous_accesses for the children of this
            IfBlock.
        '''
        prev_accesses = []
        new_accesses = self.condition.previous_accesses()
        self._merge_accesses(prev_accesses, new_accesses)
        for child in self.if_body:
            self._merge_accesses(prev_accesses, child.previous_accesses())
        if self.else_body:
            for child in self.else_body:
                self._merge_accesses(prev_accesses, child.previous_accesses())

        # FIXME Should we sort the output in some way?
        return previous_accesses
