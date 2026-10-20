# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

''' This module contains the TypeGuard node implementation.'''

from psyclone.core import VariablesAccessMap
from psyclone.errors import InternalError, GenerationError
from psyclone.psyir.nodes.node import Node
from psyclone.psyir.nodes.reference import Reference
from psyclone.psyir.nodes.schedule import Schedule
from psyclone.psyir.nodes.statement import Statement


class TypeGuard(Statement):
    '''
    Node representing an TypeGuard block within the PSyIR. It has a
    Reference child followed by a Schedule child.

    '''

    # Textual description of the node.
    _children_valid_format = "Reference, Schedule"
    _text_name = "TypeGuard"
    _colour = "red"

    @staticmethod
    def _validate_child(position: int, child: Node) -> bool:
        '''
        :param int position: the position to be validated.
        :param child: a child to be validated.

        :return: whether the given child and position are valid for this node.
        :rtype: bool

        '''
        return ((position == 0 and isinstance(child, Reference)) or
                (position == 1 and isinstance(child, Schedule)))

    @property
    def condition(self) -> Reference:
        '''
        :returns: the Reference representing the condition of the TypeGuard.

        :raises InternalError: If the TypeGuard node does not have the correct
            number of children.
        '''
        if len(self.children) < 2:
            raise InternalError(
                f"TypeGuard malformed or incomplete. It should have 2 "
                f"children, but found {len(self.children)}.")
        return self._children[0]

    @property
    def body(self) -> Schedule:
        '''
        :returns: the Schedule containing the body of this TypeGuard.

        :raises InternalError: If the TypeGuard node does not have the correct
            number of children.
        '''
        if len(self.children) < 2:
            raise InternalError(
                f"TypeGuard malformed or incomplete. It should have 2 "
                f"children, but found {len(self.children)}.")
        return self._children[1]

    @staticmethod
    def create(condition: Reference, children: list[Node]) -> "TypeGuard":
        '''
        Creates an TypeGuard given a condition and its children.

        :param condition: the Reference condition of the new TypeGuard.
        :param children: a list of Nodes contained in the body of the new
                         TypeGuard.

        :raises GenerationError: if children is not a list.

        :returns: An TypeGuard representing the inputs.
        '''
        # or for me?
        # FIXME For the reviewer - should the create method check the
        # Reference is to a class type? Is that possible in PSyIR?

        if not isinstance(children, list):
            raise GenerationError(
                f"children argument in create method of TypeGuard class should "
                f"be a list but found {type(children).__name__}."
            )
        is_type = TypeGuard()
        is_body = Schedule(parent=is_type, children=children)
        is_type.children = [condition, is_body]

        return is_type

    def __str__(self) -> str:
        '''
        :returns the string representation of this node.
        '''
        name = self._text_name
        result = f"{name}[{str(self.children[0])}]\n"
        for node in self.children[1:]:
            result += str(node) + "\n"
        result += f"End {name}"
        return result

    def reference_accesses(self) -> VariablesAccessMap:
        '''
        :returns: a map of all the symbol accessed inside this node, the
            keys are Signatures (unique identifiers to a symbol and its
            structure accessors) and the values are AccessSequence
            (a sequence of AccessTypes).

        '''
        var_accesses = self.children[0].reference_accesses()
        for child in self.children[1:]:
            var_accesses.update(child.reference_accesses())

        return var_accesses


__all__ = ["TypeGuard"]
