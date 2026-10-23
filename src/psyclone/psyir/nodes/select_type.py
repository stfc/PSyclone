# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

''' This module contains the SelectType node implementation.'''

from psyclone.core import VariablesAccessMap
from psyclone.errors import GenerationError
from psyclone.psyir.nodes.node import Node
from psyclone.psyir.nodes.type_guard import TypeGuard
from psyclone.psyir.nodes.reference import Reference
from psyclone.psyir.nodes.statement import Statement


class SelectType(Statement):
    '''
    Node representing an SelectType block within the PSyIR. It may have any
    number of TypeGuard children.

    '''

    # Textual description of the node.
    _children_valid_format = "Reference, TypeGuard+"
    _text_name = "SelectType"
    _colour = "red"

    @staticmethod
    def _validate_child(position: int, child: Node) -> bool:
        '''
        :param position: the position to be validated.
        :param child: a child to be validated.

        :return: whether the given child and position are valid for this node.

        '''
        return ((position == 0 and isinstance(child, Reference)) or
                (position > 0 and isinstance(child, TypeGuard)))

    @staticmethod
    def create(variable: Reference, istypes: list[TypeGuard]) -> "SelectType":
        '''
        Creates a SelectType from its TypeGuard children.

        :param istypes: a list containing the TypeGuard children of the new
                        SelectType.

        :raises GenerationError: if istypes it not a list.

        :returns: a new SelectType containing the provided TypeGuard children.
        '''
        if not isinstance(istypes, list):
            raise GenerationError(
                f"istypes argument in create method of SelectType class "
                f"should be a list but found {type(istypes).__name__}."
            )

        select_type = SelectType()
        select_type.children = [variable] + istypes
        return select_type

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


__all__ = ["SelectType"]
