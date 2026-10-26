# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

''' This module contains the TypeGuard node implementation.'''

from enum import Enum
from typing import Union

from psyclone.core import VariablesAccessMap
from psyclone.errors import InternalError, GenerationError
from psyclone.psyir.nodes.node import Node
from psyclone.psyir.nodes.reference import Reference
from psyclone.psyir.nodes.schedule import Schedule
from psyclone.psyir.nodes.statement import Statement
from psyclone.psyir.symbols.datatypes import DataType


class TypeGuard(Statement):
    '''
    Node representing an TypeGuard block within the PSyIR. It has a
    Reference child followed by a Schedule child.

    :param type_guard_type: The type of Type Guard Statement this represents.
    :param comparison_type: The datatype that this TypeGuard compares to.

    '''

    # Textual description of the node.
    _children_valid_format = "Schedule"
    _text_name = "TypeGuard"
    _colour = "red"

    class TypeGuardType(Enum):
        '''Enum representing the possible types of a TypeGuard Statement.'''
        TYPE_IS = 1
        CLASS_IS = 2
        CLASS_DEFAULT = 3

    _type_guard_mapping: dict[TypeGuardType, str] = {
            TypeGuardType.TYPE_IS: "type is",
            TypeGuardType.CLASS_IS: "class is",
            TypeGuardType.CLASS_DEFAULT: "class default",
    }

    def __init__(self, type_guard_type: TypeGuardType, 
                 comparison_type: Union[DataType, None], **kwargs):
        super().__init__(**kwargs)
        # Use the setter to setup the type guard type.
        self.type_guard_type = type_guard_type
        self.comparison_type = comparison_type


    @property
    def comparison_type(self) -> Union[DataType, None]:
        '''
        :returns: the datatype that this TypeGuard compares to.
        '''
        return self._comparison_type

    @comparison_type.setter
    def comparison_type(self, comparison_type: Union[DataType, None]) -> None:
        '''
        Sets the datatype that this TypeGuard compares to

        :param comparison_type: The datatype that this TypeGuard compares to.

        :raises TypeError: if the comparison_type input is the wrong type.
        '''
        if (comparison_type is not None and
                not isinstance(comparison_type, DataType)):
            raise TypeError(f"{self.name} needs a DataType or None as the "
                            f"comparison type but {comparison_type} was "
                            f"provided.")
        self._comparison_type = comparison_type

    @property
    def type_guard_type(self) -> TypeGuardType:
        '''
        :returns: the type of this TypeGuard node.
        '''
        return self._type_guard_type

    @type_guard_type.setter
    def type_guard_type(self, type_guard_type: TypeGuardType) -> None:
        '''
        Sets the type of this TypeGuard node.

        :param type_guard_type: the type of this TypeGuard node.

        :raises TypeError: if the type_guard_type input is the wrong type.
        '''
        if not isinstance(type_guard_type, TypeGuard.TypeGuardType):
            raise TypeError(f"{self.name} needs a TypeGuardType as the "
                            f"type_guard_type but {type_guard_type} was "
                            f"provided.")
        self._type_guard_type = type_guard_type

    @staticmethod
    def _validate_child(position: int, child: Node) -> bool:
        '''
        :param int position: the position to be validated.
        :param child: a child to be validated.

        :return: whether the given child and position are valid for this node.
        :rtype: bool

        '''
        return position == 0 and isinstance(child, Schedule)

    @property
    def body(self) -> Schedule:
        '''
        :returns: the Schedule containing the body of this TypeGuard.

        :raises InternalError: If the TypeGuard node does not have the correct
            number of children.
        '''
        if len(self.children) != 1:
            raise InternalError(
                f"TypeGuard malformed or incomplete. It should have 1 "
                f"child, but found {len(self.children)}.")
        return self.children[0]

    @staticmethod
    def create(type_guard_type: TypeGuardType,
               comparison_type: Union[DataType, None],
               children: list[Node]) -> "TypeGuard":
        '''
        Creates an TypeGuard given a condition and its children.

        :param type_guard_type: The type of Type Guard Statement this represents.
        :param comparison_type: The datatype that this TypeGuard compares to.
        :param children: a list of Nodes contained in the body of the new
                         TypeGuard.

        :raises GenerationError: if children is not a list.

        :returns: An TypeGuard representing the inputs.
        '''
        if not isinstance(children, list):
            raise GenerationError(
                f"children argument in create method of TypeGuard class "
                f"should be a list but found {type(children).__name__}."
            )
        is_type = TypeGuard(type_guard_type, comparison_type)
        is_body = Schedule(parent=is_type, children=children)
        is_type.children = [is_body]

        return is_type

    def __str__(self) -> str:
        '''
        :returns the string representation of this node.
        '''
        name = f"{self.name}"
        name += "[{self._type_guard_mapping[self.type_guard_type]}]"
        comparison_type = (self.comparison_type if self.comparison_type is not
                           None else "")
        result = f"{name}[{comparison_type}]\n"
        result += f"{str(self.children[0])}\n"
        result += f"End {name}"
        return result

    def begin_str(self) -> str:
        '''
        :returns: the starting Fortran string of this node.
        '''
        return self._type_guard_mapping[self.type_guard_type]

    def reference_accesses(self) -> VariablesAccessMap:
        '''
        :returns: a map of all the symbol accessed inside this node, the
            keys are Signatures (unique identifiers to a symbol and its
            structure accessors) and the values are AccessSequence
            (a sequence of AccessTypes).

        '''
        for child in self.children:
            var_accesses.update(child.reference_accesses())

        return var_accesses


__all__ = ["TypeGuard"]
