''' This module contains the implementation of the MemberReference node.'''

from __future__ import absolute_import
from psyclone.psyir.nodes.member_reference import MemberReference
from psyclone.psyir.symbols import TypeSymbol
from psyclone.psyir.symbols.datatypes import StructureType


class StructureMemberReference(MemberReference):
    '''
    Node representing a reference to a member of a structure (derived type)
    that is itself a derived type.

    '''
    # Textual description of the node.
    _children_valid_format = "[MemberReference | None]"
    _text_name = "StructureMemberReference"

    def __init__(self, target, member, parent=None):
        # Avoid circular dependency
        from psyclone.psyir.nodes.structure_reference import StructureReference

        if not isinstance(target, (StructureType, TypeSymbol)):
            raise TypeError(
                "In MemberReference initialisation expecting a ComponentType "
                "but found '{0}'.".format(type(target).__name__))
        if parent and not isinstance(parent,
                                     (StructureReference, MemberReference)):
            raise TypeError(
                "The parent of a MemberReference must be a StructureReference "
                "but found '{0}'.".format(type(parent).__name))
        super(MemberReference, self).__init__(parent=parent)

        if isinstance(target, StructureType):
            # Store the component that this member points to
            self._component = target.components[member]
        elif isinstance(target, TypeSymbol):
            if isinstance(target.datatype, StructureType):
                self._component = target.datatype.components[member]
            else:
                # We only have a symbol for this structure type
                raise NotImplementedError("Huh")
        self._children = []

    @staticmethod
    def _validate_child(position, child):
        '''
        :param int position: the position to be validated.
        :param child: a child to be validated.
        :type child: :py:class:`psyclone.psyir.nodes.Node`

        :return: whether the given child and position are valid for this node.
        :rtype: bool

        '''
        # pylint: disable=unused-argument
        if position == 0:
            # The first child must either be a MemberReference or None.
            if child:
                return isinstance(child, MemberReference)
            return True
        # Only one child is permitted
        return False

    @property
    def component(self):
        return self._component


__all__ = ['StructureMemberReference']
