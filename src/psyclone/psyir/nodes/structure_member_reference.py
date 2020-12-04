''' This module contains the implementation of the MemberReference node.'''

from __future__ import absolute_import
from psyclone.psyir.nodes.member_reference import MemberReference
from psyclone.psyir.symbols import TypeSymbol
from psyclone.psyir.symbols.datatypes import StructureType


class StructureMemberReference(MemberReference):
    '''
    Node representing a reference to a member of a structure (derived type)
    that is itself a derived type.

    :param struct_type: the datatype of the structure containing the member \
                        that is being referenced.
    :type struct_type: :py:class:`psyclone.psyir.symbols.StructureType` or \
                       :py:class:`psyclone.psyir.symbols.TypeSymbol`
    :param str member: the member of the 'target_type' structure that is \
                       being referenced.
    :param parent: the parent of this node in the PSyIR tree.
    :type parent: :py:class:`psyclone.psyir.nodes.StructureReference` or \
                  :py:class:`psyclone.psyir.nodes.MemberReference`

    '''
    # Textual description of the node.
    _children_valid_format = "[MemberReference | None]"
    _text_name = "StructureMemberReference"

    def __init__(self, struct_type, member, parent=None):
        # Avoid circular dependency
        from psyclone.psyir.nodes.structure_reference import StructureReference

        if not isinstance(struct_type, (StructureType, TypeSymbol)):
            raise TypeError(
                "In StructureMemberReference initialisation: expecting a "
                "datatype of either StructureType or TypeSymbol "
                "but found '{0}'.".format(type(struct_type).__name__))
        if parent and not isinstance(parent,
                                     (StructureReference, MemberReference)):
            raise TypeError(
                "The parent of a StructureMemberReference must be either a "
                "StructureReference or MemberReference "
                "but found '{0}'.".format(type(parent).__name))
        super(MemberReference, self).__init__(parent=parent)

        if isinstance(struct_type, StructureType):
            # Store the component that this member points to
            self._component = struct_type.components[member]
        elif isinstance(struct_type, TypeSymbol):
            if isinstance(struct_type.datatype, StructureType):
                self._component = struct_type.datatype.components[member]
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
