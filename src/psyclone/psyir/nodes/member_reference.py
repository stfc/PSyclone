''' This module contains the implementation of the MemberReference node.'''

from __future__ import absolute_import
import six
from psyclone.psyir.nodes.datanode import DataNode
from psyclone.psyir.symbols import TypeSymbol
from psyclone.psyir.symbols.datatypes import StructureType


class MemberReference(DataNode):
    '''
    Node representing a reference to a member of a structure (derived type).
    As such it is a leaf in the PSyIR tree.

    :param struct_type: the datatype of the structure containing the member \
                        that is being referenced.
    :type struct_type: :py:class:`psyclone.psyir.symbols.StructureType` or \
                       :py:class:`psyclone.psyir.symbols.TypeSymbol`
    :param str member: the member of the 'struct_type' structure that is \
                       being referenced.
    :param parent: the parent of this node in the PSyIR tree.
    :type parent: :py:class:`psyclone.psyir.nodes.StructureReference` or \
                  :py:class:`psyclone.psyir.nodes.MemberReference`

    :raises TypeError: if the supplied struct_type, member or parent are of \
                       the wrong type.
    :raises NotImplementedError: if the supplied struct_type doesn't have an \
                                 associated type definition.
    :raises TypeError: if the supplied struct_type does not contain the \
                       specified member.

    '''
    # Textual description of the node.
    _children_valid_format = "<LeafNode>"
    _text_name = "MemberReference"
    _colour_key = "Reference"

    def __init__(self, struct_type, member, parent=None):
        # Avoid circular dependency
        from psyclone.psyir.nodes.structure_reference import StructureReference
        from psyclone.psyir.nodes.structure_member_reference import \
            StructureMemberReference

        if not isinstance(struct_type, (StructureType, TypeSymbol)):
            raise TypeError(
                "In MemberReference initialisation: expecting the "
                "type of the structure to be specified with either a "
                "StructureType or a TypeSymbol but found '{0}'.".format(
                    type(struct_type).__name__))
        # Since this node represents a reference to a member of a structure,
        # its parent (if supplied) *must* subclass StructureReference or
        # StructureMemberReference.
        if (parent and
                not isinstance(parent, (StructureReference,
                                        StructureMemberReference))):
            raise TypeError(
                "The parent of a {0} must be either a "
                "(Array)StructureReference or (Array)StructureMemberReference "
                "but found '{1}'.".format(type(self).__name__,
                                          type(parent).__name__))

        super(MemberReference, self).__init__(parent=parent)

        try:
            if isinstance(struct_type, StructureType):
                # Store the component that this member points to
                self._component = struct_type.components[member]
            elif isinstance(struct_type, TypeSymbol):
                if isinstance(struct_type.datatype, StructureType):
                    self._component = struct_type.datatype.components[member]
                else:
                    # We only have a symbol for this structure type
                    raise NotImplementedError(
                        "Can only create a reference to a structure if its "
                        "type is available but member '{0}' has type "
                        "'{1}'".format(member,
                                       type(struct_type.datatype).__name__))
        except KeyError as err:
            six.raise_from(
                TypeError("The supplied {0} has no component named "
                          "'{1}'".format(type(struct_type).__name__, member)),
                err)

    @property
    def component(self):
        return self._component

    @property
    def name(self):
        return self._component.name

    def node_str(self, colour=True):
        ''' Create a text description of this node in the schedule, optionally
        including control codes for colour.

        :param bool colour: whether or not to include colour control codes.

        :return: text description of this node.
        :rtype: str
        '''
        return self.coloured_name(colour) + "[name:'" + self.name + "']"

    def __str__(self):
        return self.node_str(False)


__all__ = ['MemberReference']
