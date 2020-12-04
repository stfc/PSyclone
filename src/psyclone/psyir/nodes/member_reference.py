''' This module contains the implementation of the MemberReference node.'''

from __future__ import absolute_import
from psyclone.psyir.nodes.datanode import DataNode
from psyclone.psyir.symbols import TypeSymbol
from psyclone.psyir.symbols.datatypes import StructureType


class MemberReference(DataNode):
    '''
    Node representing a reference to a member of a structure (derived type).
    As such it is a leaf in the PSyIR tree.

    '''
    # Textual description of the node.
    _children_valid_format = "<LeafNode>"
    _text_name = "MemberReference"
    _colour_key = "Reference"

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


__all__ = ['MemberReference']
