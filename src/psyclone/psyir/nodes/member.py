# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2020-2021, Science and Technology Facilities Council.
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
# Author: A. R. Porter, STFC Daresbury Lab
# -----------------------------------------------------------------------------

''' This module contains the implementation of the Member node.'''

from __future__ import absolute_import
import six
from psyclone.psyir.nodes.node import Node
from psyclone.psyir.symbols import TypeSymbol
from psyclone.psyir.symbols.datatypes import StructureType


class Member(Node):
    '''
    Node representing a membership expression of a structure.
    As such it is a leaf in the PSyIR tree.

    :param struct_type: the datatype of the structure containing the member \
                        that is being referenced.
    :type struct_type: :py:class:`psyclone.psyir.symbols.StructureType` or \
                       :py:class:`psyclone.psyir.symbols.TypeSymbol`
    :param str member: the member of the 'struct_type' structure that is \
                       being referenced.
    :param parent: the parent of this node in the PSyIR tree.
    :type parent: :py:class:`psyclone.psyir.nodes.StructureReference` or \
                  :py:class:`psyclone.psyir.nodes.Member`

    :raises TypeError: if the supplied struct_type, member or parent are of \
                       the wrong type.
    :raises NotImplementedError: if the supplied struct_type doesn't have an \
                                 associated type definition.
    :raises TypeError: if the supplied struct_type does not contain the \
                       specified member.

    '''
    # Textual description of the node.
    _children_valid_format = "<LeafNode>"
    _text_name = "Member"
    _colour_key = "Reference"

    def __init__(self, member, struct_type=None, parent=None):
        # Avoid circular dependency
        # pylint: disable=import-outside-toplevel
        from psyclone.psyir.nodes.structure_reference import StructureReference
        from psyclone.psyir.nodes.structure_member import StructureMember

        if struct_type and not isinstance(struct_type, (StructureType,
                                                        TypeSymbol)):
            raise TypeError(
                "In Member initialisation: expecting the "
                "type of the structure to be specified with either a "
                "StructureType or a TypeSymbol but found '{0}'.".format(
                    type(struct_type).__name__))
        # Since this node represents a membership expression of a structure,
        # its parent (if supplied) *must* subclass StructureReference or
        # StructureMember.
        if (parent and
                not isinstance(parent, (StructureReference, StructureMember))):
            raise TypeError(
                "The parent of a {0} must be either a "
                "(ArrayOf)Structure(s)Reference or (ArrayOf)Structure(s)Member"
                " but found '{1}'.".format(type(self).__name__,
                                           type(parent).__name__))

        super(Member, self).__init__(parent=parent)

        self._component = None
        self._component_name = member

        if struct_type is None:
            return

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
        '''
        :returns: the component of the structure that this member represents.
        :rtype: :py:class:`psyclone.psyir.symbols.datatypes.StructureType.\
                ComponentType` or NoneType
        '''
        return self._component

    @property
    def name(self):
        '''
        :returns: the name of this member.
        :rtype: str
        '''
        return self._component_name

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


# For Sphinx AutoAPI documentation generation
__all__ = ['Member']
