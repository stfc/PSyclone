# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2020-2023, Science and Technology Facilities Council.
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
# Modified by: R. W. Ford, A. B. G. Chalk and N. Nobre, STFC Daresbury Lab
# Modified by: J. Henrichs, Bureau of Meteorology
# -----------------------------------------------------------------------------

''' This module contains the implementation of the Member node.'''


from psyclone.core import Signature
from psyclone.psyir.nodes.literal import Literal
from psyclone.psyir.nodes.node import Node
from psyclone.psyir.nodes.operation import BinaryOperation
from psyclone.psyir.nodes.reference import Reference
from psyclone.psyir.symbols import (DataTypeSymbol, ArrayType, DeferredType,
                                    UnknownType, StructureType, INTEGER_TYPE)


class Member(Node):
    '''
    Node representing a membership expression of a structure.
    As such it is a leaf in the PSyIR tree.

    :param str member_name: the name of the member of the structure that is \
                            being referenced.
    :param parent: the parent of this node in the PSyIR tree.
    :type parent: :py:class:`psyclone.psyir.nodes.StructureReference` or \
                  :py:class:`psyclone.psyir.nodes.Member`

    :raises TypeError: if the supplied parent is of the wrong type.

    '''
    # Textual description of the node.
    _children_valid_format = "<LeafNode>"
    _text_name = "Member"
    _colour = "yellow"

    def __init__(self, member_name, parent=None):
        # Avoid circular dependency
        # pylint: disable=import-outside-toplevel
        from psyclone.psyir.nodes.structure_reference import StructureReference
        from psyclone.psyir.nodes.structure_member import StructureMember

        # Since this node represents a membership expression of a structure,
        # its parent (if supplied) *must* subclass StructureReference or
        # StructureMember.
        if (parent and
                not isinstance(parent, (StructureReference, StructureMember))):
            raise TypeError(
                f"The parent of a {type(self).__name__} must be either a "
                f"(ArrayOf)Structure(s)Reference or (ArrayOf)Structure(s)"
                f"Member but found '{type(parent).__name__}'.")

        super(Member, self).__init__(parent=parent)
        # Store the name of the component that this member represents
        self._component_name = member_name

    def __eq__(self, other):
        '''
        Members are assumed to be equivalent if they have the same
        component name associated with them, and are the same type.

        :param object other: the object to check equality to.

        :returns: whether other is equal to self.
        :rtype: bool
        '''
        is_eq = super().__eq__(other)
        is_eq = is_eq and self.name == other.name
        return is_eq

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

    @property
    def is_array(self):
        ''':returns: whether this member is an array.
        :rtype: bool

        '''
        return False

    def get_signature_and_indices(self):
        ''':returns: the Signature of this member access, and a list \
        of list of the indices used for each component, which is empty \
        in this case since it is not an array access.
        :rtype: tuple(:py:class:`psyclone.core.Signature`, list of \
            lists of indices)
        '''
        return (Signature(self.name), [[]])

    def lbound(self, pos):
        '''
        Lookup the lower bound of this Member. If we don't have the
        necessary type information then a call to the LBOUND intrinsic is
        constructed and returned. This method has to be in this class so that
        it is available to both StructureMember and ArrayMember.

        :param int pos: the dimension of the array for which to lookup the \
                        lower bound.

        :returns: the declared lower bound for the specified dimension of \
            this Member or a call to the LBOUND intrinsic if it is unknown.
        :rtype: :py:class:`psyclone.psyir.nodes.Node`

        '''
        # First, walk up to the parent reference and get its type.
        root_ref = self.ancestor(Reference)
        cursor_type = root_ref.symbol.datatype

        # Walk back down the structure, looking up the type information as we
        # go. We also collect the necessary information for creating a new
        # Reference as argument to the LBOUND intrinsic in case the type
        # information is not available.
        cnames = []
        cursor = root_ref
        while cursor is not self:
            cursor = cursor.member
            # Collect member information.
            if hasattr(cursor, "indices"):
                new_indices = [idx.copy() for idx in cursor.indices]
                cnames.append((cursor.name, new_indices))
            else:
                cnames.append(cursor.name)
            # Continue to resolve datatype unless we hit an
            # UnknownType or DeferredType.
            if isinstance(cursor_type, ArrayType):
                cursor_type = cursor_type.intrinsic
            if isinstance(cursor_type, DataTypeSymbol):
                cursor_type = cursor_type.datatype
            if isinstance(cursor_type, (UnknownType, DeferredType)):
                continue
            cursor_type = cursor_type.components[cursor.name].datatype

        if isinstance(cursor_type, ArrayType):
            return cursor_type.shape[pos].lower

        # We've failed to resolve the type so we construct a call to
        # the LBOUND intrinsic instead.
        # Remove any indexing information from the ultimate member
        # of the structure access.
        if len(cnames[-1]) == 2:
            cnames[-1] = cnames[-1][0]
        # Have to import here to avoid circular dependencies.
        # pylint: disable=import-outside-toplevel
        from psyclone.psyir.nodes import (ArrayOfStructuresReference,
                                          StructureReference)
        if hasattr(root_ref, "indices"):
            new_indices = [idx.copy() for idx in root_ref.indices]
            ref = ArrayOfStructuresReference.create(
                root_ref.symbol, new_indices, cnames)
        else:
            ref = StructureReference.create(root_ref.symbol, cnames)

        return BinaryOperation.create(BinaryOperation.Operator.LBOUND, ref,
                                      Literal(str(pos+1), INTEGER_TYPE))


# For Sphinx AutoAPI documentation generation
__all__ = ['Member']
