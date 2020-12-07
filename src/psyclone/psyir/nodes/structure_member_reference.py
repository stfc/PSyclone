# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2020, Science and Technology Facilities Council.
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

''' This module contains the implementation of the MemberReference node.'''

from __future__ import absolute_import
from psyclone.psyir.nodes.member_reference import MemberReference
from psyclone.psyir.symbols import TypeSymbol
from psyclone.psyir.symbols.datatypes import StructureType, DeferredType, \
    ArrayType


class StructureMemberReference(MemberReference):
    '''
    Node representing a reference to a member of a structure (derived type)
    that is itself a derived type.

    :param struct_type: the datatype of the structure containing the member \
                        that is being referenced.
    :type struct_type: :py:class:`psyclone.psyir.symbols.StructureType` or \
                       :py:class:`psyclone.psyir.symbols.TypeSymbol`
    :param str member: the member of the 'struct_type' structure that is \
                       being referenced.
    :param parent: the parent of this node in the PSyIR tree.
    :type parent: :py:class:`psyclone.psyir.nodes.StructureReference` or \
                  :py:class:`psyclone.psyir.nodes.MemberReference`

    :raises TypeError: if the type of the specified member is not \
                       consistent with it being a structure type.

    '''
    # Textual description of the node. Since it represents a reference to a
    # structure it may have a single child which is a reference to one of its
    # members.
    _children_valid_format = "MemberReference | None"
    _text_name = "StructureMemberReference"

    def __init__(self, struct_type, member, parent=None, children=None):

        super(StructureMemberReference, self).__init__(struct_type, member,
                                                       parent=parent)
        if isinstance(self.component.datatype, ArrayType):
            target_type = self.component.datatype.intrinsic
        else:
            target_type = self.component.datatype
        if not isinstance(target_type,
                          (DeferredType, StructureType, TypeSymbol)):
            raise TypeError(
                "The member '{0}' is not of DeferredType, StructureType or a "
                "TypeSymbol and therefore cannot be the target of a "
                "StructureMemberReference.".format(member))

        if children:
            for child in children:
                self.addchild(child)
                child.parent = self

    def __str__(self):
        result = super(StructureMemberReference, self).__str__() + "\n"
        if self._children[0]:
            result += str(self._children[0]) + "\n"
        return result

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


__all__ = ['StructureMemberReference']
