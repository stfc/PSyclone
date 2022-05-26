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
# Author: A. R. Porter and N. Nobre, STFC Daresbury Lab
# Modified by J. Henrichs, Bureau of Meteorology
# -----------------------------------------------------------------------------

''' This module contains the implementation of the StructureMember node.'''

from __future__ import absolute_import
from psyclone.core import Signature
from psyclone.psyir.nodes.member import Member
from psyclone.errors import InternalError


class StructureMember(Member):
    '''
    Node representing a membership expression of the parent's Reference that
    resolves into another structure.

    '''
    # Textual description of the node. Since it represents a reference to a
    # structure it may have a single child which is a reference to one of its
    # members.
    _children_valid_format = "[Member]"
    _text_name = "StructureMember"

    @staticmethod
    def create(member_name, inner_member):
        '''
        Given the name of a structure member of a structure and a Member
        node describing the access to a component of it, construct a
        StructureMember.

        e.g. if we had the following Fortran access: grid%subdomain%xstart
        then 'subdomain' must itself be of structure type and we are accessing
        the 'xstart' component of it. We would therefore create the
        `StructureMember` for this by calling:

        >>> smem = StructureMember.create("subdomain", Member("xstart"))

        :param str member_name: name of the structure member.
        :param inner_member: a Member describing the access to a component \
                             of the named structure member.
        :type inner_member: sub-class of \
                            :py:class:`psyclone.psyir.nodes.Member`

        '''
        smem = StructureMember(member_name)
        smem.addchild(inner_member)
        return smem

    def __str__(self):
        result = super(StructureMember, self).__str__()
        if self._children:
            result += "\n" + str(self._children[0])
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
        if position == 0:
            # The first child must be a Member
            return isinstance(child, Member)
        # Only one child is permitted
        return False

    @property
    def member(self):
        '''
        :returns: the member of the structure that is being accessed.
        :rtype: (sub-class of) :py:class:`psyclone.psyir.nodes.Member`

        :raises InternalError: if the first child of this node is not an \
                               instance of Member.
        '''
        if not isinstance(self.children[0], Member):
            raise InternalError(
                f"{type(self).__name__} malformed or incomplete. The first "
                f"child must be an instance of Member, but found "
                f"'{type(self.children[0]).__name__}'")
        return self.children[0]

    def get_signature_and_indices(self):
        ''':returns: the Signature of this structure member, and \
            a list of the indices used for each component (empty list \
            for this component, since the access is not an array - but \
            other components might have indices).
        :rtype: tuple(:py:class:`psyclone.core.Signature`, list of \
            list of indices)
        '''
        sub_sig, indices = self.children[0].get_signature_and_indices()
        return (Signature(self.name, sub_sig), [[]]+indices)


# For Sphinx AutoAPI documentation generation
__all__ = ['StructureMember']
