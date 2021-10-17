# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2021, Science and Technology Facilities Council.
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
# Modified by J. Henrichs, Bureau of Meteorology
# -----------------------------------------------------------------------------

''' This module contains the implementation of the ArrayOfStructuresMember
    node.'''

from __future__ import absolute_import
from psyclone.psyir.nodes.structure_member import StructureMember
from psyclone.psyir.nodes.array_of_structures_mixin import \
    ArrayOfStructuresMixin


class ArrayOfStructuresMember(ArrayOfStructuresMixin, StructureMember):
    '''
    Node representing a membership expression of a parent structure where the
    expression resolves to the component of one or more elements of an array
    of structures.
    As such, its first child must be a member of that structure. Subsequent
    children give the array-index expressions.

    '''
    # Textual description of the node. The first child must be a Member
    # describing an access to a member of this structure. Subsequent children
    # give the array-index expressions.
    _children_valid_format = "Member, [DataNode | Range]+"
    _text_name = "ArrayOfStructuresMember"

    # pylint: disable=arguments-differ
    @staticmethod
    def create(member_name, indices, inner_member):
        '''
        Create an access to a member of one or more elements of an array of
        structures that is itself a member of a structure.

        e.g. if we had the Fortran `grid%subdomains(1)%xstart` then
        `subdomains` must be an array of structure (derived) type. We would
        construct an ArrayOfStructuresMember for this access by calling:

        >>> aosmem = ArrayOfStructuresMember.create(
                "subdomains", [Literal("1", INTEGER_TYPE)], Member("xstart"))

        :param str member_name: the name of the array member of the structure \
            that is being accessed.
        :param indices: the array-index expressions.
        :type indices: list of :py:class:`psyclone.psyir.nodes.DataNode`
        :param inner_member: the member of the `member_name` structure that \
            is being accessed.
        :type inner_member: :py:class:`psyclone.psyir.nodes.Member`

        :returns: a new ArrayOfStructuresMember object.
        :rtype: :py:class:`psyclone.psyir.nodes.ArrayOfStructuresMember`

        '''
        obj = ArrayOfStructuresMember(member_name)
        # Add the inner_member node as the first child
        obj.addchild(inner_member)
        # Add the array-index expressions as subsequent children
        for child in indices:
            obj.addchild(child)
        return obj


# For AutoAPI automatic documentation generation
__all__ = ['ArrayOfStructuresMember']
