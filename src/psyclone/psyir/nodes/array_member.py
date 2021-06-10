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

''' This module contains the implementation of the ArrayMember node.'''

from __future__ import absolute_import
from psyclone.psyir.nodes.array_mixin import ArrayMixin
from psyclone.psyir.nodes.member import Member
from psyclone.psyir.nodes.operation import BinaryOperation
from psyclone.psyir.nodes.ranges import Range
from psyclone.psyir.nodes.reference import Reference
from psyclone.errors import GenerationError


class ArrayMember(ArrayMixin, Member):
    '''
    Node representing an access to the element(s) of an array that is a
    member of a structure. Must have one or more children which give the
    array-index expressions for the array access.

    '''
    # Textual description of the node.
    _children_valid_format = "[DataNode | Range]+"
    _text_name = "ArrayMember"

    @staticmethod
    def create(member_name, indices):
        '''
        Construct an ArrayMember instance describing an array access to a
        member of a structure.

        e.g. for the Fortran `grid%subdomains(1,2)`, `subdomains` must be an
        array and we are accessing element (1,2) of it. We would therefore
        create the ArrayMember for this access by calling:

        >>> amem = ArrayMember.create("subdomains",
                                      [Literal("1", INTEGER_TYPE),
                                       Literal("2", INTEGER_TYPE)])

        :param str member_name: the name of the member of the structure that \
            is being accessed.
        :param indices: the array-index expressions.
        :type indices: list of :py:class:`psyclone.psyir.nodes.DataNode` or
            :py:class:`psyclone.psyir.nodes.Range`

        :raises GenerationError: if the supplied `indices` argument is not \
                                 a list.
        '''
        if not isinstance(indices, list):
            raise GenerationError(
                "indices argument in create method of ArrayMember class "
                "should be a list but found '{0}'."
                "".format(type(indices).__name__))

        obj = ArrayMember(member_name)
        # Add any array-index expressions as children
        for child in indices:
            obj.addchild(child)
        return obj

    def is_lower_bound(self, index):
        '''Returns True if the specified array index contains a Range node
        which has a starting value given by the 'LBOUND(access-expr,index)'
        intrinsic where 'access-expr' is the full access to the current
        ArrayMember and 'index' matches the specified array index. Otherwise
        False is returned.

        For example, if a Fortran array A%B was allocated with extent 10
        then the starting value is 1 and LBOUND(A%B,1) would
        return that value.

        :param int index: the array index to check.

        :returns: True if the array index is a range with its start \
            value being LBOUND(array,index) and False otherwise.
        :rtype: bool

        '''
        self._validate_index(index)

        array_dimension = self.indices[index]
        if not isinstance(array_dimension, Range):
            return False

        lower = array_dimension.children[0]
        if not (isinstance(lower, BinaryOperation) and
                lower.operator == BinaryOperation.Operator.LBOUND):
            return False
        if not isinstance(lower.children[0], Reference):
            return False

        # Check that the full structure access being passed to the LBOUND
        # operator matches the one for this node.
        return self._matching_access(lower.children[0])

    def is_upper_bound(self, index):
        '''Returns True if the specified array index contains a Range node
        which has a stopping value given by the 'UBOUND(access-expr,index)'
        intrinsic where 'access-expr' is the full access to the current
        ArrayMember and 'index' matches the specified array index. Otherwise
        False is returned.

        For example, if a Fortran array A%B has extent 10 then the stopping
        value is 10 and UBOUND(A%B,1) would return that value.

        :param int index: the array index to check.

        :returns: True if the array index is a range with its stop \
            value being UBOUND(array,index) and False otherwise.
        :rtype: bool

        '''
        self._validate_index(index)

        array_dimension = self.indices[index]
        if not isinstance(array_dimension, Range):
            return False

        upper = array_dimension.children[1]
        if not (isinstance(upper, BinaryOperation) and
                upper.operator == BinaryOperation.Operator.UBOUND):
            return False
        if not isinstance(upper.children[0], Reference):
            return False

        # Check that the full structure access being passed to the UBOUND
        # operator matches the one for this node.
        return self._matching_access(upper.children[0])

    def _matching_access(self, node):
        '''
        Examines the full structure access represented by the supplied node
        to see whether it is the same as the one for this node. Any indices
        on the innermost member access are ignored. e.g.
        A(3)%B%C(1) will match with A(3)%B%C but not with A(2)%B%C(1)

        :returns: True if the structure accesses match, False otherwise.
        :rtype: bool

        '''
        # This node is at the 'bottom' of the structure access so we
        # need to get the parent Reference.
        parent_member = self.ancestor(Reference)
        if not parent_member:
            return False
        self_sig, self_indices = parent_member.get_signature_and_indices()
        node_sig, node_indices = node.get_signature_and_indices()
        if self_sig != node_sig:
            return False

        # We use the FortranWriter to simplify the job of comparing array-index
        # expressions but have to import it here to avoid circular dependencies
        # pylint: disable=import-outside-toplevel
        from psyclone.psyir.backend.fortran import FortranWriter
        fwriter = FortranWriter()

        # Examine the indices, ignoring any on the innermost accesses.
        for indices in zip(self_indices[:-1], node_indices[:-1]):
            if ("".join(fwriter(idx) for idx in indices[0]) !=
                    "".join(fwriter(idx) for idx in indices[1])):
                return False
        return True


# For AutoAPI documentation generation
__all__ = ['ArrayMember']
