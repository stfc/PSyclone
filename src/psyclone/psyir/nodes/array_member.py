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
# Author: A. R. Porter and N. Nobre, STFC Daresbury Lab
# -----------------------------------------------------------------------------

''' This module contains the implementation of the ArrayMember node.'''

from psyclone.errors import GenerationError
from psyclone.psyir.nodes.array_mixin import ArrayMixin
from psyclone.psyir.nodes.literal import Literal
from psyclone.psyir.nodes.member import Member
from psyclone.psyir.nodes.operation import BinaryOperation
from psyclone.psyir.nodes.reference import Reference
from psyclone.psyir.symbols import (DataTypeSymbol, ArrayType,
                                    StructureType, INTEGER_TYPE)


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
                f"indices argument in create method of ArrayMember class "
                f"should be a list but found '{type(indices).__name__}'.")

        obj = ArrayMember(member_name)
        # Add any array-index expressions as children
        for child in indices:
            obj.addchild(child)
        return obj

    def lbound(self, pos):
        '''
        Lookup the lower bound of this ArrayMember. If we don't have the
        necessary type information then a call to the LBOUND intrinsic is
        constructed and returned.

        :param int pos: the dimension of the array for which to lookup the \
                        lower bound.

        :returns: the declared lower bound for the specified dimension of \
            this ArrayMember or a call to the LBOUND intrinsic if it is not \
            known.
        :rtype: :py:class:`psyclone.psyir.nodes.Node`

        '''
        # First, walk up to the parent reference, collecting the necessary
        # information to make a new [ArrayOf]Structure[s]Reference as we go.
        cnames = []
        cursor = self
        while not isinstance(cursor, Reference):
            if hasattr(cursor, "indices"):
                new_indices = [idx.copy() for idx in cursor.indices]
                cnames.insert(0, (cursor.name, new_indices))
            else:
                cnames.insert(0, cursor.name)
            cursor = cursor.parent
        # Now that we've reached the parent reference, we lookup its type
        # and then walk back down the type information to find that for
        # this ArrayMember (if available).
        dtype = cursor.symbol.datatype

        for entry in cnames:
            if isinstance(dtype, ArrayType):
                dtype = dtype.intrinsic
            if isinstance(dtype, DataTypeSymbol):
                dtype = dtype.datatype
            if not isinstance(dtype, StructureType):
                # We can't resolve the type.
                break
            if len(entry) == 2:
                name = entry[0]
            else:
                name = entry
            dtype = dtype.components[name]
            # dtype will be of ComponentType so get the associated datatype.
            dtype = dtype.datatype

        if not isinstance(dtype, ArrayType):
            # We've failed to resolve the type so we construct a call to
            # the LBOUND intrinsic instead.
            # Remove the indexing information from the ultimate member
            # of the structure access.
            cnames[-1] = cnames[-1][0]
            # Have to import here to avoid circular dependencies.
            # pylint: disable=import-outside-toplevel
            from psyclone.psyir.nodes import (ArrayOfStructuresReference,
                                              StructureReference)
            if hasattr(cursor, "indices"):
                new_indices = [idx.copy() for idx in cursor.indices]
                ref = ArrayOfStructuresReference.create(
                    cursor.symbol, new_indices, cnames)
            else:
                ref = StructureReference.create(cursor.symbol, cnames)
            return BinaryOperation.create(
                BinaryOperation.Operator.LBOUND, ref,
                Literal(str(pos+1), INTEGER_TYPE))

        return dtype.shape[pos].lower


# For AutoAPI documentation generation
__all__ = ['ArrayMember']
