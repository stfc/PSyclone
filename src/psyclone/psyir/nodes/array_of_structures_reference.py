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
# -----------------------------------------------------------------------------

''' This module contains the implementation of the ArrayOfStructuresReference
node. '''

from __future__ import absolute_import

# Circular import if only '...nodes' is used:
from psyclone.psyir.nodes.structure_reference import StructureReference
from psyclone.psyir import symbols
from psyclone.psyir.nodes.array_of_structures_mixin import \
    ArrayOfStructuresMixin


class ArrayOfStructuresReference(ArrayOfStructuresMixin, StructureReference):
    '''
    Node representing an access to a member of one or more elements of an
    array of structures. Since this access is to a member of the
    structure, its first child will be a subclass of Member. All subsequent
    children give the array-index expressions.

    '''
    # Textual description of the node.
    _children_valid_format = "MemberReference, [DataNode | Range]+"
    _text_name = "ArrayOfStructuresReference"

    # pylint: disable=arguments-differ
    @staticmethod
    def create(symbol, indices, members, parent=None):
        '''
        Create a reference to a member of one or more elements of an array of
        structures.

        The symbol to be referred to must be of DeferredType, UnknownType or
        ArrayType. If the latter then the 'intrinsic' type of the array must
        be specified with a DataTypeSymbol. The member of the
        structure that is accessed is specified using the 'members'
        argument. e.g. for a reference to "field(idx)%bundle(2)%flag" this
        argument would be [("bundle", [Literal("2", INTEGER4_TYPE)]), "flag"].
        The 'indices' argument specifies the DataNodes describing the indexing
        into the array of structures. For the example given previously, this
        would be [Reference(idx_symbol)] where `idx_symbol` is the Symbol
        representing the `idx` variable in the Fortran code snippet.

        :param symbol: the symbol that this reference is to.
        :type symbol: :py:class:`psyclone.psyir.symbols.DataSymbol`
        :param indices: a list of Nodes describing the array indices.
        :type indices: list of :py:class:`psyclone.psyir.nodes.Node`
        :param members: one or more component(s) of the structure(s) that \
            make up the full access. Any components that are array \
            references must provide the name of the array and a list of \
            DataNodes describing which part of it is accessed.
        :type members: list of str or 2-tuples containing (str, \
            list of nodes describing array access)
        :param parent: the parent of this node in the PSyIR.
        :type parent: sub-class of :py:class:`psyclone.psyir.nodes.Node`

        :returns: an ArrayOfStructuresReference instance.
        :rtype: :py:class:`psyclone.psyir.nodes.ArrayOfStructuresReference`

        :raises TypeError: if the arguments to the create method \
            are not of the expected type.

        '''
        if not isinstance(symbol, symbols.DataSymbol):
            raise TypeError(
                f"The 'symbol' argument to ArrayOfStructuresReference.create()"
                f" must be a DataSymbol but found '{type(symbol).__name__}'.")
        if isinstance(symbol.datatype, symbols.ArrayType):
            base_type = symbol.datatype.intrinsic
        elif isinstance(symbol.datatype, (symbols.DeferredType,
                                          symbols.UnknownType)):
            base_type = symbol.datatype
        else:
            raise TypeError(
                f"An ArrayOfStructuresReference must refer to a symbol of "
                f"ArrayType, DeferredType or UnknownType but symbol "
                f"'{symbol.name}' has type '{symbol.datatype}")
        if not isinstance(indices, list) or not indices:
            raise TypeError(
                f"The 'indices' argument to "
                f"ArrayOfStructuresReference.create() must be a list "
                f"containing at least one array-index expression but this is "
                f"missing for symbol '{symbol.name}'")

        # First use the StructureReference _create class method to create a
        # reference to the base structure of the array.
        ref = ArrayOfStructuresReference._create(symbol, base_type, members,
                                                 parent=parent)

        # Then add the array-index expressions. We don't validate the children
        # as that is handled in _validate_child.
        for child in indices:
            ref.addchild(child)
        return ref


# For AutoAPI documentation generation
__all__ = ['ArrayOfStructuresReference']
