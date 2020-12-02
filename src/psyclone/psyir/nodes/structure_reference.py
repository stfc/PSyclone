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

''' This module contains the implementation of the StructureReference node. '''

from __future__ import absolute_import
from psyclone.psyir.nodes.reference import Reference
from psyclone.psyir.nodes.datanode import DataNode
from psyclone.psyir.nodes.member_reference import MemberReference
from psyclone.psyir.symbols import DataSymbol
from psyclone.errors import GenerationError


class StructureReference(Reference):
    '''
    Node representing a reference to a structure (derived type). As such its
    children consist of a reference followed by one or more members.

    '''
    # Textual description of the node.
    _children_valid_format = "[DataNode | MemberReference]*"
    _text_name = "StructureReference"

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
            return isinstance(child, DataNode)
        return isinstance(child, MemberReference)

    @staticmethod
    def create(symbol, members):
        '''Create a StructureReference instance given a symbol and a
        MemberReference.

        :param symbol: the symbol that this array is associated with.
        :type symbol: :py:class:`psyclone.psyir.symbols.DataSymbol`
        :param children: a list of Nodes describing the array indices.
        :type children: list of :py:class:`psyclone.psyir.nodes.Node`

        :returns: an ArrayReference instance.
        :rtype: :py:class:`psyclone.psyir.nodes.ArrayReference`

        :raises GenerationError: if the arguments to the create method \
            are not of the expected type.

        '''
        if not isinstance(symbol, DataSymbol):
            raise GenerationError(
                "symbol argument in create method of ArrayReference class "
                "should be a DataSymbol but found '{0}'.".format(
                    type(symbol).__name__))
#        if not isinstance(children, list):
#            raise GenerationError(
#                "children argument in create method of ArrayReference class "
#                "should be a list but found '{0}'."
#                "".format(type(children).__name__))
#        if not symbol.is_array:
#            raise GenerationError(
#                "expecting the symbol to be an array, not a scalar.")

        ref = StructureReference(symbol)
        current = ref
        dtype = ref.symbol.datatype
        for member in members:
            subref = MemberReference(dtype,
                                     member,
                                     parent=ref)
            current.addchild(subref)
            current = subref
            dtype = subref.component.datatype
        return ref

    def __str__(self):
        result = ("StructureReference" +
                  super(StructureReference, self).__str__() + "\n")
        for entity in self._children:
            result += str(entity) + "\n"
        return result

    def reference_accesses(self, var_accesses):
        '''Get all variable access information. All variables used as indices
        in the access of the array will be added as READ.

        :param var_accesses: variable access information.
        :type var_accesses: \
            :py:class:`psyclone.core.access_info.VariablesAccessInfo`
        '''

        # This will set the array-name as READ
        super(StructureReference, self).reference_accesses(var_accesses)

        # Now add all children: Note that the class Reference
        # does not recurse to the children (which store the indices), so at
        # this stage no index information has been stored:
        list_indices = []
        for child in self._children:
            child.reference_accesses(var_accesses)
            list_indices.append(child)

        if list_indices:
            var_info = var_accesses[self.name]
            # The last entry in all_accesses is the one added above
            # in super(ArrayReference...). Add the indices to that entry.
            var_info.all_accesses[-1].indices = list_indices


# For AutoAPI documentation generation
__all__ = ['StructureReference']
