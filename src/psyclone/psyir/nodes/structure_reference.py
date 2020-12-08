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
from psyclone.psyir.nodes.member_reference import MemberReference
from psyclone.psyir.nodes.array_member_reference import ArrayMemberReference
from psyclone.psyir.nodes.array_node import ArrayNode
from psyclone.psyir.nodes.array_structure_member_reference import \
    ArrayStructureMemberReference
from psyclone.psyir.nodes.structure_member_reference import \
    StructureMemberReference
from psyclone.psyir.symbols import DataSymbol, TypeSymbol, StructureType, \
    ScalarType, ArrayType, DeferredType
from psyclone.errors import GenerationError


class StructureReference(Reference):
    '''
    Node representing a reference to a structure (derived type). As such
    it may only have a maximum of one child (representing a reference to a
    component of the structure).

    '''
    # Textual description of the node.
    _children_valid_format = "MemberReference"
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
            return isinstance(child, (MemberReference))
        return False

    @staticmethod
    def create(symbol, members, parent=None):
        '''Create a StructureReference instance given a symbol and a
        list of components. e.g. for "field%bundle(2)%flag" this
        list would be [("bundle", [Literal("2", INTEGER4_TYPE)]), "flag"].

        :param symbol: the symbol that this reference is to.
        :type symbol: :py:class:`psyclone.psyir.symbols.DataSymbol`
        :param members: the component(s) of the structure that make up \
            the full reference. Any components that are array references must \
            provide the name of the array and a list of DataNodes describing \
            which part of it is accessed.
        :type members: list of str or 2-tuples containing (str, \
            list of nodes describing array access)
        :param parent: the parent of this node in the PSyIR.
        :type parent: sub-class of :py:class:`psyclone.psyir.nodes.Node`

        :returns: a StructureReference instance.
        :rtype: :py:class:`psyclone.psyir.nodes.StructureReference`

        :raises TypeError: if the arguments to the create method are not of \
            the expected type.

        '''
        if not isinstance(symbol, DataSymbol):
            raise TypeError(
                "The 'symbol' argument to StructureReference.create() "
                "should be a DataSymbol but found '{0}'.".format(
                    type(symbol).__name__))
        if not isinstance(symbol.datatype, (StructureType,
                                            TypeSymbol,
                                            DeferredType)):
            raise TypeError(
                "A StructureReference must refer to a symbol that is (or "
                "could be) a structure, however symbol '{0}' has type "
                "'{1}'.".format(symbol.name, symbol.datatype))
        if not isinstance(members, list):
            raise TypeError(
                "The 'members' argument to StructureReference.create() "
                "should be a list but found '{0}'."
                "".format(type(members).__name__))

        # Create the base reference to the symbol that is a structure
        ref = StructureReference(symbol, parent=parent)
        current = ref

        if isinstance(ref.symbol.datatype, TypeSymbol):
            dtype = ref.symbol.datatype.datatype
        else:
            # Currently we only support references to symbols with fully-
            # specified type information.
            # TODO #363 support deferred/incomplete types
            raise NotImplementedError(
                "The symbol being referenced must have a TypeSymbol as its "
                "type but '{0}' has type '{1}'".format(symbol.name,
                                                       symbol.datatype))

        if not isinstance(dtype, StructureType):
            # TODO #363 support deferred/incomplete types
            raise NotImplementedError(
                "The TypeSymbol '{0}' for symbol '{1}' must have a defined "
                "StructureType but found '{2}'".format(
                    ref.symbol.datatype.name, symbol.name, dtype))

        # We now make our way along the list of components that makes up
        # the full reference. For each entry in this list we go down another
        # level in the PSyIR tree.
        for component in members:
            if isinstance(component, tuple):
                member_name = component[0]
                children = component[1]
            elif isinstance(component, str):
                member_name = component
                children = None
            else:
                raise TypeError(
                    "The list of 'members' passed to StructureType.create() "
                    "must consist of either 'str' or 2-tuple entries but "
                    "found '{0}' while attempting to create reference to "
                    "symbol '{1}'".format(type(component).__name__,
                                          symbol.name))
            if member_name not in dtype.components:
                raise GenerationError(
                    "The type definition for symbol '{0}' does not contain a "
                    "member named '{1}'".format(symbol.name, member_name))

            target_dtype = dtype.components[member_name].datatype
            if isinstance(target_dtype, TypeSymbol):
                # This member is also a structure
                subref = StructureMemberReference(dtype,
                                                  member_name,
                                                  parent=current)
            elif isinstance(target_dtype, ArrayType):
                if isinstance(target_dtype.intrinsic, TypeSymbol):
                    # Array of structures
                    subref = ArrayStructureMemberReference.create(
                        dtype, member_name, parent=current, indices=children)
                else:
                    # Array of intrinsic quantities
                    subref = ArrayMemberReference.create(
                        dtype, member_name, parent=current, indices=children)
            elif isinstance(target_dtype, ScalarType):
                # A scalar member
                subref = MemberReference(dtype, member_name, parent=current)
            else:
                raise NotImplementedError(
                    "Structure members must have a type given by a TypeSymbol "
                    "or be of type ArrayType or ScalarType. However, member "
                    "'{0}' of '{1}' is of type '{2}'".format(
                        member_name, current.name, target_dtype))

            # The reference to a sub-component is stored as the first child.
            # If the current node is an ArrayNode then it will already have
            # a list of children, otherwise it won't.
            if isinstance(current, ArrayNode):
                current.children[0] = subref
            else:
                current.addchild(subref)

            # Move down to the newly-added child
            current = subref
            dtype = subref.component.datatype
            if isinstance(dtype, TypeSymbol):
                # This reference is to a derived type
                dtype = dtype.datatype
            elif (isinstance(dtype, ArrayType) and
                  isinstance(dtype.intrinsic, TypeSymbol)):
                # This reference is to an array of derived types
                dtype = dtype.intrinsic.datatype
        return ref

    def __str__(self):
        result = super(StructureReference, self).__str__() + "\n"
        for entity in self._children:
            result += str(entity) + "\n"
        return result

    def reference_accesses(self, var_accesses):
        '''Get all variable access information. All variables used as indices
        in the access of the array will be added as READ.

        :param var_accesses: variable access information.
        :type var_accesses: \
            :py:class:`psyclone.core.access_info.VariablesAccessInfo`

        :raises NotImplementedError: TODO #1028 dependency analysis for \
            structures needs to be implemented.

        '''
        raise NotImplementedError(
            "Dependency analysis has not yet been implemented for Structures.")


# For AutoAPI documentation generation
__all__ = ['StructureReference']
