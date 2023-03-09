# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2021-2023, Science and Technology Facilities Council.
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
# Authors R. W. Ford, A. R. Porter, S. Siso and N. Nobre, STFC Daresbury Lab
#         I. Kavcic, Met Office
#         J. Henrichs, Bureau of Meteorology
# -----------------------------------------------------------------------------

''' This module contains the implementation of the abstract ArrayMixin. '''

import abc

from psyclone.errors import InternalError
from psyclone.psyir.nodes.call import Call
from psyclone.psyir.nodes.codeblock import CodeBlock
from psyclone.psyir.nodes.datanode import DataNode
from psyclone.psyir.nodes.literal import Literal
from psyclone.psyir.nodes.member import Member
from psyclone.psyir.nodes.operation import Operation, BinaryOperation
from psyclone.psyir.nodes.ranges import Range
from psyclone.psyir.nodes.reference import Reference
from psyclone.psyir.symbols import SymbolError, DataSymbol, DataTypeSymbol
from psyclone.psyir.symbols.datatypes import (
    ScalarType, ArrayType, DeferredType, UnknownType, INTEGER_TYPE)


class ArrayMixin(metaclass=abc.ABCMeta):
    '''
    Abstract class used to add functionality common to Nodes that represent
    Array accesses.

    '''
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
        return isinstance(child, (DataNode, Range))

    @property
    def is_array(self):
        ''':returns: if this instance indicates an array access.
        :rtype: bool

        '''
        return True

    def get_signature_and_indices(self):
        '''
        Constructs the Signature of this array access and a list of the
        indices used.

        :returns: the Signature of this array reference, and \
            a list of the indices used for each component (empty list \
            if an access is not an array). In this base class there is \
            no other component, so it just returns a list with a list \
            of all indices.
        :rtype: tuple(:py:class:`psyclone.core.Signature`, list of \
            lists of indices)
        '''
        sig, _ = super().get_signature_and_indices()
        return (sig, [self.indices[:]])

    def _validate_index(self, index):
        '''Utility function that checks that the supplied index is an integer
        and is less than the number of array dimensions.

        :param int index: the array index to check.

        :raises TypeError: if the index argument is not an integer.
        :raises ValueError: if the index value is greater than the \
            number of dimensions in the array (-1).

        '''
        if not isinstance(index, int):
            raise TypeError(
                f"The index argument should be an integer but found "
                f"'{type(index).__name__}'.")
        if index > len(self.indices)-1:
            raise ValueError(
                f"In ArrayReference '{self.name}' the specified index "
                f"'{index}' must be less than the number of dimensions "
                f"'{len(self.indices)}'.")

    def is_lower_bound(self, index):
        '''Returns True if the specified array index contains a Range node
        which has a starting value given by the 'LBOUND(name,index)'
        intrinsic where 'name' is the name of the current Array and
        'index' matches the specified array index. Also returns True
        if the starting value of the range node is an integer that
        matches the starting value of the declaration. Otherwise False
        is returned.

        For example, if a Fortran array A was declared as
        A(10) then the starting value is 1 and LBOUND(A,1) would
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

        lower = array_dimension.start

        if isinstance(lower, Literal):
            try:
                symbol = self.scope.symbol_table.lookup(self.name)
                if not isinstance(symbol, DataSymbol):
                    # We don't have any type information on this symbol
                    # (probably because it originates from a wildcard import).
                    return False
                datatype = symbol.datatype
                # Check that the symbol is of ArrayType. (It may be of
                # UnknownFortranType if the symbol is of e.g. character type.)
                if not isinstance(datatype, ArrayType):
                    return False
                shape = datatype.shape
                array_bounds = shape[index]
                if (isinstance(array_bounds, ArrayType.ArrayBounds)
                        and isinstance(array_bounds.lower, Literal)):
                    if lower.value == array_bounds.lower.value:
                        return True
            except (KeyError, SymbolError, AttributeError):
                # If any issue is found we can not guarantee that it is
                # the lower bound
                pass
            return False

        if not (isinstance(lower, BinaryOperation) and
                lower.operator == BinaryOperation.Operator.LBOUND):
            return False

        if not isinstance(lower.children[0], Reference):
            return False

        if not self.is_same_array(lower.children[0]):
            return False

        if not (isinstance(lower.children[1], Literal) and
                lower.children[1].datatype.intrinsic ==
                ScalarType.Intrinsic.INTEGER
                and lower.children[1].value == str(index+1)):
            return False
        return True

    def get_lbound_expression(self, pos):
        '''
        Lookup the lower bound of this ArrayMixin. If we don't have the
        necessary type information then a call to the LBOUND intrinsic is
        constructed and returned.

        :param int pos: the dimension of the array for which to lookup the \
                        lower bound.

        :returns: the declared lower bound for the specified dimension of \
            the array accesed or a call to the LBOUND intrinsic if it is \
            unknown.
        :rtype: :py:class:`psyclone.psyir.nodes.Node`

        '''
        # First, walk up to the parent reference and get its type. For a simple
        # ArrayReference this will just be self.
        root_ref = self.ancestor(Reference, include_self=True)
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
            if isinstance(cursor, ArrayMixin):
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

        if (isinstance(cursor_type, ArrayType) and
                cursor_type.shape[pos] not in [ArrayType.Extent.DEFERRED,
                                               ArrayType.Extent.ATTRIBUTE]):
            # We have the full type information and the lower bound is known.
            return cursor_type.shape[pos].lower.copy()

        # We've either failed to resolve the type or we don't know the extent
        # of the array dimension so construct a call to the LBOUND intrinsic.
        if cnames:
            # We have some sort of structure access - remove any indexing
            # information from the ultimate member of the structure access.
            if len(cnames[-1]) == 2:
                cnames[-1] = cnames[-1][0]
            # Have to import here to avoid circular dependencies.
            # pylint: disable=import-outside-toplevel
            from psyclone.psyir.nodes import (ArrayOfStructuresReference,
                                              StructureReference)
            if isinstance(root_ref, ArrayMixin):
                new_indices = [idx.copy() for idx in root_ref.indices]
                ref = ArrayOfStructuresReference.create(
                    root_ref.symbol, new_indices, cnames)
            else:
                ref = StructureReference.create(root_ref.symbol, cnames)
        else:
            # A simple Reference.
            ref = Reference(root_ref.symbol)

        return BinaryOperation.create(BinaryOperation.Operator.LBOUND, ref,
                                      Literal(str(pos+1), INTEGER_TYPE))

    def is_upper_bound(self, index):
        '''Returns True if the specified array index contains a Range node
        which has a stopping value given by the 'UBOUND(name,index)'
        intrinsic where 'name' is the name of the current
        ArrayReference and 'index' matches the specified array index.
        Also returns True if the stopping value of the range node is
        an integer that matches the stopping value of the
        declaration. Otherwise False is returned.

        For example, if a Fortran array A was declared as
        A(10) then the stopping value is 10 and UBOUND(A,1) would
        return that value.

        :param int index: the array index to check.

        :returns: True if the array index is a range with its stop \
            value being UBOUND(array,index) and False otherwise.
        :rtype: bool

        '''
        self._validate_index(index)

        array_dimension = self.indices[index]
        if not isinstance(array_dimension, Range):
            return False

        upper = array_dimension.stop

        if isinstance(upper, Literal):
            try:
                symbol = self.scope.symbol_table.lookup(self.name)
                if not isinstance(symbol, DataSymbol):
                    # We don't have any type information on this symbol
                    # (probably because it originates from a wildcard import).
                    return False
                datatype = symbol.datatype
                # Check that the symbol is of ArrayType. (It may be of
                # UnknownFortranType if the symbol is of e.g. character type.)
                if not isinstance(datatype, ArrayType):
                    return False
                shape = datatype.shape
                array_bounds = shape[index]
                if (isinstance(array_bounds, ArrayType.ArrayBounds) and
                        isinstance(array_bounds.upper, Literal)):
                    if upper.value == array_bounds.upper.value:
                        return True
            except (KeyError, SymbolError, AttributeError):
                # If any issue is found we can not guarantee that it is
                # the upper bound
                pass
            return False

        if not (isinstance(upper, BinaryOperation) and
                upper.operator == BinaryOperation.Operator.UBOUND):
            return False

        if not isinstance(upper.children[0], Reference):
            return False

        if not self.is_same_array(upper.children[0]):
            return False

        return (isinstance(upper.children[1], Literal) and
                upper.children[1].datatype.intrinsic ==
                ScalarType.Intrinsic.INTEGER
                and upper.children[1].value == str(index+1))

    def is_same_array(self, node):
        '''
        Checks that the provided array is the same as this node (including the
        chain of parent accessor expressions if the array is part of a
        Structure). If the array is part of a structure then any indices on
        the innermost member access are ignored, e.g.
        A(3)%B%C(1) will match with A(3)%B%C but not with A(2)%B%C(1)

        :param node: the node representing the access that is to be compared \
                     with this node.
        :type node: :py:class:`psyclone.psyir.nodes.Reference` or \
                    :py:class:`psyclone.psyir.nodes.Member`

        :returns: True if the array accesses match, False otherwise.
        :rtype: bool

        '''
        if not isinstance(node, (Member, Reference)):
            return False

        if isinstance(self, Member):
            # This node is somewhere within a structure access so we need to
            # get the parent Reference and keep a record of how deep this node
            # is within the structure access. e.g. if this node was the
            # StructureMember 'b' in a%c%b%d then its depth would be 2.
            depth = 1
            current = self
            while current.parent and not isinstance(current.parent, Reference):
                depth += 1
                current = current.parent
            parent_ref = current.parent
            if not parent_ref:
                return False
        else:
            depth = 0
            parent_ref = self

        # Now we have the parent Reference and the depth, we can construct the
        # Signatures and compare them to the required depth.
        self_sig, self_indices = parent_ref.get_signature_and_indices()
        node_sig, node_indices = node.get_signature_and_indices()
        if self_sig[:depth+1] != node_sig[:]:
            return False

        # Examine the indices, ignoring any on the innermost accesses (hence
        # the slice to `depth` rather than `depth + 1` below).
        for idx1, idx2 in zip(self_indices[:depth], node_indices[:depth]):
            if idx1 != idx2:
                return False
        return True

    def is_full_range(self, index):
        '''Returns True if the specified array index is a Range Node that
        specifies all elements in this index. In the PSyIR this is
        specified by using LBOUND(name,index) for the lower bound of
        the range, UBOUND(name,index) for the upper bound of the range
        and "1" for the range step.

        :param int index: the array index to check.

        :returns: True if the access to this array index is a range \
            that specifies all index elements. Otherwise returns \
            False.
        :rtype: bool

        '''
        self._validate_index(index)

        array_dimension = self.indices[index]
        if isinstance(array_dimension, Range):
            if self.is_lower_bound(index) and self.is_upper_bound(index):
                step = array_dimension.children[2]
                if (isinstance(step, Literal) and
                        step.datatype.intrinsic == ScalarType.Intrinsic.INTEGER
                        and str(step.value) == "1"):
                    return True
        return False

    @property
    def indices(self):
        '''
        Supports semantic-navigation by returning the list of nodes
        representing the index expressions for this array reference.

        :returns: the PSyIR nodes representing the array-index expressions.
        :rtype: list of :py:class:`psyclone.psyir.nodes.Node`

        :raises InternalError: if this node has no children or if they are \
                               not valid array-index expressions.

        '''
        if not self._children:
            raise InternalError(
                f"{type(self).__name__} malformed or incomplete: must have "
                f"one or more children representing array-index expressions "
                f"but array '{self.name}' has none.")
        for idx, child in enumerate(self._children):
            if not self._validate_child(idx, child):
                raise InternalError(
                    f"{type(self).__name__} malformed or incomplete: child "
                    f"{idx} of array '{self.name}' must be a psyir.nodes."
                    f"DataNode or Range representing an array-index "
                    f"expression but found '{type(child).__name__}'")
        return self.children

    def _get_effective_shape(self):
        '''
        :returns: the shape of the array access represented by this node.
        :rtype: List[:py:class:`psyclone.psyir.nodes.DataNode`]

        :raises NotImplementedError: if any of the array-indices involve a
                                     function call or an expression.
        '''
        def _num_elements(expr):
            '''
            Create PSyIR for the number of elements in this range. It
            is given by (stop - start)/step + 1.

            :param expr: the range for which to compute the number of elements.
            :type expr: :py:class:`psyclone.psyir.nodes.Range` or \
                :py:class:`psyclone.psyir.symbols.ArrayType.ArrayBounds`

            :returns: the PSyIR expression for the number of elements in the \
                      supplied range.
            :rtype: :py:class:`psyclone.psyir.nodes.BinaryOperation`

            '''
            if isinstance(expr, Range):
                start = expr.start
                stop = expr.stop
                step = expr.step
            elif isinstance(expr, ArrayType.ArrayBounds):
                start = expr.lower
                stop = expr.upper
                step = Literal("1", INTEGER_TYPE)
            minus = BinaryOperation.create(BinaryOperation.Operator.SUB,
                                           stop.copy(), start.copy())
            div = BinaryOperation.create(BinaryOperation.Operator.DIV,
                                         minus, step.copy())
            plus = BinaryOperation.create(BinaryOperation.Operator.ADD,
                                          div, Literal("1", INTEGER_TYPE))
            return plus

        shape = []
        for idx_expr in self.indices:
            if isinstance(idx_expr, Range):
                shape.append(_num_elements(idx_expr))

            elif isinstance(idx_expr, Reference):
                dtype = idx_expr.datatype
                if isinstance(dtype, ArrayType):
                    # An array slice can be defined by a 1D slice of another
                    # array, e.g. `a(b(1:4))`.
                    indirect_array_shape = dtype.shape
                    if len(indirect_array_shape) > 1:
                        raise InternalError(
                            f"An array defining a slice of a dimension of "
                            f"another array must be 1D but '{idx_expr.name}' "
                            f"used to index into '{self.name}' has "
                            f"{len(indirect_array_shape)} dimensions.")
                    shape.append(_num_elements(dtype.shape[0]))
            elif isinstance(idx_expr, (Call, Operation, CodeBlock)):
                # We can't yet straightforwardly query the type of a function
                # call or Operation - TODO #1799.
                raise NotImplementedError(
                    f"The array index expressions for access "
                    f"'{self.debug_string()}' include a function call or "
                    f"expression. Querying the return type of "
                    f"such things is yet to be implemented.")

        return shape

    def get_outer_range_index(self):
        ''' Return the index of the child that represents the outermost
        array dimension with a Range construct.

        :returns: the outermost index of the children that is a Range node.
        :rtype: int

        :raises IndexError: if the array does not contain a Range node.

        '''
        for child in reversed(self.indices):
            if isinstance(child, Range):
                return child.position
        raise IndexError


# For AutoAPI documentation generation
__all__ = ['ArrayMixin']
