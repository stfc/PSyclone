# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2021-2025, Science and Technology Facilities Council.
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
# Modified: A. B. G. Chalk, STFC Daresbury Lab
# -----------------------------------------------------------------------------

''' This module contains the implementation of the abstract ArrayMixin. '''

import abc
from typing import Tuple

from psyclone.core import SymbolicMaths
from psyclone.errors import InternalError
from psyclone.psyir.nodes.call import Call
from psyclone.psyir.nodes.codeblock import CodeBlock
from psyclone.psyir.nodes.datanode import DataNode
from psyclone.psyir.nodes.intrinsic_call import IntrinsicCall
from psyclone.psyir.nodes.literal import Literal
from psyclone.psyir.nodes.member import Member
from psyclone.psyir.nodes.node import Node
from psyclone.psyir.nodes.operation import Operation, BinaryOperation
from psyclone.psyir.nodes.ranges import Range
from psyclone.psyir.nodes.reference import Reference
from psyclone.psyir.symbols import DataSymbol, DataTypeSymbol
from psyclone.psyir.symbols.datatypes import (
    ScalarType, ArrayType, UnresolvedType, UnsupportedType, INTEGER_TYPE)


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

    def index_of(self, node):
        '''
        If the given node is one of the index expressions of the array, it
        returns the zero-indexed dimension of the array that it belongs to.
        Note that this is different to `node.position` because
        ArraysOfStructures have a Member child, and it is different from
        `array.indices.index(node)` because that would use the equality
        operator, but sibling indices may be equal and provide unexpected
        results.

        :param node: the node to get the index of.
        :type node: :py:class:`psyclone.psyir.nodes.Node`

        :returns: the index of the given node in the array.
        :rtype: int

        :raises ValueError: if node is not an index of the array.

        '''
        if node.parent is self:
            return node.position
        raise ValueError(f"'{node}' is not a child of '{self}'")

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
        # self.indices now returns a tuple, but we need to return a list here.
        return (sig, [list(self.indices)])

    def _validate_index(self, index):
        '''Utility function that checks that the supplied index is an integer
        and is less than the number of array dimensions.

        :param int index: the array index to check.

        :raises TypeError: if the index argument is not an integer.
        :raises ValueError: if the index value is greater than the
                            number of dimensions in the array (-1).

        '''
        if not isinstance(index, int):
            raise TypeError(
                f"The index argument should be an integer but found "
                f"'{type(index).__name__}'.")
        if index > len(self.indices)-1:
            raise ValueError(
                f"In '{type(self).__name__}' '{self.name}' the specified "
                f"index '{index}' must be less than the number of dimensions "
                f"'{len(self.indices)}'.")

    def _is_bound_op(self, expr, bound_operator, index):
        '''Utility function that checks that the provided 'expr' argument is
        in the form '[UL]BOUND(array_name, index)', where the type of
        bound operation is determined by the 'bound_operator'
        argument, array_name is the name of this array and the 'index'
        argument provides the index value.

        :param expr: a PSyIR expression.
        :type expr: :py:class:`psyclone.psyir.nodes.Node`
        :param bound_operator: the particular bound operation.
        :type bound_operator:
            :py:class:`psyclone.psyir.nodes.IntrinsicCall.Intrinsic.LBOUND` |
            :py:class:`psyclone.psyir.nodes.IntrinsicCall.Intrinsic.UBOUND`
        :param int index: the bounds index.

        :returns: True if the expr is in the expected form and False otherwise.
        :rtype: bool

        '''
        # pylint: disable=import-outside-toplevel
        from psyclone.psyir.nodes.structure_accessor_mixin import (
            StructureAccessorMixin)
        if (isinstance(expr, IntrinsicCall) and
                expr.intrinsic == bound_operator):
            array = expr.arguments[0]
            # If its a structure, we want to compare the whole accessor
            while isinstance(array, StructureAccessorMixin) and array.member:
                array = array.member
            # This is the expected bound
            if self.is_same_array(array):
                # The arrays match
                if (len(expr.arguments) > 1 and
                        isinstance(expr.arguments[1], Literal) and
                        expr.arguments[1].datatype.intrinsic ==
                        ScalarType.Intrinsic.INTEGER
                        and expr.arguments[1].value == str(index+1)):
                    # This is the correct index
                    return True
        return False

    def is_lower_bound(self, index):
        '''Returns whether this array access includes the lower bound of the
        array for the specified index. Returns True if it is and False
        if it is not or if it could not be determined.

        :param int index: the array index to check.

        :returns: True if it can be determined that the lower bound of \
            the array is accessed in this array reference for the \
            specified index.
        :rtype: bool

        '''
        return self._is_bound(index, "lower")

    def _get_bound_expression(self, pos: int, bound: str):
        '''
        Lookup the upper or lower bound of this ArrayMixin.

        :param pos: the dimension of the array for which to lookup the bound.
        :param bound: "upper" or "lower" - the bound which to lookup.

        :returns: the declared bound for the specified dimension of this array
                  or a call to the {U/L}BOUND intrinsic if it is unknown.
        :rtype: :py:class:`psyclone.psyir.nodes.Node`

        :raises InternalError: if bound is neither "upper" or "lower".

        '''
        if bound not in ("upper", "lower"):
            raise InternalError(f"'bound' argument must be 'lower' or 'upper. "
                                f"Found '{bound}'.")

        # First, walk up to the parent reference and get its type. For a simple
        # ArrayReference this will just be self.
        root_ref = self.ancestor(Reference, include_self=True)
        if isinstance(root_ref.symbol, DataSymbol):
            cursor_type = root_ref.symbol.datatype
        else:
            cursor_type = UnresolvedType()

        # Walk back down the structure, looking up the type information as we
        # go. We also collect the necessary information for creating a new
        # Reference as argument to the {U/L}BOUND intrinsic in case the type
        # information is not available.
        cnames = []
        cursor = root_ref
        while cursor is not self:
            cursor = cursor.member
            # Collect member information.
            if isinstance(cursor, ArrayMixin):
                new_indices = [idx.copy() for idx in cursor.indices]
                cnames.append((cursor.name.lower(), new_indices))
            else:
                cnames.append(cursor.name.lower())
            # Continue to resolve datatype unless we hit an
            # UnsupportedType or UnresolvedType.
            if isinstance(cursor_type, ArrayType):
                cursor_type = cursor_type.intrinsic
            if isinstance(cursor_type, DataTypeSymbol):
                cursor_type = cursor_type.datatype
            if isinstance(cursor_type, (UnsupportedType, UnresolvedType)):
                continue
            cursor_type = cursor_type.components[cursor.name.lower()].datatype

        if (isinstance(cursor_type, ArrayType) and
                cursor_type.shape[pos] not in [ArrayType.Extent.DEFERRED,
                                               ArrayType.Extent.ATTRIBUTE]):
            # We have the full type information and the bound is known.
            if bound == "lower":
                return cursor_type.shape[pos].lower.copy()
            # If the upper bound is required and is of ArrayType.Extent type
            # then we'll have to proceed to construct a call to the UBOUND
            # intrinsic.
            if not isinstance(cursor_type.shape[pos].upper, ArrayType.Extent):
                return cursor_type.shape[pos].upper.copy()

        # We've either failed to resolve the type or we don't know the extent
        # of the array dimension so construct a call to the BOUND intrinsic.
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

        if bound == "lower":
            return IntrinsicCall.create(
                IntrinsicCall.Intrinsic.LBOUND,
                [ref, ("dim", Literal(str(pos+1), INTEGER_TYPE))])
        return IntrinsicCall.create(
                IntrinsicCall.Intrinsic.UBOUND,
                [ref, ("dim", Literal(str(pos+1), INTEGER_TYPE))])

    def get_lbound_expression(self, pos):
        '''
        Lookup the lower bound of this ArrayMixin. If we don't have the
        necessary type information then a call to the LBOUND intrinsic is
        constructed and returned.

        :param int pos: the dimension of the array for which to lookup the
                        lower bound.

        :returns: the declared lower bound for the specified dimension of
            the array accesed or a call to the LBOUND intrinsic if it is
            unknown.
        :rtype: :py:class:`psyclone.psyir.nodes.Node`

        '''
        self._validate_index(pos)
        # Call the helper function
        return self._get_bound_expression(pos, "lower")

    def get_ubound_expression(self, pos):
        '''
        Lookup the upper bound of this ArrayMixin. If we don't have the
        necessary type information then a call to the UBOUND intrinsic is
        constructed and returned.

        :param int pos: the dimension of the array for which to lookup the
                        upper bound.

        :returns: the declared upper bound for the specified dimension of
            the array accesed or a call to the UBOUND intrinsic if it is
            unknown.
        :rtype: :py:class:`psyclone.psyir.nodes.Node`

        '''
        self._validate_index(pos)
        # Call the helper function
        return self._get_bound_expression(pos, "upper")

    def get_full_range(self, pos):
        '''
        Returns a Range object that covers the full indexing of the dimension
        specified by pos for this ArrayMixin object.

        :param int pos: the dimension of the array for which to lookup the
                        upper bound.

        :returns: A Range representing the full range for the dimension of
                  pos for this ArrayMixin.
        :rtype: :py:class:`psyclone.psyir.nodes.Range`
        '''
        self._validate_index(pos)

        lbound = self.get_lbound_expression(pos)
        ubound = self.get_ubound_expression(pos)

        return Range.create(lbound, ubound)

    def is_upper_bound(self, index):
        '''Returns whether this array access includes the upper bound of
        the array for the specified index. Returns True if it is and False
        if it is not or if it could not be determined.

        :param int index: the array index to check.

        :returns: True if it can be determined that the upper bound of \
            the array is accessed in this array reference for the \
            specified index.
        :rtype: bool

        '''
        return self._is_bound(index, "upper")

    def _is_bound(self, index, bound_type):
        '''Attempts to determines whether this array access includes the lower
        or upper bound (as specified by the bound_type argument).

        Checks whether the specified array index contains a Range node
        which has a starting/stopping value given by the
        '{LU}BOUND(name,index)' intrinsic where 'name' is the name of
        the current ArrayReference and 'index' matches the specified
        array index. Also checks if the starting/stopping value of the
        access matches the lower/upper value of the declaration.

        For example, if a Fortran array A was declared as A(n) then
        the stopping value is n and A(:UBOUND(A,1)), A(:n) or A(n)
        would access that value. The starting value is 1 and
        A(LBOUND(A,1):), A(1:) or A(1) would access that value.

        :param int index: the array index to check.
        :param str bound_type: the type of bound ("lower" or "upper")

        :returns: True if the array index access includes the \
            lower/upper bound of the array declaration and False if it \
            does not or if it can't be determined.
        :rtype: bool

        '''
        self._validate_index(index)

        access_shape = self.indices[index]

        # Determine the appropriate (lower or upper) bound and check
        # for a bounds intrinsic.
        if isinstance(access_shape, Range):
            if bound_type == "upper":
                intrinsic = IntrinsicCall.Intrinsic.UBOUND
                access_bound = access_shape.stop
            else:
                intrinsic = IntrinsicCall.Intrinsic.LBOUND
                access_bound = access_shape.start

            # Is this array access in the form of {UL}BOUND(array, index)?
            if self._is_bound_op(access_bound, intrinsic, index):
                return True
        else:
            access_bound = access_shape

        # Try to compare the upper/lower bound of the array access
        # with the upper/lower bound of the array declaration.

        # Finding the array declaration is only supported for an
        # ArrayReference at the moment.
        # Import here to avoid circular dependence.
        # pylint: disable=import-outside-toplevel
        from psyclone.psyir.nodes.array_reference import ArrayReference
        if not isinstance(self, ArrayReference):
            return False
        # pylint: enable=import-outside-toplevel

        symbol = self.symbol
        if not isinstance(symbol, DataSymbol):
            # There is no type information for this symbol
            # (probably because it originates from a wildcard import).
            return False
        datatype = symbol.datatype

        if not isinstance(datatype, ArrayType):
            # The declaration datatype could be of UnsupportedFortranType
            # if the symbol is of e.g. character type.
            return False

        # The bound of the declaration is available.

        if isinstance(datatype.shape[index], ArrayType.Extent):
            # The size is unspecified at compile-time (but is
            # available at run-time e.g. when the size is allocated by
            # an allocate statement).
            return False

        # The size of the bound is available.
        if bound_type == "upper":
            declaration_bound = datatype.shape[index].upper
            if isinstance(declaration_bound, ArrayType.Extent):
                # But only at run-time.
                return False
        else:
            declaration_bound = datatype.shape[index].lower

        # Do the bounds match?
        sym_maths = SymbolicMaths.get()
        return sym_maths.equal(declaration_bound, access_bound)

    def is_same_array(self, node) -> bool:
        '''
        Checks that the provided array is the same as this node (including the
        chain of parent accessor expressions if the array is part of a
        Structure). If the array is part of a structure then any indices on
        the innermost member access are ignored, e.g.
        A(3)%B%C(1) will match with A(3)%B%C but not with A(2)%B%C(1)

        :param node: the node representing the access that is to be compared
                     with this node.
        :type node: Union[:py:class:`psyclone.psyir.nodes.Reference`,
                          :py:class:`psyclone.psyir.nodes.Member`]

        :returns: True if the array accesses match, False otherwise.

        '''
        if not isinstance(node, (Member, Reference)):
            return False

        # First check that the base and depths are the same
        if isinstance(self, Member):
            self_base, depth = self.get_base_and_depth()
        else:
            self_base, depth = self, 0
        if isinstance(node, Member):
            node_base, node_depth = node.get_base_and_depth()
        else:
            node_base, node_depth = node, 0
        if (not isinstance(self_base, Reference) or
                not isinstance(node_base, Reference)):
            return False
        if self_base.symbol != node_base.symbol or depth != node_depth:
            return False

        # Then use the signatures to compare that each member until
        # depth are also the same
        self_sig, self_indices = self_base.get_signature_and_indices()
        node_sig, node_indices = node_base.get_signature_and_indices()
        if self_sig[:depth+1] != node_sig[:depth+1]:
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
    def indices(self) -> Tuple[Node]:
        '''
        Supports semantic-navigation by returning the list of nodes
        representing the index expressions for this array reference.

        :returns: the PSyIR nodes representing the array-index expressions.

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
        return tuple(self.children)

    def _extent(self, idx):
        '''
        Create PSyIR for the number of elements in dimension `idx` of this
        array access. It is given by (stop - start)/step + 1 or, if it is for
        the full range, by the SIZE intrinsic.

        :param int idx: the array index for which to compute the number of
                        elements.

        :returns: the PSyIR expression for the number of elements in the
                  specified array index.
        :rtype: :py:class:`psyclone.psyir.nodes.BinaryOperation` |
                :py:class:`psyclone.psyir.nodes.IntrinsicCall`
        '''
        expr = self.indices[idx]
        one = Literal("1", INTEGER_TYPE)

        if isinstance(expr, Range):
            start = expr.start
            stop = expr.stop
            step = expr.step
        else:
            # No range so just a single element is accessed.
            return one

        if (isinstance(start, IntrinsicCall) and
                isinstance(stop, IntrinsicCall) and self.is_full_range(idx)):
            # Access is to full range and start and stop are expressed in terms
            # of LBOUND and UBOUND. Therefore, it's simpler to use SIZE.
            return IntrinsicCall.create(
                IntrinsicCall.Intrinsic.SIZE,
                [start.arguments[0].copy(),
                 ("dim", Literal(str(idx+1), INTEGER_TYPE))])

        if start == one and step == one:
            # The range starts at 1 and the step is 1 so the extent is just
            # the upper bound.
            return stop.copy()

        extent = BinaryOperation.create(BinaryOperation.Operator.SUB,
                                        stop.copy(), start.copy())
        if step != one:
            # Step is not unity so have to divide range by it.
            result = BinaryOperation.create(BinaryOperation.Operator.DIV,
                                            extent, step.copy())
        else:
            result = extent
        # Extent is currently 'stop-start' or '(stop-start)/step' so we have
        # to add a '+ 1'
        return BinaryOperation.create(BinaryOperation.Operator.ADD,
                                      result, one.copy())

    def _get_effective_shape(self):
        '''
        :returns: the shape of the array access represented by this node.
        :rtype: list[:py:class:`psyclone.psyir.nodes.DataNode`]

        :raises NotImplementedError: if any of the array-indices involve a
                                     function call or are of unknown type.
        '''
        shape = []
        for idx, idx_expr in enumerate(self.indices):
            if isinstance(idx_expr, Range):
                shape.append(self._extent(idx))

            elif isinstance(idx_expr, (Reference, Operation)):
                dtype = idx_expr.datatype
                if isinstance(dtype, ArrayType):
                    # An array slice can be defined by a 1D slice of another
                    # array, e.g. `a(b(1:4))` or `a(b)`.
                    indirect_array_shape = dtype.shape
                    if len(indirect_array_shape) > 1:
                        raise NotImplementedError(
                            f"An array defining a slice of a dimension of "
                            f"another array must be 1D but '{idx_expr.name}' "
                            f"used to index into '{self.name}' has "
                            f"{len(indirect_array_shape)} dimensions.")
                    # pylint: disable=protected-access
                    if isinstance(idx_expr, ArrayMixin):
                        shape.append(idx_expr._extent(idx))
                    else:
                        # We have some expression with a shape but no explicit
                        # indexing. The extent of this is then the SIZE of
                        # that array (expression).
                        sizeop = IntrinsicCall.create(
                            IntrinsicCall.Intrinsic.SIZE, [idx_expr.copy()])
                        shape.append(sizeop)
                elif isinstance(dtype, (UnsupportedType, UnresolvedType)):
                    raise NotImplementedError(
                        f"The array index expression "
                        f"'{idx_expr.debug_string()}' in access "
                        f"'{self.debug_string()}' is of '{dtype}' type and "
                        f"therefore whether it is an array slice (i.e. an "
                        f"indirect access) cannot be determined.")
            elif isinstance(idx_expr, (Call, CodeBlock)):
                # We can't yet straightforwardly query the type of a function
                # call - TODO #1799.
                raise NotImplementedError(
                    f"The array index expressions for access "
                    f"'{self.debug_string()}' include a function call or "
                    f"unsupported feature. Querying the return type of "
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

    def same_range(self, index: int, array2, index2: int) -> bool:
        ''' This method compares the range of this array node at a given index
        with the range of a second array at a second index. This is useful to
        verify if array operations are valid, e.g.: A(3,:,5) + B(:,2,2).

        Note that this check supports symbolic comparisons, e.g.:
        A(3:4) has the same range as B(2+1:5-1),
        and will consider compile-time unknown dimensions as equal, e.g.:
        A(:) has the same range as B(:).

        TODO #2485. This method has false negatives: cases when the range
        is the same but it can not be proved, so we return False.

        TODO #2004. This method currently compares exact ranges, not just the
        length of them, which could be done with "(upper-lower)/step" symbolic
        comparisons. This is because arrayrange2loop does not account for
        arrays declared with different lbounds, but this could be improved.

        :param index: the index indicating the location of a range node in
            this array.
        :param array2: the array accessor that we want to compare it to.
        :param index2: the index indicating the location of a range node in
            array2.

        :returns: True if the ranges are the same and False if they are not
            the same, or if it is not possible to determine.

        :raises: TypeError if any of the arguments are of the wrong type.

        '''
        # pylint: disable=too-many-branches
        if not isinstance(index, int):
            raise TypeError(
                f"The 'index' argument of the same_range() method should be an"
                f" int but found '{type(index).__name__}'.")
        if not isinstance(array2, ArrayMixin):
            raise TypeError(
                f"The 'array2' argument of the same_range() method should be "
                f"an ArrayMixin but found '{type(array2).__name__}'.")
        if not isinstance(index2, int):
            raise TypeError(
                f"The 'index2' argument of the same_range() method should be "
                f"an int but found '{type(index2).__name__}'.")
        if not index < len(self.indices):
            raise IndexError(
                f"The value of the 'index' argument of the same_range() method"
                f" is '{index}', but it should be less than the number of "
                f"dimensions in the associated array, which is "
                f"'{len(self.indices)}'.")
        if not index2 < len(array2.indices):
            raise IndexError(
                f"The value of the 'index2' argument of the same_range() "
                f"method is '{index2}', but it should be less than the number"
                f" of dimensions in the associated array 'array2', which is "
                f"'{len(array2.indices)}'.")
        if not isinstance(self.indices[index], Range):
            raise TypeError(
                f"The child of the first array argument at the specified index"
                f" '{index}' should be a Range node, but found "
                f"'{type(self.indices[index]).__name__}'.")
        if not isinstance(array2.indices[index2], Range):
            raise TypeError(
                f"The child of the second array argument at the specified "
                f"index '{index2}' should be a Range node, but found "
                f"'{type(array2.indices[index2]).__name__}'.")

        # pylint: disable=import-outside-toplevel
        from psyclone.psyir.nodes.assignment import Assignment
        range1 = self.indices[index]
        range2 = array2.indices[index2]

        sym_maths = SymbolicMaths.get()
        # The logic below assumes array expressions come from valid Fortran,
        # (e.g. a(2:4) = b(2:5) is not valid Fortran)
        # and therefore, we assume the length of the equivalent ranges in the
        # same statement matches:
        # e.g. a(3, :) and b(4:, 4)
        # a dim 2 and b dim 1 must have the same length, but not necessarily
        # the same range (lower and upper bounds)
        n_range1 = len([x for x in self.children[:index]
                        if isinstance(x, Range)])
        n_range2 = len([x for x in array2.children[:index2]
                        if isinstance(x, Range)])
        assume_same_length = (
            (self.ancestor(Assignment) is array2.ancestor(Assignment) or
             self.is_same_array(array2)) and n_range1 == n_range2
        )

        # Try to get the datatypes, only if they are ArrayType (so we know
        # that these have the shape attribute)
        array1_type = None
        if (isinstance(self, Reference) and isinstance(self.symbol, DataSymbol)
                and isinstance(self.symbol.datatype, ArrayType)):
            array1_type = self.symbol.datatype
        array2_type = None
        if (isinstance(array2, Reference) and
                isinstance(array2.symbol, DataSymbol) and
                isinstance(array2.symbol.datatype, ArrayType)):
            array2_type = array2.symbol.datatype

        # Try to get the ranges start values
        range1_start = None
        range2_start = None
        # If we have a implicit lower bound, e.g. a(:) = b(:)
        # we need to prove that they have the same lower bound value on the
        # declaration. For example
        #   integer, dimension(1:3) :: a
        #   integer, dimension(3:5) :: b
        # would make it "not equal".
        if self.is_lower_bound(index):
            if self.is_same_array(array2) and array2.is_lower_bound(index2):
                return True
            if not array1_type:
                return False
            if array1_type.shape[index] == ArrayType.Extent.DEFERRED:
                return False
            if array1_type.shape[index] == ArrayType.Extent.ATTRIBUTE:
                range1_start = Literal("1", INTEGER_TYPE)
            else:
                range1_start = array1_type.shape[index].lower

        if array2.is_lower_bound(index2):
            if not array2_type:
                return False
            if array2_type.shape[index2] == ArrayType.Extent.DEFERRED:
                return False
            if array2_type.shape[index2] == ArrayType.Extent.ATTRIBUTE:
                range2_start = Literal("1", INTEGER_TYPE)
            else:
                range2_start = array2_type.shape[index2].lower

        # If the previous block didn't populate the start value, it's explicit
        if not range1_start:
            range1_start = range1.start
        if not range2_start:
            range2_start = range2.start

        # Now we can do a symbolic comparison of the start values
        if not sym_maths.equal(range1_start, range2_start):
            return False

        # If we can not guarantee the same length, we also need to check
        # the upper bound
        if not assume_same_length:
            if self.is_upper_bound(index):
                if array1_type:
                    range1_stop = array1_type.shape[index].upper
                else:
                    return False
            else:
                range1_stop = range1.stop

            if array2.is_upper_bound(index2):
                if array2_type:
                    range2_stop = array2_type.shape[index2].upper
                else:
                    return False
            else:
                range2_stop = range2.stop

            if not sym_maths.equal(range1_stop, range2_stop):
                return False

        # Compare steps
        if not sym_maths.equal(range1.step, range2.step):
            return False

        # Everything matches.
        return True


# For AutoAPI documentation generation
__all__ = ['ArrayMixin']
