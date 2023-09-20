# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2023, Science and Technology Facilities Council
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
# Author: R. W. Ford, STFC Daresbury Lab
# Modified: S. Siso, STFC Daresbury Lab

'''Module providing common functionality to transformation from a
PSyIR SUM, MINVAL or MAXVAL intrinsic to PSyIR code.

'''
from abc import ABC, abstractmethod

from psyclone.psyir.nodes import (
    Assignment, Reference, Literal, Loop, ArrayReference, IfBlock, Range,
    IntrinsicCall)
from psyclone.psyir.symbols import (
    DataSymbol, INTEGER_TYPE, ScalarType, ArrayType)
from psyclone.psyGen import Transformation
from psyclone.psyir.transformations.transformation_error import \
    TransformationError


class MMSBaseTrans(Transformation, ABC):
    '''An abstract parent class providing common functionality to the
    sum2code_trans, minval2code_trans and maxval2_code trans
    transformations.

    '''
    _INTRINSIC_NAME = None

    @staticmethod
    def _get_args(node):
        '''Utility method that returns the minval, maxval or sum arguments,
        (array reference, dimension and mask).

        :param node: a minval, maxval or sum intrinsic.
        :type node: :py:class:`psyclone.psyir.nodes.IntrinsicCall`

        returns: a tuple containing the 3 arguments.
        rtype: Tuple[py:class:`psyclone.psyir.nodes.reference.Reference`,
            py:class:`psyclone.psyir.nodes.Literal` |
            :py:class:`psyclone.psyir.nodes.Reference`,
            Optional[:py:class:`psyclone.psyir.nodes.Node`]]

        '''
        # Determine the arguments to the intrinsic
        args = [None, None, None]
        arg_names_map = {"array": 0, "dim": 1, "mask": 2}
        for idx, child in enumerate(node.children):
            if not node.argument_names[idx]:
                # positional arg
                args[idx] = child
            else:
                # named arg
                name = node.argument_names[idx].lower()
                args[arg_names_map[name]] = child
        return tuple(args)

    def __str__(self):
        return (f"Convert the PSyIR {self._INTRINSIC_NAME} intrinsic "
                "to equivalent PSyIR code.")

    # pylint: disable=too-many-branches
    def validate(self, node, options=None):
        '''Check that the input node is valid before applying the
        transformation.

        :param node: a Sum, Minval or Maxval intrinsic.
        :type node: :py:class:`psyclone.psyir.nodes.IntrinsicCall`
        :param options: options for the transformation.
        :type options: Optional[Dict[str, Any]]

        :raises TransformationError: if the supplied node is not an
            intrinsic.
        :raises TransformationError: if the supplied node is not a sum,
            minval, or maxval intrinsic.
        :raises TransformationError: if a valid value for the
            dimension argument can't be determined.
        :raises TransformationError: if the array argument is not an array.
        :raises TransformationError: if the shape of the array is not
            supported.
        :raises TransformationError: if the array datatype is not
            supported.
        :raises TransformationError: if the intrinsic is not part of
            an assignment.

        '''
        if not isinstance(node, IntrinsicCall):
            raise TransformationError(
                f"Error in {self.name} transformation. The supplied node "
                f"argument is not an intrinsic, found "
                f"'{type(node).__name__}'.")

        if node.routine.name.upper() != self._INTRINSIC_NAME:
            raise TransformationError(
                f"Error in {self.name} transformation. The supplied node "
                f"argument is not a {self._INTRINSIC_NAME.lower()} "
                f"intrinsic, found '{node.routine.name}'.")

        array_ref, dim_ref, _ = self._get_args(node)

        # If there is a dim argument then PSyclone curently needs to
        # be able to determine the literal value.
        # pylint: disable=unidiomatic-typecheck
        if dim_ref and not (
                isinstance(dim_ref, Literal)
                or (type(dim_ref) is Reference and
                    dim_ref.symbol.is_constant and
                    isinstance(dim_ref.symbol.initial_value, Literal))):
            if isinstance(dim_ref, Reference):
                info = f"a reference to a '{type(dim_ref.symbol).__name__}'"
            else:
                info = f"type '{type(dim_ref).__name__}'"
            raise TransformationError(
                f"Can't find the value of the 'dim' argument to the "
                f"{self._INTRINSIC_NAME} intrinsic. Expected "
                f"it to be a literal or a reference to a known constant "
                f"value, but found '{dim_ref.debug_string()}' which is "
                f"{info}.")

        # pylint: disable=unidiomatic-typecheck
        if not (isinstance(array_ref, ArrayReference) or
                (type(array_ref) is Reference)):
            raise TransformationError(
                f"{self.name} only supports arrays or plain references for "
                f"the first argument, but found '{type(array_ref).__name__}'.")

        if len(array_ref.children) == 0:
            if not array_ref.symbol.is_array:
                raise TransformationError(
                    f"Expected '{array_ref.name}' to be an array.")
            for shape in array_ref.symbol.shape:
                if not (shape in [
                        ArrayType.Extent.DEFERRED, ArrayType.Extent.ATTRIBUTE]
                        or isinstance(shape, ArrayType.ArrayBounds)):
                    raise TransformationError(
                        f"Unexpected shape for array. Expecting one of "
                        f"Deferred, Attribute or Bounds but found '{shape}'.")

        for shape in array_ref.children:
            if not isinstance(shape, Range):
                raise TransformationError(
                    f"{self.name} only supports arrays with array ranges, "
                    f"but found a fixed dimension in "
                    f"'{array_ref.debug_string()}'.")

        array_intrinsic = array_ref.symbol.datatype.intrinsic
        if array_intrinsic not in [ScalarType.Intrinsic.REAL,
                                   ScalarType.Intrinsic.INTEGER]:
            raise TransformationError(
                f"Only real and integer types supported for array "
                f"'{array_ref.name}', but found '{array_intrinsic.name}'.")

        if not node.ancestor(Assignment):
            raise TransformationError(
                f"{self.name} only works when the intrinsic is part "
                f"of an Assignment.")

    # pylint: disable=too-many-locals
    # pylint: disable=too-many-branches
    # pylint: disable=too-many-statements
    def apply(self, node, options=None):
        '''Apply the SUM, MINVAL or MAXVAL intrinsic conversion transformation
        to the specified node. This node must be one of these
        intrinsic operations which is converted to equivalent inline
        code.

        :param node: a Sum, Minval or Maxval intrinsic.
        :type node: :py:class:`psyclone.psyir.nodes.IntrinsicCall`
        :param options: options for the transformation.
        :type options: Optional[Dict[str, Any]]

        '''
        self.validate(node)

        array_ref, dimension_ref, mask_ref = self._get_args(node)

        # Determine the literal value of the dimension argument
        dimension_literal = None
        # pylint: disable=unidiomatic-typecheck
        if not dimension_ref:
            # there is no dimension argument
            pass
        elif isinstance(dimension_ref, Literal):
            dimension_literal = dimension_ref
        elif ((type(dimension_ref) is Reference) and
              dimension_ref.symbol.is_constant):
            dimension_literal = dimension_ref.symbol.initial_value
        # else exception is handled by the validate method.

        # Determine the dimension and extent of the array
        ndims = None
        allocatable = False
        if len(array_ref.children) == 0:
            # There is no bounds information in the array reference,
            # so look at the declaration.
            # Note, the potential 'if not array_ref.symbol.is_array:'
            # exception is already handled by the validate method.
            ndims = len(array_ref.datatype.shape)

            loop_bounds = []
            for idx, shape in enumerate(array_ref.datatype.shape):
                if shape in [ArrayType.Extent.DEFERRED,
                             ArrayType.Extent.ATTRIBUTE]:
                    if shape == ArrayType.Extent.DEFERRED:
                        allocatable = True
                    # runtime extent using LBOUND and UBOUND required
                    lbound = IntrinsicCall.create(
                        IntrinsicCall.Intrinsic.LBOUND,
                        [Reference(array_ref.symbol),
                         ("dim", Literal(str(idx+1), INTEGER_TYPE))])
                    ubound = IntrinsicCall.create(
                        IntrinsicCall.Intrinsic.UBOUND,
                        [Reference(array_ref.symbol),
                         ("dim", Literal(str(idx+1), INTEGER_TYPE))])
                    loop_bounds.append((lbound, ubound))
                elif isinstance(shape, ArrayType.ArrayBounds):
                    # array extent is defined in the array declaration
                    loop_bounds.append(shape)
                #  Note, the validate method guarantees that an else
                #  clause is not required.
        else:
            # The validate method guarantees that this is an array
            # reference.
            loop_bounds = []
            ndims = len(array_ref.children)
            for shape in array_ref.children:
                loop_bounds.append((shape.start.copy(), shape.stop.copy()))

        # Determine the datatype of the array's values and create a
        # scalar of that type
        array_intrinsic = array_ref.datatype.intrinsic
        array_precision = array_ref.datatype.precision
        scalar_type = ScalarType(array_intrinsic, array_precision)

        symbol_table = node.scope.symbol_table
        assignment = node.ancestor(Assignment)

        datatype = scalar_type
        array_reduction = False
        if dimension_ref and ndims > 1:
            array_reduction = True
            # We are reducing from one array to another
            shape = []
            for idx, bounds in enumerate(loop_bounds):
                # The validation constrains the transformation to only
                # allow cases where the literal value for dimension
                # is known.
                if int(dimension_literal.value)-1 == idx:
                    # This is the dimension we are performing the
                    # reduction over so do not loop over it.
                    pass
                else:
                    shape.append(bounds)
            if allocatable:
                # Reduction and allocatable means we need to make the
                # reduction array allocatable. We keep the bounds in
                # datatype_keep for allocating the array.
                datatype = ArrayType(
                    scalar_type, len(shape)*[ArrayType.Extent.DEFERRED])
            else:
                datatype = ArrayType(scalar_type, shape)

        # Detach the intrinsics array-reference argument to the
        # intrinsic as it will be used later within a loop nest.
        array_ref = node.children[0].detach()

        # Create temporary variable based on the name of the intrinsic.
        var_symbol = symbol_table.new_symbol(
            f"{self._INTRINSIC_NAME.lower()}_var", symbol_type=DataSymbol,
            datatype=datatype)
        # Replace operation with a temporary variable.
        if array_reduction:
            # This is a reduction so the number of array dimensions is
            # reduced by 1 cf. the original array.
            array_indices = (ndims - 1)*[":"]
            reference = ArrayReference.create(var_symbol, array_indices)
        else:
            reference = Reference(var_symbol)
        node.replace_with(reference)

        if allocatable and array_reduction:
            range_list = [
                Range.create(lbound, ubound) for (lbound, ubound) in shape]
            # Allocate the reduction and place it just before it is
            # initialised.
            allocate = IntrinsicCall.create(
                IntrinsicCall.Intrinsic.ALLOCATE,
                [ArrayReference.create(var_symbol, range_list)])
            assignment = reference.parent
            assignment.parent.children.insert(assignment.position, allocate)

        # Create the loop iterators
        loop_iterators = []
        array_iterators = []
        for idx in range(ndims):
            loop_iterator = symbol_table.new_symbol(
                f"i_{idx}", symbol_type=DataSymbol, datatype=INTEGER_TYPE)
            loop_iterators.append(loop_iterator)
            if array_reduction and idx != int(dimension_literal.value)-1:
                array_iterators.append(loop_iterator)

        # Initialise the temporary variable.
        rhs = self._init_var(var_symbol)
        lhs = reference.copy()
        new_assignment = Assignment.create(lhs, rhs)
        assignment.parent.children.insert(assignment.position, new_assignment)

        array_indices = []
        for idx in range(ndims):
            array_indices.append(Reference(loop_iterators[idx]))
        array_ref = ArrayReference.create(array_ref.symbol, array_indices)

        statement = self._loop_body(
            array_reduction, array_iterators, var_symbol, array_ref)

        if mask_ref:
            # A mask argument has been provided
            for ref in mask_ref.walk(Reference):
                # pylint: disable=unidiomatic-typecheck
                if ref.name == array_ref.name and type(ref) is Reference:
                    # The array is not indexed so it needs indexing
                    # for the loop nest.
                    shape = [Reference(obj) for obj in loop_iterators]
                    reference = ArrayReference.create(ref.symbol, shape)
                    ref.replace_with(reference)
            statement = IfBlock.create(mask_ref.detach(), [statement])

        for idx in range(ndims):
            statement = Loop.create(
                loop_iterators[idx].copy(), loop_bounds[idx][0].copy(),
                loop_bounds[idx][1].copy(), Literal("1", INTEGER_TYPE),
                [statement])

        assignment.parent.children.insert(assignment.position, statement)

    @abstractmethod
    def _loop_body(
            self, array_reduction, array_iterators, var_symbol, array_ref):
        '''The intrinsic-specific content of the created loop body.'''

    @abstractmethod
    def _init_var(self, var_symbol):
        '''The intrinsic-specific initial value for the temporary variable.

        '''
