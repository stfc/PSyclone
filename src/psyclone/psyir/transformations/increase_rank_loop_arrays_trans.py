# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2025-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

'''This module contains the IncreaseRankLoopArrays transformation.'''

from typing import Optional, Union

from psyclone.psyGen import Transformation
from psyclone.psyir.nodes import (
    ArrayReference, Call, CodeBlock, Loop, Range, Reference,
    Routine, Statement, IntrinsicCall, Assignment)
from psyclone.psyir.symbols import (
    ArrayType, DataSymbol, DataTypeSymbol, ScalarType, UnresolvedType)
from psyclone.psyir.transformations.transformation_error import (
    TransformationError)
from psyclone.utils import transformation_documentation_wrapper


@transformation_documentation_wrapper
class IncreaseRankLoopArraysTrans(Transformation):
    ''' This transformation takes a loop and a list of arrays accessed inside
    the loop, and increases those arrays with an additional dimension with the
    size of the iteration space. Then it indexes all accesses with the loop
    variable, so that each iteration accesses a unique location. Effectively
    making the sub-array private for each iteration of the loop. It also
    indexes assignments outside the loop to iterate over the whole new rank.

    >>> from psyclone.psyir.backend.fortran import FortranWriter
    >>> from psyclone.psyir.frontend.fortran import FortranReader
    >>> from psyclone.psyir.nodes import Loop
    >>> from psyclone.psyir.transformations import IncreaseRankLoopArraysTrans
    >>> code = ("""
    ... program test
    ...     integer :: N=10, M=10
    ...     integer :: i, j
    ...     real, dimension(N) :: ztmp
    ...     ! (Array) Assignments to the target variable outside the loop are
    ...     ! permitted, but any other use will not pass the validation
    ...     ztmp = 0
    ...     ztmp(0) = 1
    ...     do i = -5, M + 3
    ...         do j = 1, N
    ...             ztmp(j) = 1
    ...         end do
    ...         do j = 1, N
    ...             ztmp(j) = ztmp(j) + 1
    ...         end do
    ...     end do
    ... end program
    ... """)
    >>> psyir = FortranReader().psyir_from_source(code)
    >>> irla = IncreaseRankLoopArraysTrans()
    >>> irla.apply(psyir.walk(Loop)[0], arrays=['ztmp'])
    >>> print(FortranWriter()(psyir))
    program test
      integer, save :: n = 10
      integer, save :: m = 10
      integer :: i
      integer :: j
      real, dimension(n,-5:m + 3) :: ztmp
    <BLANKLINE>
      ztmp = 0
      ztmp(0,:) = 1
      do i = -5, m + 3, 1
        do j = 1, n, 1
          ztmp(j,i) = 1
        enddo
        do j = 1, n, 1
          ztmp(j,i) = ztmp(j,i) + 1
        enddo
      enddo
    <BLANKLINE>
    end program test
    <BLANKLINE>

    '''
    def __str__(self):
        return ("Increases the Rank of the supplied arrays by the iteration "
                "space of the given loop, and update all references to those"
                " arrays.")

    def validate(
        self,
        node: Loop,
        **kwargs
    ):
        ''' Checks that the supplied node is a valid target.

        :param node: target Loop node.

        :raises TransformationError: if the node is not a Loop.
        :raises TransformationError: if the node is not inside a Routine.
        :raises TransformationError: if the target node does not have static
            loop bounds (which the dimension declaration needs).
        :raises TypeError: if no list of array names is provided.
        :raises TransformationError: the given array name or the
            symbol is not local or not an array.
        :raises TransformationError: if any of the arrays are referenced inside
            a CodeBlock.
        :raises TransformationError: if any of the arrays are referenced
            outside the loop in anything other than (array) assignments to
            their variable.
        :raises TransformationError: if an array is allocatable, a character
            array, or a function return value.
        :raises TransformationError: if an array is used without indices,
            already indexed by the loop variable, or assigned a whole-array
            value from a call.

        '''
        super().validate(node, **kwargs)
        self.validate_options(**kwargs)
        arrays = self.get_option('arrays', **kwargs)

        if not isinstance(arrays, list) or len(arrays) == 0:
            raise TypeError(
                f"{self.name} has a mandatory 'arrays' option which specifies"
                f" which arrays are to have their rank increased.")

        if not isinstance(node, Loop):
            raise TransformationError(
                f"The target of the {self.name} transformation should be a "
                f"Loop, but found '{type(node).__name__}'.")
        target_loop: Loop = node

        routine: Routine = target_loop.ancestor(Routine)
        if routine is None:
            raise TransformationError(
                f"The target Loop of the {self.name} transformation must be "
                f"inside a Routine.")

        # Convert to list of symbols, if provided as strings
        arrays_symbols: list[DataSymbol] = []
        for sym_or_name in arrays:
            if isinstance(sym_or_name, DataSymbol):
                sym = sym_or_name
                arrays_symbols.append(sym)

            elif isinstance(sym_or_name, str):
                try:
                    sym = target_loop.scope.symbol_table.lookup(sym_or_name)
                except KeyError as err:
                    raise TransformationError(
                        f"{self.name} provided array '{sym_or_name}' does not "
                        f"exist in this scope."
                    ) from err
                arrays_symbols.append(sym)

            else:
                raise TransformationError(
                    f"{self.name} must be passed a list of symbol names or "
                    f"Symbol objects but found {type(sym_or_name).__name__}"
                )

        # Check if the loop bound expressions are static:
        # First, find all Symbols accessed in the loop bounds.
        values_to_check = set()
        for ref in (target_loop.start_expr.walk(Reference) +
                    target_loop.stop_expr.walk(Reference)):
            if ref.symbol.is_static:
                continue
            if isinstance(ref.parent, IntrinsicCall):
                # The routine name of a intrinsic, or the arguments of inquiry
                # functions don't need to be check
                if ref.parent.is_inquiry or ref.position == 0:
                    continue
            values_to_check.add(ref.symbol)

        # Opt-out for special assignment cases
        for assignment in routine.walk(Assignment):

            if isinstance(assignment.lhs, Reference):
                # Check that none of the loop-bounds Symbols are assigned to
                # within the Routine.
                if assignment.lhs.symbol in values_to_check:
                    raise TransformationError(
                        f"{self.name} can only be applied to loops with static"
                        f" loop bound expressions, but it has been attempted "
                        f"in a loop with the variable "
                        f"'{assignment.lhs.symbol.name}' which is assigned to:"
                        f" '{assignment.debug_string().strip()}'."
                    )

                # This checks for assignments of the form `a = ...` where 'a'
                # is requested to have its rank increased.
                if assignment.lhs.symbol not in arrays_symbols:
                    continue

                if assignment.is_descendant_of(target_loop):
                    continue

                if not assignment.rhs.walk(Call):
                    continue

                if isinstance(assignment.rhs.datatype,
                              (ArrayType, UnresolvedType)):
                    # Reject because such an assignment needs to change
                    # the function as well (or introduce one or more
                    # additional loops), e.g.:
                    #      a(1,:) = 1 + dummy_func()
                    info_str = assignment.debug_string().strip()
                    raise TransformationError(
                        f"{self.name} No call on r.h.s. of an assignment"
                        " to an array range is allowed as this would"
                        " imply a change to the return type of the call"
                        f" ({info_str})."
                    )

        # Capture all symbols used inside codeblocks, these are not permitted
        codeblock_names: set[DataSymbol] = set()
        # Capture all symbols used outside the loop anywhere other than the
        # top reference of an array assignment. This is because we cannot
        # guarantee their safety as they could change the rank of an expression
        # or read location of and index and therefore change the meaning of the
        # code.
        non_supported_outside_loop_symbols: set[DataSymbol] = set()
        for check in routine.walk((CodeBlock, Reference)):
            if isinstance(check, CodeBlock):
                for name in check.get_symbol_names():
                    codeblock_names.add(name.lower())
            else:
                if check.is_descendant_of(target_loop):
                    # We check References that appear within the target loop
                    # further down in this method.
                    continue
                if (isinstance(check.parent, Assignment) and
                        check is check.parent.lhs):
                    # Assignments to the variable are fine, because the value
                    # will just be repeated to each index of the new rank.
                    continue
                if (isinstance(check.parent, IntrinsicCall) and
                        check.parent.is_inquiry and
                        "dim" in check.parent.argument_names):
                    # Inquiry intrinsics calls to a given 'dim' are still ok
                    # because the specific dims still match (the new dimension
                    # is the outer one) and the per-rank information is still
                    # the same
                    continue
                # Everything else is currently forbidden
                non_supported_outside_loop_symbols.add(check.symbol)

        for array in arrays_symbols:

            if routine.return_symbol is array:
                raise TransformationError(
                    f"{self.name} can't be applied to array '{array.name}' "
                    f"as it is a return value in function '{routine.name}'.")

            if array.datatype.intrinsic is ScalarType.Intrinsic.CHARACTER:
                raise TransformationError(
                    f"{self.name} can't be applied to character arrays.")

            if (not array.is_automatic or not array.is_array or
                    isinstance(sym.datatype.intrinsic, DataTypeSymbol)):
                raise TransformationError(
                    f"{self.name} provided 'arrays' must be local array "
                    f"symbols of intrinsic type, but '{array}' is not.")

            if array.name.lower() in codeblock_names:
                raise TransformationError(
                    f"{self.name} does not support arrays that are referenced "
                    f"inside a Codeblock, but '{array.name}' is inside one.")

            if array in non_supported_outside_loop_symbols:
                raise TransformationError(
                    f"{self.name} does not support arrays that are referenced "
                    f"outside the given loop in a non-trivial expression "
                    f"but '{array.name}' is used outside the loop.")

            if array.datatype.is_allocatable:
                # One would also need to change the 'allocate' / deallocate
                # calls to add the new dimension, but it's not yet done.
                raise TransformationError(
                    f"{self.name} does not support arrays that are"
                    f" allocatable, but '{array.name}' is.")

        # Walk over `Reference` *within* the target Loop to also catch, e.g,
        #     `call foo(some_array)`
        # where the argument is a `Reference` and not an `ArrayReference`.
        for ref in target_loop.walk(Reference):
            # We iterate here over the arrays since it's more efficient
            # than in the outer.
            for array in arrays_symbols:

                if ref.symbol is not array:
                    continue

                if type(ref) is Reference:
                    # Check for cases where an array is used as an argument
                    # to a call, e.g., `call foo(some_array)`. We reject these
                    # since changing the rank of 'some_array' would imply a
                    # change to the interface of 'foo'.
                    call: Call = ref.ancestor(Call)
                    if isinstance(call, IntrinsicCall):
                        if call.is_inquiry and "dim" in call.argument_names:
                            continue

                    raise TransformationError(
                        f"{self.name} does not yet support all special cases "
                        f"for arrays accessed without indices. Found "
                        f"'{ref.ancestor(Statement).debug_string().strip()}'.")

                array_ref: ArrayReference = ref

                for idx in array_ref.indices:
                    for n in idx.walk(Reference):
                        if n.symbol is target_loop.variable_reference.symbol:
                            raise TransformationError(
                                f"{self.name} does not support arrays"
                                f" that are already indexed with the loop"
                                f" variable of the target loop,"
                                f" but '{array.name}' is indexed"
                                f" with '{target_loop.variable.name}'.")

    def apply(
        self,
        node: Loop,
        arrays: Optional[list[Union[DataSymbol, str]]] = None,
        **kwargs
    ):
        '''Applies the transformation.

        :param node: target Loop node.
        :param arrays: list of arrays that will have their rank increased.

        '''
        self.validate(node, arrays=arrays, **kwargs)

        # Processed array names
        array_processed: set[str] = set()

        for array in arrays:
            array_symbol: DataSymbol
            if isinstance(array, str):
                array_symbol = node.scope.symbol_table.lookup(array)
            else:
                array_symbol = array

            if array_symbol.name.lower() in array_processed:
                continue
            array_processed.add(array_symbol.name.lower())

            # Add an additional dimension to this array with the same bounds as
            # the target Loop
            array_symbol.shape.append(
                ArrayType.ArrayBounds(node.start_expr, node.stop_expr)
            )

            symtable_node = array_symbol.find_symbol_table(node).node
            for ref in symtable_node.walk(ArrayReference):
                if ref.symbol is array_symbol:
                    if ref.is_descendant_of(node):
                        # Inside the target loop index the reference
                        # using the loop variable
                        ref.addchild(Reference(node.variable))
                    else:
                        # Outside the target loop index the whole range
                        ref.addchild(Range.create(
                                        node.start_expr.copy(),
                                        node.stop_expr.copy(),
                                        node.step_expr.copy()))


__all__ = ["IncreaseRankLoopArraysTrans"]
