# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2025, Science and Technology Facilities Council.
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
# Authors: S. Siso, STFC Daresbury Lab

'''This module contains the IncreaseRankLoopArrays transformation.'''

from typing import Optional, Union

from psyclone.psyGen import Transformation
from psyclone.psyir.nodes import (
    Loop, Reference, ArrayReference, Routine, CodeBlock, Range,
    IntrinsicCall, Assignment)
from psyclone.psyir.symbols import ArrayType, Symbol
from psyclone.psyir.transformations.transformation_error \
    import TransformationError
from psyclone.utils import transformation_documentation_wrapper


@transformation_documentation_wrapper
class IncreaseRankLoopArraysTrans(Transformation):
    ''' This transformation takes a loop and a list of arrays accessed inside
    the loop, and increases those arrays with an additional dimension with the
    size of the interation space. Then it indexes all accesses with the loop
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
    ...     ! permited, but any other use will not pass the validation
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
        :raises TransformationError: if no array names are provided.
        :raises TransformationError: the given array name or the
            symbol is not local or not an array.
        :raises TransformationError: if any of the arrays are referenced inside
            a CodeBlock.
        :raises TransformationError: if any of the arrays are referenced
            outside the loop in anything other than (array) assignments to
            their variable.

        '''
        super().validate(node, **kwargs)
        self.validate_options(**kwargs)
        arrays = self.get_option('arrays', **kwargs)

        if not isinstance(node, Loop):
            raise TransformationError(
                f"The target of the {self.name} transformation should be a "
                f"Loop, but found '{type(node).__name__}'.")

        routine = node.ancestor(Routine)
        if routine is None:
            raise TransformationError(
                f"The target Loop of the {self.name} transformation must be "
                f"inside a Routine.")

        # Check if the loop bound expressions are static:
        # First, find all Symbols accessed in the loop bounds.
        values_to_check = set()
        for ref in (node.start_expr.walk(Reference) +
                    node.stop_expr.walk(Reference)):
            if ref.symbol.is_static:
                continue
            if isinstance(ref.parent, IntrinsicCall):
                # The routine name of a intrinsic, or the arguments of inquiry
                # functions don't need to be check
                if ref.parent.is_inquiry or ref.position == 0:
                    continue
            values_to_check.add(ref.symbol)
        # Second, check that none of these Symbols are assigned to within the
        # Routine
        for assignment in routine.walk(Assignment):
            if isinstance(assignment.lhs, Reference):
                if assignment.lhs.symbol in values_to_check:
                    raise TransformationError(
                        f"{self.name} can only be applied to loops with static"
                        f" loop bound expressions, but it has been attempted "
                        f"in a loop with the variable "
                        f"'{assignment.lhs.symbol.name}' which is assigned to:"
                        f" '{assignment.debug_string().strip()}'."
                    )

        # Capture all symbols used inside codeblocks, these are not permitted
        codeblock_names = set()
        # Capture all symbols used outside the loop anywhere other than the
        # top reference of an array assignment. This is because we cannot
        # guarantee their safty as they could change the rank of an expression
        # or read location of and index and therefore change the meaning of the
        # code.
        non_supported_outside_loop_symbols = set()
        for check in routine.walk((CodeBlock, Reference)):
            if isinstance(check, CodeBlock):
                for name in check.get_symbol_names():
                    codeblock_names.add(name.lower())
            if isinstance(check, Reference):
                if check.is_descendant_of(node):
                    # These are fine because inside the loop we will add an
                    # index and the resulting expression rank will be the same
                    continue
                if (isinstance(check.parent, Assignment) and
                        check is check.parent.lhs):
                    # Assignments to the variable are fine, because the value
                    # we will just be repeated to each index of the new rank
                    continue
                # Everything else is currently forbidden
                non_supported_outside_loop_symbols.add(check.symbol)

        # Each item listed in the array list must be a local Array Symbol or a
        # string that resolves to it
        if not isinstance(arrays, list) or len(arrays) == 0:
            raise TransformationError(
                f"{self.name} has a mandatory 'arrays' option that is required"
                f" to specify which arrays are to have their rank increased.")
        for array in arrays:
            if isinstance(array, str):
                try:
                    array = node.scope.symbol_table.lookup(array)
                except KeyError as err:
                    raise TransformationError(
                        f"{self.name} provided array '{array}' does not exist"
                        f"in this scope."
                    ) from err

            if (not isinstance(array, Symbol) or not array.is_automatic
                    or not array.is_array):
                raise TransformationError(
                    f"{self.name} provided 'arrays' must be local array "
                    f"symbols, but '{array}' is not")

            if array.name.lower() in codeblock_names:
                raise TransformationError(
                    f"{self.name} does not support arrays that are referenced "
                    f"inside a Codeblock, but '{array.name}' is inside one.")

            if array in non_supported_outside_loop_symbols:
                raise TransformationError(
                    f"{self.name} does not support arrays that are referenced "
                    f"outside the given loop in a non-trivial expression "
                    f"but '{array.name}' is used outside the loop.")

    def apply(
        self,
        node: Loop,
        arrays: Optional[list[Union[Symbol, str]]] = None,
        **kwargs
    ):
        '''Applies the transformation.

        :param node: target Loop node.
        :param arrays: list of arrays that will have their rank increased.

        '''
        self.validate(node, arrays=arrays, **kwargs)

        for array in arrays:
            if isinstance(array, str):
                array = node.scope.symbol_table.lookup(array, otherwise=None)

            # Add an additional dimension to this array with the same bounds as
            # the target Loop
            array.shape.append(
                ArrayType.ArrayBounds(node.start_expr, node.stop_expr)
            )

            for ref in array.find_symbol_table(node).node.walk(ArrayReference):
                if ref.symbol is array:
                    if ref.is_descendant_of(node):
                        # Inside the target loop index the reference using the
                        # loop variable
                        ref.addchild(Reference(node.variable))
                    else:
                        # Outside the target loop index the whole range
                        ref.addchild(Range.create(
                                        node.start_expr.copy(),
                                        node.stop_expr.copy(),
                                        node.step_expr.copy()))


__all__ = ["IncreaseRankLoopArraysTrans"]
