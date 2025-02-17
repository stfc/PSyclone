# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2024-2025, Science and Technology Facilities Council.
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
# Author: A. B. G. Chalk, STFC Daresbury Lab

'''This module provides the sclarization transformation class.'''

import itertools
from typing import Optional, Dict, Any, List, Tuple

from psyclone.core import VariablesAccessInfo, Signature, SymbolicMaths
from psyclone.psyGen import Kern
from psyclone.psyir.nodes import Call, CodeBlock, Literal, \
        Loop, Node, Range, Reference, Routine, StructureReference
from psyclone.psyir.symbols import DataSymbol, RoutineSymbol, INTEGER_TYPE
from psyclone.psyir.transformations.loop_trans import LoopTrans


class ScalarizationTrans(LoopTrans):
    '''This transformation takes a Loop and converts any array accesses
    to scalar if the results of the loop are unused, and the initial value
    is unused. For example:

    >>> from psyclone.psyir.backend.fortran import FortranWriter
    >>> from psyclone.psyir.frontend.fortran import FortranReader
    >>> from psyclone.psyir.transformations import ScalarizationTrans
    >>> from psyclone.psyir.nodes import Loop
    >>> code = """program test
    ... integer :: i,j
    ... real :: a(100), b(100)
    ... do i = 1,100
    ...   a(i) = i
    ...   b(i) = a(i) * a(i)
    ... end do
    ... do j = 1, 100
    ...  if(b(i) > 200) then
    ...    print *, b(i)
    ...  end if
    ... end do
    ... end program"""
    >>> psyir = FortranReader().psyir_from_source(code)
    >>> scalarise = ScalarizationTrans()
    >>> scalarise.apply(psyir.walk(Loop)[0])
    >>> print(FortranWriter()(psyir))
    program test
      integer :: i
      integer :: j
      real, dimension(100) :: a
      real, dimension(100) :: b
      real :: a_scalar
    <BLANKLINE>
      do i = 1, 100, 1
        a_scalar = i
        b(i) = a_scalar * a_scalar
      enddo
      do j = 1, 100, 1
        if (b(i) > 200) then
          ! PSyclone CodeBlock (unsupported code) reason:
          !  - Unsupported statement: Print_Stmt
          PRINT *, b(i)
        end if
      enddo
    <BLANKLINE>
    end program test
    <BLANKLINE>
    '''

    @staticmethod
    def _is_local_array(signature: Signature,
                        var_accesses: VariablesAccessInfo) -> bool:
        '''
        :param signature: The signature to check if it is a local array symbol
                          or not.
        :param var_accesses: The VariableAccessesInfo object containing
                             signature.
        :returns: whether the symbol corresponding to signature is a
                  local array symbol or not.
        '''
        if not var_accesses[signature].is_array():
            return False
        # If any of the accesses are to a CodeBlock then we stop. This can
        # happen if there is a string access inside a string concatenation,
        # e.g. NEMO4.
        for access in var_accesses[signature].all_accesses:
            if isinstance(access.node, CodeBlock):
                return False
        base_symbol = var_accesses[signature].all_accesses[0].node.symbol
        if not base_symbol.is_automatic:
            return False
        # If its a derived type then we don't scalarize.
        if isinstance(var_accesses[signature].all_accesses[0].node,
                      StructureReference):
            return False
        # Find the containing routine
        rout = var_accesses[signature].all_accesses[0].node.ancestor(Routine)
        # If the array is the return symbol then its not a local
        # array symbol
        if base_symbol is rout.return_symbol:
            return False

        return True

    @staticmethod
    def _have_same_unmodified_index(
            signature: Signature,
            var_accesses: VariablesAccessInfo) -> bool:
        '''
        :param signature: The signature to check.
        :param var_accesses: The VariableAccessesInfo object containing
                             signature.
        :returns: whether all the array accesses to signature use the
                  same index, and whether the index is unmodified in
                  the code region.
        '''
        array_indices = None
        scalarizable = True
        for access in var_accesses[signature].all_accesses:
            if array_indices is None:
                array_indices = access.component_indices
            # For some reason using == on the component_lists doesn't work
            # so we use [:] notation.
            elif array_indices[:] != access.component_indices[:]:
                scalarizable = False
                break
            # For each index, we need to check they're not written to in
            # the loop.
            flattened_indices = list(itertools.chain.from_iterable(
                    array_indices))
            for index in flattened_indices:
                # Index may not be a Reference, so we need to loop over the
                # References
                for ref in index.walk(Reference):
                    # This Reference could be the symbol for a Call or
                    # IntrinsicCall, which we don't allow to scalarize
                    if isinstance(ref.symbol, RoutineSymbol):
                        scalarizable = False
                        break
                    sig, _ = ref.get_signature_and_indices()
                    if var_accesses[sig].is_written():
                        scalarizable = False
                        break

        return scalarizable

    @staticmethod
    def _check_first_access_is_write(signature: Signature,
                                     var_accesses: VariablesAccessInfo) \
            -> bool:
        '''
        :param signature: The signature to check.
        :param var_accesses: The VariableAccessesInfo object containing
                             signature.
        :returns: whether the first access to signature is a write.
        '''
        if var_accesses[signature].is_written_first():
            return True
        return False

    @staticmethod
    def _get_index_values_from_indices(
            node: Node, indices: List[Node]) -> Tuple[bool, List[Node]]:
        '''
        TODO
        '''
        index_values = []
        has_complex_index = False
        for index in indices:
            # If the index is an array or structure and there are any more
            # accesses to the signature we're trying to scalarize, then we
            # should not scalarize.
            if (type(index) is not Range and type(index) is not Reference and
                    type(index) is not Literal):
                has_complex_index = True
            index_values.append(None)

        one_literal = Literal("1", INTEGER_TYPE)
        ancestor_loop = node.ancestor(Loop)
        # For Range or Literal array indices this is easy.
        for i, index in enumerate(indices):
            if isinstance(index, (Range, Literal)):
                index_values[i] = index

        while ancestor_loop is not None and not has_complex_index:
            for i, index in enumerate(indices):
                # Skip over indices we already set.
                if index_values[i] is not None:
                    continue
                if ancestor_loop.variable == index.symbol:
                    start_val = ancestor_loop.start_expr
                    stop_val = ancestor_loop.stop_expr
                    step_val = ancestor_loop.step_expr
                    # If the step value is not exactly 1 then we treat
                    # this as a complex index, as we can't currently
                    # do precise comparisons on non-unit stride accesses.
                    if step_val != one_literal:
                        has_complex_index = True
                    # Create a range for this and add it to the index values.
                    index_range = Range.create(
                            start_val.copy(),
                            stop_val.copy()
                    )
                    index_values[i] = index_range
            ancestor_loop = ancestor_loop.ancestor(Loop)

        # If we couldn't work out any of the index_values, then we treat this
        # as a complex index
        for index in index_values:
            if index is None:
                has_complex_index = True

        return has_complex_index, index_values

    @staticmethod
    def _value_unused_after_loop(sig: Signature,
                                 loop: Loop,
                                 var_accesses: VariablesAccessInfo) -> bool:
        '''
        :param sig: The signature to check.
        :param loop: The loop the transformation is operating on.
        :param var_accesses: The VariableAccessesInfo object containing
                             signature.
        :returns: whether the value computed in the loop containing
                  sig is read from after the loop.
        '''
        routine_var_accesses = None
        # Find the last access of the signature
        last_access = var_accesses[sig].all_accesses[-1].node
        # Compute the indices used in this loop. We know that all of the
        # indices used in this loop must be the same.
        indices = last_access.indices

        # Find the next accesses to this symbol
        next_accesses = last_access.next_accesses()

        # Compute the indices ranges.
        has_complex_index, index_values = \
            ScalarizationTrans._get_index_values_from_indices(
                        last_access, indices
            )

        for next_access in next_accesses:
            # next_accesses looks backwards to the start of the loop,
            # but we don't care about those accesses here.
            if next_access.is_descendent_of(loop):
                continue

            # If we have a next_access outside of the loop and have a complex
            # index then we do not scalarize this at the moment.
            if has_complex_index:
                return False

            # If next access is a Call or CodeBlock or Kern then
            # we have to assume the value is used. These nodes don't
            # have the is_read property that Reference has, so we need
            # to be explicit.
            if isinstance(next_access, (CodeBlock, Call, Kern)):
                return False

            # If the access is a read, then return False
            if next_access.is_read:
                return False

            # We need to ensure that the following write accesses the same
            # or more of the array.
            next_indices = next_access.indices
            next_complex_index, next_values = \
                ScalarizationTrans._get_index_values_from_indices(
                            next_access, next_indices
                )
            # If we can't compute the indices of the next access then we
            # cannot scalarize
            if next_complex_index:
                return False
            # If the two accesses don't have the same number of indices then
            # we won't scalarize - FIXME Can this happen?
            if len(next_values) != len(index_values):
                return False
            # Check the indices of next_access are greater than or equal to
            # that of the potential scalarization.
            for i in range(len(next_values)):
                # If the next index is a full range we can skip it as it must
                # cover the previous access
                if next_access.is_full_range(i):
                    continue
                # Convert both to ranges if either was a literal
                next_index = next_values[i]
                orig_index = index_values[i]
                if not isinstance(next_index, Range):
                    next_index = Range.create(next_index.copy(),
                                              next_index.copy())
                if not isinstance(orig_index, Range):
                    orig_index = Range.create(orig_index.copy(),
                                              orig_index.copy())
                sm = SymbolicMaths.get()
                # Need to check that next_index stop point is >= orig_index.
                # If its not then this can't cover the full range so we can
                # return False to not Scalarize this.
                if not (sm.greater_than(next_index.stop, orig_index.stop)
                        == SymbolicMaths.Fuzzy.TRUE or
                        sm.equal(next_index.stop, orig_index.stop)):
                    return False
                # Need to check the next_index start point is <= orig_index
                if not (sm.less_than(next_index.start, orig_index.start)
                        == SymbolicMaths.Fuzzy.TRUE or
                        sm.equal(next_index.start, orig_index.start)):
                    return False
                # If either of the start of stop points of the original
                # access range are a reference, we need to make sure that
                # reference has not been written to between the locations.
                if isinstance(next_index.stop, Reference):
                    # Find the containing Routine
                    if routine_var_accesses is None:
                        routine = loop.ancestor(Routine)
                        routine_var_accesses = VariablesAccessInfo(
                                nodes=routine
                        )
                    stop_sig = Signature(next_index.stop.symbol.name)
                    if not routine_var_accesses[stop_sig].is_read_only():
                        stop_savi = routine_var_accesses[stop_sig]
                        for access in stop_savi.all_write_accesses:
                            pos = access.node.abs_position
                            if (pos > loop.abs_position and
                                    pos < next_access.abs_position):
                                return False
                if isinstance(next_index.start, Reference):
                    # Find the containing Routine
                    if routine_var_accesses is None:
                        routine = loop.ancestor(Routine)
                        routine_var_accesses = VariablesAccessInfo(
                                nodes=routine
                        )
                    start_sig = Signature(next_index.start.symbol.name)
                    if not routine_var_accesses[start_sig].is_read_only():
                        start_savi = routine_var_accesses[start_sig]
                        for access in start_savi.all_write_accesses:
                            pos = access.node.abs_position
                            if (pos > loop.abs_position and
                                    pos < next_access.abs_position):
                                return False

        return True

    def apply(self, node: Loop, options: Optional[Dict[str, Any]] = None) \
            -> None:
        '''
        Apply the scalarization transformation to a loop.
        All of the array accesses that are identified as being able to be
        scalarized will be transformed by this transformation.

        An array access will be scalarized if:
        1. All accesses to the array use the same indexing statement.
        2. All References contained in the indexing statement are not modified
        inside of the loop (loop variables are ok).
        3. The array symbol is either not accessed again or is written to
        as its next access. If the next access is inside a conditional
        that is not an ancestor of the input loop, then PSyclone will
        assume that we cannot scalarize that value instead of attempting to
        understand the control flow.
        4. The array symbol is a local variable.

        :param node: the supplied loop to apply scalarization to.
        :param options: a dictionary with options for transformations.

        '''
        # For each array reference in the Loop:
        # Find every access to the same symbol in the loop
        # They all have to be accessed with the same index statement, and
        # that index needs to not be written to inside the loop body.
        # For each symbol that meets this criteria, we then need to check the
        # first access is a write
        # Then, for each symbol still meeting this criteria, we need to find
        # the next access outside of this loop. If its inside an ifblock that
        # is not an ancestor of this loop then we refuse to scalarize for
        # simplicity. Otherwise if its a read we can't scalarize safely.
        # If its a write then this symbol can be scalarized.

        var_accesses = VariablesAccessInfo(nodes=node.loop_body)

        # Find all the arrays that are only accessed by a single index, and
        # that index is only read inside the loop.
        potential_targets = filter(
                lambda sig:
                ScalarizationTrans._is_local_array(sig, var_accesses),
                var_accesses)
        potential_targets = filter(
                lambda sig:
                ScalarizationTrans._have_same_unmodified_index(sig,
                                                               var_accesses),
                potential_targets)

        # Now we need to check the first access is a write and remove those
        # that aren't.
        potential_targets = filter(
                lambda sig:
                ScalarizationTrans._check_first_access_is_write(sig,
                                                                var_accesses),
                potential_targets)

        # Check the values written to these arrays are not used after this loop
        finalised_targets = filter(
                lambda sig:
                ScalarizationTrans._value_unused_after_loop(sig,
                                                            node,
                                                            var_accesses),
                potential_targets)

        routine_table = node.ancestor(Routine).symbol_table
        # For each finalised target we can replace them with a scalarized
        # symbol
        for target in finalised_targets:
            target_accesses = var_accesses[target].all_accesses
            first_access = target_accesses[0].node
            symbol_type = first_access.symbol.datatype.datatype
            symbol_name = first_access.symbol.name
            scalar_symbol = routine_table.new_symbol(
                    root_name=f"{symbol_name}_scalar",
                    symbol_type=DataSymbol,
                    datatype=symbol_type)
            ref_to_copy = Reference(scalar_symbol)
            for access in target_accesses:
                node = access.node
                node.replace_with(ref_to_copy.copy())
