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

from psyclone.core import VariablesAccessMap, Signature, SymbolicMaths
from psyclone.psyGen import Kern
from psyclone.psyir.nodes import Call, CodeBlock, Literal, \
        IfBlock, Loop, Node, Range, Reference, Routine, StructureReference
from psyclone.psyir.nodes.array_mixin import ArrayMixin
from psyclone.psyir.symbols import DataSymbol, RoutineSymbol, INTEGER_TYPE
from psyclone.psyir.transformations.loop_trans import LoopTrans


class ScalarisationTrans(LoopTrans):
    '''This transformation takes a Loop and converts any array accesses
    to scalar if the results of the loop are unused, and the initial value
    is unused. For example in the following snippet the value of a(i)
    is only used inside the loop, so can be turned into a scalar, wheras
    the values of b(i) are used in the following loop so are kept as an array:

    >>> from psyclone.psyir.backend.fortran import FortranWriter
    >>> from psyclone.psyir.frontend.fortran import FortranReader
    >>> from psyclone.psyir.transformations import ScalarisationTrans
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
    >>> scalarise = ScalarisationTrans()
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
                        var_accesses: VariablesAccessMap) -> bool:
        '''
        :param signature: The signature to check if it is a local array symbol
                          or not.
        :param var_accesses: The VariableAccessesInfo object containing
                             signature.
        :returns: whether the symbol corresponding to signature is a
                  local array symbol or not.
        '''
        if not var_accesses[signature].has_indices():
            return False
        # If any of the accesses are to a CodeBlock then we stop. This can
        # happen if there is a string access inside a string concatenation,
        # e.g. NEMO4.
        for access in var_accesses[signature]:
            if isinstance(access.node, CodeBlock):
                return False
        base_symbol = var_accesses[signature][0].node.symbol
        if not base_symbol.is_automatic:
            return False
        # If its a derived type then we don't scalarise.
        if isinstance(var_accesses[signature][0].node,
                      StructureReference):
            return False
        # Find the containing routine
        rout = var_accesses[signature][0].node.ancestor(Routine)
        # If the array is the return symbol then its not a local
        # array symbol
        if base_symbol is rout.return_symbol:
            return False

        return True

    @staticmethod
    def _have_same_unmodified_index(
            signature: Signature,
            var_accesses: VariablesAccessMap) -> bool:
        '''
        :param signature: The signature to check.
        :param var_accesses: The VariableAccessesInfo object containing
                             signature.
        :returns: whether all the array accesses to signature use the
                  same index, and whether the index is unmodified in
                  the code region.
        '''
        array_indices = None
        scalarisable = True
        for access in var_accesses[signature]:
            if array_indices is None:
                array_indices = access.component_indices
            # For some reason using == on the component_lists doesn't work
            # so we use [:] notation.
            elif array_indices[:] != access.component_indices[:]:
                scalarisable = False
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
                    # IntrinsicCall, which we don't allow to scalarise
                    if isinstance(ref.symbol, RoutineSymbol):
                        scalarisable = False
                        break
                    sig, _ = ref.get_signature_and_indices()
                    if var_accesses[sig].is_written():
                        scalarisable = False
                        break

        return scalarisable

    @staticmethod
    def _check_first_access_is_write(signature: Signature,
                                     loop: Loop,
                                     var_accesses: VariablesAccessMap) \
            -> bool:
        '''
        :param signature: The signature to check.
        :param loop: The Loop object being transformed.
        :param var_accesses: The VariableAccessesInfo object containing
                             signature.
        :returns: whether the first access to signature is a write.
        '''
        if not var_accesses[signature].is_written_first():
            return False
        # Need to find the first access and check if its in a conditional.
        accesses = var_accesses[signature]
        first_node = accesses[0].node
        ifblock = first_node.ancestor(IfBlock)
        # If the depth of the ifblock is larger than loop then the write
        # is in a conditional
        if ifblock and ifblock.depth > loop.depth:
            return False
        return True

    @staticmethod
    def _get_index_values_from_indices(
            node: ArrayMixin, indices: List[Node]) -> Tuple[bool, List[Node]]:
        '''
        Compute a list of index values for a given node. Looks at loop bounds
        and range declarations to attempt to convert loop variables to an
        explicit range, i.e. an access like
        .. code-block:: fortran
            do i = 1, 100
            array(i) = ...
            end do

        the returned list would contain a range object for [1:100].

        If the computed indexes contains a non-unit stride, or an index is
        not a Range, Reference or Literal then this function will return
        True as the first element of the returned tuple, and the list of
        indices will be incomplete.

        :param node: The node to compute index values for.
        :param indices: the list of indexes to have values computed.

        :returns: a tuple containing a bool value set to True if any of the
                  index values are not computed, and a list of the computed
                  index values.
        '''
        index_values = []
        has_complex_index = False
        for index in indices:
            # If the index is an array or structure and there are any more
            # accesses to the signature we're trying to scalarise, then we
            # should not scalarise.
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
                                 var_accesses: VariablesAccessMap) -> bool:
        '''
        :param sig: The signature to check.
        :param loop: The loop the transformation is operating on.
        :param var_accesses: The VariableAccessesInfo object containing
                             signature.
        :returns: whether the value computed in the loop containing
                  sig is read from after the loop.
        '''
        # Find the last access of the signature
        last_access = var_accesses[sig][-1].node
        # Compute the indices used in this loop. We know that all of the
        # indices used in this loop must be the same.
        indices = last_access.indices

        # Find the next accesses to this symbol
        next_accesses = last_access.next_accesses()

        # Compute the indices ranges.
        has_complex_index, index_values = \
            ScalarisationTrans._get_index_values_from_indices(
                        last_access, indices
            )

        for next_access in next_accesses:
            # next_accesses looks backwards to the start of the loop,
            # but we don't care about those accesses here.
            if next_access.is_descendant_of(loop):
                continue

            # If we have a next_access outside of the loop and have a complex
            # index then we do not scalarise this at the moment.
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

            # If the next access is a Reference then we had a full range
            # access described without any range, which means a full
            # range access so we can skip the followup checks.
            if type(next_access) is Reference:
                continue

            # We need to ensure that the following write accesses the same
            # or more of the array.
            next_indices = next_access.indices
            next_complex_index, next_values = \
                ScalarisationTrans._get_index_values_from_indices(
                            next_access, next_indices
                )
            # If we can't compute the indices of the next access then we
            # cannot scalarise
            if next_complex_index:
                return False
            # Check the indices of next_access are greater than or equal to
            # that of the potential scalarisation.
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
                # return False to not Scalarise this.
                if not (sm.greater_than(next_index.stop, orig_index.stop)
                        == SymbolicMaths.Fuzzy.TRUE or
                        sm.equal(next_index.stop, orig_index.stop)):
                    return False
                # Need to check the next_index start point is <= orig_index
                if not (sm.less_than(next_index.start, orig_index.start)
                        == SymbolicMaths.Fuzzy.TRUE or
                        sm.equal(next_index.start, orig_index.start)):
                    return False

        return True

    def apply(self, node: Loop, options: Optional[Dict[str, Any]] = None) \
            -> None:
        '''
        Apply the scalarisation transformation to a loop.
        All of the array accesses that are identified as being able to be
        scalarised will be transformed by this transformation.

        An array access will be scalarised if:
        1. All accesses to the array use the same indexing statement.
        2. All References contained in the indexing statement are not modified
        inside of the loop (loop variables are ok).
        3. The array symbol is either not accessed again or is written to
        as its next access. If the next access is inside a conditional
        that is not an ancestor of the input loop, then PSyclone will
        assume that we cannot scalarise that value instead of attempting to
        understand the control flow.
        4. The array symbol is a local variable.

        :param node: the supplied loop to apply scalarisation to.
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
        # is not an ancestor of this loop then we refuse to scalarise for
        # simplicity. Otherwise if its a read we can't scalarise safely.
        # If its a write then this symbol can be scalarised.

        var_accesses = node.loop_body.reference_accesses()

        # Find all the arrays that are only accessed by a single index, and
        # that index is only read inside the loop.
        potential_targets = filter(
                lambda sig:
                ScalarisationTrans._is_local_array(sig, var_accesses),
                var_accesses)
        potential_targets = filter(
                lambda sig:
                ScalarisationTrans._have_same_unmodified_index(sig,
                                                               var_accesses),
                potential_targets)

        # Now we need to check the first access is a write and remove those
        # that aren't.
        potential_targets = filter(
                lambda sig:
                ScalarisationTrans._check_first_access_is_write(sig,
                                                                node,
                                                                var_accesses),
                potential_targets)

        # Check the values written to these arrays are not used after this loop
        finalised_targets = filter(
                lambda sig:
                ScalarisationTrans._value_unused_after_loop(sig,
                                                            node,
                                                            var_accesses),
                potential_targets)

        routine_table = node.ancestor(Routine).symbol_table
        # For each finalised target we can replace them with a scalarised
        # symbol
        for target in finalised_targets:
            target_accesses = var_accesses[target]
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
