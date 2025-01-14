# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017-2024, Science and Technology Facilities Council.
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

from psyclone.core import VariablesAccessInfo
from psyclone.psyGen import Kern
from psyclone.psyir.nodes import Assignment, Call, CodeBlock, IfBlock, \
        Loop, Node, Reference, Routine, WhileLoop
from psyclone.psyir.symbols import DataSymbol
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
    '''

    @staticmethod
    def _is_local_array(signature, var_accesses):
        '''
        :param signature: The signature to check if it is a local array symbol
                          or not.
        :type signature: :py:class:`psyclone.core.Signature`
        :param var_accesses: The VariableAccessesInfo object containing
                             signature.
        :type var_accesses: :py:class:`psyclone.core.VariablesAccessInfo`
        :returns bool: whether the symbol corresponding to signature is a
                       local symbol or not.
        '''
        if not var_accesses[signature].is_array():
            return False
        base_symbol = var_accesses[signature].all_accesses[0].node.symbol
        if not base_symbol.is_automatic:
            return False

        return True

    @staticmethod
    def _have_same_unmodified_index(signature, var_accesses):
        '''
        :param signature: The signature to check.
        :type signature: :py:class:`psyclone.core.Signature`
        :param var_accesses: The VariableAccessesInfo object containing
                             signature.
        :type var_accesses: :py:class:`psyclone.core.VariablesAccessInfo`
        :returns bool: whether all the array accesses to signature use the
                       same index, and whether the index is unmodified in
                       the code region.
        '''
        array_indices = None
        scalarizable = True
        for access in var_accesses[signature].all_accesses:
            if array_indices is None:
                array_indices = access.component_indices
            # For some reason using == on the component_lists doesn't work
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
                    sig, _ = ref.get_signature_and_indices()
                    if var_accesses[sig].is_written():
                        scalarizable = False
                        break

        return scalarizable

    @staticmethod
    def _check_first_access_is_write(signature, var_accesses):
        '''
        :param signature: The signature to check.
        :type signature: :py:class:`psyclone.core.Signature`
        :param var_accesses: The VariableAccessesInfo object containing
                             signature.
        :type var_accesses: :py:class:`psyclone.core.VariablesAccessInfo`
        :returns bool: whether the first access to signature is a write.
        '''
        if var_accesses[signature].is_written_first():
            return True
        return False

    @staticmethod
    def _value_unused_after_loop(sig, var_accesses):
        '''
        :param sig: The signature to check.
        :type sig: :py:class:`psyclone.core.Signature`
        :param var_accesses: The VariableAccessesInfo object containing
                             signature.
        :type var_accesses: :py:class:`psyclone.core.VariablesAccessInfo`
        :returns bool: whether the value computed in the loop containing
                       sig is read from after the loop.
        '''
        # Find the last access of the signature
        last_access = var_accesses[sig].all_accesses[-1].node
        # Find the next accesses to this symbol
        next_accesses = last_access.next_accesses()
        for next_access in next_accesses:
            # next_accesses looks backwards to the start of the loop,
            # but we don't care about those accesses here.
            if next_access.abs_position <= last_access.abs_position:
                continue

            # If next access is a Call or CodeBlock or Kern then
            # we have to assume the value is used.
            if isinstance(next_access, (CodeBlock, Call, Kern)):
                return False

            # If next access is in an IfBlock condition then it reads the
            # value.
            ancestor_ifblock = next_access.ancestor(IfBlock)
            if ancestor_ifblock:
                conditions = ancestor_ifblock.condition.walk(Node)
                for node in conditions:
                    if node is next_access:
                        return False

            # If next access has an ancestor WhileLoop, and its in the
            # condition then it reads the value.
            ancestor_while = next_access.ancestor(WhileLoop)
            if ancestor_while:
                conditions = ancestor_while.condition.walk(Node)
                for node in conditions:
                    if node is next_access:
                        return False

            # If next access has an ancestor Loop, and its one of the
            # start/stop/step values then it reads the value.
            ancestor_loop = next_access.ancestor(Loop)
            if ancestor_loop:
                starts = ancestor_loop.start_expr.walk(Node)
                stops = ancestor_loop.stop_expr.walk(Node)
                steps = ancestor_loop.step_expr.walk(Node)
                for node in starts:
                    if node is next_access:
                        return False
                for node in stops:
                    if node is next_access:
                        return False
                for node in steps:
                    if node is next_access:
                        return False

            # If next access is the RHS of an assignment then we need to
            # skip it
            # Handles:
            # a = next_access[i] + 1
            ancestor_assign = next_access.ancestor(Assignment)
            if (ancestor_assign is not None and
                    ancestor_assign.lhs is not next_access):
                return False

            # If it has an ancestor that is a CodeBlock or Call or Kern
            # then we can't guarantee anything, so we remove it.
            # Handles: call my_func(next_access)
            if (next_access.ancestor((CodeBlock, Call, Kern))
                    is not None):
                return False

        return True

    def apply(self, node, options=None):
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
        :type node: :py:class:`psyclone.psyir.nodes.Loop`
        :param options: a dictionary with options for transformations.
        :type options: Optional[Dict[str, Any]]

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

        # Find all the ararys that are only accessed by a single index, and
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
