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
        Reference, Routine
from psyclone.psyir.symbols import DataSymbol
from psyclone.psyir.transformations.loop_trans import LoopTrans


class ScalarizationTrans(LoopTrans):

    def _find_potential_scalarizable_array_symbols(self, node, var_accesses):

        potential_arrays = []
        signatures = var_accesses.all_signatures
        for signature in signatures:
            # Skip over non-arrays
            if not var_accesses[signature].is_array():
                continue
            # Skip over non-local symbols
            base_symbol = var_accesses[signature].all_accesses[0].node.symbol
            if not base_symbol.is_automatic:
                continue
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
                sig, _ = index.get_signature_and_indices()
                if var_accesses[sig].is_written():
                    scalarizable = False
                    break
            if scalarizable:
                potential_arrays.append(signature)

        return potential_arrays

    def _check_first_access_is_write(self, node, var_accesses, potentials):
        potential_arrays = []

        for signature in potentials:
            if var_accesses[signature].is_written_first():
                potential_arrays.append(signature)

        return potential_arrays

    def _check_valid_following_access(self, node, var_accesses, potentials):
        potential_arrays = []

        for signature in potentials:
            # Find the last access of each signature
            last_access = var_accesses[signature].all_accesses[-1].node
            # Find the next access to this symbol
            next_access = last_access.next_access()
            # If we don't use this again then its valid
            if next_access is None:
                potential_arrays.append(signature)
                continue
            # If we do and the next_access has an ancestor IfBlock
            # that isn't an ancestor of the loop then its not valid since
            # we aren't tracking down what the condition-dependent next
            # use really is.
            if_ancestor = next_access.ancestor(IfBlock)

            # If abs_position of if_ancestor is > node.abs_position
            # its not an ancestor of us.
            if (if_ancestor is not None and
                    if_ancestor.abs_position > node.abs_position):
                # Not a valid next_access pattern.
                continue

            # If next access is the LHS of an assignment, we need to
            # check that it doesn't also appear on the RHS. If so its
            # not a valid access
            # I'm not sure this code is reachable
#            if (isinstance(next_access.parent, Assignment) and
#                next_access.parent.lhs is next_access and
#                (next_access.next_access() is not None and
#                 next_access.next_access().ancestor(Assignment) is
#                 next_access.parent)):
#                continue

            # If next access is the RHS of an assignment then we need to
            # skip it
            ancestor_assign = next_access.ancestor(Assignment)
            if (ancestor_assign is not None and
                    ancestor_assign.lhs is not next_access):
                continue

            # If it has an ancestor that is a CodeBlock or Call or Kern
            # then we can't guarantee anything, so we remove it.
            if (next_access.ancestor((CodeBlock, Call, Kern))
                    is not None):
                continue

            potential_arrays.append(signature)

        return potential_arrays

    def apply(self, node, options=None):
        '''Apply the scalarization transformation to a loop.
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
        4. TODO - The array symbol is a local variable.

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
        potential_targets = self._find_potential_scalarizable_array_symbols(
                node, var_accesses)

        # Now we need to check the first access is a write and remove those
        # that aren't.
        potential_targets = self._check_first_access_is_write(
                node, var_accesses, potential_targets)

        # Check the values written to these arrays are not used after this loop
        finalised_targets = self._check_valid_following_access(
                node, var_accesses, potential_targets)

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
