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
# Authors S. Siso and A. B. G. Chalk, STFC Daresbury Lab
# -----------------------------------------------------------------------------

'''This module contains the DataSharingAttributeMixin.'''

import abc
from typing import Set, Tuple

from psyclone.core import AccessType, AccessSequence
from psyclone.psyir.nodes.codeblock import CodeBlock
from psyclone.psyir.nodes.if_block import IfBlock
from psyclone.psyir.nodes.loop import Loop
from psyclone.psyir.nodes.while_loop import WhileLoop
from psyclone.psyir.nodes.omp_clauses import OMPReductionClause
from psyclone.psyir.nodes.reference import Reference
from psyclone.psyir.symbols import DataSymbol, Symbol


class DataSharingAttributeMixin(metaclass=abc.ABCMeta):
    ''' Abstract class used to compute data sharing attributes about variables
    in regions used for parallelism.
    '''

    def infer_sharing_attributes(self) -> \
            Tuple[Set[Symbol], Set[Symbol], Set[Symbol]]:
        '''
        The PSyIR does not specify if each symbol inside an OpenMP region is
        private, firstprivate, shared or shared but needs synchronisation,
        the attributes are inferred looking at the usage of each symbol inside
        the parallel region.

        This method analyses the directive body and automatically classifies
        each symbol using the following rules:
        - All arrays are shared unless listed in the explicitly private list.
        - Scalars that are accessed only once are shared.
        - Scalars that are read-only or written outside a loop are shared.
        - Scalars written in multiple iterations of a loop are private, unless:

            * there is a write-after-read dependency in a loop iteration,
              in this case they are shared but need synchronisation;
            * they are read before in the same parallel region (but not inside
              the same loop iteration), in this case they are firstprivate.
            * they are only conditionally written in some iterations;
              in this case they are firstprivate.

        This method returns the sets of private, firstprivate, and shared but
        needing synchronisation symbols, all symbols not in these sets are
        assumed shared. How to synchronise the symbols in the third set is
        up to the caller of this method.

        :returns: three set of symbols that classify each of the symbols in
                  the directive body as PRIVATE, FIRSTPRIVATE or SHARED NEEDING
                  SYNCHRONISATION.
        '''
        # Compute the abs position caches as we'll use these a lot.
        # The compute_cached_abs_position will only do this if needed
        # so we don't need to check here.
        self.compute_cached_abs_positions()

        # TODO #598: Improve the handling of scalar variables, there are
        # remaining issues when we have accesses of variables after the
        # parallel region that we currently declare as private. We could use
        # the DefinitionUseChain to prove that there are no more uses after
        # the loop.
        # e.g:
        # !$omp parallel do <- will set private(ji, my_index)
        # do ji = 1, jpk
        #   my_index = ji+1
        #   array(my_index) = 2
        # enddo
        # #end do
        # call func(my_index) <- my_index has not been updated

        private = set()
        fprivate = set()
        need_sync = set()

        # Collate reduction variables
        # TODO #2446 Ensure this behaves correctly for OpenACC when
        # OpenACC reductions are supported.
        red_vars = []
        for clause in self.children:
            if isinstance(clause, OMPReductionClause):
                for ref in clause.children:
                    red_vars.append(ref.name)

        n_cblocks = len(self.walk(CodeBlock))

        # Determine variables that must be private, firstprivate or need_sync
        var_accesses = self.reference_accesses()
        for signature in var_accesses.all_signatures:
            if not var_accesses[signature].has_data_access():
                continue
            # Skip those that are CONSTANT accesses.
            if any(x.access_type == AccessType.CONSTANT
                    for x in var_accesses[signature]):
                continue
            accesses = var_accesses[signature]
            # TODO #2094: var_name only captures the top-level
            # component in the derived type accessor. If the attributes
            # only apply to a sub-component, this won't be captured
            # appropriately.
            name = signature.var_name
            if name in red_vars:
                continue
            symbol = accesses[0].node.scope.symbol_table.lookup(
                name, otherwise=None)
            if symbol is None:
                # The signature does not match any symbol! This is probably a
                # structure, we will consider them shared
                continue

            # A parallel loop variable is always private
            if (isinstance(self.dir_body[0], Loop) and
                    self.dir_body[0].variable is symbol):
                private.add(symbol)
                continue

            # If it is manually marked as a local symbol, add it to private or
            # firstprivate set
            if (isinstance(symbol, DataSymbol) and
                    isinstance(self.dir_body[0], Loop) and
                    symbol in self.dir_body[0].explicitly_private_symbols):
                if self._should_it_be_fprivate(accesses, n_cblocks):
                    fprivate.add(symbol)
                else:
                    private.add(symbol)
                continue

            # All arrays not explicitly marked as explicitly_private are shared
            if any(accs.has_indices() for accs in accesses):
                continue

            # If a variable is only accessed once, it is either an error
            # or a shared variable - anyway it is not private
            if len(accesses) == 1:
                continue

            # TODO #598: If we only have writes, it must be need_sync:
            # do ji = 1, jpk
            #   if ji=3:
            #      found = .true.
            # Or lastprivate in order to maintain the serial semantics
            # do ji = 1, jpk
            #   found = ji

            # We consider private variables as being the ones that are written
            # in every iteration of a loop.
            # If one such scalar is potentially read before it is written, it
            # will be considered firstprivate.

            has_been_read = False
            last_read_position = 0
            for access in accesses:
                if access.is_any_read():
                    has_been_read = True
                    last_read_position = access.node.abs_position

                if access.is_any_write():
                    # Check if the write access is outside a loop. In this case
                    # it will be marked as shared. This is done because it is
                    # likely to be re-used later. e.g:
                    # !$omp parallel
                    # jpk = 100
                    # !omp do
                    # do ji = 1, jpk
                    loop_ancestor = access.node.ancestor(
                        (Loop, WhileLoop),
                        limit=self,
                        include_self=True)
                    if not loop_ancestor:
                        # If we find it at least one WRITE outside a loop we
                        # keep it as shared
                        break

                    # Otherwise, the assignment to this variable is inside a
                    # loop (and it will be repeated for each iteration), so
                    # we declare it as [first]private or need_sync

                    # If it has been read before we have to check if ...
                    if has_been_read:
                        loop_pos = loop_ancestor.loop_body.abs_position
                        if last_read_position < loop_pos:
                            # .. it was before the loop, so it is fprivate
                            fprivate.add(symbol)
                        else:
                            # or inside the loop, in which case it needs sync
                            need_sync.add(symbol)
                        break

                    # If the write is not guaranteed (because it is inside a
                    # conditional), make it firstprivate to keep the original
                    # value when the branch is never taken. Unless this never
                    # had a value before the loop (as some compilers don't
                    # like uninitialised firstprivates)
                    conditional_write = access.node.ancestor(
                        IfBlock,
                        limit=loop_ancestor,
                        include_self=True)
                    if conditional_write:
                        if self._should_it_be_fprivate(accesses, n_cblocks):
                            fprivate.add(symbol)
                    # Otherwise it is just 'private'
                    if symbol not in fprivate:
                        private.add(symbol)
                    break

        return private, fprivate, need_sync

    def _should_it_be_fprivate(
        self, accesses: AccessSequence, num_of_codeblocks: int
    ) -> bool:
        '''
        :param accesses: the sequence of accesses to the analysed variable.
        :param num_of_codeblocs: number of codeblocks in the analysed regrion.

        :returns: whether the variable represented by the provided accesses
        should be firstprivate (because there is the possibility that one of
        the accesses gets the value that the symbol had before the loop).

        '''
        if num_of_codeblocks > 10:
            # Any codeblock would make the involved variables firstprivate
            # and we found that loops with many codeblocks are slow to
            # process, so if we have more than a certain number of codeblocks
            # we skip the analysis and just return firstprivate for all symbols
            # TODO #3183: If enters_scope gets faster we can get rid of this
            return True

        # Check if it gets a value from before the loop
        visited_nodes = set()  # Store visited nodes to reduce repetitions
        for access in accesses:
            if not isinstance(access.node, Reference):
                # TODO #3124: Remove this special-case
                # Nodes that are not References do not have
                # 'enters_scope', so the analysis below can't
                # be done and we defensively use 'firstprivate'
                return True
            if access.node.enters_scope(self, visited_nodes):
                return True

        # If not, it can be just 'private'
        return False
