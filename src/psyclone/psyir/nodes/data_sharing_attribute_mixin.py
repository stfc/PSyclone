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

from psyclone.core import AccessType, VariablesAccessInfo
from psyclone.psyir.nodes.if_block import IfBlock
from psyclone.psyir.nodes.loop import Loop
from psyclone.psyir.nodes.reference import Reference
from psyclone.psyir.nodes.while_loop import WhileLoop
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

        # Determine variables that must be private, firstprivate or need_sync
        var_accesses = self.reference_accesses()
        for signature in var_accesses.all_signatures:
            if not var_accesses[signature].has_data_access():
                continue
            accesses = var_accesses[signature].all_accesses
            # TODO #2094: var_name only captures the top-level
            # component in the derived type accessor. If the attributes
            # only apply to a sub-component, this won't be captured
            # appropriately.
            name = signature.var_name
            symbol = accesses[0].node.scope.symbol_table.lookup(
                name, otherwise=None)

            # If it is manually marked as a local symbol, add it to private or
            # firstprivate set
            if (isinstance(symbol, DataSymbol) and
                    isinstance(self.dir_body[0], Loop) and
                    symbol in self.dir_body[0].explicitly_private_symbols):
                if any(ref.symbol is symbol for ref in self.preceding()
                       if isinstance(ref, Reference)):
                    # If it's used before the loop, make it firstprivate
                    fprivate.add(symbol)
                else:
                    private.add(symbol)
                continue

            # All arrays not explicitly marked as threadprivate are shared
            if any(accs.is_array() for accs in accesses):
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
                if access.access_type == AccessType.READ:
                    has_been_read = True
                    last_read_position = access.node.abs_position

                if access.access_type == AccessType.WRITE:
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
                        # If we find it at least once outside a loop we keep it
                        # as shared
                        break

                    # Otherwise, the assignment to this variable is inside a
                    # loop (and it will be repeated for each iteration), so
                    # we declare it as private or need_synch
                    name = signature.var_name
                    # TODO #2094: var_name only captures the top-level
                    # component in the derived type accessor. If the attributes
                    # only apply to a sub-component, this won't be captured
                    # appropriately.
                    symbol = access.node.scope.symbol_table.lookup(name)

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

                    # If the write is not guaranteed, we make it firstprivate
                    # so that in the case that the write doesn't happen we keep
                    # the original value
                    conditional_write = access.node.ancestor(
                        IfBlock,
                        limit=loop_ancestor,
                        include_self=True)
                    if conditional_write:
                        fprivate.add(symbol)
                        break

                    # Already found the first write and decided if it is
                    # shared, private or firstprivate. We can stop looking.
                    private.add(symbol)
                    break

        return private, fprivate, need_sync
