# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2022-2023, Science and Technology Facilities Council.
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
# Authors: A. R. Porter, R. W. Ford, A. Chalk and S. Siso, STFC Daresbury Lab

'''
Module containing the implementation of tooling around performing
substitions in the PSyIR.

This functionality is used by the inlining transformation and by the
fparser2 frontend when handling ASSOCIATE blocks.

'''

from psyclone.psyir import nodes, symbols
from psyclone.psyir.nodes.array_mixin import ArrayMixin


_ONE = nodes.Literal("1", symbols.INTEGER_TYPE)


class SubstitutionTool:
    '''
    Class to encapsulate functionality for performing substitutions of
    Symbols referenced in PSyIR fragments.

    '''

    def replace_reference(self, ref, subst_map):
        '''
        Recursively replaces any References to Symbols named in the supplied
        substitution map with the corresponding PSyIR expression.
        If the supplied node is not a Reference to a symbol named in the
        substitution map then it is just returned (after we have recursed to
        any children).

        :param ref: the expression to update.
        :type ref: :py:class:`psyclone.psyir.nodes.Node`
        :param subst_map: mapping from names of Symbols to replace to
            expressions to replace them with.
        :type subst_map: Dict[str, :py:class:`psyclone.psyir.nodes.Node`]

        :returns: the replacement reference.
        :rtype: :py:class:`psyclone.psyir.nodes.Reference`

        '''
        if not isinstance(ref, nodes.Reference):
            # Recurse down in case this is e.g. an Operation or Range.
            for child in ref.children[:]:
                self.replace_reference(child, subst_map)
            return ref

        if ref.symbol.name not in subst_map:
            # The supplied reference is not to a formal argument.
            return ref

        # Lookup the actual argument that corresponds to this formal argument.
        actual_arg = subst_map[ref.symbol.name]

        # If the local reference is a simple Reference then we can just
        # replace it with a copy of the actual argument, e.g.
        #
        #   call my_sub(my_struc%data(i,j))
        #
        #   subroutine my_sub(var)
        #     ...
        #     var = 0.0
        #
        # pylint: disable=unidiomatic-typecheck
        if type(ref) is nodes.Reference:
            arg_copy = actual_arg.copy()
            # If the local reference we are replacing has a parent then we
            # must ensure the parent's child list is updated. (It may not
            # have a parent if we are in the process of constructing a brand
            # new reference.)
            if ref.parent:
                ref.replace_with(arg_copy)
            return arg_copy

        # Local reference is not simple but the actual argument is, e.g.:
        #
        #   call my_sub(my_struc)
        #
        #   subroutine my_sub(var)
        #     ...
        #     var%data(i,j) = 0.0
        #
        if type(actual_arg) is nodes.Reference:
            ref.symbol = actual_arg.symbol
            return ref

        # Neither the actual or local references are simple, i.e. they
        # include array accesses and/or structure accesses.
        new_ref = self.replace_struc_reference(actual_arg, ref, subst_map)
        # If the local reference we are replacing has a parent then we must
        # ensure the parent's child list is updated. (It may not have a parent
        # if we are in the process of constructing a brand new reference.)
        if ref.parent:
            ref.replace_with(new_ref)
        return new_ref

    def _create_inlined_idx(self, local_idx, decln_start, actual_start,
                            subst_map):
        '''
        Utility that creates the PSyIR for an inlined array-index access
        expression. This is not trivial since a formal argument may be
        declared with bounds that are shifted relative to those of an
        actual argument.

        If local_idx is the index of the access in the routine;
           local_decln_start is the starting index of the dimension as
                        declared in the routine;
           actual_start is the starting index of the slice at the callsite
                        (whether from the array declaration or a slice);

        then the index of the inlined access will be::

            inlined_idx = local_idx - local_decln_start + 1 + actual_start - 1
                        = local_idx - local_decln_start + actual_start

        :param local_idx: a local array-index expression (i.e. appearing \
            within the routine being inlined).
        :type local_idx: :py:class:`psyclone.psyir.nodes.Node`
        :param decln_start: the lower bound of the corresponding array \
            dimension, as declared inside the routine being inlined.
        :type decln_start: :py:class:`psyclone.psyir.nodes.Node`
        :param actual_start: the lower bound of the corresponding array \
            dimension, as defined at the call site.
        :type actual_start: :py:class:`psyclone.psyir.nodes.Node`
        :param subst_map: mapping from names to replace to expressions to
            replace them with.
        :type subst_map: Dict[str, :py:class:`psyclone.psyir.nodes.Node`]

        :returns: PSyIR for the corresponding inlined array index.
        :rtype: :py:class:`psyclone.psyir.nodes.Node`

        '''
        if isinstance(local_idx, nodes.Range):
            lower = self._create_inlined_idx(local_idx.start, decln_start,
                                             actual_start, subst_map)
            upper = self._create_inlined_idx(local_idx.stop, decln_start,
                                             actual_start, subst_map)
            step = self.replace_reference(local_idx.step, subst_map)
            return nodes.Range.create(lower.copy(), upper.copy(), step.copy())

        uidx = self.replace_reference(local_idx, subst_map)
        if decln_start == actual_start:
            # If the starting indices in the actual and formal arguments are
            # the same then we don't need to shift the index.
            return uidx

        ustart = self.replace_reference(decln_start, subst_map)
        start_sub = nodes.BinaryOperation.create(
            nodes.BinaryOperation.Operator.SUB,
            uidx.copy(), ustart.copy())
        return nodes.BinaryOperation.create(
            nodes.BinaryOperation.Operator.ADD,
            start_sub, actual_start.copy())

    def _update_actual_indices(self, actual_arg, local_ref,
                               subst_map):
        '''
        Create a new list of indices for the supplied actual argument
        (ArrayMixin) by replacing any Ranges with the appropriate expressions
        from the local access in the called routine. If there are no Ranges
        then the returned list of indices just contains copies of the inputs.

        :param actual_arg: (part of) the actual argument to the routine.
        :type actual_arg: :py:class:`psyclone.psyir.nodes.ArrayMixin`
        :param local_ref: the corresponding Reference in the called routine.
        :param subst_map: mapping from names to replace to expressions to
            replace them with.
        :type subst_map: Dict[str, :py:class:`psyclone.psyir.nodes.Node`]

        :returns: new indices for the actual argument.
        :rtype: List[:py:class:`psyclone.psyir.nodes.Node`]

        '''
        if isinstance(local_ref, ArrayMixin):
            local_indices = [idx.copy() for idx in local_ref.indices]
        # Get the locally-declared shape of the formal argument in case its
        # bounds are shifted relative to the caller.
        if isinstance(local_ref.symbol.datatype, symbols.ArrayType):
            local_decln_shape = local_ref.symbol.datatype.shape
        else:
            local_decln_shape = []

        new_indices = [idx.copy() for idx in actual_arg.indices]

        local_idx_posn = 0
        for pos, idx in enumerate(new_indices[:]):

            if not isinstance(idx, nodes.Range):
                continue

            # Starting index of slice of actual argument.
            if actual_arg.is_lower_bound(pos):
                # Range starts at lower bound of argument so that's what
                # we store.
                actual_start = actual_arg.get_lbound_expression(pos)
            else:
                actual_start = idx.start

            local_decln_start = None
            if local_decln_shape:
                if isinstance(local_decln_shape[local_idx_posn],
                              symbols.ArrayType.ArrayBounds):
                    # The formal argument declaration has a shape.
                    local_shape = local_decln_shape[local_idx_posn]
                    local_decln_start = local_shape.lower
                elif (local_decln_shape[local_idx_posn] ==
                      symbols.ArrayType.Extent.DEFERRED):
                    # The formal argument is declared to be allocatable and
                    # therefore has the same bounds as the actual argument.
                    local_shape = None
                    local_decln_start = actual_start
            if not local_decln_start:
                local_shape = None
                local_decln_start = _ONE

            if local_ref.is_full_range(local_idx_posn):
                # If the local Range is for the full extent of the formal
                # argument then the actual Range is defined by that of the
                # actual argument and no change is required unless the formal
                # argument is declared as having a Range with an extent that is
                # less than that supplied. In general we're not going to know
                # that so we have to be conservative.
                if local_shape:
                    new = nodes.Range.create(local_shape.lower.copy(),
                                             local_shape.upper.copy())
                    new_indices[pos] = self._create_inlined_idx(
                        new, local_decln_start, actual_start, subst_map)
            else:
                # Otherwise, the local index expression replaces the Range.
                new_indices[pos] = self._create_inlined_idx(
                    local_indices[local_idx_posn],
                    local_decln_start, actual_start, subst_map)
            # Each Range corresponds to one dimension of the formal argument.
            local_idx_posn += 1
        return new_indices

    def replace_struc_reference(self, actual_arg, ref, subst_map):
        '''
        Called by replace_reference() whenever a formal or actual argument
        involves an array or structure access that can't be handled with a
        simple substitution, e.g.

        .. code-block:: fortran

            call my_sub(my_struc%grid(:,2,:), 10)

            subroutine my_sub(grid, ngrids)
              ...
              do igrid = 1, ngrids
                do jgrid = ...
                  do i = 1, 10
                    do j = 1, 10
                      grid(igrid, jgrid)%data(i,j) = 0.0

        The assignment in the inlined code should become

        .. code-block:: fortran

            my_struc%grid(igrid,2,jgrid)%data(i,j) = 0.0

        This routine therefore recursively combines any References to formal
        arguments in the supplied Reference (including any array-index
        expressions) with the corresponding Reference
        from the call site to make a new Reference for use in the inlined code.

        :param actual_arg: an actual argument to the routine being inlined.
        :type actual_arg: :py:class:`psyclone.psyir.nodes.Reference`
        :param ref: the corresponding reference to a formal argument.
        :type ref: :py:class:`psyclone.psyir.nodes.Reference`
        :param subst_map: mapping from names to replace to expressions to
            replace them with.
        :type subst_map: Dict[str, :py:class:`psyclone.psyir.nodes.Node`]

        :returns: the replacement reference.
        :rtype: :py:class:`psyclone.psyir.nodes.Reference`

        '''
        # The final stage of this method creates a brand new
        # [ArrayOf]Structure[s]Reference so we have to collect the indices and
        # members as we walk down both the actual and local references.
        local_indices = None
        members = []

        # Actual arg could be var, var(:)%index, var(i,j)%grid(:) or
        # var(j)%data(i) etc. Any Ranges must correspond to dimensions of the
        # formal argument. The validate() method has already ensured that we
        # do not have any indirect accesses or non-unit strides.

        if isinstance(ref, ArrayMixin):
            local_indices = [idx.copy() for idx in ref.indices]

        # Since a Range can occur at any level of a Structure access in the
        # actual argument, we walk down it and check each Member. Any Ranges
        # are updated according to how that dimension is accessed by the
        # reference inside the routine.
        cursor = actual_arg
        while True:
            if isinstance(cursor, ArrayMixin):
                new_indices = self._update_actual_indices(
                    cursor, ref, subst_map)
                members.append((cursor.name, new_indices))
            else:
                members.append(cursor.name)

            if not isinstance(cursor, (nodes.StructureMember,
                                       nodes.StructureReference)):
                break
            cursor = cursor.member

        if not actual_arg.walk(nodes.Range) and local_indices:
            # There are no Ranges in the actual argument but the local
            # reference is an array access.
            # Create updated index expressions for that access.
            new_indices = []
            for idx in local_indices:
                new_indices.append(
                    self.replace_reference(idx.copy(), subst_map))
            # Replace the last entry in the `members` list with a new array
            # access.
            members[-1] = (cursor.name, new_indices)

        # We now walk down the *local* access, skipping its head (as that is
        # replaced by the actual arg). We don't need to worry about updating
        # index expressions in the actual argument as they are independent of
        # any array accesses within a structure passed as a formal argument.
        cursor = ref
        while isinstance(cursor, (nodes.StructureReference,
                                  nodes.StructureMember)):
            cursor = cursor.member
            if isinstance(cursor, ArrayMixin):
                new_indices = []
                for idx in cursor.indices:
                    # Update each index expression in case it refers to
                    # formal arguments.
                    new_indices.append(
                        self.replace_reference(idx.copy(), subst_map))
                members.append((cursor.name, new_indices))
            else:
                members.append(cursor.name)

        # Finally, construct the new Reference using the information we've
        # collected from both the actual argument and local access.
        if len(members) > 1:
            # We have some form of Structure reference.
            if isinstance(members[0], tuple):
                # Root of access is an array access.
                return nodes.ArrayOfStructuresReference.create(
                    actual_arg.symbol,
                    members[0][1],
                    members[1:])
            return nodes.StructureReference.create(
                actual_arg.symbol, members[1:])

        # Just an array reference.
        return nodes.ArrayReference.create(actual_arg.symbol, members[0][1])
