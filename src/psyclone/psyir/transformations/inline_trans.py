# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2022, Science and Technology Facilities Council.
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
# Authors: A. R. Porter, R. W. Ford and A. Chalk, STFC Daresbury Lab

'''
This module contains the InlineTrans transformation.

'''
from psyclone.errors import InternalError, LazyString
from psyclone.psyGen import Transformation
from psyclone.psyir.backend.fortran import FortranWriter
from psyclone.psyir.nodes import (
    ArrayReference, ArrayOfStructuresReference, BinaryOperation, Call,
    CodeBlock, Range, Routine, Reference, Return, Literal, Assignment,
    Container, StructureReference)
from psyclone.psyir.nodes.array_mixin import ArrayMixin
from psyclone.psyir.symbols import (ContainerSymbol, DataSymbol, ScalarType,
                                    RoutineSymbol, ImportInterface, Symbol,
                                    ArrayType, INTEGER_TYPE)
from psyclone.psyir.transformations.transformation_error import (
    TransformationError)


_ONE = Literal("1", INTEGER_TYPE)


class InlineTrans(Transformation):
    '''
    This transformation takes a Call (which may have a return value)
    and replaces it with the body of the target routine. It is used as
    follows:

    >>> from psyclone.psyir.backend.fortran import FortranWriter
    >>> from psyclone.psyir.frontend.fortran import FortranReader
    >>> from psyclone.psyir.nodes import Call, Routine
    >>> from psyclone.psyir.transformations import InlineTrans
    >>> code = """
    ... module test_mod
    ... contains
    ...   subroutine run_it()
    ...     integer :: i
    ...     real :: a(10)
    ...     do i=1,10
    ...       a(i) = 1.0
    ...       call sub(a(i))
    ...     end do
    ...   end subroutine run_it
    ...   subroutine sub(x)
    ...     real, intent(inout) :: x
    ...     x = 2.0*x
    ...   end subroutine sub
    ... end module test_mod"""
    >>> psyir = FortranReader().psyir_from_source(code)
    >>> call = psyir.walk(Call)[0]
    >>> inline_trans = InlineTrans()
    >>> inline_trans.apply(call)
    >>> # Uncomment the following line to see a text view of the schedule
    >>> # print(psyir.walk(Routine)[0].view())
    >>> print(FortranWriter()(psyir.walk(Routine)[0]))
    subroutine run_it()
      integer :: i
      real, dimension(10) :: a
    <BLANKLINE>
      do i = 1, 10, 1
        a(i) = 1.0
        a(i) = 2.0 * a(i)
      enddo
    <BLANKLINE>
    end subroutine run_it
    <BLANKLINE>

    .. warning::
        Routines/calls with any of the following characteristics are not
        supported and will result in a TransformationError:

        * the routine is not in the same file as the call;
        * the routine contains an early Return statement;
        * the routine has a named argument;
        * the call to the routine passes array subsections;
        * the shape of any array arguments as declared inside the routine does
          not match the shape of the arrays being passed as arguments;
        * the routine accesses an un-resolved symbol;
        * the routine accesses a symbol declared in the Container to which it
          belongs.

        Some of these restrictions will be lifted by #924.

    '''
    def apply(self, node, options=None):
        '''
        Takes the body of the routine that is the target of the supplied
        call and replaces the call with it.

        :param node: target PSyIR node.
        :type node: :py:class:`psyclone.psyir.nodes.Routine`
        :param options: a dictionary with options for transformations.
        :type options: Optional[Dict[str, Any]]

        '''
        self.validate(node, options)

        # The table we will copy symbols into.
        table = node.scope.symbol_table
        # Find the routine to be inlined.
        orig_routine = self._find_routine(node)

        if not orig_routine.children or isinstance(orig_routine.children[0],
                                                   Return):
            # Called routine is empty so just remove the call.
            node.detach()
            return

        # Ensure we don't modify the original Routine by working with a
        # copy of it.
        routine = orig_routine.copy()
        routine_table = routine.symbol_table

        # Construct lists of the nodes that will be inserted and all of the
        # References that they contain.
        new_stmts = []
        refs = []
        # Map from name of precision symbol to those Literals that use it.
        precision_map = {}
        for child in routine.children:
            new_stmts.append(child.copy())
            refs.extend(new_stmts[-1].walk(Reference))
            for lit in new_stmts[-1].walk(Literal):
                if isinstance(lit.datatype.precision, DataSymbol):
                    name = lit.datatype.precision.name
                    if name not in precision_map:
                        precision_map[name] = []
                    precision_map[name].append(lit)

        # Deal with any Container symbols first.
        self._inline_container_symbols(table, routine_table)

        # Copy each Symbol from the Routine into the symbol table associated
        # with the call site, excluding those that represent dummy arguments
        # or containers.
        self._inline_symbols(table, routine_table, precision_map)

        # Replace any references to dummy arguments with copies of the
        # actual arguments.
        dummy_args = routine_table.argument_list
        for ref in refs:
            # Check the parent is not None as some references are replaced
            # during previous calls if they are array accesses
            if ref.parent is not None:
                self.replace_dummy_arg(ref, node, dummy_args)

        # Copy the nodes from the Routine into the call site.
        if isinstance(new_stmts[-1], Return):
            # If the final statement of the routine is a return then
            # remove it from the list.
            del new_stmts[-1]

        if routine.return_symbol:
            # This is a function
            assignment = node.ancestor(Assignment)
            parent = assignment.parent
            idx = assignment.position-1
            for child in new_stmts:
                idx += 1
                parent.addchild(child, idx)
            table = parent.scope.symbol_table
            # Avoid a potential name clash with the original function
            table.rename_symbol(
                routine.return_symbol, table.next_available_name(
                    f"inlined_{routine.return_symbol.name}"))
            node.replace_with(Reference(routine.return_symbol))
        else:
            # This is a call
            parent = node.parent
            idx = node.position
            node.replace_with(new_stmts[0])
            for child in new_stmts[1:]:
                idx += 1
                parent.addchild(child, idx)

    def replace_dummy_arg(self, ref, call_node, dummy_args):
        '''
        Combines a Reference to a dummy argument with the corresponding
        Reference from the call site to make a new Reference for use in the
        inlined code. If the supplied Reference is not to a dummy argument
        then it is returned unchanged.

        :param ref: the reference to update.
        :type ref: :py:class:`psyclone.psyir.nodes.Reference`
        :param call_node: the call site.
        :type call_node: :py:class:`psyclone.psyir.nodes.Call`
        :param dummy_args: the dummy arguments of the called routine.
        :type dummy_args: List[:py:class:`psyclone.psyir.nodes.Reference`]

        :returns: the replacement reference.
        :rtype: :py:class:`psyclone.psyir.nodes.Reference`

        :raises InternalError: if the actual and dummy references both \
                               represent array-element accessors.
        '''
        if not isinstance(ref, Reference):
            # Recurse down in case this is e.g. an Operation or Range.
            for child in ref.children:
                self.replace_dummy_arg(child, call_node, dummy_args)
            return ref

        if ref.symbol not in dummy_args:
            # The supplied reference is not to a dummy argument.
            return ref

        # Lookup the actual argument that corresponds to this dummy argument.
        actual_arg = call_node.children[dummy_args.index(ref.symbol)]

        # If the local reference is a simple Reference then we can just
        # replace it with the actual argument.
        # pylint: disable=unidiomatic-typecheck
        if type(ref) is Reference:
            # call my_sub(my_struc%data(i,j))
            #
            # subroutine my_sub(var)
            #   ...
            #   var = 0.0
            arg_copy = actual_arg.copy()
            ref.replace_with(arg_copy)
            return arg_copy

        # Local reference is not simple but the actual argument is, e.g.:
        #
        # call my_sub(my_struc)
        #
        # subroutine my_sub(var)
        #   ...
        #   var%data(i,j) = 0.0
        if type(actual_arg) is Reference:
            ref.symbol = actual_arg.symbol
            return ref

        # Neither the actual or local references are simple, i.e. they
        # include array accesses and/or structure accesses, e.g.:
        #
        # call my_sub(my_struc%grid(:,2,:), 10)
        #
        # subroutine my_sub(grid, ngrids)
        #   ...
        #   do igrid = 1, ngrids
        #     do jgrid = ...
        #     do i = 1, 10
        #       do j = 1, 10
        #         grid(igrid, jgrid)%data(i,j) = 0.0
        #
        # The assignment in the inlined code should become:
        #
        #         my_struc%grid(igrid,2,jgrid)%data(i,j) = 0.0

        # So, the head of the local reference needs to be replaced by the
        # head of the actual reference (e.g. grid => my_struc%grid) and then
        # any ranges in the actual reference need to be replaced by the
        # corresponding index expressions in the local reference.
        actual_indices = None
        local_indices = None
        if isinstance(actual_arg, ArrayMixin):
            actual_indices = [idx.copy() for idx in actual_arg.indices]
        if isinstance(ref, ArrayMixin):
            local_indices = ref.indices

        # Every dimension in the dummy argument must correspond to a Range
        # in the actual argument. In Fortran, Range(s) can only occur in
        # a single part reference (i.e. this(:)%that(:) is invalid).
        # If the access that we're dealing with is to a Member of a dummy
        # argument then that is independent of the actual argument.
        if (isinstance(actual_arg, StructureReference) or
                isinstance(ref, StructureReference)):
            new_ref = self._replace_dummy_struc_arg(actual_arg, ref, call_node,
                                                    dummy_args)
        else:
            # Both accesses must be plain array accesses. Since we've already
            # handled simple References, this means that the actual argument
            # *must* contain a slice and the local access *may* contain a
            # slice. (The PSyIR does not explicitly
            # support pointers and in Fortran, an array of pointers to arrays
            # can only be achieved through having an array of structures.)
            # The shape of the actual argument must correspond to the shape
            # of the dummy argument. So, e.g.:
            #
            # call sub1a(a(1,:,:))
            #
            # subroutine sub1a(x)
            #   real, intent(inout), dimension(10,10) :: x
            #   integer :: j
            #   do j = 1, 10
            #     x(1:10,j) = 2.0 * x(1:10,j)
            #   end do
            #
            # would become:
            #
            # do j = 1, 10
            #   a(1,1:10,j) = 2.0 * a(1,1:10,j)
            # end do
            #
            # i.e. any Range in the access to the dummy argument must replace
            # a Range in the actual argument.
            # Locate the Range(s) in the actual argument.
            range_posns = []
            for posn, arg_idx in enumerate(actual_indices):
                if isinstance(arg_idx, Range):
                    range_posns.append(posn)
            # These ranges must correspond to dimensions of the dummy argument
            if len(range_posns) != len(local_indices):
                raise InternalError("oops")
            # Loop over each index in the local access
            for actual, local in zip(range_posns, local_indices):
                new_idx = local.copy()
                if isinstance(local, Range) and ref.is_full_range(local_indices.index(local)):
                    # If the local Range is for the full extent of the dummy
                    # argument then the actual Range is defined by that of the
                    # actual argument.
                    new_idx = actual_indices[actual]
                else:
                    new_idx = self.replace_dummy_arg(new_idx, call_node,
                                                     dummy_args)
                actual_indices[actual] = new_idx
            new_ref = ArrayReference.create(actual_arg.symbol, actual_indices)
        ref.replace_with(new_ref)
        return new_ref

    def _create_inlined_idx(self, call_node, dummy_args,
                            local_idx, decln_start, actual_start):
        '''
        :param local_idx: Node describing a local array index.
        :param decln_start:
        :param actual_start:

        :returns: PSyIR for the corresponding inlined array index.
        :rtype:

        '''
        # If local_idx is the index of the access in the routine;
        #    local_decln_start is the starting index of the dimension as
        #                 declared in the routine.
        #    actual_start is the starting index of the slice at the callsite
        #                 (whether from the array declaration or a slice);
        #
        # then the index of the inlined access will be:
        #
        # inlined_idx = local_idx - local_decln_start + 1 + actual_start - 1
        #             = local_idx - local_decln_start + actual_start
        #
        if isinstance(local_idx, Range):
            lower = self._create_inlined_idx(call_node, dummy_args,
                                             local_idx.start, decln_start,
                                             actual_start)
            upper = self._create_inlined_idx(call_node, dummy_args,
                                             local_idx.stop, decln_start,
                                             actual_start)
            step = self.replace_dummy_arg(local_idx.step, call_node,
                                          dummy_args)
            return Range.create(lower.copy(), upper.copy(), step.copy())
        else:
            uidx = self.replace_dummy_arg(local_idx, call_node, dummy_args)
            if decln_start != _ONE or actual_start != _ONE:
                ustart = self.replace_dummy_arg(decln_start,
                                                call_node, dummy_args)
                start_sub = BinaryOperation.create(
                    BinaryOperation.Operator.SUB,
                    uidx.copy(), ustart.copy())
                return BinaryOperation.create(
                    BinaryOperation.Operator.ADD,
                    start_sub, actual_start.copy())
            else:
                return uidx

    def _replace_dummy_struc_arg(self, actual_arg, ref, call_node, dummy_args):
        '''

        '''
        actual_indices = None
        local_indices = None
        members = []
        # Actual arg could be var, var(:)%index, var(i,j)%grid(:) or
        # var(j)%data(i) etc. Any Ranges must correspond to dimensions of the
        # dummy argument. The validate() method has already ensured that we
        # do not have any indirect accesses.
        #
        # The actual argument declaration has actual_dim_start (defaults to 1).
        # The actual argument itself may be a slice with actual_arg_start.
        # The dummy argument declaration has dummy_dim_start (defaults to 1).
        # The access within the routine is to the array (section) specified
        # by the caller.
        # actual_idx = local_idx - local_dim_start + actual_dim_start

        actual_ranges = actual_arg.walk(Range)

        if isinstance(actual_arg, ArrayMixin):
            actual_indices = [idx.copy() for idx in actual_arg.indices]
        if isinstance(ref, ArrayMixin):
            local_indices = [idx.copy() for idx in ref.indices]
        # Get the locally-declared shape of the dummy argument in case its
        # bounds are shifted relative to the caller.
        if isinstance(ref.symbol.datatype, ArrayType):
            local_decln_shape = ref.symbol.datatype.shape
        else:
            local_decln_shape = []

        actual_datatype = actual_arg.datatype

        # First determine the index expressions. There are three possibilities:
        # 1. Actual has no Ranges, e.g. my_var(i)%var so could be whole array
        #    or a scalar.
        # 2. Local has no Ranges, e.g. x%other so could also be whole array or
        #    a scalar.
        # 3. Actual has one or more Ranges;
        # 4. Local has one or more Ranges; if these are not full ranges they
        #    take precendence over any Ranges in the actual argument.

        if actual_indices:
            # If any of the indices on the root of the actual argument are
            # Ranges then they must be updated. Otherwise they remain
            # unchanged.
            local_idx_posn = 0
            for pos, idx in enumerate(actual_indices[:]):
                if not isinstance(idx, Range):
                    # Not a Range so skip it.
                    continue

                if local_decln_shape and isinstance(
                        local_decln_shape[local_idx_posn],
                        ArrayType.ArrayBounds):
                    # The dummy argument declaration has a shape.
                    local_shape = local_decln_shape[local_idx_posn]
                    local_decln_start = local_shape.lower
                else:
                    local_shape = None
                    local_decln_start = _ONE

                # Starting index of slice of actual argument.
                if actual_arg.is_lower_bound(pos):
                    # Range starts at lower bound of argument so that's what
                    # we store.
                    actual_start = actual_arg.symbol.datatype.shape[pos].lower
                else:
                    actual_start = idx.start

                if ref.is_full_range(local_idx_posn):
                    # If the local Range is for the full extent of the
                    # dummy argument then the actual Range is defined by
                    # that of the actual argument and no change is required
                    # unless the dummy argument is declared as having a
                    # Range with an extent that is less than that supplied. In
                    # general we're not going to know that so we have to be
                    # conservative.
                    if local_shape:
                        new = Range.create(local_shape.lower.copy(),
                                           local_shape.upper.copy())
                        actual_indices[pos] = self._create_inlined_idx(
                            call_node, dummy_args,
                            new, local_decln_start, actual_start)
                else:
                    # Otherwise, the local index expression replaces the
                    # Range.
                    actual_indices[pos] = self._create_inlined_idx(
                        call_node, dummy_args,
                        local_indices[local_idx_posn],
                        local_decln_start, actual_start)
                # Each Range corresponds to one dimension of
                # the dummy argument.
                local_idx_posn += 1

        if actual_ranges:
            if local_indices:
                # e.g. my_var%grid(:)%nx -> nx_array(i)
                # Ranges in the actual arg. must be replaced by local index
                # expressions unless the latter are for the full range.
                # Ranges can only occur in a single Member of a
                # StructureReference.
                cursor = actual_arg
                while hasattr(cursor, "member"):
                    cursor = cursor.member
                    if hasattr(cursor, "indices"):
                        new_indices = []
                        local_idx_posn = 0
                        for idx in cursor.indices:
                            if isinstance(idx, Range):
                                if ref.is_full_range(local_idx_posn):
                                    # If the local Range is for the full extent of the dummy
                                    # argument then the actual Range is defined by that of the
                                    # actual argument.
                                    new_indices.append(idx.copy())
                                else:
                                    # Otherwise, the local index expression
                                    # replaces the Range.
                                    new_indices.append(
                                        self.replace_dummy_arg(
                                            local_indices[local_idx_posn],
                                            call_node, dummy_args))
                                # Each Range corresponds to one dimension of
                                # the dummy argument.
                                local_idx_posn += 1
                            else:
                                # Actual arg. index expression is not a range
                                # so is copied unchanged.
                                new_indices.append(idx.copy())
                        members.append((cursor.name, new_indices))
                    else:
                        members.append(cursor.name)

                # Now that we've handled the actual argument, we proceed down
                # into the local reference in case it is a StructureReference.
                # If it is, the actual argument replaces the head of it but
                # we must copy the remainder over.
                cursor = ref
                while hasattr(cursor, "member"):
                    cursor = cursor.member
                    if hasattr(cursor, "indices"):
                        new_indices = []
                        for idx in cursor.indices:
                            new_indices.append(
                                self.replace_dummy_arg(
                                    idx.copy(), call_node, dummy_args))
                        members.append((cursor.name, new_indices))
                    else:
                        members.append(cursor.name)
            else:
                # Local access is to whole actual argument array so index
                # expressions are those in the actual argument.
                cursor = actual_arg
                while cursor.hasattr("member"):
                    cursor = cursor.member
                    if hasattr(cursor, "indices"):
                        new_indices = [idx.copy() for idx in cursor.indices]
                        members.append((cursor.name, new_indices))
                    else:
                        members.append(cursor.name)
                # Continue with local access, skipping head (as that is
                # replaced by actual arg).
                cursor = ref
                while cursor.hasattr("member"):
                    cursor = cursor.member
                    if hasattr(cursor, "indices"):
                        new_indices = []
                        for idx in cursor.indices:
                            new_indices.append(
                                self.replace_dummy_arg(
                                    idx.copy(), call_node, dummy_args))
                        members.append((cursor.name, new_indices))
                    else:
                        members.append(cursor.name)
        else:
            # There are no Ranges in the actual argument.
            if local_indices:
                # Local indexing into a whole actual argument array so local
                # index expressions are just updated and copied over. However,
                # actual argument may be a StructureReference.
                cursor = actual_arg
                while hasattr(cursor, "member"):
                    cursor = cursor.member
                    if not hasattr(cursor, "member"):
                        # We break out before the ultimate member of the
                        # structure access as we have to add indexing to it.
                        break
                    if hasattr(cursor, "indices"):
                        new_indices = [idx.copy() for idx in cursor.indices]
                        members.append((cursor.name, new_indices))
                    else:
                        members.append(cursor.name)

                # We've reached the ultimate member of the
                # StructureReference so this is where we need to
                # add the index expressions from the local access.
                new_indices = []
                for idx in local_indices:
                    new_indices.append(
                        self.replace_dummy_arg(
                            idx.copy(), call_node, dummy_args))
                members.append((cursor.name, new_indices))

                # Continue with local access, skipping head (as that is
                # replaced by actual arg).
                cursor = ref
                while hasattr(cursor, "member"):
                    cursor = cursor.member
                    if hasattr(cursor, "indices"):
                        new_indices = []
                        for idx in cursor.indices:
                            new_indices.append(
                                self.replace_dummy_arg(
                                    idx.copy(), call_node, dummy_args))
                        members.append((cursor.name, new_indices))
                    else:
                        members.append(cursor.name)
            else:
                # No indexing in either the actual or local references so
                # the root access is the same as the actual access.
                cursor = actual_arg
                while hasattr(cursor, "member"):
                    cursor = cursor.member
                    if hasattr(cursor, "indices"):
                        new_indices = [idx.copy() for idx in cursor.indices]
                        members.append((cursor.name, new_indices))
                    else:
                        members.append(cursor.name)
                # Continue with local access, skipping head.
                cursor = ref
                while hasattr(cursor, "member"):
                    cursor = cursor.member
                    if hasattr(cursor, "indices"):
                        new_indices = []
                        for idx in cursor.indices:
                            new_indices.append(
                                self.replace_dummy_arg(
                                    idx.copy(), call_node, dummy_args))
                        members.append((cursor.name, new_indices))
                    else:
                        members.append(cursor.name)

        if actual_indices:
            new_ref = ArrayOfStructuresReference.create(actual_arg.symbol,
                                                        actual_indices,
                                                        members)
        else:
            new_ref = StructureReference.create(actual_arg.symbol,
                                                members)
        return new_ref

    @staticmethod
    def _inline_container_symbols(table, routine_table):
        '''
        Takes container symbols from the symbol table of the routine being
        inlined and adds them to the table of the call site. All references
        to each container symbol are also updated.

        :param table: the symbol table at the call site.
        :type table: :py:class:`psyclone.psyir.symbols.SymbolTable`
        :param routine_table: the symbol table of the routine being inlined.
        :type routine_table: :py:class:`psyclone.psyir.symbols.SymbolTable`

        '''
        for csym in routine_table.containersymbols:
            if csym.name in table:
                # We have a clash with another symbol at the call site.
                other_csym = table.lookup(csym.name)
                if not isinstance(other_csym, ContainerSymbol):
                    # The symbol at the call site is not a Container so we
                    # can rename it.
                    table.rename_symbol(
                            other_csym,
                            table.next_available_name(
                                csym.name, other_table=routine_table))
                    # We can then add an import from the Container.
                    table.add(csym)
                else:
                    # If there is a wildcard import from this container in the
                    # routine then we'll need that at the call site.
                    if csym.wildcard_import:
                        other_csym.wildcard_import = True
            else:
                table.add(csym)
            # We must update all references to this ContainerSymbol
            # so that they point to the one in the call site instead.
            imported_syms = routine_table.symbols_imported_from(csym)
            for isym in imported_syms:
                if isym.name in table:
                    # We have a potential clash with a symbol imported
                    # into the routine.
                    callsite_sym = table.lookup(isym.name)
                    if not callsite_sym.is_import:
                        # The validate() method has already checked that we
                        # don't have a clash between symbols of the same name
                        # imported from different containers.
                        # We don't support renaming an imported symbol but the
                        # symbol at the call site can be renamed so we do that.
                        table.rename_symbol(
                            callsite_sym,
                            table.next_available_name(
                                callsite_sym.name, other_table=routine_table))
                isym.interface = ImportInterface(table.lookup(csym.name))

    @staticmethod
    def _inline_symbols(table, routine_table, precision_map):
        '''
        Takes symbols from the symbol table of the routine and adds
        them to the table of the call site. Any literals that refer to
        precision symbols are updated to refer to the appropriate symbol in
        the table at the call site.

        :param table: the symbol table at the call site.
        :type table: :py:class:`psyclone.psyir.symbols.SymbolTable`
        :param routine_table: the symbol table of the routine being inlined.
        :type routine_table: :py:class:`psyclone.psyir.symbols.SymbolTable`
        :param precision_map: Lists of literals, indexed by the name of the \
            precision symbol that they use.
        :type precision_map: Dict[str, \
            List[:py:class:`psyclone.psyir.nodes.Literal`]]

        :raises InternalError: if an imported symbol is found that has not \
            been updated to refer to a Container at the call site.

        '''
        routine_name = routine_table.node.name
        dummy_args = routine_table.argument_list

        for old_sym in routine_table.symbols:

            if old_sym in dummy_args or isinstance(old_sym, ContainerSymbol):
                # We've dealt with Container symbols in
                # _inline_container_symbols() and we deal with dummy arguments
                # in apply().
                continue

            if old_sym.name == routine_name and isinstance(old_sym,
                                                           RoutineSymbol):
                # We don't want or need the symbol representing the routine
                # that is being inlined.
                continue

            old_name = old_sym.name
            try:
                table.add(old_sym)

            except KeyError:
                # We have a clash with a symbol at the call site.
                if old_sym.is_import:
                    # This symbol is imported from a Container so should
                    # already have been updated so as to be imported from the
                    # corresponding container in scope at the call site.
                    callsite_csym = table.lookup(
                        old_sym.interface.container_symbol.name)
                    if old_sym.interface.container_symbol is not callsite_csym:
                        # pylint: disable=raise-missing-from
                        raise InternalError(
                            f"Symbol '{old_sym.name}' imported from "
                            f"'{callsite_csym.name}' has not been updated to "
                            f"refer to that container at the call site.")
                else:
                    # A Symbol with the same name already exists so we rename
                    # the one that we are adding.
                    new_name = table.next_available_name(
                        old_sym.name, other_table=routine_table)
                    routine_table.rename_symbol(old_sym, new_name)
                    table.add(old_sym)

            # Check whether this symbol is used to specify the precision of
            # any literals.
            if old_name in precision_map:
                for lit in precision_map[old_name]:
                    # A literal is immutable so create a new one with the
                    # updated symbol as its precision.
                    dtype = ScalarType(lit.datatype.intrinsic, old_sym)
                    lit.replace_with(Literal(lit.value, dtype))

    def validate(self, node, options=None):
        '''
        Checks that the supplied node is a valid target for inlining.

        :param node: target PSyIR node.
        :type node: subclass of :py:class:`psyclone.psyir.nodes.Routine`
        :param options: a dictionary with options for transformations.
        :type options: Optional[Dict[str, Any]]

        :raises TransformationError: if the supplied node is not a Call.
        :raises TransformationError: if the routine has a return value.
        :raises TransformationError: if the routine body contains a Return \
            that is not the first or last statement.
        :raises TransformationError: if the routine body contains a CodeBlock.
        :raises TransformationError: if the called routine has a named \
            argument.
        :raises TransformationError: if a symbol of a given name is imported \
            from different containers at the call site and within the routine.
        :raises TransformationError: if the routine accesses an un-resolved \
            symbol.
        :raises TransformationError: if a symbol declared in the parent \
            container is accessed in the target routine.
        :raises TransformationError: if the shape of an array dummy argument \
            does not match that of the corresponding actual argument.

        '''
        super().validate(node, options=options)

        # The node should be a Call.
        if not isinstance(node, Call):
            raise TransformationError(
                f"The target of the InlineTrans transformation "
                f"should be a Call but found '{type(node).__name__}'.")

        name = node.routine.name

        # Check that we can find the source of the routine being inlined.
        routine = self._find_routine(node)

        if not routine.children or isinstance(routine.children[0], Return):
            # An empty routine is fine.
            return

        return_stmts = routine.walk(Return)
        if return_stmts:
            if len(return_stmts) > 1 or not isinstance(routine.children[-1],
                                                       Return):
                # Either there is more than one Return statement or there is
                # just one but it isn't the last statement of the Routine.
                raise TransformationError(
                    f"Routine '{name}' contains one or more "
                    f"Return statements and therefore cannot be inlined.")

        if routine.walk(CodeBlock):
            raise TransformationError(
                f"Routine '{name}' contains one or more "
                f"CodeBlocks and therefore cannot be inlined.")

        # Support for routines with named arguments is not yet implemented.
        # TODO #924.
        for arg in node.argument_names:
            if arg:
                raise TransformationError(
                    f"Routine '{routine.name}' cannot be inlined because it "
                    f"has a named argument '{arg}' (TODO #924).")

        # Check for symbol-naming clashes that we can't handle.
        table = node.scope.symbol_table
        routine_table = routine.symbol_table

        # We can't handle a clash between (apparently) different symbols that
        # share a name but are imported from different containers.
        callsite_imports = table.imported_symbols
        routine_imports = routine_table.imported_symbols
        routine_import_names = [sym.name for sym in routine_imports]
        for sym in callsite_imports:
            if sym.name in routine_import_names:
                routine_sym = routine_table.lookup(sym.name)
                if (routine_sym.interface.container_symbol.name !=
                        sym.interface.container_symbol.name):
                    raise TransformationError(
                        f"Routine '{routine.name}' imports '{sym.name}' from "
                        f"Container "
                        f"'{routine_sym.interface.container_symbol.name}' but "
                        f"the call site has an import of a symbol with the "
                        f"same name from Container "
                        f"'{sym.interface.container_symbol.name}'.")

        # Check for unresolved symbols or for any accessed from the Container
        # containing the target routine.
        refs = routine.walk(Reference)
        for ref in refs:
            if ref.symbol.name not in routine_table:
                sym = routine_table.lookup(ref.symbol.name)
                if sym.is_unresolved:
                    raise TransformationError(
                        f"Routine '{routine.name}' cannot be inlined because "
                        f"it accesses an un-resolved variable "
                        f"'{ref.symbol.name}'.")
                if not sym.is_import:
                    raise TransformationError(
                        f"Routine '{routine.name}' cannot be inlined because "
                        f"it accesses variable '{ref.symbol.name}' from its "
                        f"parent container.")

        # Check that the shape of any dummy array arguments are the same as
        # those at the call site.
        visitor = FortranWriter()
        for dummy_arg, actual_arg in zip(routine_table.argument_list,
                                         node.children):
            dummy_rank = 0
            actual_rank = 0
            if not isinstance(actual_arg, Reference):
                # TODO #1799 this really needs the `datatype` method to be
                # extended to support all nodes. For now we have to skip
                # anything that's not a Reference.
                continue

            if hasattr(dummy_arg.datatype, "shape"):
                dummy_rank = len(dummy_arg.datatype.shape)
            if hasattr(actual_arg.datatype, "shape"):
                actual_rank = len(actual_arg.datatype.shape)
            if dummy_rank != actual_rank:
                # It's OK to use the loop variable in the lambda definition
                # because if we get to this point then we're going to quit
                # the loop.
                # pylint: disable=cell-var-from-loop
                raise TransformationError(LazyString(
                        lambda: f"Cannot inline routine '{routine.name}' "
                        f"because it reshapes an argument: actual argument "
                        f"'{visitor(actual_arg)}' has rank {actual_rank} but "
                        f"the corresponding dummy argument, '{dummy_arg.name}'"
                        f", has rank {dummy_rank}"))
            if actual_rank:
                ranges = actual_arg.walk(Range)
                for rge in ranges:
                    ancestor_ref = rge.ancestor(Reference)
                    if ancestor_ref is not actual_arg:
                        # Have a range in an indirect access.
                        raise TransformationError(LazyString(
                            lambda: f"Cannot inline routine '{routine.name}' "
                            f"because argument '{visitor(actual_arg)}' has an "
                            f"array range in an indirect access (TODO #924)."))
                    if rge.step != _ONE:
                        # TODO #1646. We could resolve this problem by making
                        # a new array and copying the necessary values into it.
                        raise TransformationError(LazyString(
                            lambda: f"Cannot inline routine '{routine.name}' "
                            f"because one of its arguments is an array slice "
                            f"with a non-unit stride: '{visitor(actual_arg)}' "
                            f"(TODO #1646)"))

    @staticmethod
    def _find_routine(call_node):
        '''Searches for the definition of the routine that is being called by
        the supplied Call.

        :param call_node: the Call that is to be inlined.
        :type call_node: :py:class:`psyclone.psyir.nodes.Call`

        :returns: the PSyIR for the target routine.
        :rtype: :py:class:`psyclone.psyir.nodes.Routine`

        :raises InternalError: if the routine symbol is local but the \
            routine definition is not found.
        :raises TransformationError: if the routine definition cannot be found.

        '''
        name = call_node.routine.name
        routine_sym = call_node.routine

        if routine_sym.is_local:
            table = routine_sym.find_symbol_table(call_node)
            for routine in table.node.walk(Routine):
                if routine.name.lower() == name.lower():
                    return routine
            raise InternalError(
                f"Failed to find the source code of the local routine "
                f"'{routine_sym.name}'.")

        if routine_sym.is_unresolved:

            # First check for any wildcard imports and see if they can
            # be used to resolve the symbol.
            wildcard_names = []
            current_table = call_node.scope.symbol_table
            while current_table:
                for container_symbol in current_table.containersymbols:
                    if container_symbol.wildcard_import:
                        wildcard_names.append(container_symbol.name)
                        routine = InlineTrans._find_routine_in_container(
                            call_node, container_symbol)
                        if routine:
                            return routine
                current_table = current_table.parent_symbol_table()

            # Next check for any "raw" Routines, i.e. ones that are not
            # in a Container.  Such Routines would exist in the PSyIR
            # as a child of a FileContainer (if the PSyIR contains a
            # FileContainer). Note, if the PSyIR does contain a
            # FileContainer, it will be the root node of the PSyIR.
            for routine in call_node.root.children:
                if (isinstance(routine, Routine) and
                        routine.name.lower() == name.lower()):
                    return routine
            raise TransformationError(
                f"Failed to find the source code of the unresolved "
                f"routine '{name}' after trying wildcard imports from "
                f"{wildcard_names} and all routines that are not in "
                f"containers.")

        if routine_sym.is_import:
            container_symbol = routine_sym.interface.container_symbol
            routine = InlineTrans._find_routine_in_container(
                call_node, container_symbol)
            if routine:
                return routine
            raise TransformationError(
                f"Failed to find the source for routine '{routine_sym.name}' "
                f"imported from '{container_symbol.name}' and therefore "
                f"cannot inline it.")

        raise InternalError(
            f"Routine Symbol '{routine_sym.name}' is not local, "
            f"unresolved or imported.")

    @staticmethod
    def _find_routine_in_container(call_node, container_symbol):
        '''Searches for the definition of a routine that is being called by
        the supplied Call. If present, this routine must exist within a
        container specified by the supplied container symbol.

        :param call_node: the Call that is to be inlined.
        :type call_node: :py:class:`psyclone.psyir.nodes.Call`

        :param container_symbol: the symbol of the container to search.
        :type container_symbol: \
            :py:class:`psyclone.psyir.symbols.ContainerSymbol`

        :returns: the PSyIR for the target routine, if found.
        :rtype: Optional[:py:class:`psyclone.psyir.nodes.Routine`]

        '''
        # The required Routine will exist within a Container and
        # that Container could exist in the PSyIR as a child of a
        # FileContainer (if the PSyIR contains a
        # FileContainer). If the PSyIR does contain a
        # FileContainer, it will be the root node of the PSyIR.
        call_routine_sym = call_node.routine
        for container in call_node.root.children:
            if (isinstance(container, Container) and
                    container.name.lower() == container_symbol.name.lower()):
                for routine in container.children:
                    if (isinstance(routine, Routine) and
                            routine.name.lower() ==
                            call_routine_sym.name.lower()):
                        # Check this routine is public
                        routine_sym = container.symbol_table.lookup(
                            routine.name)
                        if routine_sym.visibility == Symbol.Visibility.PUBLIC:
                            return routine
                # The Container has been found but it does not contain
                # the expected Routine or the Routine is not public.

                # Look in the import that names the routine if there is one.
                table = container.symbol_table
                try:
                    routine_sym = table.lookup(call_routine_sym.name)
                    if routine_sym.is_import:
                        child_container_symbol = \
                            routine_sym.interface.container_symbol
                        return (InlineTrans._find_routine_in_container(
                            call_node, child_container_symbol))
                except KeyError:
                    pass

                # Look in any wildcard imports.
                for child_container_symbol in table.containersymbols:
                    if child_container_symbol.wildcard_import:
                        result = InlineTrans._find_routine_in_container(
                            call_node, child_container_symbol)
                        if result:
                            return result
                # The required Symbol was not found in the Container.
                return None
        # The specified Container was not found in the PSyIR.
        return None


# For AutoAPI auto-documentation generation.
__all__ = ["InlineTrans"]
