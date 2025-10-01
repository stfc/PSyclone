# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2022-2025, Science and Technology Facilities Council.
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
# Author: A. R. Porter, STFC Daresbury Lab
# Modified: S. Siso, STFC Daresbury Lab

'''
This module contains the HoistLocalArraysTrans transformation.

'''

import copy

from psyclone.psyGen import Transformation
from psyclone.psyir.nodes import (Routine, Container, ArrayReference, Range,
                                  FileContainer, IfBlock, UnaryOperation,
                                  CodeBlock, ACCRoutineDirective, Literal,
                                  IntrinsicCall, BinaryOperation, Reference,
                                  DataNode)
from psyclone.psyir.symbols import (
    ArrayType, DataSymbol, DataTypeSymbol, INTEGER_TYPE, Symbol)
from psyclone.psyir.transformations.transformation_error import (
    TransformationError)


class HoistLocalArraysTrans(Transformation):
    '''This transformation takes a Routine and promotes any local arrays to
    the Container scope:

    >>> from psyclone.psyir.backend.fortran import FortranWriter
    >>> from psyclone.psyir.frontend.fortran import FortranReader
    >>> from psyclone.psyir.nodes import Assignment
    >>> from psyclone.psyir.transformations import HoistLocalArraysTrans
    >>> code = ("module test_mod\\n"
    ...         "contains\\n"
    ...         "  subroutine test_sub(n)\\n"
    ...         "  integer :: i,j,n\\n"
    ...         "  real :: a(n,n)\\n"
    ...         "  real :: value = 1.0\\n"
    ...         "  do i=1,n\\n"
    ...         "    do j=1,n\\n"
    ...         "      a(i,j) = value\\n"
    ...         "    end do\\n"
    ...         "  end do\\n"
    ...         "  end subroutine test_sub\\n"
    ...         "end module test_mod\\n")
    >>> psyir = FortranReader().psyir_from_source(code)
    >>> hoist = HoistLocalArraysTrans()
    >>> hoist.apply(psyir.walk(Routine)[0])
    >>> print(FortranWriter()(psyir).lower())
    module test_mod
      implicit none
      real, allocatable, dimension(:,:), private :: a
      public
    <BLANKLINE>
      public :: test_sub
    <BLANKLINE>
      contains
      subroutine test_sub(n)
        integer :: n
        integer :: i
        integer :: j
        real :: value = 1.0
    <BLANKLINE>
        if (.not.allocated(a) .or. ubound(a, 1) /= n .or. ubound(a, 2) /= n) \
then
          if (allocated(a)) then
            deallocate(a)
          end if
          allocate(a(1 : n, 1 : n))
        end if
        do i = 1, n, 1
          do j = 1, n, 1
            a(i,j) = value
          enddo
        enddo
    <BLANKLINE>
      end subroutine test_sub
    <BLANKLINE>
    end module test_mod
    <BLANKLINE>

    By default, the target routine will be rejected if it is found to contain
    an ACCRoutineDirective since this usually implies that the routine will be
    launched in parallel on the OpenACC device. This check can be disabled
    by setting 'allow_accroutine' to True in the `options` dictionary.

    '''
    def apply(self, node, options=None):
        '''Applies the transformation to the supplied Routine node,
        moving any local arrays up to Container scope and adding
        a suitable allocation when they are first accessed. If there
        are no local arrays or the supplied Routine is a program then
        this method does nothing.

        :param node: target PSyIR node.
        :type node: :py:class:`psyclone.psyir.nodes.Routine`
        :param options: a dictionary with options for transformations.
        :param bool options["allow_accroutine"]: permit the target routine \
            to contain an ACCRoutineDirective. These are forbidden by default \
            because their presence usually indicates that the routine will be \
            run in parallel on the OpenACC device.
        :type options: Optional[Dict[str, Any]]

        '''
        self.validate(node, options)

        if node.is_program:
            # Cannot hoist arrays out of a program so do nothing.
            return

        container = node.ancestor(Container)

        # Identify all arrays that are local to the target routine
        # (automatic interface) and not accessed within a CodeBlock.
        automatic_arrays = self._get_local_arrays(node)

        if not automatic_arrays:
            # No automatic arrays found so nothing to do.
            return

        # Get the reversed tags map so that we can lookup the tag (if any)
        # associated with the symbol being hoisted.
        tags_dict = node.symbol_table.get_reverse_tags_dict()

        for sym in automatic_arrays:
            # Check if the original is already an ALLOCATABLE variable
            already_allocatable = any(dim == ArrayType.Extent.DEFERRED
                                      for dim in sym.shape)

            # Find or Create the array reference that will be the argument to
            # the new memory allocation statement.
            if already_allocatable:
                # If it was already an allocatable, we should be able to find
                # the allocate statement.
                original_allocate = None
                not_supported = False
                for ref in node.walk(ArrayReference):
                    if (
                        isinstance(ref.parent, IntrinsicCall) and
                        (ref.parent.intrinsic ==
                            IntrinsicCall.Intrinsic.ALLOCATE) and
                        ref.symbol is sym
                    ):
                        if original_allocate is not None:
                            # This would be the second match, so just warn the
                            # user and skip this symbol
                            original_allocate.append_preceding_comment(
                                f"PSyclone warning: {self.name} found more "
                                f"than one ALLOCATE for this variable, but "
                                f"currently it just supports cases with "
                                f"single allocations")
                            not_supported = True
                            break
                        original_allocate = ref.parent
                        # alloc-options are captured as argument_names in the
                        # PSyIR
                        if any(original_allocate.argument_names):
                            original_allocate.append_preceding_comment(
                                f"PSyclone warning: {self.name} found an "
                                f"ALLOCATE with alloc-options, this is "
                                f"not supported")
                            not_supported = True
                            break
                        aref = ref.copy()
                        orig_shape = []
                        for child in aref.children:
                            if isinstance(child, Range):
                                lbound = child.start
                                ubound = child.stop
                            else:
                                lbound = Literal("1", INTEGER_TYPE)
                                ubound = child
                            orig_shape.append(
                                ArrayType.ArrayBounds(lbound, ubound)
                            )
                if not_supported or original_allocate is None:
                    continue

            else:
                # Keep a copy of the original shape of the array.
                orig_shape = sym.datatype.shape[:]
                # Modify the *existing* symbol so that any references to it
                # remain valid.
                new_type = copy.copy(sym.datatype)
                # pylint: disable=protected-access
                new_type._shape = len(orig_shape)*[ArrayType.Extent.DEFERRED]
                # pylint: enable=protected-access
                sym.datatype = new_type
                dim_list = [Range.create(dim.lower.copy(), dim.upper.copy())
                            for dim in orig_shape]
                aref = ArrayReference.create(sym, dim_list)

            # We must allow for the situation where there's a clash with a
            # symbol name already present at container scope. (The validate()
            # method will already have checked for tag clashes.)
            try:
                container.symbol_table.add(sym, tag=tags_dict.get(sym))
            except KeyError:
                new_name = container.symbol_table.next_available_name(
                    sym.name, other_table=node.symbol_table)
                node.symbol_table.rename_symbol(sym, new_name)
                container.symbol_table.add(sym, tag=tags_dict.get(sym))

            # Ensure that the promoted symbol is private to the container.
            sym.visibility = Symbol.Visibility.PRIVATE

            # Add a conditional expression to avoid repeating the allocation
            # if its already done
            allocated_expr = IntrinsicCall.create(
                    IntrinsicCall.Intrinsic.ALLOCATED,
                    [Reference(sym)])
            cond_expr = UnaryOperation.create(
                            UnaryOperation.Operator.NOT, allocated_expr)

            # Add runtime checks to verify that the boundaries haven't changed
            # (we skip literals as we know they can't have changed)
            for idx, dim in enumerate(orig_shape):
                if not isinstance(dim.lower, Literal):
                    expr = BinaryOperation.create(
                            BinaryOperation.Operator.NE,
                            IntrinsicCall.create(
                                IntrinsicCall.Intrinsic.LBOUND,
                                [Reference(sym),
                                 ("dim", Literal(str(idx+1), INTEGER_TYPE))]),
                            dim.lower.copy())
                    # We chain the new check to the already existing cond_expr
                    # which starts with the 'not allocated' condition added
                    # before this loop.
                    cond_expr = BinaryOperation.create(
                                    BinaryOperation.Operator.OR,
                                    cond_expr, expr)
                if not isinstance(dim.upper, Literal):
                    expr = BinaryOperation.create(
                            BinaryOperation.Operator.NE,
                            IntrinsicCall.create(
                                IntrinsicCall.Intrinsic.UBOUND,
                                [Reference(sym),
                                 ("dim", Literal(str(idx+1), INTEGER_TYPE))]),
                            dim.upper.copy())
                    # We chain the new check to the already existing cond_expr
                    # which starts with the 'not allocated' condition added
                    # before this loop.
                    cond_expr = BinaryOperation.create(
                                    BinaryOperation.Operator.OR,
                                    cond_expr, expr)

            if_stmt = IfBlock.create(cond_expr, [
                IntrinsicCall.create(IntrinsicCall.Intrinsic.ALLOCATE, [aref])
            ])
            # If any bound-check was added, also insert a deallocate statement
            if isinstance(cond_expr, BinaryOperation):
                if_stmt.if_body.addchild(
                    IfBlock.create(
                        allocated_expr.copy(),
                        [IntrinsicCall.create(
                            IntrinsicCall.Intrinsic.DEALLOCATE,
                            [Reference(sym)])]),
                    index=0
                )

            if already_allocatable:
                # Find and remove any deallocate statements
                for ic in node.walk(IntrinsicCall):
                    if ic.intrinsic == IntrinsicCall.Intrinsic.DEALLOCATE:
                        for ar in ic.arguments:
                            if isinstance(ar, Reference) and ar.symbol is sym:
                                self._remove_allocation_reference(ar)
                # Now insert a guarded allocate expression, and remove the
                # original one.
                original_allocate.parent.children.insert(
                        original_allocate.position, if_stmt)
                for ref in original_allocate.arguments:
                    if isinstance(ref, Reference) and ref.symbol is sym:
                        self._remove_allocation_reference(ref)

            else:
                # Insert the conditional allocation at the start of the
                # supplied routine.
                node.children.insert(0, if_stmt)

            # Finally, remove the hoisted symbols (and any associated tags)
            # from the routine scope.
            # TODO #898: Currently the SymbolTable.remove() method does not
            # support DataSymbols.
            # pylint: disable=protected-access
            del node.symbol_table._symbols[sym.name]
            tag = tags_dict.get(sym)
            if tag:
                del node.symbol_table._tags[tag]

    @staticmethod
    def _get_local_arrays(node):
        '''
        Identify all arrays that are local to the target routine, all their
        bounds/kind/type symbols are also local, do not represent a function's
        return value, are not constant. Also excludes any such arrays that are
        accessed within CodeBlocks or RESHAPE intrinsics.

        :param node: target PSyIR node.
        :type node: subclass of :py:class:`psyclone.psyir.nodes.Routine`

        :returns: symbols representing routine-local arrays.
        :rtype: list[:py:class:`psyclone.psyir.symbols.DataSymbol`]

        '''
        local_arrays = {}
        for sym in node.symbol_table.automatic_datasymbols:
            # Check that the array is not the functions return symbol, or
            # a constant or has other references in its type declaration.
            if (sym is node.return_symbol or not sym.is_array or
                    sym.is_constant):
                continue
            # Skip declarations that have dependent symbols which are not
            # local (the frontend already declares unclear symbols as local
            # when there is wildcard imports that could have brought them)
            # Shape symbols are fine because they will end up the allocate
            # statement, not the hoisted declaration
            if isinstance(sym.datatype.intrinsic, DataTypeSymbol):
                if sym.datatype.intrinsic.name in node.symbol_table:
                    sym.append_preceding_comment(
                        f"PSyclone warning: '{sym.name}' cannot be hoisted "
                        f"to the global scope as '"
                        f"{sym.datatype.intrinsic.name}'"
                        f" is not guaranteed to be a global symbol")
                    continue
            # Precision could include multiple symbols - handle in the same
            # way as for DataSymbol but check all of them.
            if isinstance(sym.datatype.precision, DataNode):
                failed = False
                for ref in sym.datatype.precision.walk(Reference):
                    if isinstance(ref.symbol, DataSymbol):
                        if ref.symbol.name in node.symbol_table:
                            sym.append_preceding_comment(
                                f"PSyclone warning: '{sym.name}' cannot "
                                f"be hoisted to the global scope as '"
                                f"{ref.symbol.name}'"
                                f" is not guaranteed to be a global symbol")
                            failed = True
                            break
                if failed:
                    continue

            # Check whether all of the bounds of the array are defined - an
            # allocatable array will have array dimensions of
            # ArrayType.Extent.DEFERRED
            if any(dim == ArrayType.Extent.DEFERRED for dim in sym.shape):
                local_arrays[sym.name] = sym
            if all(isinstance(dim, ArrayType.ArrayBounds)
                   for dim in sym.shape):
                local_arrays[sym.name] = sym

        # Exclude any arrays that are accessed within a CodeBlock (as they
        # may get renamed as part of the transformation).
        cblocks = node.walk(CodeBlock)
        all_names_in_cblock = set()
        for cblock in cblocks:
            cblock_names = set(nm.lower() for nm in cblock.get_symbol_names())
            array_names = set(nm.lower() for nm in local_arrays.keys())
            names_in_cblock = cblock_names.intersection(array_names)
            all_names_in_cblock.update(names_in_cblock)
            for name in names_in_cblock:
                del local_arrays[name]
        for name in all_names_in_cblock:
            sym = node.symbol_table.lookup(name)
            sym.append_preceding_comment(
                f"PSyclone warning: cannot hoist '{name}' to global "
                f"scope as it is accessed in a CodeBlock")

        for intrinsic in node.walk(IntrinsicCall):
            # Exclude arrays that are used in a RESHAPE expression
            if intrinsic.intrinsic == IntrinsicCall.Intrinsic.RESHAPE:
                for ref in intrinsic.walk(Reference):
                    if ref.symbol.name in local_arrays:
                        del local_arrays[ref.symbol.name]

        return list(local_arrays.values())

    def validate(self, node, options=None):
        '''Checks that the supplied node is a valid target for a hoist-
        local-arrays transformation. It must be a Routine that is within
        a Container (that is not a FileContainer).

        :param node: target PSyIR node.
        :type node: subclass of :py:class:`psyclone.psyir.nodes.Routine`
        :param options: any options for the transformation.
        :type options: Optional[Dict[str, Any]]

        :raises TransformationError: if the supplied node is not a Routine.
        :raises TransformationError: if the Routine is not within a Container \
            (that is not a FileContainer).
        :raises TransformationError: if the routine contains an OpenACC \
            routine directive and options['allow_accroutine'] is not True.
        :raises TransformationError: if any symbols corresponding to local \
            arrays have a tag that already exists in the table of the parent \
            Container.

        '''
        super().validate(node, options=options)

        # The node should be a Routine.
        if not isinstance(node, Routine):
            raise TransformationError(
                f"The target of the HoistLocalArraysTrans transformation "
                f"should be a Routine but found '{type(node).__name__}'.")

        if node.is_program:
            # We silently ignore routines that are programs - this
            # transformation will do nothing.
            return

        # The Routine must be within a Container (otherwise we have nowhere
        # to hoist any array declarations to).
        container = node.ancestor(Container)
        if not container:
            raise TransformationError(
                f"The supplied routine '{node.name}' should "
                f"be within a Container but none was found.")
        if isinstance(container, FileContainer):
            raise TransformationError(
                f"The supplied routine '{node.name}' should be within a "
                f"Container but the enclosing container is a "
                f"FileContainer (named '{container.name}').")

        if not (options and options.get("allow_accroutine")):
            if node.walk(ACCRoutineDirective):
                raise TransformationError(
                    f"The supplied routine '{node.name}' contains an ACC "
                    f"Routine directive which implies it will be run in "
                    f"parallel. Hoisting local arrays to global scope may "
                    f"create race conditions in this case. If this routine "
                    f"will be run in serial on the device then this check can "
                    f"be disabled by setting 'allow_accroutine' to "
                    f"True in the transformation options.")

        # Check for clashing tags in the container scope.
        auto_arrays = self._get_local_arrays(node)
        tags_dict = node.symbol_table.get_reverse_tags_dict()
        cont_tags_dict = container.symbol_table.tags_dict
        for sym in auto_arrays:
            tag = tags_dict.get(sym)
            if tag in cont_tags_dict:
                raise TransformationError(
                    f"The supplied routine '{node.name}' contains a local "
                    f"array '{sym.name}' with tag '{tag}' but this tag is "
                    f"also present in the symbol table of the parent "
                    f"Container (associated with variable "
                    f"'{cont_tags_dict[tag].name}').")

    def __str__(self):
        return "Hoist all local, automatic arrays to container scope."

    @staticmethod
    def _remove_allocation_reference(ref: Reference):
        ''' Remove the provided reference from its parent allocation or
        deallocation statement.

        These statements can have multiple references as arguments. If there
        are multiple arguments, just remove the provided reference. If there
        are no more arguments, remove the whole statement. If it was inside a
        condition with nothing else in it, remove the whole condition (because
        if they are single-line conditions they would become invalid).

        :param ref: the reference to delete.

        '''
        allocate_stmt = ref.parent
        if len(allocate_stmt.arguments) == 1:
            allocate_parent = allocate_stmt.parent
            allocate_stmt.detach()
            if len(allocate_parent.children) == 0:
                # Since this is only mandatory for single-line conditions we
                # don't need to iterate upwards as this cannot be nested
                if isinstance(allocate_parent.parent, IfBlock):
                    allocate_parent.parent.detach()
        else:
            for child in allocate_stmt.arguments:
                if child == ref:
                    child.detach()


# For Sphinx AutoAPI documentation generation
__all__ = ["HoistLocalArraysTrans"]
