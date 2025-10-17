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
# Authors: A. R. Porter, R. W. Ford, A. Chalk and S. Siso, STFC Daresbury Lab

'''
This module contains the InlineTrans transformation.

'''

from typing import Dict, List, Optional

from psyclone.core import SymbolicMaths
from psyclone.errors import LazyString, InternalError
from psyclone.psyGen import Kern, Transformation
from psyclone.psyir.nodes import (
    ArrayReference, ArrayOfStructuresReference, BinaryOperation, Call,
    CodeBlock, Container, DataNode, FileContainer, IfBlock, IntrinsicCall,
    Literal, Loop, Node, Range, Routine, Reference, Return, Schedule,
    ScopingNode, Statement, StructureMember, StructureReference)
from psyclone.psyir.nodes.array_mixin import ArrayMixin
from psyclone.psyir.symbols import (
    ArrayType,
    BOOLEAN_TYPE,
    DataSymbol,
    INTEGER_TYPE,
    StructureType,
    SymbolError,
    UnresolvedType,
    UnsupportedType,
    UnsupportedFortranType,
)
from psyclone.psyir.transformations.reference2arrayrange_trans import (
    Reference2ArrayRangeTrans)
from psyclone.psyir.transformations.transformation_error import (
    TransformationError)
from psyclone.psyir.nodes.call import CallMatchingArgumentsNotFound
from psyclone.utils import transformation_documentation_wrapper


_ONE = Literal("1", INTEGER_TYPE)


@transformation_documentation_wrapper
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

    The target of the call must already be present in the same Container
    (module) as the call site. This may be achieved using
    KernelModuleInlineTrans.

    .. warning::
        Routines/calls with any of the following characteristics are not
        supported and will result in a TransformationError:

        * the routine contains an early Return statement;
        * the routine contains a variable with UnknownInterface;
        * the routine contains a variable with StaticInterface;
        * the routine contains an UnsupportedType variable with
          ArgumentInterface;
        * the shape of any array arguments as declared inside the routine does
          not match the shape of the arrays being passed as arguments;
        * the routine accesses an un-resolved symbol;
        * the routine accesses a symbol declared in the Container to which it
          belongs.

        Some of these restrictions will be lifted by #924.

    '''
    def apply(self,
              node: Call,
              routine: Optional[Routine] = None,
              use_first_callee_and_no_arg_check: bool = False,
              permit_codeblocks: bool = False,
              permit_unsupported_type_args: bool = False,
              **kwargs
              ):
        '''
        Takes the body of the routine that is the target of the supplied
        call and replaces the call with it.

        :param node: the Call node to inline.
        :param routine: Optional Routine to be inlined. (By default, PSyclone
            will search for a target routine with a matching signature).
        :param use_first_callee_and_no_arg_check: if True, simply use the
            first potential callee routine. No argument type-checking is
            performed.
        :param permit_codeblocks: If `False` (the default), raise an Exception
            if the target routine contains a CodeBlock.
        :param permit_unsupported_type_args: If `True` then the target routine
            is permitted to have arguments of UnsupportedType.

        :raises InternalError: if the merge of the symbol tables fails.
            In theory this should never happen because validate() should
            catch such a situation.

        '''
        self.validate(
            node, routine=routine,
            use_first_callee_and_no_arg_check=(
                use_first_callee_and_no_arg_check),
            permit_codeblocks=permit_codeblocks,
            permit_unsupported_type_args=permit_unsupported_type_args)

        # The table associated with the scoping region holding the Call.
        table = node.ancestor(Routine).symbol_table
        if not routine:
            # No target Routine has been provided so we search for one with
            # a matching signature.
            (orig_routine, arg_match_list) = node.get_callee(
                use_first_callee_and_no_arg_check=(
                    use_first_callee_and_no_arg_check))
        else:
            # Target Routine supplied to this transformation directly.
            orig_routine = routine
            arg_match_list = node.get_argument_map(orig_routine)

        if not orig_routine.children or isinstance(orig_routine.children[0],
                                                   Return):
            # Called routine is empty so just remove the call.
            node.detach()
            return

        # Ensure we don't modify the original Routine by working with a
        # copy of it.
        routine = orig_routine.copy()
        routine_table = routine.symbol_table

        # Next, we remove all optional arguments which are not used.

        # Step 1)
        # - Build lookup dictionary for all optional arguments:
        # - For all `PRESENT(...)`:
        #   - Lookup variable in dictionary
        #   - Replace with `True` or `False`, depending on whether
        #     it's provided or not.
        self._optional_arg_resolve_present_intrinsics(
            routine, arg_match_list
        )

        # Step 2)
        # - For all If-Statements, handle constant conditions:
        #   - `True`: Replace If-Block with If-Body
        #   - `False`: Replace If-Block with Else-Body. If it doesn't exist
        #     just delete the if statement.
        self._optional_arg_eliminate_ifblock_if_const_condition(routine)

        # Construct lists of the nodes that will be inserted and all of the
        # References that they contain.
        new_stmts = []
        refs = []
        for child in routine.pop_all_children():
            new_stmts.append(child)
            refs.extend(new_stmts[-1].walk(Reference))

        # Shallow copy the symbols from the routine into the table at the
        # call site. This preserves any references to them.
        try:
            table.merge(routine_table,
                        symbols_to_skip=routine_table.argument_list[:])
        except SymbolError as err:
            raise InternalError(
                f"Error copying routine symbols to call site. This should "
                f"have been caught by the validate() method. Original error "
                f"was {err}") from err

        # When constructing new references to replace references to formal
        # args, we need to know whether any of the actual arguments are array
        # accesses. If they use 'array notation' (i.e. represent a whole array)
        # then they won't have index expressions and will have been captured
        # as a Reference.
        ref2arraytrans = Reference2ArrayRangeTrans()

        for child in node.arguments:
            try:
                # TODO #1858, this won't yet work for arrays inside structures.
                ref2arraytrans.apply(child, allow_call_arguments=True)
            except (TransformationError, ValueError):
                pass

        # Replace any references to formal arguments with copies of the
        # actual arguments.
        formal_args = routine_table.argument_list
        for ref in refs[:]:
            self._replace_formal_arg(
                ref, node, formal_args, routine_node=routine,
                use_first_callee_and_no_arg_check=(
                    use_first_callee_and_no_arg_check)
            )

        # Ensure any references to Symbols within the shape-specification of
        # other Symbols are updated. Note, we don't have to worry about
        # initialisation expressions here as they imply that a variable is
        # static. We don't support inlining routines with static variables.
        for sym in table.automatic_datasymbols:
            if not isinstance(sym.datatype, ArrayType):
                continue
            new_shape = []
            for dim in sym.datatype.shape:
                if isinstance(dim, ArrayType.Extent):
                    new_shape.append(dim)
                else:
                    lower = self._replace_formal_arg(
                        dim.lower, node, formal_args,
                        routine_node=routine,
                        use_first_callee_and_no_arg_check=(
                            use_first_callee_and_no_arg_check),
                    )
                    upper = self._replace_formal_arg(
                        dim.upper, node, formal_args,
                        routine_node=routine,
                        use_first_callee_and_no_arg_check=(
                            use_first_callee_and_no_arg_check),
                    )
                    new_shape.append(ArrayType.ArrayBounds(lower, upper))
            sym.datatype = ArrayType(sym.datatype.datatype, new_shape)

        for sym in table.datatypesymbols:
            if not isinstance(sym.datatype, StructureType):
                continue
            for name, ctype in sym.datatype.components.items():
                if isinstance(ctype.datatype, ArrayType):
                    new_shape = []
                    for dim in ctype.datatype.shape:
                        lower = self._replace_formal_arg(
                            dim.lower, node, formal_args,
                            routine_node=routine,
                            use_first_callee_and_no_arg_check=(
                                use_first_callee_and_no_arg_check),
                        )
                        upper = self._replace_formal_arg(
                            dim.upper, node, formal_args,
                            routine_node=routine,
                            use_first_callee_and_no_arg_check=(
                                use_first_callee_and_no_arg_check),
                        )
                        new_shape.append(ArrayType.ArrayBounds(lower, upper))
                    sym.datatype.components[name] = (
                        StructureType.ComponentType(
                            name=name,
                            datatype=ArrayType(ctype.datatype.datatype,
                                               new_shape),
                            visibility=ctype.visibility,
                            initial_value=ctype.initial_value))

        # Copy the nodes from the Routine into the call site.
        # TODO #924 - while doing this we should ensure that any References
        # to common/shared Symbols in the inlined code are updated to point
        # to the ones at the call site.
        if routine.return_symbol:
            # This is a function
            assignment = node.ancestor(Statement, excluding=Call)
            parent = assignment.parent
            idx = assignment.position-1
            for child in new_stmts:
                idx += 1
                parent.addchild(child, idx)
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

    def _optional_arg_resolve_present_intrinsics(self,
                                                 routine_node: Routine,
                                                 arg_match_list: List = []):
        """Replace PRESENT(some_argument) intrinsics in routine with constant
        booleans depending on whether `some_argument` has been provided
        (`True`) or not (`False`).

        :param routine_node: Routine to be inlined.
        :param arg_match_list: List of indices of arguments that match.
        """
        # We first build a lookup table of all optional arguments
        # to see whether it's present or not.
        optional_sym_present_dict: Dict[str, bool] = dict()
        for optional_arg_idx, datasymbol in enumerate(
            routine_node.symbol_table.datasymbols
        ):
            if not isinstance(datasymbol.datatype, UnsupportedFortranType):
                continue

            if ", OPTIONAL" not in str(datasymbol.datatype):
                continue

            sym_name = datasymbol.name.lower()
            optional_sym_present_dict[sym_name] = (optional_arg_idx in
                                                   arg_match_list)

        # Check if we have any optional arguments at all and if not, return
        if len(optional_sym_present_dict) == 0:
            return

        # Find all "PRESENT()" calls
        for intrinsic_call in routine_node.walk(IntrinsicCall):
            intrinsic_call: IntrinsicCall
            if intrinsic_call.intrinsic is IntrinsicCall.Intrinsic.PRESENT:

                present_arg: Reference = intrinsic_call.arguments[0]
                present_arg_name = present_arg.name.lower()
                is_present = optional_sym_present_dict.get(present_arg_name,
                                                           None)
                if is_present:
                    # The argument is present.
                    intrinsic_call.replace_with(Literal("true", BOOLEAN_TYPE))
                else:
                    intrinsic_call.replace_with(Literal("false", BOOLEAN_TYPE))

    def _optional_arg_eliminate_ifblock_if_const_condition(
        self, routine_node: Routine
    ):
        """Eliminate if-block where condition is a boolean Literal.

        :param routine_node: the Routine in which to eliminate if blocks.

        """
        def if_else_replace(main_schedule: Schedule,
                            if_block: IfBlock,
                            if_body_schedule: Schedule):
            """Little helper routine to eliminate one branch of an IfBlock.

            :param main_schedule: Schedule where if-branch is used
            :param if_block: If-else block itself
            :param if_body_schedule: The body of the if or else block

            """
            # Obtain index in main schedule
            idx = main_schedule.children.index(if_block)

            # Detach it
            if_block.detach()

            # Insert children of if-body schedule
            for child in if_body_schedule.pop_all_children():
                main_schedule.addchild(child, idx)
                idx += 1

        sym_maths = SymbolicMaths.get()

        for if_block in routine_node.walk(IfBlock):
            if_block: IfBlock

            condition = if_block.condition
            # Ensure any expressions in the condition are simplified.
            sym_maths.expand(condition)

            # Make sure we only handle a Boolean Literal as a condition
            # TODO #2802
            if not isinstance(condition, Literal):
                continue

            if condition.value == "true":
                # Only keep if_block
                if_else_replace(if_block.parent, if_block, if_block.if_body)

            else:
                # If there's an else block, replace if-condition with
                # else-block
                if not if_block.else_body:
                    if_block.detach()
                    continue

                # Only keep else block
                if_else_replace(if_block.parent, if_block, if_block.else_body)

    def _replace_formal_arg(
        self,
        ref: DataNode,
        call_node: Call,
        formal_args: List[DataSymbol],
        routine_node: Routine,
        use_first_callee_and_no_arg_check: bool = False,
    ) -> Reference:
        '''
        Recursively combines any References to formal arguments in the supplied
        PSyIR expression with the corresponding Reference (actual argument)
        from the call site to make a new Reference for use in the inlined code.
        If the supplied node is not a Reference to a formal argument then it is
        just returned (after we have recursed to any children).

        :param ref: the expression to update.
        :param call_node: the call site.
        :param formal_args: the formal arguments of the called routine.

        :returns: the replacement reference.

        '''
        if not isinstance(ref, Reference):
            # Recurse down in case this is e.g. an Operation or Range.
            for child in ref.children[:]:
                self._replace_formal_arg(
                    child, call_node, formal_args, routine_node,
                    use_first_callee_and_no_arg_check=(
                        use_first_callee_and_no_arg_check))
            return ref

        if ref.symbol not in formal_args:
            # The supplied reference is not to a formal argument.
            return ref

        # Lookup index in routine argument
        routine_arg_idx = formal_args.index(ref.symbol)

        if use_first_callee_and_no_arg_check:
            # We're not attempting to match argument types.
            actual_arg = call_node.arguments[routine_arg_idx]

        else:
            # Lookup index of actual argument
            # If this is an optional argument, but not used, this index lookup
            # shouldn't fail
            try:
                arg_match_list = call_node.get_argument_map(routine_node)
                actual_arg_idx = arg_match_list.index(routine_arg_idx)
            except ValueError as err:
                arg_list = routine_node.symbol_table.argument_list
                arg_name = arg_list[routine_arg_idx].name
                raise TransformationError(
                    f"Subroutine argument '{arg_name}' is not provided by "
                    f"'{call_node.debug_string().strip()}', but used in the "
                    f"subroutine. If this is correct code, this is likely due "
                    f"to some non-eliminated if-branches using `PRESENT(...)` "
                    f"as conditional (TODO #2802).") from err

            # Lookup the actual argument that corresponds to this formal
            # argument.
            actual_arg = call_node.arguments[actual_arg_idx]

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
        if type(ref) is Reference:
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
        if type(actual_arg) is Reference:
            ref.symbol = actual_arg.symbol
            return ref

        # Neither the actual or local references are simple, i.e. they
        # include array accesses and/or structure accesses.
        new_ref = self._replace_formal_struc_arg(
            actual_arg, ref, call_node, formal_args,
            routine_node=routine_node,
            use_first_callee_and_no_arg_check=(
                use_first_callee_and_no_arg_check))

        # If the local reference we are replacing has a parent then we must
        # ensure the parent's child list is updated. (It may not have a parent
        # if we are in the process of constructing a brand new reference.)
        if ref.parent:
            ref.replace_with(new_ref)
        return new_ref

    def _create_inlined_idx(
        self,
        call_node: Call,
        formal_args: List[DataSymbol],
        local_idx: DataNode,
        decln_start: DataNode,
        actual_start: DataNode,
        routine_node: Routine,
        use_first_callee_and_no_arg_check: bool = False,
    ) -> DataNode:
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

        :param call_node: the Call that we are inlining.
        :param formal_args: the formal arguments of the routine being called.
        :param local_idx: a local array-index expression (i.e. appearing
            within the routine being inlined).
        :param decln_start: the lower bound of the corresponding array
            dimension, as declared inside the routine being inlined.
        :param actual_start: the lower bound of the corresponding array
            dimension, as defined at the call site.
        :param routine_node: the Routine that is being inlined.
        :param use_first_callee_and_no_arg_check: use the first potential
            callee that is found without checking for argument types. Defaults
            to False.

        :returns: PSyIR for the corresponding inlined array index.

        '''
        if isinstance(local_idx, Range):
            lower = self._create_inlined_idx(
                call_node,
                formal_args,
                local_idx.start,
                decln_start,
                actual_start,
                routine_node=routine_node,
                use_first_callee_and_no_arg_check=(
                    use_first_callee_and_no_arg_check)
            )
            upper = self._create_inlined_idx(
                call_node,
                formal_args,
                local_idx.stop,
                decln_start,
                actual_start,
                routine_node=routine_node,
                use_first_callee_and_no_arg_check=(
                    use_first_callee_and_no_arg_check)
            )
            step = self._replace_formal_arg(
                local_idx.step,
                call_node,
                formal_args,
                routine_node=routine_node,
                use_first_callee_and_no_arg_check=(
                    use_first_callee_and_no_arg_check)
            )
            return Range.create(lower.copy(), upper.copy(), step.copy())

        uidx = self._replace_formal_arg(
            local_idx,
            call_node,
            formal_args,
            routine_node=routine_node,
            use_first_callee_and_no_arg_check=use_first_callee_and_no_arg_check
        )
        if decln_start == actual_start:
            # If the starting indices in the actual and formal arguments are
            # the same then we don't need to shift the index.
            return uidx

        ustart = self._replace_formal_arg(
            decln_start,
            call_node,
            formal_args,
            routine_node=routine_node,
            use_first_callee_and_no_arg_check=use_first_callee_and_no_arg_check
        )
        start_sub = BinaryOperation.create(BinaryOperation.Operator.SUB,
                                           uidx.copy(), ustart.copy())
        return BinaryOperation.create(BinaryOperation.Operator.ADD,
                                      start_sub, actual_start.copy())

    def _update_actual_indices(
        self,
        actual_arg: ArrayMixin,
        local_ref: Reference,
        call_node: Call,
        formal_args: List[DataSymbol],
        routine_node: Routine,
        use_first_callee_and_no_arg_check: bool = False,
    ) -> List[Node]:
        '''
        Create a new list of indices for the supplied actual argument
        (ArrayMixin) by replacing any Ranges with the appropriate expressions
        from the local access in the called routine. If there are no Ranges
        then the returned list of indices just contains copies of the inputs.

        :param actual_arg: (part of) the actual argument to the routine.
        :param local_ref: the corresponding Reference in the called routine.
        :param call_node: the call site.
        :param formal_args: the formal arguments of the called routine.
        :param routine_node: the Routine being inlined.
        :param use_first_callee_and_no_arg_check: use the first potential
            callee that is found without checking for argument types. Defaults
            to False.

        :returns: new indices for the actual argument.

        '''
        if isinstance(local_ref, ArrayMixin):
            local_indices = [idx.copy() for idx in local_ref.indices]
        # Get the locally-declared shape of the formal argument in case its
        # bounds are shifted relative to the caller.
        if isinstance(local_ref.symbol.datatype, ArrayType):
            local_decln_shape = local_ref.symbol.datatype.shape
        else:
            local_decln_shape = []

        new_indices = [idx.copy() for idx in actual_arg.indices]
        local_idx_posn = 0
        for pos, idx in enumerate(new_indices[:]):

            if not isinstance(idx, Range):
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
                              ArrayType.ArrayBounds):
                    # The formal argument declaration has a shape.
                    local_shape = local_decln_shape[local_idx_posn]
                    local_decln_start = local_shape.lower
                    if isinstance(local_decln_start, Node):
                        # Ensure any references to formal arguments within
                        # the declared array lower bound are updated.
                        local_decln_start = self._replace_formal_arg(
                            local_decln_start,
                            call_node,
                            formal_args,
                            routine_node=routine_node,
                            use_first_callee_and_no_arg_check=(
                                use_first_callee_and_no_arg_check),
                        )
                elif (local_decln_shape[local_idx_posn] ==
                      ArrayType.Extent.DEFERRED):
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
                    new = Range.create(local_shape.lower.copy(),
                                       local_shape.upper.copy())
                    new_indices[pos] = self._create_inlined_idx(
                        call_node,
                        formal_args,
                        new,
                        local_decln_start,
                        actual_start,
                        routine_node=routine_node,
                        use_first_callee_and_no_arg_check=(
                            use_first_callee_and_no_arg_check),
                    )
            else:
                # Otherwise, the local index expression replaces the Range.
                new_indices[pos] = self._create_inlined_idx(
                    call_node,
                    formal_args,
                    local_indices[local_idx_posn],
                    local_decln_start,
                    actual_start,
                    routine_node=routine_node,
                    use_first_callee_and_no_arg_check=(
                        use_first_callee_and_no_arg_check),
                )
            # Each Range corresponds to one dimension of the formal argument.
            local_idx_posn += 1
        return new_indices

    def _replace_formal_struc_arg(
        self,
        actual_arg: Reference,
        ref: Reference,
        call_node: Call,
        formal_args: List[DataSymbol],
        routine_node: Routine,
        use_first_callee_and_no_arg_check: bool = False,
    ) -> Reference:
        '''
        Called by _replace_formal_arg() whenever a formal or actual argument
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
        :param ref: the corresponding reference to a formal argument.
        :param call_node: the call site.
        :param formal_args: the formal arguments of the called routine.
        :param routine_node: Routine node to be inlined.
        :param use_first_callee_and_no_arg_check: Just use the first possible
            callee and do not check argument types. Defaults to False.

        :returns: the replacement reference.

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
                    cursor,
                    ref,
                    call_node,
                    formal_args,
                    routine_node=routine_node,
                    use_first_callee_and_no_arg_check=(
                        use_first_callee_and_no_arg_check),
                )
                members.append((cursor.name, new_indices))
            else:
                members.append(cursor.name)

            if not isinstance(cursor, (StructureMember, StructureReference)):
                break
            cursor = cursor.member

        # TODO #1858 - once we support converting structure accesses into
        # explicit array accesses, we can put back the testing in
        # inline_trans_test.py that covers this code and remove the pragma:
        if not actual_arg.walk(Range) and local_indices:  # pragma: no cover
            # There are no Ranges in the actual argument but the local
            # reference is an array access.
            # Create updated index expressions for that access.
            new_indices = []
            for idx in local_indices:
                new_indices.append(
                    self._replace_formal_arg(
                        idx.copy(),
                        call_node,
                        formal_args,
                        routine_node,
                        use_first_callee_and_no_arg_check=(
                            use_first_callee_and_no_arg_check),
                    )
                )
            # Replace the last entry in the `members` list with a new array
            # access.
            members[-1] = (cursor.name, new_indices)

        # We now walk down the *local* access, skipping its head (as that is
        # replaced by the actual arg). We don't need to worry about updating
        # index expressions in the actual argument as they are independent of
        # any array accesses within a structure passed as a formal argument.
        cursor = ref
        while isinstance(cursor, (StructureReference, StructureMember)):
            cursor = cursor.member
            if isinstance(cursor, ArrayMixin):
                new_indices = []
                for idx in cursor.indices:
                    # Update each index expression in case it refers to
                    # formal arguments.
                    new_indices.append(
                        self._replace_formal_arg(
                            idx.copy(),
                            call_node,
                            formal_args,
                            routine_node=routine_node,
                            use_first_callee_and_no_arg_check=(
                                use_first_callee_and_no_arg_check),
                        )
                    )
                members.append((cursor.name, new_indices))
            else:
                members.append(cursor.name)

        # Finally, construct the new Reference using the information we've
        # collected from both the actual argument and local access.
        if len(members) > 1:
            # We have some form of Structure reference.
            if isinstance(members[0], tuple):
                # Root of access is an array access.
                return ArrayOfStructuresReference.create(actual_arg.symbol,
                                                         members[0][1],
                                                         members[1:])
            return StructureReference.create(actual_arg.symbol, members[1:])

        # Just an array reference.
        return ArrayReference.create(actual_arg.symbol, members[0][1])

    def validate(
                self,
                node: Call,
                **kwargs
            ) -> None:
        '''
        Checks that the supplied node is a valid target for inlining.

        :param node: the target PSyIR Call to inline.

        :raises TransformationError: if the supplied node is not a Call, is
            an IntrinsicCall or a call to a PSyclone-generated or ELEMENTAL
            routine.
        :raises TransformationError: if the routine body contains a Return
            that is not the first or last statement.
        :raises TransformationError: if the routine body contains a CodeBlock
            and `permit_codeblocks` is not True.
        :raises TransformationError: if the routine contains an ALLOCATE (as we
            don't support adding the required DEALLOCATE).
        :raises TransformationError: if the called routine has a named
            argument.
        :raises TransformationError: if the call-site is not within a Routine.
        :raises TransformationError: if any of the variables declared within
            the called routine are of UnknownInterface.
        :raises TransformationError: if any of the variables declared within
            the called routine have a StaticInterface.
        :raises TransformationError: if any of the subroutine arguments is of
            UnsupportedType.
        :raises TransformationError: if a symbol of a given name is imported
            from different containers at the call site and within the routine.
        :raises TransformationError: if the routine accesses an un-resolved
            symbol.
        :raises TransformationError: if the number of arguments in the call
            does not match the number of formal arguments of the routine.
        :raises TransformationError: if a symbol declared in the parent
            container is accessed in the target routine.
        :raises TransformationError: if the shape of an array formal argument
            does not match that of the corresponding actual argument.
        :raises TransformationError: if one of the declaratoins in the routine
            depends on an argument that is written to prior to the call.
        :raises InternalError: if an unhandled Node type is returned by
            Reference.previous_accesses().

        '''
        self.validate_options(**kwargs)
        super().validate(node, **kwargs)
        use_first_callee_and_no_arg_check = self.get_option(
            "use_first_callee_and_no_arg_check", **kwargs)
        permit_unsupported_type_args = self.get_option(
            "permit_unsupported_type_args", **kwargs)
        permit_codeblocks = self.get_option("permit_codeblocks", **kwargs)
        routine = self.get_option("routine", **kwargs)

        # The node should be a Call.
        if not isinstance(node, Call):
            raise TransformationError(
                f"The target of the InlineTrans transformation "
                f"should be a Call but found '{type(node).__name__}'.")

        if isinstance(node, IntrinsicCall):
            raise TransformationError(
                f"Cannot inline an IntrinsicCall ('{node.routine.name}')")
        name = node.routine.name

        # The call site must be within a Routine (i.e. not detached)
        parent_routine = node.ancestor(Routine)
        if not parent_routine:
            raise TransformationError(
                f"Routine '{name}' cannot be inlined because the call site "
                f"('{node.debug_string().strip()}') is not inside a Routine.")

        if routine is None:
            # No target routine has been supplied so we must identify it.
            try:
                (routine, arg_match_list) = node.get_callee(
                    use_first_callee_and_no_arg_check=(
                        use_first_callee_and_no_arg_check))
            except (
                CallMatchingArgumentsNotFound,
                NotImplementedError,
                FileNotFoundError,
                SymbolError,
                TransformationError,
            ) as err:
                raise TransformationError(
                    f"Cannot inline routine '{name}' because the target of the"
                    f" call cannot be found:\n{str(err)}"
                ) from err

        else:
            arg_match_list = node.get_argument_map(routine)

        if not routine.children or isinstance(routine.children[0], Return):
            # An empty routine is fine.
            return

        if node.is_elemental:
            raise TransformationError(
                f"Routine '{name}' is elemental and inlining such routines is "
                f"not supported.")

        # We only inline a Call that is within a Container (not a
        # FileContainer) if the target routine is already within the same
        # Container. If it isn't, KernelModuleInlineTrans should be used as
        # this performs various checks to make sure it's safe to bring in the
        # routine.
        callsite_contr = node.ancestor(Container, excluding=FileContainer)
        if callsite_contr:
            # The call site is within a Container.
            if callsite_contr is not routine.ancestor(Container):
                raise TransformationError(
                    f"Routine '{name}' is not in the same Container as the "
                    f"call site ('{callsite_contr.name}') and therefore cannot"
                    f" be inlined. (Try using KernelModuleInlineTrans to bring"
                    f" the routine into the same Container first.)")

        return_stmts = routine.walk(Return)
        if return_stmts:
            if len(return_stmts) > 1 or not isinstance(routine.children[-1],
                                                       Return):
                # Either there is more than one Return statement or there is
                # just one but it isn't the last statement of the Routine.
                raise TransformationError(
                    f"Routine '{name}' contains one or more "
                    f"Return statements and therefore cannot be inlined.")

        if not permit_codeblocks and routine.walk(CodeBlock):
            # N.B. we allow the user to specify the "permit_codeblocks" option
            # to allow CodeBlocks to be included.
            raise TransformationError(
                f"Routine '{name}' contains one or more CodeBlocks and "
                f"therefore cannot be inlined. (If you are confident that the "
                f"code may safely be inlined despite this then use the "
                f"`permit_codeblocks=True` argument to InlineTrans.apply() "
                f"to override.)")

        # At the moment, we can't inline a routine that allocates memory that
        # is local to it as we don't support adding any deallocates (that the
        # compiler would add automatically at the end of the routine).
        for intr in routine.walk(IntrinsicCall):
            if intr.intrinsic != IntrinsicCall.Intrinsic.ALLOCATE:
                continue
            for arg_name, arg in zip(intr.argument_names, intr.arguments):
                if arg_name:
                    continue
                sym = arg.symbol
                result = intr.scope.symbol_table.lookup(
                    sym.name, scope_limit=routine, otherwise=None)
                if result:
                    raise TransformationError(
                        f"Routine '{name}' contains an ALLOCATE for local "
                        f"variable '{sym.name}'. Inlining such a routine is "
                        f"not supported.")

        if not permit_unsupported_type_args:
            for scope in routine.walk(ScopingNode):
                routine_table2 = scope.symbol_table
                for sym in routine_table2.symbols:
                    # We don't inline symbols that have an UnsupportedType and
                    # are arguments since we don't know if a simple assignment
                    # if enough (e.g. pointers)
                    if sym.is_argument and isinstance(
                        sym.datatype, UnsupportedType
                    ):
                        if ", OPTIONAL" not in sym.datatype.declaration:
                            raise TransformationError(
                                f"Routine '{routine.name}' cannot be inlined "
                                f"because it contains a Symbol '{sym.name}' "
                                f"which is an Argument of UnsupportedType: "
                                f"'{sym.datatype.declaration}'")
                    # We don't inline symbols that have an UnknownInterface, as
                    # we don't know how they are brought into this scope.
                    if sym.is_unknown_interface:
                        raise TransformationError(
                            f"Routine '{routine.name}' cannot be inlined "
                            f"because it contains a Symbol '{sym.name}' with "
                            f"an UnknownInterface: "
                            f"'{sym.datatype.declaration}'. You may be able "
                            f"to work around this limitation by adding the "
                            f"name of the module containing this Symbol to "
                            f"RESOLVE_IMPORTS in the transformation script.")
                    # Check that there are no static variables in the routine
                    # (because we don't know whether the routine is called from
                    # other places).
                    if sym.is_static and not sym.is_constant:
                        raise TransformationError(
                            f"Routine '{routine.name}' cannot be inlined "
                            f"because it has a static (Fortran SAVE) "
                            f"interface for Symbol '{sym.name}'.")

        # Check for unresolved symbols or for any accessed from the Container
        # containing the target routine.
        try:
            routine.check_outer_scope_accesses(node, "routine",
                                               permit_unresolved=False)
        except SymbolError as err:
            raise TransformationError(
                f"Cannot inline '{routine.name}' because it accesses a "
                f"variable that is unresolved or declared in an outer scope: "
                f"{err.value}") from err

        routine_table = routine.symbol_table
        # Create a list of routine arguments that is actually used
        routine_arg_list = [
            routine_table.argument_list[i] for i in arg_match_list
        ]

        for routine_arg, actual_arg in zip(
            routine_arg_list, node.arguments
        ):
            self._validate_inline_of_call_and_routine_argument_pairs(
                call_node=node,
                call_arg=actual_arg,
                routine_node=routine,
                routine_arg=routine_arg
            )

        # Check for dependencies within the SymbolTable of the target
        # routine. If any of these are used to dimension a local
        # (automatic) array, are passed by argument and are written
        # to before the call then we can't perform inlining.
        for asym in routine.symbol_table.automatic_datasymbols:
            dependent_symbols = asym.get_all_accessed_symbols()
            for sym in dependent_symbols:
                if sym not in routine_table.argument_list:
                    # This dependency is not an argument to the routine.
                    continue
                actual_arg = node.arguments[
                    routine_table.argument_list.index(sym)]
                if not isinstance(actual_arg, Reference):
                    # The corresponding actual argument is not a Reference
                    # so cannot be modified prior to the call.
                    continue
                # What form does the dependence take?
                for prev in actual_arg.previous_accesses():
                    if prev is node or prev.parent is node:
                        # Skip the Call itself and any other arguments to
                        # the call.
                        continue
                    exprn = prev.ancestor(Statement, include_self=True)
                    stmt = exprn.debug_string().strip()
                    if isinstance(prev, (CodeBlock, Call, Kern, Loop)):
                        raise TransformationError(
                            f"Cannot inline routine '{routine.name}' "
                            f"because one or more of its declarations "
                            f"depends on '{sym.name}' which is passed by "
                            f"argument and may be written to before the "
                            f"call ('{stmt}').")
                    if isinstance(prev, Reference):
                        if prev.is_write:
                            raise TransformationError(
                                f"Cannot inline routine '{routine.name}' "
                                f"because one or more of its declarations "
                                f"depends on '{sym.name}' which is passed "
                                f"by argument and is assigned to before "
                                f"the call ('{stmt}').")
                        continue

                    raise InternalError(
                        f"Unexpected node type ({type(prev).__name__}) "
                        f"returned from Reference.previous_accesses(). "
                        f"Expected a Call, CodeBlock, Kern, Loop or "
                        f"Reference.")

        return (routine, arg_match_list)

    def _validate_inline_of_call_and_routine_argument_pairs(
        self,
        call_node: Call,
        call_arg: DataNode,
        routine_node: Routine,
        routine_arg: DataSymbol
    ):
        """This function performs tests to see whether the inlining can
        cope with the specified call and corresponding dummy argument pair.

        :param call_node: The call used for inlining
        :param call_arg: The argument of a call
        :param routine: The routine to be inlined
        :param routine_arg: The argument of a routine

        :raises TransformationError: if the type of an actual argument is
            unknown and it corresponds to a formal argument that is an array.
        :raises TransformationError: if the shape of an actual argument does
            not match that of the corresponding formal argument.
        :raises TransformationError: if an actual array argument has non-unit
            stride or an array slice in an indirect access.

        """
        # If the formal argument is an array with non-default bounds then
        # we also need to know the bounds of that array at the call site.
        if not isinstance(routine_arg.datatype, ArrayType):
            # Formal argument is not an array so we don't need to do any
            # further checks.
            return

        # We have an array argument. We are only able to check that the
        # argument is not re-shaped in the called routine if we have full
        # type information on the actual argument.
        # TODO #924. It would be useful if the `datatype` property was
        # a method that took an optional 'resolve' argument to indicate
        # that it should attempt to resolve any UnresolvedTypes.
        if (isinstance(call_arg.datatype,
                       (UnresolvedType, UnsupportedType)) or
            (isinstance(call_arg.datatype, ArrayType) and
             isinstance(call_arg.datatype.intrinsic,
                        (UnresolvedType, UnsupportedType)))):
            raise TransformationError(
                f"Routine '{routine_node.name}' cannot be inlined because "
                f"the type of the actual argument "
                f"'{call_arg.debug_string()}' corresponding to an array"
                f" formal argument ('{routine_arg.name}') is unknown.")

        formal_rank = 0
        actual_rank = 0
        if isinstance(routine_arg.datatype, ArrayType):
            formal_shape = routine_arg.datatype.shape
            formal_rank = len(formal_shape)
        if isinstance(call_arg.datatype, ArrayType):
            actual_rank = len(call_arg.datatype.shape)
        if formal_rank != actual_rank:
            # It's OK to use the loop variable in the lambda definition
            # because if we get to this point then we're going to quit
            # the loop.
            # pylint: disable=cell-var-from-loop
            raise TransformationError(LazyString(
                    lambda: f"Cannot inline routine '{routine_node.name}' "
                    f"because it reshapes an argument: actual argument "
                    f"'{call_arg.debug_string()}' has rank {actual_rank}"
                    f" but the corresponding formal argument, "
                    f"'{routine_arg.name}', has rank {formal_rank}"))
        if actual_rank:
            ranges = call_arg.walk(Range)
            for rge in ranges:
                ancestor_ref = rge.ancestor(Reference)
                if ancestor_ref is not call_arg:
                    # Have a range in an indirect access.
                    # pylint: disable=cell-var-from-loop
                    raise TransformationError(LazyString(
                        lambda: f"Cannot inline routine '{routine_node.name}' "
                        f"because argument '{call_arg.debug_string()}' "
                        f"has an array range in an indirect access (TODO "
                        f"#924)."))
                if rge.step != _ONE:
                    # TODO #1646. We could resolve this problem by making
                    # a new array and copying the necessary values into it.
                    # pylint: disable=cell-var-from-loop
                    raise TransformationError(LazyString(
                        lambda: f"Cannot inline routine '{routine_node.name}' "
                        f"because one of its arguments is an array slice "
                        f"with a non-unit stride: "
                        f"'{call_arg.debug_string()}' (TODO #1646)"))


# For AutoAPI auto-documentation generation.
__all__ = ["InlineTrans"]
