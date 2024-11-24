# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2022-2024, Science and Technology Facilities Council.
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
from psyclone.psyGen import Transformation
from psyclone.psyir.nodes import (
    ArrayReference, ArrayOfStructuresReference, BinaryOperation, Call,
    CodeBlock, IntrinsicCall, Node, Range, Routine, Reference,
    Return, Literal, Statement, StructureMember, StructureReference)
from psyclone.psyir.nodes.array_mixin import ArrayMixin
from psyclone.psyir.symbols import (
    ArgumentInterface,
    ArrayType,
    DataSymbol,
    INTEGER_TYPE,
    StaticInterface,
    SymbolError,
    UnknownInterface,
    UnsupportedType,
    UnsupportedFortranType,
    IntrinsicSymbol,
)
from psyclone.psyir.transformations.reference2arrayrange_trans import (
    Reference2ArrayRangeTrans)
from psyclone.psyir.transformations.transformation_error import (
    TransformationError)

from typing import Dict, List

from psyclone.psyir.symbols import BOOLEAN_TYPE
from psyclone.psyir.symbols import ScalarType


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
        * the routine contains a variable with UnknownInterface;
        * the routine contains a variable with StaticInterface;
        * the routine contains an UnsupportedType variable with
          ArgumentInterface;
        * the routine has a named argument;
        * the shape of any array arguments as declared inside the routine does
          not match the shape of the arrays being passed as arguments;
        * the routine accesses an un-resolved symbol;
        * the routine accesses a symbol declared in the Container to which it
          belongs.

        Some of these restrictions will be lifted by #924.

    '''

    def __init__(self):
        # List of call-to-subroutine argument indices
        self._ret_arg_match_list: List[int] = None

        # Call to routine
        self._call_node: Call = None

        # Routine to be inlined for call
        self._routine_node: Routine = None

        from psyclone.psyir.tools import CallRoutineMatcher

        self._call_routine_matcher: CallRoutineMatcher = CallRoutineMatcher()

        # If 'True', don't inline if a code block is used within the
        # Routine.
        self._option_check_codeblocks: bool = True

        self._option_check_diff_container_clashes: bool = True
        self._option_check_diff_container_clashes_unres_types: bool = True
        self._option_check_resolve_imports: bool = True
        self._option_check_static_interface: bool = True
        self._option_check_array_type: bool = True
        self._option_check_argument_of_unsupported_type: bool = True
        self._option_check_argument_unresolved_symbols: bool = True

    def set_option(
        self,
        ignore_missing_modules: bool = None,
        check_argument_strict_array_datatype: bool = None,
        check_argument_matching: bool = None,

        check_inline_codeblocks: bool = None,
        check_diff_container_clashes: bool = None,
        check_diff_container_clashes_unres_types: bool = None,
        check_resolve_imports: bool = None,
        check_static_interface: bool = None,
        check_array_type: bool = None,
        check_argument_of_unsupported_type: bool = None,
        check_argument_unresolved_symbols: bool = None,
    ):
        """Set special options

        :param ignore_missing_modules: If `True`, raise ModuleNotFound if
            module is not available, defaults to None
        :type ignore_missing_modules: bool, optional
        :param check_argument_strict_array_datatype:
            If `True`, make strict checks for matching arguments of
            array data types.
            If disabled, it's sufficient that both arguments are of ArrayType.
            Then, no further checks are performed, defaults to None
        :type check_argument_strict_array_datatype: bool, optional
        :param check_argument_matching: If `True`, check for all arguments
            to match. If `False`, if no matching argument was found, take
            1st one in list. Defaults to None
        :type check_argument_matching: bool, optional
        :param check_inline_codeblocks: If `True`, raise Exception
            if encountering code blocks, defaults to None
        :type check_inline_codeblocks: bool, optional
        :param check_diff_container_clashes:
            If `True` and different symbols share a name but are imported
            from different containers, raise Exception.
        :type check_diff_container_clashes: bool, optional
        :param check_diff_container_clashes_unres_types: If `True`,
            raise Exception if unresolved types are clashing, defaults to None
        :type check_diff_container_clashes_unres_types: bool, optional
        :param check_resolve_imports: If `True`, also resolve imports,
            defaults to None
        :type check_resolve_imports: bool, optional
        :param check_static_interface:
            Check that there are no static variables in the routine
            (because we don't know whether the routine is called from
            other places). Defaults to None
        :type check_static_interface: bool, optional
        :param check_array_type: If `True` and argument is an array,
            check that inlining is working for this array type,
            defaults to None
        :type check_array_type: bool, optional
        :param check_argument_of_unsupported_type: If `True`,
            also perform checks (fail inlining) on arguments of
            unsupported type, defaults to None
        :type check_argument_of_unsupported_type: bool, optional
        :param check_argument_unresolved_symbols: If `True`,
            stop if encountering an unresolved symbol, defaults to None
        :type check_argument_unresolved_symbols: bool, optional
        """

        self._call_routine_matcher.set_option(
            ignore_missing_modules=ignore_missing_modules)
        self._call_routine_matcher.set_option(
            check_strict_array_datatype=(
                check_argument_strict_array_datatype)
            )
        self._call_routine_matcher.set_option(
                check_matching_arguments=check_argument_matching
            )

        if check_inline_codeblocks is not None:
            self._option_check_codeblocks = check_inline_codeblocks

        if check_diff_container_clashes is not None:
            self._option_check_diff_container_clashes = (
                check_diff_container_clashes)

        if check_diff_container_clashes_unres_types is not None:
            self._option_check_diff_container_clashes_unres_types = (
                check_diff_container_clashes_unres_types
            )

        if check_resolve_imports is not None:
            self._option_check_resolve_imports = check_resolve_imports

        if check_static_interface is not None:
            self._option_check_static_interface = check_static_interface

        if check_array_type is not None:
            self._option_check_array_type = check_array_type

        if check_argument_of_unsupported_type is not None:
            self._option_check_argument_of_unsupported_type = (
                check_argument_of_unsupported_type
            )

        if check_argument_unresolved_symbols is not None:
            self._option_check_argument_unresolved_symbols = (
                check_argument_unresolved_symbols
            )

    def apply(
        self, call_node: Call, routine_node: Routine = None, options=None
    ):
        """
        Takes the body of the routine that is the target of the supplied
        call and replaces the call with it.

        :param call_node: target PSyIR node.
        :type call_node: :py:class:`psyclone.psyir.nodes.Call`
        :param routine: PSyIR subroutine to be inlined.
                Default: Automatically determine subroutine (search)
        :type routine: :py:class:`psyclone.psyir.nodes.Routine`
        :param options: a dictionary with options for transformations.
        :type options: Optional[Dict[str, Any]]
        :param bool options["force"]: whether or not to permit the inlining
            of Routines containing CodeBlocks. Default is False.
        """

        # Validate that the inlining can also be accomplish.
        # This routine will also update
        # self.node_routine and self._ret_arg_match_list
        # with the routine to be inlined and the relation between the
        # arguments and to which routine arguments they are matched to.
        self.validate(call_node, routine_node=routine_node, options=options)

        # The table associated with the scoping region holding the Call.
        table = call_node.scope.symbol_table

        if not self._routine_node.children or isinstance(
            self._routine_node.children[0], Return
        ):
            # Called routine is empty so just remove the call.
            call_node.detach()
            return

        # Ensure we don't modify the original Routine by working with a
        # copy of it.
        self._routine_node = self._routine_node.copy()
        routine_table = self._routine_node.symbol_table

        # Next, we remove all optional arguments which are not used.
        # Step 1)
        # - Build lookup dictionary for all optional arguments:

        # - For all `PRESENT(...)`:
        #   - Lookup variable in dictionary
        #   - Replace with `True` or `False`, depending on whether
        #     it's provided or not.
        self._optional_arg_resolve_present_intrinsics()

        # Step 2)
        # - For all If-Statements, handle constant conditions:
        #   - `True`: Replace If-Block with If-Body
        #   - `False`: Replace If-Block with Else-Body. If it doesn't exist
        #     just delete the if statement.
        self._optional_arg_eliminate_ifblock_if_const_condition()

        # Construct lists of the nodes that will be inserted and all of the
        # References that they contain.
        new_stmts = []
        refs = []
        for child in self._routine_node.children:
            child: Node
            new_stmts.append(child.copy())
            refs.extend(new_stmts[-1].walk(Reference))

        # Shallow copy the symbols from the routine into the table at the
        # call site.
        table.merge(
            routine_table,
            symbols_to_skip=routine_table.argument_list[:],
            check_unresolved_symbols=(
                self._option_check_argument_unresolved_symbols),
        )

        # When constructing new references to replace references to formal
        # args, we need to know whether any of the actual arguments are array
        # accesses. If they use 'array notation' (i.e. represent a whole array)
        # then they won't have index expressions and will have been captured
        # as a Reference.
        ref2arraytrans = Reference2ArrayRangeTrans()

        for child in call_node.arguments:
            try:
                # TODO #1858, this won't yet work for arrays inside structures.
                ref2arraytrans.apply(child)
            except (TransformationError, ValueError):
                pass

        # Replace any references to formal arguments with copies of the
        # actual arguments.
        formal_args = routine_table.argument_list
        for ref in refs[:]:
            self._replace_formal_arg(ref, call_node, formal_args)

        # Store the Routine level symbol table and node's current scope
        # so we can merge symbol tables later if required.
        ancestor_table = call_node.ancestor(Routine).scope.symbol_table
        scope = call_node.scope

        # Copy the nodes from the Routine into the call site.
        # TODO #924 - while doing this we should ensure that any References
        # to common/shared Symbols in the inlined code are updated to point
        # to the ones at the call site.
        if isinstance(new_stmts[-1], Return):
            # If the final statement of the routine is a return then
            # remove it from the list.
            del new_stmts[-1]

        if self._routine_node.return_symbol:
            # This is a function
            assignment = call_node.ancestor(Statement)
            parent = assignment.parent
            idx = assignment.position-1
            for child in new_stmts:
                idx += 1
                parent.addchild(child, idx)
            table = parent.scope.symbol_table
            # Avoid a potential name clash with the original function
            table.rename_symbol(
                self._routine_node.return_symbol,
                table.next_available_name(
                    f"inlined_{self._routine_node.return_symbol.name}"
                ),
            )
            call_node.replace_with(Reference(self._routine_node.return_symbol))
        else:
            # This is a call
            parent = call_node.parent
            idx = call_node.position
            call_node.replace_with(new_stmts[0])
            for child in new_stmts[1:]:
                idx += 1
                parent.addchild(child, idx)

        # If the scope we merged the inlined function's symbol table into
        # is not a Routine scope then we now merge that symbol table into
        # the ancestor Routine. This avoids issues like #2424 when
        # applying ParallelLoopTrans to loops containing inlined calls.
        if ancestor_table is not scope.symbol_table:
            ancestor_table.merge(
                scope.symbol_table,
                check_unresolved_symbols=(
                    self._option_check_argument_unresolved_symbols))
            replacement = type(scope.symbol_table)()
            scope.symbol_table.detach()
            replacement.attach(scope)

    def _optional_arg_resolve_present_intrinsics(self):
        """Replace PRESENT(some_argument) intrinsics in routine with constant
        booleans depending on whether `some_argument` has been provided
        (`True`) or not (`False`).

        :rtype: None
        """
        # We first build a lookup table of all optional arguments
        # to see whether it's present or not.
        optional_sym_present_dict: Dict[str, bool] = dict()
        for optional_arg_idx, datasymbol in enumerate(
            self._routine_node.symbol_table.datasymbols
        ):
            if not isinstance(datasymbol.datatype, UnsupportedFortranType):
                continue

            if ", OPTIONAL" not in str(datasymbol.datatype):
                continue

            sym_name = datasymbol.name.lower()

            if optional_arg_idx not in self._ret_arg_match_list:
                optional_sym_present_dict[sym_name] = False
            else:
                optional_sym_present_dict[sym_name] = True

        # Check if we have any optional arguments at all and if not, return
        if len(optional_sym_present_dict) == 0:
            return

        # Find all "PRESENT()" calls
        for intrinsic_call in self._routine_node.walk(IntrinsicCall):
            intrinsic_call: IntrinsicCall
            if intrinsic_call.routine.name.lower() == "present":

                # The argument is in the 2nd child
                present_arg: Reference = intrinsic_call.children[1]
                present_arg_name = present_arg.name.lower()

                assert present_arg_name in optional_sym_present_dict

                if optional_sym_present_dict[present_arg_name]:
                    # The argument is present.
                    intrinsic_call.replace_with(Literal("true", BOOLEAN_TYPE))
                else:
                    intrinsic_call.replace_with(Literal("false", BOOLEAN_TYPE))

    def _optional_arg_eliminate_ifblock_if_const_condition(self):
        """Eliminate if-block if conditions are constant booleans.

        TODO: This also requires support of conditions containing logical
        expressions such as `(.true. .or. .false.)`
        TODO: This could also become a Psyclone transformation.

        :rtype: None
        """

        def if_else_replace(main_schedule, if_block, if_body_schedule):
            """Little helper routine to eliminate one branch of an IfBlock

            :param main_schedule: Schedule where if-branch is used
            :type main_schedule: Schedule
            :param if_block: If-else block itself
            :type if_block: IfBlock
            :param if_body_schedule: The body of the if or else block
            :type if_body_schedule: Schedule
            """

            from psyclone.psyir.nodes import Schedule

            assert isinstance(main_schedule, Schedule)
            assert isinstance(if_body_schedule, Schedule)

            # Obtain index in main schedule
            idx = main_schedule.children.index(if_block)

            # Detach it
            if_block.detach()

            # Insert childreen of if-body schedule
            for child in if_body_schedule.children:
                main_schedule.addchild(child.copy(), idx)
                idx += 1

        from psyclone.psyir.nodes import IfBlock

        for if_block in self._routine_node.walk(IfBlock):
            if_block: IfBlock

            condition = if_block.condition

            # Make sure we only handle a BooleanLiteral as a condition
            # TODO #2802
            if not isinstance(condition, Literal):
                continue

            # Check that it's a boolean Literal
            assert (
                condition.datatype.intrinsic
                is ScalarType.Intrinsic.BOOLEAN
            ), "Found non-boolean expression in conditional of if branch"

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

    def _replace_formal_arg(self, ref, call_node, formal_args):
        '''
        Recursively combines any References to formal arguments in the supplied
        PSyIR expression with the corresponding Reference (actual argument)
        from the call site to make a new Reference for use in the inlined code.
        If the supplied node is not a Reference to a formal argument then it is
        just returned (after we have recursed to any children).

        :param ref: the expression to update.
        :type ref: :py:class:`psyclone.psyir.nodes.Node`
        :param call_node: the call site.
        :type call_node: :py:class:`psyclone.psyir.nodes.Call`
        :param formal_args: the formal arguments of the called routine.
        :type formal_args: List[:py:class:`psyclone.psyir.symbols.DataSymbol`]

        :returns: the replacement reference.
        :rtype: :py:class:`psyclone.psyir.nodes.Reference`

        '''
        if not isinstance(ref, Reference):
            # Recurse down in case this is e.g. an Operation or Range.
            for child in ref.children[:]:
                self._replace_formal_arg(child, call_node, formal_args)
            return ref

        if ref.symbol not in formal_args:
            # The supplied reference is not to a formal argument.
            return ref

        # Lookup index in routine argument
        routine_arg_idx = formal_args.index(ref.symbol)

        # Lookup index of actual argument
        # If this is an optional argument, but not used, this index lookup
        # shouldn't fail
        try:
            actual_arg_idx = self._ret_arg_match_list.index(routine_arg_idx)
        except ValueError as err:
            arg_list = self._routine_node.symbol_table.argument_list
            arg_name = arg_list[routine_arg_idx].name
            raise TransformationError(
                f"Subroutine argument '{arg_name}' is not provided by call,"
                f" but used in the subroutine."
                f" If this is correct code, this is likely due to"
                f" some non-eliminated if-branches using `PRESENT(...)` as"
                f" conditional (TODO #2802).") from err

        # Lookup the actual argument that corresponds to this formal argument.
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
        new_ref = self._replace_formal_struc_arg(actual_arg, ref, call_node,
                                                 formal_args)
        # If the local reference we are replacing has a parent then we must
        # ensure the parent's child list is updated. (It may not have a parent
        # if we are in the process of constructing a brand new reference.)
        if ref.parent:
            ref.replace_with(new_ref)
        return new_ref

    def _create_inlined_idx(self, call_node, formal_args,
                            local_idx, decln_start, actual_start):
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
        :type call_node: :py:class:`psyclone.psyir.nodes.Call`
        :param formal_args: the formal arguments of the routine being called.
        :type formal_args: List[:py:class:`psyclone.psyir.symbols.DataSymbol`]
        :param local_idx: a local array-index expression (i.e. appearing \
            within the routine being inlined).
        :type local_idx: :py:class:`psyclone.psyir.nodes.Node`
        :param decln_start: the lower bound of the corresponding array \
            dimension, as declared inside the routine being inlined.
        :type decln_start: :py:class:`psyclone.psyir.nodes.Node`
        :param actual_start: the lower bound of the corresponding array \
            dimension, as defined at the call site.
        :type actual_start: :py:class:`psyclone.psyir.nodes.Node`

        :returns: PSyIR for the corresponding inlined array index.
        :rtype: :py:class:`psyclone.psyir.nodes.Node`

        '''
        if isinstance(local_idx, Range):
            lower = self._create_inlined_idx(call_node, formal_args,
                                             local_idx.start, decln_start,
                                             actual_start)
            upper = self._create_inlined_idx(call_node, formal_args,
                                             local_idx.stop, decln_start,
                                             actual_start)
            step = self._replace_formal_arg(local_idx.step, call_node,
                                            formal_args)
            return Range.create(lower.copy(), upper.copy(), step.copy())

        uidx = self._replace_formal_arg(local_idx, call_node, formal_args)
        if decln_start == actual_start:
            # If the starting indices in the actual and formal arguments are
            # the same then we don't need to shift the index.
            return uidx

        ustart = self._replace_formal_arg(decln_start, call_node, formal_args)
        start_sub = BinaryOperation.create(BinaryOperation.Operator.SUB,
                                           uidx.copy(), ustart.copy())
        return BinaryOperation.create(BinaryOperation.Operator.ADD,
                                      start_sub, actual_start.copy())

    def _update_actual_indices(self, actual_arg, local_ref,
                               call_node, formal_args):
        '''
        Create a new list of indices for the supplied actual argument
        (ArrayMixin) by replacing any Ranges with the appropriate expressions
        from the local access in the called routine. If there are no Ranges
        then the returned list of indices just contains copies of the inputs.

        :param actual_arg: (part of) the actual argument to the routine.
        :type actual_arg: :py:class:`psyclone.psyir.nodes.ArrayMixin`
        :param local_ref: the corresponding Reference in the called routine.
        :param call_node: the call site.
        :type call_node: :py:class:`psyclone.psyir.nodes.Call`
        :param formal_args: the formal arguments of the called routine.
        :type formal_args: List[:py:class:`psyclone.psyir.symbols.DataSymbol`]

        :returns: new indices for the actual argument.
        :rtype: List[:py:class:`psyclone.psyir.nodes.Node`]

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
                            local_decln_start, call_node, formal_args)
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
                        call_node, formal_args,
                        new, local_decln_start, actual_start)
            else:
                # Otherwise, the local index expression replaces the Range.
                new_indices[pos] = self._create_inlined_idx(
                    call_node, formal_args,
                    local_indices[local_idx_posn],
                    local_decln_start, actual_start)
            # Each Range corresponds to one dimension of the formal argument.
            local_idx_posn += 1
        return new_indices

    def _replace_formal_struc_arg(self, actual_arg, ref, call_node,
                                  formal_args):
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
        :type actual_arg: :py:class:`psyclone.psyir.nodes.Reference`
        :param ref: the corresponding reference to a formal argument.
        :type ref: :py:class:`psyclone.psyir.nodes.Reference`
        :param call_node: the call site.
        :type call_node: :py:class:`psyclone.psyir.nodes.Call`
        :param formal_args: the formal arguments of the called routine.
        :type formal_args: List[:py:class:`psyclone.psyir.symbols.DataSymbol`]

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
                    cursor, ref, call_node, formal_args)
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
                        idx.copy(), call_node, formal_args))
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
                            idx.copy(), call_node, formal_args))
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

    def _validate_inline_of_call_and_routine_argument_pairs(
        self,
        call_arg: DataSymbol,
        routine_arg: DataSymbol
    ) -> bool:
        """This function performs tests to see whether the
        inlining can cope with it.

        :param call_arg: The argument of a call
        :type call_arg: DataSymbol
        :param routine_arg: The argument of a routine
        :type routine_arg: DataSymbol

        :raises TransformationError: Raised if transformation can't be done

        :return: 'True' if checks are successful
        :rtype: bool
        """
        from psyclone.psyir.transformations.transformation_error import (
            TransformationError,
        )
        from psyclone.errors import LazyString
        from psyclone.psyir.nodes import Literal, Range
        from psyclone.psyir.symbols import (
            UnresolvedType,
            UnsupportedType,
            INTEGER_TYPE,
        )

        _ONE = Literal("1", INTEGER_TYPE)

        # If the formal argument is an array with non-default bounds then
        # we also need to know the bounds of that array at the call site.
        if not isinstance(routine_arg.datatype, ArrayType):
            # Formal argument is not an array so we don't need to do any
            # further checks.
            return True

        if not isinstance(call_arg, (Reference, Literal)):
            # TODO #1799 this really needs the `datatype` method to be
            # extended to support all nodes. For now we have to abort
            # if we encounter an argument that is not a scalar (according
            # to the corresponding formal argument) but is not a
            # Reference or a Literal as we don't know whether the result
            # of any general expression is or is not an array.
            # pylint: disable=cell-var-from-loop
            raise TransformationError(
                LazyString(
                    lambda: (
                        f"The call '{self._call_node.debug_string()}' "
                        "cannot be inlined because actual argument "
                        f"'{call_arg.debug_string()}' corresponds to a "
                        "formal argument with array type but is not a "
                        "Reference or a Literal."
                    )
                )
            )

        # We have an array argument. We are only able to check that the
        # argument is not re-shaped in the called routine if we have full
        # type information on the actual argument.
        # TODO #924. It would be useful if the `datatype` property was
        # a method that took an optional 'resolve' argument to indicate
        # that it should attempt to resolve any UnresolvedTypes.
        if self._option_check_array_type:
            if isinstance(
                call_arg.datatype, (UnresolvedType, UnsupportedType)
            ) or (
                isinstance(call_arg.datatype, ArrayType)
                and isinstance(
                    call_arg.datatype.intrinsic,
                    (UnresolvedType, UnsupportedType),
                )
            ):
                raise TransformationError(
                    f"Routine '{self._routine_node.name}' cannot be "
                    "inlined because the type of the actual argument "
                    f"'{call_arg.symbol.name}' corresponding to an array"
                    f" formal argument ('{routine_arg.name}') is unknown."
                )

            formal_rank = 0
            actual_rank = 0
            if isinstance(routine_arg.datatype, ArrayType):
                formal_rank = len(routine_arg.datatype.shape)
            if isinstance(call_arg.datatype, ArrayType):
                actual_rank = len(call_arg.datatype.shape)
            if formal_rank != actual_rank:
                # It's OK to use the loop variable in the lambda definition
                # because if we get to this point then we're going to quit
                # the loop.
                # pylint: disable=cell-var-from-loop
                raise TransformationError(
                    LazyString(
                        lambda: (
                            "Cannot inline routine"
                            f" '{self._routine_node.name}' because it"
                            " reshapes an argument: actual argument"
                            f" '{call_arg.debug_string()}' has rank"
                            f" {actual_rank} but the corresponding formal"
                            f" argument, '{routine_arg.name}', has rank"
                            f" {formal_rank}"
                        )
                    )
                )
            if actual_rank:
                ranges = call_arg.walk(Range)
                for rge in ranges:
                    ancestor_ref = rge.ancestor(Reference)
                    if ancestor_ref is not call_arg:
                        # Have a range in an indirect access.
                        # pylint: disable=cell-var-from-loop
                        raise TransformationError(
                            LazyString(
                                lambda: (
                                    "Cannot inline routine"
                                    f" '{self._routine_node.name}' because"
                                    " argument"
                                    f" '{call_arg.debug_string()}' has"
                                    " an array range in an indirect"
                                    " access #(TODO 924)."
                                )
                            )
                        )
                    if rge.step != _ONE:
                        # TODO #1646. We could resolve this problem by
                        # making a new array and copying the necessary
                        # values into it.
                        # pylint: disable=cell-var-from-loop
                        raise TransformationError(
                            LazyString(
                                lambda: (
                                    "Cannot inline routine"
                                    f" '{self._routine_node.name}' because"
                                    " one of its arguments is an array"
                                    " slice with a non-unit stride:"
                                    f" '{call_arg.debug_string()}' (TODO"
                                    " #1646)"
                                )
                            )
                        )

    def _validate_inline_of_call_and_routine(
                self,
                call_node: Call,
                routine_node: Routine,
                arg_index_list: List[int]
            ):
        """Performs various checks that the inlining is supported for the
        combination of the call's and routine's arguments.

        :param call_node: Call to be replaced by the inlined Routine
        :type call_node: Call
        :param routine_node: Routine to be inlined
        :type routine_node: Routine
        :param arg_index_list: Argument index list to match the arguments of
            the call to those of the routine in case of optional arguments.
        :type arg_index_list: List[int]
        :raises TransformationError: Arguments are not in a form to be inlined

        """

        name = call_node.routine.name

        if not routine_node.children or isinstance(
            routine_node.children[0], Return
        ):
            # An empty routine is fine.
            return

        return_stmts = routine_node.walk(Return)
        if return_stmts:
            if len(return_stmts) > 1 or not isinstance(
                routine_node.children[-1], Return
            ):
                # Either there is more than one Return statement or there is
                # just one but it isn't the last statement of the Routine.
                raise TransformationError(
                    f"Routine '{name}' contains one or more "
                    f"Return statements and therefore cannot be inlined.")

        if self._option_check_codeblocks:
            if routine_node.walk(CodeBlock):
                # N.B. we permit the user to specify the "force" option to
                # allow CodeBlocks to be included.
                raise TransformationError(
                    f"Routine '{name}' contains one or more CodeBlocks and "
                    "therefore cannot be inlined. (If you are confident that "
                    "the code may safely be inlined despite this then use "
                    "`check_codeblocks=False` to override.)"
                )

        table = call_node.scope.symbol_table
        routine_table = routine_node.symbol_table

        for sym in routine_table.datasymbols:
            # We don't inline symbols that have an UnsupportedType and are
            # arguments since we don't know if a simple assignment if
            # enough (e.g. pointers)
            if self._option_check_argument_of_unsupported_type:
                if isinstance(sym.interface, ArgumentInterface):
                    if isinstance(sym.datatype, UnsupportedType):
                        if ", OPTIONAL" not in sym.datatype.declaration:
                            raise TransformationError(
                                f"Routine '{routine_node.name}' cannot be"
                                " inlined because it contains a Symbol"
                                f" '{sym.name}' which is an Argument of"
                                " UnsupportedType:"
                                f" '{sym.datatype.declaration}'."
                            )
                # We don't inline symbols that have an UnknownInterface, as we
                # don't know how they are brought into this scope.
                if isinstance(sym.interface, UnknownInterface):
                    raise TransformationError(
                        f"Routine '{routine_node.name}' cannot be "
                        "inlined because it contains a Symbol "
                        f"'{sym.name}' with an UnknownInterface: "
                        f"'{sym.datatype.declaration}'."
                    )

            if self._option_check_static_interface:
                # Check that there are no static variables in the routine
                # (because we don't know whether the routine is called from
                # other places).
                if (
                    isinstance(sym.interface, StaticInterface)
                    and not sym.is_constant
                ):
                    raise TransformationError(
                        f"Routine '{routine_node.name}' cannot be "
                        "inlined because it has a static (Fortran SAVE) "
                        f"interface for Symbol '{sym.name}'."
                    )

        if self._option_check_diff_container_clashes:
            # We can't handle a clash between (apparently) different symbols
            # that share a name but are imported from different containers.
            try:
                table.check_for_clashes(
                    routine_table,
                    symbols_to_skip=routine_table.argument_list[:],
                    check_unresolved_symbols=(
                        self._option_check_diff_container_clashes_unres_types
                    ),
                )
            except SymbolError as err:
                raise TransformationError(
                    "One or more symbols from routine "
                    f"'{routine_node.name}' cannot be added to the "
                    "table at the call site."
                ) from err

        # Check for unresolved symbols or for any accessed from the Container
        # containing the target routine.
        # TODO #1792 - kind parameters will not be found by simply doing
        # `walk(Reference)`. Although SymbolTable has the
        # `precision_datasymbols` property, this only returns those Symbols
        # that are used to define the precision of other Symbols in the same
        # table. If a precision symbol is only used within Statements then we
        # don't currently capture the fact that it is a precision symbol.
        ref_or_lits = routine_node.walk((Reference, Literal))
        # Check for symbols in any initial-value expressions
        # (including Fortran parameters) or array dimensions.
        for sym in routine_table.datasymbols:
            if sym.initial_value:
                ref_or_lits.extend(
                    sym.initial_value.walk((Reference, Literal))
                )
            if isinstance(sym.datatype, ArrayType):
                for dim in sym.shape:
                    if isinstance(dim, ArrayType.ArrayBounds):
                        if isinstance(dim.lower, Node):
                            ref_or_lits.extend(
                                dim.lower.walk(Reference, Literal)
                            )
                        if isinstance(dim.upper, Node):
                            ref_or_lits.extend(
                                dim.upper.walk(Reference, Literal)
                            )
        # Keep a reference to each Symbol that we check so that we can avoid
        # repeatedly checking the same Symbol.
        _symbol_cache = set()
        for lnode in ref_or_lits:
            if isinstance(lnode, Literal):
                if not isinstance(lnode.datatype.precision, DataSymbol):
                    continue
                sym = lnode.datatype.precision
            else:
                sym = lnode.symbol
            # If we've already seen this Symbol then we can skip it.
            if sym in _symbol_cache:
                continue
            _symbol_cache.add(sym)
            if isinstance(sym, IntrinsicSymbol):
                continue

            if self._option_check_resolve_imports:
                # We haven't seen this Symbol before.
                if sym.is_unresolved:
                    try:
                        routine_table.resolve_imports(symbol_target=sym)
                    except KeyError:
                        # The symbol is not (directly) imported into the symbol
                        # table local to the routine.
                        # pylint: disable=raise-missing-from
                        raise TransformationError(
                            f"Routine '{routine_node.name}' cannot be "
                            "inlined because it accesses variable "
                            f"'{sym.name}' and this cannot be found in any "
                            "of the containers directly imported into its "
                            "symbol table."
                        )
                else:
                    if sym.name not in routine_table:
                        raise TransformationError(
                            f"Routine '{routine_node.name}' cannot be "
                            "inlined because it accesses variable "
                            f"'{sym.name}' from its parent container."
                        )

        # Create a list of routine arguments that is actually used
        routine_arg_list = [
            routine_table.argument_list[i] for i in arg_index_list
        ]

        for routine_arg, call_arg in zip(
            routine_arg_list, call_node.arguments
        ):
            self._validate_inline_of_call_and_routine_argument_pairs(
                call_arg,
                routine_arg
            )

    def validate(
        self,
        call_node: Call,
        routine_node: Routine = None,
        options: Dict[str, str] = None,
    ):
        """
        Checks that the supplied node is a valid target for inlining.

        :param call_node: target PSyIR node.
        :type call_node: subclass of :py:class:`psyclone.psyir.nodes.Call`
        :param routine_node: Routine to inline.
            Default is to search for it.
        :type routine_node: subclass of :py:class:`Routine`
        :param options: a dictionary with options for transformations.
        :type options: Optional[Dict[str, Any]]
        :param bool options["force"]: whether or not to ignore any CodeBlocks
            in the candidate routine. Default is False.

        :raises TransformationError: if the supplied node is not a Call or is
            an IntrinsicCall or call to a PSyclone-generated routine.
        :raises TransformationError: if the routine has a return value.
        :raises TransformationError: if the routine body contains a Return
            that is not the first or last statement.
        :raises TransformationError: if the routine body contains a CodeBlock
            and the 'force' option is not True.
        :raises TransformationError: if the called routine has a named
            argument.
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

        """
        super().validate(call_node, options=options)

        self._call_node = call_node
        self._routine_node = routine_node

        # The node should be a Call.
        if not isinstance(self._call_node, Call):
            raise TransformationError(
                "The target of the InlineTrans transformation should"
                f" be a Call but found '{type(self._call_node).__name__}'."
            )

        call_name = self._call_node.routine.name
        if isinstance(self._call_node, IntrinsicCall):
            raise TransformationError(
                f"Cannot inline an IntrinsicCall ('{call_name}')"
            )

        # List of indices relating the call's arguments to the subroutine
        # arguments. This can be different due to
        # - optional arguments
        # - named arguments

        from psyclone.psyir.tools import CallMatchingArgumentsNotFoundError

        self._call_routine_matcher.set_call_node(self._call_node)

        if self._routine_node is None:
            # Check that we can find the source of the routine being inlined.
            # TODO #924 allow for multiple routines (interfaces).
            try:
                (self._routine_node, self._ret_arg_match_list) = \
                    self._call_routine_matcher.get_callee()
            except (
                CallMatchingArgumentsNotFoundError,
                NotImplementedError,
                FileNotFoundError,
                SymbolError,
                TransformationError,
            ) as err:
                raise TransformationError(
                    f"Cannot inline routine '{call_name}' because its source"
                    f" cannot be found:\n{str(err)}"
                ) from err

        else:
            # A routine has been provided.
            # Therefore, we just determine the matching argument list
            # if it matches.
            try:
                rm = self._call_routine_matcher
                rm.set_routine_node(self._routine_node)
                rm.set_option(
                    check_strict_array_datatype=False)
                self._ret_arg_match_list = (
                    rm.get_argument_routine_match_list()
                )
            except CallMatchingArgumentsNotFoundError as err:
                raise TransformationError(
                    "Routine's argument(s) don't match:\n"+str(err)
                ) from err

        self._validate_inline_of_call_and_routine(
            call_node, self._routine_node, self._ret_arg_match_list)


# For AutoAPI auto-documentation generation.
__all__ = ["InlineTrans"]
