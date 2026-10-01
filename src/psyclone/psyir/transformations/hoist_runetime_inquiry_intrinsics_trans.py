# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------
'''Hoist array inquiries whose bounds are only known at run time.'''

from psyclone.psyir.nodes import (
    Assignment, Call, CodeBlock, Directive, IfBlock,
    IntrinsicCall, Literal, Loop, Reference, Return, Routine, Schedule,
    WhileLoop)
from psyclone.psyir.symbols import (
    ArrayType, DataSymbol, ScalarType, UnresolvedType, UnsupportedFortranType)
from psyclone.psyir.transformations.region_trans import RegionTrans
from psyclone.psyir.transformations.transformation_error import (
    TransformationError)
from psyclone.utils import transformation_documentation_wrapper


@transformation_documentation_wrapper
class HoistRunetimeInquiryIntrinsicsTrans(RegionTrans):
    '''Extract run-time array inquiries into reusable temporaries.

    The region selects the inquiries to transform; their assignments may move
    outside that region, up to the enclosing routine. Movement stops at changes
    to their operands, opaque code, protecting conditions and directives.
    If bodies can be crossed unless evaluating the condition changes an operand
    or the condition guards its allocation, association, presence or indices.
    A loop is crossed only if all its iterations preserve the inquiry operands.
    Whole-array assignments are conservatively treated as possible automatic
    reallocations. Array element assignments do not change bounds.
    After extraction, assignments move as early as possible within their final
    schedules, stopping immediately after the last relevant barrier.

    Unsupported result types and expressions with side effects are left intact.
    Inquiries in while-loop conditions and within directives are left intact.

    For example, apply to a routine or loop with::

        trans = HoistRunetimeInquiryIntrinsicsTrans()
        trans.apply(region, assume_reallocations_are_local=True)

    Repeated ``UBOUND(a, dim=3)`` expressions share a single ``ubound_a_3``
    temporary whenever no intervening operation can invalidate its value.
    '''

    def __str__(self):
        return 'Hoist run-time array inquiry intrinsics'

    def validate(self, nodes, options=None, **kwargs):
        '''Validate the region and transformation options.'''
        node_list = self.get_node_list(nodes)
        if not node_list:
            self.validate_options(**kwargs)
            return
        super().validate(nodes, options=options, **kwargs)
        if not node_list[0].ancestor(Routine):
            raise TransformationError(f'{self.name} requires a region within '
                                      'a Routine.')

    @staticmethod
    def _runtime_array(arg):
        '''Whether an argument refers to an array with run-time bounds.'''
        if not isinstance(arg, Reference):
            return False
        dtype = arg.datatype
        if isinstance(dtype, UnsupportedFortranType):
            dtype = dtype.partial_datatype
            if dtype is None:
                return True
        if isinstance(dtype, UnresolvedType):
            return True
        return isinstance(dtype, ArrayType) and any(
            isinstance(bound, ArrayType.Extent) or
            not all(not expr.walk((Call, CodeBlock)) and
                    all(isinstance(ref.symbol, DataSymbol) and
                        ref.symbol.is_constant for ref in expr.walk(Reference))
                    for expr in (bound.lower, bound.upper))
            for bound in dtype.shape)

    @staticmethod
    def _changes_descriptor(assignment, array):
        '''Whether an assignment can replace an array or a containing object.

        Indexing the final component writes elements or a section and cannot
        reallocate that component. A containing structure is different:
        assigning ``objects(i)`` can replace its allocatable components.
        Compare component paths even when their datatypes are unresolved.
        '''
        lhs = assignment.lhs
        if lhs.symbol is not array.symbol:
            return False
        written, indices = lhs.get_signature_and_indices()
        queried, _ = array.get_signature_and_indices()
        if written != queried[:len(written)]:
            return False
        return (assignment.is_pointer or len(written) < len(queried) or
                not indices[-1])

    @staticmethod
    def _barrier(node, inquiry, local):
        '''Whether executing node can change the inquiry's result.

        Track descriptor changes separately from value changes: the array's
        elements do not affect an inquiry, but section indices, DIM, KIND and
        other operands must retain their values.
        '''
        # pylint: disable=too-many-return-statements,too-many-branches
        arrays = {arg.symbol for arg in inquiry.arguments
                  if isinstance(arg, Reference)}
        values = set()
        for arg in inquiry.arguments:
            refs = arg.walk(Reference)
            if isinstance(arg, Reference) and \
                    HoistRunetimeInquiryIntrinsicsTrans._runtime_array(arg):
                refs = refs[1:]
            values.update(ref.symbol for ref in refs)
        symbols = arrays | values
        if node.walk((CodeBlock, Directive, Return)):
            return True
        for call in node.walk(Call):
            if call is inquiry:
                continue
            if isinstance(call, IntrinsicCall) and call.intrinsic in (
                    IntrinsicCall.Intrinsic.ALLOCATE,
                    IntrinsicCall.Intrinsic.DEALLOCATE,
                    IntrinsicCall.Intrinsic.MOVE_ALLOC):
                if any(ref.symbol in symbols
                       for arg in call.arguments
                       for ref in arg.walk(Reference)):
                    return True
            elif not call.is_pure and (not local or values):
                return True
            # Even pure procedures can modify their actual arguments.
            elif not isinstance(call, IntrinsicCall) and any(
                    ref.symbol in symbols for arg in call.arguments
                    for ref in arg.walk(Reference)):
                return True
        for assign in node.walk(Assignment):
            lhs = assign.lhs
            if not isinstance(lhs, Reference):
                continue
            if lhs.symbol in values:
                return True
            if any(HoistRunetimeInquiryIntrinsicsTrans._changes_descriptor(
                    assign, arg) for arg in inquiry.arguments
                   if isinstance(arg, Reference)):
                return True
        return any(loop.variable in symbols for loop in node.walk(Loop))

    @staticmethod
    def _result_type(call):
        """Determine the result type, allowing for unknown array ranks."""
        try:
            dtype = call.datatype
        except (AttributeError, NotImplementedError):
            dtype = UnresolvedType()
        if isinstance(dtype, UnresolvedType) and call.intrinsic in (
                IntrinsicCall.Intrinsic.LBOUND, IntrinsicCall.Intrinsic.UBOUND,
                IntrinsicCall.Intrinsic.SHAPE):
            # An allocatable vector accommodates a rank only known at runtime.
            scalar = ScalarType.integer_type()
            if 'kind' in call.argument_names:
                scalar = ScalarType(ScalarType.Intrinsic.INTEGER,
                                    call.argument_by_name('kind').copy())
            if 'dim' in call.argument_names:
                return scalar
            return ArrayType(scalar, [ArrayType.Extent.DEFERRED])
        return dtype

    @staticmethod
    def _guards_inquiry(condition, inquiry):
        '''Whether a condition protects evaluation of this inquiry.

        Allocation, association and presence tests can guard invalid array
        descriptors. Conditions on value operands can protect DIM or section
        indices. Keep dependent inquiries inside these guards.
        '''
        symbols = set()
        values = set()
        for arg in inquiry.arguments:
            refs = arg.walk(Reference)
            symbols.update(ref.symbol for ref in refs)
            if (isinstance(arg, Reference) and
                    HoistRunetimeInquiryIntrinsicsTrans._runtime_array(arg)):
                refs = refs[1:]
            values.update(ref.symbol for ref in refs)
        if any(ref.symbol in values for ref in condition.walk(Reference)):
            return True
        for call in condition.walk(IntrinsicCall):
            if call.intrinsic in (IntrinsicCall.Intrinsic.ALLOCATED,
                                  IntrinsicCall.Intrinsic.ASSOCIATED,
                                  IntrinsicCall.Intrinsic.PRESENT):
                if any(ref.symbol in symbols for arg in call.arguments
                       for ref in arg.walk(Reference)):
                    return True
        return False

    def _earliest_anchor(self, anchor, inquiry, local):
        '''Find the earliest safe insertion point in anchor's schedule.'''
        schedule = anchor.parent
        while anchor.position:
            previous = schedule.children[anchor.position - 1]
            if self._barrier(previous, inquiry, local):
                break
            anchor = previous
        return anchor

    def apply(self, nodes, options=None,
              assume_reallocations_are_local: bool = False, **kwargs):
        '''Extract and hoist inquiries in the supplied region.

        :param nodes: a statement, Schedule or consecutive list of statements.
        :param options: legacy dictionary of transformation options.
        :param assume_reallocations_are_local: assume calls do not indirectly
            reallocate arrays or change pointer associations. Explicit writes,
            allocation operations and calls receiving an operand still block
            movement. Defaults to False.
        '''
        # pylint: disable=arguments-renamed,too-many-locals
        # pylint: disable=too-many-branches,too-many-statements
        self.validate(nodes, options=options,
                      assume_reallocations_are_local=(
                          assume_reallocations_are_local), **kwargs)
        local = (options or {}).get(
            'assume_reallocations_are_local', assume_reallocations_are_local)
        self.validate_options(assume_reallocations_are_local=local)
        calls = [call for node in self.get_node_list(nodes)
                 for call in node.walk(IntrinsicCall)]
        generated = []
        # Process nested inquiries before extracting their enclosing calls.
        for call in reversed(calls):
            if call.ancestor(Directive):
                continue
            if not call.is_inquiry or not any(
                    self._runtime_array(arg) for arg in call.arguments):
                continue
            if any(not nested.is_pure for arg in call.arguments
                   for nested in arg.walk(Call)) or call.walk(CodeBlock):
                continue
            dtype = self._result_type(call)
            if not isinstance(dtype, (ScalarType, ArrayType)):
                continue
            # Locate the statement containing the expression.
            anchor = call
            while anchor.parent and not isinstance(anchor.parent, Schedule):
                anchor = anchor.parent
            if (not isinstance(anchor.parent, Schedule) or anchor is call
                    or isinstance(anchor, WhileLoop)):
                continue
            # Do not extract from a statement that may invalidate the inquiry
            # during evaluation (Fortran does not specify evaluation order).
            if any(self._barrier(other, call, local)
                   for other in anchor.walk(Call, stop_type=Schedule)
                   if other is not call and call not in other.walk(Call)):
                continue
            while True:
                schedule = anchor.parent
                anchor = self._earliest_anchor(anchor, call, local)
                if anchor.position:
                    break
                enclosing = schedule.parent
                if isinstance(enclosing, Loop):
                    # Every iteration must preserve the operands.
                    if self._barrier(enclosing, call, local):
                        break
                elif isinstance(enclosing, IfBlock):
                    # Preceding statements in this branch have already been
                    # checked. Later statements and the other branch are not
                    # executed on the path from the condition to the inquiry.
                    if (self._barrier(enclosing.condition, call, local) or
                            self._guards_inquiry(enclosing.condition, call)):
                        break
                else:
                    break
                anchor = enclosing
            # Previously generated definitions are crossed just like any other
            # independent statement. Reuse only a definition at this location.
            containing = call
            while containing.parent is not schedule:
                containing = containing.parent
            match = next((entry for entry in generated
                          if entry[0] == call and entry[1].parent is schedule
                          and anchor.position <= entry[1].position
                          < containing.position), None)
            if match:
                call.replace_with(Reference(match[1].lhs.symbol))
                continue
            name = call.intrinsic.name.lower()
            for arg in call.arguments:
                if isinstance(arg, Reference):
                    name += '_' + arg.symbol.name
                elif isinstance(arg, Literal) and arg.value.isdigit():
                    name += '_' + arg.value
            symbol = call.ancestor(Routine).symbol_table.new_symbol(
                name, symbol_type=DataSymbol, datatype=dtype.copy())
            expression = call.copy()
            call.replace_with(Reference(symbol))
            assignment = Assignment.create(Reference(symbol), expression)
            schedule.addchild(assignment, anchor.position)
            generated.append((expression, assignment))

        # Later extractions can remove barriers from earlier statements. For
        # example, extracting ASSOCIATED (classified as impure) leaves a plain
        # reference in its original statement. Revisit generated definitions
        # once extraction is complete. Creation order puts dependencies before
        # their users, so moving a definition clears the way for its users.
        for expression, assignment in generated:
            anchor = self._earliest_anchor(assignment, expression, local)
            if anchor is not assignment:
                schedule = assignment.parent
                position = anchor.position
                assignment.detach()
                schedule.addchild(assignment, position)


__all__ = ['HoistRunetimeInquiryIntrinsicsTrans']
