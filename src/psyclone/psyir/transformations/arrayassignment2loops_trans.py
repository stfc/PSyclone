# -----------------------------------------------------------------------------
# BSD 3-Clause License

# Copyright (c) 2020-2026, Science and Technology Facilities Council.
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
# Author R. W. Ford, N. Nobre and S. Siso, STFC Daresbury Lab
# Modified by J. Henrichs, Bureau of Meteorology
# Modified by A. B. G. Chalk, STFC Daresbury Lab

'''Module providing a transformation from a PSyIR Array Assignment to explicit
PSyIR Loops. This could be useful for e.g. performance reasons, to enable
further transformations e.g. loop fusion or if the back-end does
not support array ranges.

'''
from typing import Optional, Any

from psyclone.errors import LazyString
from psyclone.psyGen import Transformation
from psyclone.psyir.nodes import (
    Assignment, Call, IntrinsicCall, Loop, Literal, Node, Range, Reference,
    CodeBlock, Routine, BinaryOperation)
from psyclone.psyir.nodes.array_mixin import ArrayMixin
from psyclone.psyir.nodes.structure_accessor_mixin import (
    StructureAccessorMixin)
from psyclone.psyir.symbols import (
    DataSymbol, INTEGER_TYPE, ScalarType, SymbolError)
from psyclone.utils import transformation_documentation_wrapper
from psyclone.psyir.transformations.transformation_error import (
    TransformationError)
from psyclone.psyir.transformations.reference2arrayrange_trans import (
    Reference2ArrayRangeTrans)


@transformation_documentation_wrapper
class ArrayAssignment2LoopsTrans(Transformation):
    '''Provides a transformation from a PSyIR Array Range to a PSyIR
    Loop. For example:

    >>> from psyclone.psyir.frontend.fortran import FortranReader
    >>> from psyclone.psyir.nodes import Assignment
    >>> from psyclone.psyir.transformations import ArrayAssignment2LoopsTrans
    >>> code = """
    ... subroutine sub()
    ...   real :: tmp(10)
    ...   tmp(:) = tmp(:) + 3
    ... end subroutine sub"""
    >>> psyir = FortranReader().psyir_from_source(code)
    >>> assignment = psyir.walk(Assignment)[0]
    >>> trans = ArrayAssignment2LoopsTrans()
    >>> trans.apply(assignment)
    >>> print(psyir.debug_string())
    subroutine sub()
      real, dimension(10) :: tmp
      integer :: idx
    <BLANKLINE>
      do idx = LBOUND(tmp, dim=1), UBOUND(tmp, dim=1), 1
        tmp(idx) = tmp(idx) + 3
      enddo
    <BLANKLINE>
    end subroutine sub
    <BLANKLINE>

    By default the transformation will reject character arrays, though this
    can be overridden by setting the 'allow_strings' option to True. Note that
    PSyclone expresses syntax such as `character(LEN=100)` as
    UnsupportedFortranType, and this transformation will convert unknown or
    unsupported types to loops.
    '''
    def apply(
        self,
        node,
        options: Optional[dict[str, Any]] = None,
        allow_strings: bool = False,
        verbose: bool = False,
        **kwargs
    ):
        '''Apply the transformation to the specified array assignment node.
        Each range node within the assignment is replaced with an explicit
        loop. The bounds of the loop are determined from the bounds of the
        array range on the left hand side of the assignment.

        :param node: an Assignment node.
        :param allow_strings: whether to allow the
            transformation on a character type array range. Defaults to False.
        :param verbose: log the reason the validation failed,
            at the moment with a comment in the provided PSyIR node.
        :param options: a dictionary with options for transformations.

        '''
        self.validate(node, options, allow_strings, verbose, **kwargs)

        # If possible use the routine-level symbol table for nicer names
        if node.ancestor(Routine):
            symbol_table = node.ancestor(Routine).symbol_table
        else:
            # We checked in the validate that at least there is a scope
            symbol_table = node.scope.symbol_table

        # If there is any array reference without the accessor syntax,
        # we need to add it first.
        for reference in node.walk(Reference):
            Reference2ArrayRangeTrans().apply(reference)

        # Start by the rightmost array range
        for lhs_range in reversed(node.lhs.walk(Range)):
            lhs_array = lhs_range.parent
            lhs_index = lhs_array.index_of(lhs_range)

            # Create a new, unique, iteration variable for the new loop
            loop_variable_symbol = symbol_table.new_symbol(
                                        root_name="idx",
                                        symbol_type=DataSymbol,
                                        datatype=INTEGER_TYPE)

            # Replace one range for each top-level array expression in the
            # assignment
            for expr in reversed(node.walk(ArrayMixin, stop_type=ArrayMixin)):
                # We use a "for" to capture the value of the last range or
                # continue if there is none, but we don't iterate all Ranges
                for range_to_replace in reversed(expr.walk(Range)):
                    array = range_to_replace.parent
                    index = array.index_of(range_to_replace)
                    if lhs_array.same_range(lhs_index, array, index):
                        # If they iterate over the same bounds we just need a
                        # reference to the iteration index
                        index_expr = Reference(loop_variable_symbol)
                    else:
                        # If we can not prove that both ranges iterate over the
                        # exact same bounds we need to provide an offset like:
                        # array2(idx1 + (lbound_array2 - lbound_array1)
                        offset = BinaryOperation.create(
                                    BinaryOperation.Operator.SUB,
                                    range_to_replace.start.copy(),
                                    lhs_range.start.copy())
                        index_expr = BinaryOperation.create(
                                    BinaryOperation.Operator.ADD,
                                    Reference(loop_variable_symbol),
                                    offset)
                    range_to_replace.replace_with(index_expr)
                    break  # We just substitute one per top-level array

            # Replace the assignment with the new explicit loop structure
            start, stop, step = lhs_range.pop_all_children()
            loop = Loop.create(loop_variable_symbol, start, stop, step, [])
            node.replace_with(loop)
            loop.loop_body.addchild(node)

    def __str__(self):
        return ("Convert a PSyIR assignment with array Ranges into explicit "
                "PSyIR Loops.")

    def validate(
        self,
        node,
        options: Optional[dict[str, Any]] = None,
        allow_strings: bool = False,
        verbose: bool = False,
        **kwargs
    ):
        '''Perform various checks to ensure that it is valid to apply the
        ArrayAssignment2LoopsTrans transformation to the supplied PSyIR Node.

        By default the validate function will throw an TransformationError
        on character arrays, though this can be overridden by setting the
        allow_strings option to True. Note that PSyclone expresses syntax such
        as `character(LEN=100)` as UnsupportedFortranType, and this
        transformation will convert unknown or unsupported types to loops.

        :param node: the assignment to transform.
        :param options: a dictionary with options for transformations.

        :raises TransformationError: if the node argument is not an
            Assignment.
        :raises TransformationError: if the node argument is an
            Assignment whose left hand side is not an ArrayReference.
        :raises TransformationError: if the node argument is an
            Assignment whose left hand side is an ArrayReference that does
            not have Range specifying the access to at least one of its
            dimensions.
        :raises TransformationError: if two or more of the loop ranges
            in the assignment are different or are not known to be the
            same.
        :raises TransformationError: if node contains a character type
                                     child and the allow_strings option is
                                     not set.

        '''
        super().validate(node, **kwargs)
        if not options:
            self.validate_options(allow_strings=allow_strings, verbose=verbose,
                                  **kwargs)
        else:
            # TODO #2668: Deprecate options dictionary.
            verbose = options.get("verbose", False)
            allow_strings = options.get("allow_strings", False)

        if not isinstance(node, Assignment):
            raise TransformationError(
                f"Error in {self.name} transformation. The supplied node "
                f"should be a PSyIR Assignment, but found "
                f"'{type(node).__name__}'.")

        try:
            node.scope
        except SymbolError:
            # pylint: disable=raise-missing-from
            raise TransformationError(LazyString(
                lambda: f"Error in {self.name} transformation. The "
                f"assignment should be in a scope to create the necessary new "
                f"symbols, but '{node.debug_string()}' is not."))

        node_copy = node.copy()
        for reference in node_copy.walk(Reference):
            try:
                Reference2ArrayRangeTrans().apply(reference)
            except TransformationError as err:
                message = (
                    f"Reference2ArrayRangeTrans could not decide if the "
                    f"reference '{reference.name}' is an array or not. "
                    f"Resolving the import that brings this variable into "
                    f"scope may help.")
                if verbose:
                    node.append_preceding_comment(message)
                raise TransformationError(message) from err
        # After a successful Reference2ArrayRangeTrans all arrays are
        # guaranteed to be expresses as ArrayMixin's

        array_accessors = node_copy.lhs.walk(ArrayMixin)
        if not (isinstance(node_copy.lhs, Reference) and array_accessors):
            raise TransformationError(LazyString(
                lambda: f"Error in {self.name} transformation. The LHS of the"
                f" supplied Assignment node should be a Reference that "
                f"contains an array accessor somewhere in the expression, "
                f"but found '{node.lhs.debug_string()}'."))

        # There should be at least one Range in the LHS
        if not node_copy.lhs.walk(Range):
            raise TransformationError(LazyString(
                lambda: f"Error in {self.name} transformation. The LHS of"
                f" the supplied Assignment node should contain an array "
                f"accessor with at least one of its dimensions being a "
                f"Range, but none were found in '{node.debug_string()}'."))

        # All the ArrayMixins must have the same number of Ranges to expand
        num_of_ranges = None
        for accessor in node_copy.walk(ArrayMixin):
            count = len([x for x in accessor.indices if isinstance(x, Range)])
            if count > 0:
                if not num_of_ranges:
                    # If it's the first value we find, we store it
                    num_of_ranges = count
                else:
                    # Otherwise we compare it against the previous found value
                    if count != num_of_ranges:
                        raise TransformationError(LazyString(
                            lambda: f"{self.name} does not support statements"
                            f" containing array accesses that have varying "
                            f"numbers of ranges in their accessors, but found:"
                            f"\n{node.debug_string()}"))

        # Check if there is any dependency between the written reference and
        # any other
        written_ref = node_copy.walk(Reference)[0]
        written_sig, written_idxs = written_ref.get_signature_and_indices()
        for ref in node_copy.walk(Reference)[1:]:
            if ref.symbol is written_ref.symbol:
                if ref.is_read:  # This is here to skip INQUIRY accesses
                    ref_sig, ref_idxs = ref.get_signature_and_indices()
                    if ref_sig != written_sig:
                        # The accessor is different, there is no dependency
                        # (unless its pointers - we ignore these)
                        continue
                    found_dependency = False
                    for c1, c2 in zip(ref_idxs, written_idxs):
                        for i1, i2 in zip(c1, c2):
                            # The assumptions below are only true because we
                            # don't support impure functions that could
                            # introduce additional writes.
                            # If none of the accesses are ranges, this will be
                            # a loop-invariant
                            if isinstance(i1, Range) or isinstance(i2, Range):
                                # If the index is not exactly the same there
                                # could be a loop-carried dependency
                                if i1 != i2:
                                    found_dependency = True
                                    break
                    if found_dependency:
                        raise TransformationError(LazyString(
                            lambda: f"{self.name} does not support statements "
                            f"containing dependencies that would generate "
                            f"loop-carried dependencies when naively "
                            f"converting them to a loop, but found:"
                            f"\n{node.debug_string()}"))

        # Do not allow to transform expressions with CodeBlocks
        if node_copy.walk(CodeBlock):
            message = (f"{self.name} does not support array assignments that"
                       f" contain a CodeBlock anywhere in the expression")
            if verbose:
                node.append_preceding_comment(message)
            raise TransformationError(LazyString(
                lambda: f"{message}, but found:\n{node.debug_string()}"))

        # We don't support nested range expressions anywhere in the assignment
        for range_expr in node_copy.walk(Range, stop_type=Range):
            # Test that there are no Ranges in any children
            test_nodes = range_expr.children[:]
            # or the member sibling if it is a derived type accessor
            if isinstance(range_expr.parent, StructureAccessorMixin):
                test_nodes.append(range_expr.parent.member)

            for test in test_nodes:
                if test.has_descendant(Range):
                    message = (
                        f"{self.name} does not support array assignments "
                        f"that contain nested Range expressions")
                    if verbose:
                        node.append_preceding_comment(message)
                    # pylint: disable=cell-var-from-loop
                    raise TransformationError(LazyString(lambda: (
                        f"{message}, but found:\n"
                        f"{range_expr.parent.debug_string()}")))

        # If we allow string arrays then we can skip the check.
        if not allow_strings:
            message = (f"{self.name} does not expand ranges "
                       f"on character arrays by default (use the"
                       f"'allow_strings' option to expand them)")
            self.validate_no_char(node, message, verbose)

        # We don't accept calls that are not guaranteed to be elemental
        for call in node_copy.rhs.walk(Call):
            if isinstance(call, IntrinsicCall):
                # Intrinsics that return scalars are also fine.
                if call.intrinsic in (IntrinsicCall.Intrinsic.LBOUND,
                                      IntrinsicCall.Intrinsic.UBOUND,
                                      IntrinsicCall.Intrinsic.SIZE):
                    continue
                name = call.intrinsic.name
            else:
                name = call.routine.symbol.name
            if not call.is_elemental:
                message = (f"{self.name} does not accept calls which are not"
                           f" guaranteed to be elemental, but found: "
                           f"{name}")
                if verbose:
                    node.append_preceding_comment(message)
                # pylint: disable=cell-var-from-loop
                raise TransformationError(LazyString(
                    lambda: f"{message} in:\n{node.debug_string()}."))

    @staticmethod
    def validate_no_char(node: Node, message: str, verbose: bool) -> None:
        '''
        Check that there is no character variable accessed in the sub-tree with
        the supplied node at its root.

        :param node: the root node to check for character assignments.
        :param message: the message to use if a character assignment is found.
        :param verbose: whether log the reason the validation failed,
            at the moment with a comment in the provided PSyIR node.

        :raises TransformationError: if the supplied node contains a
            child of character type.

        '''
        for child in node.walk((Literal, Reference)):
            try:
                forbidden = ScalarType.Intrinsic.CHARACTER
                if (child.is_character(unknown_as=False) or
                        (child.symbol.datatype.intrinsic == forbidden)):
                    if verbose:
                        node.append_preceding_comment(message)
                    # pylint: disable=cell-var-from-loop
                    raise TransformationError(LazyString(
                        lambda: f"{message}, but found:"
                        f"\n{node.debug_string()}"))
            except (NotImplementedError, AttributeError):
                # We cannot always get the datatype, we ignore this for now
                pass


__all__ = [
    'ArrayAssignment2LoopsTrans']
