# -----------------------------------------------------------------------------
# BSD 3-Clause License

# Copyright (c) 2020-2024, Science and Technology Facilities Council.
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

'''Module providing a transformation from a PSyIR Array Range to a
PSyIR Loop. This could be useful for e.g. performance reasons, to
allow further transformations e.g. loop fusion or if the back-end does
not support array ranges.

By default the transformation will reject character arrays,
though this can be overriden by setting the
allow_string option to True. Note that PSyclone expresses syntax such
as `character(LEN=100)` as UnsupportedFortranType, and this
transformation will convert unknown or unsupported types to loops.

'''

from psyclone.errors import LazyString
from psyclone.psyGen import Transformation
from psyclone.psyir.nodes import (
    ArrayReference, Assignment, Call, IntrinsicCall, Loop, Literal, Range,
    Reference, CodeBlock, Routine, StructureReference)
from psyclone.psyir.nodes.array_mixin import ArrayMixin
from psyclone.psyir.symbols import DataSymbol, INTEGER_TYPE, ScalarType, \
        UnresolvedType, UnsupportedType, ArrayType, NoType
from psyclone.psyir.transformations.transformation_error \
    import TransformationError
from psyclone.psyir.transformations.reference2arrayrange_trans import (
    Reference2ArrayRangeTrans)


class ArrayRange2LoopTrans(Transformation):
    '''Provides a transformation from a PSyIR Array Range to a PSyIR
    Loop. For example:

    >>> from psyclone.parse.algorithm import parse
    >>> from psyclone.psyGen import PSyFactory
    >>> api = "nemo"
    >>> filename = "tra_adv_compute.F90"
    >>> ast, invoke_info = parse(filename, api=api)
    >>> psy = PSyFactory(api).create(invoke_info)
    >>> schedule = psy.invokes.invoke_list[0].schedule
    >>>
    >>> from psyclone.psyir.nodes import Assignment
    >>> from psyclone.psyir.transformations import ArrayRange2LoopTrans, \
    >>>     TransformationError
    >>>
    >>> print(schedule.view())
    >>> trans = ArrayRange2LoopTrans()
    >>> for assignment in schedule.walk(Assignment):
    >>>     while True:
    >>>         try:
    >>>             trans.apply(assignment)
    >>>         except TransformationError:
    >>>             break
    >>> print(schedule.view())

    '''
    def apply(self, node, options=None):
        '''Apply the ArrayRange2Loop transformation to the specified node. The
        node must be an assignment. The rightmost range node in each array
        within the assignment is replaced with a loop index and the
        assignment is placed within a loop iterating over that
        index. The bounds of the loop are determined from the bounds
        of the array range on the left hand side of the assignment.

        :param node: an Assignment node.
        :type node: :py:class:`psyclone.psyir.nodes.Assignment`
        :type options: Optional[Dict[str, Any]]
        :param bool options["allow_string"]: whether to allow the
            transformation on a character type array range. Defaults to False.

        '''
        self.validate(node, options)

        # If there is any array reference without the accessor syntax,
        # we need to add it first.
        for reference in node.walk(Reference):
            try:
                Reference2ArrayRangeTrans().apply(reference)
            except TransformationError:
                pass

        # Start by the outermost (rightmost) array range
        for array in node.lhs.walk(ArrayMixin):
            for range_node in reversed(array.walk(Range)):
                self._range_to_loop(node, range_node)


    def _range_to_loop(self, assignment, range_node):
        parent = assignment.parent
        # If possible use the routine-level symbol table for nicer names
        if assignment.ancestor(Routine):
            symbol_table = assignment.ancestor(Routine).symbol_table
        else:
            symbol_table = assignment.scope.symbol_table

        # Create a new, unique, iteration variable for the new loop
        loop_variable_symbol = symbol_table.new_symbol(root_name="idx",
                                                       symbol_type=DataSymbol,
                                                       datatype=INTEGER_TYPE)

        # Replace the loop_idx array dimension with the loop variable.
        n_ranges = None
        # Just loop the top-level arrays since we just do 1 substitution per
        # array construct, even if they have nested arrays in turn.
        for top_level_ref in assignment.walk(ArrayMixin, stop_type=ArrayMixin):
            # Then start checking from the outermost (rightmost) array
            for array in reversed(top_level_ref.walk(ArrayMixin)):
                current_n_ranges = len([child for child in array.children
                                        if isinstance(child, Range)])
                if current_n_ranges == 0:
                    continue  # This sub-expression already has explicit dims
                if n_ranges is None:
                    n_ranges = current_n_ranges
                elif n_ranges != current_n_ranges:
                    raise InternalError(
                        "The number of ranges in the arrays within this "
                        "assignment are not equal. Any such case should have "
                        "been dealt with by the validation method or "
                        "represents invalid PSyIR.")

                idx = array.get_outer_range_index()
                array.children[idx] = Reference(loop_variable_symbol)
                break  # If one is found, go to the next top level expression

        # Replace the assignment with the new explicit loop structure
        position = assignment.position
        start, stop, step = range_node.pop_all_children()
        loop = Loop.create(loop_variable_symbol, start, stop, step,
                           [assignment.detach()])
        parent.children.insert(position, loop)


    def __str__(self):
        return ("Convert a PSyIR assignment to an array Range into a "
                "PSyIR Loop.")

    @property
    def name(self):
        '''
        :returns: the name of the transformation as a string.
        :rtype: str

        '''
        return type(self).__name__

    def _validate_range_expansion(assignment, range_to_expand):
        pass
    
    def validate(self, node, options=None):
        '''Perform various checks to ensure that it is valid to apply the
        ArrayRange2LoopTrans transformation to the supplied PSyIR Node.

        By default the validate function will throw an TransofmrationError
        on character arrays, though this can be overriden by setting the
        allow_string option to True. Note that PSyclone expresses syntax such
        as `character(LEN=100)` as UnsupportedFortranType, and this
        transformation will convert unknown or unsupported types to loops.

        :param node: the node that is being checked.
        :type node: :py:class:`psyclone.psyir.nodes.Assignment`
        :param options: a dictionary with options for transformations
        :type options: Optional[Dict[str, Any]]
        :param bool options["allow_string"]: whether to allow the
            transformation on a character type array range. Defaults to False.

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
        if not options:
            options = {}

        if not isinstance(node, Assignment):
            raise TransformationError(
                f"Error in {self.name} transformation. The supplied node "
                f"should be a PSyIR Assignment, but found "
                f"'{type(node).__name__}'.")

        array_accessors = node.lhs.walk(ArrayMixin)
        if not (isinstance(node.lhs, Reference) and array_accessors):
            raise TransformationError(LazyString(
                lambda: f"Error in {self.name} transformation. The LHS of the"
                f" supplied Assignment node should be a Reference that "
                f"contains an array accessor somewhere in the expression, "
                f"but found '{node.lhs.debug_string()}'."))

        # There should be at least one Range in the LHS
        if not node.lhs.walk(Range):
            raise TransformationError(LazyString(
                lambda: f"Error in {self.name} transformation. The LHS of"
                f" the supplied Assignment node should contain an array "
                f"accessor with at least one of its dimensions being a "
                f"Range, but none were found in '{node.debug_string()}'."))

        # We don't support nested range expressions anywhere in the assignment
        for range_expr in node.walk(Range):
            ancestor_array = range_expr.parent.ancestor(ArrayMixin)
            if ancestor_array and any(index.walk(Range) for index
                                      in ancestor_array.indices):
                raise TransformationError(LazyString(
                    lambda: f"{self.name} does not support array assignments"
                    f" that contain nested Range structures, but found:"
                    f"\n{node.debug_string()}"))

        # Do not allow to transform expressions with CodeBlocks
        if node.walk(CodeBlock):
            raise TransformationError(LazyString(
                lambda: f"{self.name} does not support array assignments that"
                f" contain a CodeBlock anywhere in the expression, but found:"
                f"\n{node.debug_string()}"))

        # Add errors below this point will optionally log the resason, which
        # at the moment means adding a comment in the output code
        verbose = options.get("verbose", False)

        # We don't support nested range expressions
        for range_expr in node.walk(Range):
            ancestor_array = range_expr.parent.ancestor(ArrayMixin)
            if ancestor_array and any(index.walk(Range) for index
                                      in ancestor_array.indices):
                message = (f"{self.name} does not support array assignments "
                           f"that contain nested Range expressions")
                if verbose:
                    node.append_preceding_comment(message)
                raise TransformationError(LazyString(
                    lambda: f"{message}, but found:\n{node.debug_string()}"))

        # If we allow string arrays then we can skip the check.
        if not options.get("allow_string", False):
            for child in node.walk((Literal, Reference)):
                try:
                    if (child.datatype.intrinsic ==
                            ScalarType.Intrinsic.CHARACTER):
                        message = (f"{self.name} does not expand ranges "
                                   f"on character arrays by default (use the"
                                   f"'allow_string' option to expand them)")
                        if verbose:
                            node.append_preceding_comment(message)
                        # pylint: disable=cell-var-from-loop
                        raise TransformationError(LazyString(
                            lambda: f"{message}, but found:"
                            f"\n{node.debug_string()}"))
                except (NotImplementedError, AttributeError):
                    # We cannot always get the datatype, we ignore this for now
                    pass

        # We don't accept calls that are not guaranteed to be elemental
        for call in node.rhs.walk(Call):
            if isinstance(call, IntrinsicCall):
                if call.intrinsic.is_inquiry:
                    continue  # Inquiry intrinsic calls are fine
                name = call.intrinsic.name
            else:
                name = call.symbol.name
            if not call.is_elemental:
                message = (f"{self.name} does not accept calls which are not"
                           f" guaranteed to be elemental, but found:"
                           f"{name}")
                if verbose:
                    node.append_preceding_comment(message)
                # pylint: disable=cell-var-from-loop
                raise TransformationError(LazyString(
                    lambda: f"{message} in:\n{node.debug_string()}."))

        for reference in node.walk(Reference):
            # As special case we always allow references to whole arrays as
            # part of the LBOUND and UBOUND intrinsics, regardless of the
            # restrictions below
            if isinstance(reference.parent, IntrinsicCall):
                intrinsic = reference.parent.intrinsic
                valid = (IntrinsicCall.Intrinsic.LBOUND,
                         IntrinsicCall.Intrinsic.UBOUND)
                if intrinsic in valid:
                    continue

            # We allow any references that have explicit array syntax
            # because we infer that they are not scalars from the context
            # where they are found (even if they have UnresolvedType)
            if isinstance(reference, (ArrayReference, StructureReference)):
                continue

            # However, if it doesn't have explicity array accessors
            # syntax, we must be ensure that it represents a scalar.
            if not isinstance(reference.symbol, DataSymbol) or \
                    isinstance(reference.symbol.datatype, (
                        UnresolvedType, UnsupportedType)):
                if isinstance(reference.symbol, DataSymbol):
                    typestr = f"an {reference.symbol.datatype}"
                else:
                    typestr = "not a DataSymbol"
                message = (
                    f"{self.name} cannot expand expression because it "
                    f"contains the variable '{reference.symbol.name}' "
                    f"which is {typestr} and therefore cannot be guaranteed"
                    f" to be ScalarType.")
                if not isinstance(reference.symbol, DataSymbol) or \
                        isinstance(reference.symbol.datatype, UnresolvedType):
                    message += (" Resolving the import that brings this"
                                " variable into scope may help.")
                if verbose:
                    node.append_preceding_comment(message)
                raise TransformationError(LazyString(
                    lambda: f"{message} In:\n{node.debug_string()}"))

        # Find the outermost range for the array on the lhs of the
        # assignment and save its index.
        for idx, child in reversed(list(enumerate(node.lhs.children))):
            if isinstance(child, Range):
                lhs_index = idx
                break

        # For each array on the rhs of the assignment find the
        # outermost range if there is one, then compare this range
        # with the one on the lhs.
        for array in node.walk(ArrayReference):
            for idx, child in reversed(list(enumerate(array.children))):
                if isinstance(child, Range):
                    # Issue #814 We should add support for adding
                    # loop variables where the ranges are
                    # different, or occur in different index
                    # locations.
                    if not node.lhs.same_range(lhs_index, array, idx):
                        # Ranges are, or may be, different so we
                        # can't safely replace this range with a
                        # loop iterator.
                        raise TransformationError(
                            f"The ArrayRange2LoopTrans transformation only "
                            f"supports ranges that are known to be the "
                            f"same as each other but array access "
                            f"'{node.lhs.name}' dimension {lhs_index} and "
                            f"'{array.name}' dimension {idx} are either "
                            f"different or can't be determined in the "
                            f"assignment '{node}'.")
                    break


__all__ = [
    'ArrayRange2LoopTrans']
