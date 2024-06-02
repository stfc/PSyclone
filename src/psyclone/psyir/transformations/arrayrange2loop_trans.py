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
    Reference, CodeBlock, Routine, StructureReference, BinaryOperation)
from psyclone.psyir.nodes.array_mixin import ArrayMixin
from psyclone.psyir.symbols import (
    DataSymbol, INTEGER_TYPE, ScalarType, UnresolvedType, UnsupportedType,
    ArrayType, NoType, SymbolError)
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

        # If possible use the routine-level symbol table for nicer names
        if node.ancestor(Routine):
            symbol_table = node.ancestor(Routine).symbol_table
        else:
            # We checked in the validate that at least there is a scope
            symbol_table = node.scope.symbol_table

        # If there is any array reference without the accessor syntax,
        # we need to add it first.
        # for reference in node.walk(Reference):
        #     try:
        #         Reference2ArrayRangeTrans().apply(reference)
        #     except TransformationError:
        #         pass

        # import pdb; pdb.set_trace()
        # Start by the rightmost array range
        # import pdb; pdb.set_trace()
        for lhs_range in reversed(node.lhs.walk(Range)):
            lhs_array = lhs_range.parent
            lhs_index = lhs_array.indices.index(lhs_range)
            lhs_lbound = lhs_array.get_lbound_expression(lhs_index)

            # Create a new, unique, iteration variable for the new loop
            loop_variable_symbol = symbol_table.new_symbol(
                                        root_name="idx",
                                        symbol_type=DataSymbol,
                                        datatype=INTEGER_TYPE)

            # Replace one range for each top-level array expression in the
            # assignment
            for expr in reversed(node.walk(ArrayMixin, stop_type=ArrayMixin)):
                for range_to_replace in reversed(expr.walk(Range)):
                    array = range_to_replace.parent
                    index = array.indices.index(range_to_replace)
                    lhs_array.is_same_array(array)
                    if lhs_array.same_range(lhs_index, array, index):
                        # If they iterate over the same bounds we just need a
                        # reference to the iteration index
                        index_expr = Reference(loop_variable_symbol)
                    else:
                        # import pdb; pdb.set_trace()
                        # lhs_array.same_range(lhs_index, array, index)
                        # If we can not prove that both ranges iterate over the
                        # exact same bounds we need to provide an offset like:
                        # array2(idx1 + (lbound_array2 - lbound_array1)
                        offset = BinaryOperation.create(
                                    BinaryOperation.Operator.SUB,
                                    array.get_lbound_expression(index),
                                    lhs_lbound.copy())
                        index_expr = BinaryOperation.create(
                                    BinaryOperation.Operator.ADD,
                                    Reference(loop_variable_symbol),
                                    offset)
                    range_to_replace.replace_with(index_expr)
                    break  # We just substitue one per top-level array

            # Replace the assignment with the new explicit loop structure
            start, stop, step = lhs_range.pop_all_children()
            loop = Loop.create(loop_variable_symbol, start, stop, step, [])
            node.replace_with(loop)
            loop.loop_body.addchild(node)

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

        try:
            node.scope
        except SymbolError:
            # pylint: disable=raise-missing-from
            raise TransformationError(LazyString(
                lambda: f"Error in {self.name} transformation. The "
                f"assignment should be in a scope to create the necessary new "
                f"symbols, but '{node.debug_string()}' is not."))

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

        # All the ArrayMixins must have the same number of Ranges to expand
        found = None
        for accessor in node.walk(ArrayMixin):
            num_of_ranges = len(accessor.walk(Range))
            if num_of_ranges > 0:
                if not found:
                    # If its the first value we find, we store it
                    found = num_of_ranges
                else:
                    # Otherwise we compare it agains the previous found value
                    if found != num_of_ranges:
                        raise TransformationError(LazyString(
                            lambda: f"{self.name} does not support array "
                            f" with array accessor with a different number of"
                            "ranges in the expression, but found:"
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
                # pylint: disable=cell-var-from-loop
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
            if isinstance(reference.parent, Call):
                # The call routine references are fine
                if reference.parent.routine is reference:
                    continue
                # Arguments of inquiry intrinsics are also fine
                if isinstance(reference.parent, IntrinsicCall):
                    if reference.parent.is_inquiry:
                        continue

            # We allow any references that have explicit array syntax because
            # we can infer that they are not scalars, regarless of the type.
            if isinstance(reference, ArrayReference):
                continue
            if isinstance(reference, StructureReference):
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
                # pylint: disable=cell-var-from-loop
                raise TransformationError(LazyString(
                    lambda: f"{message} In:\n{node.debug_string()}"))


__all__ = [
    'ArrayRange2LoopTrans']
