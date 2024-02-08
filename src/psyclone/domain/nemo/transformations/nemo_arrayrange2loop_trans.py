# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
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
# Author R. W. Ford, STFC Daresbury Lab
# Modified A. R. Porter, S. Siso and N. Nobre, STFC Daresbury Lab

'''Module providing a transformation that given an Assignment node to an
ArrayReference in its left-hand-side which has at least one PSyIR Range
node (equivalent to an array assignment statement in Fortran), it converts it
to the equivalent explicit loop representation using a NemoLoop node.

'''

from psyclone.errors import LazyString, InternalError
from psyclone.nemo import NemoLoop
from psyclone.psyGen import Transformation
from psyclone.psyir.nodes import Range, Reference, ArrayReference, Call, \
    Assignment, CodeBlock, ArrayMember, Routine, IntrinsicCall, \
    StructureReference, StructureMember, Node
from psyclone.psyir.nodes.array_mixin import ArrayMixin
from psyclone.psyir.symbols import DataSymbol, INTEGER_TYPE, ScalarType
from psyclone.psyir.transformations.transformation_error import \
    TransformationError


class NemoArrayRange2LoopTrans(Transformation):
    '''Transformation that given an assignment with an ArrayReference Range
    in the LHS (equivalent to an array assignment statement in Fortran), it
    converts it to an explicit loop doing each of the individual element
    assignments separately. For example:

    >>> from psyclone.parse.algorithm import parse
    >>> from psyclone.psyGen import PSyFactory
    >>> api = "nemo"
    >>> filename = "tra_adv.F90" # examples/nemo/code
    >>> ast, invoke_info = parse(filename, api=api)
    >>> psy = PSyFactory(api).create(invoke_info)
    >>> schedule = psy.invokes.invoke_list[0].schedule
    >>> print(schedule.view())
    >>>
    >>> from psyclone.psyir.nodes import Range
    >>> from psyclone.domain.nemo.transformations import \
            NemoArrayRange2LoopTrans
    >>> from psyclone.transformations import TransformationError
    >>>
    >>> trans = NemoArrayRange2LoopTrans()
    >>> for my_range in reversed(schedule.walk(Range)):
    >>>     try:
    >>>         trans.apply(my_range)
    >>>     except TransformationError:
    >>>         pass
    >>> print(schedule.view())

    The specified Range node must be the outermost Range (specifying
    an access to an array index) within an Array Reference and the
    array reference must be on the left-hand-side of an Assignment
    node. This is required for correctness and if not satisfied the
    transformation will raise an exception.

    '''
    def apply(self, node, options=None):
        ''' Apply the transformation such that, given an assignment with an
        ArrayReference Range in the LHS (equivalent to an array assignment
        statement in Fortran), it converts it to an explicit loop doing each
        of the individual element assignments separately.

        The Range node is provided to the apply method of the transformation
        to indicate which array index should be transformed. This can only
        be applied to the outermost Range of the ArrayReference.

        This is currently specific to the 'nemo' API in that it will create
        NemoLoops.

        :param node: a Range node.
        :type node: :py:class:`psyclone.psyir.nodes.Range`
        :param options: a dictionary with options for \
            transformations. No options are used in this \
            transformation. This is an optional argument that defaults \
            to None.
        :type options: Optional[Dict[str, Any]]

        '''
        self.validate(node)

        assignment = node.ancestor(Assignment)
        parent = assignment.parent
        # Ensure we always use the routine-level symbol table
        symbol_table = node.ancestor(Routine).symbol_table

        # Create a new, unique, iteration variable for the new loop
        loop_variable_symbol = symbol_table.new_symbol(root_name="idx",
                                                       symbol_type=DataSymbol,
                                                       datatype=INTEGER_TYPE)

        # Replace the loop_idx array dimension with the loop variable.
        n_ranges = None
        # Just loop the top-level arrays since we just do 1 substitution per
        # array construct, even if they have nested arrays in turn.
        for top_level_ref in assignment.walk(ArrayMixin, stop_type=ArrayMixin):
            # Then start checking with the inner-most array
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
        start, stop, step = node.pop_all_children()
        loop = NemoLoop.create(loop_variable_symbol, start, stop, step,
                               [assignment.detach()])
        parent.children.insert(position, loop)

    def __str__(self):
        return (
            "Convert the PSyIR assignment for a specified ArrayReference "
            "Range into a PSyIR NemoLoop.")

    @property
    def name(self):
        '''
        :returns: the name of the transformation as a string.
        :rtype: str

        '''
        return type(self).__name__

    def validate(self, node, options=None):
        '''Perform various checks to ensure that it is valid to apply the
        NemoArrayRange2LoopTrans transformation to the supplied PSyIR Node.

        :param node: the node that is being checked.
        :type node: :py:class:`psyclone.psyir.nodes.Range`
        :param options: a dictionary with options for \
            transformations. No options are used in this \
            transformation. This is an optional argument that defaults \
            to None.
        :type options: Optional[Dict[str, Any]]

        :raises TransformationError: if the node argument is not a \
            Range, if the Range node is not part of an ArrayReference, \
            if the Range node is not the outermost Range node of the \
            ArrayReference or if that ArrayReference does not \
            constitute the left hand side of an Assignment node.
        :raises TransformationError: if the node argument has nested array \
            expressions with Ranges or is an invalid tree with ranges in \
            multiple locations of a structure of arrays.
        :raises TransformationError: if the node argument contains a \
            non-elemental Operation or Call.

        '''
        # Am I Range node?
        if not isinstance(node, Range):
            raise TransformationError(
                f"Error in NemoArrayRange2LoopTrans transformation. The "
                f"supplied node argument should be a PSyIR Range, but "
                f"found '{type(node).__name__}'.")
        # Am I within an array reference?
        if not node.parent or not isinstance(node.parent, ArrayMixin):
            raise TransformationError(
                f"Error in NemoArrayRange2LoopTrans transformation. The "
                f"supplied node argument should be within an array access "
                f"node, but found '{type(node.parent).__name__}'.")
        # Is the array reference within an assignment?
        assignment = node.ancestor(Assignment)
        if not assignment:
            raise TransformationError(
                f"Error in NemoArrayRange2LoopTrans transformation. The "
                f"supplied node argument should be within an Assignment node, "
                f"but found a '{node}' that is not in an assignment.")
        # Is the array reference the lhs of the assignment?
        if node not in assignment.lhs.walk(Node):
            raise TransformationError(
                "Error in NemoArrayRange2LoopTrans transformation. The "
                "supplied node argument should be within an array access "
                "node that is within the left-hand-side of an Assignment "
                "node, but it is on the right-hand-side.")

        # We don't support nested range expressions
        for range_expr in assignment.walk(Range):
            ancestor_array = range_expr.parent.ancestor(ArrayMixin)
            if ancestor_array and any(index.walk(Range) for index
                                      in ancestor_array.indices):
                raise TransformationError(LazyString(
                    lambda: f"Error in NemoArrayRange2LoopTrans transformation"
                    f". This transformation does not support array assignments"
                    f" that contain nested Range structures, but found:"
                    f"\n{assignment.debug_string()}"))

        # Does the rhs of the assignment have any operations/calls that are not
        # elemental?
        for cnode in assignment.rhs.walk(Call):
            # Allow non elemental UBOUND and LBOUND.
            # TODO #2156 - add support for marking routines as being 'inquiry'
            # to improve this special-casing.
            if isinstance(cnode, IntrinsicCall):
                if cnode.intrinsic is IntrinsicCall.Intrinsic.LBOUND:
                    continue
                if cnode.intrinsic is IntrinsicCall.Intrinsic.UBOUND:
                    continue
                name = cnode.intrinsic.name
                type_txt = "IntrinsicCall"
            else:
                name = cnode.routine.name
                type_txt = "Call"
            if not cnode.is_elemental:
                # pylint: disable=cell-var-from-loop
                raise TransformationError(LazyString(
                    lambda: f"Error in NemoArrayRange2LoopTrans "
                    f"transformation. This transformation does not support non"
                    f"-elemental {type_txt}s on the rhs of the associated "
                    f"Assignment node, but found '{name}' in:\n"
                    f"{assignment.debug_string()}'."))

        # Do a single walk to avoid doing a separate one for each type we need
        nodes_to_check = assignment.walk((CodeBlock, Reference))

        # Do not allow to transform expressions with CodeBlocks
        if any(isinstance(n, CodeBlock) for n in nodes_to_check):
            raise TransformationError(LazyString(
                lambda: f"Error in NemoArrayRange2LoopTrans transformation. "
                f"This transformation does not support array assignments that"
                f" contain a CodeBlock anywhere in the expression, but found:"
                f"\n{assignment.debug_string()}"))

        references = [n for n in nodes_to_check if isinstance(n, Reference)]
        for reference in references:
            # As special case we always allow references to whole arrays as
            # part of the LBOUND and UBOUND intrinsics, regardless of the
            # restrictions below (e.g. is a UnresolvedType reference).
            if isinstance(reference.parent, IntrinsicCall):
                intrinsic = reference.parent.intrinsic
                if intrinsic is IntrinsicCall.Intrinsic.LBOUND:
                    continue
                if intrinsic is IntrinsicCall.Intrinsic.UBOUND:
                    continue

            # We allow any references that are part of a structure syntax - we
            # analyse its child components by continuing the reference list
            if isinstance(reference, (StructureReference, StructureMember)):
                continue

            # We allow any references that have explicit array syntax
            # because we infer that they are not scalars from the context
            # where they are found (even if they have UnresolvedType)
            if isinstance(reference, (ArrayReference, ArrayMember)):
                continue

            # However, if it doesn't have array accessors or structure syntax,
            # we must be sure that it represents a scalar.
            if not isinstance(reference.symbol, DataSymbol) or \
                    not isinstance(reference.symbol.datatype, ScalarType):
                raise TransformationError(
                    f"Error in NemoArrayRange2LoopTrans transformation. "
                    f"Variable '{reference.symbol.name}' must be a DataSymbol"
                    f" of ScalarType, but it's a '{reference.symbol}'.")

        # Is the Range node the outermost Range (as if not, the
        # transformation would be invalid)?
        for child in node.parent.indices[node.position+1:]:
            if isinstance(child, Range):
                raise TransformationError(
                    "Error in NemoArrayRange2LoopTrans transformation. This "
                    "transformation can only be applied to the outermost "
                    "Range.")


# For automatic document generation
__all__ = [
    'NemoArrayRange2LoopTrans']
