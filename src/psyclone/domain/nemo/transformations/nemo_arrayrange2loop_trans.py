# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2020-2022, Science and Technology Facilities Council.
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
# Modified S. Siso, STFC Daresbury Lab

'''Module providing a transformation from an Assignment node
containing an Array Reference node in its left-hand-side which in turn
has at least one PSyIR Range node specifying an access to an array
index (equivalent to an array assignment statement in Fortran) to the
equivalent loop representation using a NemoLoop node. A Range node is
provided to the apply method of the tranformation to indicate which
array index should be transformed.

'''

from __future__ import absolute_import
from psyclone.configuration import Config
from psyclone.domain.nemo.transformations.create_nemo_kernel_trans import \
    CreateNemoKernelTrans
from psyclone.errors import LazyString, InternalError
from psyclone.nemo import NemoLoop
from psyclone.psyGen import Transformation
from psyclone.psyir.backend.fortran import FortranWriter
from psyclone.psyir.nodes import Range, Reference, ArrayReference, Call, \
    Assignment, Literal, Operation, CodeBlock, ArrayMember, Loop, Routine, \
    BinaryOperation, StructureReference, StructureMember, Node
from psyclone.psyir.nodes.array_mixin import ArrayMixin
from psyclone.psyir.symbols import DataSymbol, INTEGER_TYPE, DeferredType, \
    ScalarType
from psyclone.psyir.transformations.transformation_error import \
    TransformationError


class NemoArrayRange2LoopTrans(Transformation):
    '''Provides a transformation from a PSyIR ArrayReference Range to a
    PSyIR NemoLoop. For example:

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
        '''Apply the NemoArrayRange2Loop transformation if the supplied node
        is the outermost Range node (specifying an access to an array
        index) within an Array Reference that is on the left-hand-side
        of an Assignment node. These constraints are required for
        correctness and an exception will be raised if they are not
        satisfied. If the constraints are satisfied then the outermost
        Range nodes within array references within the Assignment node
        are replaced with references to a loop index. A NemoLoop loop
        (with the same loop index) is also placed around the modified
        assignment statement. If the array reference on the
        left-hand-side of the assignment only had one range node as an
        index (so now has none) then the assignment is also placed
        within a NemoKern.

        The name of the loop index is taken from the PSyclone
        configuration file if a name exists for the particular array
        index, otherwise a new name is generated. The bounds of the
        loop are taken from the Range node if they are provided. If
        not, the loop bounds are taken from the PSyclone configuration
        file if bounds values are supplied. If not, the LBOUND or
        UBOUND intrinsics are used as appropriate. The type of the
        NemoLoop is also taken from the configuration file if it is
        supplied for that index, otherwise it is specified as being
        "unknown".

        :param node: a Range node.
        :type node: :py:class:`psyclone.psyir.nodes.Range`
        :param options: a dictionary with options for \
            transformations. No options are used in this \
            transformation. This is an optional argument that defaults \
            to None.
        :type options: dict of string:values or None

        '''
        self.validate(node)

        array_reference = node.parent
        array_index = node.parent.indices.index(node)
        assignment = node.ancestor(Assignment)
        parent = assignment.parent
        # Ensure we always use the routine-level symbol table
        symbol_table = node.ancestor(Routine).symbol_table

        # See if there is any configuration information for this array index
        loop_type_order = Config.get().api_conf("nemo").get_index_order()
        # TODO: Add tests in get_loop_type_data() to make sure values
        # are strings that represent an integer or a valid variable
        # name, e.g. 1a should not be allowed. See issue #1035
        loop_type_data = Config.get().api_conf("nemo").get_loop_type_data()
        try:
            loop_type = loop_type_order[array_index]
            loop_type_info = loop_type_data[loop_type]
            lower_bound_info = loop_type_info['start']
            upper_bound_info = loop_type_info['stop']
            loop_variable_name = loop_type_info['var']
        except IndexError:
            lower_bound_info = None
            upper_bound_info = None
            loop_variable_name = symbol_table.next_available_name("idx")

        # Lower bound
        lower_bound = None
        if not array_reference.is_lower_bound(array_index):
            # The range specifies a lower bound so use it
            lower_bound = node.start.copy()
        elif lower_bound_info:
            # The config metadata specifies a lower bound so use it
            try:
                _ = int(lower_bound_info)
                lower_bound = Literal(lower_bound_info, INTEGER_TYPE)
            except ValueError:
                # It's not a Literal, but is it an existing symbol?
                try:
                    lower_bound = Reference(
                        symbol_table.lookup(lower_bound_info))
                except KeyError:
                    # The config lower bound symbol name does not exist
                    pass
        # The lower bound is still not set so use the LBOUND() intrinsic
        if not lower_bound:
            lower_bound = node.start.copy()

        # Upper bound
        upper_bound = None
        if not array_reference.is_upper_bound(array_index):
            # The range specifies an upper bound so use it
            upper_bound = node.stop.copy()
        elif upper_bound_info:
            # The config metadata specifies an upper bound so use it
            try:
                _ = int(upper_bound_info)
                upper_bound = Literal(upper_bound_info, INTEGER_TYPE)
            except ValueError:
                # It's not a Literal, but is it an existing symbol?
                try:
                    upper_bound = Reference(
                        symbol_table.lookup(upper_bound_info))
                except KeyError:
                    # The config upper bound symbol name does not exist
                    pass
        # The upper_bound bound is still not set so use the UBOUND() intrinsic
        if not upper_bound:
            upper_bound = node.stop.copy()

        # Just use the specified step value
        step = node.step.copy()

        # Look up the loop variable in the symbol table. If it does
        # not exist then create it.
        loop_variable_symbol = symbol_table.find_or_create(
                loop_variable_name, symbol_type=DataSymbol,
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
        loop = NemoLoop.create(loop_variable_symbol, lower_bound,
                               upper_bound, step, [assignment.detach()])
        parent.children.insert(position, loop)

        if not assignment.lhs.walk(Range):
            # All valid array ranges have been replaced with explicit
            # loops. We now need to take the content of the loop and
            # place it within a NemoKern (inlined kernel) node.
            CreateNemoKernelTrans().apply(assignment.parent)

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
        :type options: dict of string:values or None

        :raises TransformationError: if the node argument is not a \
            Range, if the Range node is not part of an ArrayReference, \
            if the Range node is not the outermost Range node of the \
            ArrayReference or if that ArrayReference does not \
            constitute the left hand side of an Assignment node.
        :raises TransformationError: if the node argument has nested array \
            expressions with Ranges or is an invalid tree with ranges in \
            multiple locations of a structure of arrays.

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
                    f"\n{FortranWriter()(assignment)}"))

        # Does the rhs of the assignment have any operations that are not
        # elemental?
        for operation in assignment.rhs.walk(Operation):
            # Allow non elemental UBOUND and LBOUND
            if operation.operator is BinaryOperation.Operator.LBOUND:
                continue
            if operation.operator is BinaryOperation.Operator.UBOUND:
                continue
            if not operation.is_elemental():
                raise TransformationError(
                    f"Error in NemoArrayRange2LoopTrans transformation. This "
                    f"transformation does not support non-elemental operations"
                    f" on the rhs of the associated Assignment node, but found"
                    f" '{operation.operator.name}'.")

        # Do a single walk to avoid doing a separate one for each type we need
        nodes_to_check = assignment.walk((CodeBlock, Call, Reference))

        # Do not allow to transform expressions with CodeBlocks
        if any(isinstance(n, CodeBlock) for n in nodes_to_check):
            raise TransformationError(LazyString(
                lambda: f"Error in NemoArrayRange2LoopTrans transformation. "
                f"This transformation does not support array assignments that"
                f" contain a CodeBlock anywhere in the expression, but found:"
                f"\n{FortranWriter()(assignment)}"))
        # Do not allow to transform expressions with function calls (to allow
        # this we need to differentiate between elemental and not elemental
        # functions as they have different semantics in array notation)
        if any(isinstance(n, Call) for n in nodes_to_check):
            raise TransformationError(LazyString(
                lambda: f"Error in NemoArrayRange2LoopTrans transformation. "
                f"This transformation does not support array assignments that"
                f" contain a Call anywhere in the expression, but found:"
                f"\n{FortranWriter()(assignment)}"))

        references = [n for n in nodes_to_check if isinstance(n, Reference)]
        for reference in references:
            # As special case we always allow references to whole arrays as
            # part of the LBOUND and UBOUND operations, regardless of the
            # restrictions below (e.g. is a DeferredType reference).
            if isinstance(reference.parent, Operation):
                operator = reference.parent.operator
                if operator is BinaryOperation.Operator.LBOUND:
                    continue
                if operator is BinaryOperation.Operator.UBOUND:
                    continue

            # We allow any references that are part of a structure syntax - we
            # analyse its child components by continuing the reference list
            if isinstance(reference, (StructureReference, StructureMember)):
                continue

            # We allow any references that have explicit array syntax
            # because we infer that they are not scalars from the context
            # where they are found (even if they have DeferredType)
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
        for child in node.parent.indices[node.parent.indices.index(node)+1:]:
            if isinstance(child, Range):
                raise TransformationError(
                    "Error in NemoArrayRange2LoopTrans transformation. This "
                    "transformation can only be applied to the outermost "
                    "Range.")

        # If there is a loop variable defined in the config file and
        # this variable is already defined in the code, is it defined
        # as an integer?
        array_index = node.parent.indices.index(node)
        loop_type_order = Config.get().api_conf("nemo").get_index_order()
        loop_type_data = Config.get().api_conf("nemo").get_loop_type_data()
        try:
            loop_type = loop_type_order[array_index]
            loop_type_info = loop_type_data[loop_type]
            loop_variable_name = loop_type_info['var']
            try:
                symbol_table = node.scope.symbol_table
                loop_variable_symbol = symbol_table.lookup(loop_variable_name)
                # Check the existing loop variable name is a scalar integer
                if isinstance(loop_variable_symbol, DeferredType) or \
                   not (loop_variable_symbol.is_scalar and
                        loop_variable_symbol.datatype.intrinsic is
                        ScalarType.Intrinsic.INTEGER):
                    raise TransformationError(
                        f"The config file specifies '{loop_variable_name}' as "
                        f"the name of the iteration variable but this is "
                        f"already declared in the code as something that is "
                        f"not a scalar integer or a deferred type.")
            except KeyError:
                # Variable is not defined
                pass

            # Make sure this array is not already inside a loop with the
            # loop_variable_name that we choose. If it is we ignore it (as
            # usually we will already have some explicit loops above this
            # construct to apply further parallelisation/optimisations).
            current = node.parent
            while not isinstance(current, Routine):
                if isinstance(current, Loop):
                    varname = current.variable.name
                    if varname.lower() == loop_variable_name.lower():
                        raise TransformationError(LazyString(
                            lambda: f"The config file specifies "
                            f"'{loop_variable_name}' as the name of the "
                            f"iteration variable but this is already used "
                            f"by an ancestor loop variable in:"
                            f"\n{FortranWriter()(current)}"))
                current = current.parent
            # Note that we don't check if the variable name is used in a
            # context other that the loop if it's an integer. We assume this
            # is a guarantee of the API.

        except IndexError:
            # There is no name for this index in the config file (it can
            # proceed as the apply will generate a new variable for it)
            pass


# For automatic document generation
__all__ = [
    'NemoArrayRange2LoopTrans']
