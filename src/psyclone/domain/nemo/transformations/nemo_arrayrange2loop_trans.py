# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2020-2021, Science and Technology Facilities Council.
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

'''Module providing a transformation from an Assignment node
containing an Array Reference node in its left-hand-side which in turn
has at least one PSyIR Range node specifying an access to an array
index (equivalent to an array assignment statement in Fortran) to the
equivalent loop representation using a NemoLoop node. A Range node is
provided to the apply method of the tranformation to indicate which
array index should be transformed.

'''

from __future__ import absolute_import

from psyclone.psyGen import Transformation, InvokeSchedule
from psyclone.psyir.transformations.transformation_error \
    import TransformationError
from psyclone.errors import InternalError
from psyclone.psyir.symbols import DataSymbol, INTEGER_TYPE, DeferredType
from psyclone.psyir.symbols.datatypes import ScalarType
from psyclone.psyir.nodes import Range, Reference, ArrayReference, \
    Assignment, Literal, Operation, BinaryOperation
from psyclone.nemo import NemoLoop
from psyclone.configuration import Config
from psyclone.domain.nemo.transformations.create_nemo_kernel_trans \
    import CreateNemoKernelTrans


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
    >>> schedule.view()
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
    >>> schedule.view()

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
        index (so now has none) then the assigment is also placed
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
        array_index = node.position
        assignment = array_reference.parent
        parent = assignment.parent
        # Ensure we always use the routine-level symbol table
        symbol_table = node.ancestor(InvokeSchedule).symbol_table

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
        if not array_reference.is_lower_bound(array_index):
            # The range specifies a lower bound so use it
            lower_bound = node.start.copy()
        elif lower_bound_info:
            # The config metadata specifies a lower bound so use it
            try:
                _ = int(lower_bound_info)
                lower_bound = Literal(lower_bound_info, INTEGER_TYPE)
            except ValueError:
                lower_bound = Reference(symbol_table.lookup(lower_bound_info))
        else:
            # The lower bound is not set or specified so use the
            # LBOUND() intrinsic
            lower_bound = node.start.copy()

        # Upper bound
        if not array_reference.is_upper_bound(array_index):
            # The range specifies an upper bound so use it
            upper_bound = node.stop.copy()
        elif upper_bound_info:
            # The config metadata specifies an upper bound so use it
            try:
                _ = int(upper_bound_info)
                upper_bound = Literal(upper_bound_info, INTEGER_TYPE)
            except ValueError:
                upper_bound = Reference(symbol_table.lookup(upper_bound_info))
        else:
            # The upper bound is not set or specified so use the
            # UBOUND() intrinsic
            upper_bound = node.stop.copy()

        # Just use the specified step value
        step = node.step.copy()

        # Look up the loop variable in the symbol table. If it does
        # not exist then create it.
        try:
            loop_variable_symbol = symbol_table.lookup(loop_variable_name)
        except KeyError:
            # Add loop variable as it does not already exist
            loop_variable_symbol = DataSymbol(loop_variable_name, INTEGER_TYPE)
            symbol_table.add(loop_variable_symbol)

        # Replace the loop_idx array dimension with the loop variable.
        n_ranges = None
        for array in assignment.walk(ArrayReference):

            # Ignore the array reference if none of its index accesses
            # are Ranges
            if not any(child for child in array.children if
                       isinstance(child, Range)):
                continue
            # Ignore the array reference if any of its parents up to
            # the Assignment node are not Operations that return
            # scalars.
            ignore = False
            current = array.parent
            while not isinstance(current, Assignment):
                # Ignore if not a scalar valued operation (vector
                # valued operations are excluded in the validate
                # method).
                if not isinstance(current, Operation):
                    ignore = True
                    break
                current = current.parent
            if ignore:
                continue

            current_n_ranges = len([child for child in array.children
                                    if isinstance(child, Range)])
            if n_ranges is None:
                n_ranges = current_n_ranges
            elif n_ranges != current_n_ranges:
                raise InternalError(
                    "The number of ranges in the arrays within this "
                    "assignment are not equal. This is invalid PSyIR and "
                    "should never happen.")
            idx = get_outer_index(array)
            array.children[idx] = Reference(loop_variable_symbol)
        position = assignment.position
        loop = NemoLoop.create(loop_variable_symbol, lower_bound,
                               upper_bound, step, [assignment.detach()])
        parent.children.insert(position, loop)

        try:
            _ = get_outer_index(array_reference)
        except IndexError:
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
            consitute the left hand side of an Assignment node.

        '''
        # Am I Range node?
        if not isinstance(node, Range):
            raise TransformationError(
                "Error in NemoArrayRange2LoopTrans transformation. The "
                "supplied node argument should be a PSyIR Range, but "
                "found '{0}'.".format(type(node).__name__))
        # Am I within an array reference?
        if not node.parent or not isinstance(node.parent, ArrayReference):
            raise TransformationError(
                "Error in NemoArrayRange2LoopTrans transformation. The "
                "supplied node argument should be within an ArrayReference "
                "node, but found '{0}'.".format(type(node.parent).__name__))
        array_ref = node.parent
        # Is the array reference within an assignment?
        if not array_ref.parent or not isinstance(array_ref.parent,
                                                  Assignment):
            raise TransformationError(
                "Error in NemoArrayRange2LoopTrans transformation. The "
                "supplied node argument should be within an ArrayReference "
                "node that is within an Assignment node, but found '{0}'."
                .format(type(array_ref.parent).__name__))
        assignment = array_ref.parent
        # Is the array reference the lhs of the assignment?
        if assignment.lhs is not array_ref:
            raise TransformationError(
                "Error in NemoArrayRange2LoopTrans transformation. The "
                "supplied node argument should be within an ArrayReference "
                "node that is within the left-hand-side of an Assignment "
                "node, but it is on the right-hand-side.")
        # Does the rhs of the assignment have any operations that
        # return arrays as this is not currently supported?
        for operation in assignment.rhs.walk(Operation):
            # At the moment the only array valued operator is matmul
            if operation.operator == BinaryOperation.Operator.MATMUL:
                raise TransformationError(
                    "Error in NemoArrayRange2LoopTrans transformation. This "
                    "transformation does not support array valued operations "
                    "on the rhs of the associated Assignment node, but found "
                    "'{0}'.".format(operation.operator.name))

        # Is the Range node the outermost Range (as if not, the
        # transformation would be invalid)?
        if any(isinstance(node, Range) for node in
               node.parent.children[node.position+1:]):
            raise TransformationError(
                "Error in NemoArrayRange2LoopTrans transformation. This "
                "transformation can only be applied to the outermost "
                "Range.")

        # If there is a loop variable defined in the config file and
        # this variable is already defined in the code, is it defined
        # as an integer?
        array_index = node.position
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
                        "The config file specifies '{0}' as the name of the "
                        "iteration variable but this is already declared in "
                        "the code as something that is not a scalar integer, "
                        "or is a deferred type.".format(loop_variable_name))
            except KeyError:
                # Variable is not defined
                pass
        except IndexError:
            # There is no name for this index in the config file
            pass


def get_outer_index(array):
    '''Find the outermost index of the array that is a Range node. If one
    does not exist then raise an exception.

    :param array: the array being examined.
    :type array: :py:class:`psyclone.psyir.nodes.ArrayReference`

    :returns: the outermost index of the array that is a Range node.

    :raises IndexError: if the array does not contain a Range node.

    '''
    for child in reversed(array.children):
        if isinstance(child, Range):
            return child.position
    raise IndexError


# For automatic document generation
__all__ = [
    'NemoArrayRange2LoopTrans']
