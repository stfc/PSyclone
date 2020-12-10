# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2020, Science and Technology Facilities Council.
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

'''Module providing a transformation from a PSyIR ArrayReference Range
to a PSyIR NemoLoop. This is useful for capturing the contents of
array ranges as kernel regions so they can be optimised.

'''

from __future__ import absolute_import
import six
from psyclone.psyGen import Transformation
from psyclone.psyir.transformations.transformation_error \
    import TransformationError
from psyclone.errors import InternalError
from psyclone.psyir.symbols import DataSymbol, INTEGER_TYPE
from psyclone.psyir.nodes import Range, Reference, ArrayReference, \
    Assignment, Literal
from psyclone.nemo import NemoLoop, NemoKern
from psyclone.configuration import Config


class NemoArrayRange2LoopTrans(Transformation):
    '''Provides a transformation from a PSyIR ArrayReference Range to a
    PSyIR NemoLoop. For example:

    >>> schedule = ...
    >>> range_node = ...
    >>> from psyclone.domain.nemo.transformations import \
    >>>     NemoArrayRange2LoopTrans
    >>> trans = NemoArrayRange2LoopTrans()
    >>> trans.apply(range_node)

    '''
    def apply(self, node, options=None):
        '''Apply the NemoArrayRange2Loop transformation to the specified
        node. The node must be a Range node within an array reference
        on the lhs of an assignment. If the bounds of the Range are
        specified then these are used. If they are not specified then
        the lbound and ubound intrinsics are used unless the config
        file has specified bounds, in which case these are used.

        :param node: a Range node.
        :type node: :py:class:`psyclone.psyir.nodes.Range`
        :param options: a dictionary with options for \
            transformations. No options are used in this \
            transformation. This is an optional argument that defaults \
            to None.
        :type options: dictionary of string:values or None

        '''
        self.validate(node)

        array_reference = node.parent
        array_index = array_reference.children.index(node)
        assignment = array_reference.parent
        parent = assignment.parent
        symbol_table = node.scope.symbol_table

        # See if there is any configuration information for this array index
        loop_type_order = Config.get().api_conf("nemo").get_index_order()
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
            loop_variable_name = symbol_table.new_symbol_name("idx")

        # Lower bound
        if not array_reference.is_lower_bound(array_index):
            # The range specifies a lower bound so use it
            lower_bound = node.start
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
            lower_bound = node.start

        # Upper bound
        if not array_reference.is_upper_bound(array_index):
            # The range specifies an upper bound so use it
            upper_bound = node.start
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
            upper_bound = node.stop

        # Just use the specified step value
        step = node.step

        # Look up the loop variable in the symbol table. If it does
        # not exist then create it.
        try:
            loop_variable_symbol = symbol_table.lookup(loop_variable_name)
        except KeyError:
            # Add loop variable as it does not already exist
            loop_variable_symbol = DataSymbol(loop_variable_name, INTEGER_TYPE)
            symbol_table.add(loop_variable_symbol)

        # Replace the loop_idx array dimension with the loop variable.
        for array in assignment.walk(ArrayReference):
            try:
                idx = _get_outer_index(array)
            except IndexError as info:
                six.raise_from(InternalError(
                    "The number of ranges in the arrays within this "
                    "assignment are not equal. This is invalid PSyIR and "
                    "should never happen."), info)
            array.children[idx] = Reference(loop_variable_symbol, parent=array)
        position = assignment.position
        loop = NemoLoop.create(loop_variable_symbol, lower_bound,
                               upper_bound, step, [assignment])
        parent.children[position] = loop
        loop.parent = parent

        try:
            _ = _get_outer_index(array_reference)
        except IndexError:
            # All valid array ranges have been replaced with explicit
            # loops. We now need to take the content of the loop and
            # place it within a NemoKern (inlined kernel) node.
            parent = assignment.parent
            # We do not provide the fparser2 ast of the code as we are
            # moving towards using visitors rather than gen_code when
            # outputting nemo api code
            inlined_kernel = NemoKern([assignment], None, parent=parent)
            parent.children = [inlined_kernel]

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

        :raises TransformationError: ...

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
        # Is the Range node the outermost Range (as if not, the
        # transformation would be invalid).
        my_index = node.parent.children.index(node)
        num_siblings = len(node.parent.children)
        for idx in range(my_index+1, num_siblings):
            following_node = node.parent.children[idx]
            if isinstance(following_node, Range):
                raise TransformationError(
                    "Error in NemoArrayRange2LoopTrans transformation. This "
                    "transformation can only be applied to the outermost "
                    "Range.")


def _get_outer_index(array):
    '''Find the outermost index of the array that is a Range node. If one
    does not exist then raise an exception.

    :param array: the array being examined.
    :type array: :py:class:`psyclone.psyir.nodes.ArrayReference`

    :returns: the outermost index of the array that is a Range node.

    :raises IndexError: if the array does not contain a Range node.

    '''
    idx = len(array.children)-1
    while idx >= 0 and not isinstance(array.children[idx], Range):
        idx -= 1
    if idx < 0:
        raise IndexError
    return idx


__all__ = [
    'NemoArrayRange2LoopTrans']
