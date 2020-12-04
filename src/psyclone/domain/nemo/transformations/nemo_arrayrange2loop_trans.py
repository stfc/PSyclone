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

'''Module providing a transformation from a PSyIR Array Range to a
PSyIR NemoLoop. This is useful for capturing the contents of array
ranges as kernel regions so they can be optimised.

'''

from __future__ import absolute_import
import six
from psyclone.psyGen import Transformation
from psyclone.psyir.transformations.transformation_error \
    import TransformationError
from psyclone.psyir.symbols import DataSymbol, INTEGER_TYPE
from psyclone.psyir.nodes import Loop, Range, Reference, Array, Assignment, \
    Node, Operation, BinaryOperation, Literal
from psyclone.nemo import NemoLoop, NemoKern
from psyclone.psyir.transformations import ArrayRange2LoopTrans
from psyclone.configuration import Config


class NemoArrayRange2LoopTrans(ArrayRange2LoopTrans):
    '''Provides a transformation from a PSyIR Array Range to a PSyIR
    NemoLoop. For example:

    >>> from psyclone.parse.algorithm import parse
    >>> from psyclone.psyGen import PSyFactory
    >>> api = "nemo"
    >>> filename = "tra_adv_compute.F90"
    >>> ast, invoke_info = parse(filename, api=api)
    >>> psy = PSyFactory(api).create(invoke_info)
    >>> schedule = psy.invokes.invoke_list[0].schedule
    >>>
    >>> from psyclone.psyir.nodes import Assignment
    >>> from psyclone.domain.nemo.transformations import NemoArrayRange2LoopTrans
    >>> from psyclone.errors import TransformationError
    >>>
    >>> schedule.view()
    >>> trans = NemoArrayRange2LoopTrans()
    >>> for assignment in schedule.walk(Assignment):
    >>>     while True:
    >>>         try:
    >>>             trans.apply(assignment)
    >>>         except TransformationError:
    >>>             break
    >>> schedule.view()

    '''
    def apply(self, node, options=None):
        '''Apply the NemoArrayRange2Loop transformation to the specified
        node. The node must be an assignment. The rightmost range node
        in each array within the assignment is replaced with a loop
        index with the expected name for the particular loop dimension
        within a NemoLoop iterating over that index. The bounds of the
        loop are also determined by the expected bounds for the loop
        dimension.

        :param node: an Assignment node.
        :type node: :py:class:`psyclone.psyir.nodes.Assignment`

        '''

        self.validate(node)

        parent = node.parent
        symbol_table = node.scope.symbol_table

        loop_type_order = Config.get().api_conf("nemo").get_index_order()
        loop_type_data = Config.get().api_conf("nemo").get_loop_type_data()

        # Find the outermost dimension of the lhs array assignment
        # that is a Range
        loop_idx = _get_outer_index(node)
        loop_type = loop_type_order[loop_idx]
        loop_type_info = loop_type_data[loop_type]
        lower_bound = loop_type_info['start']
        upper_bound = loop_type_info['stop']
        loop_variable_name = loop_type_info['var']
        
        # Look up the loop variable in the symbol table. If it does
        # not exist then create it.
        try:
            loop_variable_symbol = symbol_table.lookup(loop_variable_name)
        except KeyError:
            # Add loop variable as it does not already exist
            loop_variable_symbol = DataSymbol(loop_variable_name, INTEGER_TYPE)
            symbol_table.add(loop_variable_symbol)

        try:
            _ = int(lower_bound)
            lower_bound_reference = Literal(lower_bound, INTEGER_TYPE)
        except ValueError:
            lower_bound_reference = Reference(symbol_table.lookup(lower_bound))
        try:
            _ = int(upper_bound)
            upper_bound_reference = Literal(upper_bound, INTEGER_TYPE)
        except ValueError:
            upper_bound_reference = Reference(symbol_table.lookup(upper_bound))
        
        # Replace the loop_idx array dimension with the loop variable
        # if it is a range.
        for array in node.walk(Array):
            if loop_idx<len(array.children):
                if isinstance(array.children[loop_idx], Range):
                    array.children[loop_idx] = Reference(loop_variable_symbol,
                                                         parent=array)
        position = node.position
        loop = NemoLoop.create(loop_variable_symbol, lower_bound_reference,
                               upper_bound_reference,
                               Literal("1", INTEGER_TYPE), [node])
        parent.children[position] = loop
        loop.parent = parent

        try:
            _ = _get_outer_index(node)
        except IndexError:
            # All valid array ranges have been replaced with explicit
            # loops. We now need to take the content of the loop and
            # place it within a NemoKern (inlined kernel) node.
            parent = node.parent
            # We do not provide the fparser2 ast of the code as we are
            # moving towards using visitors rather than gen_code when
            # outputting nemo api code
            inlined_kernel = NemoKern([node], None, parent=parent)
            parent.children = [inlined_kernel]

    def __str__(self):
        return ("Convert a PSyIR assignment to an Array Range into a "
                "PSyIR NemoLoop.")

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
        :type node: :py:class:`psyclone.psyir.nodes.Assignment`

        :raises TransformationError: if no conformant loop bounds were \
            found.
        :raises TransformationError: if an associated loop bound \
            specified in the config file is not declared in the symbol \
            table.

        '''
        super(NemoArrayRange2LoopTrans, self).validate(node, options=options)

        # Check that there is a valid index in which to apply this
        # transformation
        try:
            loop_idx = _get_outer_index(node)
        except IndexError as info:
            message = (
                "Error in NemoArrayRange2LoopTrans: The lhs of the "
                "supplied Assignment node should be a PSyIR Array "
                "with at least one of its dimensions being a Range "
                "with a valid configuration file loop bound "
                "specification, but found None.")
            six.raise_from(TransformationError(message), info)

        # Check that any variables in the specified bounds are already
        # defined
        symbol_table = node.scope.symbol_table
        loop_type_order = Config.get().api_conf("nemo").get_index_order()
        loop_type_data = Config.get().api_conf("nemo").get_loop_type_data()
        loop_type = loop_type_order[loop_idx]
        loop_type_info = loop_type_data[loop_type]
        lower_bound = loop_type_info['start']
        upper_bound = loop_type_info['stop']
        for bound in [lower_bound, upper_bound]:
            try:
                _ = int(bound)
            except ValueError:
                # bound is not a literal so should be a symbol
                try:
                    _ = symbol_table.lookup(bound)
                except KeyError as info:
                    # symbol is not found in the symbol table
                    message = (
                        "Error in NemoArrayRange2LoopTrans: Loop bound "
                        "symbol '{0}' is not explicitly declared in the "
                        "code.".format(bound))
                    six.raise_from(TransformationError(message), info)


def _get_outer_index(node):
    '''Returns the outermost loop index from the array on the lhs of the
    assignment node that has a range and has config information that
    provides values for the start and end of the loop. If one is not
    found then an exception is raised.

    :param node: the assignment node.
    :type node: :py:class:`psyclone.psyir.nodes.Assignment`

    :returns: a loop index.
    :rtype: int

    :raises IndexError: if no appropriate loop index is found.

    '''
    loop_type_order = Config.get().api_conf("nemo").get_index_order()
    loop_type_data = Config.get().api_conf("nemo").get_loop_type_data()
    lhs_array = node.lhs
    for idx, child in reversed(list(enumerate(lhs_array.children))):
        if isinstance(child, Range) and idx<len(loop_type_order):
            # This is a range in an array index with config information
            # defined
            loop_type = loop_type_order[idx]
            loop_type_info = loop_type_data[loop_type]
            lower_bound = loop_type_info['start']
            upper_bound = loop_type_info['stop']
            if lower_bound and upper_bound:
                # The config information specifies values for both
                # the start and end of the loop so we can
                # transform it
                return idx
    raise IndexError()


__all__ = [
    'NemoArrayRange2LoopTrans']
