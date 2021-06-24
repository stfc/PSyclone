# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2021, Science and Technology Facilities Council.
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
has at least one constant access to an array index. The node
representing the array index is provided to the apply method of the
tranformation to indicate which array index should be transformed.

'''

from __future__ import absolute_import

from psyclone.configuration import Config
from psyclone.nemo import NemoLoop
from psyclone.psyGen import Transformation
from psyclone.psyir.nodes import Range, Reference, ArrayReference, \
    Assignment, Literal, Node, Schedule, Loop
from psyclone.psyir.symbols import DataSymbol, INTEGER_TYPE
from psyclone.psyir.transformations.transformation_error \
    import TransformationError


class NemoArrayAccess2LoopTrans(Transformation):
    '''Provides a transformation from a PSyIR ArrayReference access to a
    PSyIR NemoLoop. For example:

    >>> from psyclone.parse.algorithm import parse
    >>> from psyclone.psyGen import PSyFactory
    >>> from psyclone.domain.nemo.transformations import \
            NemoArrayAccess2LoopTrans
    >>> from psyclone.psyir.nodes import Assignment
    >>> from psyclone.transformations import TransformationError
    >>>
    >>> api = "nemo"
    >>> filename = "tra_adv.F90" # examples/nemo/code
    >>> ast, invoke_info = parse(filename, api=api)
    >>> psy = PSyFactory(api).create(invoke_info)
    >>> schedule = psy.invokes.invoke_list[0].schedule
    >>> schedule.view()
    >>>
    >>> trans = NemoArrayAccess2LoopTrans()
    >>> for assignment in schedule.walk(Assignment):
    >>>     for index in asignment.lhs.children:
    >>>         try:
    >>>             trans.apply(index)
    >>>         except TransformationError:
    >>>             pass
    >>> schedule.view()

    '''
    def apply(self, node, options=None):
        '''Apply the NemoArrayAccess2Loop transformation if the supplied node
        is an access to an array index within an Array Reference that
        is on the left-hand-side of an Assignment node. The access
        must be a scalar (i.e. not a range) and must not include a
        loop variable (as we are transforming a single access to a
        loop).

        These constraints are required for correctness and an
        exception will be raised if they are not satisfied. If the
        constraints are satisfied then the array access is replaced
        with a loop iterator and a single trip loop.

        If there are existing loops then the new loop will be placed
        at the expected nesting, complying to the ordering expected by
        the NEMO API (specified in the config file). If the existing
        loops do not conform to this expected ordering then an
        exception will be raised.

        The name of the loop index is taken from the PSyclone
        configuration file if a name exists for the particular array
        index, otherwise a new name is generated.

        :param node: an array index.
        :type node: :py:class:`psyclone.psyir.nodes.Node`
        :param options: a dictionary with options for \
            transformations. No options are used in this \
            transformation. This is an optional argument that defaults \
            to None.
        :type options: dict of string:values or None

        '''
        self.validate(node)

        array_index = node.position
        array_reference = node.parent
        assignment = array_reference.parent
        symbol_table = node.scope.symbol_table

        node_copy = node.copy()

        # See if there is any configuration information for this array index
        loop_type_order = Config.get().api_conf("nemo").get_index_order()
        # TODO: Add tests in get_loop_type_data() to make sure values
        # are strings that represent an integer or a valid variable
        # name, e.g. 1a should not be allowed. See issue #1035
        loop_type_data = Config.get().api_conf("nemo").get_loop_type_data()
        try:
            loop_type = loop_type_order[array_index]
            loop_type_info = loop_type_data[loop_type]
            loop_variable_name = loop_type_info['var']
        except IndexError:
            loop_variable_name = symbol_table.next_available_name("idx")

        # Work out where to add the new loop (at 'location'), as there
        # may be existing loops that should be inside this one.
        symbols = [ref.symbol for ref in array_reference.children[:array_index]
                   if isinstance(ref, Reference)]
        location = assignment
        idx = 0
        while (isinstance(location.parent, Schedule) and
               isinstance(location.parent.parent, Loop) and
               idx < len(symbols)):
            idx += 1
            location = location.parent.parent

        # Look up the loop variable in the symbol table. If it does
        # not exist then create it.
        try:
            loop_variable_symbol = symbol_table.lookup(loop_variable_name)
        except KeyError:
            # Add loop variable as it does not already exist
            loop_variable_symbol = DataSymbol(loop_variable_name, INTEGER_TYPE)
            symbol_table.add(loop_variable_symbol)

        # Replace array access loop variable.
        for array in assignment.walk(ArrayReference):
            if not array.ancestor(ArrayReference):
                # This is not a nested access e.g. a(b(n)).
                array.children[array_index] = Reference(loop_variable_symbol)

        # As this transformation affects ancestor nodes of the
        # supplied node we need to be careful here not to lose
        # existing ancestor nodes as they might be
        # referenced. Therefore use detach() and insert,() rather than
        # copy() and replace_with().
        
        # Find location.parent and location_index
        loc_parent = location.parent
        loc_index = location.position

        # Create the new single-trip loop and add its children.
        step = Literal("1", INTEGER_TYPE)
        loop = NemoLoop.create(loop_variable_symbol, node_copy,
                               node_copy.copy(), step, [location.detach()])
        # Replace the original assignment with a loop containing the
        # modified assignment.
        loc_parent.children.insert(loc_index, loop)

    def validate(self, node, options=None):
        '''Perform various checks to ensure that it is valid to apply the
        NemoArrayAccess2LoopTrans transformation to the supplied PSyIR Node.

        :param node: the node that is being checked.
        :type node: :py:class:`psyclone.psyir.nodes.Node`
        :param options: a dictionary with options for \
            transformations. No options are used in this \
            transformation. This is an optional argument that defaults \
            to None.
        :type options: dict of string:values or None

        '''
        # Not a PSyIR node
        if not isinstance(node, Node):
            raise TransformationError(
                "Error in NemoArrayAccess2LoopTrans transformation. The "
                "supplied node argument should be a PSyIR Node, but found "
                "'{0}'.".format(type(node).__name__))
        # Not within an array reference
        if not node.parent or not isinstance(node.parent, ArrayReference):
            raise TransformationError(
                "Error in NemoArrayAccess2LoopTrans transformation. The "
                "supplied node argument should be within an ArrayReference "
                "node, but found '{0}'.".format(type(node.parent).__name__))
        array_ref = node.parent
        # Array reference not within an assignment
        if not array_ref.parent or not isinstance(array_ref.parent,
                                                  Assignment):
            raise TransformationError(
                "Error in NemoArrayAccess2LoopTrans transformation. The "
                "supplied node argument should be within an ArrayReference "
                "node that is within an Assignment node, but found '{0}'."
                .format(type(array_ref.parent).__name__))
        assignment = array_ref.parent
        # Array reference not on lhs of the assignment
        if assignment.lhs is not array_ref:
            raise TransformationError(
                "Error in NemoArrayAccess2LoopTrans transformation. The "
                "supplied node argument should be within an ArrayReference "
                "node that is within the left-hand-side of an Assignment "
                "node, but it is on the right-hand-side.")

        # Contains a range node
        if node.walk(Range):
            raise TransformationError(
                "Error in NemoArrayAccess2LoopTrans transformation. The "
                "supplied node should not be or contain a Range node as it "
                "should be single valued.")

        # Capture loop iterator symbols in order
        iterator_symbols = []
        location = node.parent.parent
        while (isinstance(location.parent, Schedule) and
               isinstance(location.parent.parent, Loop)):
            location = location.parent.parent
            iterator_symbols.append(location.variable)

        # Index contains a loop iterator
        for reference in node.walk(Reference):
            if reference.symbol in iterator_symbols:
                raise TransformationError(
                    "Error in NemoArrayAccess2LoopTrans transformation. The "
                    "supplied node should not be or contain a loop iterator, "
                    "it should be single valued.")

        # An index before this one has more than one loop iterator
        array_access_iterators = []
        for index in node.parent.children[:node.position]:
            # Find any iterators in this index
            index_iterators = [
                my_index.symbol for my_index in index.walk(Reference)
                if my_index.symbol in iterator_symbols]
            iterator_names = [my_index.name for my_index in index_iterators]
            if len(index_iterators) > 1:
                raise TransformationError(
                    "Only a single iterator per dimension is supported by "
                    "this transformation, but found '{0}'."
                    "".format(iterator_names))
            if index_iterators:
                array_access_iterators.append(index_iterators[0])

        # Same iterator used in more than one loop index
        if len(iterator_symbols) < len(array_access_iterators):
            raise TransformationError(
                "The same iterator is used in more than one loop index "
                "'{0}' which is not supported by this transformation.".format(
                    [iterator.name for iterator in array_access_iterators]))

        # Iterators before this index are not in loop index order
        for idx, array_access_iterator in enumerate(array_access_iterators):
            if array_access_iterator != iterator_symbols[idx]:
                raise TransformationError(
                    "In array '{0}' expected iterator '{1}' at index '{2}' "
                    "but found '{3}'.".format(
                        node.parent.name, iterator_symbols[idx].name,
                        idx, array_access_iterator.name))

        # Indices on lhs and rhs array accesses are not the same
        index_pos = node.position
        assignment = node.parent.parent
        for array_reference in assignment.rhs.walk(ArrayReference):
            if array_reference.ancestor(ArrayReference):
                # skip validation as this is an array reference within
                # an array reference.
                continue
            if not array_reference.children[index_pos].math_equal(node):
                raise TransformationError(
                    "Expected index '{0}' for rhs array '{1}' to be the same "
                    "as the lhs array '{2}', but they differ."
                    "".format(
                        index_pos, array_reference.symbol.name,
                        node.parent.name))

    def __str__(self):
        return (
            "Convert the PSyIR assignment for a specified ArrayReference "
            "access into a PSyIR NemoLoop.")

    @property
    def name(self):
        '''
        :returns: the name of the transformation as a string.
        :rtype: str

        '''
        return type(self).__name__


# For automatic document generation
__all__ = [
    'NemoArrayAccess2LoopTrans']
