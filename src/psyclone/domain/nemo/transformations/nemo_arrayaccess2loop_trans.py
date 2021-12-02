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

'''Module providing a transformation that transforms a constant index
access to an array (i.e. one that does not contain a loop iterator) to
a single trip loop. The node representing the constant index access is
provided to the apply method of the transformation to indicate which
array index should be transformed.

'''

from __future__ import absolute_import

from psyclone.configuration import Config
from psyclone.core import SymbolicMaths
from psyclone.domain.nemo.transformations.create_nemo_kernel_trans \
    import CreateNemoKernelTrans
from psyclone.nemo import NemoLoop, NemoKern
from psyclone.psyGen import Transformation
from psyclone.psyir.nodes import Range, Reference, ArrayReference, \
    Assignment, Literal, Node, Schedule, Loop
from psyclone.psyir.symbols import DataSymbol, INTEGER_TYPE
from psyclone.psyir.transformations.transformation_error \
    import TransformationError

# pylint: disable=too-many-locals
# pylint: disable=too-many-branches


class NemoArrayAccess2LoopTrans(Transformation):
    '''Provides a transformation to transform a constant index access to
    an array (i.e. one that does not contain a loop iterator) to a
    single trip loop. For example:

    >>> from psyclone.domain.nemo.transformations import \\
    ...     NemoArrayAccess2LoopTrans
    >>> from psyclone.psyir.backend.fortran import FortranWriter
    >>> from psyclone.psyir.frontend.fortran import FortranReader
    >>> from psyclone.psyir.nodes import Assignment
    >>> code = ("program example\\n"
    ...         "  real a(10)\\n"
    ...         "  a(1) = 0.0\\n"
    ...         "end program example\\n")
    >>> psyir = FortranReader().psyir_from_source(code)
    >>> assignment = psyir.walk(Assignment)[0]
    >>> NemoArrayAccess2LoopTrans().apply(assignment.lhs.children[0])
    >>> print(FortranWriter()(psyir))
    program example
      real, dimension(10) :: a
      integer :: ji
    <BLANKLINE>
      do ji = 1, 1, 1
        a(ji) = 0.0
      enddo
    <BLANKLINE>
    end program example
    <BLANKLINE>

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

        The new loop will be placed immediately around the assignment
        i.e. it will not take into account any expected nesting (ji,
        jj, jk etc) constraints. Loop re-ordering should be performed by
        a separate transformation.

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

        # Look up the loop variable in the symbol table. If it does
        # not exist then create it.
        loop_variable_symbol = symbol_table.find_or_create(
                loop_variable_name, symbol_type=DataSymbol,
                datatype=INTEGER_TYPE)

        # Replace current access with loop variable.
        for array in assignment.walk(ArrayReference):
            if not array.ancestor(ArrayReference):
                # This is not a nested access e.g. a(b(n)).
                array.indices[array_index] = Reference(loop_variable_symbol)

        # Determine the loop body and where to add the loop.
        nemo_kern = assignment.ancestor(NemoKern)
        if nemo_kern:
            # This assignment is inside a NemoKern
            loop_body = nemo_kern
        else:
            # There is no parent NemoKern
            loop_body = assignment
        loc_parent = loop_body.parent
        loc_index = loop_body.position

        # Create the new single-trip loop and add its children.
        step = Literal("1", INTEGER_TYPE)
        loop = NemoLoop.create(loop_variable_symbol, node_copy,
                               node_copy.copy(), step, [loop_body.detach()])

        # Replace the original assignment with a loop containing the
        # modified assignment.
        loc_parent.children.insert(loc_index, loop)

        # Add a NemoKern if required.
        if not nemo_kern and not assignment.walk(Range):
            # This was not previously a NemoKern (as it contained no
            # loops). However, we have now created a loop so, provided
            # there are no range nodes, we must create an inlined
            # kernel.
            CreateNemoKernelTrans().apply(assignment.parent)

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
                "node that is within an Assignment node, but found '{0}' "
                "instead of an Assignment."
                .format(type(array_ref.parent).__name__))
        assignment = array_ref.parent
        # Array reference not on lhs of the assignment
        if assignment.lhs is not array_ref:
            raise TransformationError(
                "Error in NemoArrayAccess2LoopTrans transformation. The "
                "supplied node argument should be within an ArrayReference "
                "node that is within the left-hand-side of an Assignment "
                "node, but '{0}' is on the right-hand-side of '{1}'."
                "".format(self._writer(array_ref), self._writer(assignment)))

        # Contains a range node
        if node.walk(Range):
            raise TransformationError(
                "Error in NemoArrayAccess2LoopTrans transformation. The "
                "supplied node should not be or contain a Range node "
                "(array notation) as it should be single valued, but found "
                "'{0}'.".format(self._writer(node)))

        # Capture loop iterator symbols in order
        iterator_symbols = []
        location = node.parent.parent
        while (isinstance(location.parent, Schedule) and
               isinstance(location.parent.parent, (Loop, NemoKern))):
            location = location.parent.parent
            if isinstance(location, Loop):
                iterator_symbols.append(location.variable)

        # The iterator name I should be using is already being used as
        # a loop iterator.
        loop_type_order = Config.get().api_conf("nemo").get_index_order()
        loop_type_data = Config.get().api_conf("nemo").get_loop_type_data()
        try:
            loop_type = loop_type_order[node.position]
            loop_type_info = loop_type_data[loop_type]
            loop_variable_name = loop_type_info['var']
            if (loop_variable_name.lower() in [
                    var.name.lower() for var in iterator_symbols]):
                raise TransformationError(
                    "Error in NemoArrayAccess2LoopTrans transformation. The "
                    "NEMO API expects index {0} to use the '{1}' iterator "
                    "variable, but it is already being used in another index "
                    "'{2}'.".format(
                        node.position, loop_variable_name.lower(),
                        self._writer(assignment.lhs)))
        except IndexError:
            # There is no defined iterator name for this index
            pass

        # Index contains a loop iterator
        for reference in node.walk(Reference):
            if reference.symbol in iterator_symbols:
                raise TransformationError(
                    "Error in NemoArrayAccess2LoopTrans transformation. The "
                    "supplied node should not be or contain a loop iterator, "
                    "it should be single valued.")

        # Indices on lhs and rhs array accesses are not the same
        index_pos = node.position
        assignment = node.parent.parent
        sym_maths = SymbolicMaths.get()
        for array_reference in assignment.rhs.walk(ArrayReference):
            if array_reference.ancestor(ArrayReference):
                # skip validation as this is an array reference within
                # an array reference.
                continue
            if not sym_maths.equal(array_reference.children[index_pos], node):
                raise TransformationError(
                    "Expected index '{0}' for rhs array '{1}' to be the same "
                    "as that for the lhs array '{2}', but they differ in "
                    "'{3}'.".format(
                        index_pos, array_reference.symbol.name,
                        node.parent.name, self._writer(assignment)))

    def __str__(self):
        return (
            "Convert the PSyIR assignment for a specified ArrayReference "
            "access into a PSyIR NemoLoop.")


# For automatic document generation
__all__ = [
    'NemoArrayAccess2LoopTrans']
