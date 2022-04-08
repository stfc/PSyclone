# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2021-2022, Science and Technology Facilities Council.
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
# Authors R. W. Ford, A. R. Porter, and S. Siso STFC Daresbury Lab
#         A. B. G. Chalk STFC Daresbury Lab
#         J. Henrichs, Bureau of Meteorology
# Modified I. Kavcic, Met Office

''' This module provides the loop swap transformation.'''

from psyclone.psyir.nodes import Call, CodeBlock, Reference
from psyclone.psyir.transformations.loop_trans import LoopTrans
from psyclone.psyir.transformations.transformation_error import \
        TransformationError


class LoopSwapTrans(LoopTrans):
    ''' Provides a loop-swap transformation, e.g.:

    .. code-block:: fortran

        DO j=1, m
            DO i=1, n

    becomes:

    .. code-block:: fortran

        DO i=1, n
            DO j=1, m

    This transform is used as follows:

     >>> from psyclone.parse.algorithm import parse
     >>> from psyclone.psyGen import PSyFactory
     >>> ast, invokeInfo = parse("shallow_alg.f90")
     >>> psy = PSyFactory("gocean1.0").create(invokeInfo)
     >>> schedule = psy.invokes.get('invoke_0').schedule
     >>> # Uncomment the following line to see a text view of the schedule
     >>> # print(schedule.view())
     >>>
     >>> from psyclone.transformations import LoopSwapTrans
     >>> swap = LoopSwapTrans()
     >>> swap.apply(schedule.children[0])
     >>> # Uncomment the following line to see a text view of the schedule
     >>> # print(schedule.view())

    '''

    excluded_node_types = (Call, CodeBlock)

    def __str__(self):
        return "Exchange the order of two nested loops: inner becomes " + \
               "outer and vice versa"

    def validate(self, node, options=None):
        # pylint: disable=arguments-differ
        '''Checks if the given node contains a valid Fortran structure
        to allow swapping loops. This means the node must represent
        a loop, and it must have exactly one child that is also a loop.

        :param node_outer: a Loop node from an AST.
        :type node_outer: py:class:`psyclone.psyir.nodes.Loop`
        :param options: a dictionary with options for transformations.
        :type options: dict of string:values or None

        :raises TransformationError: if the supplied node does not \
                                     allow a loop swap to be done.
        :raises TransformationError: if either the inner or outer loop \
                                     has a symbol table.

        '''
        super().validate(node, options=options)
        node_outer = node

        if not node_outer.loop_body or not node_outer.loop_body.children:
            raise TransformationError(
                f"Error in LoopSwap transformation. Supplied node "
                f"'{node_outer}' must be the outer loop of a loop nest and "
                f"must have one inner loop, but this node does not have any "
                f"statements inside.")

        node_inner = node_outer.loop_body[0]

        # Check that the body of the outer loop is itself a Loop
        try:
            super().validate(node_inner, options=options)
        except TransformationError as err:
            raise TransformationError(
                f"Error in LoopSwap transformation. Supplied node "
                f"'{node_outer}' must be the outer loop of a loop nest but "
                f"the first inner statement is not a valid loop:\n"
                f"{err.value}.") from err

        if len(node_outer.loop_body.children) > 1:
            raise TransformationError(
                f"Error in LoopSwap transformation. Supplied node "
                f"'{node_outer}' must be the outer loop of a loop nest and "
                f"must have exactly one inner loop, but this node has "
                f"{len(node_outer.loop_body.children)} inner statements, the "
                f"first two being '{node_outer.loop_body[0]}' and "
                f"'{node_outer.loop_body[1]}'.")

        outer_sched = node_outer.loop_body
        if outer_sched.symbol_table and \
                not outer_sched.symbol_table.is_empty():
            raise TransformationError(
                "Error in LoopSwap transformation: The outer loop "
                "has a non-empty symbol table.")

        inner_sched = outer_sched[0].loop_body
        if inner_sched.symbol_table and \
                not inner_sched.symbol_table.is_empty():
            raise TransformationError(
                "Error in LoopSwap transformation: The inner loop "
                "has a non-empty symbol table.")

        for boundary in (node_outer.start_expr, node_outer.stop_expr,
                         node_outer.step_expr):
            symbols = [ref.symbol for ref in boundary.walk(Reference)]
            if node_inner.variable in symbols:
                raise TransformationError(
                    f"Error in LoopSwap transformation: The inner loop "
                    f"iteration variable '{node_inner.variable.name}' is part "
                    f"of the outer loop boundary expressions, so their order "
                    f"can not be swapped.")

        for boundary in (node_inner.start_expr, node_inner.stop_expr,
                         node_inner.step_expr):
            symbols = [ref.symbol for ref in boundary.walk(Reference)]
            if node_outer.variable in symbols:
                raise TransformationError(
                    f"Error in LoopSwap transformation: The outer loop "
                    f"iteration variable '{node_outer.variable.name}' is part "
                    f"of the inner loop boundary expressions, so their order "
                    f"can not be swapped.")

    def apply(self, node, options=None):
        # pylint: disable=arguments-differ
        '''The argument :py:obj:`outer` must be a loop which has exactly
        one inner loop. This transform then swaps the outer and inner loop.

        :param outer: the node representing the outer loop.
        :type outer: :py:class:`psyclone.psyir.nodes.Loop`
        :param options: a dictionary with options for transformations.
        :type options: dictionary of string:values or None

        :raises TransformationError: if the supplied node does not \
                                     allow a loop swap to be done.

        '''
        self.validate(node, options=options)
        outer = node
        inner = outer.loop_body[0]
        # Detach the inner code
        inner_loop_body = inner.loop_body.detach()

        # Swap the loops
        outer.replace_with(inner.detach())
        inner.addchild(outer.loop_body.detach())
        inner.loop_body.addchild(outer)
        # Insert again the inner code in the new inner loop
        outer.addchild(inner_loop_body)


# For Sphinx AutoAPI documentation generation
__all__ = ["LoopSwapTrans"]
