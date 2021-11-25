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
# Authors R. W. Ford, A. R. Porter, and S. Siso STFC Daresbury Lab
#         A. B. G. Chalk STFC Daresbury Lab
#         J. Henrichs, Bureau of Meteorology
# Modified I. Kavcic, Met Office

''' This module provides the loop swap transformation.'''

from psyclone.psyir.nodes import Schedule
from psyclone.psyir.transformations import LoopTrans, TransformationError


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
     >>> # schedule.view()
     >>>
     >>> from psyclone.transformations import LoopSwapTrans
     >>> swap = LoopSwapTrans()
     >>> swap.apply(schedule.children[0])
     >>> # Uncomment the following line to see a text view of the schedule
     >>> # schedule.view()

    '''
    def __str__(self):
        return "Exchange the order of two nested loops: inner becomes " + \
               "outer and vice versa"

    def validate(self, node_outer, options=None):
        # pylint: disable=arguments-differ
        '''Checks if the given node contains a valid Fortran structure
        to allow swapping loops. This means the node must represent
        a loop, and it must have exactly one child that is also a loop.

        :param node_outer: a Loop node from an AST.
        :type node_outer: py:class:`psyclone.psyir.nodes.Loop`
        :param options: a dictionary with options for transformations.
        :type options: dict of string:values or None

        :raises TransformationError: if the supplied node does not\
                                     allow a loop swap to be done.

        '''
        super().validate(node_outer, options=options)

        if not node_outer.loop_body or not node_outer.loop_body.children:
            raise TransformationError("Error in LoopSwap transformation. "
                                      "Supplied node '{0}' must be the outer "
                                      "loop of a loop nest and must have one "
                                      "inner loop, but this node does not "
                                      "have any statements inside."
                                      .format(node_outer))

        node_inner = node_outer.loop_body[0]

        # Check that the body of the outer loop is itself a Loop
        try:
            super().validate(node_inner, options=options)
        except TransformationError as err:
            raise TransformationError("Error in LoopSwap transformation. "
                                      "Supplied node '{0}' must be the outer "
                                      "loop of a loop nest but the first "
                                      "inner statement is not a valid loop:\n"
                                      "{1}.".format(node_outer,
                                                    str(err.value))) from err

        if len(node_outer.loop_body.children) > 1:
            raise TransformationError(
                "Error in LoopSwap transformation. Supplied node '{0}' must"
                " be the outer loop of a loop nest and must have exactly one "
                "inner loop, but this node has {1} inner statements, the "
                "first two being '{2}' and '{3}'"
                "".format(node_outer, len(node_outer.loop_body.children),
                          node_outer.loop_body[0], node_outer.loop_body[1]))

    def apply(self, outer, options=None):
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
        self.validate(outer, options=options)

        inner = outer.loop_body[0]

        # Detach the inner code
        inner_loop_body = inner.loop_body.detach()

        # Swap the loops
        outer.replace_with(inner.detach())
        inner.addchild(Schedule())
        inner.loop_body.addchild(outer)

        # Insert again the inner code in the new inner loop
        outer.loop_body.replace_with(inner_loop_body)


# For Sphinx AutoAPI documentation generation
__all__ = ["LoopSwapTrans"]
