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
# Author: A. R. Porter, STFC Daresbury Lab

''' This module contains the AdjointLoopTrans. '''

import six

from psyclone.psyGen import Kern
from psyclone.psyir.transformations.loop_trans import LoopTrans
from psyclone.psyir.transformations.transformation_error \
    import TransformationError
from psyclone.psyir.nodes import Schedule, Loop, UnaryOperation


class AdjointLoopTrans(LoopTrans):
    '''
    This class implements XXXXXX.

    '''
    # The types of Node that are excluded from within the target loop. Must be
    # populated by sub-class.
    excluded_node_types = ()

    def validate(self, node, options=None):
        '''Checks that the supplied node is a valid target for an adjoint
        transformation.

        :param node: target PSyIR node.
        :type node: subclass of :py:class:`psyclone.psyir.nodes.Node`
        :param options: a dictionary with options for transformations.
        :type options: dictionary of string:values or None
        :param bool options["node-type-check"]: this flag controls if the \
            type of the nodes enclosed in the loop should be tested to \
            avoid including unsupported nodes in a transformation.

        :raises TransformationError: XXXXXX

        '''
        super(AdjointLoopTrans, self).validate(node, options=options)

    def apply(self, node, options=None):
        ''' '''
        self.validate(node, options=options)
        start = node.start_expr.copy()
        stop = node.stop_expr.copy()
        step = node.step_expr.copy()
        # Swap upper and lower loop limits
        node.start_expr.replace_with(stop)
        node.stop_expr.replace_with(start)
        # Take the negative of the step
        node.step_expr = UnaryOperation.create(UnaryOperation.Operator.MINUS,
                                               step)


# For Sphinx AutoAPI documentation generation
__all__ = ["AdjointLoopTrans"]
