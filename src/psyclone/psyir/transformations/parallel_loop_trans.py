# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017-2024, Science and Technology Facilities Council.
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
# Authors R. W. Ford, A. R. Porter, S. Siso and N. Nobre, STFC Daresbury Lab
#         A. B. G. Chalk STFC Daresbury Lab
#         J. Henrichs, Bureau of Meteorology
# Modified I. Kavcic, Met Office

''' This module provides the ParallelLoopTrans transformation.'''

import abc

from psyclone import psyGen
from psyclone.domain.common.psylayer import PSyLoop
from psyclone.psyir import nodes
from psyclone.psyir.nodes import Loop
from psyclone.psyir.tools import DependencyTools, DTCode
from psyclone.psyir.transformations.loop_trans import LoopTrans
from psyclone.psyir.transformations.transformation_error import \
    TransformationError


class ParallelLoopTrans(LoopTrans, metaclass=abc.ABCMeta):
    '''
    Adds an abstract directive (it needs to be specified by sub-classing this
    transformation) to a loop indicating that it should be parallelised. It
    performs some data dependency checks to guarantee that the loop can be
    parallelised without changing the semantics of it.

    '''
    # The types of node that must be excluded from the section of PSyIR
    # being transformed.
    excluded_node_types = (nodes.Return, psyGen.HaloExchange, nodes.CodeBlock)

    @abc.abstractmethod
    def _directive(self, children, collapse=None):
        '''
        Returns the directive object to insert into the Schedule.
        Must be implemented by sub-class.

        :param children: list of nodes that will be children of this Directive.
        :type children: list of :py:class:`psyclone.psyir.nodes.Node`
        :param int collapse: the number of tightly-nested loops to which \
                             this directive applies or None.

        :returns: the new Directive node.
        :rtype: sub-class of :py:class:`psyclone.psyir.nodes.Directive`.
        '''

    def validate(self, node, options=None):
        '''
        Perform validation checks before applying the transformation

        :param node: the node we are checking.
        :type node: :py:class:`psyclone.psyir.nodes.Node`
        :param options: a dictionary with options for transformations.\
                        This transform supports "collapse", which is the\
                        number of nested loops to collapse.
        :type options: Optional[Dict[str, Any]]
        :param int options["collapse"]: number of nested loops to collapse
                                        or None.
        :param bool options["force"]: whether to force parallelisation of the
                target loop (i.e. ignore any dependence analysis).
        :param bool options["sequential"]: whether this is a sequential loop.

        :raises TransformationError: if the \
                :py:class:`psyclone.psyir.nodes.Loop` loop iterates over \
                colours.
        :raises TransformationError: if 'collapse' is supplied with an \
                invalid number of loops.
        :raises TransformationError: if there is a data dependency that \
                prevents the parallelisation of the loop unless \
                `options["force"]` is True.

        '''
        # Check that the supplied node is a Loop and does not contain any
        # unsupported nodes.
        super().validate(node, options=options)

        if not options:
            options = {}
        collapse = options.get("collapse", None)
        ignore_dep_analysis = options.get("force", False)
        sequential = options.get("sequential", False)

        # Check we are not a sequential loop
        if (not sequential and isinstance(node, PSyLoop) and
                node.loop_type == 'colours'):
            raise TransformationError(f"Error in {self.name} transformation. "
                                      f"The target loop is over colours and "
                                      f"must be computed serially.")

        # If 'collapse' is specified, check that it is an int and that the
        # loop nest has at least that number of loops in it
        if collapse:
            if not isinstance(collapse, int):
                raise TransformationError(
                    f"The 'collapse' argument must be an integer but got an "
                    f"object of type {type(collapse)}")
            if collapse < 2:
                raise TransformationError(
                    f"It only makes sense to collapse 2 or more loops "
                    f"but got a value of {collapse}")
            # Count the number of loops in the loop nest
            loop_count = 0
            cnode = node
            while isinstance(cnode, Loop):
                loop_count += 1
                # Loops must be tightly nested (no intervening statements)
                cnode = cnode.loop_body[0]
            if collapse > loop_count:
                raise TransformationError(
                    f"Cannot apply COLLAPSE({collapse}) clause to a loop nest "
                    f"containing only {loop_count} loops")

        # Check that there are no loop-carried dependencies
        if sequential or ignore_dep_analysis:
            return

        dep_tools = DependencyTools()

        if not node.independent_iterations(dep_tools=dep_tools,
                                           test_all_variables=True):
            # The DependencyTools also returns False for things that are
            # not an issue, so we ignore specific messages.
            for message in dep_tools.get_all_messages():
                if message.code == DTCode.WARN_SCALAR_WRITTEN_ONCE:
                    continue
                all_msg_str = [str(message) for message in
                               dep_tools.get_all_messages()]
                messages = "\n".join(all_msg_str)
                raise TransformationError(
                    f"Dependency analysis failed with the following "
                    f"messages:\n{messages}")

    def apply(self, node, options=None):
        '''
        Apply the Loop transformation to the specified node in a
        Schedule. This node must be a Loop since this transformation
        corresponds to wrapping the generated code with directives,
        e.g. for OpenMP:

        .. code-block:: fortran

          !$OMP DO
          do ...
             ...
          end do
          !$OMP END DO

        At code-generation time (when gen_code()` is called), this node must be
        within (i.e. a child of) a PARALLEL region.

        :param node: the supplied node to which we will apply the \
                     Loop transformation.
        :type node: :py:class:`psyclone.psyir.nodes.Node`
        :param options: a dictionary with options for transformations. \
        :type options: Optional[Dict[str, Any]]
        :param int options["collapse"]: the number of loops to collapse into \
                single iteration space or None.

        '''
        if not options:
            options = {}
        self.validate(node, options=options)

        collapse = options.get("collapse", None)

        # keep a reference to the node's original parent and its index as these
        # are required and will change when we change the node's location
        node_parent = node.parent
        node_position = node.position

        # Add our orphan loop directive setting its parent to the node's
        # parent and its children to the node. This calls down to the sub-class
        # to get the type of directive we require.
        directive = self._directive([node.detach()], collapse)

        # Add the loop directive as a child of the node's parent
        node_parent.addchild(directive, index=node_position)
