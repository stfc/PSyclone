# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017-2025, Science and Technology Facilities Council.
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
# Authors: R. W. Ford, A. R. Porter, S. Siso and N. Nobre, STFC Daresbury Lab
#          J. Henrichs, Bureau of Meteorology
# Modified I. Kavcic, Met Office
# Modified A. B. G. Chalk, STFC Daresbury Lab

'''This module provides the generic loop fusion class, which is the base
class for all API-specific loop fusion transformations.
'''

from psyclone.core import SymbolicMaths
from psyclone.domain.common.psylayer import PSyLoop
from psyclone.psyir.nodes import Reference, Routine
from psyclone.psyir.tools import DependencyTools
from psyclone.psyir.transformations.loop_trans import LoopTrans
from psyclone.psyir.transformations.transformation_error import \
    TransformationError, LazyString


class LoopFuseTrans(LoopTrans):
    ''' Provides a generic loop-fuse transformation to two Nodes in the
    PSyIR of a Schedule after performing validity checks for the supplied
    Nodes. Examples are given in the descriptions of any children classes.

    If loops have different named loop variables, when possible the loop
    variable of the second loop will be renamed to be the same as the first
    loop. This has the side effect that the second loop's variable will no
    longer have its value modified, with the expectation that that value
    isn't used anymore.

    Note that the validation of this transformation still has several
    shortcomings, especially for domain API loops. Use at your own risk.
    '''
    def __str__(self):
        return "Fuse two adjacent loops together"

    # pylint: disable=arguments-renamed
    def validate(self, node1, node2, options=None):
        ''' Performs various checks to ensure that it is valid to apply
        the LoopFuseTrans transformation to the supplied Nodes.

        :param node1: the first Node that is being checked.
        :type node1: :py:class:`psyclone.psyir.nodes.Node`
        :param node2: the second Node that is being checked.
        :type node2: :py:class:`psyclone.psyir.nodes.Node`
        :param options: a dictionary with options for transformations.
        :type options: Optional[Dict[str, Any]]
        :param bool options["force"]: whether to force fusion of the
                                      target loop (i.e. ignore any dependence
                                      analysis). This only skips a limited
                                      number of the checks, and does not
                                      fully force merging.

        :raises TransformationError: if one or both of the Nodes is/are not
                                     a :py:class:`psyclone.psyir.nodes.Loop`.
        :raises TransformationError: if one or both Nodes are not fully-formed.
        :raises TransformationError: if the Nodes do not have the same parent.
        :raises TransformationError: if the Nodes are not next to each
                                     other in the tree.
        :raises TransformationError: if the two Loops do not have the same
                                     iteration space.
        :raises TransformationError: if the two Loops don't share the same
                                     iteration variable, but make use of the
                                     other loop's variable in their body.
        :raises TransformationError: if there are dependencies between the
                                     loops that prevent the loop fusion.
        '''
        # pylint: disable=too-many-locals, too-many-branches
        if not options:
            options = {}
        # Check that the supplied Nodes are Loops
        super().validate(node1, options=options)
        super().validate(node2, options=options)

        ignore_dep_analysis = options.get("force", False)

        # Check loop1 and loop2 have the same parent
        if not node1.sameParent(node2):
            raise TransformationError(
                f"Error in {self.name} transformation. Loops do not have "
                f"the same parent.")

        # Check node1 and node2 are next to each other
        if abs(node1.position-node2.position) != 1:
            raise TransformationError(
                f"Error in {self.name} transformation. Nodes are not siblings "
                f"who are next to each other.")

        # Check node1 comes before node2:
        if node2.position-node1.position != 1:
            raise TransformationError(
                f"Error in {self.name} transformation. The second loop does "
                f"not immediately follow the first loop.")

        # Check that the iteration space is the same
        if isinstance(node1, PSyLoop) and isinstance(node2, PSyLoop):
            # TODO 1731: For some PSyLoops the iteration space is encoded just
            # in the attributes and not reflected in the loop bounds.
            if node1.iteration_space != node2.iteration_space:
                raise TransformationError(
                    f"Error in {self.name} transformation. Loops do not have "
                    f"the same iteration space.")
        else:
            if not (SymbolicMaths.equal(node1.start_expr, node2.start_expr) and
                    SymbolicMaths.equal(node1.stop_expr, node2.stop_expr) and
                    SymbolicMaths.equal(node1.step_expr, node2.step_expr)):
                # TODO #257: This transformation assumes that all domain loop
                # bodies have only POINTWISE accesses to fields and does not
                # perform any dependency analysis.
                # This is wrong and it will generate incorrect code for any
                # kernel with STENCIL access.
                raise TransformationError(LazyString(
                    lambda node1=node1, node2=node2:
                        f"Error in {self.name} transformation. Loops do not "
                        f"have the same iteration space:\n"
                        f"{node1.debug_string()}\n"
                        f"{node2.debug_string()}"))

        if not ignore_dep_analysis:
            dep_tools = DependencyTools()
            can_be_fused = dep_tools.can_loops_be_fused(node1, node2)

            if not can_be_fused:
                messages = dep_tools.get_all_messages()
                raise TransformationError(f"{self.name}. {messages[0]}")

    # -------------------------------------------------------------------------
    def apply(self, node1, node2, options=None):
        # pylint: disable=arguments-differ
        ''' Fuses two loops represented by `psyclone.psyir.nodes.Node` objects
        after performing validity checks.

        If the two loops don't have the same loop variable, the second loop's
        variable (and references to it inside the loop) will be changed to be
        references to the first loop's variable before merging. This has the
        side effect that the second loop's variable will no longer have its
        value modified, with the expectation that that value isn't used after.

        :param node1: the first Node that is being checked.
        :type node1: :py:class:`psyclone.psyir.nodes.Node`
        :param node2: the second Node that is being checked.
        :type node2: :py:class:`psyclone.psyir.nodes.Node`
        :param options: a dictionary with options for transformations.
        :type options: Optional[Dict[str, Any]]

        '''
        # Validity checks for the supplied nodes
        self.validate(node1, node2, options=options)

        # Remove node2 from the parent
        node2.detach()

        loop_var1 = node1.variable
        loop_var2 = node2.variable

        # If the loop variable isn't the same, we change all uses of the
        # second loop's variable to reference the first loop's variable.
        # This has the side effect that loop_var2 will no longer have the
        # same value after the loop, with the expectation that it isn't used.
        if loop_var1 != loop_var2:
            references = node2.loop_body.walk(Reference)
            for ref in references:
                if ref.symbol == loop_var2:
                    ref.symbol = loop_var1

        # Add loop contents of node2 to node1
        node1.loop_body.children.extend(node2.loop_body.pop_all_children())

        # We need to remove all leftover references because lfric is compiled
        # with '-Werror=unused-variable'. Since we have fused loops, we only
        # need to look at the symbols appearing in the loop control of the
        # second loop, as these are the ones that have been detached.
        routine = node1.ancestor(Routine)
        if routine:
            remaining_names = {sig.var_name for sig in
                               routine.reference_accesses().all_signatures}
            del_names = {sig.var_name for sig in
                         node2.start_expr.reference_accesses().all_signatures +
                         node2.stop_expr.reference_accesses().all_signatures +
                         node2.step_expr.reference_accesses().all_signatures}
            for name in del_names:
                if name not in remaining_names:
                    try:
                        rsym = node1.scope.symbol_table.lookup(name)
                        if rsym.is_automatic:
                            symtab = rsym.find_symbol_table(node1)
                            # TODO #898: Implement symbol removal
                            # pylint: disable=protected-access
                            symtab._symbols.pop(rsym.name)
                    except KeyError:
                        pass


# For automatic documentation generation
__all__ = ["LoopFuseTrans"]
