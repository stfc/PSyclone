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
# Authors: R. W. Ford, A. R. Porter, S. Siso and N. Nobre, STFC Daresbury Lab
#          J. Henrichs, Bureau of Meteorology
# Modified I. Kavcic, Met Office
# Modified A. B. G. Chalk, STFC Daresbury Lab

'''This module provides the generic loop fusion class, which is the base
class for all API-specific loop fusion transformations.
'''

from psyclone.core import AccessType, SymbolicMaths, VariablesAccessInfo, \
    Signature
from psyclone.domain.common.psylayer import PSyLoop
from psyclone.psyir.nodes import Reference
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

    def validate(self, node1, node2, options=None):
        # pylint: disable=arguments-differ
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

            vars1 = VariablesAccessInfo(node1)
            vars2 = VariablesAccessInfo(node2)

            # Check if the loops have the same loop variable
            loop_var1 = node1.variable
            loop_var2 = node2.variable
            if loop_var1 != loop_var2:
                # If they don't have the same variable, find out if the one
                # loop accesses the other loops variable symbol.
                # If so, then for now we disallow this merge (though we could
                # in theory allow using the unused one unless both use each
                # others)
                if Signature(loop_var2.name) in vars1:
                    raise TransformationError(
                        f"Error in {self.name} transformation. First "
                        f"loop contains accesses to the second loop's "
                        f"variable: {loop_var2.name}.")
                if Signature(loop_var1.name) in vars2:
                    raise TransformationError(
                        f"Error in {self.name} transformation. Second "
                        f"loop contains accesses to the first loop's "
                        f"variable: {loop_var1.name}.")

            # Get all variables that occur in both loops. A variable
            # that is only in one loop is not affected by fusion.
            all_vars = set(vars1).intersection(vars2)
            symbol_table = node1.scope.symbol_table

            for signature in all_vars:
                var_name = str(signature)
                # Ignore the loop variable
                if var_name == loop_var1.name:
                    continue
                var_info1 = vars1[signature]
                var_info2 = vars2[signature]

                # Variables that are only read in both loops can always be
                # fused
                if var_info1.is_read_only() and var_info2.is_read_only():
                    continue

                symbol = symbol_table.lookup(signature.var_name)
                # TODO #1270 - the is_array_access function might be moved
                is_array = symbol.is_array_access(access_info=var_info1)
                if not ignore_dep_analysis:
                    if not is_array:
                        type(self)._validate_written_scalar(var_info1,
                                                            var_info2)
                    else:
                        type(self)._validate_written_array(var_info1,
                                                           var_info2,
                                                           loop_var1)

    # -------------------------------------------------------------------------
    @staticmethod
    def _validate_written_scalar(var_info1, var_info2):
        '''Validates if the accesses to a scalar that is at least written once
        allows loop fusion. The accesses of the variable is contained in the
        two parameters (which also include the name of the variable).

        :param var_info1: access information for variable in the first loop.
        :type var_info1: :py:class:`psyclone.core.var_info.VariableInfo`
        :param var_info2: access information for variable in the second loop.
        :type var_info2: :py:class:`psyclone.core.var_info.VariableInfo`

        :raises TransformationError: a scalar variable is written in one \
            loop, but only read in the other.

        '''
        # If a scalar variable is first written in both loops, that pattern
        # is typically ok. Example:
        # - inner loops (loop variable is written then read),
        # - a=sqrt(j); b(j)=sin(a)*cos(a) - a scalar variable as 'constant'
        # TODO #641: atm the variable access information has no details
        # about a conditional access, so the test below could result in
        # incorrectly allowing fusion. But the test is essential for many
        # much more typical use cases (especially inner loops).
        if var_info1[0].access_type == AccessType.WRITE and \
                var_info2[0].access_type == AccessType.WRITE:
            return

        raise TransformationError(
            f"Scalar variable '{var_info1.var_name}' is written in one loop, "
            f"but only read in the other loop.")

    # -------------------------------------------------------------------------
    @staticmethod
    def _validate_written_array(var_info1, var_info2, loop_variable):
        '''Validates if the accesses to an array, which is at least written
        once, allows loop fusion. The access pattern to this array is
        specified in the two parameters `var_info1` and `var_info2`.

        :param var_info1: access information for variable in the first loop.
        :type var_info1: \
            :py:class:`psyclone.core.var_info.SingleVariableAccessInfo`
        :param var_info2: access information for variable in the second loop.
        :type var_info2: \
            :py:class:`psyclone.core.var_info.SingleVariableAccessInfo`
        :param loop_variable: symbol of the variable associated with the \
            loops being fused.
        :type loop_variable: :py:class:`psyclone.psyir.symbols.DataSymbol`

        :raises TransformationError: an array that is written to uses \
            inconsistent indices, e.g. a(i,j) and a(j,i).

        '''
        # TODO #1075: Loop fusion should be verified using a method in
        # DependencyTools.
        # pylint: disable=too-many-locals
        dep_tools = DependencyTools()
        all_accesses = var_info1.all_accesses + var_info2.all_accesses
        loop_var_name = loop_variable.name
        # Compare all accesses with the first one. If the loop variable
        # is used in a different subscript, raise an error. We test this
        # by computing the partition of the indices:s
        comp_1 = all_accesses[0].component_indices
        # Note that we compare an access with itself, this will
        # help us detecting if an array is accessed without using
        # the loop variable (which would indicate a kind of reduction):
        for other_access in all_accesses:
            comp_other = other_access.component_indices
            # TODO #1075: when this functionality is moved into the
            # DependencyTools, the access to partition is not an
            # access to a protected member anymore.
            # pylint: disable=protected-access
            partitions = dep_tools._partition(comp_1, comp_other,
                                              [loop_var_name])
            var_found = False
            for (set_of_vars, index) in partitions:
                # Find the partition that contains the loop variable:
                if loop_var_name not in set_of_vars:
                    continue
                var_found = True
                # If the loop variable contains more than one index, it is
                # used inconsistent:
                if len(index) <= 1:
                    continue
                # Raise the appropriate error message:
                access1 = all_accesses[0].node.debug_string()
                access2 = other_access.node.debug_string()
                error = (f"Variable '{var_info1.signature[0]}' is written to "
                         f"and the loop variable '{loop_var_name}' is used "
                         f"in different index locations: {access1} and "
                         f"{access2}.")
                raise TransformationError(error)
            if not var_found:
                error = (f"Variable '{var_info1.signature[0]}' does not "
                         f"depend on loop variable '{loop_var_name}', but is "
                         f"read and written")
                raise TransformationError(error)

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


# For automatic documentation generation
__all__ = ["LoopFuseTrans"]
