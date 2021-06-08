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
# Authors J. Henrichs, Bureau of Meteorology

'''This module contains the NEMO-specific loop fusion transformation.
'''

from psyclone.core import AccessType, Signature, VariablesAccessInfo
from psyclone.psyir.transformations import TransformationError
from psyclone.psyir.transformations import LoopFuseTrans


class NemoLoopFuseTrans(LoopFuseTrans):
    '''NEMO-specific implementation of the loop fusion transformation.
    '''

    def validate(self, node1, node2, options=None):
        ''' Perform NEMO API specific validation checks before applying
        the transformation.

        :param node1: the first Node that is being checked.
        :type node1: :py:class:`psyclone.psyir.nodes.Node`
        :param node2: the second Node that is being checked.
        :type node2: :py:class:`psyclone.psyir.nodes.Node`
        :param options: a dict with options for transformations.
        :type options: dict of string:values or None

        :raises TransformationError: if the lower or upper loop boundaries \
            are not the same.
        :raises TransformationError: if the loop step size is not the same.
        :raises TransformationError: if the loop variables are not the same.

        '''
        # pylint: disable=too-many-locals
        # First check constraints on the nodes inherited from the parent
        # LoopFuseTrans:
        super(NemoLoopFuseTrans, self).validate(node1, node2, options)

        if not node1.start_expr.math_equal(node2.start_expr):
            raise TransformationError("Lower loop bounds must be identical, "
                                      "but are '{0}'' and '{1}'"
                                      .format(node1.start_expr,
                                              node2.start_expr))
        if not node1.stop_expr.math_equal(node2.stop_expr):
            raise TransformationError("Upper loop bounds must be identical, "
                                      "but are '{0}'' and '{1}'"
                                      .format(node1.stop_expr,
                                              node2.stop_expr))
        if not node1.step_expr.math_equal(node2.step_expr):
            raise TransformationError("Step size in loops must be identical, "
                                      "but are '{0}'' and '{1}'"
                                      .format(node1.step_expr,
                                              node2.step_expr))
        loop_var1 = node1.variable
        loop_var2 = node2.variable

        if loop_var1 != loop_var2:
            raise TransformationError("Loop variables must be the same, "
                                      "but are '{0}' and '{1}'".
                                      format(loop_var1.name, loop_var2.name))
        vars1 = VariablesAccessInfo(node1)
        vars2 = VariablesAccessInfo(node2)

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

            # Variables that are only read in both loops can always be fused
            if var_info1.is_read_only() and var_info2.is_read_only():
                continue

            symbol = symbol_table.lookup(var_name)
            is_array = symbol.is_array_access(access_info=var_info1)

            if not is_array:
                NemoLoopFuseTrans.validate_written_scalar(var_info1, var_info2)
            else:
                NemoLoopFuseTrans.validate_written_array(var_info1, var_info2,
                                                         loop_var1)

    # -------------------------------------------------------------------------
    @staticmethod
    def validate_written_scalar(var_info1, var_info2):
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
            "Scalar variable '{0}' is written in one loop, but only read "
            "in the other loop.".format(var_info1.var_name))

    # -------------------------------------------------------------------------
    @staticmethod
    def validate_written_array(var_info1, var_info2, loop_variable):
        '''Validates if the accesses to an array, which is at least written
        once, allows loop fusion. The access pattern to this array is
        specified in the two parameters (which includes the name of the
        variable).

        :param var_info1: access information for variable in the first loop.
        :type var_info1: \
            :py:class:`psyclone.core.var_info.SingleVariableAccessInfo`
        :param var_info2: access information for variable in the second loop.
        :type var_info2: \
            :py:class:`psyclone.core.var_info.SingleVariableAccessInfo`
        :param loop_variable: symbol of the variable associated with the \
            loops being fused.
        :type loop_variable: \
            :py:class:`psyclone.psyir.symbols.datasymbol.DataSymbol`

        :raises TransformationError: an array that is written to uses \
            inconsistent indices, e.g. a(i,j) and a(j,i).

        '''
        # First check if all accesses to the array have the same dimension
        # based on the loop variable
        # Now detect which dimension(s) is/are used in the loop. For example
        # if a "do j..." loop is one of the fused loops, consider expressions
        # like a(i,j) and a(j+2, i-1) in the two loops:
        # In this case the dimensions 1 (a(i,j)) and 0 (a(j+2,i-1)) would
        # be accessed. Since the variable is written somewhere (read-only
        # was tested above), loop fusion will likely result in invalid code.
        all_accesses = var_info1.all_accesses + var_info2.all_accesses

        # This variable will store two indices as a tuple: first the
        # component index in which the loop variable was used, and then
        # in which dimension for the component. For example, `a%b%c(i,j)`
        # would store (2,1) for an access to j - component 2 (which is c),
        # and 2nd dimension (j).
        # TODO 1269: code duplicated with dependency_tools
        found_dimension_index = None

        # Additionally, collect all indices that are actually used, since
        # they are needed in a test further down.
        all_indices = []

        # Loop over all the accesses of this variable
        for access in all_accesses:
            component_indices = access.component_indices

            # Now determine all dimensions that depend
            # on the loop variable. This outer loop is over
            # the indices used for each component, e.g.
            # a(i,j)%b(k) it would first handle `(i,j)`, then
            # `(k)`.
            for component_index, index_expressions in \
                    enumerate(component_indices):

                # This inner loop is over all indices for the
                # current component, i.e. `[i, j]` for the first
                # component above, then `[k]`.
                for dimension_index, index_expression in \
                        enumerate(index_expressions):
                    accesses = VariablesAccessInfo()

                    index_expression.reference_accesses(accesses)
                    if Signature(loop_variable.name) not in accesses:
                        continue

                    # If a previously identified index location does not match
                    # the current index location (e.g. a(i,j), and a(j,i) ),
                    # then the loop in general cannot be fused.
                    ind_pair = (component_index, dimension_index)
                    if found_dimension_index and \
                            found_dimension_index != ind_pair:
                        # TODO #1268: improve error message
                        raise TransformationError(
                            "Variable '{0}' is written to in one or both of "
                            "the loops and the loop variable {1} is used in "
                            "different index locations ({2} and {3}) when "
                            "accessing it."
                            .format(var_info1.var_name,
                                    loop_variable.name,
                                    found_dimension_index,
                                    ind_pair))

                    found_dimension_index = ind_pair
                    all_indices.append(index_expression)

        if not all_indices:
            # An array is used that is not actually dependent on the
            # loop variable. This means the variable can not always be safely
            # fused.
            # do j=1, n
            #    a(1) = b(j)+1
            # enddo
            # do j=1, n
            #    c(j) = a(1) * 2
            # enddo
            # More tests could be done here, e.g. to see if it can be shown
            # that each access in the first loop is different from the
            # accesses in the second loop: a(1) in first, a(2) in second.
            # Other special cases: reductions (a(1) = a(1) + x),
            # array expressions : a(:) = b(j) * x(:)
            # Potentially this could use the scalar handling code!
            raise TransformationError(
                "Variable '{0}' does not depend on loop variable '{1}', "
                "but is read and written.".format(var_info1.var_name,
                                                  loop_variable.name))
