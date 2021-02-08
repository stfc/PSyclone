# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2019-2020, Science and Technology Facilities Council.
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
# Authors I. Kavcic, Met Office
# Modified by J. Henrichs, Bureau of Meteorology

'''This module contains the GOcean-specific extract transformation.
'''

from psyclone.core.access_info import VariablesAccessInfo
from psyclone.psyir.backend.fortran import FortranWriter
from psyclone.psyir.transformations import TransformationError
from psyclone.transformations import LoopFuseTrans


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
        :param options: a dictionary with options for transformations.
        :type options: dictionary of string:values or None

        :raises TransformationError: if transformation is applied to an \
            inner Loop without its parent outer Loop.
        '''

        # First check constraints on Nodes in the node_list inherited from
        # the parent classes (ExtractTrans and RegionTrans)
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
        loop_var1 = node1.variable.name
        loop_var2 = node2.variable.name

        if loop_var1 != loop_var2:
            raise TransformationError("Loop variables must be the same, "
                                      "but are '{0}' and '{1}'".
                                      format(loop_var1, loop_var2))
        vars1 = VariablesAccessInfo(node1)
        vars2 = VariablesAccessInfo(node2)

        # Get all variables that occur in both loops. A variable
        # that is only in one loop is not affected by fusion.
        all_vars = set(vars1).intersection(vars2)
        symbol_table = node1.scope.symbol_table

        for var_name in all_vars:
            # Ignore the loop variable
            if var_name == loop_var1:
                continue
            var_info1 = vars1[var_name]
            var_info2 = vars2[var_name]
            # Variables that are only read in both loops can always be fused
            if var_info1.is_read_only() and var_info2.is_read_only():
                continue

            if var_name in symbol_table:
                # Find the symbol for this variable. We only need to check
                # one
                symbol = symbol_table.lookup(var_name)
                is_array = symbol.is_array
            elif var_name in var_info1:
                symbol = symbol_table.lookup(var_name)
                is_array = symbol.is_array
            else:
                # TODO #845: Once we have symbol tables, any variable should
                # be in a symbol table, so we have to raise an exception here.
                # We need to fall-back to the old-style test, since we do not
                # have information in a symbol table. So check if the access
                # information stored an index:
                is_array = var_info1[0].indices is not None

            if not is_array:
                self.validate_scalar(var_info1, var_info2)
            else:
                self.validate_array(var_info1, var_info2, loop_var1)

    # -------------------------------------------------------------------------
    def validate_scalar(self, var_info1, var_info2):
        '''Validates if the accesses to the scalar ``var_name`` can be fused.
        :param var_info1: access information for variable in the first loop.
        :type var_info1: :py:class:`psyclone.core.var_info.VariableInfo`
        :param var_info2: access information for variable in the second loop.
        :type var_info2: :py:class:`psyclone.core.var_info.VariableInfo`

        :raises:
        '''

        # TODO: write this subroutine :)

    # -------------------------------------------------------------------------
    def validate_array(self, var_info1, var_info2, loop_variable):
        '''Validates if the accesses to the array ``var_name`` can be fused.
        :param var_info1: access information for variable in the first loop.
        :type var_info1: :py:class:`psyclone.core.var_info.VariableAccessInfo`
        :param var_info2: access information for variable in the second loop.
        :type var_info2: :py:class:`psyclone.core.var_info.VariableAccessInfo`
        :param str loop_variable: name of the loop variable that will \
            be fused.

        :raises:
        '''

        # First check if all accesses to the array have the same dimension
        # based on the loop variable
        # Now detect which dimension(s) is/are parallelised, i.e.
        # which dimension depends on the loop_variable.  For example
        # if a "do j..." loop is parallelised, consider expressions like
        # a(i,j) and a(j+2, i-1) in one loop:
        # In this case the dimensions 1 (a(i,j)) and 0 (a(j+2,i-1)) would
        # be accessed. Since the variable is written somewhere (read-only
        # was tested above), the variable can not be used in parallel.
        # Additionally, collect all indices that are actually used, since
        # they are needed in a test further down.
        all_accesses = var_info1.all_accesses + var_info2.all_accesses

        found_dimension_index = -1
        all_indices = []

        # Loop over all the accesses of this variable
        for access in all_accesses:
            list_of_indices = access.indices
            # Now determine all dimensions that depend
            # on the parallel variable:
            for dimension_index, index_expression in \
                    enumerate(list_of_indices):
                accesses = VariablesAccessInfo()

                index_expression.reference_accesses(accesses)
                if loop_variable not in accesses:
                    continue

                # if a previously identified index location does not match
                # the current index location (e.g. a(i,j), and a(j,i) ), then
                # the loop can not be parallelised
                if found_dimension_index > -1 and \
                        found_dimension_index != dimension_index:
                    raise TransformationError(
                        "Variable '{0}' is using loop variable {1} in index "
                        "{2} and {3}.".format(var_info1.var_name,
                                              loop_variable,
                                              found_dimension_index,
                                              dimension_index))
                found_dimension_index = dimension_index
                all_indices.append(index_expression)

        if not all_indices:
            # An array is used that is not actually dependent on the parallel
            # loop variable. This means the variable can not always be safely
            # parallelised. Example 1:
            # do j=1, n
            #    a(1) = b(j)+1
            #    c(j) = a(1) * 2
            # enddo
            # In this case a(1) should be a thread-private scalar.
            # Example2:
            # do j=1, n
            #    if(some_cond)
            #       a(1) = b(j)
            #    endif
            #  enddo
            # In this case it is not clear if the loop can be parallelised.
            # So in any case we add the information for the user to decide.
            #self._add_warning("Variable '{0}' is written to, and "
            #                  "does not depend on the loop "
            #                  "variable '{1}'."
            #                  .format(var_info1.var_name,
            #                          loop_variable))
            return

        # Now we have confirmed that all parallel accesses to the variable
        # are using the same dimension. If there is a case of stencil
        # access with write operations (read-only has already been tested)
        # the loop can not be parallelised. E.g. in one j loop:
        # b(j) = a(j-1) + a(j+1)
        # a(j) = c(j)

        first_index = all_indices[0]
        for index in all_indices[1:]:
            if not first_index.math_equal(index):
                visitor = FortranWriter()
                raise TransformationError(
                    "Variable {0} is written and is accessed using indices "
                    "{1} and {2} and can therefore not be parallelised."
                    .format(var_info1.var_name, visitor(first_index),
                            visitor(index)))
