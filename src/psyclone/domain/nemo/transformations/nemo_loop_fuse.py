# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2021-2024, Science and Technology Facilities Council.
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
# Authors: J. Henrichs, Bureau of Meteorology
#          N. Nobre, STFC Daresbury Lab
#          S. Siso, STFC Daresbury Lab

'''This module contains the NEMO-specific loop fusion transformation.
'''

from psyclone.core import AccessType, SymbolicMaths, VariablesAccessInfo
from psyclone.psyir.tools import DependencyTools
from psyclone.psyir.transformations import LoopFuseTrans, TransformationError


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
        :type options: Optional[Dict[str, Any]]

        :raises TransformationError: if the lower or upper loop boundaries \
            are not the same.
        :raises TransformationError: if the loop step size is not the same.
        :raises TransformationError: if the loop variables are not the same.

        '''
        # pylint: disable=too-many-locals
        # First check constraints on the nodes inherited from the parent
        # LoopFuseTrans:
        super().validate(node1, node2, options)

        sym_maths = SymbolicMaths.get()

        if not sym_maths.equal(node1.start_expr, node2.start_expr):
            raise TransformationError(f"Lower loop bounds must be identical, "
                                      f"but are '{node1.start_expr}' and "
                                      f"'{node2.start_expr}'")
        if not sym_maths.equal(node1.stop_expr, node2.stop_expr):
            raise TransformationError(f"Upper loop bounds must be identical, "
                                      f"but are '{node1.stop_expr}' and "
                                      f"'{node2.stop_expr}'")
        if not sym_maths.equal(node1.step_expr, node2.step_expr):
            raise TransformationError(f"Step size in loops must be identical, "
                                      f"but are '{node1.step_expr}' and "
                                      f"'{node2.step_expr}'")
        loop_var1 = node1.variable
        loop_var2 = node2.variable

        if loop_var1 != loop_var2:
            raise TransformationError(f"Loop variables must be the same, "
                                      f"but are '{loop_var1.name}' and "
                                      f"'{loop_var2.name}'")
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

            symbol = symbol_table.lookup(signature.var_name)
            # TODO #1270 - the is_array_access function might be moved
            is_array = symbol.is_array_access(access_info=var_info1)

            if not is_array:
                LoopFuseTrans.validate_written_scalar(var_info1, var_info2)
            else:
                LoopFuseTrans.validate_written_array(var_info1, var_info2,
                                                         loop_var1)
