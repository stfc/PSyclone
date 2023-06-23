# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017-2022, Science and Technology Facilities Council.
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
# "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT                                                                                                        # LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
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
# Authors: A. B. G. Chalk, STFC Daresbury Lab

''' Base OpenMP Loop transformation to validate directives that require
canonical loop form.'''

import abc

from psyclone.core import VariablesAccessInfo, Signature
from psyclone.psyir.nodes import (
    Reference, BinaryOperation, Literal)
from psyclone.psyir.symbols import DataSymbol, INTEGER_TYPE, ScalarType
from psyclone.psyir.transformations.parallel_loop_trans import \
    ParallelLoopTrans

class OMPCanonicalLoopTrans(ParallelLoopTrans, metaclass=abc.ABCMeta):

    def _check_valid_format(self, node, loop_var):
        '''
        TODO
        '''
        # We know node contains a Reference to loop_var to reach this call.

        if isinstance(node, Reference):
            return True

        # All other allowed cases are binary operations
        if not isinstance(node, BinaryOperation):
            return False
        # We allow var + a2, a2 + var
        if (node.operator == BinaryOperation.Operator.ADD and
                isinstance(node.children[0], Reference) and
                isinstance(node.children[1], Reference)):
            return True

        # We allow var - a2 and a2 - var
        if (node.operator == BinaryOperation.Operator.SUB and
                isinstance(node.children[0], Reference) and
                isinstance(node.children[1], Reference)):
            return True

        # We allow var * a1 and a1 * var
        if (node.operator == BinaryOperation.Operator.MUL and
                isinstance(node.children[0], Reference) and
                isinstance(node.children[1], Reference)):
            return True

        # Simple cases analysed, remaining allowed cases:
        # a1 * var + a2  - Handled
        # a2 + a1 * var - Handled
        # a1 * var - a2
        # a2 - a1 * var
        # var * a1 + a2 - Handled
        # a2 + var * a1 - Handled
        # var * a1 - a2
        # a2 - var * a1

        # Check for a1 * var + a2 - temporary version. Quick test
        # seems to show this is
        # BinaryOperation[ADD:BinaryOperation[MUL:a1, var],a2]
        # We allow the inner binary operation to be a1*var or var*a1
        # I think this also handles a2 + (a1 * var)
        if (node.operator == BinaryOperation.Operator.ADD and
                isinstance(node.children[1], Reference) and
                node.children[1].symbol != loop_var and
                isinstance(node.children[0], BinaryOperation) and
                node.children[1].operator == BinaryOperation.Operator.MUL and
                (node.children[0].children[0].symbol == loop_var or
                 node.children[0].children[1].symbol == loop_var)):
            return True

        # Check for a2 - a1*var or a2 - var*a1
        if (node.operator == BinaryOperation.Operator.SUB and
                isinstance(node.children[0], Reference) and
                node.children[0].symbol != loop_var and
                isinstance(node.children[1], BinaryOperation) and
                node.children[1].operator == BinaryOperation.Operator.MUL and
                (node.children[1].children[0].symbol == loop_var or
                 node.children[1].children[1].symbol == loop_var)):
            return True

        # Check for a1*var - a2 or var*a1 - a2
        if (node.operator == BinaryOperation.Operator.SUB and
                isinstance(node.children[1], Reference) and
                node.children[1].symbol != loop_var and
                isinstance(node.children[0], BinaryOperation) and
                node.children[0].operator == BinaryOperation.Operator.MUL and
                (node.children[0].children[0].symbol == loop_var or
                 node.children[0].children[1].symbol == loop_var)):
            return True


        # Any other structure is not allowed
        return False


    def validate(self, node, options=None):
        '''
        Perform validation checks before applying the transformation

        :param node: the node we are checking.
        :type node: :py:class:`psyclone.psyir.nodes.Node`
        :param options: a dictionary with options for transformations.\
                        This transform supports "collapse", which is the\
                        number of nested loops to collapse.
        :type options: Optional[Dict[str, Any]]
        :param int options["collapse"]: number of nested loops to collapse \
                                        or None.
        :param bool options["force"]: whether to force parallelisation of the \
                target loop (i.e. ignore any dependence analysis).

        :raises TransformationError: if the \
                :py:class:`psyclone.psyir.nodes.Loop` loop iterates over \
                colours.
        :raises TransformationError: if 'collapse' is supplied with an \
                invalid number of loops.
        :raises TransformationError: if there is a data dependency that \
                prevents the parallelisation of the loop unless \
                `options["force"]` is True.

        '''
        super().validate(node, options=options)

        # We need to check the loop is of the canonical loop form.
        # For a loop to satisfy the OpenMP canonical loop form, the following
        # must be true:
        # 1. Loop variable must be an integer type.
        # 2. The start and stop conditions must either be a variable of a type
        #    compatible to the loop variable, and either invariant with
        #    respect to the outermost associated loop, or one of a few
        #    specific formats based upon the outermost associated loop, with
        #    two values that are invariant with respect to the outermost
        #    associated loop
        # 3. The step must be invariant with respect to the outermost
        #    associated loop, and an integer type.

        # The allowed formats are:
        # var-outer
        # var-outer + a2
        # a2 + var-outer
        # var-outer - a2
        # a2 - var-outer
        # a1 * var-outer
        # a1 * var-outer + a2
        # a2 + a1 * var-outer
        # a1 * var-outer - a2
        # a2 - a1 * var-outer
        # var-outer * a1
        # var-outer * a1 + a2
        # a2 + var-outer * a1
        # var-outer * a1 - a2
        # a2 - var-outer * a1


        outer_loop_var = node.variable
        # I believe the outmost associated loop should always be node
        # and any sub nodes we don't need to check according to the
        # OpenMP manual - even in the case where we use collapse.

        # TODO This appears to be false, and depends if we allow collapse.
        # For now, it only applies to the immediate child, but if we allow
        # collapse we need to check all loops we're collapsing.

        # Check the loop variable is an integer type
        if (not isinstance(node.variable, (ScalarType, DataSymbol)) or
                node.variable.datatype.intrinsic !=
                ScalarType.Intrinsic.INTEGER):
            assert False # TODO Throw error message

        all_var_accesses = VariablesAccessInfo(node.children[3])
        # Find all of the References in the start condition.
        start_refs = node.start_expr.walk(Reference)
        # For each reference, if its not the node.variable check it is not
        # modified inside the loop.
        contains_outermost_loop_var = False
        for ref in start_refs:
            if ref.symbol is not outer_loop_var:
                # Have to check if the variable is written to in the loop.
                # If so we assume it is invariant w.r.t the outer loop.
                sig, _ = ref.get_signature_and_indices()
                all_ref_accesses = all_var_accesses.get(sig)
                if (all_ref_accesses is not None and 
                        all_ref_accesses.is_written()):
                    assert False
            else:
                contains_outermost_loop_var = True
        
        # If the loop variable exists in the start condition, we need to check
        # the format is one of the allowed formats.
        if contains_outermost_loop_var:
            if not self._check_valid_format(node.start_expr, outer_loop_var):
                assert False # TODO Throw error message

        # Same check for the stop conditions
        stop_refs = node.stop_expr.walk(Reference)
        # For each reference, if its not the node.variable check it is not
        # modified inside the loop.
        contains_outermost_loop_var = False
        for ref in stop_refs:
            if ref.symbol is not outer_loop_var:
                # Have to check if the variable is written to in the loop.
                # If so we assume it is invariant w.r.t the outer loop.
                sig, _ = ref.get_signature_and_indices()
                all_ref_accesses = all_var_accesses.get(sig)
                if (all_ref_accesses is not None and 
                        all_ref_accesses.is_written()):
                    assert False
            else:
                contains_outermost_loop_var = True
        
        # If the loop variable exists in the start condition, we need to check
        # the format is one of the allowed formats.
        if contains_outermost_loop_var:
            if not self._check_valid_format(node.stop_expr, outer_loop_var):
                assert False # TODO Throw error message

        if not isinstance(node.step_expr, (Literal, Reference)):
            assert False
        if isinstance(node.step_expr, Reference):
            sig, _ = ref.get_signature_and_indices()
            all_ref_accesses = all_var_accesses.get(sig)
            if all_ref_accesses is not None and all_ref_accesses.is_written():
                assert False
