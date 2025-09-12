# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2025, University of Cambridge, UK.
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
# Author: M. Naylor, University of Cambridge, UK
# -----------------------------------------------------------------------------

'''This module provides a class to assist with inferring reduction clauses
   for parallel loop/region directives.'''

from typing import Union, List, Tuple

from psyclone.core import (AccessSequence, Signature)
from psyclone.psyir.nodes import (
        Node, Reference, BinaryOperation, IntrinsicCall, Assignment
)


class ReductionInferenceTool():
    '''
    Instances of this class are initialsied with a set of allowed
    reduction operators. When inferring reduction clauses, only
    reductions involving these operators are considered.

    :param red_ops: a list of allowed reduction operators.
    '''
    def __init__(self, red_ops: List[Union[BinaryOperation.Operator,
                                           IntrinsicCall.Intrinsic]] = []):
        self.red_ops = red_ops

    def _get_reduction_operator(self, node: Node) -> \
            Union[BinaryOperation.Operator, IntrinsicCall.Intrinsic]:
        '''
        :param node: the node to match against.
        :returns: the reduction operator at the root of the given
        DataNode or None if there isn't one.
        '''
        if isinstance(node, BinaryOperation):
            for op in self.red_ops:
                if node.operator == op:
                    return node.operator
        if isinstance(node, IntrinsicCall):
            for op in self.red_ops:
                if node.intrinsic == op:
                    return node.intrinsic
        return None

    @staticmethod
    def _match_sig(ref: Reference, sig: Signature) -> bool:
        '''
        :param ref: the Reference node to match against.
        :param sig: the candidate reduction variable.
        :returns: True if the Signature of the given Reference matches
        the given Signature, and the Reference involves no array
        indices.  Returns False otherwise.
        '''
        (ref_sig, ref_indices) = ref.get_signature_and_indices()

        # OpenMP supports array slices as reduction variables, but we
        # have not yet considered how to infer these.
        no_indices = sum(ref_indices, []) == []

        # OpenMP doesn't currently allow variables with member accessors
        # as reduction variables. So we require the signature to be a
        # singleton. If we want to infer such variables, we can easily remove
        # this check (the OpenMP restriction could be bypassed, for example,
        # by introducing a temporary variable).
        no_members = len(ref_sig) == 1

        return ref_sig == sig and no_indices and no_members

    def _get_write_reduction(self, node: Node, sig: Signature) -> \
            Union[BinaryOperation.Operator, IntrinsicCall.Intrinsic]:
        '''
        Return the reduction operator for given node if it is the
        LHS of an Assignment of the form _either_

            <Reference> = <Reference> <op> <DataNode>

        or

            <Reference> = <DataNode> <op> <Reference>

        where <op> is an allowed reduction operator and the Signature
        of <Reference> is a scalar reference matching the given Signature.
        Otherwise, return None.

        :param node: the node to match against.
        :param sig: the candidate reduction variable.
        :returns: the reduction operator, or None.
        '''
        if isinstance(node, Reference):
            if self._match_sig(node, sig):
                # We have an access to the candidate reduction variable.
                # Now check that this access is the LHS of an Assignment.
                if (isinstance(node.parent, Assignment) and
                        node.parent.lhs is node):
                    # Check that the RHS of the assignment has a reduction
                    # operator at its root.
                    op = self._get_reduction_operator(node.parent.rhs)
                    if op:
                        # Require that exactly one child of the reduction
                        # operator is the candidate reduction variable
                        # (x = x + x is not a valid reduction involving
                        # the + operator).
                        child_ok = []
                        for child in node.parent.rhs.children[:2]:
                            child_ok.append(isinstance(child, Reference) and
                                            self._match_sig(child, sig))
                        if (child_ok == [False, True] or
                                child_ok == [True, False]):
                            return op
        return None

    def _get_read_reduction(self, node: Node, sig: Signature) -> \
            Union[BinaryOperation.Operator, IntrinsicCall.Intrinsic]:
        '''
        Return reduction operator for given node if it is the child
        of a DataNode which is the RHS of an Assignment of the form

            <Reference> = <Reference> <op> <DataNode>

        or

            <Reference> = <DataNode> <op> <Reference>

        where <op> is an allowed reduction operator and the Signature
        of <Reference> is a scalar reference matching the given Signature.
        Otherwise, return None.

        :param node: the node to match against
        :param sig: the candidate reduction variable.
        :returns: the reduction operator, or None.
        '''
        if isinstance(node, Reference):
            if self._match_sig(node, sig):
                # We have an access to the candidate reduction variable.
                # Now check that the parent of this access is an application
                # of a reduction operator.
                op = self._get_reduction_operator(node.parent)
                if op:
                    # Also check that the parent of the reduction operator
                    # is an assigment of the candidate reduction variable.
                    if isinstance(node.parent.parent, Assignment):
                        lhs = node.parent.parent.lhs
                        if isinstance(lhs, Reference):
                            if self._match_sig(lhs, sig):
                                return op
                return None

    def attempt_reduction(self, sig: Signature,
                          access_seq: AccessSequence) -> \
            Tuple[Union[BinaryOperation.Operator,
                        IntrinsicCall.Intrinsic],
                  Reference]:
        '''
        Determine if the variable with the given Signature and AccessSequence
        can be handled using a reduction clause and, if so, return that
        clause. Otherwise, return None.

        :param sig: the variable being considered as a reduction variable.
        :param access_seq: the access sequence for that variable.
        :returns: the operator/reference pair that can be used for the
           reduction if reduction is possible, or None otherwise.
        '''
        # Iterate over all read and write accesses to the candidate reduction
        # variable. If all read accesses conform to the get_read_reduction()
        # form, and all write accesses to the get_write_reduction() form,
        # and all reductions involve the same reduction operator, then
        # the variable and operator are returned as a reduction clause.
        # Return early if we ever encounter a use of the variable which is
        # not in the form of a reduction.
        ops = []
        for access in access_seq.all_read_accesses:
            op = self._get_read_reduction(access.node, sig)
            if op is None:
                return None
            ops.append(op)
            ref = access.node
        # If we reach here, then all read accesses are in the form of
        # a reduction. Now check write accesses.
        for access in access_seq.all_write_accesses:
            op = self._get_write_reduction(access.node, sig)
            if op is None:
                return None
            ops.append(op)
            ref = access.node
        # If we reach here, then all read accesses and all write accesses
        # are in the form of a reduction. But there may be no accesses,
        # in which case we return None.
        if ops == []:
            return None
        # Require that all reductions found involve the same operator.
        if any(op != ops[0] for op in ops):
            return None
        # Return the reduction operator and a (detached) copy of one of
        # the references to the reduction variable.
        return (op, ref.copy())
