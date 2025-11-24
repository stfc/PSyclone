# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2020-2025, Science and Technology Facilities Council
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
# Author: R. W. Ford, STFC Daresbury Lab
# Modified: A. R. Porter, S. Siso and N. Nobre, STFC Daresbury Lab
# Modified: A. B. G. Chalk, STFC Daresbury Lab

'''Module providing a transformation from a PSyIR ABS operator to
PSyIR code. This could be useful if the ABS operator is not supported
by the back-end or if the performance in the inline code is better
than the intrinsic.

'''
import warnings

from psyclone.psyir.transformations.intrinsics.intrinsic2code_trans import (
    Intrinsic2CodeTrans)
from psyclone.psyir.nodes import (
    BinaryOperation, Assignment, Reference, Literal, IfBlock, IntrinsicCall)
from psyclone.psyir.symbols import DataSymbol
from psyclone.utils import transformation_documentation_wrapper


@transformation_documentation_wrapper
class Abs2CodeTrans(Intrinsic2CodeTrans):
    '''Provides a transformation from a PSyIR ABS Operator node to
    equivalent code in a PSyIR tree. Validity checks are also
    performed.

    The transformation replaces

    .. code-block:: python

        R = ABS(X)

    with the following logic:

    .. code-block:: python

        IF X < 0.0:
            R = X*-1.0
        ELSE:
            R = X

    '''
    def __init__(self):
        super().__init__()
        self._intrinsic = IntrinsicCall.Intrinsic.ABS

    def validate(self, node, options=None):
        '''
        Check that it is safe to apply the transformation to the supplied node.

        :param node: the SIGN call to transform.
        :type node: :py:class:`psyclone.psyir.nodes.IntrinsicCall`
        :param options: any of options for the transformation.
        :type options: dict[str, Any]

        '''
        super().validate(node, options=options)
        super()._validate_scalar_arg(node)

    def apply(self, node, options=None, **kwargs):
        '''Apply the ABS intrinsic conversion transformation to the specified
        node. This node must be an ABS UnaryOperation. The ABS
        UnaryOperation is converted to equivalent inline code. This is
        implemented as a PSyIR transform from:

        .. code-block:: python

            R = ... ABS(X) ...

        to:

        .. code-block:: python

            tmp_abs = X
            if tmp_abs < 0.0:
                res_abs = tmp_abs*-1.0
            else:
                res_abs = tmp_abs
            R = ... res_abs ...

        where ``X`` could be an arbitrarily complex PSyIR expression
        and ``...`` could be arbitrary PSyIR code.

        This transformation requires the operation node to be a
        descendant of an assignment and will raise an exception if
        this is not the case.

        :param node: an ABS UnaryOperation node.
        :type node: :py:class:`psyclone.psyir.nodes.UnaryOperation`
        :param options: a dictionary with options for transformations.
        :type options: Optional[Dict[str, Any]]

        '''
        # TODO 2668: options are now deprecated:
        if options:
            warnings.warn(self._deprecation_warning, DeprecationWarning, 2)

        # pylint: disable=too-many-locals
        self.validate(node, options, **kwargs)

        symbol_table = node.scope.symbol_table
        assignment = node.ancestor(Assignment)

        # Create two temporary variables.
        result_type = node.arguments[0].datatype
        symbol_res_var = symbol_table.new_symbol(
            "res_abs", symbol_type=DataSymbol, datatype=result_type)
        symbol_tmp_var = symbol_table.new_symbol(
            "tmp_abs", symbol_type=DataSymbol, datatype=result_type)

        # Replace operation with a temporary (res_X).
        node.replace_with(Reference(symbol_res_var))

        # tmp_var=X
        lhs = Reference(symbol_tmp_var)
        rhs = node.arguments[0].detach()
        new_assignment = Assignment.create(lhs, rhs)
        assignment.parent.children.insert(assignment.position, new_assignment)

        # if condition: tmp_var>0.0
        lhs = Reference(symbol_tmp_var)
        rhs = Literal("0", result_type)
        if_condition = BinaryOperation.create(BinaryOperation.Operator.GT,
                                              lhs, rhs)

        # then_body: res_var=tmp_var
        lhs = Reference(symbol_res_var)
        rhs = Reference(symbol_tmp_var)
        then_body = [Assignment.create(lhs, rhs)]

        # else_body: res_var=-1.0*tmp_var
        lhs = Reference(symbol_res_var)
        lhs_child = Reference(symbol_tmp_var)
        rhs_child = Literal("-1", result_type)
        rhs = BinaryOperation.create(BinaryOperation.Operator.MUL, lhs_child,
                                     rhs_child)
        else_body = [Assignment.create(lhs, rhs)]

        # if [if_condition] then [then_body] else [else_body]
        if_stmt = IfBlock.create(if_condition, then_body, else_body)
        assignment.parent.children.insert(assignment.position, if_stmt)


# For AutoAPI auto-documentation generation.
__all__ = ["Abs2CodeTrans"]
