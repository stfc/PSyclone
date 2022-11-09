# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2021-2022, Science and Technology Facilities Council.
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
# Authors: R. W. Ford and A. R. Porter, STFC Daresbury Lab

'''Utilities for the PSyclone Adjoint (PSyAD) functionality.

'''
from psyclone.psyir.nodes import Reference, Literal, UnaryOperation, \
    BinaryOperation, Node
from psyclone.psyir.symbols import INTEGER_TYPE, DataSymbol


def node_is_active(node, active_variables):
    '''Determines whether this node contains variables that are active.

    :param node: the PSyIR node that is being evaluated.
    :type node: :py:class:`psyclone.psyir.nodes.Node`
    :param active_variables: a list of active variables either as \
        PSyIR symbols or as strings.
    :type active_variables: \
        List[:py:class:`psyclone.psyir.symbols.DataSymbol`] | \
        List[str]

    :returns: True if active and False otherwise.
    :rtype: bool

    :raises TypeError: if the node argument is not a Node.
    :raises TypeError: if the active_variables argument is not a list.
    :raises ValueError: if the active variables list does not contain \
        datasymbols or if the list does not contain strings.

    '''
    if not isinstance(node, Node):
        raise TypeError(
            f"The node argument to the node_is_active() method should be a "
            f"PSyIR Node, but found {type(node).__name__}")
    if not isinstance(active_variables, list):
        raise TypeError(
            f"The active_variables argument to the node_is_active() method "
            f"should be a list, but found {type(active_variables).__name__}.")
    if not (all(isinstance(item, DataSymbol) for item in active_variables) or
            all(isinstance(item, str) for item in active_variables)):
        item_types = [type(item).__name__ for item in active_variables]
        raise ValueError(
            f"Expected the active_variables argument to the node_is_active() "
            f"method to be a list containing either solely PSyIR DataSymbols "
            f"or solely strings, but found {item_types}.")

    if active_variables and isinstance(active_variables[0], str):
        for reference in node.walk(Reference):
            if reference.symbol.name.lower() in active_variables:
                return True
    else:
        for reference in node.walk(Reference):
            if reference.symbol in active_variables:
                return True
    return False


def node_is_passive(node, active_variables):
    '''Determines whether this node contains only variables that are
    passive.

    :param node: the PSyIR node that is being evaluated.
    :type node: :py:class:`psyclone.psyir.nodes.Node`
    :param active_variables: a list of active variables either as \
        PSyIR symbols or as strings.
    :type active_variables: \
        List[:py:class:`psyclone.psyir.symbols.DataSymbol`] or \
        List[str]

    :returns: True if passive and False otherwise.
    :rtype: bool

    '''
    return not node_is_active(node, active_variables)


def negate_expr(orig_expr):
    '''Takes a PSyIR expression and negates it by multiplying it by minus
    one. it is assumed that the expression returns an integer.

    :param orig_expr: the PSyIR expression to negate.
    :type orig_expr: :py:class:`psyclone.psyir.nodes.DataNode`

    :returns: the expression multiplied by minus one.
    :rytpe: :py:class:`psyclone.psyir.nodes.DataNode`

    '''
    expr = orig_expr.copy()
    # TODO, this would be better using sympy see #1497
    if isinstance(expr, Literal):
        if expr.value.lstrip()[0] == "-":
            # The literal has a negative value so make it positive.
            # pylint: disable=protected-access
            expr._value = expr.value.lstrip()[1:]
            return expr
        # Negate the literal with a unary minus.
        return UnaryOperation.create(
            UnaryOperation.Operator.MINUS, expr)
    if (isinstance(expr, UnaryOperation) and
            expr.operator == UnaryOperation.Operator.MINUS):
        return expr.children[0].detach()
    return BinaryOperation.create(
        BinaryOperation.Operator.MUL, Literal("-1", INTEGER_TYPE), expr)


# =============================================================================
# Documentation utils: The list of module members that we wish AutoAPI to
# generate documentation for (see https://psyclone-ref.readthedocs.io).
__all__ = ["node_is_active", "node_is_passive", "negate_expr"]
