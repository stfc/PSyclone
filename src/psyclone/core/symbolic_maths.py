# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2021-2023, Science and Technology Facilities Council.
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
# Author J. Henrichs, Bureau of Meteorology
# Modified: R. W. Ford and N. Nobre, STFC Daresbury Lab
# -----------------------------------------------------------------------------

''' This module provides access to sympy-based symbolic maths
functions.'''


from sympy import (Complexes, ConditionSet, core, EmptySet, expand, FiniteSet,
                   ImageSet, simplify, solvers, Union)


class SymbolicMaths:
    '''A wrapper around the symbolic maths package 'sympy'. It
    provides convenience functions for PSyclone. It has a Singleton
    access, e.g.:

    >>> from psyclone.psyir.backend.fortran import FortranWriter
    >>> from psyclone.core import SymbolicMaths
    >>> sympy = SymbolicMaths.get()
    >>> # Assume lhs is the PSyIR of 'i+j', and rhs is 'j+i'
    >>> if sympy.equal(lhs, rhs):
    ...     writer = FortranWriter()
    ...     print(f"'{writer(lhs)}' and '{writer(rhs)}' are equal.")
    'i + j' and 'j + i' are equal.

    '''
    # Keeps track if importing sympy has been tried.
    _has_been_imported = False

    # Class variable to store the SymbolicMaths instance if sympy is
    # available, or None otherwise.
    _instance = None

    # -------------------------------------------------------------------------
    @staticmethod
    def get():
        '''Static function that creates (if necessary) and returns the
        singleton SymbolicMaths instance.

        :returns: the instance of the symbolic maths class.
        :rtype: :py:class:`psyclone.core.SymbolicMaths`

        '''
        if SymbolicMaths._instance is None:
            SymbolicMaths._instance = SymbolicMaths()

        return SymbolicMaths._instance

    # -------------------------------------------------------------------------
    @staticmethod
    def equal(exp1, exp2):
        '''Test if the two PSyIR expressions are symbolically equivalent.

        :param exp1: the first expression to be compared.
        :type exp1: :py:class:`psyclone.psyir.nodes.Node`
        :param exp2: the second expression to be compared.
        :type exp2: :py:class:`psyclone.psyir.nodes.Node`

        :returns: whether the two expressions are mathematically \
            identical.
        :rtype: bool

        '''
        # Some tests provide a None as parameters
        if exp1 is None or exp2 is None:
            return exp1 == exp2

        diff = SymbolicMaths._subtract(exp1, exp2)
        # For ranges all values (start, stop, step) must be equal, meaning
        # each index of the difference must evaluate to 0:
        if isinstance(diff, list):
            return all(isinstance(i, core.numbers.Zero) for i in diff)
        return isinstance(diff, core.numbers.Zero)

    # -------------------------------------------------------------------------
    @staticmethod
    def never_equal(exp1, exp2):
        '''Returns if the given SymPy expressions are guaranteed to be
        different regardless of the values of symbolic variables. E.g.
        `n-1` and `n` are always different, but `5` and `n` are not always
        different.

        :param exp1: the first expression to be compared.
        :type exp1: :py:class:`psyclone.psyir.nodes.Node`
        :param exp2: the second expression to be compared.
        :type exp2: :py:class:`psyclone.psyir.nodes.Node`

        :returns: whether or not the expressions are never equal.
        :rtype: bool

        '''
        # Circular dependency:
        # pylint: disable=import-outside-toplevel
        from psyclone.psyir.backend.visitor import VisitorError

        try:
            result = SymbolicMaths._subtract(exp1, exp2)
        except VisitorError:
            return False
        # In case of a range, subtract will return a list of tuple. To
        # simplify the following tests, convert a non-list into a one
        # element list:
        if not isinstance(result, list):
            result = [result]

        # If the result(s) are all 0, the expressions are always the same:
        if all(isinstance(i, core.numbers.Zero) for i in result):
            return False

        # Handle the common case of a single integer expressions. In this
        # case we know because of the previous test that the result is not
        # 0, so if the result is an integer (i.e. not symbolic), we know that
        # that the expressions are never equal:
        if len(result) == 1 and isinstance(result[0], core.numbers.Integer):
            return True

        # TODO #2168: We can likely produce more precise results if we
        # analyse the values as ranges, e.g. the two results (1, 5, 2)
        # and (6,10, 2) coming from a(1:5:2) and (6:10:2) will never be
        # equal, but (1, 11, 2) and (5, 7, 2) will overlap. For now return
        # always False, indicating that they might be identical. A test like:
        # if any(isinstance(i, core.numbers.Integer) for i in result):
        # would allow us to handle e.g the case of all integer values. On
        # the other hand, it is not known how useful this is, i.e. if we
        # will ever see this.

        # Otherwise the result depends on one or more variables (e.g.
        # n-5), so it might be zero.
        return False

    # -------------------------------------------------------------------------
    @staticmethod
    def _subtract(exp1, exp2):
        '''Subtracts two PSyIR expressions and returns the simplified result
        of this operation. An expression might result in multiple SymPy
        expressions - for example, a `Range` node becomes a 3-tuple (start,
        stop, step). In this case, each of the components will be handled
        individually, and a list will be returned.

        :param exp1: the first expression to be compared.
        :type exp1: Optional[:py:class:`psyclone.psyir.nodes.Node`]
        :param exp2: the second expression to be compared.
        :type exp2: Optional[:py:class:`psyclone.psyir.nodes.Node`]

        :returns: the sympy expression resulting from subtracting exp2 \
            from exp1.
        :rtype: Union[:py:class:`sympy.core.basic.Basic`,
                      List[:py:class:`sympy.core.basic.Basic`]]

        '''
        # Avoid circular import
        # pylint: disable=import-outside-toplevel
        from psyclone.psyir.backend.sympy_writer import SymPyWriter

        # Use the SymPyWriter to convert the two expressions to
        # SymPy expressions:
        sympy_expressions = SymPyWriter(exp1, exp2)
        # If an expression is a range node, then the corresponding SymPy
        # expression will be a tuple:
        if isinstance(sympy_expressions[0], tuple) and \
                isinstance(sympy_expressions[1], tuple):
            result = []
            for i, j in zip(sympy_expressions[0], sympy_expressions[1]):
                result.append(simplify(i - j))
            return result

        # Simplify triggers a set of SymPy algorithms to simplify
        # the expression.
        return simplify(sympy_expressions[0] - sympy_expressions[1])

    # -------------------------------------------------------------------------
    @staticmethod
    def solve_equal_for(exp1, exp2, symbol):
        '''Returns all solutions of exp1==exp2, solved for
        the specified symbol. It restricts the solution domain to integer
        values. If there is an infinite number of solutions, it returns
        the string 'independent', indicating that the solution of exp1==exp2
        does not depend on the specified symbol. This is done to avoid that
        the SymPy instance representing an infinite set is used elsewhere
        in PSyclone (i.e. creating a dependency in other modules to SymPy).
        Otherwise a standard Python set is returned that stores the solutions.

        :param exp1: the first expression.
        :type exp1: :py:class:`sympy.core.basic.Basic`
        :param exp2: the second expression.
        :type exp2: :py:class:`sympy.core.basic.Basic`
        :param symbol: the symbol for which to solve.
        :type symbol: :py:class:`sympy.core.symbol.Symbol`

        :returns: a set of solutions, or the string "independent".
        :rtype: Union[set, str]

        '''
        # We could restrict the domain to Integers, but in case of
        # general solutions (x=i+1 or so), we get an intersection as
        # as result, which is then difficult to handle (conversion to a
        # python set results in iteration over all Integers). It's actually
        # easier to not restrict the domain, and detect and interpret
        # a non-integer solution later.
        # We use solvers.solveset to allow testing to monkeypatch solveset

        solution = solvers.solveset(exp1-exp2, symbol)
        if solution == Complexes:
            # The solution is actually independent of the symbol
            # Return a string (instead of the SymPy specific set
            # instance, which would introduce dependencies on
            # SymPy to other files).
            return "independent"

        if isinstance(solution, ConditionSet):
            # A ConditionSet indicates likely an equation that cannot be
            # solved, e.g. `indx(i)`=`indx(i+di)`. The index array `indx`
            # is treated as an unknown function by SymPy, so sympy will return
            # a ConditionSet. We return 'independent' here, indicating
            # that the solution is independent of the loop variable, which
            # means it will be triggering a dependence between loop iterations.
            return "independent"

        if isinstance(solution, ImageSet):
            # Similar to ConditionSet, this is returned if it's a mapping of
            # a set using a mathematical function, e.g. exp(i)==1. And
            # similarly we return independent, since it likely indicates a
            # dependency between the expressions.
            return "independent"

        if isinstance(solution, Union):
            # A SymPy union will only be returned if at least one of the
            # members has more than one (and likely infinite) solution, e.g.:
            # `i*(exp(i)-i)==0` (which returns the union of `i=0` and
            # `exp(i)==i`). Again, we don't handle this for now.
            return "independent"

        # If there is no solution, return a standard Python empty
        # set (to avoid using SymPy-specific types in PSyclone)
        if solution is EmptySet:
            return set()

        # There are other potential data types that could be returned by SymPy
        # (Interval, Intersection), but they seem not to be returned by
        # tests for `==0`. Testing will monkeypatch solveset to trigger this
        # line:
        if not isinstance(solution, FiniteSet):
            raise ValueError(f"Unexpected solution '{solution}'' of type "
                             f"'{type(solution)}'")

        # Convert the FiniteSet to a normal Python set:
        return set(solution)

    # -------------------------------------------------------------------------
    @staticmethod
    def expand(expr):
        '''Expand a PSyIR expression. This is done by converting the PSyIR
        expression to a sympy expression, applying the expansion
        operation and then converting the resultant output back into
        PSyIR.

        Currently does not work if the PSyIR expression contains Range
        nodes, see issue #1655.

        :param expr: the expression to be expanded.
        :type expr: :py:class:`psyclone.psyir.nodes.Node`

        '''
        # Avoid circular import
        # pylint: disable=import-outside-toplevel
        from psyclone.psyir.backend.sympy_writer import SymPyWriter
        from psyclone.psyir.frontend.sympy_reader import SymPyReader
        from psyclone.psyir.nodes import Reference, Literal

        # variables and literals do not require expansion
        if isinstance(expr, (Reference, Literal)):
            return
        # Convert the PSyIR expression to a sympy expression
        sympy_writer = SymPyWriter()
        sympy_expression = sympy_writer(expr)
        # Expand the expression
        result = expand(sympy_expression)

        # Find the required symbol table in the original PSyIR
        symbol_table = expr.scope.symbol_table

        sympy_reader = SymPyReader(sympy_writer)
        # Convert the new sympy expression to PSyIR
        new_expr = sympy_reader.psyir_from_expression(result, symbol_table)

        # If the expanded result is the same as the original then
        # nothing needs to be done.
        if new_expr == expr:
            return
        # Replace the old PSyIR expression with the new expanded PSyIR
        # expression
        expr.replace_with(new_expr)
