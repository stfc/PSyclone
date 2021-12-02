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
# Author J. Henrichs, Bureau of Meteorology
# -----------------------------------------------------------------------------

''' This module provides access to sympy-based symbolic maths
functions.'''


from sympy import Function, simplify, Symbol, true
from sympy.core.function import UndefinedFunction
from sympy.parsing.sympy_parser import parse_expr


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
    ...     print("'{0}' and '{1}' are equal."
    ...           .format(writer(lhs), writer(rhs)))
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
        :rtype: :py:class:`psyclone.core.SymbolicMaths.`

        '''
        if SymbolicMaths._instance is None:
            SymbolicMaths._instance = SymbolicMaths()

        return SymbolicMaths._instance

    # -------------------------------------------------------------------------
    def __init__(self):
        # Avoid circular import
        # pylint: disable=import-outside-toplevel
        from psyclone.psyir.backend.sympy_writer import SymPyWriter

        self._writer = SymPyWriter()

    # -------------------------------------------------------------------------
    @staticmethod
    def get_all_symbols(local_dict, exp):
        '''Declares all references in the PSyIR expression `exp` either
        as SymPy functions (if they are array, i.e. use '(...)', or symbols
        (for scalar accesses without '(...)').
        Structures ('a%b') will get all members declared as
        individual symbols ('a', 'b'), since SymPy convers a structure
        access 'a%b' into 'MOD(a,b)' (see
        https://psyclone-dev.readthedocs.io/en/latest/sympy.html#internal-details
        for details).

        '''
        # Avoid circular import
        # pylint: disable=import-outside-toplevel
        from psyclone.psyir.nodes import Reference

        for ref in exp.walk(Reference):
            # Use the signature of the variable to easily get access
            # to its components. We need the indices to know as what
            # type to declare the symbol: array accesses will need to be
            # declared as SymPy functions, scalar accesses as SymPy symbols.
            sig, indices = ref.get_signature_and_indices()

            # Loop over all components of the Reference and add them
            for ind, name in enumerate(sig):
                access_is_array = len(indices[ind]) > 0

                # If the name is already declared, make sure the type
                # is consistent:
                if name in local_dict:
                    if (access_is_array and isinstance(local_dict[name],
                                                       Symbol)):
                        raise TypeError("The symbol '{0}' was first used as "
                                        "scalar, but also as array, which is "
                                        "not supported.".format(name))
                    if not access_is_array and isinstance(local_dict[name],
                                                          UndefinedFunction):
                        raise TypeError("The symbol '{0}' was first used as "
                                        "array, but also as scalar, which is "
                                        "not supported.".format(name))

                if access_is_array:
                    # Arrays need to be defined as functions
                    local_dict[name] = Function(name)
                else:
                    local_dict[name] = Symbol(name)

    # -------------------------------------------------------------------------
    def equal(self, exp1, exp2):
        '''Test if the two PSyIR operations are identical. This is
        done by converting the operations to the equivalent Fortran
        representation, which can be fed into sympy for evaluation.

        :param exp1: the first expression to be compared.
        :type exp1: py:class:`psyclone.psyir.nodes.Node` or None
        :param exp2: the first expression to be compared.
        :type exp2: py:class:`psyclone.psyir.nodes.Node` or None

        :returns: whether the two expressions are mathematically \
            identical.
        :rtype: bool

        '''
        # Some tests provide a None as parameters
        if exp1 is None or exp2 is None:
            return exp1 == exp2

        # Get all the symbols used in the two expressions, so the
        # SymPy parser recognise them as symbols (and not e.g. as
        # internal SymPy names)
        local_dict = {}
        self.get_all_symbols(local_dict, exp1)
        self.get_all_symbols(local_dict, exp2)

        str_exp1 = parse_expr(self._writer(exp1), local_dict=local_dict)
        str_exp2 = parse_expr(self._writer(exp2), local_dict=local_dict)

        # Simplify triggers a set of SymPy algorithms to simplify
        # the expression.
        result = simplify(str_exp1 == str_exp2)

        # Convert SymPy boolean to python boolean.
        return result is true
