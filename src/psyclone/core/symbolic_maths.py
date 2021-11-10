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


from psyclone.errors import InternalError


class SymbolicMaths:
    '''A wrapper around the symbolic maths package 'sympy'. It
    provides convenience functions for PSyclone. It has a Singleton
    access, e.g.:
    >>> sympy = SymbolicMaths.get()
    >>> if sympy:
    ...     print("Sympy found")

    '''
    # Keeps track if importing sympy has been tried.
    _has_been_imported = False

    # Class variable to store the SymbolicMaths instance if sympy is
    # available, or None otherwise.
    _instance = None

    # -------------------------------------------------------------------------
    @staticmethod
    def get():
        '''Static function that if necessary creates and returns the singleton
        SymbolicMaths instance if available. If sympy is not installed,
        this function will return None.

        :returns: the instance of the symbolic maths class.
        :rtype: :py:class:`psyclone.core.SymbolicMaths.`

        '''
        if not SymbolicMaths._has_been_imported:
            try:
                # pylint: disable=unused-import, import-outside-toplevel
                import sympy
                SymbolicMaths._instance = SymbolicMaths()
            except ImportError:
                SymbolicMaths._instance = None

            SymbolicMaths._has_been_imported = True

        return SymbolicMaths._instance

    # -------------------------------------------------------------------------
    def __init__(self):

        # Avoid circular dependency (and import errors if sympy
        # is not installed)
        # pylint: disable=import-outside-toplevel
        from sympy.parsing.sympy_parser import parse_expr
        from sympy import simplify
        from psyclone.psyir.backend.fortran import FortranWriter

        self._writer = FortranWriter()
        self._parse_expr = parse_expr
        self._simplify = simplify

    # -------------------------------------------------------------------------

    def equal(self, exp1, exp2):
        '''Test if the two PSyIR operations are identical. This is
        done by converting the operations to the equivalent Fortran
        representation, which can be fed into sympy for evaluation.

        :param exp1: the first expression to be compared.
        :type exp2: py:class:`psyclone.psyir.nodes.Operation`

        :raises InternalError: if either exp1 or exp2 is not an Operation.

        '''
        # Avoid circular import
        # pylint: disable=import-outside-toplevel
        from psyclone.psyir.nodes import Operation

        str_exp1 = self._parse_expr(self._writer(exp1))
        str_exp2 = self._parse_expr(self._writer(exp2))
        if not isinstance(exp1, Operation):
            raise InternalError("Expected exp1 '{0}' to be an operation.".
                                format(str_exp1))
        if not isinstance(exp2, Operation):
            raise InternalError("Expected exp2 '{0}' to be an operation.".
                                format(str_exp2))
        result = self._simplify(str_exp1 == str_exp2)
        return result
