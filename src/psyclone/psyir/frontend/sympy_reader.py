# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2023, Science and Technology Facilities Council
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
# Author: J. Henrichs, Bureau of Meteorology


'''PSyIR frontend to convert a SymPy expression to PSyIR
'''


from psyclone.psyir.frontend.fortran import FortranReader


class SymPyReader():
    '''Implements a PSyIR-to-sympy writer, which is used to create a
    representation of the PSyIR tree that can be understood by SymPy. Most
    Fortran expressions work as expected without modification. This class
    implements special handling for constants (which can have a precision
    attached, e.g. 2_4) and some intrinsic functions (e.g. ``MAX``, which SymPy
    expects to be ``Max``). Array accesses are converted into functions (while
    SymPy supports indexed expression, they cannot be used as expected when
    solving, SymPy does not solve component-wise - ``M[x]-M[1]`` would not
    result in ``x=1``, while it does for SymPy unknown functions).
    Array expressions are supported by the writer: it will convert any array
    expression like ``a(i:j:k)`` by using three arguments: ``a(i, j, k)``.
    Then simple array accesses like ``b(i,j)`` are converted to
    ``b(i,i,1,j,j,1)``. Similarly, if ``a`` is known to be an array, then the
    writer will use ``a(sympy_lower,_upper_bound_name,1)``. This makes sure
    all SymPy unknown functions that represent an array use the same number of
    arguments.

    The simple use case of converting a (list of) PSyIR expressions to SymPy
    expressions is as follows::

        symp_expr_list = SymPyWriter(exp1, exp2, ...)

    If additional functionality is required (access to the type map or
    to convert a potentially modified SymPy expression back to PSyIR), an
    instance of SymPy writer must be created::

        writer = SymPyWriter()
        symp_expr_list = writer([exp1, exp2, ...])

    It additionally supports accesses to structure types. A full description
    can be found in the manual:
    https://psyclone-dev.readthedocs.io/en/latest/sympy.html#sympy

    '''
    _lower_bound = "lower_bound"
    _upper_bound = "upper_bound"

    def __init__(self, sympy_writer):
        SymPyReader._lower_bound = sympy_writer.lower_bound
        SymPyReader._upper_bound = sympy_writer.upper_bound

    def psyir_from_expression(self, sympy_expr, symbol_table):
        '''This function converts a SymPy expression back into PSyIR. It first
        parses the SymPy expression back into PSyIR, and then replaces all
        array indices back into the corresponding Fortran values (since they
        were replaced with three parameters to support array expressions), e.g.
        ``a(i,i,1)`` will be converted back to ``a(i)``, and ``a(-inf,5,2)``
        will become ``a(:5:2)``.

        :param sympy_expr: the original SymPy expression.
        :type sympy_expr: :py:class:`sympy.core.basic.Basic`
        :param symbol_table: the symbol table required for parsing, it
            should be the table from which the original SymPy expression
            was created from (i.e. contain all the required symbols in the
            SymPy expression).
        :type symbol_table: :py:class:`psyclone.psyir.symbols.SymbolTable`

        :returns: the PSyIR representation of the SymPy expression.
        :rtype: :py:class:`psyclone.psyir.nodes.Node`

        '''
        # Convert the new sympy expression to PSyIR
        reader = FortranReader()
        return reader.psyir_from_expression(str(sympy_expr), symbol_table)

    # -------------------------------------------------------------------------
    def print_fortran_array(self, printer):
        '''A custom print function to convert a modified
        Fortran array access back to standard Fortran. It
        converts the three values that each index is converted
        to back into the Fortran array notation.
        Access to this instance of the SymPy writer is required
        to access the names for lower and upper bounds. At the
        time this function is created, the names for these bounds
        cannot be defined (since it might clash with a variable
        name that will be seen later)

        :param printer: the SymPy writer base class.
        :type printer: :py:class:`sympy.printing.str.StrPrinter`
        :param sympy_writer: this instance of this SymPy writer.
            It is used to access the unique names for the lower-
            and upper-bounds.
        :type sympy_writer:
            :py:class:`psyclone.psyir.backend.SymPyWriter`

        '''
        # pylint: disable=protected-access, no-member
        args = [printer._print(i) for i in self.args]
        name = self.__class__.__name__
        lower_b = SymPyReader._lower_bound
        upper_b = SymPyReader._upper_bound

        # Analyse each triple of parameters, and add the
        # corresponding index into new_args:
        new_args = []
        for i in range(0, len(args), 3):
            if args[i] == args[i+1] and args[i+2] == "1":
                # a(i,i,1) --> a(i)
                new_args.append(args[i])
            elif args[i] == lower_b:
                if args[i+1] == upper_b and args[i+2] == "1":
                    # a(lower_b, upper_b, 1) --> a(:)
                    new_args.append(":")
                else:
                    if args[i+2] == "1":
                        # a(lower_b, i, 1) --> a(:i)
                        new_args.append(f":{args[i+1]}")
                    else:
                        # a(lower_b, i, j) --> a(:i:j)
                        new_args.append(f":{args[i+1]}:{args[i+2]}")
            elif args[i+1] == upper_b:
                if args[i+2] == "1":
                    # a(i, upper_b, 1) --> a(i:)
                    new_args.append(f"{args[i]}:")
                else:
                    # a(i, upper_b, j) --> a(i::j)
                    new_args.append(f"{args[i]}::{args[i+2]}")

            else:
                if args[i+2] == "1":
                    # a(i,j,1) --> a(i:j)
                    new_args.append(f"{args[i]}:{args[i+1]}")
                else:
                    # a(i,j,k) --> a(i:j:k)
                    new_args.append(f"{args[i]}:{args[i+1]}:"
                                    f"{args[i+2]}")
        return f"{name}({','.join(new_args)})"
