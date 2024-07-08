# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2023-2024, Science and Technology Facilities Council
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
    '''This class converts a SymPy expression, that was created by the
    SymPyWriter, back to PSyIR. It basically allows to use SymPy to modify
    PSyIR expressions:

        1. The SymPyWriter converts the Fortran expression to SymPy
        2. SymPy is used to manipulate these mathematical expressions
        3. The SymPyReader is used to convert these SymPy expressions back
           into PSyIR (from which source code can be recreated)

    Most SymPy expressions can be parsed as Fortran expression immediately,
    but the big exception to this are Fortran Arrays which must be able to
    support array expressions like ``a(1:5:2)``. As outlined in the
    SymPyWriter, each array is represented as an UndefinedFunction in SymPy,
    and each each dimension of the Fortran array will provide three
    arguments to this function: the start, stop, and step value. So the
    expression above will be converted to ``a(1,5,2)``, and ``a(7)`` will
    be represented as ``a(7:7:1)``. If the start- or stop-expression is
    not specified, e.g. ``a(:)``, the SymPy writer will use two special
    symbols ``sympy_lower`` and ``sympy_upper``, e.g. the above expression
    will become ``a(sympy_lower:sympy_upper:1)``. The SymPyWriter will
    change the name if required in case of a name clash (i.e. if the user
    declared a variable called ``sympy_lower`` etc).

    The task of the SymPy reader is to convert these expressions back to
    the original PSyIR representation. For example, the SymPy expressions
    ``a(i,j,k)`` will be written as ``a(i:j:k)``. This conversion is done
    by the function ``print_fortran_array`` by combining each 3-tuple of
    arguments back to the corresponding Fortran representation.

    The SymPyWriter sets the ``_sympystr`` attribute of the SymPy
    UndefinedFunction it creates to ``print_fortran_array``. The ``_sympystr``
    method is automatically called by SymPy when converting an expression
    into a string.

    In order to achieve this, the SymPyReader must know the names used for
    the lower- and upper-bounds. The constructor takes a SymPyWriter as
    argument in order to get the name of these bounds. It is important that
    the SymPyWriter provided here is the one that was used to create the SymPy
    expressions in the first place.

    :param sympy_writer: the SymPyWriter that was used to create the SymPy
        expressions.
    :type sympy_writer: :py:class:`psyclone.psyir.backend.SymPyWriter`

    '''
    # The default values for the bounds, they will be changed to the
    # correct values in the constructor.
    _lower_bound = "sympy_lower"
    _upper_bound = "sympy_upper"

    def __init__(self, sympy_writer):
        SymPyReader._lower_bound = sympy_writer.lower_bound
        SymPyReader._upper_bound = sympy_writer.upper_bound

    # -------------------------------------------------------------------------
    def psyir_from_expression(self, sympy_expr, symbol_table):
        '''This function converts a SymPy expression back into PSyIR. It
        converts the SymPy expression into a string, which is then parsed
        by the FortranReader. It relies on the ``print_fortran_array``
        function to convert the function arguments back to Fortran array
        expressions. This function will be called by SymPy when converting the
        functions that represent array indices into a string, see
        :py:class:`psyclone.psyir.backend.SymPyWriter._create_type_map`,
        where this function is defined to be called for string conversion.

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
    # pylint: disable=no-self-argument, too-many-branches
    def print_fortran_array(function, printer):
        '''A custom print function to convert a modified Fortran array access
        back to standard Fortran. This function is set as ``_sympystr_`` method
        of the SymPy functions created in the SymPyWriter (see
        ``_create_type_map`` method of the SymPyWriter), so it will be called
        by SymPy to convert this function to a string, with the function to
        convert being the first argument! This function converts
        the three values that each index is converted to back into the Fortran
        array notation. It uses the class variables
        ``SymPyReader._lower_bound`` and ``SymPyReader._upper_bound`` as the
        names that were used when the SymPy expressions were created in order
        to convert array expressions correctly back.

        :param function: this function is called from a SymPy Function class,
            therefore the first argument is a SymPy Function instance (and NOT
            a SymPyReader) instance.
        :type function: :py:class:`sympy.core.function.Function`
        :param printer: the SymPy writer base class.
        :type printer: :py:class:`sympy.printing.str.StrPrinter`

        :returns: the string representation of this array access.
        :rtype: str

        '''
        # pylint: disable=protected-access, no-member
        args = [printer._print(i) for i in function.args]
        name = function.__class__.__name__
        lower_b = SymPyReader._lower_bound
        upper_b = SymPyReader._upper_bound

        # Analyse each triple of parameters, and add the corresponding
        # converted index (or array expression) to new_args:
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

        if function._sig is None:
            # It's not a user defined type, just create the array access:
            return f"{name}({','.join(new_args)})"

        # It is a user defined type. Re-assemble the original call by
        # putting the corresponding indices to the individual members,
        # based on the information of the stored signature and number
        # of array indices for each member:

        result = []
        # This points at the next index to use from new_args, which
        # contains the indices converted back into Fortran:
        index_cursor = 0
        for i, member in enumerate(function._sig):
            # Get the number of indices this member had:
            num_dims = function._num_dims[i]
            indx = []
            for i in range(num_dims):
                indx.append(new_args[index_cursor])
                index_cursor += 1
            if indx:
                result.append(f"{member}({','.join(indx)})")
            else:
                result.append(member)
        return "%".join(result)
