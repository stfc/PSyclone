# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2021-2022, Science and Technology Facilities Council
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


'''PSyIR backend to create expressions that are handled by sympy.
'''

# pylint: disable=too-many-lines
from __future__ import absolute_import

from sympy import Function, Symbol

from psyclone.psyir.backend.fortran import FortranWriter
from psyclone.psyir.nodes import (BinaryOperation, NaryOperation,
                                  Reference)
from psyclone.psyir.symbols import ScalarType, SymbolTable


class SymPyWriter(FortranWriter):
    '''Implements a PSyIR-to-sympy writer, which is used to create a
    representation of the PSyIR tree that can be understood by SymPy. Most
    Fortran expressions work as expected without modification. This class
    implements special handling for constants (which can have a precision
    attached, e.g. 2_4) and some intrinsic functions (e.g. MAX, which SymPy
    expects to be Max).
    It additionally supports accesses to structure types. A full description
    can be found in the manual:
    https://psyclone-dev.readthedocs.io/en/latest/sympy.html#internal-details

    '''

    def __init__(self, list_of_expressions=None):
        super().__init__()

        # The symbol table is used to create unique names for structure
        # members that are being accessed (these need to be defined as
        # SymPy functions or symbols, which could clash with other
        # references in the expression).
        self._symbol_table = SymbolTable()

        if list_of_expressions is None:
            list_of_expressions = []
        # First add all references. This way we can be sure that the writer
        # will never rename a reference. This directory keeps track of which
        # names are arrays (--> must be declared as a SymPy function) or
        # non-array (--> must be declared as a SymPy symbol).
        self._sympy_type_map = {}
        for expr in list_of_expressions:
            for ref in expr.walk(Reference):
                name = ref.name
                self._symbol_table.find_or_create_tag(tag=name, root_name=name)
                if ref.is_array:
                    self._sympy_type_map[name] = Function(name)
                else:
                    self._sympy_type_map[name] = Symbol(name)

        self._intrinsic = set()
        self._op_to_str = {}

        # Create the mapping of special operators/functions to the
        # name SymPy expects.
        for operator, op_str in [(NaryOperation.Operator.MAX, "Max"),
                                 (BinaryOperation.Operator.MAX, "Max"),
                                 (NaryOperation.Operator.MIN, "Min"),
                                 (BinaryOperation.Operator.MIN, "Min"),
                                 (BinaryOperation.Operator.REM, "Mod"),
                                 ]:
            self._intrinsic.add(op_str)
            self._op_to_str[operator] = op_str

    def get_sympy_type_map(self):
        ''':returns: the mapping of symbols in the written
            PSyIR expressions to SymPy types (Function or Symbol).
        :rtype: dict of string to SymPy.Symbol or SymPy.Function

        '''
        return self._sympy_type_map

    def member_node(self, node):
        '''In SymPy an access to a member 'b' of a structure 'a'
        (i.e. a%b in Fortran) is handled as the 'MOD' function
        `MOD(a, b)`. We must therefore make sure that a member
        access is unique (e.g. `b` could already be a scalar variable).
        This is done by creating a new name, which replaces the `%`
        with an `_`. So `a%b` becomes `MOD(a, a_b)`. This makes it easier
        to see where the function names come from.
        Additionally, we still need to avoid a name clash, e.g. there
        could already be a variable `a_b`. This is done by using a symbol
        table, which was prefilled with all references (`a` in the example
        above) in the constructor. We use the string containing the '%' as
        a unique tag and get a new, unique symbol from the symbol table
        based on the new name using `_`. For example, the access to member
        `b` in `a(i)%b` would result in a new symbol with tag `a%b` and a
        name like `a_b`, `a_b_1`, ...
        '''

        # We need to find the parent reference in order to make a new
        # name (a%b%c --> a_b_c). Collect the names of members and the
        # symbol in a list.
        parent = node
        name_list = [node.name]
        while not isinstance(parent, Reference):
            parent = parent.parent
            name_list.append(parent.name)
        name_list.reverse()

        # The root name uses _, the tag uses % (which are guaranteed
        # to be unique, the root_name might clash with a user defined
        # variable otherwise).
        root_name = "_".join(name_list)
        sig_name = "%".join(name_list)
        new_sym = self._symbol_table.find_or_create_tag(tag=sig_name,
                                                        root_name=root_name)
        new_name = new_sym.name
        if node.is_array:
            self._sympy_type_map[new_name] = Function(new_name)
        else:
            self._sympy_type_map[new_name] = Symbol(new_name)

        # Now get the original string that this node produces:
        original_name = super().member_node(node)

        # And replace the `node.name` (which must be at the beginning since
        # it is a member) with the new name from the symbol table:
        return new_name + original_name[len(node.name):]

    def literal_node(self, node):
        '''This method is called when a Literal instance is found in the PSyIR
        tree. For SymPy we need to handle booleans (which are expected to
        be capitalised: True). Real values work by just ignoring any precision
        information (e.g. 2_4, 3.1_wp). Character constants are not supported
        and will raise an exception.

        :param node: a Literal PSyIR node.
        :type node: :py:class:`psyclone.psyir.nodes.Literal`

        :returns: the SymPy representation for the literal.
        :rtype: str

        :raises TypeError: if a character constant is found, which \
            is not supported with SymPy.

        '''
        if node.datatype.intrinsic == ScalarType.Intrinsic.BOOLEAN:
            # Booleans need to be converted to SymPy format
            return node.value.capitalize()

        if node.datatype.intrinsic == ScalarType.Intrinsic.CHARACTER:
            raise TypeError("SymPy cannot handle strings "
                            f"like '{node.value}'.")
        # All real (single, double precision) and integer work by just
        # using the node value. Single and double precision both use
        # 'e' as specification, which SymPy accepts, and precision
        # information can be ignored.
        return node.value

    def get_operator(self, operator):
        '''Determine the operator that is equivalent to the provided
        PSyIR operator. This implementation checks for certain functions
        that SymPy supports: Max, Min, Mod. These functions must be
        spelled with a capital first letter, otherwise SymPy will handle
        them as unknown functions. If none of these special operators
        are given, the base implementation is called (which will return
        the Fortran syntax).

        :param operator: a PSyIR operator.
        :type operator: :py:class:`psyclone.psyir.nodes.Operation.Operator`

        :returns: the operator as string.
        :rtype: str

        :raises KeyError: if the supplied operator is not known.

        '''

        try:
            return self._op_to_str[operator]
        except KeyError:
            return super().get_operator(operator)

    def is_intrinsic(self, operator):
        '''Determine whether the supplied operator is an intrinsic
        function (i.e. needs to be used as `f(a,b)`) or not (i.e. used
        as `a + b`). This tests for known SymPy names of these functions
        (e.g. Max), and otherwise calls the function in the base class.

        :param str operator: the supplied operator.

        :returns: true if the supplied operator is an \
            intrinsic and false otherwise.

        '''
        if operator in self._intrinsic:
            return True

        return super().is_intrinsic(operator)
