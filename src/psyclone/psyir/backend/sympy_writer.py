# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2021-2023, Science and Technology Facilities Council
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
# Modified: S. Siso, STFC Daresbury Lab


'''PSyIR backend to create expressions that are handled by sympy.
'''

# pylint: disable=too-many-lines

from sympy import Function, Symbol
from sympy.parsing.sympy_parser import parse_expr

from psyclone.psyir.backend.fortran import FortranWriter
from psyclone.psyir.frontend.fortran import FortranReader
from psyclone.psyir.backend.visitor import VisitorError
from psyclone.psyir.nodes import (BinaryOperation, DataNode, Literal,
                                  NaryOperation, Range, Reference,
                                  UnaryOperation)
from psyclone.psyir.symbols import (ArrayType, ScalarType, SymbolTable)


class SymPyWriter(FortranWriter):
    '''Implements a PSyIR-to-sympy writer, which is used to create a
    representation of the PSyIR tree that can be understood by SymPy. Most
    Fortran expressions work as expected without modification. This class
    implements special handling for constants (which can have a precision
    attached, e.g. 2_4) and some intrinsic functions (e.g. MAX, which SymPy
    expects to be Max).
    Array expressions are supported by the writer: it will convert any array
    expression like `a(i:j:k)` by using three arguments: `a(i, j, k)`.
    Then simple array accesses like `b(i,j)` are converted to
    `b(i,i,1,j,j,1)`. Similarly, if `a` is known to be an array, then the
    writer will use `a(-inf,inf,1)`. This makes sure all SymPy unknown
    functions that represent an array use the same number of arguments.

    It additionally supports accesses to structure types. A full description
    can be found in the manual:
    https://psyclone-dev.readthedocs.io/en/latest/sympy.html#sympy

    :param type_map: Optional initial mapping that contains the SymPy data \
        type of each reference in the expressions. This is the result of the \
        static function \
        :py:meth:`psyclone.core.sympy_writer.create_type_map`.
    :type type_map: dict of str:Sympy-data-type values
    '''
    # This option will disable the lowering of abstract nodes into language
    # level nodes, and as a consequence the backend does not need to deep-copy
    # the tree and is much faster to execute.
    # Be careful not to modify anything from the input tree when this option
    # is set to True as the modifications will persist after the Writer!
    _DISABLE_LOWERING = True

    def __init__(self):
        super().__init__()

        # The symbol table is used to create unique names for structure
        # members that are being accessed (these need to be defined as
        # SymPy functions or symbols, which could clash with other
        # references in the expression).
        self._symbol_table = SymbolTable()

        self._sympy_type_map = {}
        self._intrinsic = set()
        self._op_to_str = {}

        # Create the mapping of special operators/functions to the
        # name SymPy expects.
        for operator, op_str in [(NaryOperation.Operator.MAX, "Max"),
                                 (BinaryOperation.Operator.MAX, "Max"),
                                 (NaryOperation.Operator.MIN, "Min"),
                                 (BinaryOperation.Operator.MIN, "Min"),
                                 (UnaryOperation.Operator.FLOOR, "floor"),
                                 (BinaryOperation.Operator.REM, "Mod"),
                                 # exp is needed for a test case only, in
                                 # general the maths functions can just be
                                 # handled as unknown sympy functions.
                                 (UnaryOperation.Operator.EXP, "exp"),
                                 ]:
            self._intrinsic.add(op_str)
            self._op_to_str[operator] = op_str

    # -------------------------------------------------------------------------
    def __new__(cls, *expressions):
        '''This new function allows the SymPy writer to be used in two
        different ways: if only the SymPy expression of the PSyIR expressions
        are required, it can be called as:
        `sympy_expressions = SymPyWriter(exp1, exp2, ...)`
        But if additional information is needed (e.g. SymPy type map, or to
        convert a SymPy expression back to PSyIR), an instance of the
        SymPyWriter must be kept, e.g.:
            writer = SymPyWriter()
            sympy_expressions = writer([exp1, exp2, ...])
            writer.type_map

        :param expressions: a (potentially empty) tuple of PSyIR nodes
            to be converted to SymPy expressions.
        :type expressions: Tuple[:py:class:`psyclone.psyir.nodes.Node`]

        :returns: either an instance of SymPyWriter, if no parameter is
            specified, or a list of SymPy expressions.
        :rtype: Union[:py:class:`psyclone.psyir.backend.SymPyWriter`,
                      List[:py:class:`sympy.core.basic.Basic`]

        '''
        if expressions:
            writer = SymPyWriter()
            return writer(expressions)

        instance = super().__new__(cls)
        return instance

    # -------------------------------------------------------------------------
    def __getitem__(self, k):
        '''This function is only here to trick pylint into thinking that
        the object returned from __new__ is subscribtable, meaning that code
        like:
        ``out = SymPyWriter(exp1, exp2); out[1]`` does not trigger
        a pylint warning about unsubscriptable-object.
        '''

    # -------------------------------------------------------------------------
    def _create_type_map(self, list_of_expressions):
        '''
        This function creates a dictionary mapping each Reference in any
        of the expressions to either a Sympy Function (if the reference
        is an array reference) or a Symbol (if the reference is not an
        array reference).

        :param list_of_expressions: the list of expressions from which all \
            references are taken and added to the a symbol table to avoid \
            renaming any symbols (so that only member names will be renamed).
        :type list_of_expressions: list of \
            :py:class:`psyclone.psyir.nodes.Node`
        :returns: the dictionary mapping each reference name to a Sympy \
            data type (Function of Symbol).
        :rtype: dictionary of string:Sympy-data-type values

        '''
        for expr in list_of_expressions:
            for ref in expr.walk(Reference):
                name = ref.name
                if name in self._symbol_table:
                    continue
                self._symbol_table.find_or_create(name)

                # Test if an array or an array expression is used:
                if not ref.is_array:
                    self._sympy_type_map[name] = Symbol(name)
                    continue

                # Now a new Fortran array is used. Declare a SymPy

                def print_fortran_array(self, printer):
                    '''A custom print function to convert a modified
                    Fortran array access back to standard Fortran. It
                    converts the three values that each index is converted
                    to back into the Fortran array notation.'''
                    # pylint: disable=protected-access
                    args = [printer._print(i) for i in self.args]
                    name = self.__class__.__name__
                    new_args = []
                    # Analyse each triple of parameters, and add the
                    # corresponding index into new_argsL
                    for i in range(0, len(args), 3):
                        if args[i] == args[i+1] and args[i+2] == "1":
                            # a(i,i,1) --> a(i)
                            new_args.append(args[i])
                        elif args[i] == "-inf" and args[i+1] == "inf" and \
                                args[i+2] == "1":
                            # a(-inf, inf, 1) --> a(:)
                            new_args.append(":")
                        else:
                            if args[i+2] == "1":
                                # a(i,j,1) --> a(i:j)
                                new_args.append(f"{args[i]}:{args[i+1]}")
                            else:
                                # a(i,j,k) --> a(i:j:k)
                                new_args.append(f"{args[i]}:{args[i+1]}:"
                                                f"{args[i+2]}")
                    return f"{name}({','.join(new_args)})"

                self._sympy_type_map[name] = \
                    type(name, (Function, ),
                         {"_sympystr": print_fortran_array})

    # -------------------------------------------------------------------------
    @property
    def type_map(self):
        ''':returns: the mapping of names to SymPy objects.
        rtype: Dict[str, :py:class:`sympy.core.symbol.Symbol`]

        '''
        return self._sympy_type_map

    # -------------------------------------------------------------------------
    def _to_str(self, list_of_expressions):
        '''Converts PSyIR expressions to strings. It will replace Fortran-
        specific expressions with code that can be parsed by SymPy. The
        argument can either be a single element (in which case a single string
        is returned) or a list/tuple, in which case a list is returned.

        :param list_of_expressions: the list of expressions which are to be \
            converted into SymPy-parsable strings.
        :type list_of_expressions: Union[:py:class:`psyclone.psyir.nodes.Node`,
        List[:py:class:`psyclone.psyir.nodes.Node`]]

        :returns: the converted strings(s).
        :rtype: Union[str, List[str]]

        '''
        is_list = isinstance(list_of_expressions, (tuple, list))
        if not is_list:
            list_of_expressions = [list_of_expressions]
        # Create the writer for both expressions:
        self._create_type_map(list_of_expressions)

        expression_str_list = []
        for expr in list_of_expressions:
            expression_str_list.append(super().__call__(expr))

        # If the argument was a single expression, only return a single
        # expression, otherwise return a list
        if not is_list:
            return expression_str_list[0]
        return expression_str_list

    # -------------------------------------------------------------------------
    def __call__(self, list_of_expressions):
        '''
        This function takes a list of PSyIR expressions, and converts
        them all into Sympy expressions using the SymPy parser.
        It takes care of all Fortran specific conversion required (e.g.
        constants with kind specification, ...), including the renaming of
        member accesses, as described in
        https://psyclone-dev.readthedocs.io/en/latest/sympy.html#sympy
        It also returns the symbol map, i.e. the mapping of Fortran symbol
        names to SymPy Symbols.

        :param list_of_expressions: the list of expressions which are to be \
            converted into SymPy-parsable strings.
        :type list_of_expressions: list of \
            :py:class:`psyclone.psyir.nodes.Node`

        :returns: a 2-tuple consisting of the the converted PSyIR \
            expressions, followed by a dictionary mapping the symbol names \
            to SymPy Symbols.
        :rtype: Tuple[List[:py:class:`sympy.core.basic.Basic`], \
            Dict[str, :py:class:`sympy.core.symbol.Symbol`]]

        :raises VisitorError: if an invalid SymPy expression is found.

        '''
        is_list = isinstance(list_of_expressions, (tuple, list))
        if not is_list:
            list_of_expressions = [list_of_expressions]
        expression_str_list = self._to_str(list_of_expressions)

        result = []
        for expr in expression_str_list:
            try:
                result.append(parse_expr(expr, self.type_map))
            except SyntaxError as err:
                raise VisitorError(f"Invalid SymPy expression: '{expr}'.") \
                    from err

        if is_list:
            return result
        # We had no list initially, so only convert the one and only
        # list member
        return result[0]

    # -------------------------------------------------------------------------
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

        :param node: a Member PSyIR node.
        :type node: :py:class:`psyclone.psyir.nodes.Member`

        :returns: the SymPy representation of this member access.
        :rtype: str

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
        if new_name not in self._sympy_type_map:
            if node.is_array:
                self._sympy_type_map[new_name] = Function(new_name)
            else:
                self._sympy_type_map[new_name] = Symbol(new_name)

        # Now get the original string that this node produces:
        original_name = super().member_node(node)

        # And replace the `node.name` (which must be at the beginning since
        # it is a member) with the new name from the symbol table:
        return new_name + original_name[len(node.name):]

    # -------------------------------------------------------------------------
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
            raise TypeError(f"SymPy cannot handle strings "
                            f"like '{node.value}'.")
        # All real (single, double precision) and integer work by just
        # using the node value. Single and double precision both use
        # 'e' as specification, which SymPy accepts, and precision
        # information can be ignored.
        return node.value

    # -------------------------------------------------------------------------
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

    # -------------------------------------------------------------------------
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

    # -------------------------------------------------------------------------
    def reference_node(self, node):
        '''This method is called when a Reference instance is found in the
        PSyIR tree. It handles the case that this normal reference might
        be an array expression, which in the SymPy writer needs to have
        indices added explicitly.

        :param node: a Reference PSyIR node.
        :type node: :py:class:`psyclone.psyir.nodes.Reference`

        :returns: the text representation of this reference.
        :rtype: str

        '''
        if not node.is_array:
            return super().reference_node(node)

        # This must be an array expression without parenthesis:
        shape = node.symbol.shape
        result = ["-inf,inf,1"]*len(shape)

        return (f"{node.name}{self.array_parenthesis[0]}"
                f"{','.join(result)}{self.array_parenthesis[1]}")

    # ------------------------------------------------------------------------
    def gen_indices(self, indices, var_name=None):
        '''Given a list of PSyIR nodes representing the dimensions of an
        array, return a list of strings representing those array dimensions.
        This is used both for array references and array declarations. Note
        that 'indices' can also be a shape in case of Fortran. The
        implementation here overwrites the one in the base class to convert
        each array index into a three parameters to support array expressions.

        :param indices: list of PSyIR nodes.
        :type indices: list of :py:class:`psyclone.psyir.symbols.Node`
        :param str var_name: name of the variable for which the dimensions \
            are created. Not used in the Fortran implementation.

        :returns: the Fortran representation of the dimensions.
        :rtype: list of str

        :raises NotImplementedError: if the format of the dimension is not \
            supported.

        '''
        dims = []
        for index in indices:
            if isinstance(index, DataNode):
                # literal constant, symbol reference, or computed
                # dimension
                expression = self._visit(index)
                dims.extend([expression, expression, "1"])
            elif isinstance(index, Range):
                # literal constant, symbol reference, or computed
                # dimension
                expression = self._visit(index)
                dims.append(expression)
            elif isinstance(index, ArrayType.ArrayBounds):
                # Lower and upper bounds of an array declaration specified
                # by literal constant, symbol reference, or computed dimension
                lower_expression = self._visit(index.lower)
                upper_expression = self._visit(index.upper)
                dims.extend([lower_expression, upper_expression, "1"])
            elif isinstance(index, ArrayType.Extent):
                # unknown extent
                dims.extend(["-inf", "inf", "1"])
            else:
                raise NotImplementedError(
                    f"unsupported gen_indices index '{index}'")
        return dims

    # -------------------------------------------------------------------------
    def range_node(self, node):
        '''This method is called when a Range instance is found in the PSyIR
        tree. This implementation convers a range into three parameters
        for the corresponding SymPy function.

        :param node: a Range PSyIR node.
        :type node: :py:class:`psyclone.psyir.nodes.Range`

        :returns: the Fortran code as a string.
        :rtype: str

        '''
        if node.parent and node.parent.is_lower_bound(
                node.parent.indices.index(node)):
            # The range starts for the first element in this
            # dimension. This is the default in Fortran so no need to
            # output anything.
            start = "-inf"
        else:
            start = self._visit(node.start)

        if node.parent and node.parent.is_upper_bound(
                node.parent.indices.index(node)):
            # The range ends with the last element in this
            # dimension. This is the default in Fortran so no need to
            # output anything.
            stop = "inf"
        else:
            stop = self._visit(node.stop)
        result = f"{start},{stop}"

        if isinstance(node.step, Literal) and \
           node.step.datatype.intrinsic == ScalarType.Intrinsic.INTEGER and \
           node.step.value == "1":
            result += ",1"
            # Step is 1. This is the default in Fortran so no need to
            # output any text.
        else:
            step = self._visit(node.step)
            result += f",{step}"

        return result

    # -------------------------------------------------------------------------
    def sympy_to_psyir(self, sympy_expr, symbol_table):
        '''This function converts a SymPy expression back into PSyIR. It first
        parses the SymPy expression back into PSyIR, and then replaces all
        array indices back into the corresponding Fortran values (since they
        were replaced with three parameters to support array expressions), e.g.
        `a(i,i,1)` will be converted back to `a(i)`, and `a(-inf,5,2)` will
        become `a(:5:2)`.

        :param sympy_expr: the original SymPy expression.
        :type sympy_expr: py:class:`sympy.core.basic.Basic`
        :param symbol_table: the symbol table required for parsing, it \
            should be the table from which the original SymPy expression \
            was created from (i.e. contain all the required symbols in the \
            SymPy expression).
        :type symbol_table: :py:class:`psyclone.psyir.symbols.SymbolTable`

        :returns: the PSyIR representation of the SymPy expression.
        :rtype: :py:class:`psyclone.psyir.nodes.Node`

        '''
        # Convert the new sympy expression to PSyIR
        reader = FortranReader()
        new_expr = reader.psyir_from_expression(str(sympy_expr), symbol_table)
        return new_expr
