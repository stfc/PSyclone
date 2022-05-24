# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2019-2022, Science and Technology Facilities Council
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
# Author S. Siso, STFC Daresbury Lab.
# Modified by: J. Henrichs, Bureau of Meteorology
#              A. R. Porter, R. W. Ford and N. Nobre, STFC Daresbury Lab
#              A. B. G. Chalk, STFC Daresbury Lab


'''C PSyIR backend. Generates C code from PSyIR nodes.
Currently limited to just a few PSyIR nodes to support the OpenCL generation,
it needs to be extended for generating pure C code.

'''
from psyclone.psyir.backend.language_writer import LanguageWriter
from psyclone.psyir.backend.visitor import VisitorError
from psyclone.psyir.nodes import BinaryOperation, UnaryOperation
from psyclone.psyir.symbols import ScalarType


# PSyIR datatypes now support precision as well as intrinsics. It is
# not clear how to map PSyIR intrinsics and precision onto C types,
# see issue #738.
# Mapping from PSyIR types to C data types.
TYPE_MAP_TO_C = {ScalarType.Intrinsic.INTEGER: "int",
                 ScalarType.Intrinsic.CHARACTER: "char",
                 ScalarType.Intrinsic.BOOLEAN: "bool",
                 ScalarType.Intrinsic.REAL: "double"}


class CWriter(LanguageWriter):
    '''Implements a PSyIR-to-C back-end for the PSyIR AST.

    :param bool skip_nodes: If skip_nodes is False then an exception \
        is raised if a visitor method for a PSyIR node has not been \
        implemented, otherwise the visitor silently continues. This is an \
        optional argument which defaults to False.
    :param str indent_string: Specifies what to use for indentation. This \
        is an optional argument that defaults to two spaces.
    :param int initial_indent_depth: Specifies how much indentation to \
        start with. This is an optional argument that defaults to 0.
    :param bool check_global_constraints: whether or not to validate all \
        global constraints when walking the tree. Defaults to True.

    '''
    def __init__(self, skip_nodes=False, indent_string="  ",
                 initial_indent_depth=0, check_global_constraints=True):

        super(CWriter, self).__init__(("[", "]"), ".", skip_nodes,
                                      indent_string,
                                      initial_indent_depth,
                                      check_global_constraints)

    def gen_indices(self, indices, var_name=None):
        '''Given a list of PSyIR nodes representing the dimensions of an
        array, return a list of strings representing those array dimensions.

        :param indices: list of PSyIR nodes.
        :type indices: list of :py:class:`psyclone.psyir.symbols.Node`
        :param str var_name: Name of the field for which the indices are \
            created. The C-interface uses {var_name}LEN{n} as the size \
            of the corresonding dimension `n`.

        :returns: the C representation of the dimensions.
        :rtype: list of str

        '''
        # In C array expressions should be reversed from the PSyIR order
        # (column-major to row-major order) and flattened (1D).

        # This collects the individual terms for each dimension that
        # must be added:
        summands = []
        # This is the ongoing product of all dimension sizes, i.e.
        # ALEN1 * ALEN2 * ...
        multiplicator = ""

        for dimension, child in enumerate(indices):
            expression = self._visit(child)
            dim_str = f"{var_name}LEN{dimension+1}"
            if multiplicator:
                summands.append(expression + " * " + multiplicator)
                multiplicator = multiplicator + " * " + dim_str
            else:
                summands.append(expression)
                multiplicator = dim_str
        # This function must return a list of indices, since in C
        # there is only one dimension, return a one-dimensional list.
        return [" + ".join(summands)]

    def gen_declaration(self, symbol):
        # pylint: disable=no-self-use
        '''
        Generates string representing the C declaration of the symbol. In C
        declarations can be found inside the argument list or with the
        statments, so no indention or punctuation is generated by this method.

        :param symbol: The symbol instance.
        :type symbol: :py:class:`psyclone.psyir.symbols.DataSymbol`

        :returns: The C declaration of the given symbol.
        :rtype: str

        :raises NotImplementedError: if there are some symbol types or nodes \
            which are not implemented yet.
        '''
        code = ""
        try:
            intrinsic = symbol.datatype.intrinsic
            code = code + TYPE_MAP_TO_C[intrinsic] + " "
        except (AttributeError, KeyError) as err:
            raise NotImplementedError(
                f"Could not generate C definition for variable '{symbol.name}'"
                f", type '{symbol.datatype}' is not yet supported.") from err

        # If the argument is an array, in C language we define it
        # as an unaliased pointer.
        if symbol.is_array:
            code += "* restrict "

        code += symbol.name
        return code

    def gen_local_variable(self, symbol):
        '''
        Generate C code that declares all local symbols in the Symbol Table.

        :param symbol: The symbol instance.
        :type symbol: :py:class:`psyclone.psyir.symbols.DataSymbol`

        :returns: C languague declaration of a local variable.
        :rtype: str
        '''
        return f"{self._nindent}{self.gen_declaration(symbol)};\n"

    def assignment_node(self, node):
        '''This method is called when an Assignment instance is found in the
        PSyIR tree.

        :param node: An Assignment PSyIR node.
        :type node: :py:class:`psyclone.psyir.nodes.Assignment``

        :returns: The C code as a string.
        :rtype: str

        '''
        lhs = self._visit(node.lhs)
        rhs = self._visit(node.rhs)

        result = f"{self._nindent}{lhs} = {rhs};\n"
        return result

    def literal_node(self, node):
        # pylint: disable=no-self-use
        '''This method is called when a Literal instance is found in the PSyIR
        tree.

        :param node: A Literal PSyIR node.
        :type node: :py:class:`psyclone.psyir.nodes.Literal`

        :returns: The C code as a string.
        :rtype: str

        '''
        result = node.value
        # C Scientific notation is always an 'e' letter
        result = result.replace('d', 'e')
        result = result.replace('D', 'e')
        return result

    def ifblock_node(self, node):
        '''This method is called when an IfBlock instance is found in the
        PSyIR tree.

        :param node: An IfBlock PSyIR node.
        :type node: :py:class:`psyclone.psyir.nodes.IfBlock`

        :returns: The C code as a string.
        :rtype: str

        :raises VisitorError: If node has fewer children than expected.

        '''
        if len(node.children) < 2:
            raise VisitorError(
                f"IfBlock malformed or incomplete. It should have at least "
                f"2 children, but found {len(node.children)}.")

        condition = self._visit(node.condition)

        self._depth += 1
        if_body = ""
        for child in node.if_body:
            if_body += self._visit(child)
        else_body = ""
        # node.else_body is None if there is no else clause.
        if node.else_body:
            for child in node.else_body:
                else_body += self._visit(child)
        self._depth -= 1

        if else_body:
            result = (
                f"{self._nindent}if ({condition}) {{\n"
                f"{if_body}"
                f"{self._nindent}}} else {{\n"
                f"{else_body}"
                f"{self._nindent}}}\n")
        else:
            result = (
                f"{self._nindent}if ({condition}) {{\n"
                f"{if_body}"
                f"{self._nindent}}}\n")
        return result

    def unaryoperation_node(self, node):
        '''This method is called when a UnaryOperation instance is found in
        the PSyIR tree.

        :param node: A UnaryOperation PSyIR node.
        :type node: :py:class:`psyclone.psyir.nodes.UnaryOperation`

        :returns: The C code as a string.
        :rtype: str

        :raises VisitorError: If this node has more than one child.
        :raises NotImplementedError: If the operator is not supported by the \
            C backend.

        '''
        if len(node.children) != 1:
            raise VisitorError(
                f"UnaryOperation malformed or incomplete. It should "
                f"have exactly 1 child, but found {len(node.children)}.")

        def operator_format(operator_str, expr_str):
            '''
            :param str operator_str: String representing the operator.
            :param str expr_str: String representation of the operand.

            :returns: C language operator expression.
            :rtype: str
            '''
            return "(" + operator_str + expr_str + ")"

        def function_format(function_str, expr_str):
            '''
            :param str function_str: Name of the function.
            :param str expr_str: String representation of the operand.

            :returns: C language unary function expression.
            :rtype: str
            '''
            return function_str + "(" + expr_str + ")"

        def cast_format(type_str, expr_str):
            '''
            :param str type_str: Name of the new type.
            :param str expr_str: String representation of the operand.

            :returns: C language unary casting expression.
            :rtype: str
            '''
            return "(" + type_str + ")" + expr_str

        # Define a map with the operator string and the formatter function
        # associated with each UnaryOperation.Operator
        opmap = {
            UnaryOperation.Operator.MINUS: ("-", operator_format),
            UnaryOperation.Operator.PLUS: ("+", operator_format),
            UnaryOperation.Operator.NOT: ("!", operator_format),
            UnaryOperation.Operator.SIN: ("sin", function_format),
            UnaryOperation.Operator.COS: ("cos", function_format),
            UnaryOperation.Operator.TAN: ("tan", function_format),
            UnaryOperation.Operator.ASIN: ("asin", function_format),
            UnaryOperation.Operator.ACOS: ("acos", function_format),
            UnaryOperation.Operator.ATAN: ("atan", function_format),
            UnaryOperation.Operator.ABS: ("abs", function_format),
            UnaryOperation.Operator.REAL: ("float", cast_format),
            UnaryOperation.Operator.SQRT: ("sqrt", function_format),
            }

        # If the instance operator exists in the map, use its associated
        # operator and formatter to generate the code, otherwise raise
        # an Error.
        try:
            opstring, formatter = opmap[node.operator]
        except KeyError as err:
            raise NotImplementedError(
                f"The C backend does not support the '{node.operator}' "
                f"operator.") from err

        return formatter(opstring, self._visit(node.children[0]))

    def binaryoperation_node(self, node):
        '''This method is called when a BinaryOperation instance is found in
        the PSyIR tree.

        :param node: A BinaryOperation PSyIR node.
        :type node: :py:class:`psyclone.psyir.nodes.BinaryOperation`

        :returns: The C code as a string.
        :rtype: str

        :raises VisitorError: If this node has fewer children than expected.
        :raises NotImplementedError: If the operator is not supported by the \
            C backend.

        '''
        if len(node.children) != 2:
            raise VisitorError(
                f"BinaryOperation malformed or incomplete. It should "
                f"have exactly 2 children, but found {len(node.children)}.")

        def operator_format(operator_str, expr1, expr2):
            '''
            :param str operator_str: String representing the operator.
            :param str expr1: String representation of the LHS operand.
            :param str expr2: String representation of the RHS operand.

            :returns: C language operator expression.
            :rtype: str
            '''
            return "(" + expr1 + " " + operator_str + " " + expr2 + ")"

        def function_format(function_str, expr1, expr2):
            '''
            :param str function_str: Name of the function.
            :param str expr1: String representation of the first operand.
            :param str expr2: String representation of the second operand.

            :returns: C language binary function expression.
            :rtype: str
            '''
            return function_str + "(" + expr1 + ", " + expr2 + ")"

        # Define a map with the operator string and the formatter function
        # associated with each BinaryOperation.Operator
        opmap = {
            BinaryOperation.Operator.ADD: ("+", operator_format),
            BinaryOperation.Operator.SUB: ("-", operator_format),
            BinaryOperation.Operator.MUL: ("*", operator_format),
            BinaryOperation.Operator.DIV: ("/", operator_format),
            BinaryOperation.Operator.REM: ("%", operator_format),
            BinaryOperation.Operator.POW: ("pow", function_format),
            BinaryOperation.Operator.EQ: ("==", operator_format),
            BinaryOperation.Operator.NE: ("!=", operator_format),
            BinaryOperation.Operator.LT: ("<", operator_format),
            BinaryOperation.Operator.LE: ("<=", operator_format),
            BinaryOperation.Operator.GT: (">", operator_format),
            BinaryOperation.Operator.GE: (">=", operator_format),
            BinaryOperation.Operator.AND: ("&&", operator_format),
            BinaryOperation.Operator.OR: ("||", operator_format),
            BinaryOperation.Operator.SIGN: ("copysign", function_format),
            }

        # If the instance operator exists in the map, use its associated
        # operator and formatter to generate the code, otherwise raise
        # an Error.
        try:
            opstring, formatter = opmap[node.operator]
        except KeyError as err:
            raise VisitorError(
                f"The C backend does not support the '{node.operator}' "
                f"operator.") from err

        return formatter(opstring,
                         self._visit(node.children[0]),
                         self._visit(node.children[1]))

    def return_node(self, _):
        '''This method is called when a Return instance is found in
        the PSyIR tree.

        :param node: A Return PSyIR node.
        :type node: :py:class:`psyclone.psyir.nodes.Return`

        :returns: The C code as a string.
        :rtype: str

        '''
        return f"{self._nindent}return;\n"

    def codeblock_node(self, _):
        # pylint: disable=no-self-use
        '''This method is called when a CodeBlock instance is found in the
        PSyIR tree. At the moment all CodeBlocks contain Fortran fparser
        code.

        :raises VisitorError: The CodeBlock can not be translated to C.

        '''
        raise VisitorError("CodeBlocks can not be translated to C.")

    def loop_node(self, node):
        '''This method is called when a Loop instance is found in the
        PSyIR tree.

        :param node: a Loop PSyIR node.
        :type node: :py:class:`psyclone.psyir.nodes.Loop`

        :returns: the loop node converted into a (language specific) string.
        :rtype: str

        '''
        start = self._visit(node.start_expr)
        stop = self._visit(node.stop_expr)
        step = self._visit(node.step_expr)
        variable_name = node.variable.name

        self._depth += 1
        body = ""
        for child in node.loop_body:
            body += self._visit(child)
        self._depth -= 1

        return f"{self._nindent}for({variable_name}={start}; "\
               f"{variable_name}<={stop}; {variable_name}+={step})\n"\
               f"{self._nindent}{{\n{body}{self._nindent}}}\n"

    def regiondirective_node(self, node):
        '''This method is called when an RegionDirective instance is found in
        the PSyIR tree. It returns the opening and closing directives, and
        the statements in between as a string.

        :param node: a RegionDirective PSyIR node.
        :type node: :py:class:`psyclone.psyir.nodes.RegionDirective`

        :returns: the C code as a string.
        :rtype: str

        '''
        # Note that {{ is replaced with a single { in the format call
        result_list = [f"{self._nindent}#pragma {node.begin_string()}\n{{\n"]
        self._depth += 1
        for child in node.dir_body:
            result_list.append(self._visit(child))
        self._depth -= 1
        # Note that }} is replaced with a single } in the format call
        result_list.append(f"{self._nindent}}}\n")
        return "".join(result_list)

    def standalonedirective_node(self, node):
        '''This method is called when an StandaloneDirective instance is
        found in the PSyIR tree. It returns the opening and closing directives,
        and the statements in between as a string.

        :param node: a StandaloneDirective PSyIR node.
        :type node: :py:class:`psyclone.psyir.nodes.StandaloneDirective`

        :returns: the C code as a string.
        :rtype: str

        '''
        # pylint: disable=no-self-use
        result_list = [f"{self._nindent}#pragma {node.begin_string()}\n"]
        return "".join(result_list)

    def filecontainer_node(self, node):
        '''This method is called when a FileContainer instance is found in
        the PSyIR tree.

        :param node: a Container PSyIR node.
        :type node: :py:class:`psyclone.psyir.nodes.FileContainer`

        :returns: the C code.
        :rtype: str

        '''
        result = ""
        for child in node.children:
            result += self._visit(child)
        return result
