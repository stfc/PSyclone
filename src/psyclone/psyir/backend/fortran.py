# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2019, Science and Technology Facilities Council
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
# Author R. W. Ford, STFC Daresbury Lab.

'''Fortran PSyIR backend. Generates Fortran code from PSyIR
nodes. Currently limited to PSyIR Kernel schedules as PSy-layer PSyIR
already has a gen() method to generate Fortran.

'''

from psyclone.psyir.backend.base import PSyIRVisitor, VisitorError
from psyclone.psyGen import FORTRAN_INTRINSICS


def gen_intent(symbol):
    '''Given a Symbol instance as input, determine the Fortran intent that
    the Symbol should have and return the value as a string.

    :param symbol: The symbol instance.
    :type symbol: :py:class:`psyclone.psyGen.Symbol`

    :returns: the Fortran intent of the symbol instance in lower case, \
    or None if the access is unknown or if this is a local variable.
    :rtype: str or NoneType

    '''
    from psyclone.psyGen import Symbol

    mapping = {Symbol.Access.UNKNOWN: None,
               Symbol.Access.READ: "in",
               Symbol.Access.WRITE: "out",
               Symbol.Access.READWRITE: "inout"}
    if not symbol.interface:
        # This is a local variable
        return None
    try:
        return mapping[symbol.interface.access]
    except KeyError as excinfo:
        raise VisitorError("Unsupported access '{0}' found."
                           "".format(str(excinfo)))


def gen_dims(symbol):
    '''Given a Symbol instance as input, return a list of strings
    representing the symbol's array dimensions.

    :param symbol: The symbol instance.
    :type symbol: :py:class:`psyclone.psyGen.Symbol`

    :returns: the Fortran representation of the symbol's dimensions as \
    a list.
    :rtype: list of str

    :raises NotImplementedError: if the format of the dimension is not \
    supported.

    '''
    from psyclone.psyGen import Symbol

    dims = []
    for index in symbol.shape:
        if isinstance(index, Symbol):
            # references another symbol
            dims.append(index.name)
        elif isinstance(index, int):
            # literal constant
            dims.append(str(index))
        elif index is None:
            dims.append(":")
        else:
            raise NotImplementedError(
                "unsupported gen_dims index '{0}'".format(str(index)))
    return dims


def gen_kind(symbol):
    '''Infer the expected Fortran kind value from the Symbol
    instance. This is a temporary LFRic-specific hack which simply
    adds a hardcoded kind value for real variables and a hardcoded
    kind value for integer variables. To work correctly in general the
    symbol table needs some additional information added to it, see
    issue #375.

    :param symbol: The symbol instance.
    :type symbol: :py:class:`psyclone.psyGen.Symbol`

    :returns: the Fortran kind value for the symbol instance in lower \
    case, or None if no kind value is required.
    :rtype: str or NoneType

    '''
    kind = None
    if symbol.datatype == "real":
        kind = "r_def"
    elif symbol.datatype == "integer":
        kind = "i_def"
    return kind


def _reverse_map(op_map):
    '''
    Reverses the supplied fortran2psyir mapping to make a psyir2fortran
    mapping.

    :param op_map: mapping from string representation of operator to \
                   enumerated type.
    :type op_map: :py:class:`collections.OrderedDict`

    :returns: a mapping from PSyIR operation to the equivalent Fortran string.
    :rtype: dict with :py:class:`psyclone.psyGen.Operation.Operator` keys and
            str values.

    '''
    mapping = {}
    for operator in op_map:
        mapping_key = op_map[operator]
        mapping_value = operator
        # Only choose the first mapping value when there is more
        # than one.
        if mapping_key not in mapping:
            mapping[mapping_key] = mapping_value
    return mapping


class FortranWriter(PSyIRVisitor):
    '''Implements a PSyIR-to-Fortran back end for PSyIR kernel code (not
    currently PSyIR algorithm code which has its own gen method for
    generating Fortran).

    '''
    def gen_declaration(self, symbol):
        '''Create and return the Fortran declaration for this Symbol.

        :param symbol: The symbol instance.
        :type symbol: :py:class:`psyclone.psyGen.Symbol`
        :returns: The Fortran declaration as a string.
        :rtype: str

        '''
        intent = gen_intent(symbol)
        dims = gen_dims(symbol)
        kind = gen_kind(symbol)
        result = "{0}{1}".format(self._nindent, symbol.datatype)
        if kind:
            result += "({0})".format(kind)
        if dims:
            result += ", dimension({0})".format(",".join(dims))
        if intent:
            result += ", intent({0})".format(intent)
        if symbol.is_constant:
            result += ", parameter"
        result += " :: {0}".format(symbol.name)
        if symbol.is_constant:
            result += " = {0}".format(symbol.constant_value)
        result += "\n"
        return result

    def nemokern_node(self, node):
        '''NEMO kernels are a group of nodes collected into a schedule
        so simply call the nodes in the schedule.

        :param node: A NemoKern PSyIR node.
        :type node: :py:class:`psyclone.nemo.NemoKern`

        :returns: The Fortran code as a string.
        :rtype: str

        '''
        result = ""
        schedule = node.get_kernel_schedule()
        for child in schedule.children:
            result += self._visit(child)
        return result

    def kernelschedule_node(self, node):
        '''This method is called when a KernelSchedule instance is found in
        the PSyIR tree.

        The constants_mod module is currently hardcoded into the
        output as it is required for LFRic code. When issue #375 has
        been addressed this module can be added only when required.

        :param node: A KernelSchedule PSyIR node.
        :type node: :py:class:`psyclone.psyGen.KernelSchedule`

        :returns: The Fortran code as a string.
        :rtype: str

        '''
        if not node.name:
            raise VisitorError("Expected node name to have a value.")

        module_name = node.name.rstrip("_code") + "_mod"
        result = (
            "{0}module {1}\n"
            "".format(self._nindent, module_name))

        self._depth += 1
        args = [symbol.name for symbol in node.symbol_table.argument_list]
        result += (
            "{0}use constants_mod, only : r_def, i_def\n"
            "{0}implicit none\n"
            "{0}contains\n"
            "{0}subroutine {1}({2})\n"
            "".format(self._nindent, node.name, ",".join(args)))

        self._depth += 1
        # Declare the kernel data.
        declarations = ""
        for symbol in node.symbol_table.symbols:
            declarations += self.gen_declaration(symbol)
        # Get the executable statements.
        exec_statements = ""
        for child in node.children:
            exec_statements += self._visit(child)
        result += (
            "{0}\n"
            "{1}\n"
            "".format(declarations, exec_statements))

        self._depth -= 1
        result += (
            "{0}end subroutine {1}\n"
            "".format(self._nindent, node.name))

        self._depth -= 1
        result += (
            "{0}end module {1}\n"
            "".format(self._nindent, node.name+"_mod"))
        return result

    def assignment_node(self, node):
        '''This method is called when an Assignment instance is found in the
        PSyIR tree.

        :param node: An Assignment PSyIR node.
        :type node: :py:class:`psyclone.psyGen.Assigment`

        :returns: The Fortran code as a string.
        :rtype: str

        '''
        lhs = self._visit(node.children[0])
        rhs = self._visit(node.children[1])
        result = "{0}{1}={2}\n".format(self._nindent, lhs, rhs)
        return result

    def binaryoperation_node(self, node):
        '''This method is called when a BinaryOperation instance is found in
        the PSyIR tree.

        :param node: A BinaryOperation PSyIR node.
        :type node: :py:class:`psyclone.psyGen.BinaryOperation`

        :returns: The Fortran code as a string.
        :rtype: str

        '''
        # reverse the fortran2psyir mapping to make a psyir2fortran
        # mapping
        from psyclone.psyGen import Fparser2ASTProcessor as f2psyir
        mapping = _reverse_map(f2psyir.binary_operators)
        lhs = self._visit(node.children[0])
        rhs = self._visit(node.children[1])
        try:
            oper = mapping[node.operator]
            # This is a binary operation
            if oper.upper() in FORTRAN_INTRINSICS:
                # This is a binary intrinsic function.
                return "{0}({1}, {2})".format(oper.upper(), lhs, rhs)
            return "{0} {1} {2}".format(lhs, oper, rhs)
        except KeyError:
            raise VisitorError("Unexpected binary op '{0}'."
                               "".format(node.operator))

    def naryoperation_node(self, node):
        '''This method is called when an NaryOperation instance is found in
        the PSyIR tree.

        :param node: An NaryOperation PSyIR node.
        :type node: :py:class:`psyclone.psyGen.NaryOperation`

        :returns: The Fortran code as a string.
        :rtype: str

        :raises VisitorError: if an unexpected N-ary operator is found.

        '''
        # Reverse the fortran2psyir mapping to make a psyir2fortran
        # mapping.
        from psyclone.psyGen import Fparser2ASTProcessor as f2psyir
        mapping = _reverse_map(f2psyir.nary_operators)
        arg_list = []
        for child in node.children:
            arg_list.append(self._visit(child))
        try:
            oper = mapping[node.operator]
            return "{0}({1})".format(oper.upper(), ", ".join(arg_list))
        except KeyError:
            raise VisitorError("Unexpected N-ary op '{0}'".
                               format(node.operator))

    def reference_node(self, node):
        '''This method is called when a Reference instance is found in the
        PSyIR tree.

        :param node: A Reference PSyIR node.
        :type node: :py:class:`psyclone.psyGen.Reference`

        :returns: The Fortran code as a string.
        :rtype: str

        :raises VisitorError: If this node has children.

        '''
        if node.children:
            raise VisitorError(
                "PSyIR Reference node should not have any children.")
        return node.name

    def array_node(self, node):
        '''This method is called when an Array instance is found in the PSyIR
        tree.

        :param node: An Array PSyIR node.
        :type node: :py:class:`psyclone.psyGen.Array`

        :returns: The Fortran code as a string.
        :rtype: str

        '''
        args = []
        for child in node.children:
            args.append(str(self._visit(child)))
        result = "{0}({1})".format(node.name, ",".join(args))
        return result

    def literal_node(self, node):
        '''This method is called when a Literal instance is found in the PSyIR
        tree.

        :param node: A Literal PSyIR node.
        :type node: :py:class:`psyclone.psyGen.Literal`

        :returns: The Fortran code as a string.
        :rtype: str

        '''
        result = node.value
        return result

    def ifblock_node(self, node):
        '''This method is called when an IfBlock instance is found in the
        PSyIR tree.

        :param node: An IfBlock PSyIR node.
        :type node: :py:class:`psyclone.psyGen.IfBlock`

        :returns: The Fortran code as a string.
        :rtype: str

        '''
        condition = self._visit(node.children[0])

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
                "{0}if ({1}) then\n"
                "{2}"
                "{0}else\n"
                "{3}"
                "{0}end if\n"
                "".format(self._nindent, condition, if_body, else_body))
        else:
            result = (
                "{0}if ({1}) then\n"
                "{2}"
                "{0}end if\n"
                "".format(self._nindent, condition, if_body))
        return result

    def unaryoperation_node(self, node):
        '''This method is called when a UnaryOperation instance is found in
        the PSyIR tree.

        :param node: A UnaryOperation PSyIR node.
        :type node: :py:class:`psyclone.psyGen.UnaryOperation`

        :returns: The Fortran code as a string.
        :rtype: str

        :raises VisitorError: if an unexpected Unary op is encountered.

        '''
        # Reverse the fortran2psyir mapping to make a psyir2fortran
        # mapping.
        from psyclone.psyGen import Fparser2ASTProcessor as f2psyir
        mapping = _reverse_map(f2psyir.unary_operators)

        content = self._visit(node.children[0])
        try:
            oper = mapping[node.operator]
            # This is a unary operation
            if oper.upper() in FORTRAN_INTRINSICS:
                # This is a unary intrinsic function.
                return "{0}({1})".format(oper.upper(), content)
            return "{0}{1}".format(oper, content)
        except KeyError:
            raise VisitorError("Unexpected unary op '{0}'.".format(
                node.operator))

    def return_node(self, _):
        '''This method is called when a Return instance is found in
        the PSyIR tree.

        Notice the name of the method is `return_node`, not `return`. This
        is done by the base class to avoid a name clash with the
        Python `return` keyword.

        :param node: A Return PSyIR node.
        :type node: :py:class:`psyclone.psyGen.Return`

        :returns: The Fortran code as a string.
        :rtype: str

        '''
        return "{0}return\n".format(self._nindent)

    def codeblock_node(self, node):
        '''This method is called when a CodeBlock instance is found in the
        PSyIR tree. It returns the content of the CodeBlock as a
        Fortran string, indenting as appropriate.

        :param node: A CodeBlock PSyIR node.
        :type node: :py:class:`psyclone.psyGen.CodeBlock`

        :returns: The Fortran code as a string.
        :rtype: str

        '''
        return self._nindent.join(str(node.ast).splitlines(True))
