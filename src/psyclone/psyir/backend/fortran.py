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
# Authors R. W. Ford, S. Siso STFC Daresbury Lab.
# Modified J. Henrichs, Bureau of Meteorology

'''Fortran PSyIR backend. Generates Fortran code from PSyIR
nodes. Currently limited to PSyIR Kernel and NemoInvoke schedules as
PSy-layer PSyIR already has a gen() method to generate Fortran.

'''

from psyclone.psyir.frontend.fparser2 import Fparser2Reader
from psyclone.psyir.backend.visitor import PSyIRVisitor, VisitorError
from psyclone.psyGen import Symbol
from fparser.two import Fortran2003

# The list of Fortran instrinsic functions that we know about (and can
# therefore distinguish from array accesses). These are taken from
# fparser.
FORTRAN_INTRINSICS = Fortran2003.Intrinsic_Name.function_names


def gen_intent(symbol):
    '''Given a Symbol instance as input, determine the Fortran intent that
    the Symbol should have and return the value as a string.

    :param symbol: the symbol instance.
    :type symbol: :py:class:`psyclone.psyGen.Symbol`

    :returns: the Fortran intent of the symbol instance in lower case, \
    or None if the access is unknown or if this is a local variable.
    :rtype: str or NoneType

    '''
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

    :param symbol: the symbol instance.
    :type symbol: :py:class:`psyclone.psyGen.Symbol`

    :returns: the Fortran representation of the symbol's dimensions as \
    a list.
    :rtype: list of str

    :raises NotImplementedError: if the format of the dimension is not \
    supported.

    '''

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
    def gen_use(self, symbol):
        '''Create and return the Fortran use statement for this Symbol.

        :param symbol: the symbol instance.
        :type symbol: :py:class:`psyclone.psyGen.Symbol`

        :returns: the Fortran use statement as a string.
        :rtype: str

        :raises VisitorError: if the symbol argument does not specify \
        a use statement (its interface value is not a FortranGlobal \
        instance).

        '''
        if not isinstance(symbol.interface, Symbol.FortranGlobal):
            raise VisitorError(
                "gen_use() requires the symbol interface for symbol '{0}' to "
                "be a FortranGlobal instance but found '{1}'."
                "".format(symbol.name, type(symbol.interface).__name__))

        return "{0}use {1}, only : {2}\n".format(
            self._nindent, symbol.interface.module_name, symbol.name)

    def gen_vardecl(self, symbol):
        '''Create and return the Fortran variable declaration for this Symbol.

        :param symbol: the symbol instance.
        :type symbol: :py:class:`psyclone.psyGen.Symbol`

        :returns: the Fortran variable declaration as a string.
        :rtype: str

        :raises VisitorError: if the symbol does not specify a \
        variable declaration (it is not a local declaration or an \
        argument declaration).

        '''
        if not symbol.scope == "local" and not isinstance(symbol.interface,
                                                          Symbol.Argument):
            raise VisitorError(
                "gen_vardecl requires the symbol '{0}' to be a local "
                "declaration or an argument declaration, but found scope "
                "'{1}' and interface '{2}'."
                "".format(symbol.name, symbol.scope,
                          type(symbol.interface).__name__))

        intent = gen_intent(symbol)
        dims = gen_dims(symbol)
        result = "{0}{1}".format(self._nindent, symbol.datatype)
        # The PSyIR does not currently capture kind information, see
        # issue #375
        # kind = ...
        # if kind:
        #     result += "({0})".format(kind)
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

    def gen_decls(self, symbol_table, args_allowed=True):
        '''Create and return the Fortran declarations for the supplied
        SymbolTable.

        :param symbol_table: the SymbolTable instance.
        :type symbol: :py:class:`psyclone.psyGen.SymbolTable`
        :param bool args_allowed: if False then one or more argument
        declarations in symbol_table will cause this method to raise
        an exception. Defaults to True.

        :returns: the Fortran declarations as a string.
        :rtype: str

        :raises VisitorError: if args_allowed is False and one or more \
        argument declarations exist in symbol_table.

        '''
        declarations = ""
        # Fortran requires use statements to be specified before
        # variable declarations. As a convention, this method also
        # declares any argument variables before local variables.

        # 1: Use statements
        for symbol in [sym for sym in symbol_table.symbols if
                       isinstance(sym.interface, Symbol.FortranGlobal)]:
            declarations += self.gen_use(symbol)
        # 2: Argument variable declarations
        symbols = [sym for sym in symbol_table.symbols if
                   isinstance(sym.interface, Symbol.Argument)]
        if symbols and not args_allowed:
            raise VisitorError(
                "Arguments are not allowed in this context but this symbol "
                "table contains argument(s): '{0}'."
                "".format([symbol.name for symbol in symbols]))
        for symbol in symbols:
            declarations += self.gen_vardecl(symbol)
        # 3: Local variable declarations
        for symbol in [sym for sym in symbol_table.symbols if
                       sym.scope == "local"]:
            declarations += self.gen_vardecl(symbol)
        return declarations

    def container_node(self, node):
        '''This method is called when a Container instance is found in
        the PSyIR tree.

        A container node is mapped to a module in the Fortran back end.

        :param node: a Container PSyIR node.
        :type node: :py:class:`psyclone.psyGen.Container`

        :returns: the Fortran code as a string.
        :rtype: str

        :raises VisitorError: if the name attribute of the supplied \
        node is empty or None.
        :raises VisitorError: if any of the children of the supplied \
        Container node are not KernelSchedules.

        '''
        if not node.name:
            raise VisitorError("Expected Container node name to have a value.")

        # All children must be KernelSchedules as modules within
        # modules are not supported.
        from psyclone.psyGen import KernelSchedule
        if not all([isinstance(child, KernelSchedule)
                    for child in node.children]):
            raise VisitorError(
                "The Fortran back-end requires all children of a Container "
                "to be KernelSchedules.")

        result = "{0}module {1}\n".format(self._nindent, node.name)

        self._depth += 1

        # Declare the Container's data and specify that Containers do
        # not allow argument declarations.
        declarations = self.gen_decls(node.symbol_table, args_allowed=False)

        # Get the subroutine statements.
        subroutines = ""
        for child in node.children:
            subroutines += self._visit(child)

        result += (
            "{1}\n"
            "{0}contains\n"
            "{2}\n"
            "".format(self._nindent, declarations, subroutines))

        self._depth -= 1
        result += "{0}end module {1}\n".format(self._nindent, node.name)
        return result

    def kernelschedule_node(self, node):
        '''This method is called when a KernelSchedule instance is found in
        the PSyIR tree.

        The constants_mod module is currently hardcoded into the
        output as it is required for LFRic code. When issue #375 has
        been addressed this module can be added only when required.

        :param node: a KernelSchedule PSyIR node.
        :type node: :py:class:`psyclone.psyGen.KernelSchedule`

        :returns: the Fortran code as a string.
        :rtype: str

        :raises VisitorError: if the name attribute of the supplied \
        node is empty or None.

        '''
        if not node.name:
            raise VisitorError("Expected node name to have a value.")

        args = [symbol.name for symbol in node.symbol_table.argument_list]
        result = (
            "{0}subroutine {1}({2})\n"
            "".format(self._nindent, node.name, ",".join(args)))

        self._depth += 1
        # Declare the kernel data.
        declarations = self.gen_decls(node.symbol_table)
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

        return result

    def assignment_node(self, node):
        '''This method is called when an Assignment instance is found in the
        PSyIR tree.

        :param node: an Assignment PSyIR node.
        :type node: :py:class:`psyclone.psyGen.Assigment`

        :returns: the Fortran code as a string.
        :rtype: str

        '''
        lhs = self._visit(node.lhs)
        rhs = self._visit(node.rhs)
        result = "{0}{1}={2}\n".format(self._nindent, lhs, rhs)
        return result

    def binaryoperation_node(self, node):
        '''This method is called when a BinaryOperation instance is found in
        the PSyIR tree.

        :param node: a BinaryOperation PSyIR node.
        :type node: :py:class:`psyclone.psyGen.BinaryOperation`

        :returns: the Fortran code as a string.
        :rtype: str

        '''
        # reverse the fortran2psyir mapping to make a psyir2fortran
        # mapping
        mapping = _reverse_map(Fparser2Reader.binary_operators)
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

        :param node: an NaryOperation PSyIR node.
        :type node: :py:class:`psyclone.psyGen.NaryOperation`

        :returns: the Fortran code as a string.
        :rtype: str

        :raises VisitorError: if an unexpected N-ary operator is found.

        '''
        # Reverse the fortran2psyir mapping to make a psyir2fortran
        # mapping.
        mapping = _reverse_map(Fparser2Reader.nary_operators)
        arg_list = []
        for child in node.children:
            arg_list.append(self._visit(child))
        try:
            oper = mapping[node.operator]
            return "{0}({1})".format(oper.upper(), ", ".join(arg_list))
        except KeyError:
            raise VisitorError("Unexpected N-ary op '{0}'".
                               format(node.operator))

    def array_node(self, node):
        '''This method is called when an Array instance is found in the PSyIR
        tree.

        :param node: an Array PSyIR node.
        :type node: :py:class:`psyclone.psyGen.Array`

        :returns: the Fortran code as a string.
        :rtype: str

        '''
        args = []
        for child in node.children:
            args.append(str(self._visit(child)))
        result = "{0}({1})".format(node.name, ",".join(args))
        return result

    # pylint: disable=no-self-use
    def literal_node(self, node):
        '''This method is called when a Literal instance is found in the PSyIR
        tree.

        :param node: a Literal PSyIR node.
        :type node: :py:class:`psyclone.psyGen.Literal`

        :returns: the Fortran code as a string.
        :rtype: str

        '''
        result = node.value
        return result

    # pylint: enable=no-self-use
    def ifblock_node(self, node):
        '''This method is called when an IfBlock instance is found in the
        PSyIR tree.

        :param node: an IfBlock PSyIR node.
        :type node: :py:class:`psyclone.psyGen.IfBlock`

        :returns: the Fortran code as a string.
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

    def loop_node(self, node):
        '''This method is called when a Loop instance is found in the
        PSyIR tree.

        :param node: a Loop PSyIR node.
        :type node: :py:class:`psyclone.psyGen.Loop`

        :returns: the loop node converted into a (language specific) string.
        :rtype: str

        '''
        start = self._visit(node.start_expr)
        stop = self._visit(node.stop_expr)
        step = self._visit(node.step_expr)
        variable_name = node.variable_name

        self._depth += 1
        body = ""
        for child in node.loop_body:
            body += self._visit(child)
        self._depth -= 1

        return "{0}do {1} = {2}, {3}, {4}\n"\
               "{5}"\
               "{0}enddo\n".format(self._nindent, variable_name,
                                   start, stop, step, body)

    def unaryoperation_node(self, node):
        '''This method is called when a UnaryOperation instance is found in
        the PSyIR tree.

        :param node: a UnaryOperation PSyIR node.
        :type node: :py:class:`psyclone.psyGen.UnaryOperation`

        :returns: the Fortran code as a string.
        :rtype: str

        :raises VisitorError: if an unexpected Unary op is encountered.

        '''
        # Reverse the fortran2psyir mapping to make a psyir2fortran
        # mapping.
        mapping = _reverse_map(Fparser2Reader.unary_operators)

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

        :param node: a Return PSyIR node.
        :type node: :py:class:`psyclone.psyGen.Return`

        :returns: the Fortran code as a string.
        :rtype: str

        '''
        return "{0}return\n".format(self._nindent)

    def codeblock_node(self, node):
        '''This method is called when a CodeBlock instance is found in the
        PSyIR tree. It returns the content of the CodeBlock as a
        Fortran string, indenting as appropriate.

        At the moment it is not possible to distinguish between a
        codeblock that is one or more full lines (and therefore needs
        a newline added) and a codeblock that is part of a line (and
        therefore does not need a newline). The current implementation
        adds a newline irrespective. This is the subject of issue
        #388.

        :param node: a CodeBlock PSyIR node.
        :type node: :py:class:`psyclone.psyGen.CodeBlock`

        :returns: the Fortran code as a string.
        :rtype: str

        '''
        from psyclone.psyGen import CodeBlock
        result = ""
        if node.structure == CodeBlock.Structure.STATEMENT:
            # indent and newlines required
            for ast_node in node.get_ast_nodes:
                result += "{0}{1}\n".format(self._nindent, str(ast_node))
        elif node.structure == CodeBlock.Structure.EXPRESSION:
            for ast_node in node.get_ast_nodes:
                result += str(ast_node)
        else:
            raise VisitorError(
                ("Unsupported CodeBlock Structure '{0}' found."
                 "".format(node.structure)))
        return result

    def nemoinvokeschedule_node(self, node):
        '''A NEMO invoke schedule is the top level node in a PSyIR
        representation of a NEMO program unit (program, subroutine
        etc). It does not represent any code itself so all it needs to
        to is call its children and return the result.

        :param node: a NemoInvokeSchedule PSyIR node.
        :type node: :py:class:`psyclone.nemo.NemoInvokeSchedule`

        :returns: the Fortran code as a string.
        :rtype: str

        '''
        result = ""
        for child in node.children:
            result += self._visit(child)
        return result

    def nemokern_node(self, node):
        '''NEMO kernels are a group of nodes collected into a schedule
        so simply call the nodes in the schedule.

        :param node: a NemoKern PSyIR node.
        :type node: :py:class:`psyclone.nemo.NemoKern`

        :returns: the Fortran code as a string.
        :rtype: str

        '''
        result = ""
        schedule = node.get_kernel_schedule()
        for child in schedule.children:
            result += self._visit(child)
        return result

    def nemoimplicitloop_node(self, node):
        '''Fortran implicit loops are currently captured in the PSyIR as a
        NemoImplicitLoop node. This is a temporary solution while the
        best way to capture their behaviour is decided. This method
        outputs the Fortran representation of such a loop by simply
        using the original Fortran ast (i.e. acting in a similar way
        to a code block). As it is a temporary solution we don't
        bother fixing the _ast internal access.

        :param node: a NemoImplicitLoop PSyIR node.
        :type node: :py:class:`psyclone.psyGen.NemoImplicitLoop`

        :returns: the Fortran code as a string.
        :rtype: str

        '''
        return "{0}{1}\n".format(self._nindent, str(node.ast))

    def ompdirective_node(self, node):
        '''This method is called when an OMPDirective instance is found in
        the PSyIR tree. It returns the opening and closing directives, and
        the statements in between as a string (depending on the language).

        :param node: a Directive PSyIR node.
        :type node: :py:class:`psyclone.psyGen.Directive`

        :returns: the Fortran code as a string.
        :rtype: str

        '''
        result_list = ["!${0}\n".format(node.begin_string())]
        self._depth += 1
        for child in node.dir_body:
            result_list.append(self._visit(child))
        self._depth -= 1
        result_list.append("!${0}\n".format(node.end_string()))
        return "".join(result_list)
