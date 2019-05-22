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

from psyclone.psyir.backend.base import PSyIRVisitor


def get_intent(symbol):
    '''Given a Symbol instance as input, determine the Fortran intent that
    the Symbol should have and return the value as a string.

    :param symbol: The symbol instance.
    :type symbol: :py:class:`psyclone.psyGen.Symbol`

    :returns: the Fortran intent of the symbol instance in lower case, \
    or None if this is a local variable.
    :rtype: str or NoneType

    '''
    intent = None
    if symbol.is_input and symbol.is_output:
        intent = "inout"
    elif symbol.is_input:
        intent = "in"
    elif symbol.is_output:
        intent = "out"
    return intent


def get_dims(symbol):
    '''Given a Symbol instance as input, return a list of strings
    representing the symbols array dimensions.

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
                "unsupported get_dims index '{0}'".format(str(index)))
    return dims


def get_kind(symbol):
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


class FortranPSyIRVisitor(PSyIRVisitor):
    '''Implements a PSyIR-to-Fortran back end for PSyIR kernel code (not
    currently PSyIR algorithm code which has its own gen method for
    generating Fortran). Specialises the PSyIRVisitor class so only
    needs to implement the start and end methods for the PSyIR
    nodes.

    '''
    def get_declaration(self, symbol):
        '''Create and return the Fortran declaration for this Symbol.

        :param symbol: The symbol instance.
        :type symbol: :py:class:`psyclone.psyGen.Symbol`
        :returns: the Fortran declaration as a string.
        :rtype: str

        '''
        intent = get_intent(symbol)
        dims = get_dims(symbol)
        kind = get_kind(symbol)
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

    def node(self, node):
        '''Catch any unsupported nodes, output their class names and continue
        down the node hierarchy.

        :param node: an unsupported PSyIR node.
        :type node: subclass of :py:class:`psyclone.psyGen.Node`

        :returns: the Fortran code as a string.
        :rtype: str

        '''
        result = "{0}[ {1} start ]\n".format(self._nindent, type(node).__name__)
        self._depth += 1
        for child in node.children:
            result += self.visit(child)
        self._depth -= 1
        result += "{0}[ {1} end ]\n".format(self._nindent, type(node).__name__)
        return result

    def nemokern(self, node):
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
            result += self.visit(child)
        return result

    def kernelschedule(self, node):
        '''This method is called when a KernelSchedule instance is found in
        the PSyIR tree.

        The constants_mod module is currently hardcoded into the
        output as it is required for LFRic code. When issue #375 has
        been addressed this module can be added only when required.

        :param node: a KernelSchedule PSyIR node.
        :type node: :py:class:`psyclone.psyGen.KernelSchedule`

        :returns: the Fortran code as a string.
        :rtype: str

        '''
        result = (
            "{0}module {1}\n"
            "".format(self._nindent, node.name+"_mod"))

        self._depth += 1
        args = [symbol.name for symbol in node.symbol_table.argument_list]
        result += (
            "{0}use constants_mod, only : r_def, i_def\n"
            "{0}implicit none\n"
            "{0}contains\n"
            "{0}subroutine {1}({2})\n\n"
            "".format(self._nindent, node.name, ",".join(args)))

        self._depth += 1
        # Declare the kernel data.
        declarations = ""
        for symbol in node.symbol_table._symbols.values():
            declarations += self.get_declaration(symbol)
        # Get the executable statements.
        exec_statements = ""
        for child in node.children:
            exec_statements += self.visit(child)
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

    def assignment(self, node):
        '''This method is called when an Assignment instance is found in the
        PSyIR tree.

        :param node: an Assignment PSyIR node.
        :type node: :py:class:`psyclone.psyGen.Assigment`

        :returns: the Fortran code as a string.
        :rtype: str

        '''
        lhs = self.visit(node.children[0])
        rhs = self.visit(node.children[1])
        result = "{0}{1}={2}\n".format(self._nindent, lhs, rhs)
        return result

    def binaryoperation(self, node):
        '''This method is called when a BinaryOperation instance is found in
        the PSyIR tree.

        :param node: a BinaryOperation PSyIR node.
        :type node: :py:class:`psyclone.psyGen.BinaryOperation`

        :returns: the Fortran code as a string.
        :rtype: str

        '''
        lhs = self.visit(node.children[0])
        oper = node._operator
        rhs = self.visit(node.children[1])
        result = "{0}{1}{2}".format(lhs, oper, rhs)
        return result

    def reference(self, node):
        '''This method is called when a Reference instance is found in the
        PSyIR tree.

        :param node: a Reference PSyIR node.
        :type node: :py:class:`psyclone.psyGen.Reference`

        :returns: the Fortran code as a string.
        :rtype: str

        '''
        result = node._reference
        for child in node.children:
            result += self.visit(child)
        return result

    def array(self, node):
        '''This method is called when an Array instance is found in the PSyIR
        tree.

        :param node: an Array PSyIR node.
        :type node: :py:class:`psyclone.psyGen.Array`

        :returns: the Fortran code as a string.
        :rtype: str

        '''
        args = []
        for child in node.children:
            args.append(str(self.visit(child)))
        result = "{0}({1})".format(node.name, ",".join(args))
        return result

    def literal(self, node):
        '''This method is called when a Literal instance is found in the PSyIR
        tree.

        :param node: a Literal PSyIR node.
        :type node: :py:class:`psyclone.psyGen.Literal`

        :returns: the Fortran code as a string.
        :rtype: str

        '''
        result = node._value
        return result

    def ifblock(self, node):
        '''This method is called when an IfBlock instance is found in the
        PSyIR tree.

        :param node: an IfBlock PSyIR node.
        :type node: :py:class:`psyclone.psyGen.IfBlock`

        :returns: the Fortran code as a string.
        :rtype: str

        '''
        condition = self.visit(node.children[0])

        self._depth += 1
        if_body = ""
        for child in node.if_body:
            if_body += self.visit(child)
        else_body = ""
        for child in node.else_body:
            else_body += self.visit(child)
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

    def unaryoperation(self, node):
        '''This method is called when a UnaryOperation instance is found in
        the PSyIR tree.

        :param node: a UnaryOperation PSyIR node.
        :type node: :py:class:`psyclone.psyGen.UnaryOperation`

        :returns: the Fortran code as a string.
        :rtype: str

        '''
        content = self.visit(node.children[0])
        result = "{0}{1}".format(node._operator, content)
        return result

    def return_node(self, _):
        '''This method is called when a Return instance is found in
        the PSyIR tree.

        Notice the name of the method is `return_node`, not `return`. This
        is done by the base class to avoid a name clash with the
        Python `return` keyword.

        :param node: a Return PSyIR node.
        :type node: :py:class:`psyclone.psyGen.Return`

        :returns: the Fortran code as a string.
        :rtype: str

        '''
        return "{0}return\n".format(self._nindent)

    def codeblock(self, node):
        '''This method is called when a CodeBlock instance is found in the
        PSyIR tree. It returns the content of the CodeBlock as a
        Fortran string.

        :param node: a CodeBlock PSyIR node.
        :type node: :py:class:`psyclone.psyGen.CodeBlock`

        :returns: the Fortran code as a string.
        :rtype: str

        '''
        return self._nindent.join(str(node.ast).splitlines(True))
