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
    def gokernelschedule(self, node):
        return self.kernelschedule(node)

    def assignment(self, node):
        lhs = self.visit(node.children[0])
        rhs = self.visit(node.children[1])
        result = "{0}={1}\n".format(lhs, rhs)
        return result

        
    def binaryoperation(self, node):
        lhs = self.visit(node.children[0])
        op = node._operator
        rhs = self.visit(node.children[1])
        result = "{0}{1}{2}".format(lhs, op, rhs)
        return result
        
    def reference(self, node):
        ''' OK '''
        result = node._reference
        for child in node.children:
            result += self.visit(child)
        return result

    def array(self, node):
        args = []
        for child in node.children:
            args.append(str(self.visit(child)))
        result = "{0}({1})".format(node.name, ",".join(args))
        return result

    def literal(self, node):
        result = node._value
        return result

    def ifblock(self, node):
        condition = self.visit(node.children[0])
        if_body = ""
        for child in node.if_body:
            if_body += self.visit(child)
        else_body = ""
        for child in node.else_body:
            else_body += self.visit(child)
        if else_body:
            result = ("if ({0}) then\n"
                      "{1}"
                      "else\n"
                      "{2}"
                      "end if\n").format(condition, if_body, else_body)
        else:
            result = ("if ({0}) then\n"
                      "{1}"
                      "end if\n").format(condition, if_body)
        return result

    def unaryoperation(self, node):
        content = self.visit(node.children[0])
        result = "{0}{1}".format(node._operator, content)
        return result

    def return_node(self, node):
        return "return\n"
        
    def kernelschedule(self, node):
        '''This method is called when a KernelSchedule instance is found in
        the PSyIR tree. This method is called before any children of
        this node in the PSyIR tree are visited (and therefore before
        any other methods (associated with the PSyIR children) in this
        class are called.

        The constants_mod module is currently hardcoded into the
        output as it is required for LFRic code. When issue #375 has
        been addressed this module can be added only when required.

        :param node: a KernelSchedule PSyIR node.
        :type node: :py:class:`psyclone.psyGen.KernelSchedule`

        '''
        # Add the start of the kernel module and subroutine
        args = [symbol.name for symbol in node.symbol_table.argument_list]
        self._code += (
            "module {0}\n"
            "  use constants_mod, only : r_def, i_def\n"
            "  implicit none\n"
            "  contains\n"
            "  subroutine {1}({2})\n"
            "".format(node.name+"_mod", node.name, ",".join(args)))

        # Declare the kernel data.
        for symbol in node.symbol_table._symbols.values():
            intent = get_intent(symbol)
            dims = get_dims(symbol)
            kind = get_kind(symbol)
            self._code += "    {0}".format(symbol.datatype)
            if kind:
                self._code += "({0})".format(kind)
            if dims:
                self._code += ", dimension({0})".format(",".join(dims))
            if intent:
                self._code += ", intent({0})".format(intent)
            self._code += " :: {0}\n".format(symbol.name)

        for child in node.children:
            result = self.visit(child)
            self._code += result

        self._code += (
            "  end subroutine {0}\n"
            "end module {1}\n".format(node.name, node.name+"_mod"))

        return self._code


    def codeblock(self, node):
        '''This method is called when a CodeBlock instance is found in the
        PSyIR tree. This method is called before any children of this
        node in the PSyIR tree are visited. However, note that code
        blocks do not have children.

        Adds the content of the CodeBlock to the output code.

        :param node: a KernelSchedule PSyIR node.
        :type node: :py:class:`psyclone.psyGen.CodeBlock`

        '''
        block_str = '    '
        block_str += '    '.join(str(node.ast).splitlines(True))
        return block_str
