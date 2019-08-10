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

'''SIR PSyIR backend. Generates SIR code from PSyIR nodes. Currently
limited to PSyIR Kernel schedules as PSy-layer PSyIR already has a
gen() method to generate Fortran.

'''

from psyclone.psyir.backend.base import PSyIRVisitor, VisitorError
from psyclone.psyGen import Reference, BinaryOperation, Literal, \
    Fparser2ASTProcessor as f2psyir, Array
from psyclone.nemo import NemoLoop, NemoKern


def gen_stencil(node):
    '''Given an array access as input, determine the form of stencil
    access and return it in the form expected by the SIR as a
    string. Raise an exception if the array access is not a recognised
    stencil access.

    :param node: an array access.
    :type node: :py:class:`psyclone.psyGen.Array`

    :returns: the SIR stencil access format for the array access.
    :rtype: str

    :raises VisitorError: if the node is not the expected type or the \
    array access is not in a recognised stencil form.

    '''
    if not isinstance(node, Array):
        raise VisitorError(
            "gen_stencil expected an Array as input but found '{0}'."
            "".format(type(node)))
    dims = []
    for child in node.children:
        if isinstance(child, Reference):
            dims.append("0")
        elif isinstance(child, BinaryOperation):
            if isinstance(child.children[0], Reference) and \
               isinstance(child.children[1], Literal):
                if child.operator.name == "SUB":
                    dims.append("-"+child.children[1]._value)
                elif child.operator.name == "ADD":
                    dims.append(child.children[1]._value)
                else:
                    raise VisitorError(
                        "gen_stencil unsupported stencil operator found "
                        "'{0}'. Expecting '+' or '-'."
                        "".format(child._operator.name))
            else:
                raise VisitorError(
                    "gen_stencil unsupported stencil index found '{0}'."
                    "".format(str(child)))
        else:
            raise VisitorError(
                "gen_stencil unsupported (non-stencil) index found '{0}'."
                "".format(str(child)))
    return "[{0}]".format(",".join(dims))


class SIRWriter(PSyIRVisitor):
    '''Implements a PSyIR-to-SIR back end for PSyIR kernel code (not
    currently PSyIR algorithm code which has its own gen method for
    generating Fortran).

    :param bool skip_nodes: if skip_nodes is False then an exception \
    is raised if a visitor method for a PSyIR node has not been \
    implemented, otherwise the visitor continues, printing out a \
    representation of the unsupported node. This is an optional \
    argument which defaults to False.
    :param indent_string: Specifies what to use for indentation. This \
    is an optional argument that defaults to two spaces.
    :type indent_string: str or NoneType
    :param int initial_indent_depth: Specifies how much indentation to \
    start with. This is an optional argument that defaults to 0.

    '''
    def __init__(self, skip_nodes=False, indent_string="  ",
                 initial_indent_depth=0):
        super(SIRWriter, self).__init__(skip_nodes, indent_string,
                                        initial_indent_depth)
        self._field_names = set()
        
    def node_node(self, node):
        '''Catch any unsupported nodes, output their class names and continue
        down the node hierarchy. If skip_nodes is set to False then
        raise an exception.

        :param node: An unsupported PSyIR node.
        :type node: subclass of :py:class:`psyclone.psyGen.Node`

        :returns: The Fortran code as a string.
        :rtype: str

        :raises VisitorError: if skip_nodes is set to False.

        '''
        if not self._skip_nodes:
            raise VisitorError(
                "Class SIRWriter method node_node(), unsupported node "
                "found '{0}'".format(type(node)))
        result = "{0}[ {1} start ]\n".format(self._nindent,
                                             type(node).__name__)
        self._depth += 1
        for child in node.children:
            result += self._visit(child)
        self._depth -= 1
        result += "{0}[ {1} end ]\n".format(self._nindent, type(node).__name__)
        return result

    def nemoloop_node(self, node):
        '''Supported NEMO loops are triply nested with expected indices (not
        yet checked) and should contain a nemokern. If this is not the
        case then it is not possible to translate so raise an
        exception.

        :param node: a nemoLoop PSyIR node.
        :type node: subclass of :py:class:`psyclone.nemo.NemoLoop`

        :returns: the Fortran code as a string.
        :rtype: str

        :raises VisitorError: if the loop is not triply nested with \
        computation within the triply nested loop.

        '''
        if not (len(node.children) == 1 and
                isinstance(node.children[0], NemoLoop)):
            raise VisitorError("Child of loop should be a single loop.")

        if not (len(node.children[0].children) == 1 and
                isinstance(node.children[0].children[0], NemoLoop)):
            raise VisitorError("Child of child of loop should be a single loop.")

        if not isinstance(node.children[0].children[0].children[0],
                          NemoKern):
            raise VisitorError(
                "Child of child of child of loop should be a NemoKern.")

        result = ("{0}interval = makeInterval(Interval.Start, Interval.End, "
                  "0, 0)\n".format(self._nindent))
        result += ("{0}bodyAST = makeAST([\n".format(self._nindent))
        self._depth += 1
        result += self.nemokern_node(node.children[0].children[0].children[0])
        self._depth -= 1
        if result[-1] == "\n" and result[-2] == ",":
            result = result[:-2] + "\n"
        result += "{0}])\n".format(self._nindent)
        result += ("{0}verticalRegionFns.append(makeVerticalRegionDeclStmt("
                   "bodyAST, interval, VerticalRegion.Forward))\n"
                   "".format(self._nindent))
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

    def nemoinvokeschedule_node(self, node):
        '''This method is called when a NemoInvokeSchedule instance is found
        in the PSyIR tree.

        The constants_mod module is currently hardcoded into the
        output as it is required for LFRic code. When issue #375 has
        been addressed this module can be added only when required.

        :param node: A KernelSchedule PSyIR node.
        :type node: :py:class:`psyclone.psyGen.KernelSchedule`

        :returns: The Fortran code as a string.
        :rtype: str

        '''
        result = "# PSyclone autogenerated SIR Python\n"
        result += "verticalRegionFns = []\n"
        result += "stencilname = \"psyclone\"\n"

        exec_statements = ""
        for child in node.children:
            exec_statements += self._visit(child)
        result += (
            "{0}\n"
            "".format(exec_statements))
        result += (
            "{0}hir = makeSIR(stencilname+\".cpp\", [\n"
            "{0}{1}makeStencil(\n"
            "{0}{1}{1}stencilname,\n"
            "{0}{1}{1}makeAST(verticalRegionFns),\n"
            "{0}{1}{1}[".format(self._nindent, self._indent))
        functions = []
        for name in self._field_names:
            functions.append("makeField(\"{0}\")".format(name))
        result += ",".join(functions)
        result += "]\n"
        result += (
            "{0}{1})\n"
            "{0}])\n".format(self._nindent, self._indent))
        return result

    def assignment_node(self, node):
        '''This method is called when an Assignment instance is found in the
        PSyIR tree.

        :param node: An Assignment PSyIR node.
        :type node: :py:class:`psyclone.psyGen.Assigment`

        :returns: The Fortran code as a string.
        :rtype: str

        '''
        self._depth += 1
        lhs = self._visit(node.children[0])
        rhs = self._visit(node.children[1])
        self._depth -= 1
        result = "{0}makeAssignmentStmt(\n{1},\n{2}".format(self._nindent, lhs, rhs)
        if result[-1] == '\n':
            result = result[:-1] + ",\n"
        else:
            result += ",\n"
        result += "{0}{1}\"=\"),\n".format(self._nindent, self._indent)
        return result

    def binaryoperation_node(self, node):
        '''This method is called when a BinaryOperation instance is found in
        the PSyIR tree.

        :param node: A BinaryOperation PSyIR node.
        :type node: :py:class:`psyclone.psyGen.BinaryOperation`

        :returns: The Fortran code as a string.
        :rtype: str

        '''
        binary_operators = {
            BinaryOperation.Operator.ADD: '+',
            BinaryOperation.Operator.SUB: '-',
            BinaryOperation.Operator.MUL: '*',
            BinaryOperation.Operator.DIV: '/'}
            
        self._depth += 1
        lhs = self._visit(node.children[0])
        try:
            oper = binary_operators[node._operator]
        except KeyError:
            raise VisitorError(
                "Method binaryoperation_node in class SIRWriter, unsupported "
                "operator '{0}' found.".format(str(node._operator)))
        rhs = self._visit(node.children[1])
        self._depth -= 1
        result = "{0}makeBinaryOperator(\n{1}".format(self._nindent, lhs)
        if result[-1] == '\n':
            result = result[:-1] + ",\n"
        else:
            result += ",\n"
        result += "{0}{1}\"{2}\",\n{3}\n{0}{1})\n".format(self._nindent, self._indent, oper, rhs)
        return result

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
        return "{0}makeVarAccessExpr(\"{1}\")".format(self._nindent, node._reference)

    def array_node(self, node):
        '''This method is called when an Array instance is found in the PSyIR
        tree.

        :param node: An Array PSyIR node.
        :type node: :py:class:`psyclone.psyGen.Array`

        :returns: The Fortran code as a string.
        :rtype: str

        '''
        stencil = gen_stencil(node)
        result = "{0}makeFieldAccessExpr(\"{1}\",{2})".format(self._nindent, node.name, stencil)
        self._field_names.add(node.name)
        return result

    def literal_node(self, node):
        '''This method is called when a Literal instance is found in the PSyIR
        tree.

        :param node: A Literal PSyIR node.
        :type node: :py:class:`psyclone.psyGen.Literal`

        :returns: The Fortran code as a string.
        :rtype: str

        '''
        result = node._value
        return "{0}makeLiteralAccessExpr(\"{1}\", BuiltinType.Float)".format(self._nindent, result) 

    def unaryoperation_node(self, node):
        '''This method is called when a UnaryOperation instance is found in
        the PSyIR tree.

        :param node: A UnaryOperation PSyIR node.
        :type node: :py:class:`psyclone.psyGen.UnaryOperation`

        :returns: The Fortran code as a string.
        :rtype: str

        '''
        # Reverse the fortran2psyir mapping to make a psyir2fortran
        # mapping.
        mapping = {}
        for operator in f2psyir.unary_operators:
            mapping_key = f2psyir.unary_operators[operator]
            mapping_value = operator
            # Only choose the first mapping value when there is more
            # than one.
            if mapping_key not in mapping:
                mapping[mapping_key] = mapping_value
        oper = mapping[node._operator]
        if not (len(node.children) == 1 and isinstance(node.children[0], Literal)):
            raise VisitorError("Child of unary operator should be a literal")

        result = node.children[0]._value
        return "{0}makeLiteralAccessExpr(\"{1}{2}\", BuiltinType.Float)".format(self._nindent, oper, result) 

    def codeblock_node(self, node):
        '''This method is called when a CodeBlock instance is found in the
        PSyIR tree. It returns the content of the CodeBlock as a
        Fortran string, indenting as appropriate.

        :param node: A CodeBlock PSyIR node.
        :type node: :py:class:`psyclone.psyGen.CodeBlock`

        :returns: The Fortran code as a string.
        :rtype: str

        '''
        return "{0}[ CodeBlock ]".format(self._nindent)
