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
    Array, UnaryOperation
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
                    dims.append("-"+child.children[1].value)
                elif child.operator.name == "ADD":
                    dims.append(child.children[1].value)
                else:
                    raise VisitorError(
                        "gen_stencil unsupported stencil operator found "
                        "'{0}'. Expecting '+' or '-'."
                        "".format(child.operator.name))
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
    :param indent_string: specifies what to use for indentation. This \
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
        down the node hierarchy (if skip_node is set to True). This is
        useful for debugging and differs from the base class
        implementation of skip_nodes which silently continues. If
        skip_nodes is set to False then raise an exception if an
        unsupported node is found.

        :param node: an unsupported PSyIR node.
        :type node: subclass of :py:class:`psyclone.psyGen.Node`

        :returns: the SIR code as a string.
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

    def nemoloop_node(self, loop_node):
        '''Supported NEMO loops are triply nested with particular indices (not
        yet checked) and should contain a nemokern. If this is not the
        case then it is not possible to translate so an exception is
        raised.

        :param loop_node: a nemoLoop PSyIR node.
        :type loop_node: subclass of :py:class:`psyclone.nemo.NemoLoop`

        :returns: the SIR code as a string.
        :rtype: str

        :raises VisitorError: if the loop is not triply nested with \
        computation within the triply nested loop.

        '''
        # Check first loop has a single loop as a child.
        loop_content = loop_node.loop_body.children
        if not (len(loop_content) == 1 and
                isinstance(loop_content[0], NemoLoop)):
            raise VisitorError("Child of loop should be a single loop.")

        # Check second loop has a single loop as a child.
        loop_content = loop_content[0].loop_body.children
        if not (len(loop_content) == 1 and
                isinstance(loop_content[0], NemoLoop)):
            raise VisitorError(
                "Child of child of loop should be a single loop.")

        # Check third loop has a single NemoKern as a child.
        loop_content = loop_content[0].loop_body.children
        if not (len(loop_content) == 1 and
                isinstance(loop_content[0], NemoKern)):
            raise VisitorError(
                "Child of child of child of loop should be a NemoKern.")

        # The interval values are hardcoded for the moment (see #470).
        result = ("{0}interval = makeInterval(Interval.Start, Interval.End, "
                  "0, 0)\n".format(self._nindent))
        result += ("{0}bodyAST = makeAST([\n".format(self._nindent))
        self._depth += 1
        result += self.nemokern_node(loop_content[0])
        self._depth -= 1
        if result[-1] == "\n" and result[-2] == ",":
            # Remove the additional comma as this is the last entry in
            # makeAST.
            result = result[:-2] + "\n"
        result += "{0}])\n".format(self._nindent)
        # For the moment there is a hard coded assumption that the
        # vertical looping is in the forward (1..n) direction (see
        # #470).
        result += ("{0}verticalRegionFns.append(makeVerticalRegionDeclStmt("
                   "bodyAST, interval, VerticalRegion.Forward))\n"
                   "".format(self._nindent))
        return result

    def nemokern_node(self, node):
        '''NEMO kernels are a group of nodes collected into a schedule
        so simply call the nodes in the schedule.

        :param node: a NemoKern PSyIR node.
        :type node: :py:class:`psyclone.nemo.NemoKern`

        :returns: the SIR code as a string.
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

        :param node: a KernelSchedule PSyIR node.
        :type node: :py:class:`psyclone.psyGen.KernelSchedule`

        :returns: the SIR code as a string.
        :rtype: str

        '''
        result = "# PSyclone autogenerated SIR Python\n"
        result += "verticalRegionFns = []\n"
        # The stencil name is currently hardcoded.
        result += "stencilname = \"psyclone\"\n"

        exec_statements = ""
        for child in node.children:
            exec_statements += self._visit(child)
        result += (
            "{0}\n"
            "".format(exec_statements))
        # The file name is hard coded at the moment.
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

        :param node: an Assignment PSyIR node.
        :type node: :py:class:`psyclone.psyGen.Assigment`

        :returns: the SIR code as a string.
        :rtype: str

        '''
        self._depth += 1
        lhs = self._visit(node.lhs)
        rhs = self._visit(node.rhs)
        self._depth -= 1
        result = ("{0}makeAssignmentStmt(\n{1},\n{2}"
                  "".format(self._nindent, lhs, rhs))
        if result[-1] == '\n':
            # For better formatting, remove the newline if one exists.
            result = result[:-1]
        result += ",\n"
        result += "{0}{1}\"=\"),\n".format(self._nindent, self._indent)
        return result

    def binaryoperation_node(self, node):
        '''This method is called when a BinaryOperation instance is found in
        the PSyIR tree.

        :param node: a BinaryOperation PSyIR node.
        :type node: :py:class:`psyclone.psyGen.BinaryOperation`

        :returns: the SIR code as a string.
        :rtype: str

        :raises VisitorError: if there is no mapping from the PSyIR \
        operator to SIR.

        '''
        binary_operators = {
            BinaryOperation.Operator.ADD: '+',
            BinaryOperation.Operator.SUB: '-',
            BinaryOperation.Operator.MUL: '*',
            BinaryOperation.Operator.DIV: '/'}

        self._depth += 1
        lhs = self._visit(node.children[0])
        try:
            oper = binary_operators[node.operator]
        except KeyError:
            raise VisitorError(
                "Method binaryoperation_node in class SIRWriter, unsupported "
                "operator '{0}' found.".format(str(node.operator)))
        rhs = self._visit(node.children[1])
        self._depth -= 1
        result = "{0}makeBinaryOperator(\n{1}".format(self._nindent, lhs)
        if result[-1] == '\n':
            # For better formatting, remove the newline if one exists.
            result = result[:-1]
        result += ",\n"
        result += ("{0}{1}\"{2}\",\n{3}\n{0}{1})\n"
                   "".format(self._nindent, self._indent, oper, rhs))
        return result

    def reference_node(self, node):
        '''This method is called when a Reference instance is found in the
        PSyIR tree.

        :param node: a Reference PSyIR node.
        :type node: :py:class:`psyclone.psyGen.Reference`

        :returns: the SIR code as a string.
        :rtype: str

        :raises VisitorError: if this node has children.

        '''
        if node.children:
            raise VisitorError(
                "Method reference_node in class SIRWriter: SIR Reference "
                "node is not expected to have any children.")
        return "{0}makeVarAccessExpr(\"{1}\")".format(self._nindent, node.name)

    def array_node(self, node):
        '''This method is called when an Array instance is found in the PSyIR
        tree.

        :param node: an Array PSyIR node.
        :type node: :py:class:`psyclone.psyGen.Array`

        :returns: the SIR code as a string.
        :rtype: str

        '''
        stencil = gen_stencil(node)
        result = ("{0}makeFieldAccessExpr(\"{1}\",{2})"
                  "".format(self._nindent, node.name, stencil))
        # _field_names is a set so duplicates will be ignored.
        self._field_names.add(node.name)
        return result

    def literal_node(self, node):
        '''This method is called when a Literal instance is found in the PSyIR
        tree.

        :param node: a Literal PSyIR node.
        :type node: :py:class:`psyclone.psyGen.Literal`

        :returns: the SIR code as a string.
        :rtype: str

        '''
        result = node.value
        # There is an assumption here that the literal is a float (see
        # #468).
        return ("{0}makeLiteralAccessExpr(\"{1}\", BuiltinType.Float)"
                "".format(self._nindent, result))

    def unaryoperation_node(self, node):
        '''This method is called when a UnaryOperation instance is found in
        the PSyIR tree.

        :param node: a UnaryOperation PSyIR node.
        :type node: :py:class:`psyclone.psyGen.UnaryOperation`

        :returns: the SIR code as a string.
        :rtype: str

        :raises VisitorError: if there is no mapping from the PSyIR \
        operator to SIR, or if the child of the PSyIR operator is not \
        a literal (as only -<literal> is currently supported).

        '''
        # Currently only '-' is supported in the SIR mapping.
        unary_operators = {
            UnaryOperation.Operator.MINUS: '-'}
        try:
            oper = unary_operators[node.operator]
        except KeyError:
            raise VisitorError(
                "Method unaryoperation_node in class SIRWriter, unsupported "
                "operator '{0}' found.".format(str(node.operator)))
        # Currently only '-<literal>' is supported in the SIR mapping.
        if not (len(node.children) == 1 and
                isinstance(node.children[0], Literal)):
            raise VisitorError("Child of unary operator should be a literal.")
        result = node.children[0].value
        # There is an assumption here that the literal is a float (see #468)
        return ("{0}makeLiteralAccessExpr(\"{1}{2}\", BuiltinType.Float)"
                "".format(self._nindent, oper, result))
