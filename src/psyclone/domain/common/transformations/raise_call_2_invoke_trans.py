# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2021-2022, Science and Technology Facilities Council.
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
# Author R. W. Ford STFC Daresbury Lab

'''Specialise generic PSyIR representing an invoke call within the
algorithm layer to a PSyclone algorithm-layer-specific invoke call
which uses specialised classes.

'''

# pylint: disable=protected-access

from fparser.two.Fortran2003 import Structure_Constructor, Actual_Arg_Spec, \
    Name, Char_Literal_Constant

from psyclone.psyir.nodes import Call, ArrayReference, CodeBlock
from psyclone.psyir.symbols import Symbol, DataTypeSymbol, StructureType, \
    RoutineSymbol
from psyclone.domain.common.algorithm import AlgorithmInvokeCall, \
    KernelFunctor
from psyclone.psyGen import Transformation
from psyclone.psyir.transformations import TransformationError
from psyclone.psyir.frontend.fparser2 import Fparser2Reader


class InvokeCallTrans(Transformation):
    '''Transform a generic PSyIR representation of an Algorithm-layer
    invoke call to a PSyclone version with specialised domain-specific
    nodes.

    '''
    def __init__(self):
        super().__init__()
        self._call_name = None

    @staticmethod
    def _parse_args(code_block, fp2_node):
        '''Return the arguments from a Structure Constructor stored as a
        CodeBlock containing an fparser2 ast.

        :param code_block: the CodeBlock containing a StructureConstructor.
        :type code_block: :py:class:`psyclone.psyir.nodes.CodeBlock`
        :param fp2_node: the fparser2 Structure Constructor node.
        :type fp2_node: \
            :py:class:`fparser.two.Fortran2003.Structure_Constructor`

        :returns: a list of PSyIR nodes containing the \
            StructureConstructor arguments.
        :rtype: list of :py:class:`psyclone.psyir.nodes.Node`

        '''
        dummy_call = Call(RoutineSymbol("dummy"),
                          parent=code_block.parent)
        fparser2 = Fparser2Reader()
        for arg in fp2_node.children[1].children:
            fparser2.process_nodes(dummy_call, [arg])
        return dummy_call.pop_all_children()

    @staticmethod
    def _get_symbol(call, fp2_node):
        '''Return the name of a Structure Constructor stored as a CodeBlock
        containing an fparser2 ast.

        :param code_block: the CodeBlock containing a StructureConstructor.
        :type code_block: :py:class:`psyclone.psyir.nodes.CodeBlock`
        :param fp2_node: the fparser2 Structure Constructor node.
        :type fp2_node: \
            :py:class:`fparser.two.Fortran2003.Structure_Constructor`

        :returns: the symbol capturing the name and type of the \
            StructureConstructor.
        :rtype: :py:class:`psyclone.psyir.symbols.Symbol`

        '''
        name = fp2_node.children[0].string
        symbol_table = call.scope.symbol_table
        try:
            type_symbol = symbol_table.lookup(name)
        except KeyError:
            type_symbol = DataTypeSymbol(name, StructureType())
            symbol_table.add(type_symbol)
        return type_symbol

    @staticmethod
    def _specialise_symbol(symbol):
        '''If the symbol argument is a Symbol then change it into a
        DataTypeSymbol.

        :param symbol: a symbol that will be modified to a DataTypeSymbol \
            if it is a Symbol.
        :type symbol: :py:class:`psyclone.psyir.symbols.Symbol`

        '''
        # pylint: disable=unidiomatic-typecheck
        if type(symbol) is Symbol:
            symbol.specialise(DataTypeSymbol)
            symbol.datatype = StructureType()

    def _validate_fp2_node(self, fp2_node):
        '''Validation routine for an fparser2 node within a code block.

        :param fp2_node: an fparser2 Structure Constructor or Actual \
            Arg Spec node.
        :type fp2_node: \
            :py:class:`fparser.two.Fortran2003.Structure_Constructor` or \
            :py:class:`fparser.two.Fortran2003.Actual_Arg_Spec

        :raises TransformationError: if the named argument is not in \
            the expected form.
        :raises TransformationError: if more than one named argument \
            is found.
        :raises TransformationError: if the fparser2 node is not the \
            expected type.

        '''
        if isinstance(fp2_node, Structure_Constructor):
            pass
        elif isinstance(fp2_node, Actual_Arg_Spec):
            if not (isinstance(fp2_node.children[0], Name) and
                    fp2_node.children[0].string.lower() == "name" and
                    isinstance(fp2_node.children[1], Char_Literal_Constant)):
                raise TransformationError(
                    f"Error in {self.name} transformation. If there is a "
                    f"named argument, it must take the form name='str', but "
                    f"found '{str(fp2_node)}'.")
            if self._call_name:
                raise TransformationError(
                    f"Error in {self.name} transformation. There should be at "
                    f"most one named argument in an invoke, but there are at "
                    f"least two: {self._call_name} and "
                    f"{fp2_node.children[1].string}.")
            self._call_name = fp2_node.children[1].string
        else:
            raise TransformationError(
                f"Error in {self.name} transformation. Expecting an algorithm "
                f"invoke codeblock to contain either Structure-Constructor or "
                f"actual-arg-spec, but found '{type(fp2_node).__name__}'.")

    def validate(self, node, options=None):
        '''Validate the node argument.

        :param node: a PSyIR call node capturing an invoke call in \
            generic PSyIR.
        :type node: :py:class:`psyclone.psyir.nodes.Call`
        :param options: a dictionary with options for transformations.
        :type options: dictionary of string:values or None

        :raises TransformationError: if the supplied call argument is \
            not a PSyIR Call node.
        :raises TransformationError: if the supplied call argument \
            does not have the expected name which would identify it as an \
            invoke call.
        :raises TransformationError: if the invoke arguments are not a \
            PSyIR ArrayReference or CodeBlock.

        '''
        self._call_name = None

        if not isinstance(node, Call):
            raise TransformationError(
                f"Error in {self.name} transformation. The supplied call "
                f"argument should be a `Call` node but found "
                f"'{type(node).__name__}'.")
        if not node.routine.name.lower() == "invoke":
            raise TransformationError(
                f"Error in {self.name} transformation. The supplied call "
                f"argument should be a `Call` node with name 'invoke' but "
                f"found '{node.routine.name}'.")
        for arg in node.children:
            if isinstance(arg, ArrayReference):
                pass
            elif isinstance(arg, CodeBlock):
                for fp2_node in arg._fp2_nodes:
                    self._validate_fp2_node(fp2_node)
            else:
                raise TransformationError(
                    f"Error in {self.name} transformation. The arguments to "
                    f"this invoke call are expected to be a CodeBlock or an "
                    f"ArrayReference, but found '{type(arg).__name__}'.")

    def apply(self, call, index, options=None):
        ''' Apply the transformation to the supplied node.

        :param call: a PSyIR call node capturing an invoke call in \
            generic PSyIR.
        :type call: :py:class:`psyclone.psyir.nodes.Call`
        :param int index: the position of this invoke call relative to \
            other invokes in the algorithm layer.
        :param options: a dictionary with options for transformations.
        :type options: dictionary of string:values or None

        '''
        self.validate(call, options=options)

        call_name = None
        calls = []
        for call_arg in call.children:

            arg_info = []
            if isinstance(call_arg, ArrayReference):
                # kernel misrepresented as ArrayReference
                args = call_arg.pop_all_children()
                type_symbol = call_arg.symbol
                arg_info.append((type_symbol, args))
            else:
                for fp2_node in call_arg._fp2_nodes:
                    if isinstance(fp2_node, Actual_Arg_Spec):
                        # This child is a named argument.
                        call_name = fp2_node.children[1].string
                    else:
                        # This child is a kernel
                        type_symbol = InvokeCallTrans._get_symbol(
                            call, fp2_node)
                        args = InvokeCallTrans._parse_args(call_arg, fp2_node)
                        arg_info.append((type_symbol, args))

            for (type_symbol, args) in arg_info:
                self._specialise_symbol(type_symbol)
                calls.append(KernelFunctor.create(type_symbol, args))

        invoke_call = AlgorithmInvokeCall.create(
            call.routine, calls, index, name=call_name)
        call.replace_with(invoke_call)

    @property
    def name(self):
        '''
        :returns: a name identifying this transformation.
        :rtype: str

        '''
        return "InvokeCallTrans"


__all__ = ['InvokeCallTrans']
