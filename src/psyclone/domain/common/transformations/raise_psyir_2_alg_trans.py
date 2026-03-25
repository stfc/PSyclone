# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2021-2026, Science and Technology Facilities Council.
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
# Authors: R. W. Ford, A. R. Porter, N. Nobre and S. Siso STFC Daresbury Lab

'''Specialise generic PSyIR representing an invoke call within the
algorithm layer to a PSyclone algorithm-layer-specific invoke call
which uses specialised classes.

'''

from psyclone.psyir.frontend.fortran import FortranReader
from psyclone.psyir.nodes import Call, Literal, Routine
from psyclone.psyir.symbols import (
    Symbol, DataTypeSymbol, StructureType, ScalarType)
from psyclone.domain.common.algorithm import (
    AlgorithmInvokeCall, KernelFunctor)
from psyclone.psyGen import Transformation
from psyclone.psyir.transformations import TransformationError
from psyclone.utils import transformation_documentation_wrapper


@transformation_documentation_wrapper
class RaisePSyIR2AlgTrans(Transformation):
    '''Transform a generic PSyIR representation of an Algorithm-layer
    invoke call to a PSyclone version with specialised domain-specific
    nodes.

    '''

    def __init__(self):
        super().__init__()
        self._call_name = None

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

    def validate(self, node: Call, options=None, **kwargs):
        '''Validate the node argument.

        :param node: a PSyIR call node capturing an invoke call in
            generic PSyIR.
        :type node: :py:class:`psyclone.psyir.nodes.Call`
        :param options: a dictionary with options for transformations.
        :type options: Optional[Dict[str, Any]]

        :raises TransformationError: if the supplied call argument is
            not a PSyIR Call node.
        :raises TransformationError: if the supplied call argument
            does not have the expected name which would identify it as an
            invoke call.
        :raises TransformationError: if there is more than one named argument.
        :raises TransformationError: if the named argument does not
            conform to the name=str format.
        :raises TransformationError: if the name of the invoke is invalid.
        :raises TransformationError: if the invoke arguments are not a
            PSyIR ArrayReference or CodeBlock.

        '''
        if not options:
            self.validate_options(**kwargs)

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
        names = [name for name in node.argument_names if name]
        if len(names) > 1:
            raise TransformationError(
                f"Error in {self.name} transformation. There should be at "
                f"most one named argument in an invoke, but there are "
                f"{len(names)} in '{node.debug_string()}'.")
        for idx, arg in enumerate(node.arguments):
            if node.argument_names[idx]:
                if (not node.argument_names[idx].lower() == "name"
                    or not (isinstance(arg, Literal) and
                            isinstance(arg.datatype, ScalarType) and
                            arg.datatype.intrinsic ==
                            ScalarType.Intrinsic.CHARACTER)):
                    raise TransformationError(
                        f"Error in {self.name} transformation. If there "
                        f"is a named argument, it must take the form name"
                        f"='str', but found '{node.debug_string()}'.")
                try:
                    FortranReader.validate_name(arg.value)
                except (TypeError, ValueError) as err:
                    raise TransformationError(
                        f"Problem with invoke name: {err}") from err
            if node.argument_names[idx]:
                pass
            elif isinstance(arg, Call):
                if arg.symbol == arg.ancestor(Routine).symbol:
                    raise TransformationError(
                        f"The invoke call argument '{arg.symbol.name}' has "
                        f"been used as the Algorithm routine name. This is not"
                        f" allowed.")
            else:
                info = (
                    f"The arguments to this invoke call are expected to "
                    f"be kernel calls which are represented in generic "
                    f"PSyIR as Calls, but '{arg.debug_string()}' is of type "
                    f"'{type(arg).__name__}'.")
                raise TransformationError(
                    f"Error in {self.name} transformation. {info}")

    def apply(self, node: Call, index: int = None, options=None, **kwargs):
        ''' Apply the transformation to the supplied node.

        :param node: a PSyIR call node capturing an invoke call in
            generic PSyIR.
        :param index: the position of this invoke call relative to
            other invokes in the algorithm layer.
        :param options: a dictionary with options for transformations.
        :type options: Optional[Dict[str, Any]]

        '''
        self.validate(node, index=index, options=options, **kwargs)

        call_name = None
        calls = []
        for idx, call_arg in enumerate(node.arguments):

            # pylint: disable=protected-access
            arg_info = []
            if node.argument_names[idx]:
                call_name = f"{call_arg.value}"
                continue
            else:
                # Get the symbols and args to reconstruct it as a
                # higher-abstraction AlgorithmInvokeCall node
                type_symbol = call_arg.routine.symbol
                args = call_arg.pop_all_children()[1:]
                arg_info.append((type_symbol, args))

            for (type_symbol, args) in arg_info:
                self._specialise_symbol(type_symbol)
                calls.append(KernelFunctor.create(type_symbol, args))

        invoke_call = AlgorithmInvokeCall.create(
            node.routine.symbol, calls, index, name=call_name)

        # Keep comments
        invoke_call.preceding_comment = node.preceding_comment
        invoke_call.inline_comment = node.inline_comment

        node.replace_with(invoke_call)


__all__ = ['RaisePSyIR2AlgTrans']
