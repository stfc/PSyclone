# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2021, Science and Technology Facilities Council.
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

'''Specialise generic PSyIR representing an algorithm layer to an
LFRic algorithm-layer-specific PSyIR which uses specialised classes.

'''
from fparser.two.Fortran2003 import \
    Actual_Arg_Spec, Name, Char_Literal_Constant, Structure_Constructor
from psyclone.psyir.nodes import Call, CodeBlock, ArrayReference
from psyclone.psyir.symbols import Symbol, TypeSymbol, \
    StructureType, RoutineSymbol
from psyclone.domain.lfric.algorithm import \
    LfricBuiltinRef, LfricCodedKernelRef, LfricAlgorithmInvokeCall
from psyclone.domain.lfric.lfric_builtins import BUILTIN_MAP as builtins
from psyclone.errors import GenerationError, InternalError
from psyclone.psyir.frontend.fparser2 import Fparser2Reader

from psyclone.domain.common.transformations.invoke_trans import parse_args

class LFRicInvokeTrans(InvokeTrans):
    ''' xxx '''

    def validate(self, call, options=None):
        ''' xxx '''
        for call_arg in call.children:
            if isinstance(call_arg, CodeBlock):
                for fp2_node in call_arg._fp2_nodes:
                    if isinstance(fp2_node, Actual_Arg_Spec):
                        if not (isinstance(fp2_node.children[0], Name) and
                                fp2_node.children[0].string.lower() ==
                                "name"
                                and isinstance(fp2_node.children[1],
                                               Char_Literal_Constant)):
                            raise GenerationError(
                                "If there is a named argument, it must "
                                "take the form name='str', but found "
                                "'{0}'.".format(str(fp2_node)))
                        if call_description:
                            raise GenerationError(
                                "There should be at most one named "
                                "argument in an invoke, but there are at "
                                "least two: '{0}' and "
                                "'{1}'.".format(
                                    call_description,
                                    fp2_node.children[1].string))
                    elif isinstance(fp2_node, Structure_Constructor):
                        pass
                    else:
                        raise GenerationError(
                            "Expecting an algorithm invoke codeblock to "
                            "contain either Structure-Constructor or "
                            "actual-arg-spec, but found '{0}'."
                            "".format(type(fp2_node).__name__))
            elif isinstance(call_arg, ArrayReference):
                pass
            else:
                raise GenerationError(
                    "Unsupported argument type found. Expecting coded "
                    "call, builtin call or name='xxx', but found '{0}'."
                    "".format(call_arg))


    def apply(self, call, options=None):
        ''' xxx '''

        self.validate(call, options=options)

        call_description = None
        kernel_calls = []
        for call_arg in call.children:

            if isinstance(call_arg, CodeBlock):
                for fp2_node in call_arg._fp2_nodes:

            arg_info = []
            if isinstance(call_arg, ArrayReference):
                # Structure constructor mis-parsed as an array
                # reference.
                name = call_arg.name
                if name in builtins:
                    type_symbol = call.scope.symbol_table.lookup(name)
                    node_type = LfricBuiltinRef
                else:
                    type_symbol = call_arg.symbol
                    node_type = LfricCodedKernelRef
                arg_info.append(type_symbol, args)
            else:
                # CodeBlock containing a structure constructor
                for fp2_node in call_arg._fp2_nodes:
                    if isinstance(fp2_node, Actual_Arg_Spec):
                        # This child is a named argument
                        call_description = fp2_node.children[1].string
                    else:
                        type_symbol = get_symbol(call, fp2_node)
                        args = parse_args(call_arg, fp2_node)
                        arg_info.append(type_symbol, args)

            for (type_symbol, args) in arg_info:
                specialise_symbol(type_symbol)
                kernel_calls.append(KernelLayerRef.create(
                    type_symbol, args))

        invoke_call = LfricAlgorithmInvokeCall.create(
            call.routine, kernel_calls, description=call_description)
        call.replace_with(invoke_call)


__all__ = ['psyir_to_algpsyir']
