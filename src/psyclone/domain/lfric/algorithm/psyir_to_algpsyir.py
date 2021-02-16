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

'''Specialise generic PSyIE representing an algorithm layer to an
LFRic algorithm-layer-specific PSyIR which uses specialised classes.

'''

def psyir_to_algpsyir(psyir):
    '''Takes a generic PSyIR tree and translates it to an
    LFRic algorithm-specific PSyIR representation

    '''
    from psyclone.psyir.nodes import Call, CodeBlock, ArrayReference
    from psyclone.psyir.symbols import RoutineSymbol
    import psyclone.domain.lfric.algorithm.psyir as algpsyir
    from fparser.two.Fortran2003 import Actual_Arg_Spec

    from psyclone.configuration import Config
    from psyclone.parse.utils import check_api

    from psyclone.dynamo0p3_builtins import BUILTIN_MAP as builtins

    for call in psyir.walk(Call):
        if call.routine.name.lower() == "invoke":
            call_description = None
            kernel_calls = []
            for call_arg in call.children:
                # Children should be a named argument, builtin or
                # kernelcall.
                if isinstance(call_arg, CodeBlock) and isinstance(call_arg.ast, Actual_Arg_Spec):
                    # TODO Check call_arg.ast.children[0] is a Name with content "name"
                    # TODO Check call_arg.ast.children[1] is a Char_Literal_Constant
                    print ("It's a named argument")
                    call_description = call_arg.ast.children[1].string
                elif isinstance(call_arg, ArrayReference):
                    name = call_arg.name
                    if name in builtins:
                        print ("It's a Builtin called {0}".format(name))
                        # TODO Check name is not in symbol table
                        # TODO What about call_arg.symbol?
                        # TODO Make the name reference the psyclone_builtins module
                        try:
                            routine_symbol = call.scope.symbol_table.lookup(name)
                            routine_symbol.__class__ = RoutineSymbol
                        except KeyError:
                            routine_symbol = RoutineSymbol(name)
                            call.scope.symbol_table.add(routine_symbol)
                        kernel_calls.append(algpsyir.BuiltinCall.create(routine_symbol, call_arg.children))
                    else:
                        routine_symbol = call_arg.symbol
                        routine_symbol.__class__ = RoutineSymbol
                        kernel_calls.append(algpsyir.CodedCall.create(routine_symbol, call_arg.children))
                else:
                    print (type(call_arg))
                    print ("TBD")
                    exit(1)

            invoke_call = algpsyir.AlgorithmInvokeCall.create(
                call.routine, kernel_calls)
            invoke_call._description = call_description
            invoke_call.parent = call.parent
            position = call.position
            call.parent.children.remove(call)
            invoke_call.parent.children.insert(position, invoke_call)


__all__ = {psyir_to_algpsyir}
