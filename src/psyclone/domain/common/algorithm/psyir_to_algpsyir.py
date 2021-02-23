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

'''Specialise generic PSyIR representing an algorithm layer to a
PSyclone algorithm-layer-specific PSyIR which uses specialised classes.

'''
def psyir_to_algpsyir(psyir):
    '''Takes a generic PSyIR tree and translates it to a
    PSyclone algorithm-specific PSyIR representation

    '''
    from psyclone.psyir.nodes import Call, CodeBlock, ArrayReference
    from psyclone.psyir.symbols import RoutineSymbol, Symbol, TypeSymbol, \
        StructureType
    from psyclone.domain.common.algorithm import AlgorithmInvokeCall, \
        KernelLayerRef
    from psyclone.errors import GenerationError

    for call in psyir.walk(Call):
        if call.routine.name.lower() == "invoke":
            kernel_calls = []
            for call_arg in call.children:
                # Children should reference kernel metadata
                if isinstance(call_arg, ArrayReference):
                    routine_symbol = call_arg.symbol
                    if type(routine_symbol) is Symbol:
                        # TODO Use specialise method from PR #1063
                        # when it is on master
                        # routine_symbol.specialise_to(TypeSymbol)
                        routine_symbol.__class__ = TypeSymbol
                        routine_symbol.datatype = StructureType()
                    kernel_calls.append(KernelLayerRef.create(
                        routine_symbol, call_arg.children))
                else:
                    raise GenerationError(
                        "Unsupported argument type found. Expecting a "
                        "reference to kernel metadata, but found '{0}'."
                        "".format(type(call_arg).__name__))

            invoke_call = AlgorithmInvokeCall.create(
                call.routine, kernel_calls)
            # issue #1124, use Node.replace_with() here
            invoke_call.parent = call.parent
            position = call.position
            call.parent.children.remove(call)
            invoke_call.parent.children.insert(position, invoke_call)


__all__ = [psyir_to_algpsyir]
