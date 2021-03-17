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
from psyclone.psyir.nodes import Call, ArrayReference
from psyclone.psyir.symbols import Symbol, TypeSymbol, StructureType
from psyclone.domain.common.algorithm import AlgorithmInvokeCall, \
    KernelLayerRef
from psyclone.errors import InternalError


def psyir_to_algpsyir(psyir):
    '''Takes a generic PSyIR tree and translates it to a
    PSyclone algorithm-specific PSyIR representation.

    :param psyir: generic PSyIR tree.
    :type psyir: subclass of :py:class:`psyclone.psyir.nodes.node`

    :raises InternalError: if an unexpected argument type is found.

    '''
    for call in psyir.walk(Call):
        if call.routine.name.lower() == "invoke":
            kernel_calls = []
            for call_arg in call.children:
                # Children should reference kernel metadata
                if isinstance(call_arg, ArrayReference):
                    # Note, fparser has mis-parsed this argument as an
                    # array reference when it should have been a
                    # structure constructor.
                    routine_symbol = call_arg.symbol
                    # pylint: disable=unidiomatic-typecheck
                    if type(routine_symbol) is Symbol:
                        routine_symbol.specialise(TypeSymbol)
                        routine_symbol.datatype = StructureType()
                    kernel_calls.append(KernelLayerRef.create(
                        routine_symbol, call_arg.children))
                else:
                    raise InternalError(
                        "Unsupported argument type found. Expecting a "
                        "reference to kernel metadata, but found '{0}'."
                        "".format(type(call_arg).__name__))

            invoke_call = AlgorithmInvokeCall.create(
                call.routine, kernel_calls)
            call.replace_with(invoke_call)


__all__ = ['psyir_to_algpsyir']
