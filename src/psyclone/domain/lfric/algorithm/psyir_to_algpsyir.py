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
    Actual_Arg_Spec, Name, Char_Literal_Constant
from psyclone.psyir.nodes import Call, CodeBlock, ArrayReference
from psyclone.psyir.symbols import Symbol, TypeSymbol, \
    StructureType
from psyclone.domain.lfric.algorithm import \
    LfricBuiltinRef, LfricCodedKernelRef, LfricAlgorithmInvokeCall
from psyclone.dynamo0p3_builtins import BUILTIN_MAP as builtins
from psyclone.errors import GenerationError, InternalError


def psyir_to_algpsyir(psyir):
    '''Takes a generic PSyIR tree and translates it to an
    LFRic algorithm-specific PSyIR representation

    :param psyir: generic-PSyIR tree.
    :type psyir: subclass of :py:class:`psyclone.psyir.nodes.node`

    :raises GenerationError: if a PSyIR CodeBlock represents an \
        argument and the CodeBlock contains more than one statement (as \
        only one is expected).
    :raises GenerationError: if more than one named argument is \
        provided in an invoke call.
    :raises GenerationError: if a named argument has an incorrect \
        format.
    :raises InternalError: if an unexpected argument type is found.

    '''
    for call in psyir.walk(Call):
        if call.routine.name.lower() == "invoke":
            call_description = None
            kernel_calls = []
            for call_arg in call.children:
                # Children should be a named argument, builtin or
                # kernelcall.
                # pylint: disable=protected-access
                if (isinstance(call_arg, CodeBlock) and
                        len(call_arg._fp2_nodes) > 1):
                    raise GenerationError(
                        "If the PSyIR contains a CodeBlock as an invoke "
                        "argument it should be a Fortran named argument. "
                        "There should only be one named argument. However, "
                        "this code block contains multiple nodes.")

                if (isinstance(call_arg, CodeBlock) and
                        isinstance(call_arg.ast, Actual_Arg_Spec)):
                    # This child is a named argument
                    if not (isinstance(call_arg.ast.children[0], Name) and
                            call_arg.ast.children[0].string.lower() == "name"
                            and isinstance(call_arg.ast.children[1],
                                           Char_Literal_Constant)):
                        raise GenerationError(
                            "If there is a named argument, it must take the "
                            "form name='str', but found '{0}'."
                            "".format(str(call_arg.ast)))
                    if call_description:
                        raise GenerationError(
                            "There should be at most one named argument in an "
                            "invoke, but there are at least two: '{0}' and "
                            "'{1}'.".format(call_description,
                                            call_arg.ast.children[1].string))
                    call_description = call_arg.ast.children[1].string
                elif isinstance(call_arg, ArrayReference):
                    # This child is a kernelcall or builtin argument
                    name = call_arg.name
                    if name in builtins:
                        routine_symbol = call.scope.symbol_table.lookup(name)
                        # pylint: disable=unidiomatic-typecheck
                        if type(routine_symbol) is Symbol:
                            # Needs setting to a RoutineSymbol
                            # TODO Use specialise method from PR #1063
                            # when it is on master
                            # routine_symbol.specialise_to(TypeSymbol)
                            routine_symbol.__class__ = TypeSymbol
                            routine_symbol.datatype = StructureType()
                        kernel_calls.append(LfricBuiltinRef.create(
                            routine_symbol, call_arg.children))
                    else:
                        routine_symbol = call_arg.symbol
                        # pylint: disable=unidiomatic-typecheck
                        if type(routine_symbol) is Symbol:
                            # TODO Use specialise method from PR #1063
                            # when it is on master
                            # routine_symbol.specialise(RoutineSymbol)
                            routine_symbol.__class__ = TypeSymbol
                            routine_symbol.datatype = StructureType()
                        kernel_calls.append(LfricCodedKernelRef.create(
                            routine_symbol, call_arg.children))
                else:
                    raise InternalError(
                        "Unsupported argument type found. Expecting coded "
                        "call, builtin call or name='xxx', but found '{0}'."
                        "".format(call_arg))

            invoke_call = LfricAlgorithmInvokeCall.create(
                call.routine, kernel_calls, description=call_description)
            # issue #1124, use Node.replace_with() here
            invoke_call.parent = call.parent
            position = call.position
            call.parent.children.remove(call)
            invoke_call.parent.children.insert(position, invoke_call)


__all__ = ['psyir_to_algpsyir']
