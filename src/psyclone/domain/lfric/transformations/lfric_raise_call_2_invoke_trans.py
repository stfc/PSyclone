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
# Authors: R. W. Ford and A. R. Porter, STFC Daresbury Lab.

'''Specialise generic PSyIR representing an invoke call withing the
algorithm layer to an LFRic algorithm-layer-specific invoke call which
uses specialised classes.

'''
from psyclone.psyir.nodes import ArrayReference

from psyclone.domain.common.transformations import RaiseCall2InvokeTrans
from psyclone.domain.lfric.algorithm import LFRicBuiltinFunctor, \
    LFRicKernelFunctor, LFRicAlgorithmInvokeCall
from psyclone.domain.lfric.lfric_builtins import BUILTIN_MAP as builtins


class LFRicRaiseCall2InvokeTrans(RaiseCall2InvokeTrans):
    '''Transform a generic PSyIR representation of an Algorithm-layer
    invoke call to an LFRic version with specialised domain-specific
    nodes.

    '''
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
        for idx, call_arg in enumerate(call.children):

            arg_info = []
            if call.argument_names[idx]:
                call_name = f"'{call_arg.value}'"
            elif isinstance(call_arg, ArrayReference):
                # kernel or builtin misrepresented as ArrayReference
                args = call_arg.pop_all_children()
                name = call_arg.name
                if name in builtins:
                    node_type = LFRicBuiltinFunctor
                    type_symbol = call.scope.symbol_table.lookup(name)
                else:
                    node_type = LFRicKernelFunctor
                    type_symbol = call_arg.symbol
                arg_info.append((node_type, type_symbol, args))
            else:
                for fp2_node in call_arg.get_ast_nodes:
                    # This child is a kernel or builtin
                    name = fp2_node.children[0].string
                    if name in builtins:
                        node_type = LFRicBuiltinFunctor
                    else:
                        node_type = LFRicKernelFunctor
                    type_symbol = RaiseCall2InvokeTrans._get_symbol(
                        call, fp2_node)
                    args = RaiseCall2InvokeTrans._parse_args(
                        call_arg, fp2_node)
                    arg_info.append((node_type, type_symbol, args))

            for (node_type, type_symbol, args) in arg_info:
                self._specialise_symbol(type_symbol)
                calls.append(node_type.create(type_symbol, args))

        invoke_call = LFRicAlgorithmInvokeCall.create(
            call.routine, calls, index, name=call_name)
        call.replace_with(invoke_call)


__all__ = ['LFRicRaiseCall2InvokeTrans']
