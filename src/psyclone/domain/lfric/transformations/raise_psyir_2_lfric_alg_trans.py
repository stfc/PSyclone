# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2021-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

'''Specialise generic PSyIR to LFRic algorithm-layer PSyIR. Currently
we transform PSyIR invoke calls to LFRic algorithm-layer-specific
invoke calls which uses specialised classes.

'''
from psyclone.domain.common.transformations import RaisePSyIR2AlgTrans
from psyclone.domain.lfric.algorithm.psyir import (
    LFRicBuiltinFunctorFactory, LFRicKernelFunctor, LFRicAlgorithmInvokeCall)


class RaisePSyIR2LFRicAlgTrans(RaisePSyIR2AlgTrans):
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
        :type options: Optional[Dict[str, Any]]

        '''
        self.validate(call, options=options)

        call_name = None
        calls = []
        table = call.scope.symbol_table

        factory = LFRicBuiltinFunctorFactory.get()

        for idx, call_arg in enumerate(call.arguments):

            if call.argument_names[idx]:
                call_name = f"{call_arg.value}"
            else:
                symbol = call_arg.routine.symbol
                args = call_arg.pop_all_children()[1:]
                try:
                    calls.append(factory.create(symbol.name, table, args))
                except KeyError:
                    # No match for a builtin so create a user-defined kernel.
                    self._specialise_symbol(symbol)
                    calls.append(LFRicKernelFunctor.create(symbol, args))

        invoke_call = LFRicAlgorithmInvokeCall.create(
            call.routine.symbol, calls, index, name=call_name)

        # Copy across any comments.
        invoke_call.preceding_comment = call.preceding_comment
        invoke_call.inline_comment = call.inline_comment

        call.replace_with(invoke_call)


__all__ = ['RaisePSyIR2LFRicAlgTrans']
