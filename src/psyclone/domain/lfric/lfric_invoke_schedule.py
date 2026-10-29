# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

''' This module contains the LFRic-specific InvokeSchedule sub-class which
inherits from the InvokeSchedule class. LFRicInvokeSchedule takes an
Invoke name and a list of parsed KernelCalls as required parameters
which it passes to the base class to create a new SymbolTable for
the new InvokeSchedule.

'''

from typing import Any, Optional

from psyclone.configuration import Config
from psyclone.domain.lfric.lfric_builtins import LFRicBuiltInCallFactory
from psyclone.domain.lfric.lfric_kern_call_factory import LFRicKernCallFactory
from psyclone.parse.algorithm import KernelCall
from psyclone.psyGen import InvokeSchedule
from psyclone.psyir.nodes.node import Node
from psyclone.psyir.symbols.routinesymbol import RoutineSymbol


class LFRicInvokeSchedule(InvokeSchedule):
    ''' The LFRic-specific InvokeSchedule sub-class. This passes the LFRic-
    specific factories for creating kernel and infrastructure calls
    to the base class so it creates the ones we require.

    :param symbol: symbol representing the Invoke.
    :param alg_calls: optional list of KernelCalls parsed from the
                      algorithm layer.
    :param parent: the parent of this node in the PSyIR.

    '''
    def __init__(self, symbol: RoutineSymbol,
                 alg_calls: Optional[list[KernelCall]] = None,
                 parent: Node = None,
                 **kwargs: Any):
        if not alg_calls:
            alg_calls = []
        super().__init__(symbol, LFRicKernCallFactory,
                         LFRicBuiltInCallFactory, alg_calls,
                         parent=parent, **kwargs)

    def node_str(self, colour=True):
        ''' Creates a text summary of this node.

        :param bool colour: whether or not to include control codes for colour.

        :returns: text summary of this node, optionally with control codes
                  for colour highlighting.
        :rtype: str

        '''
        return (self.coloured_name(colour) + "[invoke='" + self.invoke.name +
                "', dm=" + str(Config.get().distributed_memory)+"]")


# ---------- Documentation utils -------------------------------------------- #
# The list of module members that we wish AutoAPI to generate
# documentation for.
__all__ = ['LFRicInvokeSchedule']
