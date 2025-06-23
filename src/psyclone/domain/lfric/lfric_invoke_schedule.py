# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017-2025, Science and Technology Facilities Council.
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
# Authors R. W. Ford, A. R. Porter and S. Siso, STFC Daresbury Lab
# Modified I. Kavcic, A. Coughtrie, L. Turner and O. Brunt, Met Office
# Modified J. Henrichs, Bureau of Meteorology
# Modified A. B. G. Chalk and N. Nobre, STFC Daresbury Lab

''' This module contains the LFRic-specific InvokeSchedule sub-class which
inherits from the InvokeSchedule class. LFRicInvokeSchedule takes an
Invoke name and a list of parsed KernelCalls as required parameters
which it passes to the base class to create a new SymbolTable for
the new InvokeSchedule.

'''

from psyclone.configuration import Config
from psyclone.domain.lfric.lfric_builtins import LFRicBuiltInCallFactory
from psyclone.domain.lfric.lfric_kern_call_factory import LFRicKernCallFactory
from psyclone.domain.lfric.lfric_symbol_table import LFRicSymbolTable
from psyclone.psyGen import InvokeSchedule


class LFRicInvokeSchedule(InvokeSchedule):
    ''' The LFRic-specific InvokeSchedule sub-class. This passes the LFRic-
    specific factories for creating kernel and infrastructure calls
    to the base class so it creates the ones we require.

    :param symbol: symbol representing the Invoke.
    :type symbol: :py:class:`psyclone.psyir.symbols.RoutineSymbol`
    :param alg_calls: optional list of KernelCalls parsed from the
                      algorithm layer.
    :type alg_calls: Optional[list of
                              :py:class:`psyclone.parse.algorithm.KernelCall`]
    :param parent: the parent of this node in the PSyIR.
    :type parent: :py:class:`psyclone.psyir.nodes.Node`

    '''
    # LFRicInvokeSchedule always uses an LFRicSymbolTable for its inner scope
    # symbol table.
    _symbol_table_class = LFRicSymbolTable

    def __init__(self, symbol, alg_calls=None, parent=None, **kwargs):
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
