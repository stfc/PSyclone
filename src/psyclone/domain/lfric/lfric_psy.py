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

''' This module creates an LFRic-specific Invokes object which controls all
    the required invocation calls. It also overrides the PSy gen method so
    that LFRic-specific PSy module code is generated.
    '''


from psyclone.configuration import Config
from psyclone.domain.lfric import (LFRicConstants, LFRicSymbolTable,
                                   LFRicInvokes)
from psyclone.psyGen import PSy
from psyclone.psyir.nodes import ScopingNode
from psyclone.psyir.symbols import ContainerSymbol


class LFRicPSy(PSy):
    '''
    The LFRic-specific PSy class. This creates an LFRic-specific
    Invokes object (which controls all the required invocation calls).
    It also overrides the PSy gen method so that we generate
    LFRic-specific PSy module code.

    :param invoke_info: object containing the required invocation information
                        for code optimisation and generation.
    :type invoke_info: :py:class:`psyclone.parse.algorithm.FileInfo`

    '''
    def __init__(self, invoke_info):
        # Make sure the scoping node creates LFRicSymbolTables
        # TODO #1954: Remove the protected access using a factory
        ScopingNode._symbol_table_class = LFRicSymbolTable
        Config.get().api = "lfric"
        PSy.__init__(self, invoke_info)

        # Add a wildcard "constants_mod" import at the Container level
        # since kinds are often disconnected.
        const = LFRicConstants()
        const_mod = const.UTILITIES_MOD_MAP["constants"]["module"]
        self.container.symbol_table.add(
            ContainerSymbol(const_mod, wildcard_import=True))

        # Then initialise the Invokes
        self._invokes = LFRicInvokes(invoke_info.calls, self)

    @property
    def name(self):
        '''
        :returns: a name for the PSy layer. This is used as the PSy module
                  name. We override the default value as the Met Office
                  prefer "_psy" to be appended, rather than prepended.
        :rtype: str

        '''
        return self._name + "_psy"

    @property
    def orig_name(self):
        '''
        :returns: the unmodified PSy-layer name.
        :rtype: str

        '''
        return self._name


# ---------- Documentation utils -------------------------------------------- #
# The list of module members that we wish AutoAPI to generate
# documentation for.
__all__ = ['LFRicPSy']
