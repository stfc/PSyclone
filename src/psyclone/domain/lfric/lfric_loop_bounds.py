# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017-2024, Science and Technology Facilities Council.
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

''' This module provides the LFRicLoopBounds Class that handles all variables
    required for specifying loop limits within an LFRic PSy-layer routine.'''

from psyclone.configuration import Config
from psyclone.domain.lfric import LFRicCollection
from psyclone.f2pygen import AssignGen, CommentGen, DeclGen


class LFRicLoopBounds(LFRicCollection):
    '''
    Handles all variables required for specifying loop limits within
    an LFRic PSy-layer routine.
    '''

    def _invoke_declarations(self, parent):
        '''
        Only needed because method is virtual in parent class.

        :param parent: the f2pygen node representing the PSy-layer routine.
        :type parent: :py:class:`psyclone.f2pygen.SubroutineGen`

        '''

    def initialise(self, parent):
        '''
        Updates the f2pygen AST so that all of the variables holding the lower
        and upper bounds of all loops in an Invoke are initialised.

        :param parent: the f2pygen node representing the PSy-layer routine.
        :type parent: :py:class:`psyclone.f2pygen.SubroutineGen`

        '''
        loops = self._invoke.schedule.loops()

        if not loops:
            return

        parent.add(CommentGen(parent, ""))
        parent.add(CommentGen(parent, " Set-up all of the loop bounds"))
        parent.add(CommentGen(parent, ""))

        sym_table = self._invoke.schedule.symbol_table
        config = Config.get()
        api_config = config.api_conf("dynamo0.3")

        for idx, loop in enumerate(loops):

            if loop.loop_type == "null":
                # 'null' loops don't need any bounds.
                continue

            root_name = f"loop{idx}_start"
            lbound = sym_table.find_or_create_integer_symbol(root_name,
                                                             tag=root_name)
            parent.add(AssignGen(parent, lhs=lbound.name,
                                 rhs=loop._lower_bound_fortran()))
            entities = [lbound.name]

            if loop.loop_type != "colour":
                root_name = f"loop{idx}_stop"
                ubound = sym_table.find_or_create_integer_symbol(root_name,
                                                                 tag=root_name)
                entities.append(ubound.name)
                parent.add(AssignGen(parent, lhs=ubound.name,
                                     rhs=loop._upper_bound_fortran()))

            parent.add(DeclGen(parent, datatype="integer",
                               kind=api_config.default_kind["integer"],
                               entity_decls=entities))


# ---------- Documentation utils -------------------------------------------- #
# The list of module members that we wish AutoAPI to generate
# documentation for. (See https://psyclone-ref.readthedocs.io)
__all__ = ['LFRicLoopBounds']
