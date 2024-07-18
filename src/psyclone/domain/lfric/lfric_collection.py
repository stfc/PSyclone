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
# Modified I. Kavcic, A. Coughtrie and L. Turner, Met Office
# Modified J. Henrichs, Bureau of Meteorology
# Modified A. B. G. Chalk and N. Nobre, STFC Daresbury Lab

''' This module implements PSyclone LFRic (Dynamo 0.3) API by specialising the
    base class for managing the declaration and initialisation of a group of
    related entities within an Invoke or Kernel stub.'''

# Imports
import abc
from psyclone.domain.lfric import (LFRicSymbolTable, LFRicInvoke,
                                   LFRicKern)
from psyclone.errors import InternalError


class LFRicCollection():
    '''
    Base class for managing the declaration and initialisation of a
    group of related entities within an Invoke or Kernel stub

    :param node: the Kernel or Invoke for which to manage variable \
                 declarations and initialisation.
    :type node: :py:class:`psyclone.domain.lfric.LFRicInvoke` or \
                :py:class:`psyclone.domain.lfric.LFRicKern`

    :raises InternalError: if the supplied node is not an LFRicInvoke or an \
                           LFRicKern.

    '''
    def __init__(self, node):
        if isinstance(node, LFRicInvoke):
            # We are handling declarations/initialisations for an Invoke
            self._invoke = node
            self._kernel = None
            self._symbol_table = self._invoke.schedule.symbol_table
            # The list of Kernel calls we are responsible for
            self._calls = node.schedule.kernels()
        elif isinstance(node, LFRicKern):
            # We are handling declarations for a Kernel stub
            self._invoke = None
            self._kernel = node
            # TODO #719 The symbol table is not connected to other parts of
            # the Stub generation.
            self._symbol_table = LFRicSymbolTable()
            # We only have a single Kernel call in this case
            self._calls = [node]
        else:
            raise InternalError(f"LFRicCollection takes only an LFRicInvoke "
                                f"or an LFRicKern but got: {type(node)}")

        # Whether or not the associated Invoke contains only Kernels that
        # operate on DoFs.
        if self._invoke:
            self._dofs_only = self._invoke.operates_on_dofs_only
        else:
            self._dofs_only = False

    def declarations(self, parent):
        '''
        Insert declarations for all necessary variables into the AST of
        the generated code. Simply calls either '_invoke_declarations()' or
        '_stub_declarations()' depending on whether we're handling an Invoke
        or a Kernel stub.

        :param parent: the node in the f2pygen AST representing the routine \
                       in which to insert the declarations.
        :type parent: :py:class:`psyclone.f2pygen.SubroutineGen`

        :raises InternalError: if neither 'self._invoke' nor 'self._kernel' \
                               are set.

        '''
        if self._invoke:
            self._invoke_declarations(parent)
        elif self._kernel:
            self._stub_declarations(parent)
        else:
            raise InternalError("LFRicCollection has neither a Kernel "
                                "nor an Invoke - should be impossible.")

    def initialise(self, parent):
        '''
        Add code to initialise the entities being managed by this class.
        We do nothing by default - it is up to the sub-class to override
        this method if initialisation is required.

        :param parent: the node in the f2pygen AST to which to add \
                       initialisation code.
        :type parent: :py:class:`psyclone.f2pygen.SubroutineGen`

        '''

    @abc.abstractmethod
    def _invoke_declarations(self, parent):
        '''
        Add all necessary declarations for an Invoke.

        :param parent: node in the f2pygen AST representing the Invoke to \
                       which to add declarations.
        :type parent: :py:class:`psyclone.f2pygen.SubroutineGen`

        '''

    def _stub_declarations(self, parent):
        '''
        Add all necessary declarations for a Kernel stub. Not abstract because
        not all entities need representing within a Kernel.

        :param parent: node in the f2pygen AST representing the Kernel stub \
                       to which to add declarations.
        :type parent: :py:class:`psyclone.f2pygen.SubroutineGen`

        '''


# ---------- Documentation utils -------------------------------------------- #
# The list of module members that we wish AutoAPI to generate
# documentation for (see https://psyclone-ref.readthedocs.io).
__all__ = ['LFRicCollection']
