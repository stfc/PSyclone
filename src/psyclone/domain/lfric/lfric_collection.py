# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

''' This module implements PSyclone LFRic API by specialising the
    base class for managing the declaration and initialisation of a group of
    related entities within an Invoke or Kernel stub.'''

import abc
from typing import List

from psyclone.domain.lfric.lfric_invoke import LFRicInvoke
from psyclone.domain.lfric.lfric_kern import LFRicKern
from psyclone.errors import InternalError
from psyclone.psyGen import Kern


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
        elif isinstance(node, LFRicKern):
            # We are handling declarations for a Kernel stub
            self._invoke = None
            self._kernel = node
        else:
            raise InternalError(f"LFRicCollection takes only an LFRicInvoke "
                                f"or an LFRicKern but got: {type(node)}")

        # Whether or not the associated Invoke contains only Kernels that
        # operate on DoFs.
        if self._invoke:
            self._dofs_only = self._invoke.operates_on_dofs_only
        else:
            self._dofs_only = False

    @property
    def symtab(self):
        '''
        :returns: associated symbol table.
        :rtype: :py:class:`psyclone.psyir.symbols.SymbolTable`
        '''
        if self._invoke:
            return self._invoke.schedule.symbol_table
        # Otherwise it is a kernel
        return self._kernel._stub_symbol_table

    @property
    def kernel_calls(self) -> List[Kern]:
        '''
        :returns: associated kernels calls.

        '''
        if self._invoke:
            return self._invoke.schedule.kernels()
        # Otherwise it is a kernel
        return [self._kernel]

    @abc.abstractmethod
    def initialise(self, cursor: int) -> int:
        '''
        Add code to initialise the entities being managed by this class.
        We do nothing by default - it is up to the sub-class to override
        this method if initialisation is required.

        :param cursor: position where to add the next initialisation
            statements.
        :returns: Updated cursor value.

        '''

    @abc.abstractmethod
    def invoke_declarations(self) -> None:
        '''
        Add necessary Invoke declarations for this Collection.

        By default we just sanity check that the class is appropriately
        initialised - it is up to the sub-class to add required declarations.

        :raises InternalError: if the class has been instantiated for a
            kernel and not an invoke.

        '''
        if not self._invoke:
            raise InternalError(
                f"invoke_declarations() can only be called with an "
                f"{type(self).__name__} instantiated for an invoke (not a "
                f"kernel).")

    def stub_declarations(self):
        '''
        Add necessary Kernel Stub declarations for this Collection.

        By default we just sanity check that the class is appropriately
        initialised - it is up to the sub-class to add required declarations.

        :raises InternalError: if the class has been instantiated for an
            invoke and not a kernel.
        '''
        if not self._kernel:
            raise InternalError(
                f"stub_declarations() can only be called with an "
                f"{type(self).__name__} instantiated for a kernel (not an "
                f"invoke).")


# ---------- Documentation utils -------------------------------------------- #
# The list of module members that we wish AutoAPI to generate
# documentation for.
__all__ = ['LFRicCollection']
