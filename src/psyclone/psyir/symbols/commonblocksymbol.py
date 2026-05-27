# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017-2026, Science and Technology Facilities Council.
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
# Authors R. W. Ford, A. R. Porter, S. Siso and N. Nobre, STFC Daresbury Lab
#         I. Kavcic, Met Office
#         J. Henrichs, Bureau of Meteorology
# -----------------------------------------------------------------------------

'''This module contains the CommonBlockSymbol class.'''

from psyclone.psyir.symbols.symbol import Symbol


class CommonBlockSymbol(Symbol):
    '''Symbol representing a Fortran COMMON block.

    Named COMMON blocks use the block name as the symbol name (e.g.
    ``CommonBlockSymbol("myblock")``). The *blank* common (``COMMON x, y``
    or ``COMMON // x, y``) is represented by using the empty string as the
    name: ``CommonBlockSymbol("")``.

    The ordered list of :py:class:`~psyclone.psyir.symbols.DataSymbol`\\ s
    that belong to the block is maintained in insertion order, which mirrors
    the order of variables in the original COMMON statement.  This order is
    significant because it determines the shared memory layout.

    :param str name: name of the COMMON block, or ``""`` for blank common.
    :param kwargs: additional keyword arguments forwarded to \
        :py:class:`psyclone.psyir.symbols.Symbol`.
    :type kwargs: unwrapped dict

    :raises TypeError: if *name* is not a ``str``.

    '''

    def __init__(self, name, **kwargs):
        # Allow the empty string (blank COMMON) but still validate type.
        if not isinstance(name, str):
            raise TypeError(
                f"CommonBlockSymbol 'name' attribute should be of type "
                f"'str' but '{type(name).__name__}' found.")
        self._variables: list = []
        # Call Symbol.__init__ directly to bypass the non-empty name check
        # that Symbol enforces for ordinary symbols (Symbol itself accepts
        # any str including "").
        super().__init__(name, **kwargs)

    @property
    def variables(self) -> list:
        '''Return the ordered list of DataSymbols that belong to this block.

        :returns: the ordered variable list.
        :rtype: list[:py:class:`psyclone.psyir.symbols.DataSymbol`]
        '''
        return list(self._variables)

    def add_variable(self, symbol) -> None:
        '''Append *symbol* to the end of the ordered variable list.

        :param symbol: the DataSymbol to add.
        :type symbol: :py:class:`psyclone.psyir.symbols.DataSymbol`

        :raises TypeError: if *symbol* is not a \
            :py:class:`psyclone.psyir.symbols.DataSymbol`.
        :raises ValueError: if *symbol* is already in the variable list.

        '''
        # pylint: disable=import-outside-toplevel
        from psyclone.psyir.symbols.datasymbol import DataSymbol
        if not isinstance(symbol, DataSymbol):
            raise TypeError(
                f"CommonBlockSymbol.add_variable: expected a DataSymbol "
                f"but got '{type(symbol).__name__}'.")
        if symbol in self._variables:
            raise ValueError(
                f"CommonBlockSymbol '{self.name}': variable "
                f"'{symbol.name}' is already in the variable list.")
        self._variables.append(symbol)

    def replace_variable(self, old_sym, new_sym) -> None:
        '''Replace *old_sym* with *new_sym* in the ordered variable list.
        Used by :py:meth:`SymbolTable.replace_symbols_using` when a symbol
        object is replaced (e.g. on deep-copy).

        :param old_sym: the symbol to replace.
        :param new_sym: the replacement symbol.

        :raises ValueError: if *old_sym* is not in the variable list.

        '''
        try:
            idx = self._variables.index(old_sym)
        except ValueError as exc:
            raise ValueError(
                f"CommonBlockSymbol '{self.name}': variable "
                f"'{old_sym.name}' is not in the variable list.") from exc
        self._variables[idx] = new_sym

    def copy(self):
        '''Create and return a copy of this symbol.

        The ``_variables`` list is intentionally left empty in the copy; it
        will be repopulated when the copied table's
        :py:meth:`~psyclone.psyir.symbols.SymbolTable.replace_symbols_using`
        is called.

        :returns: a copy of this CommonBlockSymbol with an empty variable list.
        :rtype: :py:class:`psyclone.psyir.symbols.CommonBlockSymbol`

        '''
        new_sym = type(self)(self.name, visibility=self.visibility,
                             interface=self.interface.copy())
        new_sym.preceding_comment = self.preceding_comment
        new_sym.inline_comment = self.inline_comment
        return new_sym

    def __str__(self):
        block_label = f"/{self.name}/" if self.name else "//"
        return f"{block_label}: CommonBlockSymbol"
