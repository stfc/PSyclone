# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

'''This module contains the SymbolDirective implementation.'''

from __future__ import annotations
from typing import Optional, TYPE_CHECKING

if TYPE_CHECKING:
    from psyclone.psyir.symbols import Symbol


class SymbolDirective:
    '''Mixin for PSyIR nodes associated with a parent Symbol.'''

    @property
    def parent_symbol(self) -> Optional[Symbol]:
        '''
        :returns: the Symbol associated with this node.
        '''
        return self._parent_symbol

    @parent_symbol.setter
    def parent_symbol(self, symbol: Optional[Symbol]):
        '''Set the Symbol associated with this node.

        :param symbol: the Symbol associated with this node.

        :raises TypeError: if symbol is not a Symbol.
        '''
        # Import locally to avoid a circular dependency.
        # pylint: disable=import-outside-toplevel
        from psyclone.psyir.symbols import Symbol
        if symbol and not isinstance(symbol, Symbol):
            raise TypeError(
                f"The {type(self).__name__} parent_symbol setter expects a "
                f"PSyIR Symbol but found '{type(symbol).__name__}'.")
        self._parent_symbol = symbol


__all__ = ["SymbolDirective"]