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
    '''
    Mixin for PSyIR directives associated with a parent Symbol. It
    adds a parent_symbol property (and setter), and a copy method
    which returns an 'unattached' copy, i.e. a directive with the
    parent_symbol set to None. The parent_symbol is then used to ensure
    that a SymbolDirective is not attached to more than one parent
    symbol.

    '''

    def __init__(self):
        super().__init__()
        self._parent_symbol: Optional[Symbol] = None

    def copy(self) -> SymbolDirective:
        '''Create a copy of this node. The parent_symbol is not copied as it
        is expected that the copy will be added to a new parent node.

        :returns: a copy of this node.
        :rtype: :py:class:`psyclone.psyir.nodes.Node`

        '''
        # As a mixin class, this has no superclass with a copy function,
        # but the class it is mixed in with has.
        new_node = super().copy()   # pylint: disable=no-member
        new_node.parent_symbol = None
        return new_node

    @property
    def parent_symbol(self) -> Optional[Symbol]:
        '''
        :returns: the Symbol with which this directive is associated.
        '''
        return self._parent_symbol

    @parent_symbol.setter
    def parent_symbol(self, symbol: Optional[Symbol]):
        '''Set the Symbol associated with this node.

        :param symbol: the Symbol with which this directive is associated.

        :raises TypeError: if symbol is not None but not a Symbol.
        '''
        # Import locally to avoid a circular dependency.
        # pylint: disable=import-outside-toplevel
        from psyclone.psyir.symbols import DataSymbol
        if symbol and not isinstance(symbol, DataSymbol):
            raise TypeError(
                f"The {type(self).__name__} parent_symbol setter expects a "
                f"PSyIR DataSymbol but found '{type(symbol).__name__}'.")
        self._parent_symbol = symbol


__all__ = ["SymbolDirective"]
