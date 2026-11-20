# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

'''Tests for the SymbolDirective mixin.'''

import pytest

from psyclone.psyir.nodes import OMPDeclareTargetVariable
from psyclone.psyir.symbols import DataSymbol, ScalarType


def test_symbol_directive_parent_symbol():
    '''Test that the parent symbol property accepts only DataSymbols.'''
    directive = OMPDeclareTargetVariable()
    assert directive.parent_symbol is None

    symbol = DataSymbol("var", ScalarType.integer_single_type())
    directive.parent_symbol = symbol
    assert directive.parent_symbol is symbol

    with pytest.raises(TypeError, match=(
            "The OMPDeclareTargetVariable parent_symbol setter expects a "
            "PSyIR DataSymbol but found 'str'.")):
        directive.parent_symbol = "var"

    directive.parent_symbol = None
    assert directive.parent_symbol is None


def test_symbol_directive_copy_clears_parent_symbol():
    '''Test that copying a SymbolDirective leaves it unattached.'''
    directive = OMPDeclareTargetVariable()
    symbol = DataSymbol("var", ScalarType.integer_single_type())
    directive.parent_symbol = symbol

    copied_directive = directive.copy()

    assert copied_directive is not directive
    assert copied_directive.parent_symbol is None
    assert directive.parent_symbol is symbol
