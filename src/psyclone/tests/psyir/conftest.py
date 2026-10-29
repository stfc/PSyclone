# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2019-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------


''' Module which performs pytest set-up specific to the PSyIR tests. '''

import pytest

from psyclone.parse import ModuleManager
import psyclone.psyir.frontend.fparser2 as fp2
from psyclone.psyir.symbols import DataSymbol, ScalarType


@pytest.fixture(scope="function")
def disable_declaration_check(monkeypatch):
    ''' By default a Reference checks that it has a corresponding entry in
    the Symbol Table. However, this could make constructing tests very
    long winded so this fixture simply disables the check.

    TODO #754 fix all tests so that this fixture is not required.

    '''
    monkeypatch.setattr(
        fp2, "_find_or_create_unresolved_symbol",
        lambda _1, name, symbol_type=None, datatype=None: DataSymbol(
            name, ScalarType.integer_type()))


@pytest.fixture(name="clear_module_manager", scope="function", autouse=True)
def modmanager_fixture(monkeypatch, request):
    '''
    A fixture that ensures every test gets a fresh ModuleManager instance as
    otherwise changes to search paths or file creation/removal is not detected.
    '''
    monkeypatch.setattr(ModuleManager, '_instance', None)
