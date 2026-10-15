# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2021-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------


''' Common pytest fixtures for the frontend directory. '''

import pytest
from fparser.two.symbol_table import SYMBOL_TABLES


# This is similar to the top-level pytest 'parse' fixture but that one has
# a deprecation notice (#1188). However, it is appropriate to use it inside the
# Fortran frontend tests because the fparser dependency should be encapsulated
# inside this module and the fixture won't go away.
@pytest.fixture(scope="function", name="f2008_parser")
def fixture_f2008_parser(_session_parser):
    '''
    Initialise and return an fparser2 object with the Fortran2008 standard
    after clearing any existing symbol tables.

    '''
    SYMBOL_TABLES.clear()
    return _session_parser
