# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2022-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

'''Provides fixtures for the adjoint tests.'''

import pytest
from psyclone.configuration import Config
from psyclone.domain.lfric import LFRicConstants


@pytest.fixture(name="type_map", scope="module")
def lfric_consts_fixture():
    '''pytest fixture that returns the DATA_TYPE_MAP from LFRicConstants.'''
    return LFRicConstants().DATA_TYPE_MAP


@pytest.fixture(scope="function", autouse=True)
def fixture_lfric_config():
    '''All tests here should use the lfric API config.'''
    Config.get().api = "lfric"
