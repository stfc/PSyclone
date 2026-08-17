# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2025-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

"""
Provides fixtures to set-up the ModuleManager for testing of the
driver creation.
"""

from pathlib import Path
import pytest

from psyclone.parse import ModuleManager
from psyclone.tests.utilities import get_base_path, get_infrastructure_path


@pytest.fixture(scope='function')
def init_module_manager_lfric():
    '''This fixture makes sure we are getting a new ModuleManager,
    setup to find the LFRic related files (infrastructure, test files,
    and extraction library). This fixture also ensures that the ModuleManager
    instance is deleted after each test function, which makes sure that any
    other test executed next will automatically reload the default
    ModuleManager file.
    '''

    test_files_dir = get_base_path("lfric")
    infrastructure_path = Path(get_infrastructure_path("lfric"))
    # Define the path to the ReadKernelData module (which contains functions
    # to read extracted data from a file) relative to the infrastructure path:
    psyclone_root = infrastructure_path.parents[2]
    extraction_lib = psyclone_root / "lib" / "extract" / "binary" / "lfric"
    # Enforce loading of the default ModuleManager
    ModuleManager._instance = None

    module_manager = ModuleManager.get()
    module_manager.add_search_path(test_files_dir)
    module_manager.add_search_path(str(infrastructure_path))
    module_manager.add_search_path(str(extraction_lib))

    # Now execute all tests
    yield

    # Enforce loading of the default ModuleManager
    ModuleManager._instance = None
