# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2025, Science and Technology Facilities Council.
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
# Author J. Henrichs, Bureau of Meteorology

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
