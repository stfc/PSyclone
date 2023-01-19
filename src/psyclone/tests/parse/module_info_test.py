# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2023, Science and Technology Facilities Council.
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
# Author: J. Henrichs, Bureau of Meteorology


'''Module containing tests for the ModuleInfo class.'''

import pytest

from psyclone.parse import ModuleInfo, ModuleManager
from psyclone.tests.utilities import get_base_path
from psyclone.tests.parse.module_manager_test import (
    mod_man_test_setup_directories)


@pytest.fixture(scope='function', autouse=True)
def clear_module_manager_instance():
    ''' The tests in this module all assume that there is no pre-existing
    ModuleManager object, so this fixture ensures that the module manager
    instance is deleted before and after each test function. The latter
    makes sure that any other test executed next will automatically reload
    the default ModuleManager file.
    '''

    # Enforce loading of the default ModuleManager
    ModuleManager._instance = None

    # Now execute all tests
    yield

    # Enforce loading of the default ModuleManager
    ModuleManager._instance = None


# ----------------------------------------------------------------------------
@pytest.mark.usefixtures("change_into_tmpdir")
def test_module_info():
    '''Tests the module info object.'''
    mod_info = ModuleInfo("a_mod", "file_for_a")
    assert mod_info.filename == "file_for_a"
    assert mod_info._source_code is None

    with pytest.raises(FileNotFoundError) as err:
        mod_info.get_source_code()
    assert ("Could not find file 'file_for_a' when trying to read source "
            "code for module 'a_mod'" in str(err.value))

    # Try to read the file a_mod.f90, which is contained in the d1 directory
    mod_man_test_setup_directories()
    mod_man = ModuleManager.get()
    mod_man.add_search_path("d1")

    mod_info = mod_man.get_module_info("a_mod")
    assert isinstance(mod_info, ModuleInfo)
    assert mod_info._source_code is None
    source_code = mod_info.get_source_code()
    assert "module a_mod" in source_code[:15]
    assert "module a_mod" in mod_info._source_code[:15]
    assert "end module a_mod" in mod_info._source_code

    # Now access the parse tree:
    assert mod_info._parse_tree is None
    parse_tree = mod_info.get_parse_tree()
    assert mod_info._parse_tree is parse_tree


# ----------------------------------------------------------------------------
@pytest.mark.usefixtures("change_into_tmpdir")
def test_mod_info_get_used_modules():
    '''Tests that dependencies are reported as expected. We use the standard
    directory and file setup (see mod_man_test_setup_directories).
    tmp/d1/a_mod.f90       : no dependencies
    tmp/d1/d3/b_mod.F90    : no dependencies
    tmp/d1/d3/c_mod.x90    : depends on a_mod/b_mod
    tmp/d2/d_mod.X90       : depends on c_mod
    tmp/d2/d4/e_mod.F90    : depends on netcdf
    tmp/d2/d4/f_mod.ignore
    '''

    mod_man_test_setup_directories()
    mod_man = ModuleManager.get()
    mod_man.add_search_path("d1")
    mod_man.add_search_path("d2")

    assert mod_man.get_module_info("a_mod").get_used_modules() == []
    assert mod_man.get_module_info("b_mod").get_used_modules() == []

    mod_c_info = mod_man.get_module_info("c_mod")
    dep = mod_c_info.get_used_modules()
    assert dep == [("a_mod", ["a_mod_symbol"]),
                   ("b_mod", ["b_mod_symbol"])]
    dep_cached = mod_c_info.get_used_modules()
    # The cached copy should be the same list:
    assert dep_cached is dep

    dyn_path = get_base_path("dynamo0.3")
    # This will add all subdirectories, including infrastructure:
    mod_man.add_search_path(dyn_path, recursive=True)
    # This module imports the intrinsic module iso_fortran_env,
    # (which should be ignored):
    deps = mod_man.get_module_info("field_r64_mod").get_used_modules()
    for (module, _) in deps:
        assert module != "iso_fortran_env"

    # This module has a 'use' without 'only'. Make sure that
    # the list of symbols is always an empty list
    deps = mod_man.get_module_info("testkern_wtheta_mod").get_used_modules()
    for (module, dep) in deps:
        assert module in ["constants_mod", "argument_mod",
                          "fs_continuity_mod", "kernel_mod"]
        assert dep == []
