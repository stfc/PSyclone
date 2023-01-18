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


'''Module containing py.test tests for the ModuleManager.'''

import os

import pytest

from psyclone.errors import InternalError
from psyclone.parse import ModuleInfo, ModuleManager
from psyclone.tests.utilities import get_base_path


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


# ----------------------------------------------------------------------------
def test_mod_manager_instance():
    '''Tests the singleton functionality.'''
    mod_man1 = ModuleManager.get()
    mod_man2 = ModuleManager.get()
    assert mod_man1 is mod_man2

    with pytest.raises(InternalError) as err:
        ModuleManager()

    assert ("You need to use 'ModuleManager.get()' to get the singleton "
            "instance." in str(err.value))


# ----------------------------------------------------------------------------
def test_mod_manager_get_modules_in_file():
    '''Tests that file names are mapped as expected to the module they
    contain. '''
    mod_man = ModuleManager.get()
    assert mod_man.get_modules_in_file("a_mod.f90") == ["a_mod"]
    assert mod_man.get_modules_in_file("b_mod.F90") == ["b_mod"]
    assert mod_man.get_modules_in_file("c_mod.x90") == ["c_mod"]
    assert mod_man.get_modules_in_file("d_mod.X90") == ["d_mod"]
    assert mod_man.get_modules_in_file("d.f90") == []


# ----------------------------------------------------------------------------
def mod_man_test_setup_directories():
    '''Sets up a directory and file structure for several of the following
    tests. The following structure is created - note that each Fortran file
    declares a module of the same name as the basename of the file:
    tmp/d1/a_mod.f90       : no dependencies
    tmp/d1/d3/b_mod.F90    : no dependencies
    tmp/d1/d3/c_mod.x90    : depends on a_mod/b_mod
    tmp/d2/d_mod.X90       : depends on c_mod
    tmp/d2/d4/e_mod.F90    : depends on netcdf
    tmp/d2/d4/f_mod.ignore
    '''

    os.makedirs("d1/d3")
    os.makedirs("d2/d4")
    for (name, path, dependencies) in [("a_mod.f90", "d1", []),
                                       ("b_mod.F90", "d1/d3", []),
                                       ("c_mod.x90", "d1/d3", ["a_mod",
                                                               "b_mod"]),
                                       ("d_mod.X90", "d2", ["c_mod"]),
                                       ("e_mod.F90", "d2/d4", ["netcdf"]),
                                       ("f_mod.ignore", "d2/d4", [])]:
        # Create a list of "use a_mod, only: a_mod_symbol" statements
        uses = "\n".join(f"use {dep}, only: {dep}_symbol"
                         for dep in dependencies)
        base, _ = os.path.splitext(name)
        with open(os.path.join(path, name), "w", encoding="utf-8") as f_out:
            f_out.write(f"module {base}\n{uses}\nend module {base}")


# ----------------------------------------------------------------------------
@pytest.mark.usefixtures("change_into_tmpdir")
def test_mod_manager_directory_reading():
    '''Tests that directories are read as expected. We use the standard
    directory and file setup (see mod_man_test_setup_directories).
    tmp/d1/a_mod.f90
    tmp/d1/d3/b_mod.F90
    tmp/d1/d3/c_mod.x90
    tmp/d2/d_mod.X90
    tmp/d2/d4/e_mod.F90
    tmp/d2/d4/f_mod.ignore
    '''

    mod_man_test_setup_directories()
    mod_man = ModuleManager.get()

    # Add a path to the directory recursively (as default):
    mod_man.add_search_path("d1")
    assert mod_man._search_paths == ["d1", "d1/d3"]
    # Make sure adding the same directory twice does not add anything
    # to the search path
    mod_man.add_search_path(["d1"])
    assert mod_man._search_paths == ["d1", "d1/d3"]
    mod_man.add_search_path(["d1/d3"])
    assert mod_man._search_paths == ["d1", "d1/d3"]

    # Add non-recursive:
    mod_man.add_search_path(["d2"], recursive=False)
    assert mod_man._search_paths == ["d1", "d1/d3", "d2"]
    # Added same path again with recursive, which should only
    # add the new subdirectories
    mod_man.add_search_path(["d2"], recursive=True)
    assert mod_man._search_paths == ["d1", "d1/d3", "d2", "d2/d4"]

    # Check error handling:
    with pytest.raises(IOError) as err:
        mod_man.add_search_path("does_not_exist")
    assert "Directory 'does_not_exist' does not exist" in str(err.value)


# ----------------------------------------------------------------------------
@pytest.mark.usefixtures("change_into_tmpdir")
def test_mod_manager_add_files_from_dir():
    '''Tests that directories are read as expected. We use the standard
    directory and file setup (see mod_man_test_setup_directories).
    tmp/d1/a_mod.f90
    tmp/d1/d3/b_mod.F90
    tmp/d1/d3/c_mod.x90
    tmp/d2/d_mod.X90
    tmp/d2/d4/e_mod.F90
    tmp/d2/d4/f_mod.ignore
    '''

    mod_man_test_setup_directories()
    mod_man = ModuleManager.get()

    # Now check adding files:
    assert mod_man._mod_2_filename == {}

    mod_man._add_all_files_from_dir("d1")
    assert set(mod_man._mod_2_filename.keys()) == {"a_mod"}
    mod_man._add_all_files_from_dir("d1/d3")
    assert set(mod_man._mod_2_filename.keys()) == {"a_mod", "b_mod", "c_mod"}
    mod_man._add_all_files_from_dir("d2/d4")
    assert set(mod_man._mod_2_filename.keys()) == {"a_mod", "b_mod",
                                                   "c_mod", "e_mod"}


# ----------------------------------------------------------------------------
@pytest.mark.usefixtures("change_into_tmpdir")
def test_mod_manager_get_file_for_module():
    '''Tests that directories are read as expected. We use the standard
    directory and file setup (see mod_man_test_setup_directories).
    tmp/d1/a_mod.f90
    tmp/d1/d3/b_mod.F90
    tmp/d1/d3/c_mod.x90
    tmp/d2/d_mod.X90
    tmp/d2/d4/e_mod.F90
    tmp/d2/d4/f_mod.ignore
    '''

    mod_man_test_setup_directories()
    mod_man = ModuleManager.get()
    mod_man.add_search_path("d1")
    mod_man.add_search_path("d2")
    assert mod_man._search_paths == ["d1", "d1/d3", "d2", "d2/d4"]

    # Nothing should be cached yet.
    assert len(mod_man._mod_2_filename) == 0

    # First finds a_mod, which will parse the first directory
    mod_info = mod_man.get_module_info("a_mod")
    assert mod_info.filename == "d1/a_mod.f90"
    assert mod_man._search_paths == ["d1/d3", "d2", "d2/d4"]
    assert set(mod_man._mod_2_filename.keys()) == set(["a_mod"])

    # This should be cached now, so no more change:
    mod_info_cached = mod_man.get_module_info("a_mod")
    assert mod_info == mod_info_cached
    assert mod_man._search_paths == ["d1/d3", "d2", "d2/d4"]
    assert set(mod_man._mod_2_filename.keys()) == set(["a_mod"])

    # Then parse the second file, it should cache two modules (b and c):
    mod_info = mod_man.get_module_info("b_mod")
    assert mod_info.filename == "d1/d3/b_mod.F90"
    assert mod_man._search_paths == ["d2", "d2/d4"]
    assert set(mod_man._mod_2_filename.keys()) == set(["a_mod", "b_mod",
                                                      "c_mod"])

    # Then parse the e_mod, which should remove two paths from
    # the search path:
    mod_info = mod_man.get_module_info("e_mod")
    assert mod_info.filename == "d2/d4/e_mod.F90"
    assert mod_man._search_paths == []
    assert set(mod_man._mod_2_filename.keys()) == set(["a_mod", "b_mod",
                                                       "c_mod", "d_mod",
                                                       "e_mod"])

    with pytest.raises(FileNotFoundError) as err:
        mod_man.get_module_info("does_not_exist")
    assert ("Could not find source file for module 'does_not_exist'."
            in str(err.value))


# ----------------------------------------------------------------------------
@pytest.mark.usefixtures("change_into_tmpdir")
def test_mod_manager_find_modules_used_in():
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

    assert mod_man.find_modules_used_in("a_mod") == []
    assert mod_man.find_modules_used_in("b_mod") == []

    dep = mod_man.find_modules_used_in("c_mod")
    assert dep == [("a_mod", ["a_mod_symbol"]),
                   ("b_mod", ["b_mod_symbol"])]
    dep_cached = mod_man.find_modules_used_in("c_mod")
    # The cached copy should be the same list:
    assert dep_cached is dep

    # Check handling of a non-existing module
    dep = mod_man.find_modules_used_in("does_not_exist")
    assert dep == []

    dyn_path = get_base_path("dynamo0.3")
    # This will add all subdirectories, including infrastructure:
    mod_man.add_search_path(dyn_path, recursive=True)
    # This module imports the intrinsic module iso_fortran_env,
    # (which should be ignored):
    deps = mod_man.find_modules_used_in("field_r64_mod")
    for (module, _) in deps:
        assert module != "iso_fortran_env"

    # This module has a 'use' without 'only'. Make sure that
    # the list of symbols is always an empty list
    deps = mod_man.find_modules_used_in("testkern_wtheta_mod")
    for (module, dep) in deps:
        assert module in ["constants_mod", "argument_mod",
                          "fs_continuity_mod", "kernel_mod"]
        assert dep == []
