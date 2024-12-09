# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2023-2024, Science and Technology Facilities Council.
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
from psyclone.parse import ModuleManager


# ----------------------------------------------------------------------------
@pytest.mark.usefixtures("clear_module_manager_instance")
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
@pytest.mark.usefixtures("change_into_tmpdir", "clear_module_manager_instance",
                         "mod_man_test_setup_directories")
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

    mod_man = ModuleManager.get()

    # Add a path to the directory recursively (as default):
    mod_man.add_search_path("d1")
    assert list(mod_man._remaining_search_paths) == ["d1", "d1/d3"]
    # Make sure adding the same directory twice does not add anything
    # to the search path
    mod_man.add_search_path(["d1"])
    assert list(mod_man._remaining_search_paths) == ["d1", "d1/d3"]
    mod_man.add_search_path(["d1/d3"])
    assert list(mod_man._remaining_search_paths) == ["d1", "d1/d3"]

    # Add non-recursive:
    mod_man.add_search_path(["d2"], recursive=False)
    assert list(mod_man._remaining_search_paths) == ["d1", "d1/d3", "d2"]
    # Added same path again with recursive, which should only
    # add the new subdirectories
    mod_man.add_search_path(["d2"], recursive=True)
    assert list(mod_man._remaining_search_paths) == ["d1", "d1/d3", "d2",
                                                     "d2/d4"]

    # Check error handling:
    with pytest.raises(IOError) as err:
        mod_man.add_search_path("does_not_exist")
    assert ("Directory 'does_not_exist' does not exist or cannot be read"
            in str(err.value))


# ----------------------------------------------------------------------------
@pytest.mark.usefixtures("change_into_tmpdir", "clear_module_manager_instance",
                         "mod_man_test_setup_directories")
def test_mod_manager_precedence_preprocessed():
    '''Make sure that a .f90 file is preferred over a .F90 file. Note that
    on linux systems the file names are returned alphabetically, with
    .f90 coming after .F90, which means the module manager handling of
    first seeing a .F90, then the .f90 is properly tested.

    '''
    # Create tmp/d1/a_mod.F90, lower case already exists:
    with open(os.path.join("d1", "a_mod.F90"), "w", encoding="utf-8") as f_out:
        f_out.write("module a_mod\nend module a_mod")

    mod_man = ModuleManager.get()
    mod_man.add_search_path("d1")
    mod_info = mod_man.get_module_info("a_mod")
    # Make sure we get the lower case filename:
    assert mod_info.filename == "d1/a_mod.f90"


# ----------------------------------------------------------------------------
@pytest.mark.usefixtures("change_into_tmpdir", "clear_module_manager_instance",
                         "mod_man_test_setup_directories")
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
    mod_man = ModuleManager.get()

    # Now check adding files:
    assert mod_man._modules == {}

    mod_man._add_all_files_from_dir("d1")
    assert set(mod_man._visited_files.keys()) == {"d1/a_mod.f90"}
    mod_man._add_all_files_from_dir("d1/d3")
    assert set(mod_man._visited_files.keys()) == {"d1/a_mod.f90",
                                                  "d1/d3/b_mod.F90",
                                                  "d1/d3/c_mod.x90"}
    mod_man._add_all_files_from_dir("d2/d4")
    assert set(mod_man._visited_files.keys()) == {"d1/a_mod.f90",
                                                  "d1/d3/b_mod.F90",
                                                  "d1/d3/c_mod.x90",
                                                  "d2/d4/e_mod.F90"}
    # Repeating shouldn't add anything.
    mod_man._add_all_files_from_dir("d2/d4")
    assert set(mod_man._visited_files.keys()) == {"d1/a_mod.f90",
                                                  "d1/d3/b_mod.F90",
                                                  "d1/d3/c_mod.x90",
                                                  "d2/d4/e_mod.F90"}


# ----------------------------------------------------------------------------
@pytest.mark.usefixtures("change_into_tmpdir", "clear_module_manager_instance",
                         "mod_man_test_setup_directories")
def test_mod_manager_get_module_info():
    '''Tests that module information is returned as expected. We use the
    standard directory and file setup (see mod_man_test_setup_directories).
    tmp/d1/a_mod.f90
    tmp/d1/d3/b_mod.F90
    tmp/d1/d3/c_mod.x90
    tmp/d2/d_mod.X90
    tmp/d2/d4/e_mod.F90
    tmp/d2/d4/f_mod.ignore
    '''

    mod_man = ModuleManager.get()
    mod_man.add_search_path("d1")
    mod_man.add_search_path("d2")
    assert list(mod_man._remaining_search_paths) == ["d1", "d1/d3", "d2",
                                                     "d2/d4"]

    # Nothing should be cached yet.
    assert len(mod_man._visited_files) == 0

    # First find a_mod, which will parse the first directory
    mod_info = mod_man.get_module_info("a_mod")
    assert mod_info.filename == "d1/a_mod.f90"
    assert list(mod_man._remaining_search_paths) == ["d1/d3", "d2", "d2/d4"]
    assert set(mod_man._visited_files.keys()) == set(["d1/a_mod.f90"])

    # This should be cached now, so no more change:
    mod_info_cached = mod_man.get_module_info("a_mod")
    assert mod_info == mod_info_cached
    assert list(mod_man._remaining_search_paths) == ["d1/d3", "d2", "d2/d4"]
    assert set(mod_man._visited_files.keys()) == set(["d1/a_mod.f90"])

    # Then look for a second module
    mod_info = mod_man.get_module_info("b_mod")
    assert mod_info.filename == "d1/d3/b_mod.F90"
    assert list(mod_man._remaining_search_paths) == ["d2", "d2/d4"]
    assert set(mod_man._modules.keys()) == set(["a_mod", "b_mod"])

    # Then locate the e_mod, which should remove two paths from
    # the search path:
    mod_info = mod_man.get_module_info("e_mod")
    assert mod_info.filename == "d2/d4/e_mod.F90"
    assert list(mod_man._remaining_search_paths) == []
    assert set(mod_man._visited_files.keys()) == set(["d1/a_mod.f90",
                                                      "d1/d3/b_mod.F90",
                                                      "d1/d3/c_mod.x90",
                                                      "d2/d_mod.X90",
                                                      "d2/d4/e_mod.F90",
                                                      "d2/g_mod.F90",
                                                      "d2/error_mod.F90"])
    assert set(mod_man._modules.keys()) == set(["a_mod", "b_mod",
                                                "e_mod"])

    with pytest.raises(FileNotFoundError) as err:
        mod_man.get_module_info("does_not_exist")
    assert ("Could not find source file for module 'does_not_exist' "
            "in any of the directories 'd1, d1/d3, d2, d2/d4'. You can "
            "add search paths using the '-d' command line option."
            in str(err.value))


# ----------------------------------------------------------------------------
@pytest.mark.usefixtures("change_into_tmpdir", "clear_module_manager_instance",
                         "mod_man_test_setup_directories")
def test_mod_manager_get_all_dependencies_recursively(capsys):
    '''Tests that dependencies are correctly collected recursively. We use
    the standard directory and file setup (see mod_man_test_setup_directories)
    tmp/d1/a_mod.f90       : no dependencies
    tmp/d1/d3/b_mod.F90    : no dependencies
    tmp/d1/d3/c_mod.x90    : depends on a_mod/b_mod
    tmp/d2/d_mod.X90       : depends on c_mod
    tmp/d2/d4/e_mod.F90    : depends on netcdf
    tmp/d2/d4/f_mod.ignore

    '''
    mod_man = ModuleManager.get()
    mod_man.add_search_path("d1")
    mod_man.add_search_path("d2")

    all_d = mod_man.get_all_dependencies_recursively({"d_mod"})
    assert len(all_d.keys()) == 4
    assert all_d["a_mod"] == set()
    assert all_d["b_mod"] == set()
    assert all_d["c_mod"] == set(("a_mod", "b_mod"))
    assert all_d["d_mod"] == set(("c_mod", ))

    # Test ignoring of unknown modules, in this case NetCDF
    all_e = mod_man.get_all_dependencies_recursively({"e_mod"})
    assert len(all_e.keys()) == 1
    assert all_e["e_mod"] == set()
    out, _ = capsys.readouterr()
    assert "Could not find module 'netcdf'" in out

    all_c = mod_man.get_all_dependencies_recursively({"c_mod"})
    assert "a_mod" in all_c
    assert "b_mod" in all_c
    assert "c_mod" in all_c

    # Instruct the module manager to ignore a_mod, which means
    # it should only have b_mod and c_mod in its dependencies:
    mod_man.add_ignore_module("a_mod")
    all_c = mod_man.get_all_dependencies_recursively({"c_mod"})
    assert "a_mod" not in all_c
    assert "b_mod" in all_c
    assert "c_mod" in all_c


# ----------------------------------------------------------------------------
@pytest.mark.usefixtures("clear_module_manager_instance")
def test_mod_man_sort_modules(capsys):
    '''Tests that sorting of modules works as expected.'''

    mod_man = ModuleManager.get()
    # Empty input:
    assert mod_man.sort_modules({}) == []

    # A depends on B:
    deps = {"a": {"b"}, "b": set()}
    assert mod_man.sort_modules(deps) == ["b", "a"]

    deps = {"a": {"b", "c"}, "b": set(), "c": {"b"}}
    assert mod_man.sort_modules(deps) == ["b", "c", "a"]

    deps = {"a": {"b", "c"}, "b": set(), "c": {"netcdf", "b"}}
    deps_sorted = mod_man.sort_modules(deps)
    assert deps_sorted == ["b", "c", "a"]
    out, _ = capsys.readouterr()
    assert "Cannot find module `netcdf` which is used by module 'c'." in out

    # Ignore the netcdf dependencies:
    deps = {"a": {"b", "c"}, "b": set(), "c": {"netcdf", "b"}}
    mod_man.add_ignore_module("netcdf")
    deps_sorted = mod_man.sort_modules(deps)
    assert deps_sorted == ["b", "c", "a"]
    out, _ = capsys.readouterr()
    # There should be no output now:
    assert out == ""

    deps = {"a": {"b", "c"}, "b": {"c"}, "c": {"b"}}
    deps_sorted = mod_man.sort_modules(deps)
    out, _ = capsys.readouterr()
    # The dependencies for a can be given in two orders (b,c or c,b),
    # since it is an unsorted set. Only test for the rest of the output
    # message, especially the part that shows the dependencies between
    # b and c:
    assert ("Circular dependency - cannot sort module dependencies: "
            "{'a': " in out)
    assert "'b': {'c'}, 'c': {'b'}}" in out

    # b and c must be the first two elements, but they can be in any
    # order (unsorted set)
    assert set(deps_sorted[:2]) == {"b", "c"}
    assert deps_sorted[2:] == ["a"]


# ----------------------------------------------------------------------------
@pytest.mark.usefixtures("change_into_tmpdir", "clear_module_manager_instance",
                         "mod_man_test_setup_directories")
def test_mod_manager_add_ignore_modules():
    '''Tests that ignoring modules work. We use the standard
    directory and file setup (see mod_man_test_setup_directories).
    tmp/d1/a_mod.f90
    tmp/d1/d3/b_mod.F90
    tmp/d1/d3/c_mod.x90
    tmp/d2/d_mod.X90
    tmp/d2/d4/e_mod.F90
    tmp/d2/d4/f_mod.ignore

    '''
    mod_man = ModuleManager.get()
    mod_man.add_search_path("d1")

    # First finds a_mod, which will parse the first directory
    mod_man.add_ignore_module("a_mod")
    mod_info = mod_man.get_module_info("a_mod")
    assert mod_info is None
    assert "a_mod" in mod_man.ignores()

    # Just in case verify that other modules are not affected
    mod_info = mod_man.get_module_info("b_mod")
    assert mod_info.filename == "d1/d3/b_mod.F90"
