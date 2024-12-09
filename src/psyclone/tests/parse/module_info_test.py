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


'''Module containing tests for the ModuleInfo class.'''

import os
import pytest

from fparser.two import Fortran2003

from psyclone.errors import InternalError
from psyclone.parse import FileInfo, ModuleInfo, ModuleInfoError, ModuleManager
from psyclone.psyir.nodes import Container
from psyclone.psyir.symbols import RoutineSymbol
from psyclone.tests.utilities import get_base_path


# -----------------------------------------------------------------------------
@pytest.mark.usefixtures("change_into_tmpdir", "clear_module_manager_instance",
                         "mod_man_test_setup_directories")
def test_module_info():
    '''Tests the module info object.'''
    mod_info = ModuleInfo("a_mod", FileInfo("file_for_a"))
    assert mod_info.filename == "file_for_a"
    assert mod_info.name == "a_mod"

    with pytest.raises(ModuleInfoError) as err:
        mod_info.get_parse_tree()
    assert ("Could not find file 'file_for_a' when trying to read source "
            "code for module 'a_mod'" in str(err.value))

    # Try to read the file a_mod.f90, which is contained in the d1 directory
    mod_man = ModuleManager.get()
    mod_man.add_search_path("d1")
    assert len(mod_man._visited_files) == 0

    mod_info = mod_man.get_module_info("a_mod")
    assert isinstance(mod_info, ModuleInfo)
    # Check that we've only read one file.
    assert len(mod_man._visited_files) == 1
    assert "d1/a_mod.f90" in mod_man._visited_files
    source_code = mod_info.get_source_code()
    assert source_code.startswith("module a_mod")
    assert "end module a_mod" in source_code

    # Now access the parse tree:
    assert mod_info._parse_tree is None
    parse_tree = mod_info.get_parse_tree()
    assert mod_info._parse_tree is parse_tree
    assert isinstance(mod_info._parse_tree, Fortran2003.Program)


# -----------------------------------------------------------------------------
@pytest.mark.usefixtures("clear_module_manager_instance")
def test_module_info_get_psyir(tmpdir, monkeypatch, capsys):
    '''Tests that we can get the PSyIR from the module info object:
    '''
    filepath = os.path.join(tmpdir, "my_mod.f90")
    with open(filepath, "w", encoding="utf-8") as fout:
        fout.write('''
module my_mod
  contains
real function myfunc1()
  myfunc1 = 42.0
end function myfunc1
end module my_mod''')

    mod_info = ModuleInfo("my_mod", FileInfo(filepath))

    psyir = mod_info.get_psyir()
    assert isinstance(psyir, Container)
    assert psyir.name == "my_mod"

    # Create a file with some invalid Fortran content. fparser doesn't check
    # symbol names so it will create a parse tree for it.
    with open(filepath, "w", encoding="utf-8") as fout:
        fout.write('''
module my_mod
  implicit none
contains
  subroutine broken()
    broken = 1
  end subroutine broken
  function broken()
    broken = 2
  end function broken
end module my_mod''')
    mod_info = ModuleInfo("my_mod", FileInfo(filepath))
    psyir = mod_info.get_psyir()
    assert psyir is None
    out, _ = capsys.readouterr()
    assert "Error trying to create PSyIR for " in out

    # Check that we handle the case where get_parse_tree() returns None.
    # The simplest way to do this is to monkeypatch.
    mod_info._psyir = None
    monkeypatch.setattr(mod_info, "get_parse_tree", lambda: None)
    assert mod_info.get_psyir() is None


# -----------------------------------------------------------------------------
def test_mod_info_get_psyir_wrong_file(tmpdir, capsys):
    '''
    Test the error handling in the get_psyir() method.

    '''
    filepath = os.path.join(tmpdir, "my_mod.f90")
    with open(filepath, "w", encoding="utf-8") as fout:
        fout.write('''
module my_mod
  contains
real function myfunc1()
  myfunc1 = 42.0
end function myfunc1
end module my_mod''')

    mod_info = ModuleInfo("wrong_name_mod", FileInfo(filepath))
    with pytest.raises(InternalError) as err:
        mod_info.get_psyir()
    assert ("my_mod.f90' does not contain a module named 'wrong_name_mod'"
            in str(err.value))

    # Break the PSyIR so that, while it is valid, it does not contain the named
    # module.
    mod_info = ModuleInfo("my_mod", FileInfo(filepath))
    mod_info._psyir = Container("other_mod")
    assert mod_info.get_psyir() is None
    out, _ = capsys.readouterr()
    assert ("my_mod.f90' does contain module 'my_mod' but PSyclone is unable "
            "to create the PSyIR of it." in out)


# -----------------------------------------------------------------------------
@pytest.mark.usefixtures("change_into_tmpdir", "clear_module_manager_instance",
                         "mod_man_test_setup_directories")
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

    mod_man = ModuleManager.get()
    mod_man.add_search_path("d1")
    mod_man.add_search_path("d2")

    assert mod_man.get_module_info("a_mod").get_used_modules() == set()
    assert mod_man.get_module_info("b_mod").get_used_modules() == set()

    mod_c_info = mod_man.get_module_info("c_mod")
    assert mod_c_info.name == "c_mod"
    dep = mod_c_info.get_used_modules()
    assert dep == set(("a_mod", "b_mod"))

    dep_cached = mod_c_info.get_used_modules()
    # Calling the method a second time should return the same
    # (cached) list object
    assert dep_cached is dep

    dyn_path = get_base_path("lfric")
    # This will add all subdirectories, including infrastructure:
    mod_man.add_search_path(dyn_path, recursive=True)
    # This module imports the intrinsic module iso_fortran_env,
    # (which should be ignored):
    deps = mod_man.get_module_info("field_r64_mod").get_used_modules()
    assert "iso_fortran_env" not in deps

    # This module has a 'use' without 'only'. Make sure that
    # the modules are still added to the dependencies, but that no
    # symbols are added:
    mod_info = mod_man.get_module_info("testkern_wtheta_mod")
    deps = mod_info.get_used_modules()
    for module in deps:
        assert module in ["constants_mod", "argument_mod",
                          "fs_continuity_mod", "kernel_mod"]
        assert mod_info.get_used_symbols_from_modules()[module] == set()


# -----------------------------------------------------------------------------
@pytest.mark.usefixtures("change_into_tmpdir", "clear_module_manager_instance",
                         "mod_man_test_setup_directories")
def test_mod_info_get_used_symbols_from_modules():
    '''Tests that symbols from dependencies are reported as expected. We
    use the standard directory and file setup (see
    mod_man_test_setup_directories).
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

    mod_info = mod_man.get_module_info("c_mod")
    assert mod_info._used_symbols_from_module is None
    used_symbols = mod_info.get_used_symbols_from_modules()
    assert used_symbols["a_mod"] == {"a_mod_symbol"}
    assert used_symbols["b_mod"] == {"b_mod_symbol"}

    used_symbols_cached = mod_info.get_used_symbols_from_modules()
    # The cached copy should be the same dictionary
    assert used_symbols_cached is used_symbols


# -----------------------------------------------------------------------------
@pytest.mark.usefixtures("change_into_tmpdir", "clear_module_manager_instance")
def test_mod_info_get_psyir(capsys, tmpdir):
    '''This tests the handling of PSyIR representation of the module.
    '''

    mod_man = ModuleManager.get()
    dyn_path = get_base_path("lfric")
    mod_man.add_search_path(f"{dyn_path}/driver_creation", recursive=False)

    mod_info = mod_man.get_module_info("testkern_import_symbols_mod")
    assert mod_info._psyir is None
    psyir = mod_info.get_psyir()
    assert isinstance(psyir, Container)
    assert psyir.name == "testkern_import_symbols_mod"
    # Make sure the PSyIR is cached:
    assert mod_info._psyir.children[0] is psyir
    # Test that we get the cached value (and not a new instance)
    psyir_cached = mod_info.get_psyir()
    assert psyir_cached is psyir

    # Test that a file that can't be converted to PSyIR returns an
    # empty FileContainer.
    with open("broken_mod.f90", "w", encoding="utf-8") as mod_file:
        mod_file.write('''module broken_mod
  ERROR
end module broken_mod''')
    mod_man.add_search_path(str(tmpdir), recursive=False)
    broken_builtins = mod_man.get_module_info("broken_mod")
    broken_builtins_psyir = broken_builtins.get_psyir()
    # We should get no PSyIR
    assert broken_builtins_psyir is None

    out, _ = capsys.readouterr()
    assert "Error parsing" in out


# -----------------------------------------------------------------------------
@pytest.mark.usefixtures("change_into_tmpdir", "clear_module_manager_instance",
                         "mod_man_test_setup_directories")
def test_generic_interface():
    '''Tests that a generic interface works as expected. This test relies on
    the directories and files set up by `mod_man_test_setup_directories`:
    the module `g_mod` contains:
        interface myfunc
            procedure myfunc1
            procedure myfunc2
        end interface myfunc
    Therefore, the ModuleInfo object needs to contain `myfunc`, `myfunc1`, and
    `myfunc2`

    '''
    mod_man = ModuleManager.get()
    mod_man.add_search_path("d1")
    mod_man.add_search_path("d2")

    mod_info = mod_man.get_module_info("g_mod")

    # It should contain all two concrete functions
    contr = mod_info.get_psyir()

    assert contr.find_routine_psyir("myfunc1")
    assert contr.find_routine_psyir("myfunc2")


# -----------------------------------------------------------------------------
@pytest.mark.usefixtures("change_into_tmpdir", "clear_module_manager_instance",
                         "mod_man_test_setup_directories")
def test_module_info_extract_import_information_error():
    '''Test handling of files that cannot be parsed in
    _extract_import_information. This relies on the directories and file setup
    my `mod_man_test_setup_directories`, which will create a file
    `d2/error_mod.f90`, which is invalid Fortran.

    '''
    # TODO 2120: Once proper error handling is implemented, this should
    # likely just raise an exception.
    mod_man = ModuleManager.get()
    mod_man.add_search_path("d2")
    mod_info = mod_man.get_module_info("error_mod")
    assert mod_info.name == "error_mod"

    assert mod_info._used_modules is None
    assert mod_info._used_symbols_from_module is None
    mod_info._extract_import_information()
    # Make sure the internal attributes are set to not None to avoid
    # trying to parse them again later
    assert mod_info._used_modules == set()
    assert mod_info._used_symbols_from_module == {}


# -----------------------------------------------------------------------------
def test_module_info_get_symbol(tmpdir, monkeypatch):
    '''Test the get_symbol() method of ModuleInfo.'''
    filepath = os.path.join(tmpdir, "my_mod.f90")
    with open(filepath, "w", encoding="utf-8") as fout:
        fout.write('''
module my_mod
  contains
real function myfunc1()
  myfunc1 = 42.0
end function myfunc1
end module my_mod''')

    minfo = ModuleInfo("my_mod", FileInfo(filepath))
    assert isinstance(minfo.get_symbol("myfunc1"), RoutineSymbol)
    # A Symbol that doesn't exist.
    assert minfo.get_symbol("amos") is None
    # When no Container has been created. Monkeypatch get_psyir() to simplify
    # this.
    monkeypatch.setattr(minfo, "get_psyir", lambda: None)
    assert minfo.get_symbol("amos") is None
