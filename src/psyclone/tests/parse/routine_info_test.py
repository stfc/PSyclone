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


'''Module containing tests for the RoutineInfo class.'''

import pytest

from fparser.two.Fortran2003 import (Subroutine_Subprogram)
from fparser.two.utils import walk

from psyclone.core import Signature
from psyclone.parse import ModuleInfo, ModuleManager
from psyclone.parse.routine_info import (GenericRoutineInfo, RoutineInfo,
                                         RoutineInfoBase)
from psyclone.psyir.nodes import FileContainer


# -----------------------------------------------------------------------------
def test_routine_info_base():
    '''Tests the RoutineInfoBase class.'''
    mod_info = ModuleInfo("a_mod", "file_for_a")
    routine_info = RoutineInfoBase(mod_info, "a_subroutine")

    assert routine_info.name == "a_subroutine"
    psyir = FileContainer("dummy")
    routine_info.set_psyir(psyir)
    assert routine_info.get_psyir() is psyir

    assert routine_info.module_info is mod_info


# -----------------------------------------------------------------------------
@pytest.mark.usefixtures("change_into_tmpdir", "clear_module_manager_instance",
                         "mod_man_test_setup_directories")
def test_routine_info_get_psyir():
    '''Tests that we can get the PSyIR from the module info object:
    '''

    mod_man = ModuleManager.get()
    mod_man.add_search_path("d2")
    mod_info = mod_man.get_module_info("g_mod")

    routine_info = mod_info.get_routine_info("myfunc1")
    assert routine_info
    psyir = routine_info.get_psyir()
    assert psyir.name == "myfunc1"


# -----------------------------------------------------------------------------
@pytest.mark.usefixtures("change_into_tmpdir", "clear_module_manager_instance",
                         "mod_man_test_setup_directories")
def test_routine_info_constructor():
    '''Tests the RoutineInfo class:
    '''

    mod_man = ModuleManager.get()
    mod_man.add_search_path("d2")
    mod_info = mod_man.get_module_info("g_mod")

    # Get the AST from the module, and manually create the routine info:
    ast = mod_info.get_parse_tree()
    # Take the first function in this module ('myfunc1')
    func1 = walk(ast, Subroutine_Subprogram)[0]
    routine_info = RoutineInfo(mod_info, func1)
    assert routine_info.name == "myfunc1"


# -----------------------------------------------------------------------------
@pytest.mark.usefixtures("change_into_tmpdir", "clear_module_manager_instance",
                         "mod_man_test_setup_directories")
def test_routine_info_var_accesses():
    '''Tests the RoutineInfo class:
    '''

    mod_man = ModuleManager.get()
    mod_man.add_search_path("d2")
    mod_info = mod_man.get_module_info("g_mod")
    routine_info = mod_info.get_routine_info("myfunc1")
    var_accesses = routine_info.get_var_accesses()
    assert str(var_accesses) == ("a: WRITE, module_var: READ, "
                                 "module_var_1: READ, p: READ")


# -----------------------------------------------------------------------------
@pytest.mark.usefixtures("change_into_tmpdir", "clear_module_manager_instance",
                         "mod_man_test_setup_directories")
def test_generic_routine_info():
    '''Tests the GenericRoutineInfo class, which should return the combined
    results from all individual subroutines. The example in g_mod declares
    myfunc to be myfunc1 and myfunc2, which are implemented as:
        subroutine myfunc1() ...
            a = p + module_var_1 + module_var
        end subroutine myfunc1

        subroutine myfunc2() ...
            module_var = p + module_var_2
        end subroutine myfunc2
    So they both use the module variable module_var, but myfunc1 reads it,
    myfunc2 writes it. '''
    mod_man = ModuleManager.get()
    mod_man.add_search_path("d2")
    mod_info = mod_man.get_module_info("g_mod")
    routine_info = mod_info.get_routine_info("myfunc")
    assert isinstance(routine_info, GenericRoutineInfo)
    all_non_locals = routine_info.get_non_local_symbols()
    # Both functions of the generic interface use 'module_var',
    # and in addition my_func1 uses module_var_1, myfunc2 uses module_var_2
    # So three variables should be reported, i.e. module_var should only
    # be reported once (even though it is used in both functions), and
    # each variable specific to the two functions:
    expected = set([("reference", "g_mod", Signature("module_var_1"),
                     'module_var_1:READ(0)'),
                    ("reference", "g_mod", Signature("module_var_2"),
                     'module_var_2:READ(0)'),
                    ("reference", "g_mod", Signature("module_var"),
                     'module_var:READ(0)'),
                    ("reference", "g_mod", Signature("module_var"),
                     'module_var:WRITE(0)')])
    # Convert the access info to a string for easy comparison:
    assert (set((i[0], i[1], i[2], str(i[3])) for i in all_non_locals) ==
            expected)
