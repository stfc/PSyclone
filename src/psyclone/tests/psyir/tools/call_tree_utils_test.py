# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2020-2023, Science and Technology Facilities Council.
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
# Author: A. R. Porter, STFC Daresbury Lab
# Modified: S. Siso, STFC Daresbury Lab
# Modified: J. Henrichs, Bureau of Meteorology
# -----------------------------------------------------------------------------

''' This module contains the pytest tests for the Routine class. '''

import os
import pytest

from psyclone.core import Signature
from psyclone.domain.lfric import LFRicKern
from psyclone.parse import ModuleManager
from psyclone.psyGen import BuiltIn
from psyclone.psyir.nodes import (Reference, Schedule)
from psyclone.psyir.tools import CallTreeUtils
from psyclone.tests.utilities import get_base_path, get_invoke


# -----------------------------------------------------------------------------
@pytest.mark.usefixtures("clear_module_manager_instance")
def test_routine_get_used_symbols_from_modules():
    '''Tests that we get the used symbols from a routine reported correctly.
    '''
    test_dir = os.path.join(get_base_path("dynamo0.3"), "driver_creation")

    mod_man = ModuleManager.get()
    mod_man.add_search_path(test_dir)

    mod_info = mod_man.get_module_info("testkern_import_symbols_mod")
    psyir = mod_info.get_psyir("testkern_import_symbols_code")
    ctu = CallTreeUtils()
    non_locals = ctu.get_non_local_symbols(psyir)

    # Check that the expected symbols, modules and internal type are correct:
    expected = [("unknown", "constants_mod", "eps"),
                ("reference", "testkern_import_symbols_mod",
                 "dummy_module_variable"),
                ("routine", "module_with_var_mod", "module_subroutine"),
                ("unknown", "module_with_var_mod", "module_var_a"),
                ("routine", "testkern_import_symbols_mod", "local_subroutine"),
                ("routine", None, "unknown_subroutine"),
                ]
    # First check the length. This will ensure that e.g. the constant in the
    # example subroutine is not reported.
    assert len(non_locals) == len(expected)
    for sym_type, module, name in expected:
        assert ((sym_type, module, Signature(name)) in
                [(i[0], i[1], i[2]) for i in non_locals])

    # Check the handling of a symbol that is not found: _compute_non_locals
    # should return None:
    ref = psyir.walk(Reference)[0]
    # Change the name of the symbol so that it is not in the symbol table:
    ref.symbol._name = "not-in-any-symbol-table"
    psyir = mod_info.get_psyir("testkern_import_symbols_code")
    assert ctu._compute_non_locals_references(ref, ref.symbol) is None


# -----------------------------------------------------------------------------
@pytest.mark.usefixtures("clear_module_manager_instance")
def test_routine_get_used_symbols_from_modules_renamed():
    '''Tests that we get the used symbols from a routine reported correctly
    when a symbol is renamed, we need to get the original name.
    '''
    test_dir = os.path.join(get_base_path("dynamo0.3"), "driver_creation")

    mod_man = ModuleManager.get()
    mod_man.add_search_path(test_dir)

    mod_info = mod_man.get_module_info("module_renaming_external_var_mod")
    psyir = mod_info.get_psyir("renaming_subroutine")
    ctu = CallTreeUtils()
    non_locals = ctu.get_non_local_symbols(psyir)

    # This example should report just one non-local module:
    # use module_with_var_mod, only: renamed_var => module_var_a
    # It must report the name in the module "module_var_a", not "renamed_var"
    assert len(non_locals) == 1
    # Ignore the last element, variable access
    assert non_locals[0][0:3] == ("unknown", "module_with_var_mod",
                                  Signature("module_var_a"))


# -----------------------------------------------------------------------------
@pytest.mark.usefixtures("clear_module_manager_instance")
def test_routine_non_locals_invokes():
    '''Tests that kernels and builtins are handled correctly. We need to get
    the PSyIR after being processed by PSyclone, so that the invoke-call has
    been replaced with the builtin/kernel.
    '''

    # Get the PSyclone-processed PSyIR
    test_file = os.path.join("driver_creation", "module_with_builtin_mod.f90")
    mod_psyir, _ = get_invoke(test_file, "dynamo0.3", 0, dist_mem=False)

    # Now create the module and routine info
    test_dir = os.path.join(get_base_path("dynamo0.3"), "driver_creation")
    mod_man = ModuleManager.get()
    mod_man.add_search_path(test_dir)
    mod_info = mod_man.get_module_info("module_with_builtin_mod")

    # Replace the generic PSyir with the PSyclone processed PSyIR, which
    # has a builtin
    psyir = mod_psyir.invokes.invoke_list[0].schedule

    # This will return three schedule - the DynInvokeSchedule, and two
    # schedules for the kernel and builtin:
    schedules = psyir.walk(Schedule)
    assert isinstance(schedules[1].children[0], LFRicKern)
    assert isinstance(schedules[2].children[0], BuiltIn)

    ctu = CallTreeUtils()
    non_locals = ctu._compute_all_non_locals(psyir)
    # There should be exactly one entry - the kernel, but not the builtin:
    assert len(non_locals) == 1
    assert non_locals[0] == ("routine", "testkern_import_symbols_mod",
                             Signature("testkern_import_symbols_code"))

    # Test that the assignment of the result of a function is not reported
    # as an access:
    mod_info = mod_man.get_module_info("testkern_import_symbols_mod")
    non_locals = ctu._compute_all_non_locals(mod_info.get_psyir("local_func"))
    assert len(non_locals) == 0
