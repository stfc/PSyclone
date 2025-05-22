# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2020-2025, Science and Technology Facilities Council.
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
import re

import pytest

from psyclone.configuration import Config
from psyclone.core import Signature, SingleVariableAccessInfo
from psyclone.domain.lfric import LFRicKern
from psyclone.parse import ModuleManager
from psyclone.psyGen import BuiltIn, Kern
from psyclone.psyir.nodes import CodeBlock, Reference, Schedule
from psyclone.psyir.symbols import RoutineSymbol
from psyclone.psyir.tools import CallTreeUtils, ReadWriteInfo
from psyclone.tests.utilities import get_base_path, get_invoke

# This is used in a fixture
# pylint: disable-next=unused-import
from psyclone.tests.parse.conftest \
    import mod_man_test_setup_directories  # noqa: F401


# -----------------------------------------------------------------------------
@pytest.mark.usefixtures("clear_module_manager_instance")
def test_call_tree_compute_all_non_locals_non_kernel():
    '''Test _compute_all_non_locals() functionality for source code
    that has no kernels.
    '''
    test_dir = os.path.join(get_base_path("lfric"), "driver_creation")
    mod_man = ModuleManager.get()
    mod_man.add_search_path(test_dir)
    mod_info = mod_man.get_module_info("module_call_tree_mod")

    ctu = CallTreeUtils()

    container_node = mod_info.get_psyir()

    # Check that using a local variable is not reported:
    psyir = container_node.find_routine_psyir("local_var_sub")
    info = ctu._compute_all_non_locals(psyir)
    assert info == []

    # Check using a variable that is used from the current module
    psyir = container_node.find_routine_psyir("module_var_sub")
    info = ctu._compute_all_non_locals(psyir)
    assert info == [('reference', 'module_call_tree_mod',
                     Signature("module_var"))]

    # Test that a call of a function in the same module is reported as
    # module routine:
    psyir = container_node.find_routine_psyir("call_local_function")
    info = ctu._compute_all_non_locals(psyir)
    assert info == [('routine', 'module_call_tree_mod',
                     Signature("module_function"))]

    # Check using a local constant
    psyir = container_node.find_routine_psyir("local_const_sub")
    info = ctu._compute_all_non_locals(psyir)
    assert info == []

    # Check using an argument
    psyir = container_node.find_routine_psyir("argument_sub")
    info = ctu._compute_all_non_locals(psyir)
    assert info == []

    # Check assigning the result to a function
    psyir = container_node.find_routine_psyir("module_function")
    info = ctu._compute_all_non_locals(psyir)
    assert info == []

    # Check calling an undeclared function
    psyir = \
        container_node.find_routine_psyir("calling_unknown_subroutine")
    info = ctu._compute_all_non_locals(psyir)
    assert info == [("routine", None, Signature("unknown_subroutine"))]

    # Check calling an imported subroutine
    psyir = \
        container_node.find_routine_psyir("calling_imported_subroutine")
    info = ctu._compute_all_non_locals(psyir)
    assert info == [("routine", "some_module", Signature("module_subroutine"))]

    # Check using an imported symbol
    psyir = container_node.find_routine_psyir("use_imported_symbol")
    info = ctu._compute_all_non_locals(psyir)
    assert info == [("unknown", "some_module1", Signature("module_var1")),
                    ("unknown", "some_module2", Signature("module_var2"))]

    # Check calling an undeclared function
    psyir = container_node.find_routine_psyir("intrinsic_call")
    info = ctu._compute_all_non_locals(psyir)
    assert info == []


# -----------------------------------------------------------------------------
@pytest.mark.usefixtures("clear_module_manager_instance")
def test_call_tree_generic_functions():
    '''Test handling of generic functions. Each function specified in a
    generic interface must be visited, since PSyclone might not always be
    able to determine the right function to be called due to missing type
    information.
    '''
    test_dir = os.path.join(get_base_path("lfric"), "driver_creation")
    mod_man = ModuleManager.get()
    mod_man.add_search_path(test_dir)
    mod_info_call_tree = mod_man.get_module_info("module_call_tree_mod")
    # First make sure we get indeed all three functions (even though
    # one of the functions does not exist, which is required for testing
    # exceptions):
    all_names = (mod_info_call_tree.get_psyir().
                 resolve_routine("generic_function"))
    assert all_names == ["real_func", "double_func", "integer_func"]

    ctu = CallTreeUtils()
    mod_info = mod_man.get_module_info("module_calling_generic_function")
    todo = ctu.get_non_local_symbols(mod_info.get_psyir())
    rw_info = ReadWriteInfo()
    ctu._resolve_calls_and_unknowns(todo, rw_info)
    assert (set(rw_info.read_list) ==
            set([('module_call_tree_mod', Signature("module_var_real")),
                 ('module_call_tree_mod', Signature("module_var_double"))]))


# -----------------------------------------------------------------------------
@pytest.mark.usefixtures("clear_module_manager_instance")
def test_call_tree_compute_all_non_locals_kernel():
    '''This tests the handling of (LFRic-specific) kernels and builtins. This
    example contains an explicit kernel and a builtin, so both will be tested.

    '''
    # We need to get the PSyIR after being processed by PSyclone, so that the
    # invoke-call and builtin has been replaced with the builtin/kernel
    # objects.
    test_file = os.path.join("driver_creation", "module_with_builtin_mod.f90")
    mod_psyir, _ = get_invoke(test_file, "lfric", 0, dist_mem=False)
    psyir = mod_psyir.invokes.invoke_list[0].schedule

    # This will return three schedule - the LFRicInvokeSchedule, and two
    # schedules for the kernel and builtin. Just make sure we have
    # the right parts before doing the actual test:
    schedules = psyir.walk(Schedule)
    assert isinstance(schedules[1].children[0], LFRicKern)
    assert isinstance(schedules[2].children[0], BuiltIn)

    ctu = CallTreeUtils()
    non_locals = ctu._compute_all_non_locals(psyir)

    # There should be exactly one entry - the kernel, but not the builtin:
    assert len(non_locals) == 1
    assert non_locals[0] == ("routine", "testkern_import_symbols_mod",
                             Signature("testkern_import_symbols_code"))


# -----------------------------------------------------------------------------
@pytest.mark.usefixtures("clear_module_manager_instance")
def test_call_tree_get_used_symbols_from_modules():
    '''Tests that we get the used symbols from a routine reported correctly.
    '''
    test_dir = os.path.join(get_base_path("lfric"), "driver_creation")

    mod_man = ModuleManager.get()
    mod_man.add_search_path(test_dir)

    mod_info = mod_man.get_module_info("testkern_import_symbols_mod")
    container_node = mod_info.get_psyir()
    psyir = container_node.find_routine_psyir("testkern_import_symbols_code")
    ctu = CallTreeUtils()
    non_locals = ctu.get_non_local_symbols(psyir)

    non_locals_without_access = set((i[0], i[1], str(i[2]))
                                    for i in non_locals)
    # Check that the expected symbols, modules and internal type are correct.
    # Note that a constant variable from another module is still reported here
    expected = set([
            ("unknown", "constants_mod", "eps"),
            ("unknown", "module_with_var_mod", "module_const"),
            ("reference", "testkern_import_symbols_mod",
             "dummy_module_variable"),
            ('routine', 'testkern_import_symbols_mod', "local_func"),
            ("routine", "module_with_var_mod", "module_subroutine"),
            ("unknown", "module_with_var_mod", "module_var_a"),
            ("routine", "testkern_import_symbols_mod", "local_subroutine"),
            ("routine", None, "unknown_subroutine")]
            )
    assert non_locals_without_access == expected

    # Check the handling of a symbol that is not found: _compute_all_non_locals
    # should return None:
    ref = psyir.walk(Reference)[0]
    # Change the name of the symbol so that it is not in the symbol table:
    ref.symbol._name = "not-in-any-symbol-table"
    # Just pass in the Reference, otherwise we would get additional output
    # (as above):
    assert ctu._compute_all_non_locals(ref) == []


# -----------------------------------------------------------------------------
@pytest.mark.usefixtures("clear_module_manager_instance")
def test_call_tree_get_used_symbols_from_modules_renamed():
    '''Tests that we get the used symbols from a routine reported correctly
    when a symbol is renamed, we need to get the original name.
    '''
    test_dir = os.path.join(get_base_path("lfric"), "driver_creation")

    mod_man = ModuleManager.get()
    mod_man.add_search_path(test_dir)

    mod_info = mod_man.get_module_info("module_renaming_external_var_mod")
    container_node = mod_info.get_psyir()
    psyir = container_node.find_routine_psyir("renaming_subroutine")
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
def test_get_non_local_read_write_info(capsys):
    '''Tests the collection of non-local input and output parameters.
    '''
    Config.get().api = "lfric"
    ctu = CallTreeUtils()

    test_file = os.path.join("driver_creation", "module_with_builtin_mod.f90")
    psyir, _ = get_invoke(test_file, "lfric", 0, dist_mem=False)
    schedule = psyir.invokes.invoke_list[0].schedule

    # Set up the module manager with a search directory that does not
    # contain any files used here:
    kernels_dir = os.path.join(get_base_path("lfric"),
                               "kernels", "dead_end", "no_really")
    mod_man = ModuleManager.get()
    mod_man.add_search_path(kernels_dir)

    # Since the right search path is missing, this will result
    # in the testkern_import_symbols_mod module not being found:
    read_write_info = ReadWriteInfo()
    rw_info = ctu.get_non_local_read_write_info(schedule, read_write_info)
    out, _ = capsys.readouterr()
    assert ("Could not find module 'testkern_import_symbols_mod' - ignored."
            in out)

    # The search directories are absolute, so use a regex:
    assert re.search("Could not find source file for module "
                     "'testkern_import_symbols_mod' in any of the "
                     "directories '.*kernels/dead_end/no_really'.", out)

    # Now add the correct search path of the driver creation tests to the
    # module manager:
    test_dir = os.path.join(get_base_path("lfric"), "driver_creation")
    mod_man = ModuleManager.get()
    mod_man.add_search_path(test_dir)

    # The example does contain an unknown subroutine (by design), and the
    # infrastructure directory has not been added, so constants_mod cannot
    # be found:
    rw_info = ReadWriteInfo()
    ctu.get_non_local_read_write_info(schedule, rw_info)
    out, _ = capsys.readouterr()
    assert "Unknown routine 'unknown_subroutine - ignored." in out
    assert ("Cannot find module 'constants_mod' - ignoring unknown symbol "
            "'eps'." in out)

    # We don't test the 14 local variables here, this was tested earlier.
    # Focus on the remote symbols that are read:
    assert (('module_with_var_mod', Signature("module_var_b"))
            in rw_info.read_list)
    # And check the remote symbols that are written:
    assert (('module_with_var_mod', Signature("module_var_a"))
            in rw_info.write_list)
    assert (('module_with_var_mod', Signature("module_var_b"))
            in rw_info.write_list)
    assert (('testkern_import_symbols_mod', Signature("dummy_module_variable"))
            in rw_info.write_list)

    # Make sure that accessing a constant from a different module is
    # not included:
    assert (('module_with_var_mod', Signature("module_const"))
            not in rw_info.read_list)

    # Check that we can ignore a module:
    mod_man.add_ignore_module("constants_mod")
    rw_info = ReadWriteInfo()
    ctu.get_non_local_read_write_info(schedule, rw_info)
    out, _ = capsys.readouterr()
    assert "Unknown routine 'unknown_subroutine - ignored." in out
    assert "constants_mod" not in out


@pytest.mark.usefixtures("clear_module_manager_instance")
def test_get_non_local_read_write_info_errors(capsys):
    '''
    Test get_non_local_read_write_info() when we fail to get either the Routine
    PSyIR or the Container PSyIR.
    '''
    Config.get().api = "lfric"
    ctu = CallTreeUtils()
    test_file = os.path.join("driver_creation", "module_with_builtin_mod.f90")
    psyir, _ = get_invoke(test_file, "lfric", 0, dist_mem=False)
    schedule = psyir.invokes.invoke_list[0].schedule

    test_dir = os.path.join(get_base_path("lfric"), "driver_creation")
    mod_man = ModuleManager.get()
    mod_man.add_search_path(test_dir)
    kernels = schedule.walk(Kern)
    minfo = mod_man.get_module_info(kernels[0].module_name)
    cntr = minfo.get_psyir()
    routine = cntr.find_routine_psyir("testkern_import_symbols_code")
    # Remove the kernel routine from the PSyIR.
    routine.detach()

    rw_info = ReadWriteInfo()
    ctu.get_non_local_read_write_info(schedule, rw_info)
    out, _ = capsys.readouterr()
    assert (f"Could not get PSyIR for Routine 'testkern_import_symbols_code' "
            f"from module '{kernels[0].module_name}' as no possible" in out)

    # Add a RoutineSymbol back into the symbol table to mimic a CodeBlock
    # representing the routine.
    cntr.symbol_table.add(RoutineSymbol("testkern_import_symbols_code"))
    rw_info = ReadWriteInfo()
    ctu.get_non_local_read_write_info(schedule, rw_info)
    out, _ = capsys.readouterr()
    assert (f"Could not get PSyIR for Routine 'testkern_import_symbols_code' "
            f"from module '{kernels[0].module_name}' -" in out)

    # Remove the module Container from the PSyIR.
    cntr.detach()
    ctu.get_non_local_read_write_info(schedule, rw_info)
    out, _ = capsys.readouterr()
    assert (f"Could not get PSyIR for module '{kernels[0].module_name}'"
            in out)


# -----------------------------------------------------------------------------
@pytest.mark.usefixtures("clear_module_manager_instance")
def test_call_tree_utils_resolve_calls_unknowns(capsys):
    '''Tests resolving symbols in case of missing modules, subroutines, and
    unknown type (e.g. function call or array access).
    '''
    # pylint: disable=too-many-statements
    # Add the search path of the driver creation tests to the
    # module manager:
    test_dir = os.path.join(get_base_path("lfric"), "driver_creation")
    mod_man = ModuleManager.get()
    mod_man.add_search_path(test_dir)

    # Test if the internal todo handling cannot find a subroutine in the
    # module it is supposed to be in. Create a todo list indicating that
    # the "unknown_subroutine" is in "unknown_module", but this module
    # does not exist:
    todo = [("routine", "unknown_module", Signature("unknown_subroutine"),
             None)]
    ctu = CallTreeUtils()
    rw_info = ReadWriteInfo()
    ctu._resolve_calls_and_unknowns(todo, rw_info)
    out, _ = capsys.readouterr()
    assert "Cannot find module 'unknown_module' - ignored." in out
    assert rw_info.read_list == []
    assert rw_info.write_list == []

    # Now query for a routine that exists, and make sure we do not
    # get a warning printed for this (which we did in the past):
    todo = [('routine', 'module_with_var_mod', Signature("module_subroutine"),
             None)]
    ctu._resolve_calls_and_unknowns(todo, rw_info)
    out, _ = capsys.readouterr()
    assert ("Cannot resolve routine 'module_subroutine' in module "
            "'module_with_var_mod' - ignored." not in out)

    rw_info = ReadWriteInfo()
    # Now try to find a routine that does not exist in an existing module:
    todo = [('routine', 'module_with_var_mod', Signature("does-not-exist"),
             None)]
    ctu._resolve_calls_and_unknowns(todo, rw_info)
    out, _ = capsys.readouterr()
    assert ("Cannot resolve routine 'does-not-exist' in module "
            "'module_with_var_mod' - ignored." in out)
    assert rw_info.read_list == []
    assert rw_info.write_list == []

    # Now ask for an unknown symbol (in this case a subroutine), it
    # should be detected to be a subroutine, and the accesses inside
    # this subroutine should then be reported:
    todo = [('unknown', 'module_with_var_mod',
             Signature("module_subroutine"), None)]
    ctu._resolve_calls_and_unknowns(todo, rw_info)
    assert set(rw_info.read_list) == {('module_with_var_mod',
                                       Signature("module_var_b")),
                                      ("module_with_var_mod",
                                       Signature("const_size_array"))}
    assert set(rw_info.write_list) == {('module_with_var_mod',
                                        Signature("module_var_b")),
                                       ("module_with_var_mod",
                                        Signature("const_size_array"))}

    # Get the associated PSyIR and break it by removing the Routine and
    # associated Symbol.
    info = SingleVariableAccessInfo(Signature("module_subroutine"))
    minfo = mod_man.get_module_info("module_with_var_mod")
    cntr = minfo.get_psyir()
    cntr.find_routine_psyir("module_subroutine").detach()
    # Since the Routine detach removes the routine symbol, we add a
    # Routine symbol back in. This mimics the behaviour of having a
    # CodeBlock representing this Routine
    cntr.symbol_table.add(RoutineSymbol("module_subroutine"))
    todo = [('routine', 'module_with_var_mod',
             Signature("module_subroutine"), info)]
    ctu._resolve_calls_and_unknowns(todo, rw_info)
    out, _ = capsys.readouterr()
    assert ("Cannot find routine 'module_subroutine' in module "
            "'module_with_var_mod' - ignored" in out)

    # Note that module_subroutine has been removed from the PSyIR,
    # so it cannot be found:
    rsym = cntr.symbol_table.lookup("module_subroutine")
    cntr.symbol_table.remove(rsym)
    todo = [('unknown', 'module_with_var_mod',
             Signature("module_subroutine"), info)]
    ctu._resolve_calls_and_unknowns(todo, rw_info)
    out, _ = capsys.readouterr()
    assert "Cannot find symbol 'module_subroutine'." in out

    todo = [('routine', 'module_with_var_mod',
             Signature("module_subroutine"), info)]
    ctu._resolve_calls_and_unknowns(todo, rw_info)
    out, _ = capsys.readouterr()
    assert ("Cannot resolve routine 'module_subroutine' in module "
            "'module_with_var_mod' - ignored" in out)

    # Break the PSyIR more seriously by removing the Container.
    cntr.detach()
    todo = [('unknown', 'module_with_var_mod',
             Signature("module_subroutine"), info)]
    ctu._resolve_calls_and_unknowns(todo, rw_info)
    out, _ = capsys.readouterr()
    assert ("Cannot get PSyIR for module 'module_with_var_mod' - ignoring "
            "unknown symbol 'module_subroutine'" in out)


# -----------------------------------------------------------------------------
@pytest.mark.usefixtures("change_into_tmpdir", "clear_module_manager_instance",
                         "mod_man_test_setup_directories")
def test_module_info_generic_interfaces():
    '''Tests the handling of generic interfaces, which should return the
    combined results from all individual subroutines. The example in g_mod
    declares myfunc to be myfunc1 and myfunc2, which are implemented as:
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
    ctu = CallTreeUtils()

    cntr = mod_info.get_psyir()
    all_routines = cntr.resolve_routine("myfunc")
    all_non_locals = []
    for routine_name in all_routines:
        all_non_locals.extend(
            ctu.get_non_local_symbols(
                mod_info.get_psyir().
                find_routine_psyir(routine_name)))
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


# -----------------------------------------------------------------------------
def test_call_tree_utils_inout_parameters_generic(fortran_reader):
    '''Test detection of input and output parameters in the generic PSyIR.
    '''
    source = '''program test
                integer :: ji, jj, jpj
                real :: a(5,5), c(5,5), b, dummy(5,5)
                do jj = lbound(dummy,1), jpj   ! loop 0
                   do ji = lbound(dummy,2), ubound(dummy,2)
                      a(ji, jj) = b+c(ji, jj)
                    end do
                end do
                end program test'''
    psyir = fortran_reader.psyir_from_source(source)
    loops = psyir.children[0].children

    read_write_info_read = ReadWriteInfo()
    ctu = CallTreeUtils()
    ctu.get_input_parameters(read_write_info_read, loops)

    # Use set to be order independent
    input_set = set(read_write_info_read.signatures_read)
    # Note that by default the read access to `dummy` in lbound etc should
    # not be reported, since it does not really read the array values.
    assert input_set == set([Signature("b"), Signature("c"),
                             Signature("jpj")])

    read_write_info_write = ReadWriteInfo()
    ctu.get_output_parameters(read_write_info_write, loops)
    # Use set to be order independent
    output_set = set(read_write_info_write.signatures_written)
    assert output_set == set([Signature("jj"), Signature("ji"),
                              Signature("a")])

    read_write_info_all = ctu.get_in_out_parameters(loops)

    assert read_write_info_read.read_list == read_write_info_all.read_list
    assert read_write_info_write.write_list == read_write_info_all.write_list

    # Check that we can also request to get the access to 'dummy'
    # inside the ubound/lbound function calls.
    read_write_info = ReadWriteInfo()
    ctu.get_input_parameters(read_write_info, loops,
                             include_non_data_accesses=True)
    input_set = set(sig for _, sig in read_write_info.set_of_all_used_vars)
    assert input_set == set([Signature("b"), Signature("c"),
                             Signature("jpj"), Signature("dummy")])

    read_write_info = ctu.get_in_out_parameters(loops,
                                                include_non_data_accesses=True)
    output_set = set(read_write_info.signatures_read)
    assert output_set == set([Signature("b"), Signature("c"),
                              Signature("jpj"), Signature("dummy")])


# -----------------------------------------------------------------------------
def test_call_tree_utils_const_argument():
    '''Check that using a const scalar as parameter works, i.e. is not
    listed as input variable.'''
    _, invoke = get_invoke("test00.1_invoke_kernel_using_const_scalar.f90",
                           api="gocean", idx=0)
    cut = CallTreeUtils()
    read_write_info = ReadWriteInfo()
    cut.get_input_parameters(read_write_info, invoke.schedule)
    # Make sure the constant '0' is not listed
    assert "0" not in read_write_info.signatures_read
    assert Signature("0") not in read_write_info.signatures_read


# -----------------------------------------------------------------------------
@pytest.mark.usefixtures("clear_module_manager_instance", "lfric_config")
def testcall_tree_utils_non_local_inout_parameters(capsys):
    '''Tests the collection of non-local input and output parameters.
    '''
    ctu = CallTreeUtils()

    test_file = os.path.join("driver_creation", "module_with_builtin_mod.f90")
    psyir, _ = get_invoke(test_file, "lfric", 0, dist_mem=False)
    schedule = psyir.invokes.invoke_list[0].schedule

    test_dir = os.path.join(get_base_path("lfric"), "driver_creation")
    mod_man = ModuleManager.get()
    mod_man.add_search_path(test_dir)

    # The example does contain an unknown subroutine (by design), and the
    # infrastructure directory has not been added, so constants_mod cannot
    # be found:
    rw_info = ctu.get_in_out_parameters(schedule,
                                        collect_non_local_symbols=True)
    out, _ = capsys.readouterr()
    assert "Unknown routine 'unknown_subroutine - ignored." in out
    assert ("Cannot find module 'constants_mod' - ignoring unknown symbol "
            "'eps'." in out)

    # We don't test the 14 local variables here, this was tested earlier.
    # Focus on the remote symbols that are read:
    assert (('module_with_var_mod', Signature("module_var_b"))
            in rw_info.read_list)
    # And check the remote symbols that are written:
    assert (('module_with_var_mod', Signature("module_var_a"))
            in rw_info.write_list)
    assert (('module_with_var_mod', Signature("module_var_b"))
            in rw_info.write_list)
    assert (('testkern_import_symbols_mod', Signature("dummy_module_variable"))
            in rw_info.write_list)


# -----------------------------------------------------------------------------
def test_call_tree_error_var_not_found(capsys):
    '''Tests that trying to import a variable from a module that does not
    contain the variable is handled, i.e. printing a warning and otherwise
    ignores (TODO #2120)
    '''
    dyn_test_dir = get_base_path("lfric")
    mod_man = ModuleManager.get()
    mod_man.add_search_path(os.path.join(dyn_test_dir, "infrastructure"))

    read_write_info = ReadWriteInfo()
    ctu = CallTreeUtils()
    sva = SingleVariableAccessInfo(Signature("a"))
    ctu._resolve_calls_and_unknowns([("unknown", "constants_mod",
                                      Signature("does_not_exist"), sva)],
                                    read_write_info)
    out, _ = capsys.readouterr()

    assert "Cannot find symbol 'does_not_exist'." in out


# -----------------------------------------------------------------------------
@pytest.mark.usefixtures("clear_module_manager_instance")
def test_call_tree_error_module_is_codeblock(capsys):
    '''Tests that a module that cannot be parsed and becomes a codeblock
    is handled correctly.
    '''
    dyn_test_dir = get_base_path("lfric")
    mod_man = ModuleManager.get()
    mod_man.add_search_path(os.path.join(dyn_test_dir, "driver_creation"))

    cblock = CodeBlock([], "dummy")
    mod_info = mod_man.get_module_info("testkern_import_symbols_mod")
    # get_psyir returns the module PSyIR, which we need to replace with
    # the codeblock in order to reproduce this error:
    container = mod_info.get_psyir()
    container.replace_with(cblock)

    ctu = CallTreeUtils()
    sva = SingleVariableAccessInfo(Signature("a"))
    read_write_info = ReadWriteInfo()
    ctu._resolve_calls_and_unknowns(
        [("routine", "testkern_import_symbols_mod",
          Signature("testkern_import_symbols_code"), sva)], read_write_info)
    out, _ = capsys.readouterr()
    assert ("_symbols_mod.f90' does contain module "
            "'testkern_import_symbols_mod' but PSyclone is unable to create "
            "the PSyIR of it." in out)
