# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2021-2025, Science and Technology Facilities Council
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
# Authors R. W. Ford, A. R. Porter and S. Siso, STFC Daresbury Lab
# Modified I. Kavcic, A. Coughtrie and L. Turner, Met Office,
#          C. M. Maynard, Met Office/University of Reading,
#          J. Henrichs, Bureau of Meteorology.

''' This module uses pytest to test the LFRicLoop class. This is the LFRic-
    specific subclass of the Loop class. '''

import os
import pytest

from fparser import api as fpapi

from psyclone.configuration import Config
from psyclone.core import AccessType
from psyclone.domain.lfric import (LFRicConstants, LFRicSymbolTable,
                                   LFRicKern, LFRicKernMetadata, LFRicLoop,
                                   LFRicInvokeSchedule)
from psyclone.errors import GenerationError, InternalError
from psyclone.parse.algorithm import parse
from psyclone.psyGen import PSyFactory, InvokeSchedule, Kern
from psyclone.psyir.nodes import Call, ScopingNode, Loop
from psyclone.psyir.symbols import RoutineSymbol
from psyclone.psyir.tools import DependencyTools
from psyclone.psyir.tools.dependency_tools import Message, DTCode
from psyclone.tests.lfric_build import LFRicBuild
from psyclone.tests.utilities import get_invoke
from psyclone.transformations import LFRicColourTrans

BASE_PATH = os.path.join(
    os.path.dirname(os.path.dirname(os.path.dirname(
        os.path.abspath(__file__)))),
    "test_files", "lfric")
TEST_API = "lfric"


def test_constructor_loop_bound_names():
    ''' Check that the constructor creates the appropriate loop bound
    references (with names with a sequentially ascending index)
    '''
    with pytest.raises(InternalError) as err:
        _ = LFRicLoop(loop_type="null")
    assert ("LFRic loops must be inside an InvokeSchedule, a parent "
            "argument is mandatory when they are created." in str(err.value))

    schedule = LFRicInvokeSchedule.create("test")
    schedule.addchild(LFRicLoop(parent=schedule))
    schedule.addchild(LFRicLoop(parent=schedule))
    schedule.addchild(LFRicLoop(parent=schedule))
    loops = schedule.loops()
    assert loops[0].start_expr.name == "uninitialised_loop0_start"
    assert loops[1].start_expr.name == "uninitialised_loop1_start"
    assert loops[2].start_expr.name == "uninitialised_loop2_start"
    assert loops[0].stop_expr.name == "uninitialised_loop0_stop"
    assert loops[1].stop_expr.name == "uninitialised_loop1_stop"
    assert loops[2].stop_expr.name == "uninitialised_loop2_stop"


def test_constructor_invalid_loop_type(monkeypatch):
    ''' Check that the constructor raises the expected errors when an invalid
    loop type is specified.

    '''
    # An invalid type should be caught by the setter in the base Loop class.
    with pytest.raises(TypeError) as err:
        LFRicLoop(loop_type="wrong")
    const = LFRicConstants()
    assert (f"Error, loop_type value (wrong) is invalid. Must be one of "
            f"{const.VALID_LOOP_TYPES}." in str(err.value))
    # Monkeypatch the list of valid loop types so as to reach the code
    # that attempts to set the loop variable.
    monkeypatch.setattr(LFRicConstants, "VALID_LOOP_TYPES", ["wrong"])
    parent = InvokeSchedule(RoutineSymbol("test"), None, None)
    with pytest.raises(InternalError) as err:
        LFRicLoop(loop_type="wrong", parent=parent)
    assert ("Unsupported loop type 'wrong' found when creating loop variable."
            " Supported values are: [" in str(err.value))


def test_set_lower_bound_functions(monkeypatch):
    ''' Test that we raise appropriate exceptions when the lower bound of
    an LFRicLoop is set to invalid values.

    '''
    # Make sure we get an LFRicSymbolTable
    # TODO #1954: Remove the protected access using a factory
    monkeypatch.setattr(ScopingNode, "_symbol_table_class",
                        LFRicSymbolTable)
    schedule = LFRicInvokeSchedule.create("test")
    my_loop = LFRicLoop(parent=schedule)
    schedule.children = [my_loop]
    with pytest.raises(GenerationError) as excinfo:
        my_loop.set_lower_bound("invalid_loop_bounds_name")
    assert "lower bound loop name is invalid" in str(excinfo.value)
    with pytest.raises(GenerationError) as excinfo:
        my_loop.set_lower_bound("inner", index=0)
    assert "specified index" in str(excinfo.value)
    assert "lower loop bound is invalid" in str(excinfo.value)


def test_set_upper_bound_functions(monkeypatch):
    ''' Test that we raise appropriate exceptions when the upper bound of
    an LFRicLoop is set to invalid values.

    '''
    # Make sure we get an LFRicSymbolTable
    # TODO #1954: Remove the protected access using a factory
    monkeypatch.setattr(ScopingNode, "_symbol_table_class",
                        LFRicSymbolTable)
    schedule = LFRicInvokeSchedule.create("test")
    my_loop = LFRicLoop(parent=schedule)
    schedule.children = [my_loop]
    with pytest.raises(GenerationError) as excinfo:
        my_loop.set_upper_bound("invalid_loop_bounds_name")
    assert "upper loop bound name is invalid" in str(excinfo.value)
    with pytest.raises(GenerationError) as excinfo:
        my_loop.set_upper_bound("start")
    assert "'start' is not a valid upper bound" in str(excinfo.value)
    with pytest.raises(GenerationError) as excinfo:
        my_loop.set_upper_bound("inner", halo_depth=0)
    assert ("specified halo depth '0' for this loop upper bound is < 1 which "
            "is invalid" in str(excinfo.value))
    with pytest.raises(TypeError) as excinfo:
        my_loop.set_upper_bound("inner", halo_depth="wrong")
    assert ("When setting the upper bound of a loop, any halo depth must be "
            "supplied as an int or PSyIR DataNode but got "
            in str(excinfo.value))


def test_lower_bound_psyir_1():
    ''' Tests we raise an exception in the LFRicLoop:lower_bound_psyir()
    method - first GenerationError.

    '''
    _, invoke_info = parse(os.path.join(BASE_PATH, "1_single_invoke.f90"),
                           api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=False).create(invoke_info)
    my_loop = psy.invokes.invoke_list[0].schedule.children[0]
    my_loop.set_lower_bound("inner", index=1)
    with pytest.raises(GenerationError) as excinfo:
        _ = my_loop.lower_bound_psyir()
    assert ("lower bound must be 'start' if we are sequential" in
            str(excinfo.value))


def test_lower_bound_psyir_2(monkeypatch):
    ''' Tests we raise an exception in the LFRicLoop:lower_bound_psyir()
    method - second GenerationError.

    '''
    _, invoke_info = parse(os.path.join(BASE_PATH, "1_single_invoke.f90"),
                           api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(invoke_info)
    my_loop = psy.invokes.invoke_list[0].schedule.children[4]
    # We can not use the standard set_lower_bound function as that
    # checks for valid input
    monkeypatch.setattr(my_loop, "_lower_bound_name", value="invalid")
    with pytest.raises(GenerationError) as excinfo:
        _ = my_loop.lower_bound_psyir()
    assert ("Unsupported lower bound name 'invalid' found" in
            str(excinfo.value))


@pytest.mark.parametrize("name, index, output",
                         [("inner", 10, "inner_cell(11)"),
                          ("ncells", 10, "inner_cell(1)"),
                          ("cell_halo", 1, "ncells_cell()"),
                          ("cell_halo", 10, "cell_halo_cell(9)")])
def test_lower_bound_psyir_3(monkeypatch, name, index, output):
    ''' Test 'lower_bound_psyir()' with multiple valid iteration spaces.

    '''
    _, invoke_info = parse(os.path.join(BASE_PATH, "1_single_invoke.f90"),
                           api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(invoke_info)
    my_loop = psy.invokes.invoke_list[0].schedule.children[4]
    # We can not use the standard set_lower_bound function as that
    # checks for valid input
    monkeypatch.setattr(my_loop, "_lower_bound_name", value=name)
    monkeypatch.setattr(my_loop, "_lower_bound_index", value=index)
    expected = "mesh%get_last_" + output + " + 1"
    assert my_loop.lower_bound_psyir().debug_string() == expected


def test_mesh_name():
    ''' Tests for the '_mesh_name' property of LFRicLoop.

    '''
    _, invoke_info = parse(os.path.join(BASE_PATH, "1_single_invoke.f90"),
                           api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(invoke_info)
    # TODO #1010. Replace this psy.gen with a call to lower_to_language_level()
    # pylint: disable=pointless-statement
    psy.gen
    loops = psy.invokes.invoke_list[0].schedule.walk(LFRicLoop)
    assert loops[0]._mesh_name == "mesh"


def test_mesh_name_intergrid():
    ''' Tests for the '_mesh_name' property of LFRicLoop for an intergrid
    kernel.

    '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "22.1_intergrid_restrict.f90"),
                           api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(invoke_info)
    # TODO #1010. Replace this psy.gen with a call to lower_to_language_level()
    # pylint: disable=pointless-statement
    psy.gen
    loops = psy.invokes.invoke_list[0].schedule.walk(LFRicLoop)
    assert loops[0]._mesh_name == "mesh_field1"


def test_lower_to_language_normal_loop():
    ''' Test that we can call lower_to_language_level on a normal
    (i.e. not a domain) LFRicLoop. The new loop type should not be a
    LFRicLoop anymore, but a Loop. Additionally, also test that
    without lowering the symbols (for start and stop expressions)
    will change if the loop order is modified, but after lowering
    the symbols should not change anymore.
    '''

    _, invoke = get_invoke("4.8_multikernel_invokes.f90", TEST_API,
                           dist_mem=False, idx=0)
    sched = invoke.schedule
    loop1 = sched.children[1]
    assert loop1.start_expr.symbol.name == "uninitialised_loop1_start"

    # The same test with the lowered schedule should not change the
    # symbol anymore:
    _, invoke = get_invoke("4.8_multikernel_invokes.f90", TEST_API,
                           dist_mem=False, idx=0)
    # Now lower the loop:
    sched = invoke.schedule
    # Verify that we have the right node:
    assert isinstance(sched.children[1], LFRicLoop)
    sched.lower_to_language_level()
    loop1 = sched.children[1]
    assert not isinstance(loop1, LFRicLoop)
    assert isinstance(loop1, Loop)

    # Verify that after lowering the symbol name does not change
    # anymore if a previous loop is removed:
    assert loop1.start_expr.symbol.name == "uninitialised_loop1_start"
    sched.children.pop(0)
    assert loop1.start_expr.symbol.name == "uninitialised_loop1_start"


def test_lower_to_language_domain_loops():
    ''' Tests that we can call lower_to_language_level on a DOMAIN LFRicLoop.
    The DOMAIN loop is replaced by the kernel call inside it.
    '''

    _, invoke = get_invoke("25.1_kern_two_domain.f90", TEST_API, idx=0)
    sched = invoke.schedule

    # The lowering converts the loops into a single calls
    assert isinstance(sched.children[0], LFRicLoop)
    assert isinstance(sched.children[1], LFRicLoop)
    sched.lower_to_language_level()
    assert isinstance(sched.children[0], Call)
    assert isinstance(sched.children[1], Call)


def test_lower_to_language_domain_loops_multiple_statements():
    ''' Tests lower_to_language_level on a DOMAIN LFRicLoop with multiple
    statements in its loop_body.
    '''

    _, invoke = get_invoke("25.1_kern_two_domain.f90", TEST_API, idx=0)
    sched = invoke.schedule
    # Force the two statements to be inside the same loop
    loop1 = sched.children[1].detach()
    kern = loop1.loop_body.children[0].detach()
    sched.children[0].loop_body.children.insert(1, kern)
    with pytest.raises(NotImplementedError) as err:
        sched.lower_to_language_level()
    assert ("Lowering LFRic domain loops that produce more than one "
            "children is not yet supported, but found:" in str(err.value))


def test_lfricloop_load_unexpected_func_space():
    ''' The load function of an instance of the LFRicLoop class raises an
    error if an unexpected function space is found. This test makes
    sure this error works correctly. It's a little tricky to raise
    this error as it is unreachable. However, we can sabotage an
    earlier function to make it return an invalid value.

    '''
    # first create a working instance of the LFRicLoop class
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "19.1_single_stencil.f90"),
        api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(invoke_info)
    # now get access to the LFRicLoop class, the associated kernel class
    # and the associated field.
    schedule = psy.invokes.invoke_list[0].schedule
    loop = schedule.children[4]
    kernel = loop.loop_body[0]
    field = kernel.arguments.iteration_space_arg()
    # break the fields function space
    field._function_spaces[0]._orig_name = "broken"
    # create a function which always returns the broken field

    def broken_func():
        ''' Returns the above field no matter what '''
        return field
    # Replace the iteration_space_arg method with our broke
    # function. This is required as iteration_space_arg currently
    # never returns a field with an invalid function space.
    kernel.arguments.iteration_space_arg = broken_func
    # We can now raise the exception.
    with pytest.raises(GenerationError) as err:
        loop.load(kernel)
    const = LFRicConstants()
    assert ("Generation Error: Unexpected function space found. Expecting "
            "one of " + str(const.VALID_FUNCTION_SPACES) +
            " but found 'broken'" in str(err.value))


def test_loop_load_builtin_bound_names(monkeypatch, dist_mem, annexed):
    ''' Test that the 'load()' method sets the loop bounds correctly when
    supplied with a Built-in kernel. We test with both possible settings of
    'api_config.compute_annexed_dofs'.

    '''
    api_config = Config.get().api_conf(TEST_API)
    monkeypatch.setattr(api_config, "_compute_annexed_dofs", annexed)
    # First create a working instance of the LFRicLoop class.
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "15.1.2_builtin_and_normal_kernel_invoke.f90"),
        api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=dist_mem).create(invoke_info)
    # Now get access to the LFRicLoop and its associated Built-in kernel.
    schedule = psy.invokes.invoke_list[0].schedule
    loop = schedule.walk(LFRicLoop)[0]
    kernel = loop.loop_body[0]
    new_loop0 = LFRicLoop(parent=schedule)
    new_loop0.load(kernel)
    if dist_mem and annexed:
        assert new_loop0._upper_bound_name == "nannexed"
    else:
        assert new_loop0._upper_bound_name == "ndofs"


def test_loop_load_bound_names_continuous(dist_mem):
    ''' Test that the 'load()' method sets the loop bounds names as
    expected when given a user-supplied kernel that updates fields on
    continuous function spaces.

    '''
    # First create a working instance of the LFRicLoop class.
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "1_single_invoke.f90"), api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=dist_mem).create(invoke_info)
    # Now get access to the LFRicLoop and its associated kernel.
    schedule = psy.invokes.invoke_list[0].schedule
    loop = schedule.walk(LFRicLoop)[0]
    kernel = loop.loop_body[0]
    new_loop0 = LFRicLoop(parent=schedule)
    assert new_loop0._lower_bound_name is None
    assert new_loop0._upper_bound_name is None
    new_loop0.load(kernel)
    assert new_loop0._lower_bound_name == "start"
    # This kernel has GH_INC access for a field on a continuous space.
    if dist_mem:
        assert new_loop0._upper_bound_name == "cell_halo"
    else:
        assert new_loop0._upper_bound_name == "ncells"
    # Patch it so that a second field has GH_WRITE access. As there is still
    # a GH_INC, this should make no difference to the loop bounds.
    kernel.args[2]._access = AccessType.WRITE
    new_loop1 = LFRicLoop(parent=schedule)
    new_loop1.load(kernel)
    if dist_mem:
        assert new_loop1._upper_bound_name == "cell_halo"
    else:
        assert new_loop1._upper_bound_name == "ncells"
    # Patch it again to change the GH_INC argument into a GH_WRITE. The loop
    # bound should no longer go into the halo.
    kernel.args[1]._access = AccessType.WRITE
    new_loop2 = LFRicLoop(parent=schedule)
    new_loop2.load(kernel)
    assert new_loop2._upper_bound_name == "ncells"


def test_loop_load_bound_names_anyspace(dist_mem):
    ''' Test that the 'load()' method sets the loop bounds names as
    expected when given a user-supplied kernel that updates fields on
    unknown ('any_space') function spaces.

    '''
    # First create a working instance of the LFRicLoop class.
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "11_any_space.f90"), api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=dist_mem).create(invoke_info)
    # Now get access to the LFRicLoop and its associated kernel.
    schedule = psy.invokes.invoke_list[0].schedule
    loop = schedule.walk(LFRicLoop)[0]
    kernel = loop.loop_body[0]
    new_loop0 = LFRicLoop(parent=schedule)
    assert new_loop0._lower_bound_name is None
    assert new_loop0._upper_bound_name is None
    new_loop0.load(kernel)
    assert new_loop0._lower_bound_name == "start"
    # As the updated argument is on 'anyspace' we have to assume that it is
    # continuous and loop into the halo if DM is enabled.
    if dist_mem:
        assert new_loop0._upper_bound_name == "cell_halo"
    else:
        assert new_loop0._upper_bound_name == "ncells"
    # Patch the kernel so that the updated argument has GH_WRITE access
    # instead of GH_INC. We no longer need to loop into the halo to get correct
    # results for annexed dofs.
    kernel.args[0]._access = AccessType.WRITE
    new_loop1 = LFRicLoop(parent=schedule)
    new_loop1.load(kernel)
    assert new_loop1._upper_bound_name == "ncells"


def test_unsupported_halo_read_access():
    ''' This test checks that we raise an error if the halo_read_access
    method finds an upper bound other than halo or ncells. The
    particular issue at the moment is that if inner is specified we do
    not know whether the stencil accesses the halo or not. However,
    this limitation is not going to affect anyone until we add in loop
    iteration space splitting transformations.

    '''
    # create a valid loop with a stencil access
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "19.1_single_stencil.f90"),
        api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(invoke_info)
    # get access to the LFRicLoop object
    schedule = psy.invokes.invoke_list[0].schedule
    loop = schedule.children[4]
    # access to the argument that has a stencil access in the kernel
    kernel = loop.loop_body[0]
    stencil_arg = kernel.arguments.args[1]
    loop.set_upper_bound("inner", 1)
    # call our method
    with pytest.raises(GenerationError) as err:
        _ = loop._halo_read_access(stencil_arg)
    assert ("Loop bounds other than 'cell_halo' and 'ncells' are currently "
            "unsupported for kernels with stencil accesses. Found "
            "'inner'." in str(err.value))


def test_itn_space_write_w2broken_w1(dist_mem, tmpdir):
    ''' Check that generated loop over cells in the PSy layer has the
    correct upper bound when a kernel writes to two fields, the first on
    a discontinuous space (w2broken) and the second on a continuous space (w1).
    The resulting loop (when dm=True) must include the L1 halo because of
    the second field argument which is continuous.

    '''
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "1.5.1_single_invoke_write_multi_fs.f90"),
        api=TEST_API)
    psy = PSyFactory(TEST_API,
                     distributed_memory=dist_mem).create(invoke_info)
    generated_code = str(psy.gen)

    assert "loop0_start = 1\n" in generated_code
    if dist_mem:
        assert "loop0_stop = mesh%get_last_halo_cell(1)\n" in generated_code
        output = (
            "    do cell = loop0_start, loop0_stop, 1\n")
        assert output in generated_code
    else:
        assert "loop0_stop = m2_proxy%vspace%get_ncell()\n" in generated_code
        output = (
            "    ! Call kernels\n"
            "    do cell = loop0_start, loop0_stop, 1\n")
        assert output in generated_code

    assert LFRicBuild(tmpdir).code_compiles(psy)


def test_itn_space_fld_and_op_writers(tmpdir):
    ''' Check that generated loop over cells in the psy layer has the
    correct upper bound when a kernel writes to both an operator and a
    field, the latter on a discontinuous space and first in the list
    of args. (Loop must include L1 halo because we're writing to an
    operator.)

    '''
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "1.5.2_single_invoke_write_fld_op.f90"),
        api=TEST_API)
    for dist_mem in [False, True]:
        psy = PSyFactory(TEST_API,
                         distributed_memory=dist_mem).create(invoke_info)
        generated_code = str(psy.gen)
        if dist_mem:
            assert ("loop0_stop = mesh%get_last_halo_cell(1)\n" in
                    generated_code)
            output = (
                "    do cell = loop0_start, loop0_stop, 1\n")
            assert output in generated_code
        else:
            assert ("loop0_stop = op1_proxy%fs_from%get_ncell()\n" in
                    generated_code)
            output = (
                "    ! Call kernels\n"
                "    do cell = loop0_start, loop0_stop, 1")
            assert output in generated_code

        assert LFRicBuild(tmpdir).code_compiles(psy)


def test_itn_space_any_any_discontinuous(dist_mem, tmpdir):
    ''' Check that generated loop over cells has correct upper
    bound when a kernel writes to fields on any_space (continuous)
    and any_discontinuous_space.

    '''
    _, invoke_info = parse(
        os.path.join(BASE_PATH,
                     "1.5.3_single_invoke_write_any_anyd_space.f90"),
        api=TEST_API)
    psy = PSyFactory(TEST_API,
                     distributed_memory=dist_mem).create(invoke_info)
    generated_code = str(psy.gen)

    assert LFRicBuild(tmpdir).code_compiles(psy)

    assert "loop0_start = 1\n" in generated_code

    if dist_mem:
        assert "loop0_stop = mesh%get_last_halo_cell(1)" in generated_code
        output = (
            "    do cell = loop0_start, loop0_stop, 1\n")
        assert output in generated_code
    else:
        assert "loop0_stop = f1_proxy%vspace%get_ncell()" in generated_code
        output = (
            "    ! Call kernels\n"
            "    do cell = loop0_start, loop0_stop, 1\n")
        assert output in generated_code


def test_itn_space_any_w2trace(dist_mem, tmpdir):
    ''' Check generated loop over cells has correct upper bound when a
    kernel writes to fields on any_space and W2trace (both continuous).

    '''
    _, invoke_info = parse(
        os.path.join(BASE_PATH,
                     "1.5.4_single_invoke_write_anyspace_w2trace.f90"),
        api=TEST_API)
    psy = PSyFactory(TEST_API,
                     distributed_memory=dist_mem).create(invoke_info)
    generated_code = str(psy.gen)

    assert LFRicBuild(tmpdir).code_compiles(psy)

    assert "loop0_start = 1\n" in generated_code

    if dist_mem:
        assert "loop0_stop = mesh%get_last_halo_cell(1)\n" in generated_code
        output = (
            "    do cell = loop0_start, loop0_stop, 1\n")
        assert output in generated_code
    else:
        # Loop upper bound should use f2 as that field is *definitely*
        # on a continuous space (as opposed to the one on any_space
        # that might be).
        assert "loop0_stop = f2_proxy%vspace%get_ncell()" in generated_code
        output = (
            "    ! Call kernels\n"
            "    do cell = loop0_start, loop0_stop, 1\n")
        assert output in generated_code


def test_no_halo_for_discontinuous(tmpdir):
    ''' Test that we do not create halo exchange calls when our loop
    only iterates over owned cells (e.g. it writes to a discontinuous
    field), we only read from a discontinuous field and there are no
    stencil accesses.

    '''
    _, info = parse(os.path.join(BASE_PATH,
                                 "1_single_invoke_w2v.f90"),
                    api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(info)
    result = str(psy.gen)
    assert "halo_exchange" not in result

    assert LFRicBuild(tmpdir).code_compiles(psy)


def test_halo_for_discontinuous(tmpdir, monkeypatch, annexed):
    '''This test checks the case when our loop iterates over owned cells
    (e.g. it writes to a discontinuous field), we read from a
    continuous field, there are no stencil accesses, but we do not
    know anything about the previous writer.

    As we don't know anything about the previous writer we have to
    assume that it may have been over dofs. If so, we could have dirty
    annexed dofs so need to add a halo exchange (for the three
    continuous fields being read (f1, f2 and m1). This is the case
    when api_config.compute_annexed_dofs is False.

    If we always iterate over annexed dofs by default, our annexed
    dofs will always be clean. Therefore we do not need to add a halo
    exchange. This is the case when
    api_config.compute_annexed_dofs is True.

    '''
    api_config = Config.get().api_conf(TEST_API)
    monkeypatch.setattr(api_config, "_compute_annexed_dofs", annexed)
    _, info = parse(os.path.join(BASE_PATH,
                                 "1_single_invoke_w3.f90"),
                    api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(info)
    result = str(psy.gen)
    if annexed:
        assert "halo_exchange" not in result
    else:
        assert "if (f1_proxy%is_dirty(depth=1)) then" in result
        assert "call f1_proxy%halo_exchange(depth=1)" in result
        assert "if (f2_proxy%is_dirty(depth=1)) then" in result
        assert "call f2_proxy%halo_exchange(depth=1)" in result
        assert "if (m1_proxy%is_dirty(depth=1)) then" in result
        assert "call m1_proxy%halo_exchange(depth=1)" in result

    assert LFRicBuild(tmpdir).code_compiles(psy)


def test_halo_for_discontinuous_2(tmpdir, monkeypatch, annexed):
    '''This test checks the case when our loop iterates over owned cells
    (e.g. it writes to a discontinuous field), we read from a
    continuous field, there are no stencil accesses, and the previous
    writer iterates over ndofs or nannexed.

    When the previous writer iterates over ndofs we have dirty annexed
    dofs so need to add a halo exchange. This is the case when
    api_config.compute_annexed_dofs is False.

    When the previous writer iterates over nannexed we have clean
    annexed dofs so do not need to add a halo exchange. This is the
    case when api_config.compute_annexed_dofs is True

    '''
    api_config = Config.get().api_conf(TEST_API)
    monkeypatch.setattr(api_config, "_compute_annexed_dofs", annexed)
    _, info = parse(os.path.join(BASE_PATH,
                                 "14.7_halo_annexed.f90"),
                    api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(info)
    result = str(psy.gen)
    if annexed:
        assert "halo_exchange" not in result
    else:
        assert "if (f1_proxy%is_dirty(depth=1)) then" not in result
        assert "call f1_proxy%halo_exchange(depth=1)" in result
        assert "if (f2_proxy%is_dirty(depth=1)) then" not in result
        assert "call f2_proxy%halo_exchange(depth=1)" in result
        assert "if (m1_proxy%is_dirty(depth=1)) then" in result
        assert "call m1_proxy%halo_exchange(depth=1)" in result

    assert LFRicBuild(tmpdir).code_compiles(psy)


def test_halo_for_annexed_dofs_read(tmpdir, annexed, monkeypatch):
    '''
    Test that a halo exchange is inserted before a kernel that reads from
    both a continuous field and an operator in order to ensure annexed dofs
    are clean.

    '''
    api_config = Config.get().api_conf(TEST_API)
    monkeypatch.setattr(api_config, "_compute_annexed_dofs", annexed)
    psy, invoke = get_invoke("15.1.11_builtin_and_op_kernel_invoke.f90",
                             idx=0, api=TEST_API, dist_mem=True)
    result = str(psy.gen).lower()
    if annexed:
        assert "loop0_stop = f2_proxy%vspace%get_last_dof_annexed" in result
        assert "loop1_stop = mesh%get_last_edge_cell()" in result
        # Halo dofs are left dirty, even though the annexed dofs are clean
        assert "call f2_proxy%set_dirty()" in result
        # No halo exchange required
        assert "call f2_proxy%halo_exchange" not in result
    else:
        assert "loop0_stop = f2_proxy%vspace%get_last_dof_owned()" in result
        assert "loop1_stop = mesh%get_last_edge_cell()" in result
        # f2 must be halo-exchanged to clean its annexed dofs.
        assert "call f2_proxy%set_dirty()" in result
        assert "call f2_proxy%halo_exchange(depth=1)" in result
    assert LFRicBuild(tmpdir).code_compiles(psy)


def test_lfricloop_halo_read_access_error1(monkeypatch):
    '''Test that the halo_read_access method in class LFRicLoop raises the
    expected exception when an unsupported field access is found.

    '''
    _, invoke_info = parse(os.path.join(BASE_PATH, "1_single_invoke.f90"),
                           api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(invoke_info)
    schedule = psy.invokes.invoke_list[0].schedule
    loop = schedule[4]
    kernel = loop.loop_body[0]
    field = kernel.arguments.args[1]
    monkeypatch.setattr(field, "_access", "unsupported")
    with pytest.raises(InternalError) as info:
        loop._halo_read_access(field)
    assert ("Unexpected field access type 'unsupported' found for arg 'f1'."
            in str(info.value))


def test_lfricloop_halo_read_access_error2(monkeypatch):
    '''Test that the halo_read_access method in class LFRicLoop raises the
    expected exception when an unsupported field type is found.

    '''
    _, invoke_info = parse(os.path.join(BASE_PATH, "1_single_invoke.f90"),
                           api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(invoke_info)
    schedule = psy.invokes.invoke_list[0].schedule
    loop = schedule[4]
    kernel = loop.loop_body[0]
    field = kernel.arguments.args[1]
    monkeypatch.setattr(field, "_argument_type", "unsupported")
    with pytest.raises(InternalError) as info:
        loop._halo_read_access(field)
    assert ("Expecting arg 'f1' to be an operator, scalar or field, but "
            "found 'unsupported'." in str(info.value))


@pytest.mark.usefixtures("lfric_config")
def test_null_loop():
    ''' Check that we can create a 'null'-type loop and that the validation
    check in the 'load()' method behaves as expected.

    '''
    loop = LFRicLoop(loop_type="null", parent=LFRicInvokeSchedule.create("a"))
    assert loop.loop_type == "null"
    assert loop.node_str(colour=False) == "Loop[type='null']"

    # Create a kernel by parsing some metadata
    ast = fpapi.parse('''
module testkern_mod
  type, extends(kernel_type) :: testkern_type
     type(arg_type), meta_args(2) =                         &
          (/ arg_type(gh_scalar, gh_real, gh_read),         &
             arg_type(gh_field,  gh_real, gh_readwrite, w3) &
           /)
     integer :: operates_on = cell_column
   contains
     procedure, nopass :: code => testkern_code
  end type testkern_type
contains
  subroutine testkern_code(a, b, c, d)
  end subroutine testkern_code
end module testkern_mod
''', ignore_comments=False)
    dkm = LFRicKernMetadata(ast, name="testkern_type")
    kern = LFRicKern()
    kern.load_meta(dkm)
    with pytest.raises(GenerationError) as err:
        loop.load(kern)
    assert ("A LFRicLoop of type 'null' can only contain a kernel that "
            "operates on the 'domain' but kernel 'testkern_code' operates "
            "on 'cell_column'" in str(err.value))


def test_loop_independent_iterations(monkeypatch, dist_mem):
    '''Tests for the independent_iterations() method.'''
    # A 'null' loop cannot be parallelised (because there's nothing to
    # parallelise).
    loop = LFRicLoop(loop_type="null", parent=LFRicInvokeSchedule.create("a"))
    assert not loop.independent_iterations()
    # A loop over all columns that contains a kernel that increments a field
    # on a continuous function space does not have independent iterations.
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "1_single_invoke.f90"),
                           api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=dist_mem).create(invoke_info)
    schedule = psy.invokes.invoke_list[0].schedule
    loop = schedule.walk(LFRicLoop)[0]
    assert not loop.independent_iterations()
    dtools = DependencyTools()
    # Check that we get the expected message back from the DA
    loop.independent_iterations(dep_tools=dtools)
    msgs = dtools.get_all_messages()
    assert msgs[0].code == DTCode.ERROR_WRITE_WRITE_RACE
    # Colour the loop.
    trans = LFRicColourTrans()
    trans.apply(loop)
    loops = schedule.walk(LFRicLoop)
    assert not loops[0].independent_iterations()
    assert loops[1].independent_iterations()
    # Unsupported/unknown loop type
    monkeypatch.setattr(loops[1], "_loop_type", "broken")
    with pytest.raises(InternalError) as err:
        loops[1].independent_iterations()
    assert "loop of type 'broken' is not supported" in str(err.value)
    monkeypatch.undo()
    # Test when the DA returns True. Since this currently never happens for an
    # LFRic kernel that operates on cell columns we use monkeypatch.
    monkeypatch.setattr(DependencyTools, "can_loop_be_parallelised",
                        lambda _1, _2, test_all_variables=False,
                        signatures_to_ignore=[]: True)
    assert loop.independent_iterations()
    # Test when the DA raises an exception. This is hard to reproduce (it
    # depends on the precise names used for arguments to a kernel) so we
    # simply use monkeypatch again to check that it is handled.

    def fake(_1, _2, test_all_variables=False, signatures_to_ignore=None):
        raise KeyError("This is just a test")
    monkeypatch.setattr(DependencyTools, "can_loop_be_parallelised", fake)
    assert loop.independent_iterations()


def test_dof_loop_independent_iterations(monkeypatch, dist_mem):
    '''
    Test that independent_iterations() behaves as expected when a loop
    is over dofs.
    '''
    # Loop over dofs containing a reduction.
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "15.9.1_X_innerproduct_Y_builtin.f90"),
                           api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=dist_mem).create(invoke_info)
    loop = psy.invokes.invoke_list[0].schedule.walk(LFRicLoop)[0]
    assert loop.loop_type == "dof"
    assert not loop.independent_iterations()
    # Test that we can get hold of any DA messages if we supply our own
    # instance of DependencyTools.
    dtools = DependencyTools()
    loop.independent_iterations(dep_tools=dtools)
    msgs = dtools.get_all_messages()
    assert len(msgs) == 2
    msg_codes = [msg.code for msg in msgs]
    assert DTCode.WARN_SCALAR_WRITTEN_ONCE in msg_codes
    assert DTCode.WARN_SCALAR_REDUCTION in msg_codes
    # Loop over dofs with no reduction.
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "15.1.6_aX_plus_bY_builtin.f90"),
                           api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=dist_mem).create(invoke_info)
    schedule = psy.invokes.invoke_list[0].schedule
    loop = schedule.walk(LFRicLoop)[0]
    assert loop.independent_iterations()
    # Test that DA warnings about a scalar variable are ignored if the loop is
    # over DoFs.
    monkeypatch.setattr(DependencyTools, "get_all_messages",
                        lambda _1: [Message("just a test",
                                            DTCode.WARN_SCALAR_REDUCTION)])
    assert loop.independent_iterations()


def test_upper_bound_psyir_invalid_bound():
    ''' Tests we raise an exception in the LFRicLoop:_upper_bound_psyir()
    method when 'cell_halo', 'dof_halo' or 'inner' are used.

    '''
    _, invoke_info = parse(os.path.join(BASE_PATH, "1_single_invoke.f90"),
                           api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=False).create(invoke_info)
    my_loop = psy.invokes.invoke_list[0].schedule.children[0]
    for option in ["cell_halo", "dof_halo", "inner"]:
        my_loop.set_upper_bound(option, halo_depth=1)
        with pytest.raises(GenerationError) as excinfo:
            _ = my_loop.upper_bound_psyir()
            assert (
                f"'{option}' is not a valid loop upper bound for sequential/"
                f"shared-memory code" in str(excinfo.value))


def test_upper_bound_psyir_invalid_within_colouring(monkeypatch):
    ''' Tests we raise an exception in the LFRicLoop:_upper_bound_psyir()
    method if an invalid state is provided.

    '''
    _, invoke_info = parse(os.path.join(BASE_PATH, "1_single_invoke.f90"),
                           api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=False).create(invoke_info)
    my_loop = psy.invokes.invoke_list[0].schedule.children[0]
    monkeypatch.setattr(my_loop, "_upper_bound_name", value="invalid")
    with pytest.raises(GenerationError) as excinfo:
        _ = my_loop.upper_bound_psyir()
    assert (
        "Unsupported upper bound name 'invalid' found" in str(excinfo.value))
    # Pretend the loop is over colours and passes the is_coloured check, but
    # it does not have colouring symbols associated
    monkeypatch.setattr(my_loop, "_upper_bound_name", value="ncolours")
    monkeypatch.setattr(Kern, "is_coloured", lambda x: True)
    with pytest.raises(InternalError) as excinfo:
        _ = my_loop.upper_bound_psyir()
    assert ("All kernels within a loop over colours must have been coloured "
            "but kernel 'testkern_code' has not"
            in str(excinfo.value))
    # Pretend the loop is over tiled-colours and passes the is_coloured check,
    # but it does not have colouring symbols associated
    monkeypatch.setattr(my_loop, "_upper_bound_name", value="ntilecolours")
    monkeypatch.setattr(Kern, "is_coloured", lambda x: True)
    with pytest.raises(InternalError) as excinfo:
        _ = my_loop.upper_bound_psyir()
    assert ("All kernels within a loop over colours must have been coloured "
            "but kernel 'testkern_code' has not"
            in str(excinfo.value))
    # Pretend the loop is over colours and does not contain a kernel
    monkeypatch.setattr(my_loop, "_upper_bound_name", value="ncolours")
    monkeypatch.setattr(my_loop, "walk", lambda x: [])
    with pytest.raises(InternalError) as excinfo:
        _ = my_loop.upper_bound_psyir()
    assert ("Failed to find a kernel within a loop over colours"
            in str(excinfo.value))
    # Pretend the loop is over tilecolours and does not contain a kernel
    monkeypatch.setattr(my_loop, "_upper_bound_name", value="ntilecolours")
    monkeypatch.setattr(my_loop, "walk", lambda x: [])
    with pytest.raises(InternalError) as excinfo:
        _ = my_loop.upper_bound_psyir()
    assert ("Failed to find a kernel within a loop over tile-colours"
            in str(excinfo.value))
    # Pretend the loop is over ntiles_per_colour_halo with no dist_mem
    monkeypatch.setattr(my_loop, "_upper_bound_name",
                        value="ntiles_per_colour_halo")
    st = psy.invokes.invoke_list[0].schedule.symbol_table
    st.new_symbol("mesh", tag="mesh")
    with pytest.raises(GenerationError) as excinfo:
        _ = my_loop.upper_bound_psyir()
    assert ("'last_halo_tile_per_colour' is not a valid loop upper bound for"
            " non-distributed-memory code" in str(excinfo.value))
    # Pretend the loop is over ncells_per_colour_and_tile_halo with no dist_mem
    monkeypatch.setattr(my_loop, "_upper_bound_name",
                        value="ncells_per_colour_and_tile_halo")
    st = psy.invokes.invoke_list[0].schedule.symbol_table
    with pytest.raises(GenerationError) as excinfo:
        _ = my_loop.upper_bound_psyir()
    assert ("'last_halo_cell_per_colour_and_tile' is not a valid loop upper "
            "bound for non-distributed-memory code" in str(excinfo.value))


def test_upper_bound_psyir_inner(monkeypatch):
    ''' Check that we get the correct Fortran generated if a loop's upper
    bound is 'inner'. There are no transformations that allow this
    configuration, so we need to patch the value.

    '''
    _, invoke_info = parse(os.path.join(BASE_PATH, "1_single_invoke.f90"),
                           api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(invoke_info)
    my_loop = psy.invokes.invoke_list[0].schedule.children[4]
    monkeypatch.setattr(my_loop, "_upper_bound_name", value="inner")
    ubound = my_loop.upper_bound_psyir()
    assert "mesh%get_last_inner_cell(1)" in ubound.debug_string()
