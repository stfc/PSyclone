# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017-2026, Science and Technology Facilities Council.
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
# Authors: R. W. Ford, A. R. Porter, S. Siso and N. Nobre, STFC Daresbury Lab
# Modified: I. Kavcic, A. Coughtrie, O. Brunt and L. Turner, Met Office
#          C.M. Maynard, Met Office / University of Reading
# Modified: J. Henrichs, Bureau of Meteorology
# Modified: A. B. G. Chalk, STFC Daresbury Lab
#           J. Dendy, Met Office

''' Tests of the LFRic redundant-computation transformation. '''

import pytest

from psyclone.configuration import Config
from psyclone.core import AccessType
from psyclone.domain.lfric import LFRicLoop
from psyclone.domain.lfric.transformations import (
    LFRicRedundantComputationTrans)
from psyclone.errors import GenerationError
from psyclone.lfric import LFRicHaloExchange
from psyclone.psyGen import HaloExchange
from psyclone.psyir.nodes import Loop
from psyclone.psyir.transformations import TransformationError
from psyclone.tests.lfric_build import LFRicBuild
from psyclone.tests.utilities import get_invoke


TEST_API = "lfric"


def test_rc_str():
    '''Test the str method and name property of the
    LFRicRedundantComputationTrans class. '''
    rc_trans = LFRicRedundantComputationTrans()
    rc_name = str(rc_trans)
    assert rc_name == "Change iteration space to perform redundant computation"


def test_rc_node_not_loop():
    '''Test that LFRicRedundantComputationTrans raises an exception if the
    node argument is not a loop. '''
    _, invoke = get_invoke("1_single_invoke.f90", TEST_API,
                           idx=0, dist_mem=True)
    schedule = invoke.schedule
    rc_trans = LFRicRedundantComputationTrans()
    with pytest.raises(TransformationError) as excinfo:
        rc_trans.apply(schedule.children[0])
    assert ("Target of LFRicRedundantComputationTrans transformation must "
            "be a sub-class of Loop but got \'LFRicHaloExchange\'" in
            str(excinfo.value))


def test_rc_invalid_loop(monkeypatch):
    ''' Test that LFRicRedundantComputationTrans raises an exception if the
    supplied loop does not iterate over cells or dofs. '''
    _, invoke = get_invoke("1_single_invoke.f90", TEST_API,
                           idx=0, dist_mem=True)
    schedule = invoke.schedule
    rc_trans = LFRicRedundantComputationTrans()
    loop = schedule.children[4]
    # set the loop to a type that should raise an exception
    monkeypatch.setattr(loop, "loop_type", value="colours")
    with pytest.raises(TransformationError) as excinfo:
        rc_trans.apply(loop)
    assert ("In the LFRicRedundantComputation transformation apply "
            "method the loop type must be one of '' (cell-columns), 'dof' or "
            "'cells_in_colour', but found 'colours'") in str(excinfo.value)


def test_rc_nodm():
    '''Test that LFRicRedundantComputationTrans raises an exception if
    distributed memory is not set. '''
    _, invoke = get_invoke("1_single_invoke.f90", TEST_API,
                           idx=0, dist_mem=False)
    schedule = invoke.schedule
    rc_trans = LFRicRedundantComputationTrans()
    loop = schedule.children[0]
    with pytest.raises(TransformationError) as excinfo:
        rc_trans.apply(loop)
    assert ("In the LFRicRedundantComputation transformation apply method "
            "distributed memory must be switched on") in str(excinfo.value)


def test_rc_no_halo_kernels():
    '''
    Test that LFRicRedundantComputationTrans refuses to transform a kernel
    that operates on halo cells.

    '''
    _, invoke = get_invoke("1.4.1_into_halos_plus_domain_invoke.f90",
                           TEST_API, idx=0, dist_mem=True)
    rc_trans = LFRicRedundantComputationTrans()
    loop = invoke.schedule.walk(LFRicLoop)[0]
    with pytest.raises(TransformationError) as err:
        rc_trans.validate(loop)
    assert ("LFRicRedundantComputationTrans transformation to kernels that"
            " operate on halo cells but kernel 'testkern_halo_only_code' "
            "operates on 'halo_cell_column'" in str(err.value))


def test_rc_no_owned_cell_kernels(monkeypatch):
    '''
    Test that LFRicRedundantComputationTrans refuses to transform a kernel
    that must operate only on owned cells or dofs.

    '''
    # Set compute_annexed_dofs to False as that's the only permitted setting
    # for the kernels in this test.
    config = Config.get()
    lfric_config = config.api_conf("lfric")
    monkeypatch.setattr(lfric_config, "_compute_annexed_dofs", False)
    _, invoke = get_invoke("1.4.5_owned_only_invoke.f90",
                           TEST_API, idx=0, dist_mem=True)
    rc_trans = LFRicRedundantComputationTrans()
    loops = invoke.schedule.walk(LFRicLoop)
    with pytest.raises(TransformationError) as err:
        rc_trans.validate(loops[0])
    assert ("LFRicRedundantComputationTrans transformation to kernel "
            "'testkern_owned_cell_code' because it does not support redundant "
            "computation (it operates on 'owned_cell_column')"
            in str(err.value))
    with pytest.raises(TransformationError) as err:
        rc_trans.validate(loops[1])
    assert ("LFRicRedundantComputationTrans transformation to kernel "
            "'setval_random' because it does not support redundant computation"
            " (it operates on 'owned_dof')" in str(err.value))


def test_rc_invalid_depth():
    ''' Test that LFRicRedundantComputationTrans raises an exception if the
    supplied depth is less than 1. '''
    _, invoke = get_invoke("1_single_invoke.f90", TEST_API,
                           idx=0, dist_mem=True)
    schedule = invoke.schedule
    rc_trans = LFRicRedundantComputationTrans()
    loop = schedule.children[4]
    # TODO #2668: remove options dictionary.
    with pytest.raises(TransformationError) as excinfo:
        rc_trans.apply(loop, {"depth": 0})
    assert ("In the LFRicRedundantComputation transformation apply method "
            "the supplied depth is less than 1") in str(excinfo.value)
    with pytest.raises(TransformationError) as excinfo:
        rc_trans.apply(loop, depth=0)
    assert ("In the LFRicRedundantComputation transformation apply method "
            "the supplied depth is less than 1") in str(excinfo.value)


def test_rc_invalid_depth_continuous():
    ''' Test that LFRicRedundantComputationTrans raises an exception if the
    supplied depth equals 1 when modifying a continuous field. '''
    _, invoke = get_invoke("1_single_invoke.f90", TEST_API,
                           idx=0, dist_mem=True)
    schedule = invoke.schedule
    rc_trans = LFRicRedundantComputationTrans()
    loop = schedule.children[4]
    # TODO #2668: Deprecate options dictionary.
    with pytest.raises(TransformationError) as excinfo:
        rc_trans.apply(loop, {"depth": 1})
    assert ("In the LFRicRedundantComputation transformation apply method "
            "the supplied depth (1) must be greater than the existing halo "
            "depth (1)") in str(excinfo.value)
    with pytest.raises(TransformationError) as excinfo:
        rc_trans.apply(loop, depth=1)
    assert ("In the LFRicRedundantComputation transformation apply method "
            "the supplied depth (1) must be greater than the existing halo "
            "depth (1)") in str(excinfo.value)


def test_rc_continuous_depth():
    ''' Test that the loop bounds for a continuous kernel (iterating over
    cells) are modified appropriately, that set_clean() is added
    correctly and halo_exchange modified appropriately after applying
    the redundant computation transformation with a fixed value for
    halo depth.

    '''
    rc_trans = LFRicRedundantComputationTrans()
    # TODO #2668: Deprecate options dictionary and remove this loop.
    for do_test in (1, 2):
        psy, invoke = get_invoke("1_single_invoke.f90", TEST_API,
                                 idx=0, dist_mem=True)
        loop = invoke.schedule.children[4]
        if do_test == 1:
            # TODO #2668: Deprecate options dictionary
            rc_trans.apply(loop, {"depth": 3})
        else:
            rc_trans.apply(loop, depth=3)

        result = str(psy.gen)

        for field_name in ["f2", "m1", "m2"]:
            assert f"if ({field_name}_proxy%is_dirty(depth=3)) then" in result
            assert f"call {field_name}_proxy%halo_exchange(depth=3)" in result
            assert "loop0_stop = mesh%get_last_halo_cell(3)" in result
            assert "do cell = loop0_start, loop0_stop" in result
            assert ("    call f1_proxy%set_dirty()\n"
                    "    call f1_proxy%set_clean(2)") in result


def test_rc_continuous_no_depth():
    ''' Test that the loop bounds for a continuous kernel (iterating over
    cells) are modified appropriately, that set_clean() is added
    correctly and halo_exchange modified appropriately after applying
    the redundant computation transformation with no value for halo
    depth.

    '''
    psy, invoke = get_invoke("1_single_invoke.f90", TEST_API,
                             idx=0, dist_mem=True)
    schedule = invoke.schedule
    rc_trans = LFRicRedundantComputationTrans()
    loop = schedule.children[4]
    rc_trans.apply(loop)
    result = str(psy.gen)

    assert ("    if (f1_proxy%is_dirty(depth=max_halo_depth_mesh - 1)) then"
            "\n"
            "      call f1_proxy%halo_exchange(depth=max_halo_depth_mesh"
            " - 1)" in result)
    for fname in ["f2", "m1", "m2"]:
        assert (f"    if ({fname}_proxy%is_dirty(depth=max_halo_depth_mesh"
                f")) then\n"
                f"      call {fname}_proxy%halo_exchange(depth=max_halo_"
                f"depth_mesh)" in result)
    assert "loop0_stop = mesh%get_last_halo_cell()" in result
    assert "do cell = loop0_start, loop0_stop" in result
    assert ("    call f1_proxy%set_dirty()\n"
            "    call f1_proxy%set_clean(max_halo_depth_mesh - 1)") in result


def test_rc_discontinuous_depth(tmpdir, annexed):
    '''Test that the loop bounds for a discontinuous kernel (iterating
    over cells) with continuous reads are modified appropriately and
    set_clean() added correctly and halo_exchange added appropriately
    after applying the redundant computation transformation with a
    fixed value for halo depth. Also test with and without annexed
    dofs being computed as this affects the generated code.

    '''
    psy, invoke = get_invoke("1_single_invoke_w3.f90", TEST_API,
                             idx=0, dist_mem=True)
    schedule = invoke.schedule
    rc_trans = LFRicRedundantComputationTrans()
    if annexed:
        # there are no halo exchange calls
        index = 0
    else:
        # there are 3 halo exchange calls
        index = 3
    loop = schedule.children[index]
    rc_trans.apply(loop, depth=3)
    result = str(psy.gen)
    for field_name in ["f1", "f2", "m1"]:
        assert (f"    if ({field_name}_proxy%is_dirty(depth=3)) then\n"
                f"      call {field_name}_proxy%halo_exchange(depth=3)"
                in result)
    assert "loop0_stop = mesh%get_last_halo_cell(3)" in result
    assert "do cell = loop0_start, loop0_stop" in result
    assert ("    call m2_proxy%set_dirty()\n"
            "    call m2_proxy%set_clean(3)") in result

    assert LFRicBuild(tmpdir).code_compiles(psy)


def test_rc_discontinuous_no_depth(annexed):
    '''Test that the loop bounds for a discontinuous kernel (iterating
    over cells) with continuous reads are modified appropriately and
    set_clean() added correctly and halo_exchange added appropriately
    after applying the redundant computation transformation with no
    halo depth value. Also test with and without annexed dofs being
    computed as this affects the generated code.

    '''
    psy, invoke = get_invoke("1_single_invoke_w3.f90", TEST_API,
                             idx=0, dist_mem=True)
    schedule = invoke.schedule
    rc_trans = LFRicRedundantComputationTrans()
    if annexed:
        # there are no halo exchange calls
        index = 0
    else:
        # there are 3 halo exchange calls
        index = 3
    loop = schedule.children[index]
    rc_trans.apply(loop)
    result = str(psy.gen)

    for field_name in ["f1", "f2", "m1"]:
        assert (f"if ({field_name}_proxy%is_dirty(depth=max_halo_depth_mesh)) "
                f"then" in result)
        assert (f"call {field_name}_proxy%halo_exchange("
                f"depth=max_halo_depth_mesh)" in result)
    assert "loop0_stop = mesh%get_last_halo_cell()" in result
    assert "do cell = loop0_start, loop0_stop" in result
    assert "call m2_proxy%set_dirty()" not in result
    assert "call m2_proxy%set_clean(max_halo_depth_mesh)" in result


def test_rc_all_discontinuous_depth(tmpdir):
    ''' Test that the loop bounds for a discontinuous kernel
    (iterating over cells) with discontinuous reads are modified
    appropriately and set_clean() added correctly and halo_exchange
    added appropriately after applying the redundant computation
    transformation with a fixed value for halo depth. '''
    psy, invoke = get_invoke("1_single_invoke_wtheta.f90", TEST_API,
                             idx=0, dist_mem=True)
    schedule = invoke.schedule
    rc_trans = LFRicRedundantComputationTrans()
    loop = schedule.children[0]
    rc_trans.apply(loop, depth=3)
    result = str(psy.gen)
    assert "if (f2_proxy%is_dirty(depth=3)) then" in result
    assert "call f2_proxy%halo_exchange(depth=3)" in result
    assert "loop0_stop = mesh%get_last_halo_cell(3)" in result
    assert "do cell = loop0_start, loop0_stop" in result
    assert "call f1_proxy%set_dirty()" in result
    assert "call f1_proxy%set_clean(3)" in result

    assert LFRicBuild(tmpdir).code_compiles(psy)


def test_rc_all_discontinuous_no_depth(tmpdir):
    ''' Test that the loop bounds for a discontinuous kernel
    (iterating over cells) with discontinuous reads are modified
    appropriately and set_clean() added correctly and halo_exchange
    added appropriately after applying the redundant computation
    transformation with no halo depth value. '''
    psy, invoke = get_invoke("1_single_invoke_w2v.f90", TEST_API,
                             idx=0, dist_mem=True)
    schedule = invoke.schedule
    rc_trans = LFRicRedundantComputationTrans()
    loop = schedule.children[0]
    rc_trans.apply(loop)
    result = str(psy.gen)

    assert "if (f2_proxy%is_dirty(depth=max_halo_depth_mesh)) then" in result
    assert "call f2_proxy%halo_exchange(depth=max_halo_depth_mesh)" in result
    assert "loop0_stop = mesh%get_last_halo_cell()" in result
    assert "do cell = loop0_start, loop0_stop" in result
    assert "call f1_proxy%set_clean(max_halo_depth_mesh)" in result

    assert LFRicBuild(tmpdir).code_compiles(psy)


def test_rc_all_discontinuous_vector_depth(tmpdir):
    ''' Test that the loop bounds for a discontinuous kernel (iterating
    over cells) are modified appropriately and set_clean() added
    correctly and halo_exchange added appropriately for vector fields
    after applying the redundant computation transformation with a
    fixed value for halo depth. '''
    psy, invoke = get_invoke("1_single_invoke_w3_only_vector.f90", TEST_API,
                             idx=0, dist_mem=True)
    schedule = invoke.schedule
    rc_trans = LFRicRedundantComputationTrans()
    loop = schedule.children[0]
    rc_trans.apply(loop, depth=3)
    result = str(psy.gen)

    for idx in range(1, 4):
        assert f"if (f2_proxy({idx})%is_dirty(depth=3)) then" in result
        assert f"call f2_proxy({idx})%halo_exchange(depth=3)" in result
    assert "loop0_stop = mesh%get_last_halo_cell(3)" in result
    assert "do cell = loop0_start, loop0_stop" in result
    for idx in range(1, 4):
        assert f"call f1_proxy({idx})%set_dirty()" in result
        assert f"call f1_proxy({idx})%set_clean(3)" in result

    assert LFRicBuild(tmpdir).code_compiles(psy)


def test_rc_all_discontinuous_vector_no_depth(tmpdir):
    ''' Test that the loop bounds for a discontinuous kernel (iterating
    over cells) are modified appropriately and set_clean() added
    correctly and halo_exchange added appropriately for vector fields
    after applying the redundant computation transformation with no
    halo depth value. '''
    psy, invoke = get_invoke("1_single_invoke_wtheta_only_vector.f90",
                             TEST_API, idx=0, dist_mem=True)
    schedule = invoke.schedule
    rc_trans = LFRicRedundantComputationTrans()
    loop = schedule.children[0]
    rc_trans.apply(loop)
    result = str(psy.gen)
    for idx in range(1, 4):
        assert (f"if (f2_proxy({idx})%is_dirty(depth=max_halo_depth_mesh"
                f")) then") in result
        assert (f"call f2_proxy({idx})%halo_exchange(depth=max_halo_depth_mesh"
                f")") in result
    assert "loop0_stop = mesh%get_last_halo_cell()" in result
    assert "do cell = loop0_start, loop0_stop" in result
    for idx in range(1, 4):
        assert f"call f1_proxy({idx})%set_clean(max_halo_depth_mesh)" in result

    assert LFRicBuild(tmpdir).code_compiles(psy)


def test_rc_all_disc_prev_depend_depth(tmpdir):
    ''' Test that the loop bounds for a discontinuous kernel
    (iterating over cells) with discontinuous reads are modified
    appropriately and set_clean() added correctly and halo_exchange
    added appropriately in the case where the field requiring a halo
    exchange has a previous non-halo dependence, after applying the
    redundant computation transformation with a fixed value for halo
    depth. '''
    psy, invoke = get_invoke("4.12_multikernel_invokes_w2v.f90", TEST_API,
                             idx=0, dist_mem=True)
    schedule = invoke.schedule
    rc_trans = LFRicRedundantComputationTrans()
    loop = schedule[1]
    rc_trans.apply(loop, depth=3)
    result = str(psy.gen)
    assert "if (f1_proxy%is_dirty(depth=3)) then" not in result
    assert "call f1_proxy%halo_exchange(depth=3)" in result
    assert "loop1_stop = mesh%get_last_halo_cell(3)" in result
    assert "do cell = loop1_start, loop1_stop" in result
    assert "call f1_proxy%set_dirty()" in result
    assert "call f3_proxy%set_dirty()" in result
    assert "call f3_proxy%set_clean(3)" in result

    assert LFRicBuild(tmpdir).code_compiles(psy)


def test_rc_all_disc_prev_depend_no_depth():
    ''' Test that the loop bounds for a discontinuous kernel
    (iterating over cells) are modified appropriately and set_clean()
    added correctly and halo_exchange added appropriately in the case
    where the field now requiring a halo exchange has a previous
    non-halo dependence after applying the redundant computation
    transformation with no halo depth value. '''
    psy, invoke = get_invoke("4.12_multikernel_invokes_w2v.f90", TEST_API,
                             idx=0, dist_mem=True)
    schedule = invoke.schedule
    rc_trans = LFRicRedundantComputationTrans()
    loop = schedule[1]
    rc_trans.apply(loop)
    result = str(psy.gen)
    assert "call f1_proxy%set_dirty()" in result
    assert ("if (f1_proxy%is_dirty(depth=max_halo_depth_mesh)) "
            "then") not in result
    assert "call f1_proxy%halo_exchange(depth=max_halo_depth_mesh)" in result
    assert "loop1_stop = mesh%get_last_halo_cell()" in result
    assert "do cell = loop1_start, loop1_stop" in result
    assert "call f3_proxy%set_clean(max_halo_depth_mesh)" in result


def test_rc_all_disc_prev_dep_depth_vector(tmpdir):
    ''' Test that the loop bounds for a discontinuous kernel (iterating
    over cells) with discontinuous reads are modified appropriately
    and set_clean() added correctly and halo_exchange added
    appropriately in the case where the vector field requiring a halo
    exchange has a previous non-halo dependence, after applying the
    redundant computation transformation with a fixed value for halo
    depth. '''
    psy, invoke = get_invoke("8.2.1_multikernel_invokes_w3_vector.f90",
                             TEST_API, idx=0, dist_mem=True)
    schedule = invoke.schedule
    rc_trans = LFRicRedundantComputationTrans()
    loop = schedule[1]
    rc_trans.apply(loop, depth=3)
    result = str(psy.gen)
    for idx in range(1, 4):
        assert f"if (f1_proxy({idx})%is_dirty(depth=3)) then" not in result
        assert f"call f1_proxy({idx})%halo_exchange(depth=3)" in result
        assert "loop1_stop = mesh%get_last_halo_cell(3)" in result
    assert "do cell = loop1_start, loop1_stop" in result
    for idx in range(1, 4):
        assert f"call f1_proxy({idx})%set_dirty()" in result
        assert f"call f3_proxy({idx})%set_dirty()" in result
        assert f"call f3_proxy({idx})%set_clean(3)" in result

    assert LFRicBuild(tmpdir).code_compiles(psy)


def test_rc_all_disc_prev_dep_no_depth_vect(tmpdir):
    ''' Test that the loop bounds for a discontinuous kernel (iterating
    over cells) are modified appropriately and set_clean() added
    correctly and halo_exchange added appropriately in the case where
    the vector field now requiring a halo exchange has a previous non-halo
    dependence after applying the redundant computation transformation
    with no halo depth value. '''
    psy, invoke = get_invoke("8.2.1_multikernel_invokes_w3_vector.f90",
                             TEST_API, idx=0, dist_mem=True)
    schedule = invoke.schedule
    rc_trans = LFRicRedundantComputationTrans()
    loop = schedule[1]
    rc_trans.apply(loop)
    result = str(psy.gen)
    assert "is_dirty" not in result
    for idx in range(1, 4):
        assert (f"call f1_proxy({idx})%halo_exchange(depth=max_halo_depth_"
                f"mesh)") in result
    assert "loop1_stop = mesh%get_last_halo_cell()" in result
    assert "do cell = loop1_start, loop1_stop" in result
    for idx in range(1, 4):
        assert f"call f1_proxy({idx})%set_dirty()" in result
        assert f"call f3_proxy({idx})%set_clean(max_halo_depth_mesh)" in result

    assert LFRicBuild(tmpdir).code_compiles(psy)


def test_rc_all_disc_prev_dep_no_depth_vect_readwrite(tmpdir):
    ''' Test that the loop bounds for a discontinuous kernel (iterating
    over cells) are modified appropriately and set_clean() added
    correctly and halo_exchange added appropriately in the case where
    the vector field now requiring a halo exchange has a previous halo
    dependence (readwrite access) after applying the redundant computation
    transformation with no halo depth value. '''
    psy, invoke = get_invoke("8.2.2_multikernel_invokes_wtheta_vector.f90",
                             TEST_API, idx=0, dist_mem=True)
    schedule = invoke.schedule
    rc_trans = LFRicRedundantComputationTrans()
    loop = schedule[1]
    rc_trans.apply(loop)
    result = str(psy.gen)
    # f3 has readwrite access so need to check the halos
    for idx in range(1, 4):
        assert (f"if (f3_proxy({idx})%is_dirty(depth=max_halo_depth_mesh))"
                in result)
        assert (f"call f3_proxy({idx})%halo_exchange(depth=max_halo_depth_mesh"
                ")" in result)
    # f1 has RW to W dependency
    for idx in range(1, 4):
        assert (f"call f1_proxy({idx})%halo_exchange(depth=max_halo_depth_mesh"
                f")" in result)
    assert "loop1_stop = mesh%get_last_halo_cell()" in result
    assert "do cell = loop1_start, loop1_stop" in result
    for idx in range(1, 4):
        assert f"call f1_proxy({idx})%set_dirty()" in result
        assert f"call f3_proxy({idx})%set_clean(max_halo_depth_mesh)" in result

    assert LFRicBuild(tmpdir).code_compiles(psy)


def test_rc_dofs_depth():
    ''' Test that the loop bounds when iterating over DoFs are modified
    appropriately and set_clean() added correctly and halo_exchange
    added appropriately after applying the redundant computation
    transformation with a fixed value for halo depth where the halo
    fields have no previous dependence.

    '''
    psy, invoke = get_invoke("15.1.2_inc_X_plus_Y_builtin.f90",
                             TEST_API, idx=0, dist_mem=True)
    schedule = invoke.schedule
    rc_trans = LFRicRedundantComputationTrans()
    loop = schedule.children[0]
    rc_trans.apply(loop, {"depth": 3})
    result = str(psy.gen)
    for field in ["f1", "f2"]:
        assert f"if ({field}_proxy%is_dirty(depth=3)) then" in result
        assert f"call {field}_proxy%halo_exchange(depth=3)" in result
    assert "loop0_stop = f1_proxy%vspace%get_last_dof_halo(3)" in result
    assert "do df = loop0_start, loop0_stop" in result
    assert "call f1_proxy%set_dirty()" in result
    assert "call f1_proxy%set_clean(3)" in result


def test_rc_dofs_no_depth():
    ''' Test that the loop bounds when iterating over DoFs are modified
    appropriately and set_clean() added correctly and halo_exchange
    added appropriately after applying the redundant computation
    transformation with no halo depth value where the halo fields have
    no previous dependence.

    '''
    psy, invoke = get_invoke("15.1.2_inc_X_plus_Y_builtin.f90",
                             TEST_API, idx=0, dist_mem=True)
    schedule = invoke.schedule
    rc_trans = LFRicRedundantComputationTrans()
    loop = schedule.children[0]
    rc_trans.apply(loop)
    result = str(psy.gen)

    assert "if (f2_proxy%is_dirty(depth=max_halo_depth_mesh)) then" in result
    assert "call f2_proxy%halo_exchange(depth=max_halo_depth_mesh)" in result
    assert "loop0_stop = f1_proxy%vspace%get_last_dof_halo()" in result
    assert "do df = loop0_start, loop0_stop" in result
    assert "call f1_proxy%set_dirty()" not in result
    assert "call f1_proxy%set_clean(max_halo_depth_mesh)" in result


def test_rc_dofs_depth_prev_dep(annexed, tmpdir):
    ''' Test that the loop bounds when iterating over DoFs are modified
    appropriately and set_clean() added correctly and halo_exchange
    added appropriately after applying the redundant computation
    transformation with a fixed value for halo depth where the halo
    fields have a previous (non-halo-exchange) dependence. Also test
    with and without annexed dofs.

    '''
    psy, invoke = get_invoke("15.1.1_builtin_and_normal_kernel_invoke_2.f90",
                             TEST_API, idx=0, dist_mem=True)
    schedule = invoke.schedule
    rc_trans = LFRicRedundantComputationTrans()
    if annexed:
        index = 4
    else:
        index = 5
    loop = schedule.children[index]
    rc_trans.apply(loop, {"depth": 3})
    result = str(psy.gen)

    assert LFRicBuild(tmpdir).code_compiles(psy)

    # Check that the f2 halo exchange is modified
    assert "call f2_proxy%halo_exchange(depth=3)" in result
    # There is a need for a run-time is_dirty check for field f2 as
    # this field is not modified in this invoke and therefore its halo
    # is in an unknown state before it is read
    assert ("if (f2_proxy%is_dirty(depth=3)) "
            "then") in result

    # Check that the existing halo exchanges (for the first un-modified
    # loop) remain unchanged. These are on f1, m1 and m2 without annexed
    # dofs and only on m1 and m2 with annexed dofs.
    fld_hex_names = ["f1", "m1", "m2"]
    if annexed:
        fld_hex_names.remove("f1")
    for field_name in fld_hex_names:
        assert f"if ({field_name}_proxy%is_dirty(depth=1)) then" in result
        assert f"call {field_name}_proxy%halo_exchange(depth=1)" in result
    assert "loop1_stop = f1_proxy%vspace%get_last_dof_halo(3)" in result
    assert "do df = loop1_start, loop1_stop" in result
    assert "call f1_proxy%set_dirty()" in result
    assert "call f1_proxy%set_clean(3)" in result


def test_rc_dofs_no_depth_prev_dep():
    ''' Test that the loop bounds when iterating over DoFs are modified
    appropriately and set_clean() added correctly and halo_exchange
    added appropriately after applying the redundant computation
    transformation with no halo depth value where the halo
    fields have a previous (non-halo-exchange) dependence.

    '''
    psy, invoke = get_invoke("15.1.1_builtin_and_normal_kernel_invoke_2.f90",
                             TEST_API, idx=0, dist_mem=True)
    schedule = invoke.schedule
    rc_trans = LFRicRedundantComputationTrans()
    loop = schedule[5]
    rc_trans.apply(loop)
    result = str(psy.gen)

    # Check that the f2 halo exchange is modified
    assert "call f2_proxy%halo_exchange(depth=max_halo_depth_mesh)" in result
    assert "if (f2_proxy%is_dirty(depth=max_halo_depth_mesh)) then" in result
    # Check that the existing f1, m1 and m2 halo exchanges remain unchanged
    for fname in ["f1", "m1", "m2"]:
        assert f"if ({fname}_proxy%is_dirty(depth=1)) then" in result
        assert f"call {fname}_proxy%halo_exchange(depth=1)" in result
    assert "loop1_stop = f1_proxy%vspace%get_last_dof_halo()" in result
    assert "do df = loop1_start, loop1_stop" in result
    assert "call f1_proxy%set_dirty()" in result
    assert "call f1_proxy%set_clean(max_halo_depth_mesh)" in result


def test_continuous_no_set_clean():
    '''Test that set_clean is not added for the default iteration space of
    a continuous loop. This is probably covered from tests in
    lfric_test.py but it is good to have a specific test. '''
    psy, _ = get_invoke("1_single_invoke.f90",
                        TEST_API, idx=0, dist_mem=True)
    result = str(psy.gen)
    assert "loop0_stop = mesh%get_last_halo_cell(1)" in result
    assert "do cell = loop0_start, loop0_stop" in result
    assert "call f1_proxy%set_dirty()" in result
    assert "call f1_proxy%set_clean(" not in result


def test_discontinuous_no_set_clean():
    ''' Test that set_clean is not added for the default iteration
    space of a discontinuous loop. This is probably covered from tests
    in lfric_test.py but it is good to have a specific test. '''
    psy, _ = get_invoke("1_single_invoke_w3.f90", TEST_API,
                        idx=0, dist_mem=True)
    result = str(psy.gen)
    assert "loop0_stop = mesh%get_last_edge_cell()" in result
    assert "call m2_proxy%set_dirty()" in result
    assert "call m2_proxy%set_clean(" not in result


def test_dofs_no_set_clean(annexed):
    ''' Test that set_clean is not added for the default iteration space
    of a loop over dofs. This is probably covered from tests in
    lfric_builtins_test.py but it is good to have a specific
    test. Also test with and without annexed dofs being computed as
    this affects the generated code.

    '''
    psy, _ = get_invoke("15.7.1_setval_c_builtin.f90", TEST_API,
                        idx=0, dist_mem=True)
    result = str(psy.gen)
    assert "halo_exchange" not in result
    if annexed:
        assert "loop0_stop = f1_proxy%vspace%get_last_dof_annexed()" in result
    else:
        assert "loop0_stop = f1_proxy%vspace%get_last_dof_owned()" in result
    assert "call f1_proxy%set_dirty()" in result
    assert "call f1_proxy%set_clean(" not in result


def test_rc_vector_depth(tmpdir):
    ''' Test that the loop bounds for a (continuous) vector are modified
    appropriately and set_clean() added correctly and halo_exchange
    added/modified appropriately after applying the redundant
    computation transformation with a fixed value for halo depth.

    '''
    psy, invoke = get_invoke("8_vector_field.f90", TEST_API, idx=0,
                             dist_mem=True)
    schedule = invoke.schedule
    rc_trans = LFRicRedundantComputationTrans()
    loop = schedule[5]
    rc_trans.apply(loop, {"depth": 3})
    result = str(psy.gen)

    assert LFRicBuild(tmpdir).code_compiles(psy)

    assert "if (f2_proxy%is_dirty(depth=3)) then" in result
    assert "call f2_proxy%halo_exchange(depth=3)" in result
    assert "loop0_stop = mesh%get_last_halo_cell(3)" in result
    for index in range(1, 4):
        assert f"call chi_proxy({index})%set_dirty()" in result
    for index in range(1, 4):
        assert f"call chi_proxy({index})%set_clean(2)" in result


def test_rc_vector_no_depth(tmpdir):
    ''' Test that the loop bounds for a (continuous) vector are modified
    appropriately and set_clean() added correctly and halo_exchange
    added/modified appropriately after applying the redundant
    computation transformation with no halo depth value.

    '''
    psy, invoke = get_invoke("8_vector_field.f90", TEST_API, idx=0,
                             dist_mem=True)
    schedule = invoke.schedule
    rc_trans = LFRicRedundantComputationTrans()
    loop = schedule[5]
    rc_trans.apply(loop)
    result = str(psy.gen)

    assert LFRicBuild(tmpdir).code_compiles(psy)

    assert "if (f2_proxy%is_dirty(depth=max_halo_depth_mesh)) then" in result
    assert "call f2_proxy%halo_exchange(depth=max_halo_depth_mesh)" in result
    assert "loop0_stop = mesh%get_last_halo_cell()" in result
    for idx in range(1, 4):
        assert f"call chi_proxy({idx})%set_dirty()" in result
    for idx in range(1, 4):
        assert (f"call chi_proxy({idx})%set_clean(max_halo_depth_mesh - 1)"
                in result)


def test_rc_no_halo_decrease():
    ''' Test that we do not decrease an existing halo size when setting it
    to a particular value. This situation may happen when the
    redundant computation affects the same field in two different
    loops and both depend on the same halo exchange.

    '''
    psy, invoke = get_invoke("15.1.1_builtin_and_normal_kernel_invoke_2.f90",
                             TEST_API, idx=0, dist_mem=True)
    schedule = invoke.schedule
    rc_trans = LFRicRedundantComputationTrans()
    # First, change the size of the f2 halo exchange to 3 by performing
    # redundant computation in the first loop
    loop = schedule.walk(Loop)[0]
    rc_trans.apply(loop, {"depth": 3})
    result = str(psy.gen)
    assert "if (f2_proxy%is_dirty(depth=3)) then" in result
    assert "if (m1_proxy%is_dirty(depth=3)) then" in result
    assert "if (m2_proxy%is_dirty(depth=3)) then" in result
    # Second, try to change the size of the f2 halo exchange to 2 by
    # performing redundant computation in the second loop
    schedule = invoke.schedule
    loop = schedule.walk(Loop)[1]
    rc_trans.apply(loop, {"depth": 2})
    result = str(psy.gen)
    assert "if (f2_proxy%is_dirty(depth=3)) then" in result
    assert "if (m1_proxy%is_dirty(depth=3)) then" in result
    assert "if (m2_proxy%is_dirty(depth=3)) then" in result
    # Third, set the size of the f2 halo exchange to the full halo
    # depth by performing redundant computation in the second loop
    schedule = invoke.schedule
    loop = schedule.walk(Loop)[1]
    rc_trans.apply(loop)
    result = str(psy.gen)
    assert "if (f2_proxy%is_dirty(depth=max_halo_depth_mesh)) then" in result
    assert "if (m1_proxy%is_dirty(depth=3)) then" in result
    assert "if (m2_proxy%is_dirty(depth=3)) then" in result
    # Fourth, try to change the size of the f2 halo exchange to 4 by
    # performing redundant computation in the first loop
    loop = schedule.walk(Loop)[0]
    rc_trans.apply(loop, {"depth": 4})
    result = str(psy.gen)
    assert "if (f2_proxy%is_dirty(depth=max_halo_depth_mesh)) then" in result
    assert "if (m1_proxy%is_dirty(depth=4)) then" in result
    assert "if (m2_proxy%is_dirty(depth=4)) then" in result


def test_rc_updated_dependence_analysis():
    ''' Test that the dependence analysis updates when new halo exchanges
    are added to the schedule. '''
    _, invoke = get_invoke("1_single_invoke_wtheta.f90", TEST_API, idx=0,
                           dist_mem=True)
    schedule = invoke.schedule
    loop = schedule.children[0]
    kernel = loop.loop_body[0]
    f2_field = kernel.args[1]
    assert not f2_field.backward_dependence()
    # set our loop to redundantly compute to the level 2 halo. This
    # introduces a new halo exchange
    rc_trans = LFRicRedundantComputationTrans()
    loop = schedule.children[0]
    rc_trans.apply(loop, {"depth": 2})
    previous_field = f2_field.backward_dependence()
    previous_node = previous_field.call
    # check f2_field has a backward dependence with the new halo
    # exchange field
    assert isinstance(previous_node, LFRicHaloExchange)
    # check the new halo exchange field has a forward dependence with
    # the kernel f2_field
    assert previous_field.forward_dependence() == f2_field


def test_rc_no_loop_decrease():
    ''' Test that we raise an exception if we try to reduce the size of a
    loop halo when using the redundant computation transformation. This is
    not allowed partly for simplicity but also because, in the current
    implementation we might not decrease the size of the relevant halo
    exchange as these can only be increased with the current logic. '''
    _, invoke = get_invoke("1_single_invoke_w2v.f90",
                           TEST_API, idx=0, dist_mem=True)
    schedule = invoke.schedule
    rc_trans = LFRicRedundantComputationTrans()
    # first set our loop to redundantly compute to the level 2 halo
    loop = schedule.children[0]
    rc_trans.apply(loop, {"depth": 2})
    # now try to reduce the redundant computation to the level 1 halo
    # f1 and f2 have read accesses (readwrite and read) so there
    # is one halo exchange for each before the loop
    loop = schedule.children[2]
    with pytest.raises(TransformationError) as excinfo:
        rc_trans.apply(loop, {"depth": 1})
    assert ("supplied depth (1) must be greater than the existing halo depth "
            "(2)") in str(excinfo.value)
    # second set our loop to redundantly compute to the maximum halo depth
    rc_trans.apply(loop)
    # now try to reduce the redundant computation to a fixed value
    with pytest.raises(TransformationError) as excinfo:
        rc_trans.apply(loop, {"depth": 2})
    assert ("loop is already set to the maximum halo depth so can't be "
            "set to a fixed value") in str(excinfo.value)
    # now try to set the redundant computation to the same (max) value
    # it is now
    with pytest.raises(TransformationError) as excinfo:
        rc_trans.apply(loop)
    assert ("loop is already set to the maximum halo depth so this "
            "transformation does nothing") in str(excinfo.value)


def test_rc_remove_halo_exchange(tmpdir, monkeypatch):
    '''Test that a halo exchange is removed if redundant computation means
    that it is no longer required. Halo exchanges are not required in
    this example when we compute annexed dofs. Therefore we ensure we
    compute over owned dofs (via monkeypatch) to perform the test.

    '''
    api_config = Config.get().api_conf(TEST_API)
    monkeypatch.setattr(api_config, "_compute_annexed_dofs", False)
    psy, _ = get_invoke("14.7_halo_annexed.f90",
                        TEST_API, idx=0, dist_mem=True)
    result = str(psy.gen)
    assert "call f1_proxy%halo_exchange(depth=1)" in result
    assert "call f2_proxy%halo_exchange(depth=1)" in result
    assert "if (m1_proxy%is_dirty(depth=1)) then" in result
    assert "call m1_proxy%halo_exchange(depth=1)" in result

    assert LFRicBuild(tmpdir).code_compiles(psy)

    #
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    #
    rc_trans = LFRicRedundantComputationTrans()
    loop = schedule.walk(Loop)[0]
    rc_trans.apply(loop, {"depth": 1})
    result = str(psy.gen)
    assert "call f1_proxy%halo_exchange(depth=1)" not in result
    assert "call f2_proxy%halo_exchange(depth=1)" in result
    assert "if (m1_proxy%is_dirty(depth=1)) then" in result
    assert "call m1_proxy%halo_exchange(depth=1)" in result
    #
    loop = schedule.walk(Loop)[1]
    rc_trans.apply(loop, {"depth": 1})
    result = str(psy.gen)
    assert "call f1_proxy%halo_exchange(depth=1)" not in result
    assert "call f2_proxy%halo_exchange(depth=1)" not in result
    assert "if (m1_proxy%is_dirty(depth=1)) then" in result
    assert "call m1_proxy%halo_exchange(depth=1)" in result


def test_rc_max_remove_halo_exchange(tmpdir):
    ''' Add test to redundantly compute a discontinuous (wtheta) and
    continuous (w2) field to the maximum halo depth and then check
    that a discontinuous halo exchange is removed in this case as we
    always remove the halo exchange if we write to a discontinuous
    field to maximum depth. Also check that the halo exchange is not
    removed for the continuous case as the outer halo stays dirty.
    The halo should also have an if round it as we do not know how
    much redundant computation we are doing.

    '''
    psy, invoke = get_invoke("15.1.2_builtin_and_normal_kernel_invoke.f90",
                             TEST_API, idx=0, dist_mem=True)
    schedule = invoke.schedule
    result = str(psy.gen)
    #
    # f3 has "inc" access so there is a check for the halo exchange
    # of depth 1
    assert "call f3_proxy%halo_exchange(depth=1)" in result
    assert "if (f3_proxy%is_dirty(depth=1)) then" in result
    rc_trans = LFRicRedundantComputationTrans()
    loop = schedule.walk(Loop)[0]
    rc_trans.apply(loop)
    result = str(psy.gen)

    # f3 halo exchange is not removed even though we redundantly
    # compute f3 as the redundant computation is on a continuous field
    # and therefore the outermost halo stays dirty. We can not be
    # certain whether the halo exchange is required or not as we don't
    # know the depth of the halo.
    assert "call f3_proxy%halo_exchange(depth=1)" in result
    # We do not know whether we need the halo exchange so we include an if
    assert "if (f3_proxy%is_dirty(depth=1)) then" in result
    #
    assert "call f4_proxy%halo_exchange(depth=1)" in result
    loop = schedule.walk(Loop)[-1]
    rc_trans.apply(loop)
    result = str(psy.gen)
    # f4 halo exchange is removed as it is redundantly computed to the
    # last level halo and is discontinuous so all levels of the halo
    # are clean. However, we introduce a new halo exchange for
    # f5. This could be removed by redundant computation but we don't
    # bother as that is not relevant to this test.
    assert "call f4_proxy%halo_exchange(depth=1)" not in result

    assert LFRicBuild(tmpdir).code_compiles(psy)


def test_rc_continuous_halo_remove(fortran_writer):
    ''' Check that we do not remove a halo exchange when the field is
    continuous and the redundant computation depth equals the required
    halo access depth. The reason for this is that the outer halo
    remains invalid when written to for a continuous field. Also check
    that we do remove the halo exchange when the redundant computation
    depth is one more than the required halo access depth.

    '''
    psy, invoke = get_invoke("15.1.2_builtin_and_normal_kernel_invoke.f90",
                             TEST_API, idx=0, dist_mem=True)
    # Have to ensure PSy-layer symbols have been generated as we don't call
    # psy.gen.
    invoke.setup_psy_layer_symbols()
    schedule = invoke.schedule
    result = fortran_writer(schedule)
    rc_trans = LFRicRedundantComputationTrans()
    hexches = schedule.walk(HaloExchange)
    loops = schedule.walk(Loop, stop_type=Loop)

    f3_inc_hex = hexches[0]
    f3_inc_loop = loops[2]
    f3_read_hex = hexches[3]
    f3_read_loop = loops[-1]
    # field "f3" has "inc" access resulting in two halo exchanges of
    # depth 1, one of which is conditional. One of these halo
    # exchanges is placed before the f3_inc_loop and one is placed
    # before the f3_read_loop (there are three other halo exchanges,
    # one each for fields f1, f2 and f4).
    assert result.count("call f3_proxy%halo_exchange(depth=1") == 2
    assert result.count("if (f3_proxy%is_dirty(depth=1)) then") == 1
    #
    # Applying redundant computation to equal depth on f3_inc_loop and
    # f3_read_loop does not remove the initial number of halo exchanges.
    # However, the "is_dirty" check and the halo exchange before the
    # f3_inc_loop are now to depth 2.
    rc_trans.apply(f3_read_loop, {"depth": 3})
    rc_trans.apply(f3_inc_loop, {"depth": 3})
    result = fortran_writer(schedule)
    assert result.count("call f3_proxy%halo_exchange(depth=") == 2
    assert f3_inc_hex._compute_halo_depth().value == "2"
    assert f3_read_hex._compute_halo_depth().value == "3"
    assert "if (f3_proxy%is_dirty(depth=2)) then" in result
    assert "if (f3_proxy%is_dirty(depth=3)) then" not in result
    # Applying redundant computation to one more depth to f3_inc_loop
    # removes the halo exchange before the f3_read_loop.
    # The "is_dirty" check and the halo exchange before the
    # f3_inc_loop are now to depth 3.
    rc_trans.apply(f3_inc_loop, {"depth": 4})
    result = fortran_writer(schedule)
    assert result.count("call f3_proxy%halo_exchange(depth=") == 1
    assert f3_inc_hex._compute_halo_depth().value == "3"
    # Last halo exchange is now on f4 instead of f3
    hexches = schedule.walk(HaloExchange)
    assert len(hexches) == 4
    hexches[3].field == "f4"
    assert "if (f3_proxy%is_dirty(depth=4)" not in result


def test_rc_discontinuous_halo_remove(monkeypatch, fortran_writer):
    ''' Check that we do remove a halo exchange when the field is
    discontinuous and the redundant computation depth equals the
    required halo access depth. Also check that we do not remove the
    halo exchange when the redundant computation depth is one less
    than the required halo access depth.

    '''
    psy, invoke = get_invoke("15.1.2_builtin_and_normal_kernel_invoke.f90",
                             TEST_API, idx=0, dist_mem=True)
    # Have to ensure PSy-layer symbols have been generated.
    invoke.setup_psy_layer_symbols()
    schedule = invoke.schedule
    result = fortran_writer(schedule)
    rc_trans = LFRicRedundantComputationTrans()
    loops = schedule.walk(Loop)
    f4_write_loop = loops[3]
    f4_read_loop = loops[4]
    assert "call f4_proxy%halo_exchange(depth=1)" in result
    assert "if (f4_proxy%is_dirty(depth=1)) then" not in result
    rc_trans.apply(f4_read_loop, {"depth": 3})
    rc_trans.apply(f4_write_loop, {"depth": 2})
    result = fortran_writer(schedule)
    assert "call f4_proxy%halo_exchange(depth=3)" in result
    assert "if (f4_proxy%is_dirty(depth=3)) then" not in result
    # Increase RC depth to 3 and check that halo exchange is removed
    # when a discontinuous field has write access
    rc_trans.apply(f4_write_loop, {"depth": 3})
    result = fortran_writer(schedule)
    assert "call f4_proxy%halo_exchange(depth=" not in result
    assert "if (f4_proxy%is_dirty(depth=" not in result
    # Increase RC depth to 3 and check that halo exchange is not removed
    # when a discontinuous field has readwrite access
    call = f4_write_loop.loop_body[0]
    f4_arg = call.arguments.args[0]
    monkeypatch.setattr(f4_arg, "_access", value=AccessType.READWRITE)
    monkeypatch.setattr(f4_write_loop, "_upper_bound_halo_depth", value=2)
    rc_trans.apply(f4_write_loop, {"depth": 3})
    result = fortran_writer(schedule)
    assert "call f4_proxy%halo_exchange(depth=" in result
    assert "if (f4_proxy%is_dirty(depth=" in result


def test_rc_reader_halo_remove(fortran_writer):
    ''' Check that we do not add an unnecessary halo exchange when we
    increase the depth of halo that a loop computes but the previous loop
    still computes deep enough into the halo to avoid needing a halo
    exchange.

    '''
    psy, invoke = get_invoke("15.1.2_builtin_and_normal_kernel_invoke.f90",
                             TEST_API, idx=0, dist_mem=True)
    # Have to ensure PSy-layer symbols have been generated.
    invoke.setup_psy_layer_symbols()
    schedule = invoke.schedule
    result = fortran_writer(schedule)
    assert "call f2_proxy%halo_exchange(depth=1)" in result

    loops = schedule.walk(Loop)
    rc_trans = LFRicRedundantComputationTrans()

    # Redundant computation to avoid halo exchange for f2
    rc_trans.apply(loops[1], {"depth": 2})
    result = fortran_writer(schedule)
    assert "call f2_proxy%halo_exchange(" not in result

    # Redundant computation to depth 2 in f2 reader loop should not
    # cause a new halo exchange as it is still covered by depth=2 in
    # the writer loop
    rc_trans.apply(loops[2], {"depth": 2})
    result = fortran_writer(schedule)
    assert "call f2_proxy%halo_exchange(" not in result


def test_rc_vector_reader_halo_remove(fortran_writer):
    ''' Check that we do not add unnecessary halo exchanges for a vector
    field when we increase the depth of halo that a loop computes but
    the previous loop still computes deep enough into the halo to
    avoid needing halo exchanges. '''
    psy, invoke = get_invoke("8.2.1_multikernel_invokes_w3_vector.f90",
                             TEST_API, idx=0, dist_mem=True)
    # Have to ensure PSy-layer symbols have been generated.
    invoke.setup_psy_layer_symbols()
    schedule = invoke.schedule
    result = fortran_writer(schedule)

    assert "is_dirty" not in result
    assert "halo_exchange" not in result

    loops = schedule.walk(Loop)
    rc_trans = LFRicRedundantComputationTrans()

    # Redundant computation for first loop
    rc_trans.apply(loops[0], {"depth": 1})
    result = fortran_writer(schedule)
    assert result.count("is_dirty") == 3
    assert result.count("halo_exchange") == 3

    # Redundant computation in reader loop should not
    # cause a new halo exchange as it is still covered by depth=1 in
    # the writer loop
    rc_trans.apply(loops[1], {"depth": 1})
    result = fortran_writer(schedule)
    assert result.count("is_dirty") == 3
    assert result.count("halo_exchange") == 3


def test_rc_vector_reader_halo_readwrite(fortran_writer):
    ''' When we increase the depth of halo that a loop computes but the
    previous loop still computes deep enough into the halo the added
    halo exchanges stem from the vector readwrite access. '''
    psy, invoke = get_invoke("8.2.2_multikernel_invokes_wtheta_vector.f90",
                             TEST_API, idx=0, dist_mem=True)
    # Have to ensure PSy-layer symbols have been generated as we don't call
    # psy.gen.
    invoke.setup_psy_layer_symbols()
    schedule = invoke.schedule
    result = fortran_writer(schedule)

    assert "is_dirty" not in result
    assert "halo_exchange" not in result

    rc_trans = LFRicRedundantComputationTrans()

    loops = schedule.walk(Loop, stop_type=Loop)

    # Redundant computation for first loop: both fields have
    # read dependencies for all three components
    rc_trans.apply(loops[0], {"depth": 1})
    result = fortran_writer(schedule)
    assert result.count("is_dirty") == 6
    assert result.count("halo_exchange") == 6

    # Redundant computation in reader loop causes new halo exchanges
    # due to readwrite dependency in f3
    rc_trans.apply(loops[1], {"depth": 1})
    result = fortran_writer(schedule)
    assert result.count("is_dirty") == 9
    assert result.count("halo_exchange") == 9

    # Now increase RC depth of the reader loop to 2 to check for
    # additional halo exchanges (3 more due to readwrite to read
    # dependency in f1)
    rc_trans.apply(loops[1], {"depth": 2})
    result = fortran_writer(schedule)
    # Check for additional halo exchanges
    hexches = schedule.walk(HaloExchange)
    assert len(hexches) == 12
    assert result.count("halo_exchange") == 12
    # Check that additional halo exchanges for all three f1
    # vector field components are of depth 2 and that they
    # do not have if tests around them
    for idvct in range(1, 4):
        idx = str(idvct)
        assert (
            "call f1_proxy(" + idx + ")%halo_exchange(depth=2)") in result
        assert (
            "      if (f1_proxy(" + idx + ")%is_dirty(depth=2)) then\n"
            "         call f1_proxy(" + idx + ")%halo_exchange(depth=2)\n"
            "      end if\n") not in result


def test_stencil_rc_max_depth_1(monkeypatch):
    ''' If a loop contains a kernel with a stencil access and the loop
    attempts to compute redundantly into the halo to the maximum depth
    then the stencil will access beyond the halo bounds. This is
    therefore not allowed and exceptions are raised in the
    LFRicRedundantComputationTrans transformation and in
    _compute_single_halo_info. This test checks these exceptions are
    raised correctly.

    '''
    _, invoke = get_invoke("19.1_single_stencil.f90",
                           TEST_API, idx=0, dist_mem=True)
    schedule = invoke.schedule
    loop = schedule[4]
    rc_trans = LFRicRedundantComputationTrans()
    with pytest.raises(TransformationError) as excinfo:
        rc_trans.apply(loop)
    assert ("In the LFRicRedundantComputation transformation apply method "
            "the loop contains field 'f2' with a stencil access in kernel "
            "'testkern_stencil_code', so it is invalid to set redundant "
            "computation to maximum depth" in str(excinfo.value))

    halo_exchange = schedule[1]
    monkeypatch.setattr(loop, "_upper_bound_halo_depth", None)
    with pytest.raises(GenerationError) as excinfo:
        _ = halo_exchange._compute_halo_read_info()
    assert ("redundant computation to max depth with a stencil is "
            "invalid" in str(excinfo.value))


def test_rc_invalid_depth_type():
    ''' If an incorrect type is passed as a depth value to the redundant
    computation transformation an exception should be raised. This test
    checks that this exception is raised as expected.

    '''
    _, invoke = get_invoke("1_single_invoke.f90",
                           TEST_API, idx=0, dist_mem=True)
    schedule = invoke.schedule
    loop = schedule.children[4]
    rc_trans = LFRicRedundantComputationTrans()
    with pytest.raises(TransformationError) as excinfo:
        rc_trans.apply(loop, {"depth": "2"})
    assert (f"the supplied depth should be an integer but found "
            f"type '{type('txt')}'" in str(excinfo.value))
