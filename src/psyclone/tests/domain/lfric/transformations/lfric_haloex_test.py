# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2018-2024, Science and Technology Facilities Council.
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
# Authors: R. W. Ford, A. R. Porter and S. Siso, STFC Daresbury Lab
# Modified: O. Brunt and L. Turner, Met Office

'''This module tests the LFRic API-specific halo exchange
   implementation. '''

import os
import pytest

from psyclone.configuration import Config
from psyclone.core import AccessType
from psyclone.domain.lfric import LFRicLoop
from psyclone.dynamo0p3 import (LFRicHaloExchange, HaloDepth,
                                _create_depth_list)
from psyclone.errors import InternalError
from psyclone.parse.algorithm import parse
from psyclone.psyGen import PSyFactory, GenerationError
from psyclone.tests.lfric_build import LFRicBuild
from psyclone.transformations import (Dynamo0p3RedundantComputationTrans,
                                      Dynamo0p3AsyncHaloExchangeTrans)


# constants
API = "dynamo0.3"
BASE_PATH = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                         os.pardir, os.pardir, os.pardir,
                         "test_files", "dynamo0p3")


@pytest.fixture(scope="module", autouse=True)
def setup():
    '''Make sure that all tests here use dynamo0.3 as API.'''
    Config.get().api = "dynamo0.3"
    yield
    Config._instance = None


def test_gh_inc_nohex_1(tmpdir, monkeypatch):
    '''If COMPUTE_ANNEXED_DOFS is True, then a gh_inc access to a field in
    a kernel (iterating to the l1 halo) does not require a halo
    exchange when the previous writer is known and iterates over dofs
    to nannexed, halo(1), or halo max depth

    '''
    # ensure that COMPUTE_ANNEXED_DOFS is True
    config = Config.get()
    dyn_config = config.api_conf(API)
    monkeypatch.setattr(dyn_config, "_compute_annexed_dofs", True)

    # parse and get psy schedule
    _, info = parse(os.path.join(BASE_PATH,
                                 "14.12_halo_wdofs_to_inc.f90"),
                    api=API)
    psy = PSyFactory(API, distributed_memory=True).create(info)
    schedule = psy.invokes.invoke_list[0].schedule

    def check_schedule(schedule):
        '''Check this schedule has expected structure (loop, haloexchange,
        loop). In paricular there should be no halo exchange for the
        write-to-gh_inc dependence.

        :param schedule: a dynamo0.3 API schedule object
        :type schedule: :py:class:`psyclone.dynamo0p3.DynInvokeSchedule`.

        '''
        assert len(schedule.children) == 3
        loop1 = schedule.children[0]
        haloex = schedule.children[1]
        loop2 = schedule.children[2]
        assert isinstance(loop1, LFRicLoop)
        assert isinstance(haloex, LFRicHaloExchange)
        assert haloex.field.name == "f2"
        assert haloex.required() == (True, False)
        assert isinstance(loop2, LFRicLoop)

    # 1st loop should iterate over dofs to nannexed. Check output
    assert schedule.children[0].upper_bound_name == "nannexed"
    check_schedule(schedule)

    # just check compilation here (not later in this test) as
    # compilation of redundant computation is checked separately
    assert LFRicBuild(tmpdir).code_compiles(psy)

    # make 1st loop iterate over dofs to the level 1 halo and check output
    rc_trans = Dynamo0p3RedundantComputationTrans()
    rc_trans.apply(schedule.children[0], {"depth": 1})
    assert schedule.children[0].upper_bound_name == "dof_halo"
    assert schedule.children[0].upper_bound_halo_depth == 1
    check_schedule(schedule)

    # make 1st loop iterate over dofs to the maximum halo depth and
    # check output
    rc_trans.apply(schedule.children[0])
    assert schedule.children[0].upper_bound_name == "dof_halo"
    assert not schedule.children[0].upper_bound_halo_depth
    check_schedule(schedule)


def test_gh_inc_nohex_2(tmpdir, monkeypatch):
    '''If COMPUTE_ANNEXED_DOFS is False, then a gh_inc access to a field in
    a kernel (iterating to the l1 halo) does require a halo
    exchange when the previous writer is known and iterates over dofs
    to ndofs but does not if it iterates to halo(1), or halo max depth

    '''
    # ensure that COMPUTE_ANNEXED_DOFS is False
    config = Config.get()
    dyn_config = config.api_conf(API)
    monkeypatch.setattr(dyn_config, "_compute_annexed_dofs", False)

    # parse and get psy schedule
    _, info = parse(os.path.join(BASE_PATH,
                                 "14.12_halo_wdofs_to_inc.f90"),
                    api=API)
    psy = PSyFactory(API, distributed_memory=True).create(info)
    schedule = psy.invokes.invoke_list[0].schedule

    # 1st loop should iterate over dofs to ndofs. Check output
    loop1 = schedule.children[0]
    haloex1 = schedule.children[1]
    haloex2 = schedule.children[2]
    loop2 = schedule.children[3]
    assert len(schedule.children) == 4
    assert isinstance(loop1, LFRicLoop)
    assert loop1.upper_bound_name == "ndofs"
    assert isinstance(haloex1, LFRicHaloExchange)
    assert haloex1.field.name == "f1"
    assert haloex1.required() == (True, True)
    assert isinstance(haloex2, LFRicHaloExchange)
    assert haloex2.field.name == "f2"
    assert haloex2.required() == (True, False)
    assert isinstance(loop2, LFRicLoop)

    # just check compilation here (not later in this test) as
    # compilation of redundant computation is checked separately
    assert LFRicBuild(tmpdir).code_compiles(psy)

    # make 1st loop iterate over dofs to the level 1 halo and check
    # output. There should be no halo exchange for field "f1"
    rc_trans = Dynamo0p3RedundantComputationTrans()
    rc_trans.apply(schedule.children[0], {"depth": 1})
    loop1 = schedule.children[0]
    haloex = schedule.children[1]
    loop2 = schedule.children[2]
    assert len(schedule.children) == 3
    assert isinstance(loop1, LFRicLoop)
    assert loop1.upper_bound_name == "dof_halo"
    assert loop1.upper_bound_halo_depth == 1
    assert isinstance(haloex, LFRicHaloExchange)
    assert haloex.field.name == "f2"
    assert haloex.required() == (True, False)
    assert isinstance(loop2, LFRicLoop)

    # make 1st loop iterate over dofs to the maximum halo depth and
    # check output
    rc_trans.apply(schedule.children[0])
    loop1 = schedule.children[0]
    haloex = schedule.children[1]
    loop2 = schedule.children[2]
    assert len(schedule.children) == 3
    assert isinstance(loop1, LFRicLoop)
    assert loop1.upper_bound_name == "dof_halo"
    assert not loop1.upper_bound_halo_depth
    assert isinstance(haloex, LFRicHaloExchange)
    assert haloex.field.name == "f2"
    assert haloex.required() == (True, False)
    assert isinstance(loop2, LFRicLoop)


def test_gh_inc_nohex_3(tmpdir, monkeypatch):
    '''If COMPUTE_ANNEXED_DOFS is True, then a gh_inc access to a field in
    a kernel (iterating to the l1 halo) does not require a halo
    exchange when the previous writer is known and iterates over cells
    to halo(1), halo(2) and halo max depth. Also, if the previous
    writer is a gh_inc access and its previous writer is unknown then
    it does not require a halo exchange if it writes to halo(1), but
    requires a speculative halo exchange to halo(n-1) if iterating to
    halo(n) and a speculative halo exchange to halo(max_depth-1) if
    iterating to the maximum halo depth

    '''
    # ensure that COMPUTE_ANNEXED_DOFS is True
    config = Config.get()
    dyn_config = config.api_conf(API)
    monkeypatch.setattr(dyn_config, "_compute_annexed_dofs", True)

    # parse and get psy schedule
    _, info = parse(os.path.join(BASE_PATH,
                                 "14.13_halo_inc_to_inc.f90"),
                    api=API)
    psy = PSyFactory(API, distributed_memory=True).create(info)
    schedule = psy.invokes.invoke_list[0].schedule

    # check we have no halo exchanges for field "f1"
    assert len(schedule.children) == 3
    haloex = schedule.children[0]
    loop1 = schedule.children[1]
    loop2 = schedule.children[2]
    assert isinstance(haloex, LFRicHaloExchange)
    assert haloex.field.name == "f2"
    assert haloex.required() == (True, False)
    assert haloex._compute_halo_depth() == "1"
    assert isinstance(loop1, LFRicLoop)
    assert isinstance(loop2, LFRicLoop)

    # just check compilation here (not later in this test) as
    # compilation of redundant computation is checked separately
    assert LFRicBuild(tmpdir).code_compiles(psy)

    # make 1st loop iterate over cells to the level 2 halo and check output
    rc_trans = Dynamo0p3RedundantComputationTrans()
    rc_trans.apply(schedule.children[1], {"depth": 2})

    def check(schedule, f1depth, f2depth):
        '''check that the schedule is modified in the expected way. In
        particular, check that the depth of the halo exchange for
        field 'f1' is what we are expecting

        :param schedule: a dynamo0.3 API schedule object
        :type schedule: :py:class:`psyclone.dynamo0p3.DynInvokeSchedule`.
        :param int f1depth: The expected depth of the halo exchange \
        associated with field f1
        :param int f2depth: The expected depth of the halo exchange \
        associated with field f2

        '''
        assert len(schedule.children) == 4
        haloex1 = schedule.children[0]
        haloex2 = schedule.children[1]
        loop1 = schedule.children[2]
        loop2 = schedule.children[3]
        assert isinstance(haloex1, LFRicHaloExchange)
        assert haloex1.field.name == "f2"
        assert haloex1._compute_halo_depth() == f2depth
        assert haloex1.required() == (True, False)
        assert isinstance(haloex2, LFRicHaloExchange)
        assert haloex2.field.name == "f1"
        assert haloex2._compute_halo_depth() == f1depth
        assert haloex2.required() == (True, False)
        assert isinstance(loop1, LFRicLoop)
        assert isinstance(loop2, LFRicLoop)

    # we should now have a speculative halo exchange at the start of
    # the schedule for "f1" to depth 1 and "f2" to depth 2
    check(schedule, f1depth="1", f2depth="2")

    # make 1st loop iterate over cells to the maximum halo depth and
    # check output
    rc_trans.apply(schedule.children[2])
    # we should now have a speculative halo exchange at the start of
    # the schedule for "f1" to depth max halo - 1 and "f2" to max halo
    check(schedule, f1depth="max_halo_depth_mesh-1",
          f2depth="max_halo_depth_mesh")


def test_gh_inc_nohex_4(tmpdir, monkeypatch):
    '''If COMPUTE_ANNEXED_DOFS is False, then a gh_inc access to a field
    in a kernel (iterating to the l1 halo) does not require a halo
    exchange when the previous writer is known and iterates over cells
    to halo(1), halo(2) and halo max depth. Also, if the previous
    writer is a gh_inc access and its previous writer is unknown then
    it does require a halo exchange if it writes to halo(1) and
    requires a speculative halo exchange to halo(n-1) if iterating to
    halo(n) and a speculative halo exchange to halo(max_depth-1) if
    iterating to the maximum halo depth

    '''
    # ensure that COMPUTE_ANNEXED_DOFS is False
    config = Config.get()
    dyn_config = config.api_conf(API)
    monkeypatch.setattr(dyn_config, "_compute_annexed_dofs", False)

    # parse and get psy schedule
    _, info = parse(os.path.join(BASE_PATH,
                                 "14.13_halo_inc_to_inc.f90"),
                    api=API)
    psy = PSyFactory(API, distributed_memory=True).create(info)
    schedule = psy.invokes.invoke_list[0].schedule

    def check(schedule, f1depth, f2depth):
        '''check that the schedule is modified in the expected way. In
        particular, check that the depth of the halo exchange for
        field 'f1' is what we are expecting

        :param schedule: a dynamo0.3 API schedule object
        :type schedule: :py:class:`psyclone.dynamo0p3.DynInvokeSchedule`.
        :param int f1depth: The expected depth of the halo exchange \
        associated with field f1
        :param int f2depth: The expected depth of the halo exchange \
        associated with field f2

        '''
        assert len(schedule.children) == 4
        haloex1 = schedule.children[0]
        haloex2 = schedule.children[1]
        loop1 = schedule.children[2]
        loop2 = schedule.children[3]
        assert isinstance(haloex1, LFRicHaloExchange)
        assert haloex1.field.name == "f1"
        assert haloex1._compute_halo_depth() == f1depth
        assert haloex1.required() == (True, False)
        assert isinstance(haloex2, LFRicHaloExchange)
        assert haloex2.field.name == "f2"
        assert haloex2._compute_halo_depth() == f2depth
        assert haloex2.required() == (True, False)
        assert isinstance(loop1, LFRicLoop)
        assert isinstance(loop2, LFRicLoop)

    # we should now have a speculative halo exchange at the start of
    # the schedule for "f1" to depth 1 and "f2" to depth 1
    check(schedule, f1depth="1", f2depth="1")

    # just check compilation here (not later in this test) as
    # compilation of redundant computation is checked separately
    assert LFRicBuild(tmpdir).code_compiles(psy)

    # make 1st loop iterate over cells to the level 2 halo and check output
    rc_trans = Dynamo0p3RedundantComputationTrans()
    rc_trans.apply(schedule.children[2], {"depth": 2})
    # we should now have a speculative halo exchange at the start of
    # the schedule for "f1" to depth 1 and "f2" to depth 2
    check(schedule, f1depth="1", f2depth="2")

    # make 1st loop iterate over cells to the maximum halo depth and
    # check output
    rc_trans.apply(schedule.children[2])
    # we should now have a speculative halo exchange at the start of
    # the schedule for "f1" to depth max halo - 1 and "f2" to max halo
    check(schedule, f1depth="max_halo_depth_mesh-1",
          f2depth="max_halo_depth_mesh")


def test_gh_inc_max(tmpdir, monkeypatch, annexed):
    '''Check we generate correct halo exchange bounds when we have
    multiple read dependencies. In this case we have a gh_inc with a
    read-only reader and a gh_inc reader. We also test when annexed
    is False and True as it affects how many halo exchanges are
    generated.

    '''
    config = Config.get()
    dyn_config = config.api_conf(API)
    monkeypatch.setattr(dyn_config, "_compute_annexed_dofs", annexed)

    # parse and get psy schedule
    _, info = parse(os.path.join(BASE_PATH,
                                 "14.14_halo_inc_times3.f90"),
                    api=API)
    psy = PSyFactory(API, distributed_memory=True).create(info)
    schedule = psy.invokes.invoke_list[0].schedule
    rc_trans = Dynamo0p3RedundantComputationTrans()

    def check(haloex, depth):
        '''check the halo exchange has the expected properties

        :param haloex: a dynamo0.3 API halo-exchange object
        :type haloex: :py:class:`psyclone.dynamo0p3.LFRicHaloExchange`.
        :param int depth: The expected depth of the halo exchange \
        passed in as the first argument

        '''

        assert isinstance(haloex, LFRicHaloExchange)
        assert haloex.field.name == "f1"
        assert haloex.required() == (True, True)
        assert haloex._compute_halo_depth() == depth
    if annexed:
        haloidx = 2
        loop1idx = 3
        loop2idx = 5
    else:
        haloidx = 4
        loop1idx = 5
        loop2idx = 7

    # f1 halo exchange should be depth 1 : max(1,0)
    haloex = schedule.children[haloidx]
    check(haloex, "1")
    rc_trans.apply(schedule.children[loop2idx], {"depth": 2})
    # f1 halo exchange should still be depth 1 : max(1,1)
    haloex = schedule.children[haloidx]
    check(haloex, "1")
    rc_trans.apply(schedule.children[loop2idx], {"depth": 3})
    # f1 halo exchange should be depth 2 (max(1,2)
    haloex = schedule.children[haloidx]
    check(haloex, "2")
    rc_trans.apply(schedule.children[loop2idx])
    # f1 halo exchange should be depth max(1,max-1)
    haloex = schedule.children[haloidx]
    check(haloex, "max(max_halo_depth_mesh-1,1)")
    # just check compilation here as it is the most
    # complicated. (Note, compilation of redundant computation is
    # checked separately)
    assert LFRicBuild(tmpdir).code_compiles(psy)
    rc_trans.apply(schedule.children[loop1idx])
    # f1 halo exchange should be depth max
    haloex = schedule.children[haloidx]
    check(haloex, "max_halo_depth_mesh")


def test_write_cont_dirty(tmpdir, monkeypatch, annexed):
    ''' Check that no halo-exchange call is added before a
    kernel that has a field on any space with a 'GH_WRITE' access. '''
    config = Config.get()
    dyn_config = config.api_conf(API)
    monkeypatch.setattr(dyn_config, "_compute_annexed_dofs", annexed)
    _, invoke_info = parse(os.path.join(
        BASE_PATH, "14.1.1_halo_cont_write.f90"), api=API)
    psy = PSyFactory(API, distributed_memory=True).create(invoke_info)
    schedule = psy.invokes.invoke_list[0].schedule
    # We should have no halo exchange since this kernel is a special case
    # and does not read from annexed dofs.
    hexchs = schedule.walk(LFRicHaloExchange)
    assert len(hexchs) == 0
    # The field that is written to should be marked as dirty.
    code = str(psy.gen)
    assert "CALL f1_proxy%set_dirty()\n" in code

    assert LFRicBuild(tmpdir).code_compiles(psy)


def test_setval_x_then_user(tmpdir, monkeypatch):
    ''' Check that the correct halo exchanges are added if redundant
    computation is enabled for a built-in kernel called before a
    user-supplied kernel. '''
    api_config = Config.get().api_conf(API)
    monkeypatch.setattr(api_config, "_compute_annexed_dofs", True)
    _, invoke_info = parse(os.path.join(
        BASE_PATH, "15.7.3_setval_X_before_user_kern.f90"), api=API)
    psy = PSyFactory(API, distributed_memory=True).create(invoke_info)

    first_invoke = psy.invokes.invoke_list[0]
    # Since (redundant) computation over annexed dofs is enabled, there
    # should be no halo exchange before the first (builtin) kernel call
    assert isinstance(first_invoke.schedule[0], LFRicLoop)
    # There should be a halo exchange for field f1 before the second
    # kernel call
    assert isinstance(first_invoke.schedule[1], LFRicHaloExchange)
    assert first_invoke.schedule[1].field.name == "f1"
    # Now transform the first loop to perform redundant computation out to
    # the level-1 halo
    rtrans = Dynamo0p3RedundantComputationTrans()
    rtrans.apply(first_invoke.schedule[0], options={"depth": 1})
    # There should now be a halo exchange for f1 before the first
    # (builtin) kernel call
    assert isinstance(first_invoke.schedule[0], LFRicHaloExchange)
    assert first_invoke.schedule[0].field.name == "f1"
    assert isinstance(first_invoke.schedule[1], LFRicLoop)
    # There should only be one halo exchange for field f1
    assert len([node for node in first_invoke.schedule.walk(LFRicHaloExchange)
                if node.field.name == "f1"]) == 1
    assert LFRicBuild(tmpdir).code_compiles(psy)


# Tests for LFRicHaloExchange
# Tests for _compute_halo_read_info() within LFRicHaloExchange
def test_compute_halo_read_info_read_dep(monkeypatch):
    '''Check that _compute_halo_read_info() in LFRicHaloExchange raises the
    expected exception when there is more than one read dependence
    associated with a halo exchange in the read dependence list. This
    should never happen as the field access for a halo exchange is
    readwrite, therefore the first access will stop any further
    accesses (due to the write). Also check that the expected
    exception is raised when there is a read dependence associated
    with a halo exchange which is not the last entry in the
    list. Again this should never happen as the field access for a
    halo exchange is readwrite.

    '''
    api_config = Config.get().api_conf(API)
    monkeypatch.setattr(api_config, "_compute_annexed_dofs", True)
    _, invoke_info = parse(os.path.join(
        BASE_PATH, "15.7.3_setval_X_before_user_kern.f90"), api=API)
    psy = PSyFactory(API, distributed_memory=True).create(invoke_info)

    schedule = psy.invokes.invoke_list[0].schedule

    hex_f1 = schedule[1]
    # Modify the adjacent halo exchange to reference field f1 and
    # make the access read-only. This stops the dependence from being
    # the last on the list.
    schedule[2].field._name = "f1"
    schedule[2].field.access = AccessType.READ
    with pytest.raises(InternalError) as info:
        hex_f1._compute_halo_read_info(ignore_hex_dep=True)
    assert ("If there is a read dependency associated with a halo exchange "
            "in the list of read dependencies then it should be the last "
            "one in the list." in str(info.value))

    # Now modify a 3rd halo exchange to reference field f1 making more
    # than one dependency associated with a halo exchange.
    schedule[3].field._name = "f1"
    with pytest.raises(InternalError) as info:
        hex_f1._compute_halo_read_info(ignore_hex_dep=True)
    assert ("There should only ever be at most one read dependency associated "
            "with a halo exchange in the read dependency list, but found 2 "
            "for field f1." in str(info.value))


def test_compute_halo_read_info_async(monkeypatch):
    '''Check that _compute_halo_read_info() in LFRicHaloExchange raises the
    expected exception when there is a read dependence associated with
    an asynchronous halo exchange in the read dependence list.

    '''
    api_config = Config.get().api_conf(API)
    monkeypatch.setattr(api_config, "_compute_annexed_dofs", True)
    _, invoke_info = parse(os.path.join(
        BASE_PATH, "15.7.3_setval_X_before_user_kern.f90"), api=API)
    psy = PSyFactory(API, distributed_memory=True).create(invoke_info)

    schedule = psy.invokes.invoke_list[0].schedule

    hex_f1 = schedule[1]

    schedule[2].field._name = "f1"
    async_hex = Dynamo0p3AsyncHaloExchangeTrans()
    async_hex.apply(schedule[2])
    with pytest.raises(GenerationError) as info:
        hex_f1._compute_halo_read_info(ignore_hex_dep=True)
    assert ("Please perform redundant computation transformations "
            "before asynchronous halo exchange transformations."
            in str(info.value))


# Tests for LFRicLoop
# Tests for _add_field_component_halo_exchange() within LFRicLoop
def test_add_halo_exchange_code_nreader(monkeypatch):
    '''Check that _add_field_component_halo_exchange() in LFRicLoop raises
    the expected exception when there is more than one read dependence
    associated with a halo exchange in the read dependence list.

    '''
    api_config = Config.get().api_conf(API)
    monkeypatch.setattr(api_config, "_compute_annexed_dofs", True)
    _, invoke_info = parse(os.path.join(
        BASE_PATH, "15.7.3_setval_X_before_user_kern.f90"), api=API)
    psy = PSyFactory(API, distributed_memory=True).create(invoke_info)

    schedule = psy.invokes.invoke_list[0].schedule
    loop = schedule[0]
    rtrans = Dynamo0p3RedundantComputationTrans()
    rtrans.apply(loop, options={"depth": 1})
    f1_field = schedule[0].field
    del schedule.children[0]
    schedule[1].field._name = "f1"
    schedule[2].field._name = "f1"
    with pytest.raises(InternalError) as info:
        loop._add_field_component_halo_exchange(f1_field)
    assert ("When replacing a halo exchange with another one for field f1, "
            "a subsequent dependent halo exchange was found. This should "
            "never happen." in str(info.value))


def test_gh_readinc(tmpdir):
    '''Test that the GH_READINC access requires a halo exchange before the
    loop is executed if its level 1 halo is dirty (and it is in a
    standard loop that iterates to the level1 halo). This is in
    contrast to GH_INC which does not require a halo exchange in this
    case.

    '''
    # Parse and get psy schedule.
    _, info = parse(os.path.join(BASE_PATH,
                                 "14.15_halo_readinc.f90"),
                    api=API)
    psy = PSyFactory(API, distributed_memory=True).create(info)
    schedule = psy.invokes.invoke_list[0].schedule

    # Check that a halo exchange is added before a GH_READINC access
    # and after a GH_INC access to a field (f1).
    # Also check that 'check_dirty == False' and 'depth == 1' in the
    # halo exchange.
    f1_hex = schedule[3]
    assert isinstance(f1_hex, LFRicHaloExchange)
    assert f1_hex.field.name == "f1"
    _, known = f1_hex.required()
    check_dirty = not known
    assert not check_dirty
    assert f1_hex._compute_halo_depth() == '1'

    assert LFRicBuild(tmpdir).code_compiles(psy)


def test_stencil_then_w3_read(tmpdir):
    '''Test that a stencil access to a discontinuous field followed by a
    'read' (or 'readwrite)' in a subsequent kernel to the same field
    results in the expected halo exchange extents. There used to be an
    error resulting in incorrect code being produced here. Now we
    expect the value of depth to be 'extent', as the subsequent 'read'
    is to owned dofs so does not access the halo (a halo depth of 0).

    '''
    # Check that an instance of the HaloDepth class returns the
    # expected results for this case.
    halo_depth = HaloDepth(None)
    assert str(halo_depth) == "0"
    halo_depth2 = HaloDepth(None)
    halo_depth2._var_depth = "extent"
    assert str(halo_depth2) == "extent"
    # Quick check when we have both depth and literal depth > 0.
    halo_depth2.literal_depth = 1
    assert str(halo_depth2) == "extent+1"
    # Go back to original 0 depth case.
    halo_depth2.literal_depth = 0

    # Check that '_create_depth_list' removes depths that are 0 from
    # its return list. It takes two entries as input and returns one.
    result = _create_depth_list([halo_depth, halo_depth2], None)
    assert isinstance(result, list)
    assert len(result) == 1
    assert isinstance(result[0], HaloDepth)
    assert str(result[0]) == "extent"

    # Check that it all works in practice (functional test).
    _, info = parse(os.path.join(BASE_PATH,
                                 "14.16_disc_stencil_then_read.f90"),
                    api=API)
    psy = PSyFactory(API, distributed_memory=True).create(info)
    schedule = psy.invokes.invoke_list[0].schedule
    f4_hex = schedule.children[1]
    assert isinstance(f4_hex, LFRicHaloExchange)
    assert f4_hex.field.name == "f4"

    result = str(psy.gen)
    assert ("      IF (f4_proxy%is_dirty(depth=extent)) THEN\n"
            "        CALL f4_proxy%halo_exchange(depth=extent)\n"
            "      END IF" in result)

    assert LFRicBuild(tmpdir).code_compiles(psy)
