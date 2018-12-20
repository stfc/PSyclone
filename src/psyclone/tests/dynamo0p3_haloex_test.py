# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2018, Science and Technology Facilities Council
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
# Authors R. W. Ford and A. R. Porter, STFC Daresbury Lab

'''This module tests the Dynamo 0.3 API-specific halo exchange
implementation for gh_inc dependencies using pytest. '''

from __future__ import absolute_import
import os
from psyclone.parse import parse
from psyclone.psyGen import PSyFactory
from psyclone.dynamo0p3 import DynLoop, DynHaloExchange
from psyclone.transformations import Dynamo0p3RedundantComputationTrans
from psyclone.configuration import Config
import psyclone_test_utils as utils

# constants
API = "dynamo0.3"
BASE_PATH = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                         "test_files", "dynamo0p3")


def test_gh_inc_nohex_1(tmpdir, f90, f90flags, monkeypatch):
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
    psy = PSyFactory(API).create(info)
    schedule = psy.invokes.invoke_list[0].schedule

    def check_schedule(schedule):
        '''Check this schedule has expected structure (loop, haloexchange,
        loop). In paricular there should be no halo exchange for the
        write-to-gh_inc dependence.

        :param schedule: a dynamo0.3 API schedule object
        :type schedule: :py:class:`psyclone.dynamo0p3.DynSchedule`.

        '''
        assert len(schedule.children) == 3
        loop1 = schedule.children[0]
        haloex = schedule.children[1]
        loop2 = schedule.children[2]
        assert isinstance(loop1, DynLoop)
        assert isinstance(haloex, DynHaloExchange)
        assert haloex.field.name == "f2"
        assert haloex.required() == (True, False)
        assert isinstance(loop2, DynLoop)

    # 1st loop should iterate over dofs to nannexed. Check output
    assert schedule.children[0].upper_bound_name == "nannexed"
    check_schedule(schedule)

    # just check compilation here (not later in this test) as
    # compilation of redundant computation is checked separately
    if utils.TEST_COMPILE:
        # If compilation testing has been enabled
        # (--compile --f90="<compiler_name>" flags to py.test)
        assert utils.code_compiles(API, psy, tmpdir, f90, f90flags)

    # make 1st loop iterate over dofs to the level 1 halo and check output
    rc_trans = Dynamo0p3RedundantComputationTrans()
    rc_trans.apply(schedule.children[0], depth=1)
    assert schedule.children[0].upper_bound_name == "dof_halo"
    assert schedule.children[0].upper_bound_halo_depth == 1
    check_schedule(schedule)

    # make 1st loop iterate over dofs to the maximum halo depth and
    # check output
    rc_trans.apply(schedule.children[0])
    assert schedule.children[0].upper_bound_name == "dof_halo"
    assert not schedule.children[0].upper_bound_halo_depth
    check_schedule(schedule)


def test_gh_inc_nohex_2(tmpdir, f90, f90flags, monkeypatch):
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
    psy = PSyFactory(API).create(info)
    schedule = psy.invokes.invoke_list[0].schedule

    # 1st loop should iterate over dofs to ndofs. Check output
    loop1 = schedule.children[0]
    haloex1 = schedule.children[1]
    haloex2 = schedule.children[2]
    loop2 = schedule.children[3]
    assert len(schedule.children) == 4
    assert isinstance(loop1, DynLoop)
    assert loop1.upper_bound_name == "ndofs"
    assert isinstance(haloex1, DynHaloExchange)
    assert haloex1.field.name == "f1"
    assert haloex1.required() == (True, True)
    assert isinstance(haloex2, DynHaloExchange)
    assert haloex2.field.name == "f2"
    assert haloex2.required() == (True, False)
    assert isinstance(loop2, DynLoop)

    # just check compilation here (not later in this test) as
    # compilation of redundant computation is checked separately
    if utils.TEST_COMPILE:
        # If compilation testing has been enabled
        # (--compile --f90="<compiler_name>" flags to py.test)
        assert utils.code_compiles(API, psy, tmpdir, f90, f90flags)

    # make 1st loop iterate over dofs to the level 1 halo and check
    # output. There should be no halo exchange for field "f1"
    rc_trans = Dynamo0p3RedundantComputationTrans()
    rc_trans.apply(schedule.children[0], depth=1)
    loop1 = schedule.children[0]
    haloex = schedule.children[1]
    loop2 = schedule.children[2]
    assert len(schedule.children) == 3
    assert isinstance(loop1, DynLoop)
    assert loop1.upper_bound_name == "dof_halo"
    assert loop1.upper_bound_halo_depth == 1
    assert isinstance(haloex, DynHaloExchange)
    assert haloex.field.name == "f2"
    assert haloex.required() == (True, False)
    assert isinstance(loop2, DynLoop)

    # make 1st loop iterate over dofs to the maximum halo depth and
    # check output
    rc_trans.apply(schedule.children[0])
    loop1 = schedule.children[0]
    haloex = schedule.children[1]
    loop2 = schedule.children[2]
    assert len(schedule.children) == 3
    assert isinstance(loop1, DynLoop)
    assert loop1.upper_bound_name == "dof_halo"
    assert not loop1.upper_bound_halo_depth
    assert isinstance(haloex, DynHaloExchange)
    assert haloex.field.name == "f2"
    assert haloex.required() == (True, False)
    assert isinstance(loop2, DynLoop)


def test_gh_inc_nohex_3(tmpdir, f90, f90flags, monkeypatch):
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
    psy = PSyFactory(API).create(info)
    schedule = psy.invokes.invoke_list[0].schedule

    # check we have no halo exchanges for field "f1"
    assert len(schedule.children) == 3
    haloex = schedule.children[0]
    loop1 = schedule.children[1]
    loop2 = schedule.children[2]
    assert isinstance(haloex, DynHaloExchange)
    assert haloex.field.name == "f2"
    assert haloex.required() == (True, False)
    assert haloex._compute_halo_depth() == "1"
    assert isinstance(loop1, DynLoop)
    assert isinstance(loop2, DynLoop)

    # just check compilation here (not later in this test) as
    # compilation of redundant computation is checked separately
    if utils.TEST_COMPILE:
        # If compilation testing has been enabled
        # (--compile --f90="<compiler_name>" flags to py.test)
        assert utils.code_compiles(API, psy, tmpdir, f90, f90flags)

    # make 1st loop iterate over cells to the level 2 halo and check output
    rc_trans = Dynamo0p3RedundantComputationTrans()
    rc_trans.apply(schedule.children[1], depth=2)

    def check(schedule, f1depth, f2depth):
        '''check that the schedule is modified in the expected way. In
        particular, check that the depth of the halo exchange for
        field 'f1' is what we are expecting

        :param schedule: a dynamo0.3 API schedule object
        :type schedule: :py:class:`psyclone.dynamo0p3.DynSchedule`.
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
        assert isinstance(haloex1, DynHaloExchange)
        assert haloex1.field.name == "f2"
        assert haloex1._compute_halo_depth() == f2depth
        assert haloex1.required() == (True, False)
        assert isinstance(haloex2, DynHaloExchange)
        assert haloex2.field.name == "f1"
        assert haloex2._compute_halo_depth() == f1depth
        assert haloex2.required() == (True, False)
        assert isinstance(loop1, DynLoop)
        assert isinstance(loop2, DynLoop)

    # we should now have a speculative halo exchange at the start of
    # the schedule for "f1" to depth 1 and "f2" to depth 2
    check(schedule, f1depth="1", f2depth="2")

    # make 1st loop iterate over cells to the maximum halo depth and
    # check output
    rc_trans.apply(schedule.children[2])
    # we should now have a speculative halo exchange at the start of
    # the schedule for "f1" to depth max halo - 1 and "f2" to max halo
    check(schedule, f1depth="mesh%get_halo_depth()-1",
          f2depth="mesh%get_halo_depth()")


def test_gh_inc_nohex_4(tmpdir, f90, f90flags, monkeypatch):
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
    psy = PSyFactory(API).create(info)
    schedule = psy.invokes.invoke_list[0].schedule

    def check(schedule, f1depth, f2depth):
        '''check that the schedule is modified in the expected way. In
        particular, check that the depth of the halo exchange for
        field 'f1' is what we are expecting

        :param schedule: a dynamo0.3 API schedule object
        :type schedule: :py:class:`psyclone.dynamo0p3.DynSchedule`.
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
        assert isinstance(haloex1, DynHaloExchange)
        assert haloex1.field.name == "f1"
        assert haloex1._compute_halo_depth() == f1depth
        assert haloex1.required() == (True, False)
        assert isinstance(haloex2, DynHaloExchange)
        assert haloex2.field.name == "f2"
        assert haloex2._compute_halo_depth() == f2depth
        assert haloex2.required() == (True, False)
        assert isinstance(loop1, DynLoop)
        assert isinstance(loop2, DynLoop)

    # we should now have a speculative halo exchange at the start of
    # the schedule for "f1" to depth 1 and "f2" to depth 1
    check(schedule, f1depth="1", f2depth="1")

    # just check compilation here (not later in this test) as
    # compilation of redundant computation is checked separately
    if utils.TEST_COMPILE:
        # If compilation testing has been enabled
        # (--compile --f90="<compiler_name>" flags to py.test)
        assert utils.code_compiles(API, psy, tmpdir, f90, f90flags)

    # make 1st loop iterate over cells to the level 2 halo and check output
    rc_trans = Dynamo0p3RedundantComputationTrans()
    rc_trans.apply(schedule.children[2], depth=2)
    # we should now have a speculative halo exchange at the start of
    # the schedule for "f1" to depth 1 and "f2" to depth 2
    check(schedule, f1depth="1", f2depth="2")

    # make 1st loop iterate over cells to the maximum halo depth and
    # check output
    rc_trans.apply(schedule.children[2])
    # we should now have a speculative halo exchange at the start of
    # the schedule for "f1" to depth max halo - 1 and "f2" to max halo
    check(schedule, f1depth="mesh%get_halo_depth()-1",
          f2depth="mesh%get_halo_depth()")


def test_gh_inc_max(tmpdir, f90, f90flags, monkeypatch, annexed):
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
    psy = PSyFactory(API).create(info)
    schedule = psy.invokes.invoke_list[0].schedule
    rc_trans = Dynamo0p3RedundantComputationTrans()

    def check(haloex, depth):
        '''check the halo exchange has the expected properties

        :param haloex: a dynamo0.3 API halo-exchange object
        :type haloex: :py:class:`psyclone.dynamo0p3.DynHaloExchange`.
        :param int depth: The expected depth of the halo exchange \
        passed in as the first argument

        '''

        assert isinstance(haloex, DynHaloExchange)
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
    rc_trans.apply(schedule.children[loop2idx], depth=2)
    # f1 halo exchange should still be depth 1 : max(1,1)
    haloex = schedule.children[haloidx]
    check(haloex, "1")
    rc_trans.apply(schedule.children[loop2idx], depth=3)
    # f1 halo exchange should be depth 2 (max(1,2)
    haloex = schedule.children[haloidx]
    check(haloex, "2")
    rc_trans.apply(schedule.children[loop2idx])
    # f1 halo exchange should be depth max(1,max-1)
    haloex = schedule.children[haloidx]
    check(haloex, "max(mesh%get_halo_depth()-1,1)")
    # just check compilation here as it is the most
    # complicated. (Note, compilation of redundant computation is
    # checked separately)
    if utils.TEST_COMPILE:
        # If compilation testing has been enabled
        # (--compile --f90="<compiler_name>" flags to py.test)
        assert utils.code_compiles(API, psy, tmpdir, f90, f90flags)
    rc_trans.apply(schedule.children[loop1idx])
    # f1 halo exchange should be depth max
    haloex = schedule.children[haloidx]
    check(haloex, "mesh%get_halo_depth()")
