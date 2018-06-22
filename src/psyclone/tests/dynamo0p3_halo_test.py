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
# Author R. Ford and A. R. Porter, STFC Daresbury Lab

'''This module tests the Dynamo 0.3 API-specific halo exchange
implementation using pytest. '''

import os
from psyclone.parse import parse, ParseError
from psyclone.psyGen import PSyFactory, GenerationError
from psyclone.dynamo0p3 import DynLoop, DynHaloExchange
from psyclone.transformations import Dynamo0p3RedundantComputationTrans
import utils

# constants
BASE_PATH = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                         "test_files", "dynamo0p3")


def test_gh_inc_nohex_1(tmpdir, f90, f90flags, monkeypatch):
    '''If COMPUTE_ANNEXED_DOFS is True, then a gh_inc access to a field in
    a kernel (iterating to the l1 halo) does not require a halo
    exchange when the previous writer is known and iterates over dofs
    to nannexed, halo(1) and halo max depth

    '''
    # ensure that COMPUTE_ANNEXED_DOFS is True
    import psyclone.config
    monkeypatch.setattr(psyclone.config, "COMPUTE_ANNEXED_DOFS", True)

    # parse and get psy schedule
    _, info = parse(os.path.join(BASE_PATH,
                                 "14.12_halo_wdofs_to_inc.f90"),
                    api="dynamo0.3")
    psy = PSyFactory("dynamo0.3").create(info)
    schedule = psy.invokes.invoke_list[0].schedule

    def check_schedule(schedule):
        '''check this schedule has expected structure (loop, haloexchange,
        loop). In paricular there should be no halo exchange for the
        write-to-gh_inc dependence

        '''
        schedule.view()
        assert len(schedule.children) == 3
        loop1 = schedule.children[0]
        haloex = schedule.children[1]
        loop2 = schedule.children[2]
        assert isinstance(loop1, DynLoop)
        assert isinstance(haloex, DynHaloExchange)
        assert haloex.field.name == "f2"
        assert haloex.required() == (True, False)
        assert isinstance(loop2, DynLoop)

    # 1st loop iterate over dofs to nannexed. Check output
    assert schedule.children[0].upper_bound_name == "nannexed"
    check_schedule(schedule)

    # just check compilation at this point as compilation of redundant
    # computation is checked separately
    if utils.TEST_COMPILE:
        # If compilation testing has been enabled
        # (--compile --f90="<compiler_name>" flags to py.test)
        assert utils.code_compiles("dynamo0.3", psy, tmpdir, f90, f90flags)

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
    '''If COMPUTE_ANNEXED_DOFS is True, then a gh_inc access to a field in
    a kernel (iterating to the l1 halo) does not require a halo
    exchange when the previous writer is known and iterates over cells
    to halo(1), halo(2) and halo max depth. Also, if the previous
    writer is a gh_inc access and its previous writer is unknown then
    it does not require a halo exchange if it writes to halo(1), but
    requires a speculative halo exchange to halo(1) if iterating to
    halo(2) and a speculative halo exchange to halo(max_depth-1) if
    iterating to the maximum halo depth

    '''
    # ensure that COMPUTE_ANNEXED_DOFS is True
    import psyclone.config
    monkeypatch.setattr(psyclone.config, "COMPUTE_ANNEXED_DOFS", True)

    # parse and get psy schedule
    _, info = parse(os.path.join(BASE_PATH,
                                 "14.13_halo_inc_to_inc.f90"),
                    api="dynamo0.3")
    psy = PSyFactory("dynamo0.3").create(info)
    schedule = psy.invokes.invoke_list[0].schedule

    schedule.view()
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

    # just check compilation at this point as compilation of redundant
    # computation is checked separately
    if utils.TEST_COMPILE:
        # If compilation testing has been enabled
        # (--compile --f90="<compiler_name>" flags to py.test)
        assert utils.code_compiles("dynamo0.3", psy, tmpdir, f90, f90flags)

    # make 1st loop iterate over cells to the level 2 halo and check output
    rc_trans = Dynamo0p3RedundantComputationTrans()
    rc_trans.apply(schedule.children[1], depth=2)
    schedule.view()

    assert len(schedule.children) == 4
    haloex1 = schedule.children[0]
    haloex2 = schedule.children[1]
    loop1 = schedule.children[2]
    loop2 = schedule.children[3]
    assert isinstance(haloex1, DynHaloExchange)
    assert haloex1.field.name == "f2"
    assert haloex1._compute_halo_depth() == "2"
    assert haloex1.required() == (True, False)
    assert isinstance(haloex2, DynHaloExchange)
    assert haloex2.field.name == "f1"
    assert haloex2._compute_halo_depth() == "1"
    assert haloex2.required() == (True, False)
    assert isinstance(loop1, DynLoop)
    assert loop1.upper_bound_name == "cell_halo"
    assert loop1.upper_bound_halo_depth == 2
    assert isinstance(loop2, DynLoop)

    # make 1st loop iterate over cells to the maximum halo depth and
    # check output
    rc_trans.apply(schedule.children[2])
    schedule.view()
    assert len(schedule.children) == 4
    haloex1 = schedule.children[0]
    haloex2 = schedule.children[1]
    loop1 = schedule.children[2]
    loop2 = schedule.children[3]
    assert isinstance(haloex1, DynHaloExchange)
    assert haloex1.field.name == "f2"
    assert haloex1._compute_halo_depth() == "mesh%get_halo_depth()"
    assert haloex1.required() == (True, False)
    assert isinstance(haloex2, DynHaloExchange)
    assert haloex2.field.name == "f1"
    assert haloex2._compute_halo_depth() == "mesh%get_halo_depth()-1"
    assert haloex2.required() == (True, False)
    assert isinstance(loop1, DynLoop)
    assert loop1.upper_bound_name == "cell_halo"
    assert not loop1.upper_bound_halo_depth
    assert isinstance(loop2, DynLoop)


# redundant computation correct value (1st gh_inc, depth=1, 2nd gh_inc depth=2)
    
# multiple read dependencies, one with max-1, other with a value
# multiple read dependencies, both with max-1

    
# 2: If COMPUTE_ANNEXED_DOFS is False, then a gh_inc access to a field
# in a kernel (iterating to the l1 halo) does not require a halo
# exchange when the previous writer is

# b) known and iterates over dofs to the level 1 halo (or greater)
# c) known and iterates over dofs to the maximum halo depth
# d) known and iterates over cells to the l1 halo
# e) known and iterates over cells to the l2 halo or greater
# f) known and iterates over cells to the meximum halo depth

# 3: If COMPUTE_ANNEXED_DOFS is False, then a gh_inc access to a field
# in a kernel (iterating to the l1 halo) does require a halo
# exchange when the previous writer is

# a) known and iterates over dofs to ndofs

# 4: If COMPUTE_ANNEXED_DOFS is False, then a gh_inc access to a field
# in a kernel (iterating to the l1 halo) might require a halo
# exchange when the previous writer is

# g) unknown (as it is outside the scope of an invoke)
