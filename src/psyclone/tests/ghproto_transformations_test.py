# -------------------------------------------------------------------------
# (c) The copyright relating to this work is owned jointly by the Crown,
# Met Office and NERC 2015.
# However, it has been created with the help of the GungHo Consortium,
# whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
# -------------------------------------------------------------------------
# Authors R. Ford and A. R. Porter, STFC Daresbury Lab

''' Tests for transformations applied to the GH-proto API '''

from psyclone.parse import parse
from psyclone.psyGen import PSyFactory
from psyclone.transformations import LoopFuseTrans
import os
import pytest


@pytest.mark.xfail(reason="bug 3")
def test_loop_fuse_trans():
    ''' test of the loop-fuse transformation '''
    _, info = parse(os.path.
                    join(os.path.dirname(os.path.abspath(__file__)),
                         "test_files", "gunghoproto",
                         "3_two_functions_shared_arguments.f90"),
                    api="gunghoproto")
    psy = PSyFactory("gunghoproto").create(info)
    invoke = psy.invokes.get("invoke_0")
    schedule = invoke.schedule
    loop1 = schedule.children[0]
    loop2 = schedule.children[1]
    trans = LoopFuseTrans()
    schedule, _ = trans.apply(loop1, loop2)
    gen = str(psy.gen)
    for idx, line in enumerate(gen.split('\n')):
        if line.find("DO column=1,topology") != -1:
            do_idx = idx
        if line.find("CALL testkern1_code(") != -1:
            call1_idx = idx
        if line.find("CALL testkern2_code(") != -1:
            call2_idx = idx
        if line.find("END DO") != -1:
            enddo_idx = idx
    # 4 lines should be in sequence as calls have been fused into one loop
    assert enddo_idx-call2_idx == 1 and\
        call2_idx-call1_idx == 1 and\
        call1_idx-do_idx == 1
