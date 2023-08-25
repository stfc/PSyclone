from psyclone.domain.lfric.tools import LFRicDependencyTools
from psyclone.psyir.nodes import Loop
from psyclone.tests.utilities import get_invoke
from psyclone.transformations import Dynamo0p3ColourTrans

TEST_API = "dynamo0.3"


def test_array_access_parallelisable():
    '''
    '''
    psy, invoke = get_invoke("4.8_multikernel_invokes.f90",
                             TEST_API, idx=0, dist_mem=False)
    schedule = invoke.schedule
    print(schedule.view())
    loops = schedule.walk(Loop)
    dtools = LFRicDependencyTools()
    # By default, none of the loops are parallelisable.
    for loop in loops:
        assert not dtools.can_loop_be_parallelised(loop)
    # Once coloured, they are parallelisable
    ctrans = Dynamo0p3ColourTrans()
    for loop in loops:
        ctrans.apply(loop)
    print(schedule.view())
    loops = schedule.walk(Loop)
    for loop in loops:
        if loop.loop_type == "colour":
            #import pdb; pdb.set_trace()
            assert dtools.can_loop_be_parallelised(loop)
        else:
            assert not dtools.can_loop_be_parallelised(loop)
