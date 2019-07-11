
from __future__ import absolute_import, print_function
import pytest
from fparser.common.readfortran import FortranStringReader
from psyclone.psyGen import PSyFactory, TransInfo
from psyclone.transformations import TransformationError


# The PSyclone API under test
API = "nemo"

def test_profile_single_loop(parser):
    ''' Check that the correct code is added to the generated Fortran
    when profiling a single loop nest. '''
    reader = FortranStringReader("program do_loop\n"
                                 "use kind_mod, only: wp\n"
                                 "real :: sto_tmp(jpj), sto_tmp2(jpj)\n"
                                 "do ji = 1,jpj\n"
                                 "  sto_tmp(ji) = 1.0d0\n"
                                 "end do\n"
                                 "do ji = 1,jpj\n"
                                 "  sto_tmp2(ji) = 1.0d0\n"
                                 "end do\n"
                                 "end program do_loop\n")
    code = parser(reader)
    psy = PSyFactory(API, distributed_memory=False).create(code)
    schedule = psy.invokes.invoke_list[0].schedule
    acc_trans = TransInfo().get_trans_name('ProfileRegionTrans')
    schedule, _ = acc_trans.apply(schedule.children[0])
    code = str(psy.gen)
    assert (
        "  USE profile_mod, ONLY: ProfileData, ProfileStart, ProfileEnd\n"
        "  USE kind_mod, ONLY: wp\n" in code)
    assert (
        "  REAL :: sto_tmp(jpj), sto_tmp2(jpj)\n"
        "  TYPE(ProfileData), SAVE :: profile0\n" in code)
    assert (
        "  CALL ProfileStart('do_loop', 'region_0', profile0)\n"
        "  DO ji = 1, jpj\n"
        "    sto_tmp(ji) = 1.0D0\n"
        "  END DO\n"
        "  CALL ProfileEnd(region_0)\n" in code)


