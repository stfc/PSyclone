# ----------------------------------------------------------------------------
# (c) Science and Technology Facilities Council, 2016
# ----------------------------------------------------------------------------
# Author A. Porter, STFC Daresbury Laboratory

''' This module tests the GOcean 0.1 API using pytest. '''

import pytest
from parse import parse
from psyGen import PSyFactory
import os

API = "gocean0.1"


def test_loop_bounds_gen_multiple_loops():
    ''' Test that we only generate one assignment for a loop-bounds
    variable when we have multiple loops '''
    _, info = parse(os.path.join(os.path.
                                 dirname(os.path.abspath(__file__)),
                                 "test_files", "gocean0p1",
                                 "openmp_fuse_test.f90"),
                    api=API)
    psy = PSyFactory(API).create(info)
    gen = str(psy.gen)
    print gen

    expected = (
        "      idim2 = SIZE(uold, 2)\n"
        "      idim1 = SIZE(uold, 1)\n"
        "      DO j=1,idim2\n"
        "        DO i=1,idim1\n"
        "          CALL time_smooth_code(i, j, u, unew, uold)\n"
        "        END DO \n"
        "      END DO \n"
        "      idim2 = SIZE(vold, 2)\n"
        "      idim1 = SIZE(vold, 1)\n"
        "      DO j=1,idim2\n"
        "        DO i=1,idim1\n"
        "          CALL time_smooth_code(i, j, v, vnew, vold)\n"
        "        END DO \n"
        "      END DO \n"
        "      idim2 = SIZE(pold, 2)\n"
        "      idim1 = SIZE(pold, 1)\n"
        "      DO j=1,idim2\n"
        "        DO i=1,idim1\n"
        "          CALL time_smooth_code(i, j, p, pnew, pold)\n"
        "        END DO \n"
        "      END DO ")
    assert expected in gen
