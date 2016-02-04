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
    lines = gen.splitlines()
    matching_lines = []
    for idx, line in enumerate(lines):
        if "idim2 = SIZE" in line:
            if "idim2 = SIZE" in lines[idx-1]:
                assert False

