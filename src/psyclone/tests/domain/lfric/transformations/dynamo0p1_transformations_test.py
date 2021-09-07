# -------------------------------------------------------------------------
# (c) The copyright relating to this work is owned jointly by the Crown,
# Met Office and NERC 2015.
# However, it has been created with the help of the GungHo Consortium,
# whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
# -------------------------------------------------------------------------
# Authors R. Ford, A. R. Porter and S. Siso, STFC Daresbury Lab

''' Contains tests for transformations on the Dynamo 0.1 API '''

from __future__ import absolute_import
import pytest
from psyclone.configuration import Config
from psyclone.tests.utilities import get_invoke

from psyclone.transformations import OMPParallelTrans

TEST_API = "dynamo0.1"


@pytest.fixture(scope="module", autouse=True)
def setup():
    '''Make sure that all tests here use dynamo0.1 as API.'''
    Config.get().api = "dynamo0.1"
    yield()
    Config._instance = None


def test_openmp_region():
    ''' Test the application of an OpenMP parallel region transformation
    to a single loop '''
    psy, invoke = get_invoke("algorithm/1_single_function.f90", TEST_API,
                             name="invoke_0_testkern_type")
    schedule = invoke.schedule
    rtrans = OMPParallelTrans()
    rtrans.apply(schedule.children[0])
    gen = str(psy.gen)

    # Check that our list of private variables is correct
    assert "!$omp parallel default(shared), private(cell,map)" in gen

    for idx, line in enumerate(gen.split('\n')):
        if "!$omp parallel default(shared)" in line:
            startpara_idx = idx
        if "DO cell=1,f1%get_ncell()" in line:
            do_idx = idx
        if "CALL f1%vspace%get_cell_dofmap(cell, map)" in line:
            dmap_idx = idx
        if "CALL testkern_code(nlayers, ndf, map, f1%data, "\
           "f2%data, m1%data)" in line:
            kcall_idx = idx
        if "END DO" in line:
            enddo_idx = idx
        if "!$omp end parallel" in line:
            endpara_idx = idx
    assert do_idx == startpara_idx + 1
    assert dmap_idx == do_idx + 1
    assert kcall_idx == dmap_idx + 1
    assert enddo_idx == kcall_idx + 1
    assert endpara_idx == enddo_idx + 1
