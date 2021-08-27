# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2020-2021, Science and Technology Facilities Council
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
# Author J. Henrichs, Bureau of Meteorology
# Modifications: R. W. Ford, STFC Daresbury Lab
#                A. R. Porter, STFC Daresbury Lab
#                S. Siso, STFC Daresbury Lab

''' Module containing tests for generating PSyData hooks'''

from __future__ import absolute_import

import pytest

from fparser.common.readfortran import FortranStringReader

from psyclone.errors import InternalError
from psyclone.gocean1p0 import GOInvokeSchedule
from psyclone.psyir.nodes import colored, PSyDataNode, Schedule
from psyclone.psyir.transformations import PSyDataTrans, TransformationError
from psyclone.psyGen import Loop, PSyFactory
from psyclone.tests.utilities import get_invoke
from psyclone.transformations import GOceanOMPLoopTrans
from psyclone.transformations import ACCKernelsTrans


# -----------------------------------------------------------------------------
def test_psy_data_trans_empty_list():
    ''' Check that the transformation rejects an empty list of nodes. '''
    data_trans = PSyDataTrans()
    with pytest.raises(TransformationError) as err:
        data_trans.apply([])
    assert "Cannot apply transformation to an empty list" in str(err.value)


# -----------------------------------------------------------------------------
def test_psy_data_get_unique_region_name():
    '''Check the creation of unique region names.'''
    _, invoke = get_invoke("test11_different_iterates_over_one_invoke.f90",
                           "gocean1.0", idx=0, dist_mem=False)
    schedule = invoke.schedule
    # This test expects constant loop bounds
    schedule._const_loop_bounds = True

    data_trans = PSyDataTrans()

    # Test that we can specify the region names we want:
    region_name = \
        data_trans.get_unique_region_name(schedule,
                                          {"region_name": ("a", "b")})
    assert region_name == ("a", "b")

    # Test default names used with multiple kernels:
    region_name = data_trans.get_unique_region_name(schedule, {})
    assert region_name == ('psy_single_invoke_different_iterates_over',
                           'invoke_0:r0')

    # Test default with a single kernel (note that the transformation caches
    # kernel names, so thi)
    region_name = data_trans.get_unique_region_name([schedule.children[0]], {})
    assert region_name == ('psy_single_invoke_different_iterates_over',
                           'invoke_0:compute_cv_code:r1')

    # Check that incorrect region names are handled:
    with pytest.raises(InternalError) as err:
        data_trans.get_unique_region_name(schedule,
                                          {"region_name": 1})
    assert "Error in PSyDataTrans. The name must be a tuple containing two " \
           "non-empty strings." in str(err.value)


# -----------------------------------------------------------------------------
def test_psy_data_trans_basic(capsys):
    '''Check basic functionality: node names, schedule view.
    '''
    _, invoke = get_invoke("test11_different_iterates_over_one_invoke.f90",
                           "gocean1.0", idx=0, dist_mem=False)
    schedule = invoke.schedule
    # This test expects constant loop bounds
    schedule._const_loop_bounds = True

    data_trans = PSyDataTrans()
    assert "Create a sub-tree of the PSyIR that has a node of type " \
           "PSyDataNode at its root" in str(data_trans)

    assert data_trans.name == "PSyDataTrans"
    data_trans.apply(schedule)

    assert isinstance(invoke.schedule[0], PSyDataNode)

    schedule.view()
    out, _ = capsys.readouterr()

    gsched = colored("GOInvokeSchedule", GOInvokeSchedule._colour)
    sched = colored("Schedule", Schedule._colour)
    loop = Loop().coloured_name(True)
    data = invoke.schedule[0].coloured_name(True)

    # Do one test based on schedule view, to make sure colouring
    # and indentation is correct
    expected = (
        gsched + "[invoke='invoke_0', Constant loop bounds=True]\n"
        "    0: " + data + "[]\n"
        "        " + sched + "[]\n"
        "            0: " + loop + "[type='outer', field_space='go_cv', "
        "it_space='go_internal_pts']\n")
    assert expected in out

    # Insert a DataTrans call between outer and inner loop.
    # This tests that we find the subroutine node even
    # if it is not the immediate parent.
    data_trans.apply(invoke.schedule[0].psy_data_body[0].loop_body[0])

    new_sched_str = str(invoke.schedule)
    correct = ("""GOInvokeSchedule[invoke='invoke_0', \
Constant loop bounds=True]:
PSyDataStart[var=psy_data]
GOLoop[id:'', variable:'j', loop_type:'outer']
Literal[value:'2', Scalar<INTEGER, UNDEFINED>]
Literal[value:'jstop-1', Scalar<INTEGER, UNDEFINED>]
Literal[value:'1', Scalar<INTEGER, UNDEFINED>]
Schedule:
PSyDataStart[var=psy_data_1]
GOLoop[id:'', variable:'i', loop_type:'inner']
Literal[value:'2', Scalar<INTEGER, UNDEFINED>]
Literal[value:'istop', Scalar<INTEGER, UNDEFINED>]
Literal[value:'1', Scalar<INTEGER, UNDEFINED>]
Schedule:
kern call: compute_cv_code
End Schedule
End GOLoop
PSyDataEnd[var=psy_data_1]
End Schedule
End GOLoop
GOLoop[id:'', variable:'j', loop_type:'outer']
Literal[value:'1', Scalar<INTEGER, UNDEFINED>]
Literal[value:'jstop+1', Scalar<INTEGER, UNDEFINED>]
Literal[value:'1', Scalar<INTEGER, UNDEFINED>]
Schedule:
GOLoop[id:'', variable:'i', loop_type:'inner']
Literal[value:'1', Scalar<INTEGER, UNDEFINED>]
Literal[value:'istop+1', Scalar<INTEGER, UNDEFINED>]
Literal[value:'1', Scalar<INTEGER, UNDEFINED>]
Schedule:
kern call: bc_ssh_code
End Schedule
End GOLoop
End Schedule
End GOLoop
PSyDataEnd[var=psy_data]
End Schedule""")

    assert correct in new_sched_str


# -----------------------------------------------------------------------------
def test_class_definitions():
    '''Tests if the class-prefix can be set and behaves as expected.
    '''

    psy, invoke = get_invoke("test11_different_iterates_over_one_invoke.f90",
                             "gocean1.0", idx=0)
    schedule = invoke.schedule

    data_trans = PSyDataTrans()
    data_trans.apply(schedule)
    code = str(psy.gen)

    # By default, no prefix should be used:
    assert "USE psy_data_mod, ONLY: PSyDataType" in code
    assert "TYPE(PSyDataType), target, save :: psy_data" in code
    assert "CALL psy_data" in code

    # This puts the new PSyDataNode with prefix "extract" around the
    # previous PSyDataNode, but the prefix was not used previously.
    data_trans.apply(schedule, {"prefix": "extract"})
    code = str(psy.gen)
    assert "USE extract_psy_data_mod, ONLY: extract_PSyDataType" in code
    assert "TYPE(extract_PSyDataType), target, save :: extract_psy_data" \
        in code
    assert "CALL extract_psy_data" in code
    # The old call must still be there (e.g. not somehow be changed
    # by setting the prefix)
    assert "USE psy_data_mod, ONLY: PSyDataType" in code
    assert "TYPE(PSyDataType), target, save :: psy_data" in code
    assert "CALL psy_data" in code

    # Now add a third class: "profile", and make sure all previous
    # and new declarations and calls are there:
    data_trans.apply(schedule, {"prefix": "profile"})
    code = str(psy.gen)
    assert "USE psy_data_mod, ONLY: PSyDataType" in code
    assert "USE extract_psy_data_mod, ONLY: extract_PSyDataType" in code
    assert "USE profile_psy_data_mod, ONLY: profile_PSyDataType" in code

    assert "TYPE(PSyDataType), target, save :: psy_data" in code
    assert "TYPE(extract_PSyDataType), target, save :: extract_psy_data" \
        in code
    assert "TYPE(profile_PSyDataType), target, save :: profile_psy_data" \
        in code

    assert "CALL psy_data" in code
    assert "CALL extract_psy_data" in code
    assert "CALL profile_psy_data" in code

    with pytest.raises(TransformationError) as err:
        data_trans.apply(schedule, {"prefix": "invalid-prefix"})
    assert "Error in 'prefix' parameter: found 'invalid-prefix', expected " \
        "one of " in str(err.value)
    assert "as defined in /" in str(err.value)


# -----------------------------------------------------------------------------
def test_psydata_validation_errors():
    '''Tests error handling of the profile region transformation.'''

    # Test that we don't add a PSyData node inside an OMP do loop:
    _, invoke = get_invoke("test27_loop_swap.f90", "gocean1.0",
                           name="invoke_loop1", dist_mem=False)
    schedule = invoke.schedule

    # Parallelise the first loop:
    omp_loop = GOceanOMPLoopTrans()
    omp_loop.apply(schedule[0])

    pdt = PSyDataTrans()
    # Inserting a ProfileTrans inside a omp do loop is syntactically
    # incorrect, the inner part must be a do loop only:
    with pytest.raises(TransformationError) as excinfo:
        pdt.apply(invoke.schedule[0].dir_body[0])

    assert "A PSyData node cannot be inserted between an OpenMP/ACC "\
           "directive and the loop(s) to which it applies!" \
           in str(excinfo.value)

    with pytest.raises(TransformationError) as excinfo:
        pdt.apply(invoke.schedule[0], {"region_name": "xx"})
    assert "Error in PSyDataTrans. User-supplied region name must be a " \
        "tuple containing two non-empty strings" in str(excinfo.value)


# -----------------------------------------------------------------------------
def test_psydata_validation_acc(parser):
    ''' Check putting PSyData in an OpenACC region (requires NEMO api).'''

    reader = FortranStringReader("program write_out\n"
                                 "integer :: ji, jpj\n"
                                 "real(kind=wp) :: sto_tmp(5)\n"
                                 "do ji = 1,jpj\n"
                                 "sto_tmp(ji)=1\n"
                                 "end do\n"
                                 "do ji = 1,jpj\n"
                                 "sto_tmp(ji)=1\n"
                                 "end do\n"
                                 "end program write_out\n")
    code = parser(reader)
    psy = PSyFactory("nemo", distributed_memory=False).create(code)
    schedule = psy.invokes.invoke_list[0].schedule

    acc_trans = ACCKernelsTrans()
    acc_trans.apply(schedule.children[0])
    pdt = PSyDataTrans()
    with pytest.raises(TransformationError) as err:
        pdt.apply(schedule.children[0].children[0])
    assert "A PSyData node cannot be inserted inside an OpenACC region" \
           in str(err.value)


# -----------------------------------------------------------------------------
def test_psydata_mod_use_clash(parser):
    ''' Check that we abort cleanly if we encounter a 'use' of a module that
    clashes with the one we would 'use' for the profiling API. '''
    reader = FortranStringReader("program the_clash\n"
                                 "  use psy_data_mod, only: "
                                 "some_var\n"
                                 "  real :: my_array(20,10)\n"
                                 "  my_array(:,:) = 0.0\n"
                                 "end program the_clash\n")
    code = parser(reader)
    psy = PSyFactory("nemo", distributed_memory=False).create(code)
    schedule = psy.invokes.invoke_list[0].schedule
    pdt = PSyDataTrans()

    with pytest.raises(TransformationError) as err:
        pdt.apply(schedule.children[0])
    assert ("Cannot add PSyData calls because there is already a symbol "
            "named 'psy_data_mod' which clashes" in str(err.value))
