# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2018-2021, Science and Technology Facilities Council.
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
# Modified by R. W. Ford, STFC Daresbury Lab
# Modified by A. R. Porter, STFC Daresbury Lab
# Modified by I. Kavcic, Met Office

''' Module containing tests for generating monitoring hooks'''

from __future__ import absolute_import

import re
import pytest

from psyclone.generator import GenerationError
from psyclone.profiler import Profiler
from psyclone.psyir.nodes import (colored, ProfileNode, Loop, Literal,
                                  Assignment, Return, Reference,
                                  KernelSchedule, Schedule)
from psyclone.psyir.symbols import (SymbolTable, REAL_TYPE, DataSymbol)
from psyclone.psyir.transformations import TransformationError
from psyclone.psyir.transformations import ProfileTrans
from psyclone.tests.utilities import get_invoke
from psyclone.transformations import GOceanOMPLoopTrans, OMPParallelTrans
from psyclone.gocean1p0 import GOInvokeSchedule


# -----------------------------------------------------------------------------
def teardown_function():
    '''This function is called at the end of any test function. It disables
    any automatic profiling set. This is necessary in case of a test failure
    to make sure any further tests will not be run with profiling enabled.
    '''
    Profiler.set_options([])


# -----------------------------------------------------------------------------
def test_profile_basic(capsys):
    '''Check basic functionality: node names, schedule view.
    '''
    Profiler.set_options([Profiler.INVOKES])
    _, invoke = get_invoke("test11_different_iterates_over_one_invoke.f90",
                           "gocean1.0", idx=0, dist_mem=False)
    # This test expects constant loop bounds
    invoke.schedule._const_loop_bounds = True
    Profiler.add_profile_nodes(invoke.schedule, Loop)

    assert isinstance(invoke.schedule[0], ProfileNode)

    invoke.schedule.view()
    out, _ = capsys.readouterr()

    gsched = colored("GOInvokeSchedule", GOInvokeSchedule._colour)
    sched = colored("Schedule", Schedule._colour)
    loop = Loop().coloured_name(True)
    profile = invoke.schedule[0].coloured_name(True)

    # Do one test based on schedule view, to make sure colouring
    # and indentation is correct
    expected = (
        gsched + "[invoke='invoke_0', Constant loop bounds=True]\n"
        "    0: " + profile + "[]\n"
        "        " + sched + "[]\n"
        "            0: " + loop + "[type='outer', field_space='go_cv', "
        "it_space='go_internal_pts']\n")
    assert expected in out

    prt = ProfileTrans()

    # Insert a profile call between outer and inner loop.
    # This tests that we find the subroutine node even
    # if it is not the immediate parent.
    new_sched, _ = prt.apply(invoke.schedule[0].profile_body[0].loop_body[0])

    new_sched_str = str(new_sched)
    correct = ("""GOInvokeSchedule[invoke='invoke_0', \
Constant loop bounds=True]:
ProfileStart[var=profile_psy_data]
GOLoop[id:'', variable:'j', loop_type:'outer']
Literal[value:'2', Scalar<INTEGER, UNDEFINED>]
Literal[value:'jstop-1', Scalar<INTEGER, UNDEFINED>]
Literal[value:'1', Scalar<INTEGER, UNDEFINED>]
Schedule:
ProfileStart[var=profile_psy_data_1]
GOLoop[id:'', variable:'i', loop_type:'inner']
Literal[value:'2', Scalar<INTEGER, UNDEFINED>]
Literal[value:'istop', Scalar<INTEGER, UNDEFINED>]
Literal[value:'1', Scalar<INTEGER, UNDEFINED>]
Schedule:
kern call: compute_cv_code
End Schedule
End GOLoop
ProfileEnd
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
ProfileEnd
End Schedule""")
    assert correct in new_sched_str

    Profiler.set_options(None)


# -----------------------------------------------------------------------------
def test_profile_errors2():
    '''Test various error handling.'''

    with pytest.raises(GenerationError) as gen_error:
        Profiler.set_options(["invalid"])
    assert ("options must be one of 'invokes', 'kernels'"
            in str(gen_error.value))


# -----------------------------------------------------------------------------
def test_profile_invokes_gocean1p0():
    '''Check that an invoke is instrumented correctly
    '''
    Profiler.set_options([Profiler.INVOKES])
    _, invoke = get_invoke("test11_different_iterates_over_one_invoke.f90",
                           "gocean1.0", idx=0)
    Profiler.add_profile_nodes(invoke.schedule, Loop)

    # Convert the invoke to code, and remove all new lines, to make
    # regex matching easier
    code = str(invoke.gen()).replace("\n", "")

    # First a simple test that the nesting is correct - the
    # profile regions include both loops. Note that indeed
    # the function 'compute_cv_code' is in the module file
    # kernel_ne_offset_mod.
    correct_re = ("subroutine invoke.*"
                  "use profile_psy_data_mod, ONLY: profile_PSyDataType.*"
                  r"TYPE\(profile_PsyDataType\), target, save :: profile_"
                  r"psy_data.*call profile_psy_data%PreStart\(\"psy_single_"
                  r"invoke_different_iterates_over\", \"invoke_0:r0\", 0, "
                  r"0\).*"
                  "do j.*"
                  "do i.*"
                  "call.*"
                  "end.*"
                  "end.*"
                  r"call profile_psy_data%PostEnd")
    assert re.search(correct_re, code, re.I) is not None

    # Check that if gen() is called more than once the same profile
    # variables and region names are created:
    code_again = str(invoke.gen()).replace("\n", "")
    assert code == code_again

    # Test that two kernels in one invoke get instrumented correctly.
    _, invoke = get_invoke("single_invoke_two_kernels.f90", "gocean1.0", 0)
    Profiler.add_profile_nodes(invoke.schedule, Loop)

    # Convert the invoke to code, and remove all new lines, to make
    # regex matching easier
    code = str(invoke.gen()).replace("\n", "")

    correct_re = ("subroutine invoke.*"
                  "use profile_psy_data_mod, only: profile_PSyDataType.*"
                  r"TYPE\(profile_PSyDataType\), target, save :: "
                  "profile_psy_data.*"
                  r"call profile_psy_data%PreStart\(\"psy_single_invoke_two"
                  r"_kernels\", \"invoke_0:r0\", 0, 0\).*"
                  "do j.*"
                  "do i.*"
                  "call.*"
                  "end.*"
                  "end.*"
                  "do j.*"
                  "do i.*"
                  "call.*"
                  "end.*"
                  "end.*"
                  r"call profile_psy_data%PostEnd")
    assert re.search(correct_re, code, re.I) is not None
    Profiler.set_options(None)


# -----------------------------------------------------------------------------
def test_unique_region_names():
    '''Test that unique region names are created even when the kernel
    names are identical.'''

    Profiler.set_options([Profiler.KERNELS])
    _, invoke = get_invoke("single_invoke_two_identical_kernels.f90",
                           "gocean1.0", 0)
    Profiler.add_profile_nodes(invoke.schedule, Loop)

    # Convert the invoke to code, and remove all new lines, to make
    # regex matching easier

    code = str(invoke.gen()).replace("\n", "")

    # This regular expression puts the region names into groups.
    # Make sure that the created regions have different names, even
    # though the kernels have the same name.
    correct_re = ("subroutine invoke.*"
                  "use profile_psy_Data_mod, only: profile_PSyDataType.*"
                  r"TYPE\(profile_PSyDataType\), target, save :: "
                  "profile_psy_data.*"
                  r"TYPE\(profile_PSyDataType\), target, save :: "
                  "profile_psy_data.*"
                  r"call profile_psy_data.*%PreStart\(\"psy_single_invoke_two"
                  r"_kernels\", "
                  r"\"invoke_0:compute_cu_code:r0\", 0, 0\).*"
                  "do j.*"
                  "do i.*"
                  "call compute_cu_code.*"
                  "end.*"
                  "end.*"
                  r"call profile_psy_data.*%PostEnd.*"
                  r"call profile_psy_data.*%PreStart\(\"psy_single_invoke_two_"
                  r"kernels\", \"invoke_0:compute_cu_code:r1\", 0, 0\).*"
                  "do j.*"
                  "do i.*"
                  "call compute_cu_code.*"
                  "end.*"
                  "end.*"
                  r"call profile_psy_data.*%PostEnd")

    assert re.search(correct_re, code, re.I) is not None


# -----------------------------------------------------------------------------
def test_profile_kernels_gocean1p0():
    '''Check that all kernels are instrumented correctly
    '''
    Profiler.set_options([Profiler.KERNELS])
    _, invoke = get_invoke("single_invoke_two_kernels.f90", "gocean1.0",
                           idx=0)
    Profiler.add_profile_nodes(invoke.schedule, Loop)

    # Convert the invoke to code, and remove all new lines, to make
    # regex matching easier
    code = str(invoke.gen()).replace("\n", "")

    # Test that kernel profiling works in case of two kernel calls
    # in a single invoke subroutine - i.e. we need to have one profile
    # start call before two nested loops, and one profile end call
    # after that.
    # Also note that the '.*' after compute_cu_code is necessary since
    # the name could be changed to avoid duplicates (depending on order
    # in which the tests are executed).
    correct_re = ("subroutine invoke.*"
                  "use profile_psy_data_mod, only: profile_PSyDataType.*"
                  r"TYPE\(profile_PSyDataType\), target, save :: "
                  "profile_psy_data.*"
                  r"TYPE\(profile_PSyDataType\), target, save :: "
                  "profile_psy_data.*"
                  r"call (?P<profile1>\w*)%PreStart\(\"psy_single_invoke_two"
                  r"_kernels\", \"invoke_0:compute_cu_code:r0\", 0, 0\).*"
                  "do j.*"
                  "do i.*"
                  "call.*"
                  "end.*"
                  "end.*"
                  r"call (?P=profile1)%PostEnd.*"
                  r"call (?P<profile2>\w*)%PreStart\(\"psy_single_invoke_two"
                  r"_kernels\", \"invoke_0:time_smooth_code:r1\", 0, 0\).*"
                  "do j.*"
                  "do i.*"
                  "call.*"
                  "end.*"
                  "end.*"
                  r"call (?P=profile2)%PostEnd")
    groups = re.search(correct_re, code, re.I)
    assert groups is not None
    assert groups.group(1) != groups.group(2)

    Profiler.set_options(None)


# -----------------------------------------------------------------------------
def test_profile_named_gocean1p0():
    '''Check that the gocean 1.0 API is instrumented correctly when the
    profile name is supplied by the user.

    '''
    psy, invoke = get_invoke("test11_different_iterates_over_one_invoke.f90",
                             "gocean1.0", idx=0)
    schedule = invoke.schedule
    profile_trans = ProfileTrans()
    options = {"region_name": (psy.name, invoke.name)}
    _ = profile_trans.apply(schedule.children, options=options)
    result = str(invoke.gen())
    assert ("CALL profile_psy_data%PreStart("
            "\"psy_single_invoke_different_iterates_over\", "
            "\"invoke_0\", 0, 0)") in result


# -----------------------------------------------------------------------------
def test_profile_invokes_dynamo0p3():
    '''Check that a Dynamo 0.3 invoke is instrumented correctly
    '''
    Profiler.set_options([Profiler.INVOKES])

    # First test for a single invoke with a single kernel work as expected:
    _, invoke = get_invoke("1_single_invoke.f90", "dynamo0.3", idx=0)
    Profiler.add_profile_nodes(invoke.schedule, Loop)

    # Convert the invoke to code, and remove all new lines, to make
    # regex matching easier
    code = str(invoke.gen()).replace("\n", "")

    correct_re = ("subroutine invoke.*"
                  "use profile_psy_data_mod, only: profile_PSyDataType.*"
                  r"TYPE\(profile_PSyDataType\), target, save :: "
                  "profile_psy_data.*"
                  r"call profile_psy_data%PreStart\(\"single_invoke_psy\", "
                  r"\"invoke_0_testkern_type:testkern_code:r0\", 0, 0\).*"
                  "do cell.*"
                  "call.*"
                  "end.*"
                  r"call profile_psy_data%PostEnd")
    assert re.search(correct_re, code, re.I) is not None

    # Next test two kernels in one invoke:
    _, invoke = get_invoke("1.2_multi_invoke.f90", "dynamo0.3", idx=0)
    Profiler.add_profile_nodes(invoke.schedule, Loop)
    # Convert the invoke to code, and remove all new lines, to make
    # regex matching easier
    code = str(invoke.gen()).replace("\n", "")

    # The .* after testkern_code is necessary since the name can be changed
    # by PSyclone to avoid name duplications.
    correct_re = ("subroutine invoke.*"
                  "use profile_psy_data_mod, only: profile_PSyDataType.*"
                  r"TYPE\(profile_PSyDataType\), target, save :: "
                  "profile_psy_data.*"
                  r"call profile_psy_data%PreStart\(\"multi_invoke_psy\", "
                  r"\"invoke_0:r0.*\", 0, 0\).*"
                  "do cell.*"
                  "call.*"
                  "end.*"
                  "do cell.*"
                  "call.*"
                  "end.*"
                  r"call profile_psy_data%PostEnd")
    assert re.search(correct_re, code, re.I) is not None

    # Lastly, test an invoke whose first kernel is a builtin
    _, invoke = get_invoke("15.1.1_X_plus_Y_builtin.f90", "dynamo0.3", idx=0)
    Profiler.add_profile_nodes(invoke.schedule, Loop)
    code = str(invoke.gen())
    assert "USE profile_psy_data_mod, ONLY: profile_PSyDataType" in code
    assert "TYPE(profile_PSyDataType), target, save :: profile_psy_data" \
        in code
    assert "CALL profile_psy_data%PreStart(\"single_invoke_psy\", "\
           "\"invoke_0:x_plus_y:r0\", 0, 0)" in code
    assert "CALL profile_psy_data%PostEnd" in code

    Profiler.set_options(None)


# -----------------------------------------------------------------------------
def test_profile_kernels_dynamo0p3():
    '''Check that all kernels are instrumented correctly in a
    Dynamo 0.3 invoke.
    '''
    Profiler.set_options([Profiler.KERNELS])
    _, invoke = get_invoke("1_single_invoke.f90", "dynamo0.3", idx=0)
    Profiler.add_profile_nodes(invoke.schedule, Loop)

    # Convert the invoke to code, and remove all new lines, to make
    # regex matching easier
    code = str(invoke.gen()).replace("\n", "")

    correct_re = ("subroutine invoke.*"
                  "use profile_psy_data_mod, only: profile_PSyDataType.*"
                  r"TYPE\(profile_PSyDataType\), target, save :: "
                  "profile_psy_data.*"
                  r"call profile_psy_data%PreStart\(\"single_invoke_psy\", "
                  r"\"invoke_0_testkern_type:testkern_code:r0.*\", 0, 0\).*"
                  "do cell.*"
                  "call.*"
                  "end.*"
                  r"call profile_psy_data%PostEnd")
    assert re.search(correct_re, code, re.I) is not None

    _, invoke = get_invoke("1.2_multi_invoke.f90", "dynamo0.3", idx=0)
    Profiler.add_profile_nodes(invoke.schedule, Loop)

    # Convert the invoke to code, and remove all new lines, to make
    # regex matching easier
    code = str(invoke.gen()).replace("\n", "")

    correct_re = ("subroutine invoke.*"
                  "use profile_psy_data_mod, only: profile_PSyDataType.*"
                  r"TYPE\(profile_PSyDataType\), target, save :: "
                  r"(?P<profile2>\w*) .*"
                  r"TYPE\(profile_PSyDataType\), target, save :: "
                  r"(?P<profile1>\w*) .*"
                  r"call (?P=profile1)%PreStart\(\"multi_invoke_psy\", "
                  r"\"invoke_0:testkern_code:r0\", 0, 0\).*"
                  "do cell.*"
                  "call.*"
                  "end.*"
                  r"call (?P=profile1)%PostEnd.*"
                  r"call (?P=profile2)%PreStart\(\"multi_invoke_psy\", "
                  r"\"invoke_0:testkern_code:r1\", 0, 0\).*"
                  "do cell.*"
                  "call.*"
                  "end.*"
                  r"call (?P=profile2)%PostEnd")

    groups = re.search(correct_re, code, re.I)
    assert groups is not None
    # Check that the variables are different
    assert groups.group(1) != groups.group(2)
    Profiler.set_options(None)


# -----------------------------------------------------------------------------
def test_profile_named_dynamo0p3():
    '''Check that the Dynamo 0.3 API is instrumented correctly when the
    profile name is supplied by the user.

    '''
    psy, invoke = get_invoke("1_single_invoke.f90", "dynamo0.3", idx=0)
    schedule = invoke.schedule
    profile_trans = ProfileTrans()
    options = {"region_name": (psy.name, invoke.name)}
    _, _ = profile_trans.apply(schedule.children, options=options)
    result = str(invoke.gen())
    assert ("CALL profile_psy_data%PreStart(\"single_invoke_psy\", "
            "\"invoke_0_testkern_type\", 0, 0)") in result


# -----------------------------------------------------------------------------
def test_transform(capsys):
    '''Tests normal behaviour of profile region transformation.'''

    # pylint: disable=too-many-locals
    _, invoke = get_invoke("test27_loop_swap.f90", "gocean1.0",
                           name="invoke_loop1", dist_mem=False)
    schedule = invoke.schedule
    # This test expects constant loop bounds
    schedule._const_loop_bounds = True

    prt = ProfileTrans()
    assert str(prt) == "Create a sub-tree of the PSyIR that has " \
                       "a node of type ProfileNode at its root."
    assert prt.name == "ProfileTrans"

    # Try applying it to a list
    sched1, _ = prt.apply(schedule.children)

    correct = ("""GOInvokeSchedule[invoke='invoke_loop1', \
Constant loop bounds=True]:
ProfileStart[var=profile_psy_data]
GOLoop[id:'', variable:'j', loop_type:'outer']
Literal[value:'2', Scalar<INTEGER, UNDEFINED>]
Literal[value:'jstop', Scalar<INTEGER, UNDEFINED>]
Literal[value:'1', Scalar<INTEGER, UNDEFINED>]
Schedule:
GOLoop[id:'', variable:'i', loop_type:'inner']
Literal[value:'2', Scalar<INTEGER, UNDEFINED>]
Literal[value:'istop', Scalar<INTEGER, UNDEFINED>]
Literal[value:'1', Scalar<INTEGER, UNDEFINED>]
Schedule:
kern call: bc_ssh_code
End Schedule
End GOLoop
End Schedule
End GOLoop
GOLoop[id:'', variable:'j', loop_type:'outer']
Literal[value:'1', Scalar<INTEGER, UNDEFINED>]
Literal[value:'jstop+1', Scalar<INTEGER, UNDEFINED>]
Literal[value:'1', Scalar<INTEGER, UNDEFINED>]
Schedule:
GOLoop[id:'', variable:'i', loop_type:'inner']
Literal[value:'1', Scalar<INTEGER, UNDEFINED>]
Literal[value:'istop', Scalar<INTEGER, UNDEFINED>]
Literal[value:'1', Scalar<INTEGER, UNDEFINED>]
Schedule:
kern call: bc_solid_u_code
End Schedule
End GOLoop
End Schedule
End GOLoop
GOLoop[id:'', variable:'j', loop_type:'outer']
Literal[value:'1', Scalar<INTEGER, UNDEFINED>]
Literal[value:'jstop', Scalar<INTEGER, UNDEFINED>]
Literal[value:'1', Scalar<INTEGER, UNDEFINED>]
Schedule:
GOLoop[id:'', variable:'i', loop_type:'inner']
Literal[value:'1', Scalar<INTEGER, UNDEFINED>]
Literal[value:'istop+1', Scalar<INTEGER, UNDEFINED>]
Literal[value:'1', Scalar<INTEGER, UNDEFINED>]
Schedule:
kern call: bc_solid_v_code
End Schedule
End GOLoop
End Schedule
End GOLoop
ProfileEnd
End Schedule""")
    assert correct in str(sched1)

    # Now only wrap a single node - the middle loop:
    sched2, _ = prt.apply(schedule[0].profile_body[1])

    correct = ("""GOInvokeSchedule[invoke='invoke_loop1', \
Constant loop bounds=True]:
ProfileStart[var=profile_psy_data]
GOLoop[id:'', variable:'j', loop_type:'outer']
Literal[value:'2', Scalar<INTEGER, UNDEFINED>]
Literal[value:'jstop', Scalar<INTEGER, UNDEFINED>]
Literal[value:'1', Scalar<INTEGER, UNDEFINED>]
Schedule:
GOLoop[id:'', variable:'i', loop_type:'inner']
Literal[value:'2', Scalar<INTEGER, UNDEFINED>]
Literal[value:'istop', Scalar<INTEGER, UNDEFINED>]
Literal[value:'1', Scalar<INTEGER, UNDEFINED>]
Schedule:
kern call: bc_ssh_code
End Schedule
End GOLoop
End Schedule
End GOLoop
ProfileStart[var=profile_psy_data_1]
GOLoop[id:'', variable:'j', loop_type:'outer']
Literal[value:'1', Scalar<INTEGER, UNDEFINED>]
Literal[value:'jstop+1', Scalar<INTEGER, UNDEFINED>]
Literal[value:'1', Scalar<INTEGER, UNDEFINED>]
Schedule:
GOLoop[id:'', variable:'i', loop_type:'inner']
Literal[value:'1', Scalar<INTEGER, UNDEFINED>]
Literal[value:'istop', Scalar<INTEGER, UNDEFINED>]
Literal[value:'1', Scalar<INTEGER, UNDEFINED>]
Schedule:
kern call: bc_solid_u_code
End Schedule
End GOLoop
End Schedule
End GOLoop
ProfileEnd
GOLoop[id:'', variable:'j', loop_type:'outer']
Literal[value:'1', Scalar<INTEGER, UNDEFINED>]
Literal[value:'jstop', Scalar<INTEGER, UNDEFINED>]
Literal[value:'1', Scalar<INTEGER, UNDEFINED>]
Schedule:
GOLoop[id:'', variable:'i', loop_type:'inner']
Literal[value:'1', Scalar<INTEGER, UNDEFINED>]
Literal[value:'istop+1', Scalar<INTEGER, UNDEFINED>]
Literal[value:'1', Scalar<INTEGER, UNDEFINED>]
Schedule:
kern call: bc_solid_v_code
End Schedule
End GOLoop
End Schedule
End GOLoop
ProfileEnd
End Schedule""")
    assert correct in str(sched2)

    # Check that a sublist created from individual elements
    # can be wrapped
    sched3, _ = prt.apply([sched2[0].profile_body[0],
                           sched2[0].profile_body[1]])
    sched3.view()
    out, _ = capsys.readouterr()

    gsched = colored("GOInvokeSchedule", GOInvokeSchedule._colour)
    prof = colored("Profile", ProfileNode._colour)
    sched = colored("Schedule", Schedule._colour)
    loop = colored("Loop", Loop._colour)

    indent = 4*" "
    correct = (gsched+"[invoke='invoke_loop1', Constant loop bounds=True]\n" +
               indent + "0: " + prof + "[]\n" +
               2*indent + sched + "[]\n" +
               3*indent + "0: " + prof + "[]\n" +
               4*indent + sched + "[]\n" +
               5*indent + "0: " + loop + "[type='outer', field_space='go_ct',"
               " it_space='go_internal_pts']\n")
    assert correct in out
    correct2 = (5*indent + "1: " + prof + "[]\n" +
                6*indent + sched + "[]\n" +
                7*indent + "0: " + loop + "[type='outer', field_space='go_cu',"
                " it_space='go_all_pts']\n")
    assert correct2 in out


# -----------------------------------------------------------------------------
def test_transform_errors(capsys):
    '''Tests error handling of the profile region transformation.'''

    # This has been imported and tested before, so we can assume
    # here that this all works as expected/
    _, invoke = get_invoke("test27_loop_swap.f90", "gocean1.0",
                           name="invoke_loop1", dist_mem=False)

    schedule = invoke.schedule
    prt = ProfileTrans()

    # Just to be sure: also check that the right order does indeed work!
    sched1, _ = prt.apply([schedule.children[0],
                           schedule.children[1],
                           schedule.children[2]])
    sched1.view()
    out, _ = capsys.readouterr()
    # out is unicode, and has no replace function, so convert to string first
    out = str(out).replace("\n", "")

    correct_re = (".*GOInvokeSchedule.*?"
                  r"Profile.*?"
                  r"Loop.*\[type='outer'.*?"
                  r"Loop.*\[type='outer'.*?"
                  r"Loop.*\[type='outer'")
    assert re.search(correct_re, out)

    # Test that we don't add a profile node inside a OMP do loop (which
    # would be invalid syntax):
    _, invoke = get_invoke("test27_loop_swap.f90", "gocean1.0",
                           name="invoke_loop1", dist_mem=False)
    schedule = invoke.schedule

    prt = ProfileTrans()
    omp_loop = GOceanOMPLoopTrans()

    # Parallelise the first loop:
    sched1, _ = omp_loop.apply(schedule[0])

    # Inserting a ProfileTrans inside a omp do loop is syntactically
    # incorrect, the inner part must be a do loop only:
    with pytest.raises(TransformationError) as excinfo:
        prt.apply(sched1[0].dir_body[0])

    assert "A PSyData node cannot be inserted between an OpenMP/ACC "\
           "directive and the loop(s) to which it applies!" \
           in str(excinfo.value)

    with pytest.raises(TransformationError) as excinfo:
        prt.apply(sched1[0], {"region_name": "xx"})
    assert "Error in ProfileTrans. User-supplied region name must be a " \
        "tuple containing two non-empty strings" in str(excinfo.value)


# -----------------------------------------------------------------------------
def test_region():
    ''' Tests that the profiling transform works correctly when a region of
    code is specified that does not cover the full invoke and also
    contains multiple kernels.

    '''
    _, invoke = get_invoke("3.1_multi_functions_multi_invokes.f90",
                           "dynamo0.3", name="invoke_0", dist_mem=True)
    schedule = invoke.schedule
    prt = ProfileTrans()
    # Just halo exchanges.
    _ = prt.apply(schedule[0:4])
    # Two loops.
    _ = prt.apply(schedule[1:3])
    result = str(invoke.gen())
    assert ("CALL profile_psy_data%PreStart(\"multi_functions_multi_invokes_"
            "psy\", \"invoke_0:r0\", 0, 0)" in result)
    assert ("CALL profile_psy_data_1%PreStart(\"multi_functions_multi_"
            "invokes_psy\", \"invoke_0:r1\", 0, 0)" in result)
    # Make nested profiles.
    _ = prt.apply(schedule[1].profile_body[1])
    _ = prt.apply(schedule)
    result = str(invoke.gen())
    assert ("CALL profile_psy_data_3%PreStart(\"multi_functions_multi_"
            "invokes_psy\", \"invoke_0:r0\", 0, 0)" in result)
    assert ("CALL profile_psy_data%PreStart(\"multi_functions_multi_"
            "invokes_psy\", \"invoke_0:r1\", 0, 0)" in result)
    assert ("CALL profile_psy_data_1%PreStart(\"multi_functions_multi_"
            "invokes_psy\", \"invoke_0:r2\", 0, 0)" in result)
    assert ("CALL profile_psy_data_2%PreStart(\"multi_functions_multi_"
            "invokes_psy\", \"invoke_0:testkern_code:r3\", 0, 0)" in result)


# -----------------------------------------------------------------------------
def test_omp_transform():
    '''Tests that the profiling transform works correctly with OMP
     parallelisation.'''

    _, invoke = get_invoke("test27_loop_swap.f90", "gocean1.0",
                           name="invoke_loop1", dist_mem=False)
    schedule = invoke.schedule
    # This test expects constant loop bounds
    schedule._const_loop_bounds = True

    prt = ProfileTrans()
    omp_loop = GOceanOMPLoopTrans()
    omp_par = OMPParallelTrans()

    # Parallelise the first loop:
    sched1, _ = omp_loop.apply(schedule[0])
    sched2, _ = omp_par.apply(sched1[0])
    sched3, _ = prt.apply(sched2[0])

    correct = (
        "      CALL profile_psy_data%PreStart(\"psy_test27_loop_swap\", "
        "\"invoke_loop1:bc_ssh_code:r0\", 0, 0)\n"
        "      !$omp parallel default(shared), private(i,j)\n"
        "      !$omp do schedule(static)\n"
        "      DO j=2,jstop\n"
        "        DO i=2,istop\n"
        "          CALL bc_ssh_code(i, j, 1, t%data, t%grid%tmask)\n"
        "        END DO\n"
        "      END DO\n"
        "      !$omp end do\n"
        "      !$omp end parallel\n"
        "      CALL profile_psy_data%PostEnd")
    code = str(invoke.gen())
    assert correct in code

    # Now add another profile node between the omp parallel and omp do
    # directives:
    sched3, _ = prt.apply(sched3[0].profile_body[0].dir_body[0])

    code = str(invoke.gen())

    correct = \
        "CALL profile_psy_data%PreStart(\"psy_test27_loop_swap\", " + \
        '''"invoke_loop1:bc_ssh_code:r0", 0, 0)
      !$omp parallel default(shared), private(i,j)
      CALL profile_psy_data_1%PreStart("psy_test27_loop_swap", ''' + \
        '''"invoke_loop1:bc_ssh_code:r1", 0, 0)
      !$omp do schedule(static)
      DO j=2,jstop
        DO i=2,istop
          CALL bc_ssh_code(i, j, 1, t%data, t%grid%tmask)
        END DO
      END DO
      !$omp end do
      CALL profile_psy_data_1%PostEnd
      !$omp end parallel
      CALL profile_psy_data%PostEnd'''

    assert correct in code


def test_auto_invoke_return_last_stmt(parser):
    ''' Check that using the auto-invoke profiling option avoids including
    a return statement within the profiling region if it is the last statement
    in the routine. '''
    symbol_table = SymbolTable()
    arg1 = symbol_table.new_symbol(
        symbol_type=DataSymbol, datatype=REAL_TYPE)
    zero = Literal("0.0", REAL_TYPE)
    assign1 = Assignment.create(Reference(arg1), zero)
    kschedule = KernelSchedule.create(
        "work", symbol_table, [assign1, Return()])
    # Double-check that the tree is as we expect
    assert isinstance(kschedule[-1], Return)

    Profiler.set_options([Profiler.INVOKES])
    Profiler.add_profile_nodes(kschedule, Loop)
    # The Return should be a sibling of the ProfileNode rather than a child
    assert isinstance(kschedule[0], ProfileNode)
    assert isinstance(kschedule[0].children[0].children[0], Assignment)
    assert isinstance(kschedule[1], Return)


def test_auto_invoke_no_return(capsys):
    ''' Check that using the auto-invoke profiling option does not add any
    profiling if the invoke contains a Return anywhere other than as the
    last statement. '''
    Profiler.set_options([Profiler.INVOKES])
    symbol_table = SymbolTable()
    arg1 = symbol_table.new_symbol(
        symbol_type=DataSymbol, datatype=REAL_TYPE)
    zero = Literal("0.0", REAL_TYPE)
    assign1 = Assignment.create(Reference(arg1), zero)
    assign2 = Assignment.create(Reference(arg1), zero.copy())

    # Create Schedule with Return at the start.
    kschedule = KernelSchedule.create(
        "work1", symbol_table, [Return(), assign1, assign2])
    Profiler.add_profile_nodes(kschedule, Loop)
    # No profiling should have been added
    assert not kschedule.walk(ProfileNode)
    _, err = capsys.readouterr()
    assert ("Not adding profiling to routine 'work1' because it contains one "
            "or more Return statements" in err)

    # Create Schedule with Return in the middle.
    kschedule = KernelSchedule.create(
        "work2", symbol_table, [assign1, Return(), assign2])
    Profiler.add_profile_nodes(kschedule, Loop)
    # No profiling should have been added
    assert not kschedule.walk(ProfileNode)
    _, err = capsys.readouterr()
    assert ("Not adding profiling to routine 'work2' because it contains one "
            "or more Return statements" in err)

    # Create Schedule with a Return at the end as well as in the middle.
    kschedule = KernelSchedule.create(
        "work3", symbol_table, [assign1, Return(), assign2, Return()])
    Profiler.add_profile_nodes(kschedule, Loop)
    # No profiling should have been added
    assert not kschedule.walk(ProfileNode)
    _, err = capsys.readouterr()
    assert ("Not adding profiling to routine 'work3' because it contains one "
            "or more Return statements" in err)
