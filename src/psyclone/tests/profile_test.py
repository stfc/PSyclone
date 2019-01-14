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

''' Module containing tests for generating monitoring hooks'''

from __future__ import absolute_import

import re
import pytest

from psyclone.generator import GenerationError
from psyclone.gocean1p0 import GOKern, GOSchedule
from psyclone.profiler import Profiler, ProfileNode
from psyclone.psyGen import Loop, NameSpace
from psyclone.transformations import GOceanOMPLoopTrans, OMPParallelTrans, \
    ProfileRegionTrans, TransformationError
from psyclone_test_utils import get_invoke


# -----------------------------------------------------------------------------
def teardown_function():
    '''This function is called at the end of any test function. It disables
    any automatic profiling set. This is necessary in case of a test failure
    to make sure any further tests will not be ran with profiling enabled.
    It also creates a new NameSpace manager, which is responsible to create
    unique region names - this makes sure the test works if the order or
    number of tests run is changed, otherwise the created region names will
    change.
    '''
    Profiler.set_options([])
    # pylint: disable=protected-access
    Profiler._namespace = NameSpace()


# -----------------------------------------------------------------------------
def test_profile_basic(capsys):
    '''Check basic functionality: node names, schedule view.
    '''
    Profiler.set_options([Profiler.INVOKES])
    _, invoke = get_invoke("test11_different_iterates_over_one_invoke.f90",
                           "gocean1.0", idx=0)

    assert isinstance(invoke.schedule.children[0], ProfileNode)

    invoke.schedule.view()
    out, _ = capsys.readouterr()

    coloured_schedule = GOSchedule([]).coloured_text
    coloured_loop = Loop().coloured_text
    coloured_kern = GOKern().coloured_text
    coloured_profile = invoke.schedule.children[0].coloured_text

    # Do one test based on schedule view, to make sure colouring
    # and indentation is correct
    correct = (
        '''{0}[invoke='invoke_0',Constant loop bounds=True]
    {3}
        {1}[type='outer',field_space='go_cv',it_space='go_internal_pts']
            {1}[type='inner',field_space='go_cv',it_space='go_internal_pts']
                {2} compute_cv_code(cv_fld,p_fld,v_fld) '''
        '''[module_inline=False]
        {1}[type='outer',field_space='go_ct',it_space='go_all_pts']
            {1}[type='inner',field_space='go_ct',it_space='go_all_pts']
                {2} bc_ssh_code(ncycle,p_fld,tmask) '''
        '''[module_inline=False]'''.format(coloured_schedule, coloured_loop,
                                           coloured_kern, coloured_profile)
    )
    assert correct in out

    prt = ProfileRegionTrans()

    # Insert a profile call between outer and inner loop.
    # This tests that we find the subroutine node even
    # if it is not the immediate parent.
    new_sched, _ = prt.apply(invoke.schedule.children[0]
                             .children[0].children[0])

    new_sched_str = str(new_sched)

    correct = ("""GOSchedule(Constant loop bounds=True):
ProfileStart[var=profile]
Loop[]: j= lower=2,jstop-1,1
ProfileStart[var=profile_1]
Loop[]: i= lower=2,istop,1
kern call: compute_cv_code
EndLoop
ProfileEnd
EndLoop
Loop[]: j= lower=1,jstop+1,1
Loop[]: i= lower=1,istop+1,1
kern call: bc_ssh_code
EndLoop
EndLoop
ProfileEnd
End Schedule""")

    assert correct in new_sched_str

    Profiler.set_options(None)


# -----------------------------------------------------------------------------
def test_profile_errors2():
    '''Test various error handling.'''

    with pytest.raises(GenerationError) as gen_error:
        Profiler.set_options(["invalid"])
    assert "options must be one of 'invokes', 'kernels'" in str(gen_error)


# -----------------------------------------------------------------------------
def test_profile_invokes_gocean1p0():
    '''Check that an invoke is instrumented correctly
    '''
    Profiler.set_options([Profiler.INVOKES])
    _, invoke = get_invoke("test11_different_iterates_over_one_invoke.f90",
                           "gocean1.0", idx=0)

    # Convert the invoke to code, and remove all new lines, to make
    # regex matching easier
    code = str(invoke.gen()).replace("\n", "")

    # First a simple test that the nesting is correct - the
    # profile regions include both loops. Note that indeed
    # the function 'compute_cv_code' is in the module file
    # kernel_ne_offset_mod.
    correct_re = ("subroutine invoke.*"
                  "use profile_mod, only: ProfileData.*"
                  r"TYPE\(ProfileData\), save :: profile.*"
                  r"call ProfileStart\(\"kernel_ne_offset_mod\", "
                  r"\"compute_cv_code\", profile\).*"
                  "do j.*"
                  "do i.*"
                  "call.*"
                  "end.*"
                  "end.*"
                  r"call ProfileEnd\(profile\)")
    assert re.search(correct_re, code, re.I) is not None

    # Check that if gen() is called more than once the same profile
    # variables and region names are created:
    code_again = str(invoke.gen()).replace("\n", "")
    assert code == code_again

    # Test that two kernels in one invoke get instrumented correctly.
    _, invoke = get_invoke("single_invoke_two_kernels.f90", "gocean1.0", 0)

    # Convert the invoke to code, and remove all new lines, to make
    # regex matching easier
    code = str(invoke.gen()).replace("\n", "")

    correct_re = ("subroutine invoke.*"
                  "use profile_mod, only: ProfileData.*"
                  r"TYPE\(ProfileData\), save :: profile.*"
                  r"call ProfileStart\(\"compute_cu_mod\", "
                  r"\"compute_cu_code\", profile\).*"
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
                  r"call ProfileEnd\(profile\)")
    assert re.search(correct_re, code, re.I) is not None
    Profiler.set_options(None)


# -----------------------------------------------------------------------------
def test_unique_region_names():
    '''Test that unique region names are created even when the kernel
    names are identical.'''

    Profiler.set_options([Profiler.KERNELS])
    _, invoke = get_invoke("single_invoke_two_identical_kernels.f90",
                           "gocean1.0", 0)

    # Convert the invoke to code, and remove all new lines, to make
    # regex matching easier

    code = str(invoke.gen()).replace("\n", "")

    # This regular expression puts the region names into groups.
    # Make sure even though the kernels have the same name, that
    # the created regions have different names. In order to be
    # flexible for future changes, we get the region names from
    # the ProfileStart calls using a regular expressions (\w*
    # being the group name enclosed in "") group. Python will store
    # those two groups and they can be accessed using the resulting
    # re object.group(n).
    correct_re = ("subroutine invoke.*"
                  "use profile_mod, only: ProfileData.*"
                  r"TYPE\(ProfileData\), save :: profile.*"
                  r"TYPE\(ProfileData\), save :: profile.*"
                  r"call ProfileStart\(\"compute_cu_mod\", \"(\w*)\", "
                  r"profile\).*"
                  "do j.*"
                  "do i.*"
                  "call compute_cu_code.*"
                  "end.*"
                  "end.*"
                  r"call ProfileEnd\(profile\).*"
                  r"call ProfileStart\(\"compute_cu_mod\", \"(\w*)\", "
                  r"profile_1\).*"
                  "do j.*"
                  "do i.*"
                  "call compute_cu_code.*"
                  "end.*"
                  "end.*"
                  r"call ProfileEnd\(profile_1\)")

    groups = re.search(correct_re, code, re.I)
    assert groups is not None

    # Check that the region names are indeed different: group(1)
    # is the first kernel region name crated by PSyclone, and
    # group(2) the name used in the second ProfileStart.
    # Those names must be different (otherwise the profiling tool
    # would likely combine the two different regions into one).
    assert groups.group(1) != groups.group(2)


# -----------------------------------------------------------------------------
def test_profile_kernels_gocean1p0():
    '''Check that all kernels are instrumented correctly
    '''
    Profiler.set_options([Profiler.KERNELS])
    _, invoke = get_invoke("single_invoke_two_kernels.f90", "gocean1.0",
                           idx=0)

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
                  "use profile_mod, only: ProfileData.*"
                  r"TYPE\(ProfileData\), save :: profile.*"
                  r"TYPE\(ProfileData\), save :: profile.*"
                  r"call ProfileStart\(\"compute_cu_mod\", "
                  r"\"compute_cu_code.*\", (?P<profile1>\w*)\).*"
                  "do j.*"
                  "do i.*"
                  "call.*"
                  "end.*"
                  "end.*"
                  r"call ProfileEnd\((?P=profile1)\).*"
                  r"call ProfileStart\(\"time_smooth_mod\", "
                  r"\"time_smooth_code\", (?P<profile2>\w*)\).*"
                  "do j.*"
                  "do i.*"
                  "call.*"
                  "end.*"
                  "end.*"
                  r"call ProfileEnd\((?P=profile2)\)")
    groups = re.search(correct_re, code, re.I)
    assert groups is not None
    # Check that the variables are different
    assert groups.group(1) != groups.group(2)

    Profiler.set_options(None)


# -----------------------------------------------------------------------------
def test_profile_invokes_dynamo0p3():
    '''Check that a Dynamo 0.3 invoke is instrumented correctly
    '''
    Profiler.set_options([Profiler.INVOKES])

    # First test for a single invoke with a single kernel work as expected:
    _, invoke = get_invoke("1_single_invoke.f90", "dynamo0.3", idx=0)

    # Convert the invoke to code, and remove all new lines, to make
    # regex matching easier
    code = str(invoke.gen()).replace("\n", "")

    correct_re = ("subroutine invoke.*"
                  "use profile_mod, only: ProfileData.*"
                  r"TYPE\(ProfileData\), save :: profile.*"
                  r"call ProfileStart\(\"testkern\", \"testkern_code\", "
                  r"profile\).*"
                  "do cell.*"
                  "call.*"
                  "end.*"
                  r"call ProfileEnd\(profile\)")
    assert re.search(correct_re, code, re.I) is not None

    # Next test two kernels in one invoke:
    _, invoke = get_invoke("1.2_multi_invoke.f90", "dynamo0.3", idx=0)

    # Convert the invoke to code, and remove all new lines, to make
    # regex matching easier
    code = str(invoke.gen()).replace("\n", "")

    # The .* after testkern_code is necessary since the name can be changed
    # by PSyclone to avoid name duplications.
    correct_re = ("subroutine invoke.*"
                  "use profile_mod, only: ProfileData.*"
                  r"TYPE\(ProfileData\), save :: profile.*"
                  r"call ProfileStart\(\"testkern\", \"testkern_code.*\","
                  r" profile\).*"
                  "do cell.*"
                  "call.*"
                  "end.*"
                  "do cell.*"
                  "call.*"
                  "end.*"
                  r"call ProfileEnd\(profile\)")
    assert re.search(correct_re, code, re.I) is not None
    Profiler.set_options(None)


# -----------------------------------------------------------------------------
def test_profile_kernels_dynamo0p3():
    '''Check that all kernels are instrumented correctly in a
    Dynamo 0.3 invoke.
    '''
    Profiler.set_options([Profiler.KERNELS])
    _, invoke = get_invoke("1_single_invoke.f90", "dynamo0.3", idx=0)

    # Convert the invoke to code, and remove all new lines, to make
    # regex matching easier
    code = str(invoke.gen()).replace("\n", "")

    correct_re = ("subroutine invoke.*"
                  "use profile_mod, only: ProfileData, ProfileStart, "
                  "ProfileEnd.*"
                  r"TYPE\(ProfileData\), save :: profile.*"
                  r"call ProfileStart\(\"testkern\", \"testkern_code.*\", "
                  r"profile\).*"
                  "do cell.*"
                  "call.*"
                  "end.*"
                  r"call ProfileEnd\(profile\)")
    assert re.search(correct_re, code, re.I) is not None

    _, invoke = get_invoke("1.2_multi_invoke.f90", "dynamo0.3", idx=0)

    # Convert the invoke to code, and remove all new lines, to make
    # regex matching easier
    code = str(invoke.gen()).replace("\n", "")

    correct_re = ("subroutine invoke.*"
                  "use profile_mod, only: ProfileData, ProfileStart, "
                  "ProfileEnd.*"
                  r"TYPE\(ProfileData\), save :: profile.*"
                  r"TYPE\(ProfileData\), save :: profile.*"
                  r"call ProfileStart\(\"testkern\", \"testkern_code.*\", "
                  r"(?P<profile1>\w*)\).*"
                  "do cell.*"
                  "call.*"
                  "end.*"
                  r"call ProfileEnd\((?P=profile1)\).*"
                  r"call ProfileStart\(.*, (?P<profile2>\w*)\).*"
                  "do cell.*"
                  "call.*"
                  "end.*"
                  r"call ProfileEnd\((?P=profile2)\).*")
    groups = re.search(correct_re, code, re.I)
    assert groups is not None
    # Check that the variables are different
    assert groups.group(1) != groups.group(2)
    Profiler.set_options(None)


# -----------------------------------------------------------------------------
def test_transform(capsys):
    '''Tests normal behaviour of profile region transformation.'''

    _, invoke = get_invoke("test27_loop_swap.f90", "gocean1.0",
                           name="invoke_loop1")
    schedule = invoke.schedule

    prt = ProfileRegionTrans()
    assert str(prt) == "Insert a profile start and end call."
    assert prt.name == "ProfileRegionTrans"

    # Try applying it to a list
    sched1, _ = prt.apply(schedule.children)

    correct = ("""OSchedule(Constant loop bounds=True):
ProfileStart[var=profile]
Loop[]: j= lower=2,jstop,1
Loop[]: i= lower=2,istop,1
kern call: bc_ssh_code
EndLoop
EndLoop
Loop[]: j= lower=1,jstop+1,1
Loop[]: i= lower=1,istop,1
kern call: bc_solid_u_code
EndLoop
EndLoop
Loop[]: j= lower=1,jstop,1
Loop[]: i= lower=1,istop+1,1
kern call: bc_solid_v_code
EndLoop
EndLoop
ProfileEnd
End Schedule""")

    assert correct in str(sched1)

    # Now only wrap a single node - the middle loop:
    sched2, _ = prt.apply(schedule.children[0].children[1])

    correct = ("""GOSchedule(Constant loop bounds=True):
ProfileStart[var=profile]
Loop[]: j= lower=2,jstop,1
Loop[]: i= lower=2,istop,1
kern call: bc_ssh_code
EndLoop
EndLoop
ProfileStart[var=profile_1]
Loop[]: j= lower=1,jstop+1,1
Loop[]: i= lower=1,istop,1
kern call: bc_solid_u_code
EndLoop
EndLoop
ProfileEnd
Loop[]: j= lower=1,jstop,1
Loop[]: i= lower=1,istop+1,1
kern call: bc_solid_v_code
EndLoop
EndLoop
ProfileEnd
End Schedule""")
    assert correct in str(sched2)

    # Check that an sublist created from individual elements
    # can be wrapped
    sched3, _ = prt.apply([sched2.children[0].children[0],
                           sched2.children[0].children[1]])
    sched3.view()
    out, _ = capsys.readouterr()  # .replace("\n", "")
    # out is unicode, and has no replace function, so convert to string first
    out = str(out).replace("\n", "")
    correct_re = (".*GOSchedule.*"
                  r"    .*Profile.*"
                  r"        .*Profile.*"
                  r"            .*Loop.*\[type='outer'.*"
                  r"            .*Profile.*"
                  r"                .*Loop.*\[type='outer'.*"
                  r"        .*Loop.*\[type='outer'.*")
    assert re.search(correct_re, out)


# -----------------------------------------------------------------------------
def test_transform_errors(capsys):
    '''Tests error handling of the profile region transformation.'''

    # This has been imported and tested before, so we can assume
    # here that this all works as expected/
    _, invoke = get_invoke("test27_loop_swap.f90", "gocean1.0",
                           name="invoke_loop1")

    schedule = invoke.schedule
    prt = ProfileRegionTrans()

    with pytest.raises(TransformationError) as excinfo:
        prt.apply([schedule.children[0].children[0], schedule.children[1]])
    assert "supplied nodes are not children of the same Schedule/parent." \
           in str(excinfo)

    # Supply not a node object:
    with pytest.raises(TransformationError) as excinfo:
        prt.apply(5)
    assert "Argument must be a single Node in a schedule or a list of Nodes " \
           "in a schedule but have been passed an object of type: " \
           in str(excinfo)
    # Python 3 reports 'class', python 2 'type' - so just check for both
    assert "<type 'int'>" in str(excinfo) or "<class 'int'>" in str(excinfo)

    # Test that it will only allow correctly ordered nodes:
    with pytest.raises(TransformationError) as excinfo:
        sched1, _ = prt.apply([schedule.children[1], schedule.children[0]])
    assert "Children are not consecutive children of one parent:" \
           in str(excinfo)

    with pytest.raises(TransformationError) as excinfo:
        sched1, _ = prt.apply([schedule.children[0], schedule.children[2]])
    assert "Children are not consecutive children of one parent:" \
           in str(excinfo)

    # Test 3 element lists: first various incorrect ordering:
    with pytest.raises(TransformationError) as excinfo:
        sched1, _ = prt.apply([schedule.children[0],
                               schedule.children[2],
                               schedule.children[1]])
    assert "Children are not consecutive children of one parent:" \
           in str(excinfo)

    with pytest.raises(TransformationError) as excinfo:
        sched1, _ = prt.apply([schedule.children[1],
                               schedule.children[0],
                               schedule.children[2]])
    assert "Children are not consecutive children of one parent:" \
           in str(excinfo)

    # Just to be sure: also check that the right order does indeed work!
    sched1, _ = prt.apply([schedule.children[0],
                           schedule.children[1],
                           schedule.children[2]])
    sched1.view()
    out, _ = capsys.readouterr()
    # out is unicode, and has no replace function, so convert to string first
    out = str(out).replace("\n", "")

    correct_re = (".*GOSchedule.*"
                  r"    .*Profile.*"
                  r"        .*Loop.*\[type='outer'.*"
                  r"        .*Loop.*\[type='outer'.*"
                  r"        .*Loop.*\[type='outer'.*")
    assert re.search(correct_re, out)

    # Test that we don't add a profile node inside a OMP do loop (which
    # would be invalid syntax):
    _, invoke = get_invoke("test27_loop_swap.f90", "gocean1.0",
                           name="invoke_loop1")
    schedule = invoke.schedule

    prt = ProfileRegionTrans()
    omp_loop = GOceanOMPLoopTrans()

    # Parallelise the first loop:
    sched1, _ = omp_loop.apply(schedule.children[0])

    # Inserting a ProfileRegion inside a omp do loop is syntactically
    # incorrect, the inner part must be a do loop only:
    with pytest.raises(TransformationError) as excinfo:
        prt.apply(sched1.children[0].children[0])

    assert "A ProfileNode cannot be inserted between an OpenMP/ACC directive "\
           "and the loop(s) to which it applies!" in str(excinfo)


# -----------------------------------------------------------------------------
def test_omp_transform():
    '''Tests that the profiling transform works correctly with OMP
     parallelisation.'''

    _, invoke = get_invoke("test27_loop_swap.f90", "gocean1.0",
                           name="invoke_loop1")
    schedule = invoke.schedule

    prt = ProfileRegionTrans()
    omp_loop = GOceanOMPLoopTrans()
    omp_par = OMPParallelTrans()

    # Parallelise the first loop:
    sched1, _ = omp_loop.apply(schedule.children[0])
    sched2, _ = omp_par.apply(sched1.children[0])
    sched3, _ = prt.apply(sched2.children[0])

    correct = (
        "      CALL ProfileStart(\"boundary_conditions_ne_offset_mod\", "
        "\"bc_ssh_code\", profile)\n"
        "      !$omp parallel default(shared), private(j,i)\n"
        "      !$omp do schedule(static)\n"
        "      DO j=2,jstop\n"
        "        DO i=2,istop\n"
        "          CALL bc_ssh_code(i, j, 1, t%data, t%grid%tmask)\n"
        "        END DO \n"
        "      END DO \n"
        "      !$omp end do\n"
        "      !$omp end parallel\n"
        "      CALL ProfileEnd(profile)")
    code = str(invoke.gen())
    assert correct in code

    # Now add another profile node between the omp parallel and omp do
    # directives:
    sched3, _ = prt.apply(sched3.children[0].children[0].children[0])

    code = str(invoke.gen())

    correct = '''      CALL ProfileStart("boundary_conditions_ne_offset_mod", \
"bc_ssh_code", profile)
      !$omp parallel default(shared), private(j,i)
      CALL ProfileStart("boundary_conditions_ne_offset_mod", "bc_ssh_code_1", \
profile_1)
      !$omp do schedule(static)
      DO j=2,jstop
        DO i=2,istop
          CALL bc_ssh_code(i, j, 1, t%data, t%grid%tmask)
        END DO\x20
      END DO\x20
      !$omp end do
      CALL ProfileEnd(profile_1)
      !$omp end parallel
      CALL ProfileEnd(profile)'''
    assert correct in code
