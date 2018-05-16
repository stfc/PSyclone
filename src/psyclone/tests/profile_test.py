# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 018, Science and Technology Facilities Council
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

import os
import re
import pytest

from psyclone.generator import GenerationError
from psyclone.parse import parse
from psyclone.profiler import Profiler
from psyclone.psyGen import PSyFactory
from psyclone.transformations import ProfileRegionTrans, TransformationError


def get_invoke(api, algfile, key):
    ''' Utility method to get the idx'th invoke from the algorithm
    specified in file '''

    if api == "gocean1.0":
        dir_name = "gocean1p0"
    elif api == "dynamo0.3":
        dir_name = "dynamo0p3"
    else:
        assert False
    _, info = parse(os.path.
                    join(os.path.dirname(os.path.abspath(__file__)),
                         "test_files", dir_name, algfile),
                    api=api)
    psy = PSyFactory(api).create(info)
    invokes = psy.invokes
    if isinstance(key, str):
        invoke = invokes.get(key)
    else:
        # invokes does not have a method by which to request the i'th
        # in the list so we do this rather clumsy lookup of the name
        # of the invoke that we want
        invoke = invokes.get(invokes.names[key])
    return psy, invoke


# -----------------------------------------------------------------------------
def test_profile_basic(capsys):
    '''Check basic functionality: node names, schedule view.
    '''
    Profiler.set_options([Profiler.INVOKES])
    _, invoke = get_invoke("gocean1.0", "test11_different_iterates_over_"
                           "one_invoke.f90", 0)

    assert str(invoke.schedule.children[0]) == "Profile"

    invoke.schedule.view()
    out, _ = capsys.readouterr()

    correct = (
        '''GOSchedule[invoke='invoke_0',Constant loop bounds=True]
    [Profile]
        Loop[type='outer',field_space='cv',it_space='internal_pts']
            Loop[type='inner',field_space='cv',it_space='internal_pts']
                KernCall compute_cv_code(cv_fld,p_fld,v_fld) '''
        '''[module_inline=False]
        Loop[type='outer',field_space='ct',it_space='all_pts']
            Loop[type='inner',field_space='ct',it_space='all_pts']
                KernCall bc_ssh_code(ncycle,p_fld,tmask) '''
        '''[module_inline=False]'''
    )
    assert correct in out

    prt = ProfileRegionTrans()

    # Insert a profile call between outer and inner loop.
    # This forces the profile node to loop up in the tree
    # to find the subroutine node (i.e. we are testing
    # the while loop in the ProfileNode).
    new_sched, _ = prt.apply(invoke.schedule.children[0]
                             .children[0].children[0])

    new_sched.view()
    out, _ = capsys.readouterr()

    correct = (
        '''GOSchedule[invoke='invoke_0',Constant loop bounds=True]
    [Profile]
        Loop[type='outer',field_space='cv',it_space='internal_pts']
            [Profile]
                Loop[type='inner',field_space='cv',it_space='internal_pts']
                    KernCall compute_cv_code(cv_fld,p_fld,v_fld) '''
        '''[module_inline=False]
        Loop[type='outer',field_space='ct',it_space='all_pts']
            Loop[type='inner',field_space='ct',it_space='all_pts']
                KernCall bc_ssh_code(ncycle,p_fld,tmask) '''
        '''[module_inline=False]'''
    )
    assert correct in out

    # ... but only if we do call the actual invoke now - but no need
    # to test the result.
    invoke.gen()

    Profiler.set_options(None)


# -----------------------------------------------------------------------------
def test_module_name_not_found():
    '''Test that the profile node handles a mnissing module node
    correctly - by using the name "unknown module". I could not create
    this error with normal code. So instead I create an artificial tree
    that contains a subroutine, which does not have a module as parent,
    and pass this on directly to profile's gen_code. Then the children
    of that dummy subroutine are checked for the correct ProfileStart
    call with 'unknown module' as parameter:

    '''

    Profiler.set_options([Profiler.INVOKES])
    _, invoke = get_invoke("gocean1.0", "test11_different_iterates_over_"
                           "one_invoke.f90", 0)
    schedule = invoke.schedule

    assert str(schedule.children[0]) == "Profile"

    from psyclone.f2pygen import CallGen, ModuleGen, SubroutineGen

    # You need a parent in order to create a subroutine, so first
    # create a dummy module"
    module = ModuleGen(name="test")
    subroutine = SubroutineGen(module, name="test")
    # And then set the parent of the subroutine (atm the module object)
    # to none:
    subroutine._parent = None

    # This call should not find a module object
    schedule.children[0].gen_code(subroutine)

    # Now one of the children should contain a call to ProfileStart
    # with 'unknown module' as parameter:
    found = False
    for i in subroutine.children:
        if isinstance(i, CallGen) and i._call.designator == "ProfileStart" \
                and i._call.items[0] == "\"unknown module\"":
            found = True

    assert found

    Profiler.set_options(None)


# -----------------------------------------------------------------------------
def test_profile_errors2():
    '''Test various error handling.'''
    Profiler.set_options([Profiler.INVOKES])
    _, invoke = get_invoke("gocean1.0", "test11_different_iterates_over_"
                           "one_invoke.f90", 0)
    schedule = invoke.schedule

    assert str(schedule.children[0]) == "Profile"

    # Raise an error if no subroutine object is found:
    with pytest.raises(GenerationError):
        schedule.children[0].gen_code(None)


# -----------------------------------------------------------------------------
def test_profile_invokes_gocean1p0():
    '''Check that an invoke is instrumented correctly
    '''
    Profiler.set_options([Profiler.INVOKES])
    _, invoke = get_invoke("gocean1.0", "test11_different_iterates_over_"
                           "one_invoke.f90", 0)

    # Conver the invoke to code, and remove all new lines, to make
    # regex matching easier
    code = str(invoke.gen()).replace("\n", "")

    correct_re = ("subroutine invoke.*"
                  "use profile_mod, only: ProfileData.*"
                  r"TYPE\(ProfileData\), save :: profile.*"
                  "call ProfileStart.*"
                  "do j.*"
                  "do i.*"
                  "call.*"
                  "end.*"
                  "end.*"
                  "call ProfileEnd")
    assert re.search(correct_re, code, re.I) is not None

    _, invoke = get_invoke("gocean1.0", "single_invoke_"
                           "two_kernels.f90", 0)

    # Conver the invoke to code, and remove all new lines, to make
    # regex matching easier
    code = str(invoke.gen()).replace("\n", "")

    correct_re = ("subroutine invoke.*"
                  "use profile_mod, only: ProfileData.*"
                  r"TYPE\(ProfileData\), save :: profile.*"
                  "call ProfileStart.*"
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
                  "call ProfileEnd")
    assert re.search(correct_re, code, re.I) is not None
    Profiler.set_options(None)


# -----------------------------------------------------------------------------
def test_profile_kernels_gocean1p0():
    '''Check that all kernels are instrumented correctly
    '''
    Profiler.set_options([Profiler.KERNELS])
    _, invoke = get_invoke("gocean1.0", "test11_different_iterates_over_"
                           "one_invoke.f90", 0)

    # Conver the invoke to code, and remove all new lines, to make
    # regex matching easier
    code = str(invoke.gen()).replace("\n", "")

    correct_re = ("subroutine invoke.*"
                  "use profile_mod, only: ProfileData.*"
                  r"TYPE\(ProfileData\), save :: profile.*"
                  "call ProfileStart.*"
                  "do j.*"
                  "do i.*"
                  "call.*"
                  "end.*"
                  "end.*"
                  "call ProfileEnd")
    assert re.search(correct_re, code, re.I) is not None

    _, invoke = get_invoke("gocean1.0", "single_invoke_"
                           "two_kernels.f90", 0)

    # Conver the invoke to code, and remove all new lines, to make
    # regex matching easier
    code = str(invoke.gen()).replace("\n", "")

    correct_re = ("subroutine invoke.*"
                  "use profile_mod, only: ProfileData.*"
                  r"TYPE\(ProfileData\), save :: profile.*"
                  r"TYPE\(ProfileData\), save :: profile.*"
                  r"call ProfileStart\(.*, (?P<profile1>\w*)\).*"
                  "do j.*"
                  "do i.*"
                  "call.*"
                  "end.*"
                  "end.*"
                  r"call ProfileEnd\((?P=profile1)\).*"
                  r"call ProfileStart\(.*, (?P<profile2>\w*)\).*"
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
    '''Check that an invoke is instrumented correctly
    '''
    Profiler.set_options([Profiler.INVOKES])
    _, invoke = get_invoke("dynamo0.3", "1_single_invoke.f90", 0)

    # Conver the invoke to code, and remove all new lines, to make
    # regex matching easier
    code = str(invoke.gen()).replace("\n", "")

    correct_re = ("subroutine invoke.*"
                  "use profile_mod, only: ProfileData.*"
                  r"TYPE\(ProfileData\), save :: profile.*"
                  "call ProfileStart.*"
                  "do cell.*"
                  "call.*"
                  "end.*"
                  "call ProfileEnd")
    assert re.search(correct_re, code, re.I) is not None

    _, invoke = get_invoke("dynamo0.3", "1.2_multi_invoke.f90", 0)

    # Conver the invoke to code, and remove all new lines, to make
    # regex matching easier
    code = str(invoke.gen()).replace("\n", "")

    correct_re = ("subroutine invoke.*"
                  "use profile_mod, only: ProfileData.*"
                  r"TYPE\(ProfileData\), save :: profile.*"
                  "call ProfileStart.*"
                  "do cell.*"
                  "call.*"
                  "end.*"
                  "do cell.*"
                  "call.*"
                  "end.*"
                  "call ProfileEnd")
    assert re.search(correct_re, code, re.I) is not None
    Profiler.set_options(None)


# -----------------------------------------------------------------------------
def test_profile_kernels_dynamo0p3():
    '''Check that all kernels are instrumented correctly
    '''
    Profiler.set_options([Profiler.KERNELS])
    _, invoke = get_invoke("dynamo0.3", "1_single_invoke.f90", 0)

    # Conver the invoke to code, and remove all new lines, to make
    # regex matching easier
    code = str(invoke.gen()).replace("\n", "")

    correct_re = ("subroutine invoke.*"
                  "use profile_mod, only: ProfileData.*"
                  r"TYPE\(ProfileData\), save :: profile.*"
                  "call ProfileStart.*"
                  "do cell.*"
                  "call.*"
                  "end.*"
                  "call ProfileEnd")
    assert re.search(correct_re, code, re.I) is not None

    _, invoke = get_invoke("dynamo0.3", "1.2_multi_invoke.f90", 0)

    # Conver the invoke to code, and remove all new lines, to make
    # regex matching easier
    code = str(invoke.gen()).replace("\n", "")

    correct_re = ("subroutine invoke.*"
                  "use profile_mod, only: ProfileData.*"
                  r"TYPE\(ProfileData\), save :: profile.*"
                  r"TYPE\(ProfileData\), save :: profile.*"
                  r"call ProfileStart\(.*, (?P<profile1>\w*)\).*"
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

    _, invoke = get_invoke("gocean1.0", "test27_loop_swap.f90", "invoke_loop1")
    schedule = invoke.schedule

    prt = ProfileRegionTrans()
    assert str(prt) == "Insert a profile start and end call."
    assert prt.name == "ProfileRegionTrans"

    # Try applying it to a list
    sched1, _ = prt.apply(schedule.children)
    sched1.view()
    out, _ = capsys.readouterr()
    # out is unicode, and has no replace function, so convert to string first
    out = str(out).replace("\n", "")
    correct_re = ("GOSchedule.*"
                  r"    \[Profile\].*"
                  r"        Loop\[type='outer'.*"
                  r"        Loop\[type='outer'.*"
                  r"        Loop\[type='outer'.*")
    assert re.search(correct_re, out)

    # Now only wrap a single node - the middle loop:
    sched2, _ = prt.apply(schedule.children[0].children[1])
    sched2.view()
    out, _ = capsys.readouterr()  # .replace("\n", "")
    # out is unicode, and has no replace function, so convert to string first
    out = str(out).replace("\n", "")
    correct_re = ("GOSchedule.*"
                  r"    \[Profile\].*"
                  r"        Loop\[type='outer'.*"
                  r"        \[Profile\].*"
                  r"            Loop\[type='outer'.*"
                  r"        Loop\[type='outer'.*")
    assert re.search(correct_re, out)

    # Check that an sublist created from individual elements
    # can be wrapped
    sched3, _ = prt.apply([sched2.children[0].children[0],
                           sched2.children[0].children[1]])
    sched3.view()
    out, _ = capsys.readouterr()  # .replace("\n", "")
    # out is unicode, and has no replace function, so convert to string first
    out = str(out).replace("\n", "")
    correct_re = ("GOSchedule.*"
                  r"    \[Profile\].*"
                  r"        \[Profile\].*"
                  r"            Loop\[type='outer'.*"
                  r"            \[Profile\].*"
                  r"                Loop\[type='outer'.*"
                  r"        Loop\[type='outer'.*")
    assert re.search(correct_re, out)


# -----------------------------------------------------------------------------
def test_transform_errors(capsys):
    '''Tests error handling of the profile region transformation.'''

    # This has been imported and tested before, so we can assume
    # here that this all works as expected/
    _, invoke = get_invoke("gocean1.0", "test27_loop_swap.f90", "invoke_loop1")

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
           "<type 'int'>" in str(excinfo)

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

    correct_re = ("GOSchedule.*"
                  r"    \[Profile\].*"
                  r"        Loop\[type='outer'.*"
                  r"        Loop\[type='outer'.*"
                  r"        Loop\[type='outer'.*")
    assert re.search(correct_re, out)
