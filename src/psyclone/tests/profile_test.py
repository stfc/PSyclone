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
from psyclone.parse import parse
from psyclone.psyGen import PSyFactory
from psyclone.profiler import Profiler


def get_invoke(api, algfile, idx):
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
    # invokes does not have a method by which to request the i'th
    # in the list so we do this rather clumsy lookup of the name
    # of the invoke that we want
    invoke = invokes.get(invokes.names[idx])
    return psy, invoke


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
                  "use profiler_mod, only: ProfilerData.*"
                  r"TYPE\(ProfilerData\), save :: profile.*"
                  "call profile_start.*"
                  "do j.*"
                  "do i.*"
                  "call.*"
                  "end.*"
                  "end.*"
                  "call profile_end")
    assert re.search(correct_re, code, re.I) is not None

    _, invoke = get_invoke("gocean1.0", "single_invoke_"
                           "two_kernels.f90", 0)

    # Conver the invoke to code, and remove all new lines, to make
    # regex matching easier
    code = str(invoke.gen()).replace("\n", "")

    correct_re = ("subroutine invoke.*"
                  "use profiler_mod, only: ProfilerData.*"
                  r"TYPE\(ProfilerData\), save :: profile.*"
                  "call profile_start.*"
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
                  "call profile_end")
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
                  "use profiler_mod, only: ProfilerData.*"
                  r"TYPE\(ProfilerData\), save :: profile.*"
                  "call profile_start.*"
                  "do j.*"
                  "do i.*"
                  "call.*"
                  "end.*"
                  "end.*"
                  "call profile_end")
    assert re.search(correct_re, code, re.I) is not None

    _, invoke = get_invoke("gocean1.0", "single_invoke_"
                           "two_kernels.f90", 0)

    # Conver the invoke to code, and remove all new lines, to make
    # regex matching easier
    code = str(invoke.gen()).replace("\n", "")

    correct_re = ("subroutine invoke.*"
                  "use profiler_mod, only: ProfilerData.*"
                  r"TYPE\(ProfilerData\), save :: profile.*"
                  r"TYPE\(ProfilerData\), save :: profile.*"
                  r"call profile_start\(.*, (?P<profile1>\w*)\).*"
                  "do j.*"
                  "do i.*"
                  "call.*"
                  "end.*"
                  "end.*"
                  r"call profile_end\((?P=profile1)\).*"
                  r"call profile_start\(.*, (?P<profile2>\w*)\).*"
                  "do j.*"
                  "do i.*"
                  "call.*"
                  "end.*"
                  "end.*"
                  r"call profile_end\((?P=profile2)\)")
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
                  "use profiler_mod, only: ProfilerData.*"
                  r"TYPE\(ProfilerData\), save :: profile.*"
                  "call profile_start.*"
                  "do cell.*"
                  "call.*"
                  "end.*"
                  "call profile_end")
    assert re.search(correct_re, code, re.I) is not None

    _, invoke = get_invoke("dynamo0.3", "1.2_multi_invoke.f90", 0)

    # Conver the invoke to code, and remove all new lines, to make
    # regex matching easier
    code = str(invoke.gen()).replace("\n", "")

    correct_re = ("subroutine invoke.*"
                  "use profiler_mod, only: ProfilerData.*"
                  r"TYPE\(ProfilerData\), save :: profile.*"
                  "call profile_start.*"
                  "do cell.*"
                  "call.*"
                  "end.*"
                  "do cell.*"
                  "call.*"
                  "end.*"
                  "call profile_end")
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
                  "use profiler_mod, only: ProfilerData.*"
                  r"TYPE\(ProfilerData\), save :: profile.*"
                  "call profile_start.*"
                  "do cell.*"
                  "call.*"
                  "end.*"
                  "call profile_end")
    assert re.search(correct_re, code, re.I) is not None

    _, invoke = get_invoke("dynamo0.3", "1.2_multi_invoke.f90", 0)

    # Conver the invoke to code, and remove all new lines, to make
    # regex matching easier
    code = str(invoke.gen()).replace("\n", "")

    correct_re = ("subroutine invoke.*"
                  "use profiler_mod, only: ProfilerData.*"
                  r"TYPE\(ProfilerData\), save :: profile.*"
                  r"TYPE\(ProfilerData\), save :: profile.*"
                  r"call profile_start\(.*, (?P<profile1>\w*)\).*"
                  "do cell.*"
                  "call.*"
                  "end.*"
                  r"call profile_end\((?P=profile1)\).*"
                  r"call profile_start\(.*, (?P<profile2>\w*)\).*"
                  "do cell.*"
                  "call.*"
                  "end.*"
                  r"call profile_end\((?P=profile2)\).*")
    groups = re.search(correct_re, code, re.I)
    assert groups is not None
    # Check that the variables are different
    assert groups.group(1) != groups.group(2)
    Profiler.set_options(None)
