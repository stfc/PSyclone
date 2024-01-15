# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2023-2024, Science and Technology Facilities Council.
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
# Author:  A. R. Porter, STFC Daresbury Lab

''' This module contains tests for the lfric_build.py file. '''

import os
import subprocess
import pytest

from psyclone.tests.lfric_build import LFRicBuild
from psyclone.tests.utilities import Compile, CompileError


@pytest.fixture(name="enable_compilation")
def protect_infrastructure_path_fixture(monkeypatch):
    '''A pytest fixture that mocks testing with compilation enabled while
    ensuring that any pre-existing LFRicBuild._compilation_path is
    preserved.'''
    # Pretend that the infrastructure has already been built so that the
    # LFRicBuild constructor doesn't attempt to trigger it.
    monkeypatch.setattr(LFRicBuild, "_infrastructure_built", True)
    # Pretend that compilation testing is enabled.
    monkeypatch.setattr(Compile, "TEST_COMPILE", True)
    # If compilation testing is enabled then the infrastructure lib will
    # already have been built so we use monkeypatch to ensure its
    # location is automatically restored at the end of this test.
    monkeypatch.setattr(LFRicBuild, "_compilation_path", "/no/path")


def test_lf_build_get_infrastructure_flags(monkeypatch, tmpdir):
    '''
    Test the get_infrastructure_flags method.

    '''
    # Pretend that compilation testing is disabled.
    monkeypatch.setattr(Compile, "TEST_COMPILE", False)
    builder = LFRicBuild(tmpdir)
    flags = builder.get_infrastructure_flags()
    dir_list = []
    for idx, flag in enumerate(flags):
        if idx % 2 == 0:
            assert flag == '-I'
        else:
            # Just keep a list of the base directories
            dir_list.append(os.path.split(flag)[1])
    assert 'configuration' in dir_list
    assert 'function_space' in dir_list
    assert 'field' in dir_list


@pytest.mark.usefixtures("enable_compilation")
def test_lfric_build_compiler_flags(tmpdir, monkeypatch):
    '''
    Check that the compiler settings supplied to pytest are passed through
    when building the (stub) LFRic infrastructure.

    '''
    def fake_popen(arg_list, stdout=None, stderr=None):
        '''Mock implementation of Popen that just raises an Exception.'''
        raise OSError(f"arg_list = {arg_list}")

    # Monkeypatch subprocess.Popen so that it just raises an exception.
    monkeypatch.setattr(subprocess, "Popen", fake_popen)
    builder = LFRicBuild(tmpdir)
    # Set-up a custom compiler and flags.
    builder._f90 = "my_compiler"
    builder._f90flags = "-my -special -flags"
    # Finally, check that these get passed through to the call to Popen when
    # the infrastructure is built.
    with pytest.raises(CompileError) as err:
        builder._build_infrastructure()
    assert ("['make', 'F90=my_compiler', 'F90FLAGS=-my -special -flags', '-f'"
            in str(err.value))


@pytest.mark.usefixtures("enable_compilation")
def test_lfric_build_infrastructure(tmpdir, monkeypatch):
    '''
    Test the _build_infrastructure method when compilation appears to proceed
    (i.e. the Popen.subprocess() command completes without raising an
    exception). Test with a return status of both 0 (success) and 1 (fail).

    '''
    class Build():
        '''Mock object for use when monkeypatching Popen.'''
        RETURN_CODE = 1

        def __init__(self):
            self.returncode = Build.RETURN_CODE

        def __enter__(self):
            return self

        def __exit__(self, _1, _2, _3):
            return

        def communicate(self):
            '''
            :returns: fake stdout output.
            :rtype: Tuple[bytes, NoneType]
            '''
            return (bytes("fake_out", "utf-8"), None)

    def fake_popen(arg_list, stdout=None, stderr=None):
        '''Mock implementation of Popen that just returns a Build instance.'''
        # pylint: disable=unused-argument
        return Build()

    # Monkeypatch subprocess.Popen so that compilation appears to run but
    # returns a status of 1.
    monkeypatch.setattr(subprocess, "Popen", fake_popen)
    builder = LFRicBuild(tmpdir)
    with pytest.raises(CompileError) as err:
        builder._build_infrastructure()
    assert "Compile error: fake_out" in str(err.value)
    # Repeat but alter the Build class to mock a successful build (return
    # status of 0).
    monkeypatch.setattr(Build, "RETURN_CODE", 0)
    monkeypatch.setattr(LFRicBuild, "_infrastructure_built", False)
    builder._build_infrastructure()
    # Check that the '_infrastructure_built' flag is set after a successful
    # build.
    assert LFRicBuild._infrastructure_built is True
