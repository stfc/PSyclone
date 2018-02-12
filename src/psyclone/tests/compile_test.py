# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017, Science and Technology Facilities Council
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
# Author R. Ford and A. R. Porter, STFC Daresbury Lab

''' This module contains tests for the infrastructure used to test
the compilation of generated Fortran code '''

from __future__ import absolute_import
import os
import pytest
import utils


HELLO_CODE = '''
program hello
  write (*,*) "Hello"
end program hello
'''


@utils.COMPILE
def test_compiler_works(tmpdir, f90, f90flags):
    ''' Check that the specified compiler works for a hello-world
    example '''
    old_pwd = tmpdir.chdir()
    try:
        with open("hello_world.f90", "w") as ffile:
            ffile.write(HELLO_CODE)
            success = utils.compile_file("hello_world.f90", f90, f90flags)
    finally:
        os.chdir(str(old_pwd))
    assert success


def code_compiles_invalid_api(tmpdir, f90, f90flags):
    ''' Check that utils.code_compiles() reject an unrecognised API '''
    with pytest.raises(utils.CompileError) as excinfo:
        utils.code_compiles("not_an_api", "fake_psy", tmpdir, f90, f90flags)
    assert "Unsupported API in " in str(excinfo)


@utils.COMPILE
def test_compiler_with_flags(tmpdir, f90):
    ''' Check that we can pass through flags to the Fortran compiler.
    Since correct flags are compiler-dependent and hard to test,
    we pass something that is definitely not a flag and check that
    the compiler complains. This test is skipped if no compilation
    tests have been requested (--compile flag to py.test). '''
    old_pwd = tmpdir.chdir()
    try:
        with open("hello_world.f90", "w") as ffile:
            ffile.write(HELLO_CODE)
        with pytest.raises(utils.CompileError) as excinfo:
            _ = utils.compile_file("hello_world.f90", f90, "not-a-flag")
        assert "not-a-flag" in str(excinfo)
        # For completeness we also try with a valid flag although we
        # can't actually check its effect.
        success = utils.compile_file("hello_world.f90", f90, "-g")
    finally:
        os.chdir(str(old_pwd))
    assert success


@utils.COMPILE
def test_build_invalid_fortran(tmpdir, f90, f90flags):
    ''' Check that we raise the expected error when attempting
    to compile some invalid Fortran. Skips test if --compile not
    supplied to py.test on command-line. '''
    invalid_code = HELLO_CODE.replace("write", "wite", 1)
    old_pwd = tmpdir.chdir()
    try:
        with open("hello_world.f90", "w") as ffile:
            ffile.write(invalid_code)
        with pytest.raises(utils.CompileError) as excinfo:
            _ = utils.compile_file("hello_world.f90", f90, f90flags)
    finally:
        os.chdir(str(old_pwd))
    assert "Compile error" in str(excinfo)


def test_find_fortran_file(tmpdir):
    ''' Check that our find_fortran_file routine raises the expected
    error if it can't find a matching file. Also check that it returns
    the correct name if the file does exist. '''
    with pytest.raises(IOError) as excinfo:
        utils.find_fortran_file(str(tmpdir), "missing_file")
    assert "missing_file' with suffix in ['f90', 'F90'," in str(excinfo)
    old_pwd = tmpdir.chdir()
    try:
        with open("hello_world.f90", "w") as ffile:
            ffile.write(HELLO_CODE)
        name = utils.find_fortran_file(str(tmpdir), "hello_world")
        assert name.endswith("hello_world.f90")
    finally:
        os.chdir(str(old_pwd))
