# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017-2024, Science and Technology Facilities Council.
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
# Authors: R. W. Ford and A. R. Porter, STFC Daresbury Lab
# Modified by J. Henrichs, Bureau of Meteorology

''' This module contains tests for the the various utility functions in
tests/utilities.py.'''

import os

import pytest

from psyclone.parse.algorithm import parse
from psyclone.parse.utils import ParseError
from psyclone.psyGen import PSyFactory
from psyclone.tests.utilities import (change_dir, count_lines, Compile,
                                      CompileError, get_invoke, line_number,
                                      print_diffs)


HELLO_CODE = '''
program hello
  write (*,*) "Hello"
end program hello
'''


def test_enable_disable_compilation(monkeypatch, capsys):
    '''Test the behaviour when disabling compilation ... even if
    compilation is enabled.
    '''
    # We need to disable both compilation flags to fully disable
    # compilation (since otherwise when OpenCL is enabled the
    # infrastructure will still be compiled)
    monkeypatch.setattr(Compile, "TEST_COMPILE", False)
    monkeypatch.setattr(Compile, "TEST_COMPILE_OPENCL", False)
    # Test that compile_file will do nothing if compilation is disabled:
    _compile = Compile()
    # Any of these commands would fail if the compiler would be
    # started incorrectly.
    _compile.compile_file("nothing-to-compile")
    _compile.code_compiles(None)
    _compile.string_compiles("")

    # Check that skip_if_compilation_disabled works as expected:
    # modify the pytest.skip function to print the message, and
    # then check that the message was printed to stdout:
    monkeypatch.setattr(pytest, "skip", print)
    Compile.skip_if_compilation_disabled()
    out, _ = capsys.readouterr()
    assert "Need --compile option to run" in out


# -----------------------------------------------------------------------------
def test_enable_disable_opencl_compilation(monkeypatch, capsys):
    '''Test the behaviour when disabling opencl compilation ... even if
    compilation is enabled.
    '''
    monkeypatch.setattr(Compile, "TEST_COMPILE_OPENCL", False)
    # Check that skip_if_compilation_disabled works as expected:
    # modify the pytest.skip function to print the message, and
    # then check that the message was printed to stdout:
    monkeypatch.setattr(pytest, "skip", print)
    Compile.skip_if_opencl_compilation_disabled()
    out, _ = capsys.readouterr()
    assert "Need --compileopencl option to run" in out


# -----------------------------------------------------------------------------
@pytest.mark.usefixtures("change_into_tmpdir")
def test_compiler_works(monkeypatch):
    ''' Check that the specified compiler works for a hello-world
    example.'''

    _compile = Compile("/some-random-dir")
    assert _compile.base_path is None
    assert _compile._tmpdir == "/some-random-dir"
    _compile.base_path = "/tmp"
    assert _compile.base_path == "/tmp"

    if not Compile.TEST_COMPILE:

        # If compilation is disable, use '/usr/bin/true' as 'compile'
        # to cover more lines:
        monkeypatch.setattr(Compile, "TEST_COMPILE", True)
        monkeypatch.setattr(Compile, "F90", "true")

    _compile = Compile()
    assert _compile._tmpdir == os.getcwd()
    # Check compile_file:
    with open("hello_world.f90", "w", encoding="utf-8") as ffile:
        ffile.write(HELLO_CODE)
    _compile.compile_file("hello_world.f90", link=True)
    # Check string_compiles functions
    _compile.string_compiles(HELLO_CODE)

    monkeypatch.setattr(_compile, "_f90", "does-not-exist")
    with pytest.raises(CompileError) as err:
        _compile.compile_file("hello_world.f90")
    assert _compile.string_compiles(HELLO_CODE) is False
    # The actual error message might vary, e.g.:
    # No such file or directory: 'does-not-exist
    # But it should contain the invalid command in any case
    assert "does-not-exist" in str(err.value)


# -----------------------------------------------------------------------------
@pytest.mark.usefixtures("change_into_tmpdir")
def test_compiler_with_flags(monkeypatch):
    ''' Check that we can pass through flags to the Fortran compiler.
    Since correct flags are compiler-dependent and hard to test,
    we pass something that is definitely not a flag and check that
    the compiler complains. This test is skipped if no compilation
    tests have been requested (--compile flag to py.test). '''
    if not Compile.TEST_COMPILE:
        # If compilation is disable, use '/usr/bin/true' as 'compile'
        # to cover more lines:
        monkeypatch.setattr(Compile, "TEST_COMPILE", True)
        monkeypatch.setattr(Compile, "F90", "false")

    with open("hello_world.f90", "w", encoding="utf-8") as ffile:
        ffile.write(HELLO_CODE)
    _compile = Compile()
    _compile._f90flags = "-not-a-flag"
    with pytest.raises(CompileError) as excinfo:
        _compile.compile_file("hello_world.f90")

    assert "not-a-flag" in str(excinfo.value) or Compile.F90 == "false"

    # We have monkeypatched TEST_COMPILE to be true if compilation is
    # disabled. So check for the specified compiler here:
    if Compile.F90 == "false":
        # If we are not compiling, use 'true' as compiler in
        # the next step that is supposed to be successful.
        _compile._f90 = "true"

    # For completeness we also try with a valid flag although we
    # can't actually check its effect.
    _compile._f90flags = "-g"
    _compile.compile_file("hello_world.f90", link=True)


# -----------------------------------------------------------------------------
@pytest.mark.usefixtures("change_into_tmpdir")
def test_build_invalid_fortran(monkeypatch):
    ''' Check that we raise the expected error when attempting
    to compile some invalid Fortran. Skips test if --compile not
    supplied to py.test on command-line. '''
    if not Compile.TEST_COMPILE:
        # If compilation is disable, use '/usr/bin/true' as 'compile'
        # to cover more lines:
        monkeypatch.setattr(Compile, "TEST_COMPILE", True)
        monkeypatch.setattr(Compile, "F90", "false")

    invalid_code = HELLO_CODE.replace("write", "wite", 1)
    with open("hello_world.f90", "w", encoding="utf-8") as ffile:
        ffile.write(invalid_code)
    _compile = Compile()
    with pytest.raises(CompileError) as excinfo:
        _compile.compile_file("hello_world.f90")

    assert "Compile error" in str(excinfo.value)


# -----------------------------------------------------------------------------
@pytest.mark.usefixtures("change_into_tmpdir")
def test_find_fortran_file():
    ''' Check that our find_fortran_file routine raises the expected
    error if it can't find a matching file. Also check that it returns
    the correct name if the file does exist. '''
    with pytest.raises(IOError) as excinfo:
        Compile.find_fortran_file([os.getcwd()], "missing_file")
    assert "missing_file' with suffix in ['f90', 'F90'," in str(excinfo.value)

    with open("hello_world.f90", "w", encoding="utf-8") as ffile:
        ffile.write(HELLO_CODE)
    name = Compile.find_fortran_file([os.getcwd()], "hello_world")
    assert name.endswith("hello_world.f90")
    # Check that we also succeed if the file suffix is included
    name = Compile.find_fortran_file([os.getcwd()], "hello_world.f90")
    assert name.endswith("hello_world.f90")


# -----------------------------------------------------------------------------
def test_compile_str(monkeypatch, tmpdir):
    ''' Checks for the routine that compiles Fortran supplied as a string '''
    if not Compile.TEST_COMPILE:
        # If compilation is disable, use '/usr/bin/true' as 'compile'
        # to cover more lines:
        monkeypatch.setattr(Compile, "TEST_COMPILE", True)
        monkeypatch.setattr(Compile, "F90", "true")

    _compile = Compile(tmpdir)

    # Check that we always return True if compilation testing is disabled
    monkeypatch.setattr(Compile, "TEST_COMPILE", False)
    monkeypatch.setattr(Compile, "TEST_COMPILE_OPENCL", False)
    assert _compile.string_compiles("not fortran")

    # Re-enable compilation testing and check that we can build hello world
    monkeypatch.setattr(Compile, "TEST_COMPILE", True)
    assert _compile.string_compiles(HELLO_CODE)

    # Repeat for some broken code
    if Compile.F90 == "true":
        monkeypatch.setattr(_compile, "_f90", "false")
    invalid_code = HELLO_CODE.replace("write", "wite", 1)
    assert not _compile.string_compiles(invalid_code)


# -----------------------------------------------------------------------------
@pytest.mark.usefixtures("change_into_tmpdir")
def test_code_compile(monkeypatch):
    '''A dummy test of the underlying code_compiles function, which takes
    an AST. Note that the derived classes (GOceanBuild and LFRicBuild)
    will test this properly (especially when compilation is enabled),
    here we only cover the other lines handling AST conversion. So,
    here we just use 'true' as 'compiler'.
    '''
    _, invoke_info = parse(os.path.join(os.path.
                                        dirname(os.path.
                                                abspath(__file__)),
                                        "test_files", "gocean1p0",
                                        "single_invoke.f90"),
                           api="gocean1.0")
    psy = PSyFactory("gocean1.0", distributed_memory=False).create(invoke_info)
    # Always trigger 'compilation' to avoid early abort, before the code
    # to be tested is executed.
    monkeypatch.setattr(Compile, "TEST_COMPILE", True)
    monkeypatch.setattr(Compile, "F90", "true")
    _compile = Compile()

    # GOceanBuild or LFRicBuild will set the 'base_path' for the
    # infrastructure files. We just need to set it to something:
    _compile.base_path = os.getcwd()
    # Only used to specify an existing dependency:
    with open("hello_world.f90", "w", encoding="utf-8") as ffile:
        ffile.write(HELLO_CODE)
    assert _compile.code_compiles(psy, dependencies=["something.cl",
                                                     "hello_world.f90"])
    # Now check compilation failure, which we simulate by
    # using 'false' as compiler:
    monkeypatch.setattr(_compile, "_f90", "false")
    assert (_compile.code_compiles(psy, dependencies=["something.cl",
                                                      "hello_world.f90"])
            is False)


# -----------------------------------------------------------------------------
def test_line_number():
    '''Tests the line number function.
    '''
    assert line_number("a\nb\nc", "a") == 0
    assert line_number("a\nb\nc", "c") == 2
    assert line_number("a\nb\nc", "x") == -1


# -----------------------------------------------------------------------------
def test_count_lines():
    '''Tests the count_lines function.
    '''
    assert count_lines("a\nbab\ncaaa", "a") == 3
    assert count_lines("a\nbab\ncaaa", "b") == 1
    assert count_lines("a\nbab\ncaaa", "c") == 1


# -----------------------------------------------------------------------------
def test_print_diff(capsys):
    '''Tests the print-diff function.'''
    string_list1 = "aa\nbb\ncc"
    string_list2 = "aa\ncc\ndd"
    print_diffs(string_list1, string_list2)
    out, err = capsys.readouterr()
    assert out == "['  aa', '- bb', '  cc', '+ dd']\n"
    assert err == ""


# -----------------------------------------------------------------------------
def test_get_invoke():
    '''Tests get_invokes. '''

    # First test all 3 valid APIs - we only make sure that no exception
    # is raised, so no assert required

    get_invoke("test14_module_inline_same_kernel.f90", "gocean1.0", idx=0)
    get_invoke("1_single_invoke.f90", "dynamo0.3", idx=0)
    # Check that dist_mem is being accepted:
    get_invoke("1_single_invoke.f90", "dynamo0.3", idx=0, dist_mem=True)
    get_invoke("1_single_invoke.f90", "dynamo0.3", idx=0, dist_mem=False)
    get_invoke("explicit_do.f90", "nemo", idx=0)

    # Test that an invalid name raises an exception
    with pytest.raises(RuntimeError) as excinfo:
        get_invoke("test11_different_iterates_over_one_invoke.f90",
                   "gocean1.0", name="invalid_name")
    assert "Cannot find an invoke named 'invalid_name'" in str(excinfo.value)

    # Test that an invalid API raises the right exception:
    with pytest.raises(RuntimeError) as excinfo:
        get_invoke("test11_different_iterates_over_one_invoke.f90",
                   "invalid-api", name="invalid_name")
    assert "The API 'invalid-api' is not supported" in str(excinfo.value)

    # Test that invalid parameter combinations raise an exception:
    with pytest.raises(RuntimeError) as excinfo:
        get_invoke("does_not_exist", "nemo")
    assert ("Either the index or the name of the requested invoke must "
            "be specified") in str(excinfo.value)

    with pytest.raises(RuntimeError) as excinfo:
        get_invoke("does_not_exist", "nemo", idx=0, name="name")
    assert ("Either the index or the name of the requested invoke must "
            "be specified") in str(excinfo.value)

    # Test that a non-existent file raises the right exception
    with pytest.raises(ParseError) as excinfo:
        get_invoke("does_not_exist", "nemo", idx=0)
    assert "No such file or directory" in str(excinfo.value)


# -----------------------------------------------------------------------------
def test_change_directory():
    '''Tests the change_directory context manager.'''

    old_dir = os.getcwd()

    with change_dir("/tmp"):
        tmp_dir = os.getcwd()
        assert tmp_dir == "/tmp"

    assert os.getcwd() == old_dir
