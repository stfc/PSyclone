# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2018-2022, Science and Technology Facilities Council
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
# Author: A. R. Porter, STFC Daresbury Lab
# Modified: I. Kavcic, Met Office

''' Tests for the psyclone-kern driver. '''

from __future__ import absolute_import
import os
import sys
import pytest

from psyclone import kernel_tools


def test_run_missing_action(monkeypatch, capsys):
    ''' Test that failing to specify whether to create a stub or an algorithm
    results in the expected message. '''
    # Use monkeypatch to spoof some command-line arguments.
    monkeypatch.setattr(sys, "argv", ["psyclone-kern", "not_a_file.f90"])
    with pytest.raises(SystemExit):
        kernel_tools.run()
    out, err = capsys.readouterr()
    assert ("Error, no action specified: one or both of --stub-gen/-oalg or "
            "--alg-gen/-ogen must be supplied." in err)


def test_run(monkeypatch, capsys, tmpdir):
    ''' Basic test for the run() routine. '''
    # Use a dynamo 0.3 kernel so that we check that the default API
    # (dynamo 0.3) is picked up correctly
    kern_file = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                             "test_files", "dynamo0p3", "testkern_w0_mod.f90")
    # Use monkeypatch to spoof some command-line arguments - first with --limit
    monkeypatch.setattr(sys, "argv", ["psyclone-kern", str(kern_file),
                                      "--limit", "output", "--stub-gen"])
    kernel_tools.run()
    result, _ = capsys.readouterr()
    assert "Kernel stub code:" in result
    assert "MODULE testkern_w0_mod" in result

    # Test without --limit, but with -ostub:
    psy_file = tmpdir.join("psy.f90")
    monkeypatch.setattr(sys, "argv", ["genkernelstub", str(kern_file),
                                      "-api", "dynamo0.3", "-ostub",
                                      str(psy_file)])
    kernel_tools.run()
    result, _ = capsys.readouterr()

    # Now read output file into a string and check:
    with psy_file.open("r") as psy:
        output = psy.read()
    assert "MODULE testkern_w0_mod" in str(output)


def test_run_version(monkeypatch, capsys):
    ''' Test that the flag requesting version information works correctly. '''
    monkeypatch.setattr(sys, "argv", ["psyclone-kern", "-v", "not-a-file.f90"])
    with pytest.raises(SystemExit):
        kernel_tools.run()
    result, _ = capsys.readouterr()
    from psyclone.version import __VERSION__
    assert f"psyclone-kern version: {__VERSION__}" in result


def test_run_invalid_api(monkeypatch, capsys):
    ''' Test that the expected error is reported if an invalid API is
    specified. '''
    monkeypatch.setattr(sys, "argv", ["psyclone-kern", "-api", "invalid",
                                      "not-a-file.f90"])
    with pytest.raises(SystemExit):
        kernel_tools.run()
    _, err = capsys.readouterr()
    assert "Unsupported API 'invalid' specified. Supported APIs are" in err


def test_run_include_flag(monkeypatch, capsys):
    ''' Check that the -I flag can be used to pass an include path into
    the configuration object. We actually specify an invalid location and
    check that the expected error is raised. '''
    monkeypatch.setattr(sys, "argv", ["psyclone-kern", "-I", "./some/path",
                                      "not-a-file.f90"])
    with pytest.raises(SystemExit):
        kernel_tools.run()
    _, err = capsys.readouterr()
    assert ("PSyclone configuration error: Include path './some/path' does "
            "not exist" in err)


def test_run_missing_file(monkeypatch, capsys):
    ''' Test that an IOError is handled correctly. '''
    # Use monkeypatch to spoof some command-line arguments
    monkeypatch.setattr(sys, "argv", ["psyclone-kern", "--stub-gen",
                                      str("/does_not_exist")])
    with pytest.raises(SystemExit):
        kernel_tools.run()
    _, result = capsys.readouterr()
    assert ("Error: Kernel stub generator: File '/does_not_exist' "
            "not found" in str(result))


def test_unexpected_exception(monkeypatch, capsys):
    ''' Check that an unexpected Exception is caught correctly. '''
    from psyclone import alg_gen

    def broken_gen(kernel_filename, api):
        ''' Broken generate() function that just raises a general
        Exception. '''
        raise Exception("This is just a test")

    monkeypatch.setattr(alg_gen, "generate", broken_gen)
    monkeypatch.setattr(sys, "argv", ["psyclone-kern", "--alg-gen",
                                      str("/does_not_exist")])
    with pytest.raises(SystemExit):
        kernel_tools.run()
    _, err = capsys.readouterr()
    assert "unexpected exception:" in err
    assert "This is just a test" in err


def test_run_alg_gen(monkeypatch, capsys):
    ''' Check that the kernel_tools run method attempts to generate an
    algorithm layer if requested. Currently this raises a
    NotImplementedError. '''
    monkeypatch.setattr(sys, "argv", ["psyclone-kern", "--alg-gen",
                                      str("/does_not_exist")])
    with pytest.raises(SystemExit):
        kernel_tools.run()
    _, err = capsys.readouterr()
    assert ("Algorithm generation from kernel metadata is not yet "
            "implemented - #1555" in err)


@pytest.mark.parametrize("limit", [True, False])
@pytest.mark.parametrize("mode", ["alg", "stub"])
def test_run_line_length(monkeypatch, capsys, limit, mode):
    ''' Check that line-length limiting is applied to generated algorithm
    and kernel-stub code when requested. '''
    from psyclone import alg_gen, gen_kernel_stub

    def long_gen(kernel_filename, api):
        ''' generate() function that returns a string longer than
        132 chars. '''
        return f"long_str = '{140*' '}'"

    # Monkeypatch both the algorithm and stub 'generate' functions.
    monkeypatch.setattr(alg_gen, "generate", long_gen)
    monkeypatch.setattr(gen_kernel_stub, "generate", long_gen)
    args = ["psyclone-kern", f"--{mode}-gen", str("/does_not_exist")]
    if limit:
        args.extend(["--limit", "output"])
    monkeypatch.setattr(sys, "argv", args)
    kernel_tools.run()
    out, _ = capsys.readouterr()
    if limit:
        assert "long_str = '" in out
        assert "   &\n&                     '" in out
    else:
        assert f"long_str = '{140*' '}'" in out


@pytest.mark.parametrize("mode", ["alg", "stub"])
def test_file_output(monkeypatch, mode, tmpdir):
    ''' Check that the output of the generate() function is written to file
    if requested. We test for both the kernel-stub & algorithm generation. '''
    from psyclone import alg_gen, gen_kernel_stub

    def fake_gen(kernel_filename, api):
        ''' generate() function that simply returns a string. '''
        return "the_answer = 42"

    # Monkeypatch both the algorithm and stub 'generate' functions.
    monkeypatch.setattr(alg_gen, "generate", fake_gen)
    monkeypatch.setattr(gen_kernel_stub, "generate", fake_gen)
    tmpdir.chdir()
    monkeypatch.setattr(sys, "argv", ["psyclone-kern", f"-o{mode}",
                                      f"output_file_{mode}",
                                      str("/does_not_exist")])
    kernel_tools.run()
    with open(f"output_file_{mode}", "r") as infile:
        content = infile.read()
        assert "the_answer = 42" in content
