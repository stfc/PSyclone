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


def test_run_missing_file(monkeypatch, capsys):
    ''' Test that an IOError is handled correctly. '''
    # Test error handling of command line options
    with pytest.raises(SystemExit):
        # Use monkeypatch to spoof some command-line arguments
        monkeypatch.setattr(sys, "argv", ["psyclone-kern", "--stub-gen",
                                          str("/does_not_exist")])
        kernel_tools.run()
    _, result = capsys.readouterr()
    assert ("Error: Kernel stub generator: File '/does_not_exist' "
            "not found" in str(result))
