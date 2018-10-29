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
# Author: A. R. Porter, STFC Daresbury Lab

''' Tests for the kernel-stub generator. '''

from __future__ import absolute_import
import os
import sys
import pytest


def test_run(monkeypatch, capsys):
    ''' Basic test for the run() routine. '''
    from psyclone.gen_kernel_stub import run
    # Use a dynamo 0.3 kernel so that we check that the default API
    # (dynamo 0.3) is picked up correctly
    kern_file = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                             "test_files", "dynamo0p3", "testkern_w0_mod.f90")
    # Use monkeypatch to spoof some command-line arguments - first with --limit
    monkeypatch.setattr(sys, "argv", ["genkernelstub", str(kern_file),
                                      "--limit"])
    run()
    result, _ = capsys.readouterr()
    assert "Kernel stub code:" in result
    assert "MODULE testkern_w0_mod" in result

    # Test without --limit, but with -o:
    import tempfile
    filetemp_psy = tempfile.NamedTemporaryFile()
    psy_filename = filetemp_psy.name
    monkeypatch.setattr(sys, "argv", ["genkernelstub", str(kern_file),
                                      "-api", "dynamo0.3", "-o", psy_filename])
    run()
    result, _ = capsys.readouterr()

    # Now read output file into a string and check:
    output = filetemp_psy.read()
    assert "MODULE testkern_w0_mod" in str(output)


# -----------------------------------------------------------------------------

def test_failures(monkeypatch, capsys):
    '''Tests various failures of the generate call.
    '''

    from psyclone.gen_kernel_stub import generate, run
    from psyclone.parse import ParseError
    from psyclone.psyGen import GenerationError

    # Test error handling of command line options
    with pytest.raises(SystemExit) as err:
        # Use monkeypatch to spoof some command-line arguments
        monkeypatch.setattr(sys, "argv", ["genkernelstub",
                                          str("/does_not_exist")])
        run()
    result, _ = capsys.readouterr()
    assert "Error: file '/does_not_exist' not found" in str(result)

    # Test empty API (and file not found)
    with pytest.raises(IOError) as err:
        generate("/does_not_exist", api="")
    assert "file '/does_not_exist' not found" in str(err)

    # CHeck invalid API
    with pytest.raises(GenerationError) as err:
        generate("filename", api="invalid")
    assert "Unsupported API 'invalid' specified." in str(err)

    # Trapping Fortran errors:
    with pytest.raises(ParseError) as err:
        # Use this python file to trigger invalid Fortran
        generate(__file__, api="dynamo0.3")
    assert "Code appears to be invalid" in str(err)
