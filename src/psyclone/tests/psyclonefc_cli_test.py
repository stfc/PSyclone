# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2025, Science and Technology Facilities Council.
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
# Author: S. Siso, STFC Daresbury Lab

''' Test for the psyclonefc command '''

import os
import pytest
from psyclone.psyclonefc_cli import compiler_wrapper


def test_psyclonefc_errors(monkeypatch):
    ''' Test the cli error exits. '''
    with pytest.raises(SystemExit) as err:
        compiler_wrapper([])
    assert ("psyclonefc error: PSYCLONE_COMPILER environment variable not "
            "found! This environment variable must be set to the Fortran "
            "compiler to use." in str(err.value))
    monkeypatch.setattr(os, 'environ', {'PSYCLONE_COMPILER': 'psyclonefc'})
    with pytest.raises(SystemExit) as err:
        compiler_wrapper([])
    assert ("PSYCLONE_COMPILER environment variable must not be set to "
            "psyclonefc. This environment variable must be set to the "
            "Fortran compiler to use." in str(err.value))


def test_psyclonefc(monkeypatch, capsys):
    ''' Test the cli with different arguments provided. '''

    # Use the true command as compiler, this will always return a successful
    # errcode 0 and accepts any flag. Also monkeypatch the psyclone main to
    # don't do anything. The tests use the companion print statements
    monkeypatch.setattr(os, 'environ', {'PSYCLONE_COMPILER': 'true'})
    monkeypatch.setattr('psyclone.psyclonefc_cli.main', lambda x: None)

    # If we provide no argument it goes through without failing
    with pytest.raises(SystemExit) as err:
        compiler_wrapper([])
    # This is a successful exit
    assert err.value.code == 0

    # Same for flags not related to a Fortan file
    with pytest.raises(SystemExit) as err:
        compiler_wrapper(['--version'])
    # This is a successful exit
    assert err.value.code == 0

    # Now provide more typical compilation arguments
    with pytest.raises(SystemExit) as err:
        compiler_wrapper(['source.f90', '-c', '-o', 'source.o'])
    assert err.value.code == 0
    stdout, _ = capsys.readouterr()
    # This will execute:
    # What comes after -I is pytest dependent, so we skip it
    assert "psyclone -I " in stdout
    assert "-o source.psycloned.f90 source.f90" in stdout
    assert "true source.psycloned.f90 -c -o source.o" in stdout

    # Now with PSYCONE_OPTS and multiple files
    monkeypatch.setattr(os, 'environ', {
        'PSYCLONE_COMPILER': 'true',
        # Also check that multi-spaces are fine
        'PSYCLONE_OPTS': '  -l   output  ',
    })
    with pytest.raises(SystemExit) as err:
        compiler_wrapper(['source1.f90', 'source2.f90', '-c', '-o', 'app.exe'])
    assert err.value.code == 0
    stdout, _ = capsys.readouterr()
    # This will execute:
    assert "psyclone -l output -I " in stdout
    assert "-o source1.psycloned.f90 source1.f90" in stdout
    assert "-o source2.psycloned.f90 source2.f90" in stdout
    assert ("true source1.psycloned.f90 source2.psycloned.f90 -c -o app.exe"
            in stdout)
