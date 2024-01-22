# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2018-2024, Science and Technology Facilities Council
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
# Modified by J. Henrichs, Bureau of Meteorology

''' Tests for the psyclone-kern driver. '''

import os
import pytest

from psyclone import gen_kernel_stub, kernel_tools
from psyclone.configuration import Config
from psyclone.domain.lfric import algorithm
from psyclone.psyir.nodes import Container, Routine
from psyclone.psyir.symbols import SymbolTable, DataSymbol, CHARACTER_TYPE
from psyclone.version import __VERSION__


def test_config_loaded_before_constants_created():
    '''Make sure that the main entry point sets the flag that the config
    has been loaded, before an instance of LFRicConstants is created. '''

    kern_file = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                             "test_files", "dynamo0p3", "testkern_w0_mod.f90")
    Config._HAS_CONFIG_BEEN_INITIALISED = False
    kernel_tools.run([str(kern_file)])
    assert Config.has_config_been_initialised() is True


def test_run_default_mode(capsys):
    ''' Test that the default behaviour is to create a kernel stub. '''
    kern_file = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                             "test_files", "dynamo0p3", "testkern_w0_mod.f90")
    kernel_tools.run([str(kern_file)])
    out, err = capsys.readouterr()
    assert "Kernel-stub code:\n   MODULE testkern_w0_mod\n" in out
    assert not err


def test_run(capsys, tmpdir):
    ''' Basic test for the run() routine. '''
    # Use a dynamo 0.3 kernel so that we check that the default API
    # (dynamo 0.3) is picked up correctly
    kern_file = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                             "test_files", "dynamo0p3", "testkern_w0_mod.f90")
    kernel_tools.run([str(kern_file), "--limit", "output", "-gen", "stub"])
    result, _ = capsys.readouterr()
    assert "Kernel-stub code:" in result
    assert "MODULE testkern_w0_mod" in result

    # Test without --limit, but with -o:
    psy_file = tmpdir.join("psy.f90")
    kernel_tools.run([str(kern_file), "-api", "dynamo0.3", "-o",
                      str(psy_file)])
    result, _ = capsys.readouterr()

    # Now read output file into a string and check:
    with psy_file.open("r") as psy:
        output = psy.read()
    assert "MODULE testkern_w0_mod" in str(output)


def test_run_version(capsys):
    ''' Test that the flag requesting version information works correctly. '''
    for arg in ["-v", "--version"]:
        with pytest.raises(SystemExit):
            kernel_tools.run([arg])
        result, _ = capsys.readouterr()
        assert f"psyclone-kern version: {__VERSION__}" in result


def test_run_invalid_api(capsys):
    ''' Test that the expected error is reported if an invalid API is
    specified. '''
    with pytest.raises(SystemExit):
        kernel_tools.run(["-api", "invalid", "not-a-file.f90"])
    _, err = capsys.readouterr()
    assert "Unsupported API 'invalid' specified. Supported APIs are" in err


def test_run_include_flag(capsys):
    ''' Check that the -I flag can be used to pass an include path into
    the configuration object. We actually specify an invalid location and
    check that the expected error is raised. '''
    with pytest.raises(SystemExit):
        kernel_tools.run(["-I", "./some/path", "not-a-file.f90"])
    _, err = capsys.readouterr()
    assert ("PSyclone configuration error: Include path './some/path' does "
            "not exist" in err)


def test_run_missing_file(capsys):
    ''' Test that an IOError is handled correctly. '''
    with pytest.raises(SystemExit):
        kernel_tools.run([str("/does_not_exist")])
    _, result = capsys.readouterr()
    assert ("Error: Kernel stub generator: File '/does_not_exist' "
            "not found" in str(result))


def test_run_alg_gen(capsys):
    ''' Check that the kernel_tools run method attempts to generate an
    algorithm layer if requested. '''
    kern_file = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                             "test_files", "dynamo0p3", "testkern_w0_mod.f90")
    kernel_tools.run(["-gen", "alg", str(kern_file)])
    out, err = capsys.readouterr()
    assert not err
    assert "Algorithm code:\n module test_alg_mod\n" in out


def test_run_alg_gen_unsupported_api(capsys):
    ''' Test that the expected message is output if an API is specified for
    which algorithm-generation is not supported. '''
    with pytest.raises(SystemExit):
        kernel_tools.run(["-api", "gocean1.0", "-gen", "alg",
                          str("/does_not_exist")])
    _, err = capsys.readouterr()
    assert ("Algorithm generation from kernel metadata is not yet implemented "
            "for API 'gocean1.0'" in err)


def test_invalid_gen_arg(capsys, monkeypatch):
    ''' Test that the expected error is raised if the generation option
    specified on the command line is not supported. '''
    # The ArgumentParser class already checks that the supplied value is
    # correct so we have to monkeypatch the list of names that it uses
    # when performing this check.
    monkeypatch.setattr(kernel_tools, "GEN_MODES", {"wrong": "nothing"})
    with pytest.raises(SystemExit):
        kernel_tools.run(["-gen", "wrong", str("/does_not_exist")])
    _, err = capsys.readouterr()
    assert "Expected -gen option to be one of" in err


@pytest.mark.parametrize("limit", [True, False])
@pytest.mark.parametrize("mode", ["alg", "stub"])
def test_run_line_length(fortran_reader, monkeypatch, capsys, limit, mode):
    ''' Check that line-length limiting is applied to generated algorithm
    and kernel-stub code when requested. '''

    def long_psyir_gen(_1, _2, _3):
        ''' Function that returns PSyIR containing a line longer
        than 132 chars. '''
        routine = Routine.create("my_sub", SymbolTable(), [])
        routine.symbol_table.new_symbol("long_str", symbol_type=DataSymbol,
                                        datatype=CHARACTER_TYPE)
        stmt = fortran_reader.psyir_from_statement(f"long_str = '{140*' '}'",
                                                   routine.symbol_table)
        routine.addchild(stmt)
        container = Container.create("my_mod", SymbolTable(), [routine])
        return container

    def long_gen(_1, api=None):
        ''' generate() function that returns a string longer than
        132 chars. '''
        # pylint: disable=unused-argument
        return f"long_str = '{140*' '}'"

    # Monkeypatch both the algorithm and stub creation functions.
    monkeypatch.setattr(algorithm.lfric_alg.LFRicAlg,
                        "create_from_kernel", long_psyir_gen)
    monkeypatch.setattr(gen_kernel_stub, "generate", long_gen)
    args = ["-gen", mode, str("/does_not_exist")]
    if limit:
        args.extend(["--limit", "output"])
    kernel_tools.run(args)
    out, _ = capsys.readouterr()
    if limit:
        assert "long_str = '" in out
        assert "   &\n& " in out
    else:
        assert f"long_str = '{140*' '}'" in out


@pytest.mark.usefixtures("change_into_tmpdir")
@pytest.mark.parametrize("mode", ["alg", "stub"])
def test_file_output(fortran_reader, monkeypatch, mode):
    ''' Check that the output of the generate() function is written to file
    if requested. We test for both the kernel-stub & algorithm generation. '''

    def fake_psyir_gen(_1, _2, _3):
        '''Returns PSyIR for a module containing a particular string for
        testing purposes.'''
        return fortran_reader.psyir_from_source(
            '''
 module my_mod
 contains
  subroutine my_sub
    integer :: the_answer
    the_answer = 42
  end subroutine
 end module
''')

    def fake_gen(_1, api=None):
        ''' generate() function that simply returns a string. '''
        # pylint: disable=unused-argument
        return "the_answer = 42"

    # Monkeypatch both the algorithm and stub creation functions.
    monkeypatch.setattr(algorithm.lfric_alg.LFRicAlg, "create_from_kernel",
                        fake_psyir_gen)
    monkeypatch.setattr(gen_kernel_stub, "generate", fake_gen)

    kernel_tools.run(["-gen", mode, "-o", f"output_file_{mode}",
                      str("/does_not_exist")])
    with open(f"output_file_{mode}", "r", encoding="utf-8") as infile:
        content = infile.read()
        assert "the_answer = 42" in content


def test_no_args_usage_msg(capsys):
    ''' Check that kernel_tools.run() prints a usage message
    if no arguments are supplied. '''

    usage_msg = (
        "usage: psyclone-kern [-h] [-gen {alg,stub}] [-o OUT_FILE] [-api API]")
    with pytest.raises(SystemExit):
        kernel_tools.run([])
    out, err = capsys.readouterr()
    assert out == ""
    assert usage_msg in err
