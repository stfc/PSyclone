# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2022-2026, Science and Technology Facilities Council.
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
# Author: J. Henrichs, Bureau of Meteorology


'''
Module containing tests related to building generated code for
the GOcean domain.
'''

from pathlib import Path

import pytest

from psyclone.tests.gocean_build import GOceanBuild, GOceanOpenCLBuild
from psyclone.tests.utilities import Compile, CompileError


@pytest.fixture(scope="function", autouse=True)
def reset_infrastructure_compiled_flag():
    '''During testing the compilation path will be modified. Make sure
    we restore the original path (which actually contains the compiled
    files) after each test.

    '''
    saved_orig_path = GOceanBuild._compilation_path
    yield
    GOceanBuild._compilation_path = saved_orig_path


def test_make_flags_with_compilation(tmpdir: Path,
                                     monkeypatch: pytest.MonkeyPatch) -> None:
    '''Test that the compiler flags contains a single "-I",
    followed by a path when testing with compilation. If compilation is
    disabled, we monkey patch GOceanBuild to still take the paths
    that would be taken if compilation is enabled.

    '''
    if not Compile.TEST_COMPILE:
        # Compilation is disabled, so there would be no infrastructure
        # to point to (it would point to external/dl_esm_inf). In order
        # to test behaviour when compiling is actually disabled, patch
        # GOceanBuild to have a specific (though invalid) infrastructure
        # path, and indicating that the infrastructure has already been
        # compiled (to avoid it being compiled in the constructor).
        monkeypatch.setattr(GOceanBuild, "_infrastructure_built", True)
        monkeypatch.setattr(GOceanBuild, "_compilation_path",
                            "/tmp/path/dl_esm_inf")
        monkeypatch.setattr(Compile, "TEST_COMPILE", True)
    flags = GOceanBuild(tmpdir).get_infrastructure_flags()
    assert len(flags) == 2
    assert flags[0] == "-I"
    # This should be a temporary path (*or the above /tmp/path/dl_esm/inf one),
    # so we can't easily check this (different relative paths when testing in
    # parallel). So just check for the name, and that it does not include
    # `external` (so it is not the directory in the PSyclone directory)
    assert "external" not in flags[1]
    assert "dl_esm_inf" in flags[1]


def test_make_flags_without_compilation(
        tmpdir: Path,
        monkeypatch: pytest.MonkeyPatch) -> None:
    '''Test that the compiler flags contains a single "-I", followed by a
    path when testing without compilation. In this case, it should point
    to the external/dl_esm_inf directory. If compilation is actually enabled
    (in which case the compiled directory path in a temp directory would be
    used), we monkey-patch GOceanBuild so that it takes the path used when
    compilation is disabled.

    '''
    if Compile.TEST_COMPILE:
        # Compilation is enabled. In order to test behaviour when not
        # compiling, patch GOceanBuild to have specific (though invalid)
        # infrastructure path, and indicating that the infrastructure has
        # already been compiled:
        monkeypatch.setattr(GOceanBuild, "_infrastructure_built", True)
        monkeypatch.setattr(GOceanBuild, "_compilation_path",
                            "/tmp/path/dl_esm_inf")
        monkeypatch.setattr(Compile, "TEST_COMPILE", False)
    flags = GOceanBuild(tmpdir).get_infrastructure_flags()
    assert len(flags) == 2
    assert flags[0] == "-I"
    # When compilation is disabled, it must return the path of dl_esm_inf
    # included in PSyclone:
    assert "external/dl_esm_inf" in flags[1]


def test_make_fail(tmpdir: Path, monkeypatch: pytest.MonkeyPatch) -> None:
    '''Test that compilation fails as expected if there is no `make`
    installed. This is simulated by replacing the 'make' command
    with a non-existing command.

    '''
    monkeypatch.setattr(Compile, "TEST_COMPILE", True)
    monkeypatch.setattr(GOceanBuild, "_make_command", "make_does_not_exist")

    with pytest.raises(CompileError) as excinfo:
        GOceanBuild(tmpdir)._build_infrastructure()
    assert ("No such file or directory: 'make_does_not_exist'"
            in str(excinfo.value))


def test_make_error_code(tmpdir: Path,
                         monkeypatch: pytest.MonkeyPatch) -> None:
    '''Test that a non-zero return code from the build command is
    handled correctly.

    '''
    monkeypatch.setattr(Compile, "TEST_COMPILE", True)
    monkeypatch.setattr(GOceanBuild, "_make_command", "false")

    with pytest.raises(CompileError) as excinfo:
        GOceanBuild(tmpdir)._build_infrastructure()
    assert ("Compile error: "
            in str(excinfo.value))


def test_make_works(tmpdir: Path, monkeypatch: pytest.MonkeyPatch) -> None:
    '''Tests that no error is raised if the build process worked.
    This done by using `true` as build command.

    '''
    assert GOceanBuild._infrastructure_built is False
    monkeypatch.setattr(Compile, "TEST_COMPILE", True)
    monkeypatch.setattr(Compile, "TEST_COMPILE_OPENCL", True)
    monkeypatch.setattr(GOceanBuild, "_make_command", "true")

    GOceanBuild(tmpdir)._build_infrastructure()
    assert GOceanBuild._infrastructure_built is True


def test_opencl_compiles(tmpdir: Path,
                         monkeypatch: pytest.MonkeyPatch) -> None:
    '''Test that the OpenCL compilation works as expected.

    '''
    monkeypatch.setattr(Compile, "TEST_COMPILE_OPENCL", False)
    opencl_build = GOceanOpenCLBuild(tmpdir)
    assert opencl_build.code_compiles(None, None) is True
    monkeypatch.setattr(Compile, "TEST_COMPILE_OPENCL", True)

    # GOceanOpenCLBuild.code_compiles will call Compile._code_compiles.
    # In case that compilation is disabled, patch this function to just
    # return True
    monkeypatch.setattr(Compile, "_code_compiles", lambda a, b, c: True)
    assert opencl_build.code_compiles(None, None) is True
