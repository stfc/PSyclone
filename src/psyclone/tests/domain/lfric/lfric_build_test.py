# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2022-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------


'''
Module containing tests related to building generated code for
the LFRic domain.
'''

import pytest

from psyclone.tests.lfric_build import LFRicBuild
from psyclone.tests.utilities import Compile, CompileError


@pytest.fixture(scope="function", autouse=True)
def reset_infrastructure_compiled_flag():
    '''During testing the compilation path will be modified. Make sure
    we restore the original path (which actually contains the compiled
    files) after each test. Also set the built flag to false to always
    trigger a fresh building of the infrastructure.

    '''
    LFRicBuild._infrastructure_built = False
    saved_orig_path = LFRicBuild._compilation_path
    yield
    LFRicBuild._compilation_path = saved_orig_path


def test_make_flags(tmpdir):
    '''Test that the compiler flags consists of a list with "-I"
    in every second position: `-I operator -I field -I mesh`

    '''
    flags = LFRicBuild(tmpdir).get_infrastructure_flags()
    i = 0
    while i < len(flags):
        assert flags[i] == "-I"
        i += 2


def test_make_fail(tmpdir, monkeypatch):
    '''Test that compilation fails as expected if there is no `make`
    installed. This is simulated by replacing the 'make' command
    with a non-existing command.

    '''
    monkeypatch.setattr(Compile, "TEST_COMPILE", True)
    monkeypatch.setattr(LFRicBuild, "_make_command", "make_does_not_exist")

    with pytest.raises(CompileError) as excinfo:
        LFRicBuild(tmpdir)._build_infrastructure()
    assert ("No such file or directory: 'make_does_not_exist'"
            in str(excinfo.value))


def test_make_error_code(tmpdir, monkeypatch):
    '''Test that a non-zero return code from the build command is
    handled correctly.

    '''
    monkeypatch.setattr(Compile, "TEST_COMPILE", True)
    monkeypatch.setattr(LFRicBuild, "_make_command", "false")

    with pytest.raises(CompileError) as excinfo:
        LFRicBuild(tmpdir)._build_infrastructure()
    assert ("Compile error: "
            in str(excinfo.value))


def test_make_works(tmpdir, monkeypatch):
    '''Tests that no error is raised if the build process worked.
    This done by using `true` as build command.

    '''
    assert LFRicBuild._infrastructure_built is False
    monkeypatch.setattr(Compile, "TEST_COMPILE", True)
    monkeypatch.setattr(LFRicBuild, "_make_command", "true")

    LFRicBuild(tmpdir)._build_infrastructure()
    assert LFRicBuild._infrastructure_built is True
