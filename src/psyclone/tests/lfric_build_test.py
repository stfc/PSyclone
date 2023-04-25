import pytest
import subprocess

from psyclone.tests.lfric_build import LFRicBuild
from psyclone.tests.utilities import Compile, CompileError


def test_lfric_build_infrastructure_flags(tmpdir, monkeypatch):
    '''
    Check that the compiler settings supplied to pytest are passed through
    when building the (stub) LFRic infrastructure.

    '''
    def fake_popen(arg_list, stdout=None, stderr=None):
        '''Mock implementation of Popen that just raises an Exception.'''
        raise OSError(f"arg_list = {arg_list}")

    # Pretend that the infrastructure has already been built so that the
    # LFRicBuild constructor doesn't attempt to trigger it.
    monkeypatch.setattr(LFRicBuild, "_infrastructure_built", True)
    # Pretend that compilation testing is enabled.
    monkeypatch.setattr(Compile, "TEST_COMPILE", True)
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
