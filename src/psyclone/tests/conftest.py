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
# Author A. R. Porter, STFC Daresbury Lab
# Modified by R. W. Ford, STFC Daresbury Lab
# Modified by J. Henrichs, Bureau of Meteorology


''' Module which performs pytest set-up so that we can specify
    command-line options. Also creates certain test fixtures. '''

import os
import copy
import pytest

from fparser.two.parser import ParserFactory
from fparser.two.symbol_table import SYMBOL_TABLES
from psyclone.configuration import Config
from psyclone.psyir.backend.fortran import FortranWriter
from psyclone.psyir.frontend.fortran import FortranReader
from psyclone.tests.gocean_build import GOceanBuild
from psyclone.tests.lfric_build import LFRicBuild
from psyclone.tests.utilities import Compile


# fixtures defined here are available to all tests
@pytest.fixture(scope="module", params=[False, True])
def annexed(request):
    ''' Return the content of params in turn '''
    return request.param


def pytest_addoption(parser):
    ''' Adds command-line options to py.test '''
    # parser is already defined, and we can't rename the argument here
    # (since pytest otherwise fails).
    # pylint: disable=redefined-outer-name
    parser.addoption("--f90", action="store", default="gfortran",
                     help="The Fortran compiler to use")
    parser.addoption("--f90flags", action="store", default="",
                     help="Flags to pass to the Fortran compiler")
    parser.addoption("--compile", action="store_true", default=False,
                     help="run tests for code compilation")
    parser.addoption("--compileopencl", action="store_true", default=False,
                     help="run tests for compilation of OpenCL code")


@pytest.fixture
def have_graphviz():
    ''' Whether or not the system has graphviz installed. Note that this
    only checks for the Python bindings. The underlying library must
    also have been installed for dag generation to work correctly. '''
    try:
        # pylint: disable=import-outside-toplevel, unused-import
        import graphviz
    except ImportError:
        return False
    return True


@pytest.fixture(scope="session", autouse=True)
def setup_psyclone_config():
    '''This per session fixture defines the environment variable
    PSYCLONE_CONFIG to point to the config file included in the
    PSyclone repo. This way all tests will get the same config,
    independent of a potential psyclone config file installed by
    the user.
    '''
    config_file = Config.get_repository_config_file()

    # In case that PSyclone is installed and tested (e.g. GitHub Actions),
    # the 'repository' config file does not exist (since it is
    # installed in a different directory). In that case the standard
    # search path of the Configuration object will find the right
    # config file. So only overwrite the default search path behaviour
    # if the repository config file is actually found:
    if os.path.isfile(config_file):
        os.environ["PSYCLONE_CONFIG"] = config_file


@pytest.fixture(scope="session", autouse=True)
def setup_config_before_constants():
    '''PSyclone will raise an exception if an instance of LFRicConstants
    is created before the config file was read: since some of the values
    of LFRicConstants depend on the config file, we have to make sure that
    a user-specified config file is read before creating an instance of
    LFRicConstants. This flag is set when `Config.load()` is called. But the
    tests will not set this flag. This fixture will make sure that
    the tests will not trigger this exception.
    '''
    Config._HAS_CONFIG_BEEN_INITIALISED = True


@pytest.fixture(name="config_instance", scope="function", autouse=True)
def config_fixture(monkeypatch):
    ''' A fixture that ensures every test gets its own copy of the Config
    'singleton'. Otherwise, settings can leak between tests.

    '''
    orig_config = Config.get()
    new_config = copy.copy(orig_config)
    monkeypatch.setattr(Config, "_instance", new_config)
    yield new_config
    monkeypatch.undo()


@pytest.fixture(scope="function", params=[False, True])
def dist_mem(request, monkeypatch, config_instance):
    ''' Fixture for testing with and without distributed memory. Monkeypatches
    the test-local copy of the Config object (provided by the `config_instance`
    fixture) with the appropriate setting for distributed memory, returns that
    setting and then finally undoes the monkeypatching.

    '''
    monkeypatch.setattr(config_instance, "_distributed_mem", request.param)
    yield request.param
    monkeypatch.undo()


@pytest.fixture(scope="session", autouse=True)
def infra_compile(tmpdir_factory, request):
    '''A per-session initialisation function that sets the compilation flags
    in the Compile class based on command line options for --compile,
    --compileopencl, --f90, --f90flags. Then makes sure that the
    infrastructure files for the dynamo0p3 and gocean APIs are compiled
    (if compilation was enabled).
    '''
    Compile.store_compilation_flags(request.config)

    # Create a temporary directory to store the compiled files.
    # Note that this directory is unique even if compiled in
    # parallel, i.e. each process has its own copy of the
    # compiled infrastructure file, which avoids the problem
    # of synchronisation between the processes.
    tmpdir = tmpdir_factory.mktemp('dynamo_wrapper')
    # This is the first instance created. This will trigger
    # compilation of the infrastructure files.
    LFRicBuild(tmpdir)

    tmpdir = tmpdir_factory.mktemp('dl_esm_inf')
    GOceanBuild(tmpdir)


@pytest.fixture(name="_session_parser", scope="session")
def _session_parser():
    '''
    Creates and returns an fparser object. Since this is expensive we only
    do this once per test session (scope="session" above). This fixture is
    only intended to be used in the 'public' fixture `parser` below.

    TODO #1188 - move this to tests/psyir/frontend/conftest.py.

    '''
    return ParserFactory().create(std="f2008")


@pytest.fixture(scope="function")
def parser(_session_parser):
    '''
    Returns the session fparser object but clears any existing symbol tables
    before doing so.

    TODO #1188 - as part of isolating fparser usage to the PSyIR frontend,
    this fixture will be removed and replaced by the one in
    tests/psyir/frontend/conftest.py.

    Note: If this fixture is not used to get the fparser parse tree but is
    used as just a step in getting the PSyIR, use the fortran_reader fixture
    below.

    '''
    SYMBOL_TABLES.clear()
    return _session_parser


@pytest.fixture(scope="function")
def kernel_outputdir(tmpdir, monkeypatch):
    '''Sets the PSyclone _kernel_output_dir Config parameter to tmpdir.'''
    config = Config.get()
    monkeypatch.setattr(config, "_kernel_output_dir", str(tmpdir))
    return tmpdir


@pytest.fixture(scope="function")
def change_into_tmpdir(tmpdir):
    '''This fixture changes into a temporary working directory,
    and changes automatically back at the end. '''
    prev_dir = os.getcwd()
    os.chdir(os.path.expanduser(tmpdir))
    try:
        yield tmpdir
    finally:
        os.chdir(prev_dir)


@pytest.fixture(scope="function", name="fortran_reader")
def fixture_fortran_reader():
    '''Create and return a FortranReader object with default settings.'''
    return FortranReader()


@pytest.fixture(scope="function", name="fortran_writer")
def fixture_fortran_writer():
    '''Create and return a FortranWriter object with default settings.'''
    return FortranWriter()
