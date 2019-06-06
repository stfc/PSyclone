# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017-2019, Science and Technology Facilities Council.
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


''' Module which performs pytest set-up so that we can specify
    command-line options '''

from __future__ import absolute_import
import pytest


# fixtures defined here are available to all tests
@pytest.fixture(scope="module", params=[False, True])
def annexed(request):
    ''' Return the content of params in turn '''
    return request.param


@pytest.fixture(scope="module", params=[False, True])
def dist_mem(request):
    ''' Fixture for testing with and without distributed memory.
        Returns the content of params in turn. '''
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
        # pylint: disable=unused-variable
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
    from psyclone.configuration import Config
    import os
    config_file = Config.get_repository_config_file()

    # In case that PSyclone is installed and tested (e.g. travis),
    # the 'repository' config file does not exist (since it is
    # installed in a different directory). In that case the standard
    # search path of the Configuration object will find the right
    # config file. So only overwrite the default search path behaviour
    # if the repository config file is actually found:
    if os.path.isfile(config_file):
        os.environ["PSYCLONE_CONFIG"] = config_file


@pytest.fixture(scope="session", autouse=True)
def infra_compile(tmpdir_factory, request):
    '''A per-session initialisation function that sets the compilation flags
    in the Compile class based on command line options for --compile,
    --compileopencl, --f90, --f90flags. Then makes sure that the
    infrastructure files for the dynamo0p3 and gocean1p0 APIs are compiled
    (if compilation was enabled).
    '''
    from psyclone_test_utils import Compile
    Compile.store_compilation_flags(request.config)

    from dynamo0p3_build import Dynamo0p3Build
    # Create a temporary directory to store the compiled files.
    # Note that this directory is unique even if compiled in
    # parallel, i.e. each process has its own copy of the
    # compiled infrastructure file, which avoids the problem
    # of synchronisation between the processes.
    tmpdir = tmpdir_factory.mktemp('dynamo_wrapper')
    # This is the first instance created. This will trigger
    # compilation of the infrastructure files.
    Dynamo0p3Build(tmpdir)

    from gocean1p0_build import GOcean1p0Build
    tmpdir = tmpdir_factory.mktemp('dl_esm_inf')
    GOcean1p0Build(tmpdir)


@pytest.fixture(scope="session")
def parser():
    '''
    Creates and returns an fparser object. Since this is expensive we only
    do this once per test session (scope="session" above).
    '''
    from fparser.two.parser import ParserFactory
    return ParserFactory().create()
