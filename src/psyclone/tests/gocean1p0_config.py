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
# Author J. Henrichs, Bureau of Meteorology

''' Module containing tests for gocean1.0 specific config files.'''

from __future__ import absolute_import

import os
import tempfile
import pytest

from psyclone.configuration import ConfigurationError
from psyclone.generator import main
from psyclone.gocean1p0 import GOReadConfigFile


def xtest_command_line(capsys):
    '''Tests that the config command line flag works as expected.
    '''
    f90_file = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                            "test_files", "gocean1p0",
                            "test27_loop_swap.f90")

    # Using "" adds the directory separator to the end:
    config_file = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                               "test_files", "gocean1p0",
                               "new_iteration_space.psyclone")

    options = ["-api", "gocean1.0"]

    # Check that --config with a parameter is accepted
    main(options+["--config", config_file, f90_file])

    # Check that a missing parameter raises an error:
    with pytest.raises(SystemExit):
        main(options+["--config", f90_file])
    _, outerr = capsys.readouterr()

    assert "too few arguments" in str(outerr)


# =============================================================================
def xtest_invalid_config_files():
    ''' Test various error conditions.
    '''

    # Create a config file without any sections
    content = "COMPUTE_ANNEXED_DOFS = false\n"
    with tempfile.NamedTemporaryFile(delete=False, mode="w") as new_cfg:
        new_name = new_cfg.name
        new_cfg.write(content)
        new_cfg.close()

        with pytest.raises(ConfigurationError) as err:
            GOReadConfigFile.read_config_file(new_name)
        assert "ConfigParser failed to read the configuration file '{0}'. Is "\
               "it formatted correctly? (Error was: File contains no section "\
               "headers".format(new_name) in str(err)

    # Create a config file with an incorrect section
    content = "[not-gocean1.0]\nkey=value\n"
    with tempfile.NamedTemporaryFile(delete=False, mode="w") as new_cfg:
        new_name = new_cfg.name
        new_cfg.write(content)
        new_cfg.close()

        with pytest.raises(ConfigurationError) as err:
            GOReadConfigFile.read_config_file(new_name)
        assert "Configuration file '{0}' does not contain a "\
               "'gocean1.0' section".format(new_name) in str(err)

    # Create a config files with gocean1.0 section, but an unsupported key:
    content = "[gocean1.0]\nkey=value\n"
    with tempfile.NamedTemporaryFile(delete=False, mode="w") as new_cfg:
        new_name = new_cfg.name
        new_cfg.write(content)
        new_cfg.close()

        with pytest.raises(ConfigurationError) as err:
            GOReadConfigFile.read_config_file(new_name)
        assert "Invalid key 'key' in file '{0}'".format(new_name) in str(err)


# =============================================================================
def test_valid_config_files():
    ''' Test if valid config files lead to the expected new loop boundaries
    '''
    from utils import get_invoke

    # Using "" adds the directory separator to the end:
    config_file = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                               "test_files", "gocean1p0",
                               "new_iteration_space.psyclone")

    GOReadConfigFile.read_config_file(config_file)

    psy, _ = get_invoke(
        os.path.join("gocean1p0", "new_iteration_space.f90"), "gocean1.0",
        idx=0)

    gen = str(psy.gen)
    new_loop1 = '''      DO j=3,4
        DO i=1,2
          CALL compute_kern1_code(i, j, cu_fld%data, p_fld%data, u_fld%data)
        END DO 
      END DO '''
    assert new_loop1 in gen

    # pylint: disable=
    new_loop2 = '''      DO j=1,jstop+1
        DO i=1,istop
          CALL compute_kern2_code(i, j, cu_fld%data, p_fld%data, u_fld%data)
        END DO 
      END DO '''
    assert new_loop2 in gen
