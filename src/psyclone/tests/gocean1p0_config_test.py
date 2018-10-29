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

from psyclone.configuration import Config, ConfigurationError
from psyclone.generator import main
from psyclone.gocean1p0 import GOLoop
from psyclone.psyGen import InternalError


def teardown_function():
    '''This teardown function is called at the end of all tests and makes
    sure that we wipe the Config object so we get a fresh/default one
    for any further test (and not a left-over one from a test here).
    '''
    # Enforce loading of the default config file
    Config._instance = None


# =============================================================================
def test_command_line(capsys):
    '''Tests that the config command line flag works as expected.
    '''
    f90_file = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                            "test_files", "gocean1p0",
                            "test27_loop_swap.f90")

    # Get the full path to the gocean1.0 config file that adds
    # new iteration spaces for tests.
    config_file = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                               "test_files", "gocean1p0",
                               "new_iteration_space.psyclone")

    options = ["-api", "gocean1.0"]

    # Make sure we always trigger the GOLoop.setup_bounds()
    # in the constructor so that part is always tested!
    GOLoop._bounds_lookup = {}
    # Check that --config with a parameter is accepted
    main(options+["--config", config_file, f90_file])

    # Check that a missing parameter raises an error:
    with pytest.raises(SystemExit):
        main(options+["--config", f90_file])
    _, outerr = capsys.readouterr()

    # Python 2 has the first error message, python 3 the second :()
    assert "too few arguments" in str(outerr) or \
           "the following arguments are required:" in str(outerr)


# =============================================================================
def test_invalid_config_files():
    ''' Test various error conditions.
    '''

    # Valid configuration file without iteration spaces. We add several
    # iteration spaces to it to test for various error conditions
    # pylint: disable=invalid-name
    # pylint: disable=too-many-statements
    _CONFIG_CONTENT = '''\
    [DEFAULT]
    DEFAULTAPI = dynamo0.3
    DEFAULTSTUBAPI = dynamo0.3
    DISTRIBUTED_MEMORY = true
    REPRODUCIBLE_REDUCTIONS = false
    REPROD_PAD_SIZE = 8
    [gocean1.0]
    '''
    # Create a config files with gocean1.0 section, but an
    # invalid iteration space:
    content = _CONFIG_CONTENT + "iteration-spaces=a:b"
    with tempfile.NamedTemporaryFile(delete=False, mode="w") as new_cfg:
        new_name = new_cfg.name
        new_cfg.write(content)
        new_cfg.close()

        config = Config()
        with pytest.raises(ConfigurationError) as err:
            config.load(new_name)
        assert "An iteration space must be in the form" in str(err)
        assert "But got \"a:b\"" in str(err)

    # Try a multi-line specification to make sure all lines are tested
    content = _CONFIG_CONTENT + "iteration-spaces=a:b:c:1:2:3:4\n        d:e"
    with tempfile.NamedTemporaryFile(delete=False, mode="w") as new_cfg:
        new_name = new_cfg.name
        new_cfg.write(content)
        new_cfg.close()

        config = Config()
        with pytest.raises(ConfigurationError) as err:
            config.load(new_name)
        assert "An iteration space must be in the form" in str(err)
        assert "But got \"d:e\"" in str(err)

    # Invalid {} expression in first loop bound
    content = _CONFIG_CONTENT + "iteration-spaces=a:b:c:{X}:2:3:4"
    with tempfile.NamedTemporaryFile(delete=False, mode="w") as new_cfg:
        new_name = new_cfg.name
        new_cfg.write(content)
        new_cfg.close()

        config = Config()
        with pytest.raises(ConfigurationError) as err:
            config.load(new_name)
        assert "Only '{start}' and '{stop}' are allowed as bracketed "\
               "expression in an iteration space." in str(err)
        assert "But got {X}" in str(err)

    # Invalid {} expression in last loop bound:
    content = _CONFIG_CONTENT + "iteration-spaces=a:b:c:1:2:3:{Y}"
    with tempfile.NamedTemporaryFile(delete=False, mode="w") as new_cfg:
        new_name = new_cfg.name
        new_cfg.write(content)
        new_cfg.close()

        config = Config()
        with pytest.raises(ConfigurationError) as err:
            config.load(new_name)
        assert "Only '{start}' and '{stop}' are allowed as bracketed "\
               "expression in an iteration space." in str(err)
        assert "But got {Y}" in str(err)

    # Add an invalid key:
    content = _CONFIG_CONTENT + "invalid-key=value"
    with tempfile.NamedTemporaryFile(delete=False, mode="w") as new_cfg:
        new_name = new_cfg.name
        new_cfg.write(content)
        new_cfg.close()

        config = Config()
        with pytest.raises(ConfigurationError) as err:
            config.load(new_name)
        assert "Invalid key \"invalid-key\" found in \"{0}\".".\
            format(new_name) in str(err)

        for i in ["DEFAULTAPI", "DEFAULTSTUBAPI", "DISTRIBUTED_MEMORY",
                  "REPRODUCIBLE_REDUCTIONS"]:
            # They keys are returned in lower case
            assert i.lower() in config.get_default_keys()

    with pytest.raises(InternalError) as err:
        GOLoop.add_bounds(1)
    # Different error message (for type) in python2 vs python3:
    assert "The parameter 'bound_info' must be a string, got '1' "\
           "(type <type 'int'>)" in str(err) or \
           "The parameter 'bound_info' must be a string, got '1' "\
           "(type <class 'int'>)" in str(err)

    # Test syntactically invalid loop boundaries
    with pytest.raises(ConfigurationError) as err:
        GOLoop.add_bounds("offset:field:space:1(:2:3:4")
    assert "Expression '1(' is not a valid do loop boundary" in str(err)
    with pytest.raises(ConfigurationError) as err:
        GOLoop.add_bounds("offset:field:space:1:2:3:4+")
    assert "Expression '4+' is not a valid do loop boundary" in str(err)


# =============================================================================
def test_valid_config_files():
    ''' Test if valid config files lead to the expected new loop boundaries
    '''
    from psyclone_test_utils import get_invoke

    config_file = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                               "test_files", "gocean1p0",
                               "new_iteration_space.psyclone")

    Config.get().load(config_file)

    psy, _ = get_invoke("new_iteration_space.f90", "gocean1.0", idx=0)

    gen = str(psy.gen)
    # "# nopep8" suppresses the pep8 warning about trailing white space at end of
    # line (after the "END DO ")
    new_loop1 = '''      DO j=1,2
        DO i=3,4
          CALL compute_kern1_code(i, j, cu_fld%data, p_fld%data, u_fld%data)
        END DO 
      END DO '''   # nopep8
    assert new_loop1 in gen

    new_loop2 = '''      DO j=2,jstop
        DO i=1,istop+1
          CALL compute_kern2_code(i, j, cu_fld%data, p_fld%data, u_fld%data)
        END DO 
      END DO '''   # nopep8
    assert new_loop2 in gen

    # The third kernel tests {start} and {stop}
    new_loop3 = '''      DO j=2-2,1
        DO i=istop,istop+1
          CALL compute_kern3_code(i, j, cu_fld%data, p_fld%data, u_fld%data)
        END DO 
      END DO '''   # nopep8
    assert new_loop3 in gen
