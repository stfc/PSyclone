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
# Author: J. Henrichs, Bureau of Meteorology
# Modified: S. Siso and A. R. Porter, STFC Daresbury Lab

''' Module containing tests for gocean1.0 specific config files.'''

import os
import pytest

from psyclone.configuration import Config, ConfigurationError, GOceanConfig
from psyclone.domain.gocean.transformations import GOConstLoopBoundsTrans
from psyclone.errors import InternalError
from psyclone.generator import main
from psyclone.gocean1p0 import GOLoop
from psyclone.tests.utilities import get_invoke


@pytest.fixture(scope="function", autouse=True)
def clear_config_instance():
    ''' The tests in this module all assume that there is no pre-existing
    Config object, so this fixture ensures that the config instance is
    deleted before and after each test function. The latter makes sure that
    any other test executed next will automatically reload the default
    config file.
    '''

    # Enforce loading of the default config file
    Config._instance = None

    # Now execute all tests
    yield

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
def test_invalid_config_files(tmpdir):
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
    config_file = tmpdir.join("config1")
    with config_file.open(mode="w") as new_cfg:
        new_cfg.write(content)
        new_cfg.close()

        config = Config()
        with pytest.raises(ConfigurationError) as err:
            config.load(str(config_file))
        assert "An iteration space must be in the form" in str(err.value)
        assert "But got \"a:b\"" in str(err.value)

    # Try a multi-line specification to make sure all lines are tested
    content = _CONFIG_CONTENT + "iteration-spaces=a:b:c:1:2:3:4\n        d:e"
    config_file = tmpdir.join("config2")
    with config_file.open(mode="w") as new_cfg:
        new_cfg.write(content)
        new_cfg.close()

        config = Config()
        with pytest.raises(ConfigurationError) as err:
            config.load(str(config_file))
        assert "An iteration space must be in the form" in str(err.value)
        assert "But got \"d:e\"" in str(err.value)

    # Invalid {} expression in first loop bound
    content = _CONFIG_CONTENT + "iteration-spaces=a:b:c:{X}:2:3:4"
    config_file = tmpdir.join("config3")
    with config_file.open(mode="w") as new_cfg:
        new_cfg.write(content)
        new_cfg.close()

        config = Config()
        with pytest.raises(ConfigurationError) as err:
            config.load(str(config_file))
        assert "Only '{start}' and '{stop}' are allowed as bracketed "\
               "expression in an iteration space." in str(err.value)
        assert "But got {X}" in str(err.value)

    # Invalid {} expression in last loop bound:
    content = _CONFIG_CONTENT + "iteration-spaces=a:b:c:1:2:3:{Y}"
    config_file = tmpdir.join("config4")
    with config_file.open(mode="w") as new_cfg:
        new_cfg.write(content)
        new_cfg.close()

        config = Config()
        with pytest.raises(ConfigurationError) as err:
            config.load(str(config_file))
        assert "Only '{start}' and '{stop}' are allowed as bracketed "\
               "expression in an iteration space." in str(err.value)
        assert "But got {Y}" in str(err.value)

    # Add an invalid key:
    content = _CONFIG_CONTENT + "invalid-key=value"
    config_file = tmpdir.join("config5")
    with config_file.open(mode="w") as new_cfg:
        new_cfg.write(content)
        new_cfg.close()

        config = Config()
        with pytest.raises(ConfigurationError) as err:
            config.load(str(config_file))
        assert (f"Invalid key 'invalid-key' found in '{config_file}'."
                in str(err.value))

        for i in ["DEFAULTAPI", "DEFAULTSTUBAPI", "DISTRIBUTED_MEMORY",
                  "REPRODUCIBLE_REDUCTIONS"]:
            # They keys are returned in lower case
            assert i.lower() in config.get_default_keys()

    with pytest.raises(InternalError) as err:
        GOLoop.add_bounds(1)
    # Different error message (for type) in python2 vs python3:
    assert "The parameter 'bound_info' must be a string, got '1' "\
           "(type <type 'int'>)" in str(err.value) or \
           "The parameter 'bound_info' must be a string, got '1' "\
           "(type <class 'int'>)" in str(err.value)

    # Test syntactically invalid loop boundaries
    with pytest.raises(ConfigurationError) as err:
        GOLoop.add_bounds("offset:field:space:1(:2:3:4")
    assert "Expression '1(' is not a valid do loop boundary" in str(err.value)
    with pytest.raises(ConfigurationError) as err:
        GOLoop.add_bounds("offset:field:space:1:2:3:4+")
    assert "Expression '4+' is not a valid do loop boundary" in str(err.value)

    # Test invalid field properties - too many fields
    content = _CONFIG_CONTENT + "grid-properties = a: {0}%%b:c:d:e"
    config_file = tmpdir.join("config1")
    with config_file.open(mode="w") as new_cfg:
        new_cfg.write(content)
        new_cfg.close()

        config = Config()
        with pytest.raises(ConfigurationError) as err:
            config.load(str(config_file))
        assert ("Invalid property 'a' found with value '{0}%b:c:d:e'"
                in str(err.value))

    # Test invalid field properties - not enough fields
    content = _CONFIG_CONTENT + "grid-properties = a:b"
    config_file = tmpdir.join("config1")
    with config_file.open(mode="w") as new_cfg:
        new_cfg.write(content)
        new_cfg.close()

        config = Config()
        with pytest.raises(ConfigurationError) as err:
            config.load(str(config_file))
        assert "Invalid property 'a' found with value 'b'" in str(err.value)

    # Test missing required values
    content = _CONFIG_CONTENT + "grid-properties = a:b:array:real"
    config_file = tmpdir.join("config1")
    with config_file.open(mode="w") as new_cfg:
        new_cfg.write(content)
        new_cfg.close()

        config = Config()
        with pytest.raises(ConfigurationError) as err:
            config.load(str(config_file))
        # The config file {0} does not contain values for "..."
        assert ("does not contain values for the following, mandatory grid "
                "property: 'go_grid_xstop'" in str(err.value))


def test_debug_mode(tmpdir):
    '''Test creation of GOcean debug_mode congifuration.
    '''
    _CONFIG_CONTENT = '''\
    [DEFAULT]
    DEFAULTAPI = dynamo0.3
    DEFAULTSTUBAPI = dynamo0.3
    DISTRIBUTED_MEMORY = true
    REPRODUCIBLE_REDUCTIONS = false
    REPROD_PAD_SIZE = 8
    [gocean1.0]
    '''

    # Test with invalid debug mode
    content = _CONFIG_CONTENT + "DEBUG_MODE = 4"
    config_file = tmpdir.join("config1")
    with config_file.open(mode="w") as new_cfg:
        new_cfg.write(content)
        new_cfg.close()

        config = Config()
        with pytest.raises(ConfigurationError) as err:
            config.load(str(config_file))
        assert ("error while parsing DEBUG_MODE in the [gocean1p0] section "
                "of the config file: Not a boolean") in str(err.value)

    # Test with debug mode True
    content = _CONFIG_CONTENT + "DEBUG_MODE = true"
    config_file = tmpdir.join("config2")
    with config_file.open(mode="w") as new_cfg:
        new_cfg.write(content)
        new_cfg.close()

        config = Config()
        config.load(str(config_file))
        api_config = config.api_conf("gocean1.0")
        assert api_config.debug_mode is True

    # Test with debug mode False
    content = _CONFIG_CONTENT + "DEBUG_MODE = false"
    config_file = tmpdir.join("config3")
    with config_file.open(mode="w") as new_cfg:
        new_cfg.write(content)
        new_cfg.close()

        config = Config()
        config.load(str(config_file))
        api_config = config.api_conf("gocean1.0")
        assert api_config.debug_mode is False

    # Test that if DEBUG_MODE key doesn't exist it defaults to False
    content = _CONFIG_CONTENT
    config_file = tmpdir.join("config4")
    with config_file.open(mode="w") as new_cfg:
        new_cfg.write(content)
        new_cfg.close()

        config = Config()
        config.load(str(config_file))
        api_config = config.api_conf("gocean1.0")
        assert api_config.debug_mode is False


# =============================================================================
def test_properties():
    '''Test creation of properties.
    '''

    config = Config.get()
    api_config = config.api_conf("gocean1.0")

    all_props = api_config.grid_properties

    assert all_props["go_grid_area_t"].fortran == "{0}%grid%area_t"
    assert all_props["go_grid_area_t"].type == "array"
    assert all_props["go_grid_area_t"].intrinsic_type == "real"

    with pytest.raises(InternalError) as error:
        new_prop = GOceanConfig.make_property("my_fortran", "my_type",
                                              "integer")
    assert "Type must be 'array' or 'scalar' but is 'my_type'" \
        in str(error.value)

    with pytest.raises(InternalError) as error:
        new_prop = GOceanConfig.make_property("my_fortran", "scalar",
                                              "my_intrinsic")
    assert "Intrinsic type must be 'integer' or 'real' but is 'my_intrinsic'" \
        in str(error.value)

    new_prop = GOceanConfig.make_property("my_fortran", "array", "integer")
    assert new_prop.fortran == "my_fortran"
    assert new_prop.type == "array"
    assert new_prop.intrinsic_type == "integer"

    new_prop = GOceanConfig.make_property("my_fortran", "scalar", "real")
    assert new_prop.fortran == "my_fortran"
    assert new_prop.type == "scalar"
    assert new_prop.intrinsic_type == "real"


# =============================================================================
def test_valid_config_files():
    ''' Test if valid config files lead to the expected new loop boundaries
    '''
    config_file = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                               "test_files", "gocean1p0",
                               "new_iteration_space.psyclone")

    Config.get().load(config_file)

    psy, invoke = get_invoke("new_iteration_space.f90", "gocean1.0", idx=0)
    # This test expects constant loop bounds
    ctrans = GOConstLoopBoundsTrans()
    ctrans.apply(invoke.schedule)

    gen = str(psy.gen)
    new_loop1 = '''\
      DO j = 1, 2, 1
        DO i = 3, 4, 1
          CALL compute_kern1_code(i, j, cu_fld%data, p_fld%data, u_fld%data)
        END DO
      END DO'''
    assert new_loop1 in gen

    new_loop2 = '''\
      DO j = 2, jstop, 1
        DO i = 1, istop + 1, 1
          CALL compute_kern2_code(i, j, cu_fld%data, p_fld%data, u_fld%data)
        END DO
      END DO'''
    assert new_loop2 in gen

    # The third kernel tests {start} and {stop}
    new_loop3 = '''\
      DO j = 2 - 2, 1, 1
        DO i = istop, istop + 1, 1
          CALL compute_kern3_code(i, j, cu_fld%data, p_fld%data, u_fld%data)
        END DO
      END DO'''
    assert new_loop3 in gen

    # Note that this file can not be compiled, since the new iteration space
    # is not defined in any fortran file, so the line:
    # integer :: ITERATES_OVER = internal_ns_halo
    # causes the compilation to abort.
    # assert GOcean1p0Build(tmpdir).code_compiles(psy)
