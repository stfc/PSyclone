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
# Author: R. W. Ford, STFC Daresbury Lab
# Modified by J. Henrichs, Bureau of Meteorology
# Modified by A. R. Porter, S. Siso and N. Nobre, STFC Daresbury Lab
# Modified by I. Kavcic, Met Office


'''
A module to perform pytest unit and functional tests on the code in
the generator.py file. This includes the generate and the main
functions.
'''

import os
import re
import shutil
import stat
from sys import modules

import pytest

from fparser.common.readfortran import FortranStringReader
from fparser.two.parser import ParserFactory

from psyclone import generator
from psyclone.alg_gen import NoInvokesError
from psyclone.configuration import Config
from psyclone.domain.lfric import LFRicConstants
from psyclone.errors import GenerationError
from psyclone.generator import (
    generate, main, check_psyir, add_builtins_use)
from psyclone.parse.algorithm import parse
from psyclone.parse.utils import ParseError
from psyclone.profiler import Profiler
from psyclone.psyGen import PSyFactory
from psyclone.psyir.frontend.fortran import FortranReader
from psyclone.psyir.transformations import LoopFuseTrans
from psyclone.version import __VERSION__


BASE_PATH = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                         "test_files")
NEMO_BASE_PATH = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                              "nemo", "test_files")
DYN03_BASE_PATH = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                               "test_files", "dynamo0p3")
GOCEAN_BASE_PATH = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                                "test_files", "gocean1p0")


def delete_module(modname):
    '''A function to remove a module from Python's internal modules
    list. This is useful as some tests affect others by importing
    modules.

    '''
    del modules[modname]
    for mod in modules.values():
        try:
            delattr(mod, modname)
        except AttributeError:
            pass


def teardown_function():
    '''This teardown function is called at the end of each test and makes
    sure that we wipe the Config object so we get a fresh/default one
    for any further test (and not a left-over one from a test here).

    '''
    Config._instance = None


# handle_script() tests

def test_script_file_not_found():
    '''Checks that handle_script() in generator.py raises the expected
    exception when a script file is supplied that does not exist. This test
    uses the generate() function to call handle_script as this is a simple way
    to create its required arguments.

    '''
    with pytest.raises(GenerationError) as error:
        _, _ = generate(
            os.path.join(BASE_PATH, "dynamo0p3", "1_single_invoke.f90"),
            api="dynamo0.3", script_name="non_existent.py")
    assert "script file 'non_existent.py' not found" in str(error.value)


def test_script_file_no_extension():
    '''Checks that handle_script() in generator.py raises the expected
    exception when a script file does not have an extension. This test
    uses the generate() function to call handle_script as this is a
    simple way to create its required arguments.

    '''
    with pytest.raises(GenerationError) as error:
        _, _ = generate(
            os.path.join(BASE_PATH, "dynamo0p3", "1_single_invoke.f90"),
            api="dynamo0.3",
            script_name=os.path.join(BASE_PATH, "dynamo0p3",
                                     "invalid_script_name"))
    assert ("expected the script file 'invalid_script_name' to have the "
            "'.py' extension" in str(error.value))


def test_script_file_wrong_extension():
    '''Checks that handle_script() in generator.py raises the expected
    exception when a script file does not have the '.py' extension. This test
    uses the generate() function to call handle_script as this is a simple way
    to create its required arguments.

    '''
    with pytest.raises(GenerationError) as error:
        _, _ = generate(
            os.path.join(BASE_PATH, "dynamo0p3", "1_single_invoke.f90"),
            api="dynamo0.3",
            script_name=os.path.join(BASE_PATH, "dynamo0p3",
                                     "1_single_invoke.f90"))
    assert ("expected the script file '1_single_invoke.f90' to have the '.py' "
            "extension" in str(error.value))


def test_script_invalid_content():
    '''Checks that handle_script() in generator.py raises the expected
    exception when a script file does not contain valid python. This
    test uses the generate() function to call handle_script as this is
    a simple way to create its required arguments.

    '''
    with pytest.raises(GenerationError) as error_syntax:
        _, _ = generate(
            os.path.join(BASE_PATH, "dynamo0p3", "1_single_invoke.f90"),
            api="dynamo0.3", script_name=os.path.join(BASE_PATH, "dynamo0p3",
                                                      "error_syntax.py"))
    assert ("attempted to import specified PSyclone transformation module "
            "'error_syntax' but a problem was found: "
            in str(error_syntax.value))

    with pytest.raises(GenerationError) as error_import:
        _, _ = generate(
            os.path.join(BASE_PATH, "dynamo0p3", "1_single_invoke.f90"),
            api="dynamo0.3", script_name=os.path.join(BASE_PATH, "dynamo0p3",
                                                      "error_import.py"))
    assert ("attempted to import specified PSyclone transformation module "
            "'error_import' but a problem was found: "
            in str(error_import.value))


def test_script_invalid_content_runtime():
    '''Checks that handle_script() function in generator.py raises the
    expected exception when a script file contains valid python
    syntactically but produces a runtime exception. This test uses the
    generate() function to call handle_script as this is a simple way
    to create its required arguments.

    '''
    with pytest.raises(GenerationError) as error:
        _, _ = generate(
            os.path.join(BASE_PATH, "dynamo0p3", "1_single_invoke.f90"),
            api="dynamo0.3",
            script_name=os.path.join(
                BASE_PATH, "dynamo0p3", "runtime_error.py"))
    assert ("raised the following exception during execution..."
            in str(error.value))
    assert ("line 3, in trans\n"
            "    psy = b\n" in str(error.value))
    assert ("    NameError: name 'b' is not defined\n"
            "}\n"
            "please check your script" in str(error.value))


def test_script_no_trans():
    '''Checks that handle_script() function in generator.py raises the
    expected exception when a script file does not contain a trans()
    function. This test uses the generate() function to call
    handle_script as this is a simple way to create its required
    arguments.

    '''
    with pytest.raises(GenerationError) as error:
        _, _ = generate(
            os.path.join(BASE_PATH, "dynamo0p3", "1_single_invoke.f90"),
            api="dynamo0.3",
            script_name=os.path.join(
                BASE_PATH, "dynamo0p3", "no_trans.py"))
    assert ("attempted to use specified PSyclone transformation module "
            "'no_trans' but it does not contain a 'trans' function"
            in str(error.value))


def test_script_no_trans_alg():
    '''Checks that handle_script() function in generator.py does not raise
    an exception when a script file does not contain a trans_alg()
    function as these are optional. At the moment this function is
    only supported in the gocean1.0 API. This test uses the generate()
    function to call handle_script as this is a simple way to create
    its required arguments.

    '''
    _, _ = generate(
        os.path.join(BASE_PATH, "gocean1p0", "single_invoke.f90"),
        api="gocean1.0",
        script_name=os.path.join(BASE_PATH, "gocean1p0", "script.py"))


# a set of unit tests for the generate function

def test_non_existent_filename():
    '''Checks that alg_gen raises appropriate error when a non-existent
    filename is supplied.

    '''
    with pytest.raises(IOError):
        generate("non_existent_file.f90")


def test_invalid_api():
    '''Checks that alg_gen raises appropriate error when an invalid api
    is supplied.

    '''
    with pytest.raises(GenerationError):
        generate(os.path.join(BASE_PATH, "dynamo0p3", "1_single_invoke.f90"),
                 api="invalid")


def test_invalid_kernel_paths():
    '''Checks that alg_gen raises appropriate error when an invalid search
    path for kernel source files is supplied, even if a valid path is
    also supplied.

    '''
    with pytest.raises(IOError) as info:
        generate(os.path.join(BASE_PATH, "dynamo0p3", "1_single_invoke.f90"),
                 api="dynamo0.3",
                 kernel_paths=[
                     os.path.join(BASE_PATH, "dynamo0p3"), "does_not_exist"])
    assert "Kernel search path 'does_not_exist' not found" in str(info.value)


def test_wrong_kernel_paths():
    '''Checks that alg_gen raises appropriate error when the kernel code
    cannot be found in the specified search path.

    '''
    with pytest.raises(ParseError):
        generate(os.path.join(BASE_PATH, "dynamo0p3",
                              "1.1.0_single_invoke_xyoz_qr.f90"),
                 api="dynamo0.3",
                 kernel_paths=[os.path.join(BASE_PATH, "gocean1p0")])


def test_correct_kernel_paths():
    '''Checks that alg_gen succeeds when the location of the kernel source
    code is *not* the same as that of the algorithm code. Also adds a
    path that does not contain the required kernel.

    '''
    _, _ = generate(
        os.path.join(BASE_PATH, "dynamo0p3", "1_single_invoke_kern.f90"),
        api="dynamo0.3",
        kernel_paths=[
            os.path.join(BASE_PATH, "dynamo0p3", "kernels", "dead_end"),
            os.path.join(BASE_PATH, "dynamo0p3", "kernels", "in_here")])


def test_same_kernel_paths():
    '''Checks that the generator succeeds when the search directory is the
    same as the algorithm code directory and a path is specified.

    '''
    path = os.path.join(BASE_PATH, "dynamo0p3")
    _, _ = generate(os.path.join(path, "1_single_invoke.f90"),
                    api="dynamo0.3", kernel_paths=[path])


def test_similar_kernel_name():
    '''Checks that the generator does not match incorrect files.'''

    with pytest.raises(ParseError) as info:
        _, _ = generate(
            os.path.join(BASE_PATH, "dynamo0p3", "1_single_invoke.f90"),
            api="dynamo0.3",
            kernel_paths=[os.path.join(BASE_PATH, "dynamo0p3", "kernels",
                                       "dead_end", "no_really")])
    assert ("Kernel file 'testkern_mod.[fF]90' not found in"
            in str(info.value))
    assert "kernels/dead_end/no_really" in str(info.value)


def test_recurse_correct_kernel_paths():
    '''Checks that the generator succeeds when the location of the kernel
    source code is *not* the same as that of the algorithm code and
    recursion through subdirectories is required.

    '''
    _, _ = generate(
        os.path.join(BASE_PATH, "dynamo0p3", "1_single_invoke_kern.f90"),
        api="dynamo0.3",
        kernel_paths=[os.path.join(BASE_PATH, "dynamo0p3", "kernels")])


def test_kernel_parsing_internalerror(capsys):
    '''Checks that the expected output is provided if an internal error is
    caught when parsing a kernel using fparser2.

    '''
    kern_filename = (os.path.join(
        GOCEAN_BASE_PATH, "test30_invalid_kernel_declaration.f90"))
    with pytest.raises(SystemExit):
        main([kern_filename, "-api", "gocean1.0"])
    out, err = capsys.readouterr()
    assert out == ""
    assert "In kernel file " in str(err)
    assert (
        "PSyclone internal error: The argument list ['i', 'j', 'cu', 'p', "
        "'u'] for routine 'compute_code' does not match the variable "
        "declarations:\n"
        "IMPLICIT NONE\n"
        "INTEGER, INTENT(IN) :: I, J\n"
        "REAL(KIND = go_wp), INTENT(OUT), DIMENSION(:, :) :: cu\n"
        "REAL(KIND = go_wp), INTENT(IN), DIMENSION(:, :) :: p\n"
        "(Note that PSyclone does not support implicit declarations.) Specific"
        " PSyIR error is \"Could not find 'u' in the Symbol Table.\".\n"
        in str(err))


def test_script_file_too_short():
    '''Checks that generator.py raises an appropriate error when a script
    file name is too short to contain the '.py' extension.

    '''
    with pytest.raises(GenerationError):
        _, _ = generate(os.path.join(BASE_PATH, "dynamo0p3",
                                     "1_single_invoke.f90"),
                        api="dynamo0.3",
                        script_name=os.path.join(
                            BASE_PATH,
                            "dynamo0p3", "testkern_xyz_mod.f90"))


def test_no_script_gocean():
    '''Test that the generate function in generator.py returns
    successfully if no script is specified for the gocean1.0 api.

    '''
    alg, psy = generate(
        os.path.join(BASE_PATH, "gocean1p0", "single_invoke.f90"),
        api="gocean1.0")
    assert "program single_invoke_test" in alg
    assert "MODULE psy_single_invoke_test" in str(psy)


def test_script_gocean():
    '''Test that the generate function in generator.py returns
    successfully if a script (containing both trans_alg() and trans()
    functions) is specified.

    '''
    _, _ = generate(
        os.path.join(BASE_PATH, "gocean1p0", "single_invoke.f90"),
        api="gocean1.0",
        script_name=os.path.join(BASE_PATH, "gocean1p0", "alg_script.py"))


def test_profile_gocean():
    '''Test that the generate function in generator.py adds profiling
    information if this has been specified.

    '''
    Profiler.set_options(['invokes'], "gocean1.0")
    _, psy = generate(
        os.path.join(BASE_PATH, "gocean1p0", "single_invoke.f90"),
        api="gocean1.0")
    assert "CALL profile_psy_data" in str(psy)
    # Reset the stored options.
    Profiler._options = []


def test_script_attr_error():
    '''Checks that generator.py raises an appropriate error when a script
    file contains a trans() function which raises an attribute
    error. This is what we previously used to check for a script file
    not containing a trans() function.

    '''
    with pytest.raises(GenerationError) as excinfo:
        _, _ = generate(os.path.join(BASE_PATH, "dynamo0p3",
                                     "1_single_invoke.f90"),
                        api="dynamo0.3",
                        script_name=os.path.join(BASE_PATH,
                                                 "dynamo0p3",
                                                 "error_trans.py"))
    assert 'object has no attribute' in str(excinfo.value)


def test_script_null_trans():
    '''Checks that generator.py works correctly when the trans() function
    in a valid script file does no transformations (it simply passes
    input to output). In this case the valid script file has an
    explicit path and must therefore exist at this location.

    '''
    alg1, psy1 = generate(os.path.join(BASE_PATH, "dynamo0p3",
                                       "1_single_invoke.f90"),
                          api="dynamo0.3")
    alg2, psy2 = generate(os.path.join(BASE_PATH, "dynamo0p3",
                                       "1_single_invoke.f90"),
                          api="dynamo0.3",
                          script_name=os.path.join(BASE_PATH,
                                                   "dynamo0p3",
                                                   "null_trans.py"))
    # remove module so we do not affect any following tests
    delete_module("null_trans")
    # we need to remove the first line before comparing output as
    # this line is an instance specific header
    assert '\n'.join(str(alg1).split('\n')[1:]) == \
        '\n'.join(str(alg2).split('\n')[1:])
    assert '\n'.join(str(psy1).split('\n')[1:]) == \
        '\n'.join(str(psy2).split('\n')[1:])


def test_script_null_trans_relative():
    '''Checks that generator.py works correctly when the trans() function
    in a valid script file does no transformations (it simply passes
    input to output). In this case the valid script file contains no
    path and must therefore be found via the PYTHOPATH path list.

    '''
    alg1, psy1 = generate(os.path.join(BASE_PATH, "dynamo0p3",
                                       "1_single_invoke.f90"),
                          api="dynamo0.3")
    # set up the python path so that null_trans.py can be found
    os.sys.path.append(os.path.join(BASE_PATH, "dynamo0p3"))
    alg2, psy2 = generate(os.path.join(BASE_PATH, "dynamo0p3",
                                       "1_single_invoke.f90"),
                          api="dynamo0.3", script_name="null_trans.py")
    # remove imported module so we do not affect any following tests
    delete_module("null_trans")
    os.sys.path.pop()
    # we need to remove the first line before comparing output as
    # this line is an instance specific header
    assert '\n'.join(str(alg1).split('\n')[1:]) == \
        '\n'.join(str(alg2).split('\n')[1:])
    assert str(psy1) == str(psy2)


def test_script_trans_dynamo0p3():
    '''Checks that generator.py works correctly when a transformation is
    provided as a script, i.e. it applies the transformations
    correctly. We use loop fusion as an example.

    '''
    root_path = os.path.dirname(os.path.abspath(__file__))
    base_path = os.path.join(root_path, "test_files", "dynamo0p3")
    # First loop fuse explicitly (without using generator.py)
    parse_file = os.path.join(base_path, "4_multikernel_invokes.f90")
    _, invoke_info = parse(parse_file, api="dynamo0.3")
    psy = PSyFactory("dynamo0.3", distributed_memory=True).create(invoke_info)
    invoke = psy.invokes.get("invoke_0")
    schedule = invoke.schedule
    loop1 = schedule.children[4]
    loop2 = schedule.children[5]
    trans = LoopFuseTrans()
    trans.apply(loop1, loop2)
    generated_code_1 = psy.gen
    # Second loop fuse using generator.py and a script
    _, generated_code_2 = generate(parse_file, api="dynamo0.3",
                                   script_name=os.path.join(
                                       base_path, "loop_fuse_trans.py"))
    # remove module so we do not affect any following tests
    delete_module("loop_fuse_trans")
    # third - check that the results are the same ...
    assert str(generated_code_1) == str(generated_code_2)


def test_api_no_alg():
    ''' Checks that generate works OK for an API (NEMO) which doesn't have
    an Algorithm layer '''
    alg, psy = generate(os.path.join(NEMO_BASE_PATH, "explicit_do.f90"),
                        api="nemo")
    assert alg is None
    assert isinstance(psy, str)
    assert psy.startswith("program")


def test_alg_lines_too_long_tested():
    '''Test that the generate function causes an exception if the
    line_length argument is set to True and the algorithm file has
    lines longer than 132 characters. We use the dynamo0.3 API in this
    case but could have chosen any.

    '''
    alg_filename = os.path.join(DYN03_BASE_PATH, "13_alg_long_line.f90")
    with pytest.raises(ParseError) as excinfo:
        _, _ = generate(alg_filename, api="dynamo0.3", line_length=True)
    assert 'file does not conform' in str(excinfo.value)


def test_alg_lines_too_long_not_tested():
    '''Test that the generate function returns successfully if the
    line_length argument is not set (as it should default to False)
    when the algorithm file has lines longer than 132 characters. We
    use the dynamo0.3 API in this case but could have chosen any.

    '''
    alg_filename = os.path.join(DYN03_BASE_PATH, "13_alg_long_line.f90")
    _, _ = generate(alg_filename, api="dynamo0.3")


def test_kern_lines_too_long_tested():
    '''Test that the generate function raises an exception if the
    line_length argument is set to True and a Kernel file has lines
    longer than 132 characters. We use the dynamo0.3 API in this case
    but could have chosen any.

    '''
    alg_filename = os.path.join(DYN03_BASE_PATH, "13.1_kern_long_line.f90")
    with pytest.raises(ParseError) as excinfo:
        _, _ = generate(alg_filename, api="dynamo0.3", line_length=True)
    assert 'file does not conform' in str(excinfo.value)


def test_kern_lines_too_long_not_tested():
    '''Test that the generate function returns successfully if the
    line_length argument is not set (as it should default to False)
    when a kernel file has lines longer than 132 characters. We use
    the dynamo0.3 API in this case but could have chosen any.

    '''
    alg_filename = os.path.join(DYN03_BASE_PATH, "13.1_kern_long_line.f90")
    _, _ = generate(alg_filename, api="dynamo0.3")


def test_continuators():
    '''Tests that input files with long lines that already have
    continuators to make the code conform to the line length limit do
    not cause an error.

    '''
    _, _ = generate(os.path.join(os.path.dirname(os.path.abspath(__file__)),
                                 "test_files", "dynamo0p3",
                                 "1.1.0_single_invoke_xyoz_qr.f90"),
                    api="dynamo0.3", line_length=True)


def test_main_version(capsys):
    '''Tests that the version info is printed correctly.'''

    # First test if -h includes the right version info:
    for arg in ["-h", "--help"]:
        with pytest.raises(SystemExit):
            main([arg])
        output, _ = capsys.readouterr()
        assert f"Display version information ({__VERSION__})" in output

    for arg in ["-v", "--version"]:
        with pytest.raises(SystemExit) as _:
            main([arg])
        output, _ = capsys.readouterr()
        assert f"PSyclone version: {__VERSION__}" in output


def test_main_profile(capsys):
    '''Tests that the profiling command line flags are working as
    expected.

    '''
    filename = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                            "test_files", "gocean1p0",
                            "test27_loop_swap.f90")

    options = ["-api", "gocean1.0"]

    # Check for invokes only parameter:
    main(options+["--profile", "invokes", filename])
    assert not Profiler.profile_kernels()
    assert Profiler.profile_invokes()

    # Check for kernels only parameter:
    main(options+["--profile", "kernels", filename])
    assert Profiler.profile_kernels()
    assert not Profiler.profile_invokes()

    # Check for routines (aka invokes) + kernels
    main(options+["--profile", "kernels",
                  '--profile', 'routines', filename])
    assert Profiler.profile_kernels()
    assert Profiler.profile_invokes()

    # Check for missing parameter (argparse then
    # takes the filename as parameter for profiler):
    with pytest.raises(SystemExit):
        main(options+["--profile", filename])
    _, outerr = capsys.readouterr()

    correct_re = "invalid choice.*choose from 'invokes', 'routines', 'kernels'"
    assert re.search(correct_re, outerr) is not None

    # Check for invalid parameter
    with pytest.raises(SystemExit):
        main(options+["--profile", "invalid", filename])
    _, outerr = capsys.readouterr()

    assert re.search(correct_re, outerr) is not None

    # Check that 'kernels' is rejected for the 'nemo' API.
    Profiler._options = []
    with pytest.raises(SystemExit):
        main(["-api", "nemo", "--profile", "kernels", filename])
    _, outerr = capsys.readouterr()
    assert ("The 'kernels' automatic profiling option is not compatible with "
            "the 'nemo' API." in outerr)

    # Reset profile flags to avoid further failures in other tests
    Profiler._options = []


def test_main_invalid_api(capsys):
    '''Tests that we get the expected output and the code exits with an
    error if the supplied API is not known.

    '''
    filename = (os.path.join(os.path.dirname(os.path.abspath(__file__)),
                             "test_files", "dynamo0p3",
                             "1_single_invoke.f90"))
    with pytest.raises(SystemExit) as excinfo:
        main([filename, "-api", "madeup"])
    # The error code should be 1
    assert str(excinfo.value) == "1"
    _, output = capsys.readouterr()
    expected_output = ("Unsupported API 'madeup' specified. Supported APIs "
                       "are ['dynamo0.3', 'gocean1.0', 'nemo'].\n")
    assert output == expected_output


def test_main_api():
    '''Tests the three ways of specifying an API: command line, config
    file, or relying on the default.

    '''

    # 1) Make sure if no parameters are given,
    #   config will give us the default API

    # Make sure we get a default config instance
    Config._instance = None
    Config.get()

    assert Config.get().api == Config.get().default_api

    # 2) Check that a command line option will overwrite the default
    filename = (os.path.join(os.path.dirname(os.path.abspath(__file__)),
                             "test_files", "gocean1p0",
                             "single_invoke.f90"))

    assert Config.get().api != "gocean1.0"
    main([filename, "-api", "gocean1.0"])
    assert Config.get().api == "gocean1.0"

    # 3) Check that a config option will overwrite the default
    Config._instance = None
    Config.get()
    # This config file specifies the gocean1.0 as the default API
    config_name = (os.path.join(os.path.dirname(os.path.abspath(__file__)),
                                "test_files", "gocean1p0",
                                "gocean_default.cfg"))
    assert Config.get().api != "gocean1.0"
    main([filename, "--config", config_name])
    assert Config.get().api == "gocean1.0"

    # 4) Check that a command line option overwrites what is specified in
    #    in the config file (and the default)
    Config._instance = None
    Config.get()

    filename = (os.path.join(os.path.dirname(os.path.abspath(__file__)),
                             "test_files", "dynamo0p3", "1_single_invoke.f90"))

    # Check that specifying a config file also sets the
    # HAS_CONFIG_BEEN_INITIALISED flag!
    Config._HAS_CONFIG_BEEN_INITIALISED = False
    # This config file specifies the gocean1.0 api, but
    # command line should take precedence
    main([filename, "--config", config_name, "-api", "dynamo0.3"])
    assert Config.get().api == "dynamo0.3"
    assert Config.has_config_been_initialised() is True


def test_main_directory_arg(capsys):
    '''Test the -d option in main().'''

    # No -d option supplied
    filename = os.path.join(DYN03_BASE_PATH, "1_single_invoke.f90")
    main([filename, "-api", "dynamo0.3"])
    # Invalid -d path supplied
    with pytest.raises(SystemExit):
        main([filename, "-api", "dynamo0.3", "-d", "invalid"])
    _, output = capsys.readouterr()
    assert "Kernel search path 'invalid' not found" in output
    # Multiple -d paths supplied
    main([filename, "-api", "dynamo0.3", "-d", DYN03_BASE_PATH,
          "-d", NEMO_BASE_PATH])


def test_main_disable_backend_validation_arg(capsys):
    '''Test the --backend option in main().'''
    filename = os.path.join(DYN03_BASE_PATH, "1_single_invoke.f90")
    with pytest.raises(SystemExit):
        main([filename, "--backend", "invalid"])
    _, output = capsys.readouterr()
    assert "--backend: invalid choice: 'invalid'" in output

    # Make sure we get a default config instance
    Config._instance = None
    # Default is to have checks enabled.
    assert Config.get().backend_checks_enabled is True
    main([filename, "--backend", "disable-validation"])
    assert Config.get().backend_checks_enabled is False
    Config._instance = None
    main([filename, "--backend", "enable-validation"])
    assert Config.get().backend_checks_enabled is True
    Config._instance = None


def test_main_expected_fatal_error(capsys):
    '''Tests that we get the expected output and the code exits with an
    error when an expected fatal error is returned from the generate
    function.

    '''
    filename = (os.path.join(os.path.dirname(os.path.abspath(__file__)),
                             "test_files", "dynamo0p3",
                             "2_incorrect_number_of_args.f90"))
    with pytest.raises(SystemExit) as excinfo:
        main([filename])
    # the error code should be 1
    assert str(excinfo.value) == "1"
    _, output = capsys.readouterr()
    expected_output = ("Parse Error: Kernel 'testkern_type' called from the "
                       "algorithm layer with an insufficient number of "
                       "arguments as specified by the metadata. Expected at "
                       "least '5' but found '4'.\n")
    assert output == expected_output


def test_generate_trans_error(tmpdir, capsys, monkeypatch):
    '''Test that a TransformationError exception in the generate function
    is caught and output as expected by the main function.  The
    exception is only raised with the new PSyIR approach to modify the
    algorithm layer which is currently in development so is protected
    by a switch. This switch is turned on in this test by
    monkeypatching.

    '''
    monkeypatch.setattr(generator, "LFRIC_TESTING", True)
    code = (
        "module setval_c_mod\n"
        "contains\n"
        "subroutine setval_c()\n"
        "  use psyclone_builtins\n"
        "  use constants_mod, only: r_def\n"
        "  use field_mod, only : field_type\n"
        "  type(field_type) :: field\n"
        "  real(kind=r_def) :: value\n"
        "  call invoke(setval_c(field, value))\n"
        "end subroutine setval_c\n"
        "end module setval_c_mod\n")
    filename = str(tmpdir.join("alg.f90"))
    with open(filename, "w", encoding='utf-8') as my_file:
        my_file.write(code)
    with pytest.raises(SystemExit) as excinfo:
        main([filename])
    # the error code should be 1
    assert str(excinfo.value) == "1"
    _, output = capsys.readouterr()
    # The output is split as the location of the algorithm file varies
    # due to it being stored in a temporary directory by pytest.
    expected_output1 = "Generation Error: In algorithm file '"
    expected_output2 = (
        "alg.f90':\nTransformation Error: Error in RaisePSyIR2LFRicAlgTrans "
        "transformation. The invoke call argument 'setval_c' has been used as"
        " a routine name. This is not allowed.\n")
    assert expected_output1 in output
    assert expected_output2 in output


def test_generate_no_builtin_container(tmpdir, monkeypatch):
    '''Test that a builtin use statement is removed if it has been added
    to a Container (a module). Also tests that everything works OK if
    no use statement is found in a symbol table (as FileContainer does
    not contain one).

    '''
    monkeypatch.setattr(generator, "LFRIC_TESTING", True)
    code = (
        "module test_mod\n"
        "  contains\n"
        "  subroutine test()\n"
        "    use field_mod, only : field_type\n"
        "    type(field_type) :: field\n"
        "    call invoke(setval_c(field, 0.0))\n"
        "  end subroutine test\n"
        "end module\n")
    filename = str(tmpdir.join("alg.f90"))
    with open(filename, "w", encoding='utf-8') as my_file:
        my_file.write(code)
    alg, _ = generate(filename, api="dynamo0.3")
    assert "use _psyclone_builtins" not in alg


def test_main_unexpected_fatal_error(capsys, monkeypatch):
    '''Tests that we get the expected output and the code exits with an
    error when an unexpected fatal error is returned from the generate
    function.

    '''
    # Make sure the attribute VALID_ARG_TYPE_NAMES exist
    # before we modify it.
    _ = LFRicConstants()
    # sabotage the code so one of our constant lists is now an int
    monkeypatch.setattr(LFRicConstants, "VALID_ARG_TYPE_NAMES",
                        value=1)
    filename = (os.path.join(os.path.dirname(os.path.abspath(__file__)),
                             "test_files", "dynamo0p3",
                             "1_single_invoke.f90"))
    with pytest.raises(SystemExit) as excinfo:
        main([filename])
    # the error code should be 1
    assert str(excinfo.value) == "1"
    _, output = capsys.readouterr()
    assert ("Error, unexpected exception, please report to the authors:"
            in output)
    assert "Traceback (most recent call last):" in output
    assert "TypeError: argument of type 'int' is not iterable" in output


@pytest.mark.parametrize("limit", ['all', 'output'])
def test_main_fort_line_length(capsys, limit):
    '''Tests that the Fortran line length object works correctly. Without
    the -l option one of the generated psy-layer lines would be longer
    than 132 characters. Since it is in the output code, both the
    'all' and 'output' options should cause the limit to be
    applied.

    '''
    filename = (os.path.join(os.path.dirname(os.path.abspath(__file__)),
                             "test_files", "dynamo0p3",
                             "10.3_operator_different_spaces.f90"))
    main([filename, '-api', 'dynamo0.3', '-l', limit])
    output, _ = capsys.readouterr()
    assert all(len(line) <= 132 for line in output.split('\n'))


@pytest.mark.parametrize("limit", [[], ['-l', 'off']])
def test_main_fort_line_length_off(capsys, limit):
    '''Tests that the Fortran line-length limiting is off by default and
    is also disabled by `-l off`. One of the generated psy-layer lines
    should be longer than 132 characters.

    '''
    filename = (os.path.join(os.path.dirname(os.path.abspath(__file__)),
                             "test_files", "dynamo0p3",
                             "10.3_operator_different_spaces.f90"))
    main([filename, '-api', 'dynamo0.3'] + limit)
    output, _ = capsys.readouterr()
    assert not all(len(line) <= 132 for line in output.split('\n'))


def test_main_fort_line_length_output_only(capsys):
    '''Check that the '-l output' option disables the line-length check
    on input files but still limits the line lengths in the
    output.

    '''
    alg_filename = os.path.join(NEMO_BASE_PATH, "explicit_do_long_line.f90")
    # If line-length checking is enabled then we should abort
    with pytest.raises(SystemExit):
        main([alg_filename, '-api', 'nemo', '-l', 'all'])
    _, error = capsys.readouterr()
    assert "does not conform to the specified 132 line length limit" in error
    # If we only mandate that the output be limited then we should be fine
    main([alg_filename, '-api', 'nemo', '-l', 'output'])
    output, _ = capsys.readouterr()
    for line in output.split('\n'):
        assert len(line) <= 132


def test_main_no_invoke_alg_stdout(capsys):
    '''Tests that the main() function outputs the original algorithm input
    file to stdout when the algorithm file does not contain an invoke
    and that it does not produce any psy output.

    '''
    # pass in a kernel file as that has no invokes in it
    kern_filename = (os.path.join(os.path.dirname(os.path.abspath(__file__)),
                                  "test_files", "dynamo0p3",
                                  "testkern_mod.F90"))
    main([kern_filename])
    out, _ = capsys.readouterr()

    with open(kern_filename, encoding="utf8") as kern_file:
        kern_str = kern_file.read()
        expected_output = (
            f"Warning: Algorithm Error: Algorithm file contains no "
            f"invoke() calls: refusing to generate empty PSy code\n"
            f"Transformed algorithm code:\n{kern_str}\n")
        assert expected_output == out


def test_main_write_psy_file(capsys, tmpdir):
    '''Tests that the main() function outputs successfully writes the
    generated psy output to a specified file.

    '''
    alg_filename = (os.path.join(os.path.dirname(os.path.abspath(__file__)),
                                 "test_files", "dynamo0p3",
                                 "1_single_invoke.f90"))

    psy_filename = str(tmpdir.join("psy.f90"))

    main([alg_filename, '-opsy', psy_filename])

    # check psy file is created
    assert os.path.isfile(psy_filename)

    # extract psy file content
    with open(psy_filename, encoding="utf8") as psy_file:
        psy_str = psy_file.read()
        # check content of generated psy file by comparing it with stdout
        main([alg_filename])
        stdout, _ = capsys.readouterr()
        assert psy_str in stdout


def test_main_no_invoke_alg_file(capsys, tmpdir):
    '''Tests that the main() function outputs the original algorithm input
    file to file when the algorithm file does not contain an invoke
    and that it does not produce any psy output.

    '''
    # pass in a kernel file as that has no invokes in it
    kern_filename = (os.path.join(os.path.dirname(os.path.abspath(__file__)),
                                  "test_files", "dynamo0p3",
                                  "testkern_mod.F90"))

    alg_filename = str(tmpdir.join("alg.f90"))
    psy_filename = str(tmpdir.join("psy.f90"))
    # no need to delete the files as they have not been created

    main([kern_filename, '-oalg', alg_filename, '-opsy', psy_filename])
    stdout, _ = capsys.readouterr()

    # check stdout contains warning
    with open(kern_filename, encoding="utf8") as kern_file:
        kern_str = kern_file.read()
        expected_stdout = ("Warning: Algorithm Error: Algorithm file contains "
                           "no invoke() calls: refusing to generate empty PSy "
                           "code\n")
        assert expected_stdout == stdout

    # check alg file has same output as input file
    with open(alg_filename, encoding="utf8") as expected_file:
        expected_alg_str = expected_file.read()
        assert expected_alg_str == kern_str
    os.remove(alg_filename)

    # check psy file is not created
    assert not os.path.isfile(psy_filename)


def test_main_kern_output_no_dir(capsys):
    '''Test for when the specified output directory (for transformed
    kernels) does not exist.

    '''
    alg_filename = (os.path.join(os.path.dirname(os.path.abspath(__file__)),
                                 "test_files", "dynamo0p3",
                                 "1_single_invoke.f90"))
    with pytest.raises(SystemExit) as err:
        main([alg_filename, '-okern', "/does/not/exist"])
    assert str(err.value) == "1"
    _, output = capsys.readouterr()
    assert ("Specified kernel output directory (/does/not/exist) does not "
            "exist" in output)


def test_main_kern_output_no_write(tmpdir, capsys):
    '''Test for when the specified output directory (for transformed
    kernels) cannot be written to.

    '''
    alg_filename = (os.path.join(os.path.dirname(os.path.abspath(__file__)),
                                 "test_files", "dynamo0p3",
                                 "1_single_invoke.f90"))
    # Create a new directory and make it readonly
    new_dir = os.path.join(str(tmpdir), "no_write_access")
    os.mkdir(new_dir)
    os.chmod(new_dir, stat.S_IREAD)
    with pytest.raises(SystemExit) as err:
        main([alg_filename, '-okern', str(new_dir)])
    assert str(err.value) == "1"
    _, output = capsys.readouterr()
    assert (f"Cannot write to specified kernel output directory "
            f"({str(new_dir)})" in output)


def test_main_kern_output_dir(tmpdir):
    '''Test that we can specify a valid kernel output directory.'''

    alg_filename = (os.path.join(os.path.dirname(os.path.abspath(__file__)),
                                 "test_files", "dynamo0p3",
                                 "1_single_invoke.f90"))
    main([alg_filename, '-okern', str(tmpdir)])
    # The specified kernel output directory should have been stored in
    # the configuration object
    assert Config.get().kernel_output_dir == str(tmpdir)

    # If no kernel_output_dir is set, it should default to the
    # current directory
    Config.get().kernel_output_dir = None
    assert Config.get().kernel_output_dir == str(os.getcwd())


def test_invalid_kern_naming():
    '''Check that we raise the expected error if an invalid
    kernel-renaming scheme is supplied.

    '''
    alg_filename = (os.path.join(os.path.dirname(os.path.abspath(__file__)),
                                 "test_files", "dynamo0p3",
                                 "1_single_invoke.f90"))
    # Simply supplying the wrong value on the command line is picked up
    # by the argparse module so we call generate() directly with an
    # incorrect value
    with pytest.raises(GenerationError) as err:
        _, _ = generate(alg_filename, api="dynamo0.3",
                        kern_naming="not-a-scheme")
    assert "Invalid kernel-renaming scheme supplied" in str(err.value)
    assert "but got 'not-a-scheme'" in str(err.value)


def test_main_include_invalid(capsys, tmpdir):
    '''Check that the main function complains if a non-existent location
    is specified as a search path for INCLUDE files.

    '''
    alg_file = (os.path.join(os.path.dirname(os.path.abspath(__file__)),
                             "nemo", "test_files", "include_stmt.f90"))
    fake_path = tmpdir.join('does_not_exist')
    with pytest.raises(SystemExit) as err:
        main([alg_file, '-api', 'nemo', '-I', fake_path.strpath])
    assert str(err.value) == "1"
    capout = capsys.readouterr()
    assert "does_not_exist' does not exist" in capout.err


def test_main_include_path(capsys):
    '''Test that the main function supplies any INCLUDE paths to
    fparser.

    '''
    # This algorithm file INCLUDE's a file that defines a variable called
    # "some_fake_mpi_handle"
    alg_file = (os.path.join(os.path.dirname(os.path.abspath(__file__)),
                             "nemo", "test_files", "include_stmt.f90"))
    # First try without specifying where to find the include file. This
    # is not supported and should raise an error.
    with pytest.raises(SystemExit):
        main([alg_file, '-api', 'nemo'])
    _, err = capsys.readouterr()
    assert ("Found an unresolved Fortran INCLUDE file 'local_mpi.h' while"
            in err)
    # Now specify two locations to search with only the second containing
    # the necessary header file
    inc_path1 = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                             "test_files")
    inc_path2 = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                             "nemo", "test_files", "include_files")
    main([alg_file, '-api', 'nemo', '-I', str(inc_path1),
          '-I', str(inc_path2)])
    stdout, _ = capsys.readouterr()
    assert "some_fake_mpi_handle" in stdout
    # Check that the Config object contains the provided include paths
    assert str(inc_path1) in Config.get().include_paths
    assert str(inc_path2) in Config.get().include_paths


def test_utf_char(tmpdir):
    '''Test that the generate method works OK when both the Algorithm and
    Kernel code contain utf-encoded chars.

    '''
    algfile = os.path.join(str(tmpdir), "alg.f90")
    main([os.path.join(BASE_PATH, "gocean1p0", "test29_utf_chars.f90"),
          "-api", "gocean1.0", "-oalg", algfile])
    # We only check the algorithm layer since we generate the PSy
    # layer from scratch in this API (and thus it contains no
    # non-ASCII characters).
    with open(algfile, "r", encoding="utf8") as afile:
        alg = afile.read().lower()
        assert "max reachable coeff" in alg
        assert "call invoke_0_kernel_utf" in alg
    # Check for NEMO API when processing existing Fortran
    test_file = os.path.join(NEMO_BASE_PATH, "utf_char.f90")
    tmp_file = os.path.join(str(tmpdir), "test_psy.f90")
    main(["-api", "nemo", "-opsy", tmp_file, test_file])
    assert os.path.isfile(tmp_file)


def test_check_psyir():
    '''Tests for the check_psyir utility method.'''

    # multiple program, module etc.
    code = (
        "program test_prog\n"
        "end program\n"
        "subroutine test_sub\n"
        "end subroutine\n")
    psyir = FortranReader().psyir_from_source(code)
    filename = "dummy"
    with pytest.raises(GenerationError) as info:
        check_psyir(psyir, filename)
    assert ("Expecting LFRic algorithm-layer code within file 'dummy' to be "
            "a single program or module, but found '2' of type "
            "['Routine', 'Routine']." in str(info.value))
    # not a program or module
    code = (
        "subroutine test_sub\n"
        "end subroutine\n")
    psyir = FortranReader().psyir_from_source(code)
    with pytest.raises(GenerationError) as info:
        check_psyir(psyir, filename)
    assert ("Expecting LFRic algorithm-layer code within file 'dummy' to be "
            "a single program or module, but found 'Routine'."
            in str(info.value))
    # OK
    code = (
        "program test_sub\n"
        "end\n")
    psyir = FortranReader().psyir_from_source(code)
    check_psyir(psyir, filename)


def test_add_builtins_use():
    '''Tests for the add_builtins_use utility method.'''

    # no spec_part
    code = (
        "program test_prog\n"
        "end program\n")
    parser = ParserFactory().create(std="f2008")
    reader = FortranStringReader(code)
    fp2_tree = parser(reader)
    add_builtins_use(fp2_tree, "my_name")
    assert "USE my_name" in str(fp2_tree)
    # spec_part
    code = (
        "program test_prog\n"
        "  integer :: i\n"
        "end program\n")
    reader = FortranStringReader(code)
    fp2_tree = parser(reader)
    add_builtins_use(fp2_tree, "ANOTHER_NAME")
    assert "USE ANOTHER_NAME" in str(fp2_tree)
    # multiple modules/programs
    code = (
        "program test_prog\n"
        "end program\n"
        "module test_mod1\n"
        "end module\n"
        "module test_mod2\n"
        "end module\n")
    reader = FortranStringReader(code)
    fp2_tree = parser(reader)
    add_builtins_use(fp2_tree, "builtins")
    assert str(fp2_tree) == (
        "PROGRAM test_prog\n  USE builtins\nEND PROGRAM\n"
        "MODULE test_mod1\n  USE builtins\nEND MODULE\n"
        "MODULE test_mod2\n  USE builtins\nEND MODULE")


def test_no_script_lfric_new(monkeypatch):
    '''Test that the generate function in generator.py returns
    successfully if no script is specified for the dynamo0.3 (LFRic)
    api. This test uses the new PSyIR approach to modify the algorithm
    layer which is currently in development so is protected by a
    switch. This switch is turned on in this test by monkeypatching.

    '''
    monkeypatch.setattr(generator, "LFRIC_TESTING", True)
    alg, _ = generate(
        os.path.join(BASE_PATH, "dynamo0p3", "1_single_invoke.f90"),
        api="dynamo0.3")
    # new call replaces invoke
    assert "use single_invoke_psy, only : invoke_0_testkern_type" in alg
    assert "call invoke_0_testkern_type(a, f1, f2, m1, m2)" in alg
    # functor symbol is removed
    assert " testkern_type" not in alg
    # module symbol is removed
    assert "testkern_mod" not in alg
    # _psyclone_builtins symbol (that was added by PSyclone) is removed
    assert "use _psyclone_builtins" not in alg


def test_script_lfric_new(monkeypatch):
    '''Test that the generate function in generator.py returns
    successfully if a script (containing both trans_alg() and trans()
    functions) is specified. This test uses the new PSyIR approach to
    modify the algorithm layer which is currently in development so is
    protected by a switch. This switch is turned on in this test by
    monkeypatching.

    '''
    monkeypatch.setattr(generator, "LFRIC_TESTING", True)
    alg, _ = generate(
        os.path.join(BASE_PATH, "dynamo0p3", "1_single_invoke.f90"),
        api="dynamo0.3",
        script_name=os.path.join(BASE_PATH, "dynamo0p3", "alg_script.py"))
    # new call replaces invoke
    assert "use single_invoke_psy, only : invoke_0_testkern_type" in alg
    assert "call invoke_0_testkern_type(a, f1, f2, m1, m2)" in alg
    # functor symbol is removed
    assert " testkern_type" not in alg
    # module symbol is removed
    assert "testkern_mod" not in alg
    # _psyclone_builtins symbol (that was added by PSyclone) is removed
    assert "use _psyclone_builtins" not in alg


def test_builtins_lfric_new(monkeypatch):
    '''Test that the generate function in generator.py returns
    successfully when the algorithm layer contains a mixture of
    kernels and builtins. This test uses the new PSyIR approach to
    modify the algorithm layer which is currently in development so is
    protected by a switch. This switch is turned on in this test by
    monkeypatching.

    '''
    monkeypatch.setattr(generator, "LFRIC_TESTING", True)
    alg, _ = generate(
        os.path.join(BASE_PATH, "dynamo0p3",
                     "15.1.2_builtin_and_normal_kernel_invoke.f90"),
        api="dynamo0.3")
    # new call replaces invoke
    assert "use single_invoke_builtin_then_kernel_psy, only : invoke_0" in alg
    assert "call invoke_0(f5, f2, f3, f4, scalar, f1)" in alg
    # functor symbols are removed
    assert " testkern_type" not in alg
    assert " testkern_wtheta_type" not in alg
    assert " testkern_w2_only_type" not in alg
    # module symbols are removed
    assert " testkern_mod" not in alg
    assert " testkern_wtheta_mod" not in alg
    assert " testkern_w2_only_mod" not in alg
    # _psyclone_builtins symbol (that was added by PSyclone) is removed
    assert "use _psyclone_builtins" not in alg


def test_no_invokes_lfric_new(monkeypatch):
    '''Test that the generate function in generator.py raises the expected
    exception if the algorithm layer contains no invoke() calls. This
    test uses the new PSyIR approach to modify the algorithm layer
    which is currently in development so is protected by a
    switch. This switch is turned on in this test by monkeypatching.

    '''
    monkeypatch.setattr(generator, "LFRIC_TESTING", True)
    # pass a kernel file as it has no invoke's in it.
    with pytest.raises(NoInvokesError) as info:
        _, _ = generate(
            os.path.join(BASE_PATH, "dynamo0p3", "testkern_mod.F90"),
            api="dynamo0.3")
    assert ("Algorithm file contains no invoke() calls: refusing to generate "
            "empty PSy code" in str(info.value))


@pytest.mark.parametrize("invoke", ["call invoke", "if (.true.) call invoke"])
def test_generate_unresolved_container_lfric(invoke, tmpdir, monkeypatch):
    '''Test that a GenerationError exception in the generate function is
    raised for the LFRic DSL if one of the functors is not explicitly
    declared. This can happen in LFRic algorithm code as it is never
    compiled. The exception is only raised with the new PSyIR approach
    to modify the algorithm layer which is currently in development so
    is protected by a switch. This switch is turned on in this test by
    monkeypatching. Test when the functor is at different levels of
    PSyIR hierarchy to ensure that the name of the parent routine is
    always found.

    At the moment this exception is only raised if the functor is
    declared in a different subroutine or function, as the original
    parsing approach picks up all other cases. However, the original
    parsing approach will eventually be removed.

    '''
    monkeypatch.setattr(generator, "LFRIC_TESTING", True)
    code = (
        f"module some_kernel_mod\n"
        f"use module_mod, only : module_type\n"
        f"contains\n"
        f"subroutine dummy_kernel()\n"
        f" use testkern_mod, only: testkern_type\n"
        f"end subroutine dummy_kernel\n"
        f"subroutine some_kernel()\n"
        f"  use constants_mod, only: r_def\n"
        f"  use field_mod, only : field_type\n"
        f"  type(field_type) :: field1, field2, field3, field4\n"
        f"  real(kind=r_def) :: scalar\n"
        f"  {invoke}(testkern_type(scalar, field1, field2, field3, field4))\n"
        f"end subroutine some_kernel\n"
        f"end module some_kernel_mod\n")
    alg_filename = str(tmpdir.join("alg.f90"))
    with open(alg_filename, "w", encoding='utf-8') as my_file:
        my_file.write(code)
    kern_filename = os.path.join(DYN03_BASE_PATH, "testkern_mod.F90")
    shutil.copyfile(kern_filename, str(tmpdir.join("testkern_mod.F90")))
    with pytest.raises(GenerationError) as info:
        _, _ = generate(alg_filename)
    assert ("Kernel functor 'testkern_type' in routine 'some_kernel' from "
            "algorithm file '" in str(info.value))
    assert ("alg.f90' must be named in a use statement (found ["
            "'constants_mod', 'field_mod', '_psyclone_builtins', "
            "'module_mod']) or be a recognised built-in (one of "
            "['x_plus_y', 'inc_x_plus_y'," in str(info.value))


def test_generate_unresolved_container_gocean(tmpdir):
    '''Test that a GenerationError exception in the generate function is
    raised for the GOcean DSL if one of the functors is not explicitly
    declared. This can happen in GOcean algorithm code as it is never
    compiled.

    At the moment this exception is only raised if the functor is
    declared in a different subroutine or function, as the original
    parsing approach picks up all other cases. However, the original
    parsing approach will eventually be removed.

    '''
    code = (
        "module some_kernel_mod\n"
        "use module_mod, only : module_type\n"
        "contains\n"
        "subroutine dummy_kernel()\n"
        "  use compute_cu_mod,  only: compute_cu\n"
        "end subroutine dummy_kernel\n"
        "subroutine some_kernel()\n"
        "  use kind_params_mod\n"
        "  use grid_mod, only: grid_type\n"
        "  use field_mod, only: r2d_field\n"
        "  type(grid_type), target :: model_grid\n"
        "  type(r2d_field) :: p_fld, u_fld, cu_fld\n"
        "  call invoke( compute_cu(cu_fld, p_fld, u_fld) )\n"
        "end subroutine some_kernel\n"
        "end module some_kernel_mod\n")
    alg_filename = str(tmpdir.join("alg.f90"))
    with open(alg_filename, "w", encoding='utf-8') as my_file:
        my_file.write(code)
    kern_filename = os.path.join(GOCEAN_BASE_PATH, "compute_cu_mod.f90")
    shutil.copyfile(kern_filename, str(tmpdir.join("compute_cu_mod.f90")))
    with pytest.raises(GenerationError) as info:
        _, _ = generate(alg_filename, api="gocean1.0")
    assert ("Kernel functor 'compute_cu' in routine 'some_kernel' from "
            "algorithm file '" in str(info.value))
    assert ("alg.f90' must be named in a use statement (found "
            "['kind_params_mod', 'grid_mod', 'field_mod', 'module_mod'])."
            in str(info.value))
