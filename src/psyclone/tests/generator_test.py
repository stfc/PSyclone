# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017-2020, Science and Technology Facilities Council.
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
# Author R. W. Ford STFC Daresbury Lab
# Modified work Copyright (c) 2018 by J. Henrichs, Bureau of Meteorology
# Modified by A. R. Porter, STFC Daresbury Lab
# Modified by I. Kavcic, Met Office
# Modified by R. W. Ford, STFC Daresbury Lab


'''
A module to perform pytest unit and functional tests on the code in
the generator.py file. This includes the generate and the main
functions.
'''

from __future__ import absolute_import
import os
import re
import pytest
from psyclone.generator import generate, main
from psyclone.errors import GenerationError, InternalError
from psyclone.parse.utils import ParseError
from psyclone.configuration import Config

BASE_PATH = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                         "test_files")
NEMO_BASE_PATH = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                              "nemo", "test_files")
DYN03_BASE_PATH = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                               "test_files", "dynamo0p3")


def delete_module(modname):
    '''a function to remove a module from Python's internal modules
       list. This is useful as some tests affect others by importing
       modules.'''
    from sys import modules
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


# a set of unit tests for the generate function


def test_non_existant_filename():
    ''' checks that alg_gen raises appropriate error when a
    non-existant filename is supplied '''
    with pytest.raises(IOError):
        generate("non_existant_file.f90")


def test_invalid_api():
    ''' checks that alg_gen raises appropriate error when an invalid
        api is supplied '''
    with pytest.raises(GenerationError):
        generate(os.path.join(BASE_PATH, "dynamo0p1", "algorithm",
                              "1_single_function.f90"), api="invalid")


def test_invalid_kernel_path():
    ''' checks that alg_gen raises appropriate error when an invalid
        search path for kernel source files is supplied '''
    with pytest.raises(IOError):
        generate(os.path.join(BASE_PATH, "dynamo0p1", "algorithm",
                              "1_single_function.f90"),
                 api="dynamo0.1",
                 kernel_path="does_not_exist")


def test_wrong_kernel_path():
    ''' checks that alg_gen raises appropriate error when the kernel
        code cannot be found in the specified search path '''
    with pytest.raises(ParseError):
        generate(os.path.join(BASE_PATH, "dynamo0p3",
                              "1.1.0_single_invoke_xyoz_qr.f90"),
                 api="dynamo0.3",
                 kernel_path=os.path.join(BASE_PATH, "gocean0p1"))


def test_correct_kernel_path():
    ''' checks that alg_gen succeeds when the location of the kernel
        source code is *not* the same as that of the algorithm code '''
    _, _ = generate(os.path.join(BASE_PATH, "dynamo0p1", "algorithm",
                                 "1_single_function.f90"),
                    api="dynamo0.1",
                    kernel_path=os.path.join(BASE_PATH,
                                             "dynamo0p1", "kernels"))


def test_same_kernel_path():
    ''' checks that the generator succeeds when the search directory
        is the same as the algorithm code directory and a path is
        specified '''
    path = os.path.join(BASE_PATH, "dynamo0p1", "algorithm")
    _, _ = generate(os.path.join(path, "1_single_function.f90"),
                    api="dynamo0.1", kernel_path=path)


def test_similar_kernel_name():
    ''' checks that the generator does not match incorrect files '''
    _, _ = generate(os.path.join(BASE_PATH, "dynamo0p1",
                                 "algorithm", "1_single_function.f90"),
                    api="dynamo0.1",
                    kernel_path=os.path.join(BASE_PATH,
                                             "dynamo0p1", "kernels2"))


def test_recurse_correct_kernel_path():
    '''checks that the generator succeeds when the location of the kernel
       source code is *not* the same as that of the algorithm code and
       recursion through subdirectories is required'''
    _, _ = generate(os.path.join(BASE_PATH, "dynamo0p1",
                                 "algorithm", "1_single_function.f90"),
                    api="dynamo0.1",
                    kernel_path=os.path.join(BASE_PATH,
                                             "dynamo0p1", "kernels3"))


def test_script_file_not_found():
    ''' checks that generator.py raises an appropriate error when a
        script file is supplied that can't be found in the Python path.
        In this case the script path is supplied'''
    with pytest.raises(IOError):
        _, _ = generate(os.path.join(BASE_PATH, "dynamo0p3",
                                     "1_single_invoke.f90"),
                        api="dynamo0.3", script_name="./non_existant.py")


def test_script_file_not_found_relative():
    ''' checks that generator.py raises an appropriate error when a script
        file is supplied that can't be found in the Python path. In
        this case the script path is not supplied so must be found via the
        PYTHONPATH variable'''
    with pytest.raises(GenerationError):
        _, _ = generate(os.path.join(BASE_PATH, "dynamo0p3",
                                     "1_single_invoke.f90"),
                        api="dynamo0.3", script_name="non_existant.py")


def test_script_file_too_short():
    ''' checks that generator.py raises an appropriate error when a
        script file name is too short to contain the '.py' extension'''
    with pytest.raises(GenerationError):
        _, _ = generate(os.path.join(BASE_PATH, "dynamo0p3",
                                     "1_single_invoke.f90"),
                        api="dynamo0.3",
                        script_name=os.path.join(BASE_PATH,
                                                 "dynamo0p3", "xyz"))


def test_script_file_no_extension():
    ''' checks that generator.py raises an appropriate error when a
        script file does not have an extension'''
    with pytest.raises(GenerationError):
        _, _ = generate(os.path.join(BASE_PATH, "dynamo0p3",
                                     "1_single_invoke.f90"),
                        api="dynamo0.3",
                        script_name=os.path.join(BASE_PATH, "dynamo0p3",
                                                 "invalid_script_name"))


def test_script_file_wrong_extension():
    ''' checks that generator.py raises an appropriate error when a
        script file does not have the '.py' extension'''
    with pytest.raises(GenerationError):
        _, _ = generate(os.path.join(BASE_PATH, "dynamo0p3",
                                     "1_single_invoke.f90"),
                        api="dynamo0.3",
                        script_name=os.path.join(BASE_PATH, "dynamo0p3",
                                                 "1_single_invoke.f90"))


def test_script_invalid_content():
    ''' checks that generator.py raises an appropriate error when a
        script file does not contain valid python '''
    with pytest.raises(GenerationError):
        _, _ = generate(os.path.join(BASE_PATH, "dynamo0p3",
                                     "1_single_invoke.f90"),
                        api="dynamo0.3",
                        script_name=os.path.join(
                            BASE_PATH, "dynamo0p3", "error.py"))


def test_script_invalid_content_runtime():
    ''' checks that generator.py raises an appropriate error when a
        script file contains valid python syntactically but produces a
        runtime exception. '''
    with pytest.raises(GenerationError):
        _, _ = generate(os.path.join(BASE_PATH, "dynamo0p3",
                                     "1_single_invoke.f90"),
                        api="dynamo0.3",
                        script_name=os.path.join(
                            BASE_PATH, "dynamo0p3", "runtime_error.py"))


def test_script_no_trans():
    ''' checks that generator.py raises an appropriate error when a
        script file does not contain a trans() function '''
    with pytest.raises(GenerationError) as excinfo:
        _, _ = generate(os.path.join(BASE_PATH, "dynamo0p3",
                                     "1_single_invoke.f90"),
                        api="dynamo0.3",
                        script_name=os.path.join(BASE_PATH, "dynamo0p3",
                                                 "no_trans.py"))
    assert 'attempted to import' in str(excinfo.value)


def test_script_attr_error():
    ''' checks that generator.py raises an appropriate error when a
        script file contains a trans() function which raises an
        attribute error. This is what we previously used to check for
        a script file not containing a trans() function.'''
    with pytest.raises(GenerationError) as excinfo:
        _, _ = generate(os.path.join(BASE_PATH, "dynamo0p3",
                                     "1_single_invoke.f90"),
                        api="dynamo0.3",
                        script_name=os.path.join(BASE_PATH,
                                                 "dynamo0p3",
                                                 "error_trans.py"))
    assert 'object has no attribute' in str(excinfo.value)


def test_script_null_trans():
    ''' checks that generator.py works correctly when the trans()
        function in a valid script file does no transformations (it
        simply passes input to output). In this case the valid
        script file has an explicit path and must therefore exist at
        this location. '''
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
    ''' checks that generator.py works correctly when the trans()
        function in a valid script file does no transformations (it
        simply passes input to output). In this case the valid
        script file contains no path and must therefore be found via
        the PYTHOPATH path list. '''
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


def test_script_trans():
    ''' Checks that generator.py works correctly when a
        transformation is provided as a script, i.e. it applies the
        transformations correctly. We use loop fusion as an
        example.

    '''
    from psyclone.parse.algorithm import parse
    from psyclone.psyGen import PSyFactory
    from psyclone.transformations import LoopFuseTrans
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
    schedule, _ = trans.apply(loop1, loop2)
    invoke.schedule = schedule
    generated_code_1 = psy.gen
    # Second loop fuse using generator.py and a script
    _, generated_code_2 = generate(parse_file, api="dynamo0.3",
                                   script_name=os.path.join(
                                       base_path, "loop_fuse_trans.py"))
    # remove module so we do not affect any following tests
    delete_module("loop_fuse_trans")
    # third - check that the results are the same ...
    assert str(generated_code_1) == str(generated_code_2)


def test_alg_lines_too_long_tested():
    ''' Test that the generate function causes an exception if the
    line_length argument is set to True and the algorithm file has
    lines longer than 132 characters. We use the dynamo0.3 API in this
    case but could have chosen any. '''
    alg_filename = os.path.join(DYN03_BASE_PATH, "13_alg_long_line.f90")
    with pytest.raises(ParseError) as excinfo:
        _, _ = generate(alg_filename, api="dynamo0.3", line_length=True)
    assert 'file does not conform' in str(excinfo.value)


def test_alg_lines_too_long_not_tested():
    ''' Test that the generate function returns successfully if the
    line_length argument is not set (as it should default to False)
    when the algorithm file has lines longer than 132 characters. We
    use the dynamo0.3 API in this case but could have chosen any.'''
    alg_filename = os.path.join(DYN03_BASE_PATH, "13_alg_long_line.f90")
    _, _ = generate(alg_filename, api="dynamo0.3")


def test_kern_lines_too_long_tested():
    ''' Test that the generate function raises an exception if the
    line_length argument is set to True and a Kernel file has
    lines longer than 132 characters. We use the dynamo0.3 API in this
    case but could have chosen any. '''
    alg_filename = os.path.join(DYN03_BASE_PATH, "13.1_kern_long_line.f90")
    with pytest.raises(ParseError) as excinfo:
        _, _ = generate(alg_filename, api="dynamo0.3", line_length=True)
    assert 'file does not conform' in str(excinfo.value)


def test_kern_lines_too_long_not_tested():
    ''' Test that the generate function returns successfully if the
    line_length argument is not set (as it should default to False)
    when a kernel file has lines longer than 132 characters. We
    use the dynamo0.3 API in this case but could have chosen any.'''
    alg_filename = os.path.join(DYN03_BASE_PATH, "13.1_kern_long_line.f90")
    _, _ = generate(alg_filename, api="dynamo0.3")


def test_continuators():
    '''Tests that input files with long lines that already have
       continuators to make the code conform to the line length limit
       do not cause an error '''
    _, _ = generate(os.path.join(os.path.dirname(os.path.abspath(__file__)),
                                 "test_files", "dynamo0p3",
                                 "1.1.0_single_invoke_xyoz_qr.f90"),
                    api="dynamo0.3", line_length=True)


def test_main_version(capsys):
    '''Tests that the version info is printed correctly.'''
    # First test if -h includes the right version info:
    with pytest.raises(SystemExit):
        main(["-h"])
    output, _ = capsys.readouterr()
    from psyclone.version import __VERSION__
    assert "Display version information ({0})".format(__VERSION__) in output

    # Now test -v, but it needs a filename for argparse to work. Just use
    # some invalid parameters - "-v" prints its output before that.
    with pytest.raises(SystemExit) as _:
        main(["-v", "does-not-exist"])
    output, _ = capsys.readouterr()

    assert "PSyclone version: {0}".format(__VERSION__) in output


def test_main_profile(capsys):
    '''Tests that the profiling command line flags are working as expected.
    '''
    filename = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                            "test_files", "gocean1p0",
                            "test27_loop_swap.f90")

    from psyclone.profiler import Profiler
    options = ["-api", "gocean1.0"]

    # Check for invokes only parameter:
    main(options+["--profile", "invokes", filename])
    assert not Profiler.profile_kernels()
    assert Profiler.profile_invokes()

    # Check for kernels only parameter:
    main(options+["--profile", "kernels", filename])
    assert Profiler.profile_kernels()
    assert not Profiler.profile_invokes()

    # Check for invokes + kernels
    main(options+["--profile", "kernels",
                  '--profile', 'invokes', filename])
    assert Profiler.profile_kernels()
    assert Profiler.profile_invokes()

    # Check for missing parameter (argparse then
    # takes the filename as parameter for profiler):
    with pytest.raises(SystemExit):
        main(options+["--profile", filename])
    _, outerr = capsys.readouterr()

    correct_re = "invalid choice.*choose from 'invokes', 'kernels'"
    assert re.search(correct_re, outerr) is not None

    # Check for invalid parameter
    with pytest.raises(SystemExit):
        main(options+["--profile", "invalid", filename])
    _, outerr = capsys.readouterr()

    assert re.search(correct_re, outerr) is not None

    # Reset profile flags to avoid further failures in other tests
    Profiler.set_options(None)


def test_main_invalid_api(capsys):
    '''Tests that we get the expected output and the code exits
    with an error if the supplied API is not known'''
    filename = (os.path.join(os.path.dirname(os.path.abspath(__file__)),
                             "test_files", "dynamo0p3",
                             "1_single_invoke.f90"))
    with pytest.raises(SystemExit) as excinfo:
        main([filename, "-api", "madeup"])
    # The error code should be 1
    assert str(excinfo.value) == "1"
    _, output = capsys.readouterr()
    expected_output = ("Unsupported API 'madeup' specified. Supported API's "
                       "are ['dynamo0.1', 'dynamo0.3', "
                       "'gocean0.1', 'gocean1.0', 'nemo'].\n")
    assert output == expected_output


def test_main_api():
    '''Tests the three ways of specifying an API: command line, config file,
    or relying on the default.'''

    # 1) Make sure if no paramenters are given,
    #   config will give us the default API

    # Make sure we get a default config instance
    Config._instance = None
    Config.get()

    assert Config.get().api == Config.get().default_api

    # 2) Check that a command line option will overwrite the default
    filename = (os.path.join(os.path.dirname(os.path.abspath(__file__)),
                             "test_files", "gocean1p0",
                             "single_invoke.f90"))

    main([filename, "-api", "gocean1.0"])
    assert Config.get().api == "gocean1.0"

    # 3) Check that a config option will overwrite the default
    Config._instance = None
    Config.get()
    # This config file specifies the gocean1.0 api
    config_name = (os.path.join(os.path.dirname(os.path.abspath(__file__)),
                                "test_files", "gocean1p0",
                                "new_iteration_space.psyclone"))
    main([filename, "--config", config_name])
    assert Config.get().api == "gocean1.0"

    # 4) Check that a command line option overwrites what is specified in
    #    in the config file (and the default)
    Config._instance = None
    Config.get()

    filename = (os.path.join(os.path.dirname(os.path.abspath(__file__)),
                             "test_files", "dynamo0p1",
                             "1_kg_inline.f90"))

    # This config file specifies the gocean1.0 api, but
    # command line should take precedence
    main([filename, "--config", config_name, "-api", "dynamo0.1"])
    assert Config.get().api == "dynamo0.1"


def test_main_expected_fatal_error(capsys):
    '''Tests that we get the expected output and the code exits with an
    error when an expected fatal error is returned from the generate
    function.'''
    filename = (os.path.join(os.path.dirname(os.path.abspath(__file__)),
                             "test_files", "dynamo0p3",
                             "2_incorrect_number_of_args.f90"))
    with pytest.raises(SystemExit) as excinfo:
        main([filename])
    # the error code should be 1
    assert str(excinfo.value) == "1"
    _, output = capsys.readouterr()
    expected_output = ("\"Parse Error: Kernel 'testkern_type' called from the "
                       "algorithm layer with an insufficient number of "
                       "arguments as specified by the metadata. Expected at "
                       "least '5' but found '4'.\"\n")
    assert output == expected_output


def test_main_unexpected_fatal_error(capsys, monkeypatch):
    ''' Tests that we get the expected output and the code exits with an
    error when an unexpected fatal error is returned from the generate
    function. '''
    # sabotage the code so one of our constant lists is now an int
    from psyclone.domain.lfric import LFRicArgDescriptor
    monkeypatch.setattr(LFRicArgDescriptor, "VALID_ARG_TYPE_NAMES",
                        value=1)
    filename = (os.path.join(os.path.dirname(os.path.abspath(__file__)),
                             "test_files", "dynamo0p3",
                             "1_single_invoke.f90"))
    with pytest.raises(SystemExit) as excinfo:
        main([filename])
    # the error code should be 1
    assert str(excinfo.value) == "1"
    _, output = capsys.readouterr()
    expected_output = (
        "Error, unexpected exception, please report to the authors:\n"
        "Description ...\n"
        "argument of type 'int' is not iterable\n"
        "Type ...\n"
        "%s\n"
        "Stacktrace ...\n" % type(TypeError()))
    assert expected_output in output


@pytest.mark.parametrize("limit", ['all', 'output'])
def test_main_fort_line_length(capsys, limit):
    '''Tests that the Fortran line length object works correctly. Without
    the -l option one of the generated psy-layer lines would be longer
    than 132 characters. Since it is in the output code, both the 'all' and
   'output' options should cause the limit to be applied. '''
    filename = (os.path.join(os.path.dirname(os.path.abspath(__file__)),
                             "test_files", "dynamo0p3",
                             "10.3_operator_different_spaces.f90"))
    main([filename, '-api', 'dynamo0.3', '-l', limit])
    output, _ = capsys.readouterr()
    assert all(len(line) <= 132 for line in output.split('\n'))


@pytest.mark.parametrize("limit", [[], ['-l', 'off']])
def test_main_fort_line_length_off(capsys, limit):
    '''Tests that the Fortran line-length limiting is off by default and is
    also disabled by `-l off`. One of the generated psy-layer lines should be
    longer than 132 characters. '''
    filename = (os.path.join(os.path.dirname(os.path.abspath(__file__)),
                             "test_files", "dynamo0p3",
                             "10.3_operator_different_spaces.f90"))
    main([filename, '-api', 'dynamo0.3'] + limit)
    output, _ = capsys.readouterr()
    assert not all(len(line) <= 132 for line in output.split('\n'))


def test_main_fort_line_length_output_only(capsys):
    ''' Check that the '-l output' option disables the line-length check on
    input files but still limits the line lengths in the output. '''
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
    file to stdout when the algorithm file does not contain an invoke and that
    it does not produce any psy output.'''

    # pass in a kernel file as that has no invokes in it
    kern_filename = (os.path.join(os.path.dirname(os.path.abspath(__file__)),
                                  "test_files", "dynamo0p3",
                                  "testkern_mod.F90"))
    main([kern_filename])
    out, _ = capsys.readouterr()

    kern_file = open(kern_filename)
    kern_str = kern_file.read()
    expected_output = ("Warning: 'Algorithm Error: Algorithm file contains no "
                       "invoke() calls: refusing to generate empty PSy code'\n"
                       "Transformed algorithm code:\n") + kern_str + "\n"
    assert expected_output == out


def test_main_write_psy_file(capsys, tmpdir):
    '''Tests that the main() function outputs successfully writes the
    generated psy output to a specified file'''

    alg_filename = (os.path.join(os.path.dirname(os.path.abspath(__file__)),
                                 "test_files", "dynamo0p3",
                                 "1_single_invoke.f90"))

    psy_filename = str(tmpdir.join("psy.f90"))

    main([alg_filename, '-opsy', psy_filename])

    # check psy file is created
    assert os.path.isfile(psy_filename)

    # extract psy file content
    psy_file = open(psy_filename)
    psy_str = psy_file.read()

    # check content of generated psy file by comparing it with stdout
    main([alg_filename])
    stdout, _ = capsys.readouterr()

    assert psy_str in stdout


def test_main_no_invoke_alg_file(capsys, tmpdir):
    '''Tests that the main() function outputs the original algorithm input
    file to file when the algorithm file does not contain an invoke and that
    it does not produce any psy output.'''

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
    kern_file = open(kern_filename)
    kern_str = kern_file.read()
    expected_stdout = ("Warning: 'Algorithm Error: Algorithm file contains "
                       "no invoke() calls: refusing to generate empty PSy "
                       "code'\n")
    assert expected_stdout == stdout

    # check alg file has same output as input file
    expected_file = open(alg_filename)
    expected_alg_str = expected_file.read()
    assert expected_alg_str == kern_str
    os.remove(alg_filename)

    # check psy file is not created
    assert not os.path.isfile(psy_filename)


def test_main_kern_output_no_dir(capsys):
    ''' Test for when the specified output directory (for transformed
    kernels) does not exist. '''
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
    ''' Test for when the specified output directory (for transformed
    kernels) cannot be written to. '''
    import stat
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
    assert ("Cannot write to specified kernel output directory ({0})".
            format(str(new_dir)) in output)


def test_main_kern_output_dir(tmpdir):
    ''' Test that we can specify a valid kernel output directory. '''
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
    ''' Check that we raise the expected error if an invalid kernel-renaming
    scheme is supplied. '''
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
    ''' Check that the main function complains if a non-existant location
    is specified as a search path for INCLUDE files. '''
    alg_file = (os.path.join(os.path.dirname(os.path.abspath(__file__)),
                             "nemo", "test_files", "include_stmt.f90"))
    fake_path = tmpdir.join('does_not_exist')
    with pytest.raises(SystemExit) as err:
        main([alg_file, '-api', 'nemo', '-I', fake_path.strpath])
    assert str(err.value) == "1"
    capout = capsys.readouterr()
    assert "does_not_exist' does not exist" in capout.err


def test_main_include_path(capsys):
    ''' Test that the main function supplies any INCLUDE paths to
    fparser. '''
    # This algorithm file INCLUDE's a file that defines a variable called
    # "some_fake_mpi_handle"
    alg_file = (os.path.join(os.path.dirname(os.path.abspath(__file__)),
                             "nemo", "test_files", "include_stmt.f90"))
    # First try without specifying where to find the include file. Currently
    # fparser2 just removes any include statement that it cannot resolve
    # (https://github.com/stfc/fparser/issues/138).
    main([alg_file, '-api', 'nemo'])
    stdout, _ = capsys.readouterr()
    assert "some_fake_mpi_handle" not in stdout
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


def test_write_utf_file(tmpdir, monkeypatch):
    ''' Unit tests for the write_unicode_file utility routine. '''
    import six
    import io
    from psyclone.generator import write_unicode_file

    # First for plain ASCII
    out_file1 = os.path.join(str(tmpdir), "out1.txt")
    write_unicode_file("This contains only ASCII", out_file1)

    # Second with a character that has no ASCII representation
    with open(out_file1, "r") as infile:
        content = infile.read()
        assert "This contains only ASCII" in content
    out_file2 = os.path.join(str(tmpdir), "out2.txt")
    if six.PY2:
        # pylint: disable=undefined-variable
        test_str = u"This contains UTF: "+unichr(1200)
        # pylint: enable=undefined-variable
    else:
        test_str = "This contains UTF: "+chr(1200)
    encoding = {'encoding': 'utf-8'}
    write_unicode_file(test_str, out_file2)

    with io.open(out_file2, mode="r", **encoding) as infile:
        content = infile.read()
    assert test_str in content

    # monkeypatch the six module so that the check on which Python
    # version is being used fails.
    monkeypatch.setattr(six, "PY2", value=None)
    monkeypatch.setattr(six, "PY3", value=None)
    with pytest.raises(InternalError) as err:
        write_unicode_file("Some stuff", out_file2)
    assert "Unrecognised Python version" in str(err.value)


def test_utf_char(tmpdir):
    ''' Test that the generate method works OK when both the Algorithm and
    Kernel code contain utf-encoded chars. '''
    import io
    algfile = os.path.join(str(tmpdir), "alg.f90")
    main([os.path.join(BASE_PATH, "gocean1p0", "test29_utf_chars.f90"),
          "-api", "gocean1.0", "-oalg", algfile])
    # We only check the algorithm layer since we generate the PSy
    # layer from scratch in this API (and thus it contains no
    # non-ASCII characters).
    encoding = {'encoding': 'utf-8'}
    with io.open(algfile, "r", **encoding) as afile:
        alg = afile.read().lower()
        assert "max reachable coeff" in alg
        assert "call invoke_0_kernel_utf" in alg
