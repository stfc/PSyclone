# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017-2026, Science and Technology Facilities Council.
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
# Modified by A. B. G. Chalk, STFC Daresbury Lab


'''
A module to perform pytest unit and functional tests on the code in
the generator.py file. This includes the generate and the main
functions.
'''

import logging
import os
from pathlib import Path
import re
import shutil
import stat
from sys import modules
from typing import Optional
import pytest

from fparser.common.readfortran import FortranStringReader
from fparser.two.parser import ParserFactory

from psyclone import generator
from psyclone.alg_gen import NoInvokesError
from psyclone.configuration import Config, ConfigurationError
from psyclone.domain.lfric import LFRicConstants
from psyclone.domain.lfric.transformations import LFRicLoopFuseTrans
from psyclone.errors import GenerationError
from psyclone.generator import (
    generate, main, check_psyir, add_builtins_use, code_transformation_mode)
from psyclone.parse import ModuleManager
from psyclone.parse.algorithm import parse
from psyclone.parse.utils import ParseError
from psyclone.profiler import Profiler
from psyclone.psyGen import PSyFactory
from psyclone.psyir.frontend.fortran import FortranReader
from psyclone.version import __VERSION__
from psyclone.tests.utilities import get_base_path

BASE_PATH = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                         "test_files")
NEMO_BASE_PATH = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                              "nemo", "test_files")
LFRIC_BASE_PATH = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                               "test_files", "lfric")
GOCEAN_BASE_PATH = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                                "test_files", "gocean1p0")


@pytest.fixture(name="script_factory", scope="function")
def create_script_factor(tmpdir):
    ''' Fixture that creates a psyclone optimisation script given the string
    representing the body of the script:

        script_path = script_factory("def trans(psyir):\n  pass")

    It has a 'function' scope and a tear down section because using a script
    imports the file and this is kept in the python interpreter state, so we
    delete it for future tests.

    '''
    tmpfile = os.path.join(tmpdir, "test_script.py")

    def populate_script(string):
        with open(tmpfile, 'w+', encoding="utf8") as script:
            script.write(string)
        return tmpfile

    yield populate_script
    # Tear down section executed after each test that uses the fixture
    # If the created script was used, then its module (file) was imported
    # into the interpreter runtime, we need to make sure it is deleted
    modname = "test_script"
    if modname in modules:
        del modules[modname]
    for mod in modules.values():
        try:
            delattr(mod, modname)
        except AttributeError:
            pass


def test_script_file_not_found():
    '''Checks that load_script() in generator.py raises the expected
    exception when a script file is supplied that does not exist. This test
    uses the generate() function to call load_script as this is a simple way
    to create its required arguments.

    '''
    with pytest.raises(GenerationError) as error:
        _, _ = generate(
            os.path.join(BASE_PATH, "lfric", "1_single_invoke.f90"),
            api="lfric", script_name="non_existent.py")
    assert "script file 'non_existent.py' not found" in str(error.value)


def test_script_file_no_extension():
    '''Checks that load_script() in generator.py raises the expected
    exception when a script file does not have an extension. This test
    uses the generate() function to call load_script as this is a
    simple way to create its required arguments.

    '''
    with pytest.raises(GenerationError) as error:
        _, _ = generate(
            os.path.join(BASE_PATH, "lfric", "1_single_invoke.f90"),
            api="lfric",
            script_name=os.path.join(BASE_PATH, "lfric",
                                     "invalid_script_name"))
    assert ("expected the script file 'invalid_script_name' to have the "
            "'.py' extension" in str(error.value))


def test_script_file_wrong_extension():
    '''Checks that load_script() in generator.py raises the expected
    exception when a script file does not have the '.py' extension. This test
    uses the generate() function to call load_script as this is a simple way
    to create its required arguments.

    '''
    with pytest.raises(GenerationError) as error:
        _, _ = generate(
            os.path.join(BASE_PATH, "lfric", "1_single_invoke.f90"),
            api="lfric",
            script_name=os.path.join(BASE_PATH, "lfric",
                                     "1_single_invoke.f90"))
    assert ("expected the script file '1_single_invoke.f90' to have the '.py' "
            "extension" in str(error.value))


def test_script_invalid_content(script_factory):
    '''Checks that load_script() in generator.py raises the expected
    exception when a script file does not contain valid python. This
    test uses the generate() function to call load_script as this is
    a simple way to create its required arguments.

    '''
    error_syntax = script_factory("""
this is invalid python
    """)
    with pytest.raises(Exception) as err:
        _, _ = generate(
            os.path.join(BASE_PATH, "lfric", "1_single_invoke.f90"),
            api="lfric", script_name=error_syntax)
    assert "invalid syntax (test_script.py, line 2)" in str(err.value)

    error_import = script_factory("""
import non_existent
    """)
    with pytest.raises(Exception) as err:
        _, _ = generate(
            os.path.join(BASE_PATH, "lfric", "1_single_invoke.f90"),
            api="lfric", script_name=error_import)
    assert "No module named 'non_existent'" in str(err.value)


def test_script_invalid_content_runtime(script_factory):
    '''Checks that load_script() function in generator.py raises the
    expected exception when a script file contains valid python
    syntactically but produces a runtime exception. This test uses the
    generate() function to call load_script as this is a simple way
    to create its required arguments.

    '''
    runtime_error = script_factory("""
def trans(psyir):
    # this will produce a runtime error as b has not been assigned
    psyir = b
    """)
    with pytest.raises(Exception) as error:
        _, _ = generate(
            os.path.join(BASE_PATH, "lfric", "1_single_invoke.f90"),
            api="lfric", script_name=runtime_error)
    assert "name 'b' is not defined" in str(error.value)


def test_script_no_trans(script_factory):
    '''Checks that load_script() function in generator.py raises the
    expected exception when a script file does not contain a trans()
    function. This test uses the generate() function to call
    load_script as this is a simple way to create its required
    arguments.

    '''
    no_trans_script = script_factory("""
def nottrans(psyir):
    pass

def tran():
    pass
""")
    with pytest.raises(GenerationError) as error:
        _, _ = generate(
            os.path.join(BASE_PATH, "lfric", "1_single_invoke.f90"),
            api="lfric", script_name=no_trans_script)
    assert ("attempted to use specified PSyclone transformation module "
            "'test_script' but it does not contain a callable 'trans' function"
            in str(error.value))


def test_script_no_trans_alg(capsys, script_factory):
    '''Checks that load_script() function in generator.py does not raise
    an exception when a script file does not contain a trans_alg()
    function as these are optional. At the moment this function is
    only supported in the gocean API. This test uses the generate()
    function to call load_script as this is a simple way to create
    its required arguments.

    '''
    no_alg_script = script_factory("def trans(psyir):\n  pass")
    _, _ = generate(
        os.path.join(BASE_PATH, "gocean1p0", "single_invoke.f90"),
        api="gocean", script_name=no_alg_script)

    # The legacy script deprecation warning is not printed in this case
    captured = capsys.readouterr()
    assert "Deprecation warning:" not in captured.err


def test_script_with_legacy_trans_signature(capsys, script_factory):
    '''Checks that load_script() function in generator.py does not raise
    an exception when a script file uses the legacy trans signature.

    These are scripts that receive a PSy object and use the psy.invokes....
    to access the PSyIR.

    This will eventually be deprecated.

    '''
    legacy_script = script_factory("""
def trans(psy):
    # The following are backwards-compatible expressions with legacy scripts
    _ = psy.invokes.invoke_list
    _ = psy.invokes.names
    return psy
""")
    _, _ = generate(
        os.path.join(BASE_PATH, "gocean1p0", "single_invoke.f90"),
        api="gocean", script_name=legacy_script)

    # The deprecation warning message was printed
    captured = capsys.readouterr()
    assert ("Deprecation warning: PSyclone script uses the legacy "
            "transformation signature 'def trans(psy)', please update the "
            "script to receive the root psyir node as argument."
            in captured.err)


# a set of unit tests for the generate function

def test_non_existent_filename():
    '''Checks that alg_gen raises appropriate error when a non-existent
    filename is supplied.

    '''
    with pytest.raises(IOError):
        generate("non_existent_file.f90", api="lfric")


def test_invalid_api():
    '''Checks that alg_gen raises appropriate error when an invalid api
    is supplied.

    '''
    with pytest.raises(GenerationError):
        generate(os.path.join(BASE_PATH, "lfric", "1_single_invoke.f90"),
                 api="invalid")


def test_invalid_kernel_paths():
    '''Checks that alg_gen raises appropriate error when an invalid search
    path for kernel source files is supplied, even if a valid path is
    also supplied.

    '''
    with pytest.raises(IOError) as info:
        generate(os.path.join(BASE_PATH, "lfric", "1_single_invoke.f90"),
                 api="lfric",
                 kernel_paths=[
                     os.path.join(BASE_PATH, "lfric"), "does_not_exist"])
    assert "Kernel search path 'does_not_exist' not found" in str(info.value)


def test_wrong_kernel_paths():
    '''Checks that alg_gen raises appropriate error when the kernel code
    cannot be found in the specified search path.

    '''
    with pytest.raises(ParseError):
        generate(os.path.join(BASE_PATH, "lfric",
                              "1.1.0_single_invoke_xyoz_qr.f90"),
                 api="lfric",
                 kernel_paths=[os.path.join(BASE_PATH, "gocean1p0")])


def test_correct_kernel_paths():
    '''Checks that alg_gen succeeds when the location of the kernel source
    code is *not* the same as that of the algorithm code. Also adds a
    path that does not contain the required kernel.

    '''
    _, _ = generate(
        os.path.join(BASE_PATH, "lfric", "1_single_invoke_kern.f90"),
        api="lfric",
        kernel_paths=[
            os.path.join(BASE_PATH, "lfric", "kernels", "dead_end"),
            os.path.join(BASE_PATH, "lfric", "kernels", "in_here")])


def test_same_kernel_paths():
    '''Checks that the generator succeeds when the search directory is the
    same as the algorithm code directory and a path is specified.

    '''
    path = os.path.join(BASE_PATH, "lfric")
    _, _ = generate(os.path.join(path, "1_single_invoke.f90"),
                    api="lfric", kernel_paths=[path])


def test_similar_kernel_name():
    '''Checks that the generator does not match incorrect files.'''

    with pytest.raises(ParseError) as info:
        _, _ = generate(
            os.path.join(BASE_PATH, "lfric", "1_single_invoke.f90"),
            api="lfric",
            kernel_paths=[os.path.join(BASE_PATH, "lfric", "kernels",
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
        os.path.join(BASE_PATH, "lfric", "1_single_invoke_kern.f90"),
        api="lfric",
        kernel_paths=[os.path.join(BASE_PATH, "lfric", "kernels")])


def test_kernel_parsing_internalerror(capsys, caplog):
    '''Checks that the expected output is provided if an internal error is
    caught when parsing a kernel using fparser2.

    '''
    kern_filename = (os.path.join(
        GOCEAN_BASE_PATH, "test30_invalid_kernel_declaration.f90"))
    with pytest.raises(SystemExit):
        main([kern_filename, "-api", "gocean"])
    out, err = capsys.readouterr()
    assert out == ""
    assert "Failed to create PSyIR from kernel file '" in str(err)
    # Clear previous logging messages (primarily from fparser)
    caplog.clear()
    with caplog.at_level(logging.ERROR, "psyclone.generator"):
        with pytest.raises(SystemExit):
            main([kern_filename, "-api", "gocean"])
        assert caplog.records[0].levelname == "ERROR"
        assert (
            "PSyclone internal error: The argument list ['i', 'j', 'cu', 'p', "
            "'u'] for routine 'compute_code' does not match the variable "
            "declarations:\n"
            "IMPLICIT NONE\n"
            "INTEGER, INTENT(IN) :: I, J\n"
            "REAL(KIND = go_wp), INTENT(OUT), DIMENSION(:, :) :: cu\n"
            "REAL(KIND = go_wp), INTENT(IN), DIMENSION(:, :) :: p\n"
            "(Note that PSyclone does not support implicit declarations.) "
            "Specific"
            " PSyIR error is \"Could not find 'u' in the Symbol Table.\".\n"
            in caplog.text)


def test_script_file_too_short():
    '''Checks that generator.py raises an appropriate error when a script
    file name is too short to contain the '.py' extension.

    '''
    with pytest.raises(GenerationError) as err:
        _, _ = generate(os.path.join(BASE_PATH, "lfric",
                                     "1_single_invoke.f90"),
                        api="lfric",
                        script_name=os.path.join(
                            BASE_PATH,
                            "lfric", "testkern_xyz_mod.f90"))
    assert ("expected the script file 'testkern_xyz_mod.f90' to have the "
            "'.py' extension" in str(err.value))


def test_no_script_gocean():
    '''Test that the generate function in generator.py returns
    successfully if no script is specified for the gocean api.

    '''
    alg, psy = generate(
        os.path.join(BASE_PATH, "gocean1p0", "single_invoke.f90"),
        api="gocean")
    assert "program single_invoke_test" in alg
    assert "module psy_single_invoke_test" in str(psy)


def test_script_gocean(script_factory):
    '''Test that the generate function in generator.py returns
    successfully if a script (containing both trans_alg() and trans()
    functions) is specified.

    '''
    alg_script = script_factory("""
def trans_alg(psyir):
    pass

def trans(psyir):
    pass
    """)

    _, _ = generate(
        os.path.join(BASE_PATH, "gocean1p0", "single_invoke.f90"),
        api="gocean", script_name=alg_script)


def test_profile_gocean():
    '''Test that the generate function in generator.py adds profiling
    information if this has been specified.

    '''
    Profiler.set_options(['invokes'], "gocean")
    _, psy = generate(
        os.path.join(BASE_PATH, "gocean1p0", "single_invoke.f90"),
        api="gocean")
    assert "CALL profile_psy_data" in str(psy)
    # Reset the stored options.
    Profiler._options = []


def test_invalid_gocean_alg(monkeypatch, caplog, capsys):
    '''
    Test that an error creating PSyIR for a GOcean algorithm layer is
    handled correctly.

    '''
    # It's easiest to monkeypatch the psyir_from_file() method so that it
    # raises an error.
    def _broken(_1, _2):
        raise ValueError("This is a test")

    monkeypatch.setattr(FortranReader, "psyir_from_file", _broken)
    with caplog.at_level(logging.ERROR, logger="psyclone.generator"):
        with pytest.raises(SystemExit):
            _ = generate(
                os.path.join(BASE_PATH, "gocean1p0", "single_invoke.f90"),
                api="gocean")
        assert "This is a test" in caplog.text
        assert "Traceback" in caplog.text
        _, err = capsys.readouterr()
        assert "Failed to create PSyIR from file '" in err


def test_script_attr_error(script_factory):
    '''Checks that generator.py raises an appropriate error when a script
    file contains a trans() function which raises an attribute error.

    '''
    error_script = script_factory("""
from psyclone.psyGen import Loop
from psyclone.transformations import ColourTrans

def trans(psyir):
    ''' A valid trans function which produces an attribute error as
    we have mistyped apply()'''
    ctrans = ColourTrans()
    for child in psyir.walk(Loop):
        if isinstance(child, Loop) and child.field_space != "w3":
            ctrans.appy(child)
""")
    with pytest.raises(Exception) as excinfo:
        _, _ = generate(os.path.join(BASE_PATH, "lfric",
                                     "1_single_invoke.f90"),
                        api="lfric", script_name=error_script)
    assert 'object has no attribute' in str(excinfo.value)


def test_script_null_trans(script_factory):
    '''Checks that generator.py works correctly when the trans() function
    in a valid script file does no transformations.

    '''
    empty_script = script_factory("def trans(psyir):\n  pass")
    alg1, psy1 = generate(os.path.join(BASE_PATH, "lfric",
                                       "1_single_invoke.f90"),
                          api="lfric")
    alg2, psy2 = generate(os.path.join(BASE_PATH, "lfric",
                                       "1_single_invoke.f90"),
                          api="lfric", script_name=empty_script)
    # we need to remove the first line before comparing output as
    # this line is an instance specific header
    assert '\n'.join(str(alg1).split('\n')[1:]) == \
        '\n'.join(str(alg2).split('\n')[1:])
    assert '\n'.join(str(psy1).split('\n')[1:]) == \
        '\n'.join(str(psy2).split('\n')[1:])


def test_script_null_trans_relative(script_factory):
    '''Checks that generator.py works correctly when the trans() function
    in a valid script file does no transformations (it simply passes
    input to output). In this case the valid script file contains no
    path and must therefore be found via the PYTHOPATH path list.

    '''
    alg1, psy1 = generate(os.path.join(BASE_PATH, "lfric",
                                       "1_single_invoke.f90"),
                          api="lfric")
    empty_script = script_factory("def trans(psyir):\n  pass")
    basename = os.path.basename(empty_script)
    path = os.path.dirname(empty_script)
    # Set the script directory in the PYTHONPATH
    os.sys.path.append(path)
    alg2, psy2 = generate(os.path.join(BASE_PATH, "lfric",
                                       "1_single_invoke.f90"),
                          api="lfric", script_name=basename)
    # Remove the path from PYTHONPATH
    os.sys.path.pop()
    # we need to remove the first line before comparing output as
    # this line is an instance specific header
    assert '\n'.join(str(alg1).split('\n')[1:]) == \
        '\n'.join(str(alg2).split('\n')[1:])
    assert str(psy1) == str(psy2)


def test_script_trans_lfric(script_factory):
    '''Checks that generator.py works correctly when a transformation is
    provided as a script, i.e. it applies the transformations
    correctly.

    '''
    fuse_loop_script = script_factory("""
from psyclone.domain.lfric.transformations import LFRicLoopFuseTrans
def trans(psyir):
    module = psyir.children[0]
    schedule = [x for x in module.children if x.name == "invoke_0"][0]
    loop1 = schedule.children[4]
    loop2 = schedule.children[5]
    transform = LFRicLoopFuseTrans()
    transform.apply(loop1, loop2)
""")
    root_path = os.path.dirname(os.path.abspath(__file__))
    base_path = os.path.join(root_path, "test_files", "lfric")
    # First loop fuse explicitly (without using generator.py)
    parse_file = os.path.join(base_path, "4_multikernel_invokes.f90")
    _, invoke_info = parse(parse_file, api="lfric")
    psy = PSyFactory("lfric", distributed_memory=True).create(invoke_info)
    invoke = psy.invokes.get("invoke_0")
    schedule = invoke.schedule
    loop1 = schedule.children[4]
    loop2 = schedule.children[5]
    trans = LFRicLoopFuseTrans()
    trans.apply(loop1, loop2)
    generated_code_1 = psy.gen
    # Second loop fuse using generator.py and a script
    _, generated_code_2 = generate(parse_file, api="lfric",
                                   script_name=fuse_loop_script)
    # third - check that the results are the same ...
    assert str(generated_code_1) == str(generated_code_2)


def test_alg_lines_too_long_tested():
    '''Test that the generate function causes an exception if the
    line_length argument is set to True and the algorithm file has
    lines longer than 132 characters. We use the lfric API in this
    case but could have chosen any.

    '''
    alg_filename = os.path.join(LFRIC_BASE_PATH, "13_alg_long_line.f90")
    with pytest.raises(ParseError) as excinfo:
        _, _ = generate(alg_filename, api="lfric", line_length=True)
    assert "/13_alg_long_line.f90' does not conform" in str(excinfo.value)


def test_alg_lines_too_long_not_tested():
    '''Test that the generate function returns successfully if the
    line_length argument is not set (as it should default to False)
    when the algorithm file has lines longer than 132 characters. We
    use the lfric API in this case but could have chosen any.

    '''
    alg_filename = os.path.join(LFRIC_BASE_PATH, "13_alg_long_line.f90")
    _, _ = generate(alg_filename, api="lfric")


def test_kern_lines_too_long_tested():
    '''Test that the generate function raises an exception if the
    line_length argument is set to True and a Kernel file has lines
    longer than 132 characters. We use the lfric API in this case
    but could have chosen any.

    '''
    alg_filename = os.path.join(LFRIC_BASE_PATH, "13.1_kern_long_line.f90")
    with pytest.raises(ParseError) as excinfo:
        _, _ = generate(alg_filename, api="lfric", line_length=True)
    assert "/longkern_mod.f90' does not conform" in str(excinfo.value)


def test_kern_lines_too_long_not_tested():
    '''Test that the generate function returns successfully if the
    line_length argument is not set (as it should default to False)
    when a kernel file has lines longer than 132 characters. We use
    the lfric API in this case but could have chosen any.

    '''
    alg_filename = os.path.join(LFRIC_BASE_PATH, "13.1_kern_long_line.f90")
    _, _ = generate(alg_filename, api="lfric")


def test_continuators():
    '''Tests that input files with long lines that already have
    continuators to make the code conform to the line length limit do
    not cause an error.

    '''
    _, _ = generate(os.path.join(os.path.dirname(os.path.abspath(__file__)),
                                 "test_files", "lfric",
                                 "1.1.0_single_invoke_xyoz_qr.f90"),
                    api="lfric", line_length=True)


def test_main_version(capsys):
    '''Tests that the version info is printed correctly.'''

    # First test if -h includes the right version info:
    for arg in ["-h", "--help"]:
        with pytest.raises(SystemExit):
            main([arg])
        output, _ = capsys.readouterr()
        assert "display version information" in output

    for arg in ["-v", "--version"]:
        with pytest.raises(SystemExit) as _:
            main([arg])
        output, _ = capsys.readouterr()
        assert f"PSyclone version: {__VERSION__}" in output


def test_wrong_flags_for_mode(capsys):
    '''Tests that -o is not accepted for psykal and psykal-specific flags
    are not accepted in code-transformation mode.'''

    # Code-transformation mode
    filename = os.path.join(NEMO_BASE_PATH, "explicit_do_long_line.f90")
    for flag in ["-okern", "-opsy", "-oalg", "-d"]:
        with pytest.raises(SystemExit):
            main([filename, flag, "FILE"])
        output, _ = capsys.readouterr()
        assert ("When using the code-transformation mode (with no -api or"
                " --psykal-dsl flags), the psykal-mode arguments must not "
                "be present in the command, but found" in output)

    # PSyKAl-DSL mode
    filename = os.path.join(GOCEAN_BASE_PATH, "single_invoke.f90")
    with pytest.raises(SystemExit):
        main([filename, "--psykal-dsl", "gocean", "-o", "FILE"])
    output, _ = capsys.readouterr()
    assert ("The '-o' flag is not valid when using the psykal mode (-api/"
            "--psykal-dsl flag), use the -oalg, -opsy, -okern to specify the "
            "output destination of each psykal layer." in output)


def test_main_profile(capsys):
    '''Tests that the profiling command line flags are working as
    expected.

    '''
    filename = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                            "test_files", "gocean1p0",
                            "test27_loop_swap.f90")

    options = ["-api", "gocean"]

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

    # regex is slightly complicated to allow for changes in the formatting
    # of the message between versions of argparse.
    correct_re = ("invalid choice[.:].*choose from '?invokes'?, "
                  "'?routines'?, '?kernels'?")
    assert re.search(correct_re, outerr) is not None

    # Check for invalid parameter
    with pytest.raises(SystemExit):
        main(options+["--profile", "invalid", filename])
    _, outerr = capsys.readouterr()

    assert re.search(correct_re, outerr) is not None

    # Check 'kernels' and 'invokes' are rejected when not using a PSyKAl DSL
    Profiler._options = []
    with pytest.raises(SystemExit):
        main(["--profile", "kernels", filename])
    _, outerr = capsys.readouterr()
    assert ("Invalid profiling option: The profiling 'kernels' and 'invokes' "
            "options are only available when using PSyKAl DSLs." in outerr)

    with pytest.raises(SystemExit):
        main(["--profile", "invokes", filename])
    _, outerr = capsys.readouterr()
    assert ("Invalid profiling option: The profiling 'kernels' and 'invokes' "
            "options are only available when using PSyKAl DSLs." in outerr)

    # Reset profile flags to avoid further failures in other tests
    Profiler._options = []


def test_main_invalid_api(capsys):
    '''Tests that we get the expected output and the code exits with an
    error if the supplied API is not known.

    '''
    filename = (os.path.join(os.path.dirname(os.path.abspath(__file__)),
                             "test_files", "lfric",
                             "1_single_invoke.f90"))
    with pytest.raises(SystemExit) as excinfo:
        main([filename, "-api", "madeup"])
    # The error code should be 1
    assert str(excinfo.value) == "1"
    _, output = capsys.readouterr()
    expected_output = ("Unsupported PSyKAL DSL / API 'madeup' specified. "
                       "Supported DSLs are ['lfric', 'gocean'].\n")
    assert output == expected_output


def test_main_logger(capsys, caplog, tmp_path):
    """
    Test the setup of the logger.
    """

    # The conftest `setup_logging` fixture will add a handler to the
    # PSyclone top-level logger - meaning the corresponding line in
    # generator.py is not executed. Remove the handler here so we
    # trigger adding a handler in generator.py
    logger = logging.getLogger("psyclone")
    logger.removeHandler(logger.handlers[0])

    filename = os.path.join(NEMO_BASE_PATH, "explicit_do_long_line.f90")
    # Give invalid logging level
    # Reset capsys
    capsys.readouterr()
    with pytest.raises(SystemExit):
        main([filename, "-api", "lfric", "--log-level", "fail"])
    _, err = capsys.readouterr()
    # Error message check truncated as Python 3.13 changes how the
    # array is output.
    assert ("error: argument --log-level: invalid choice: 'fail'"
            in err)

    # Test we get the logging debug correctly with caplog, including
    # redirection into a file:
    caplog.clear()
    out_file = str(tmp_path / "test.out")
    with caplog.at_level(logging.DEBUG):
        main([filename, "-api", "dynamo0.3", "--log-level", "DEBUG",
              "--log-file", out_file])
        assert Config.get().api == "lfric"
        assert caplog.records[0].levelname == "DEBUG"
        assert "Logging system initialised. Level is DEBUG." in caplog.text
        # Check that we have a file handler installed as expected
        file_handlers = [h for h in logger.handlers
                         if isinstance(h, logging.FileHandler)]
        # There should be exactly one file handler, pointing to out_file:
        assert len(file_handlers) == 1
        assert file_handlers[0].baseFilename == out_file


def test_main_api():
    ''' Test that the API can be set by a command line parameter, also using
    the API name aliases. '''

    filename = os.path.join(NEMO_BASE_PATH, "explicit_do_long_line.f90")

    # By default we don't use an API
    main([filename])
    assert Config.get().api == ""

    filename = os.path.join(GOCEAN_BASE_PATH, "single_invoke.f90")
    # Check that a command line option sets the API value
    main([filename, "-api", "gocean"])
    assert Config.get().api == "gocean"

    # Reset api and try with "--psykal-dsl" flag
    Config.get().api = ""
    main([filename, "--psykal-dsl", "gocean"])
    assert Config.get().api == "gocean"

    main([filename, "-api", "gocean1.0"])
    assert Config.get().api == "gocean"

    filename = os.path.join(LFRIC_BASE_PATH, "1_single_invoke.f90")
    main([filename, "-api", "lfric"])
    assert Config.get().api == "lfric"

    main([filename, "-api", "dynamo0.3"])
    assert Config.get().api == "lfric"


def test_keep_comments_and_keep_directives(capsys, caplog, tmpdir_factory):
    ''' Test the keep comments and keep directives arguments to main. '''
    filename = str(tmpdir_factory.mktemp('psyclone_test').join("test.f90"))
    code = """subroutine a()
    ! Here is a comment
    integer :: a

    !comment 1
    !$omp parallel
    !$omp do
    !comment 2
    do a = 1, 100
    end do
    !$omp end do
    !$omp end parallel
    end subroutine"""
    with open(filename, "w", encoding='utf-8') as wfile:
        wfile.write(code)

    main([filename, "--keep-comments"])
    output, _ = capsys.readouterr()

    correct = """subroutine a()
  ! Here is a comment
  integer :: a

  ! comment 1
  ! comment 2
  do a = 1, 100, 1
  enddo

end subroutine a

"""
    assert output == correct

    main([filename, "--keep-comments", "--keep-directives"])
    output, _ = capsys.readouterr()

    correct = """subroutine a()
  ! Here is a comment
  integer :: a

  ! comment 1
  !$omp parallel
  !$omp do

  ! comment 2
  do a = 1, 100, 1
  enddo
  !$omp end do
  !$omp end parallel

end subroutine a

"""
    assert output == correct

    with caplog.at_level(logging.WARNING, logger="psyclone.generator"):
        main([filename, "--keep-directives"])
    assert ("keep_directives requires keep_comments so "
            "PSyclone enabled keep_comments." in caplog.text)


def test_conditional_openmp_statements(capsys, tmpdir_factory):
    ''' Check that the Conditional OpenMP statements are ignored
    or parser depending on the flags provided to psyclone.
    '''
    code = """subroutine x
    !$ use omp_lib

    integer :: i
    !$ integer :: omp_threads

    i = 1
    !$ omp_threads = omp_get_num_threads()
    end subroutine x"""
    filename = str(tmpdir_factory.mktemp('psyclone_test').join("test.f90"))
    with open(filename, "w", encoding='utf-8') as wfile:
        wfile.write(code)
    main([filename])
    output, _ = capsys.readouterr()
    correct = """subroutine x()
  integer :: i

  i = 1

end subroutine x

"""
    assert output == correct

    main([filename, "--keep-conditional-openmp-statements"])
    output, _ = capsys.readouterr()
    correct = """subroutine x()
  use omp_lib
  integer :: i
  integer :: omp_threads

  i = 1
  omp_threads = omp_get_num_threads()

end subroutine x

"""
    assert output == correct


def test_keep_comments_lfric(capsys, monkeypatch):
    '''Test that the LFRic API correctly keeps comments and directives
    when applied the appropriate arguments.'''
    # Test this for LFRIC algorithm domain.
    monkeypatch.setattr(generator, "LFRIC_TESTING", True)
    filename = os.path.join(LFRIC_BASE_PATH,
                            "1_single_invoke_with_omp_dir.f90")
    main([filename, "-api", "lfric", "--keep-comments"])
    output, _ = capsys.readouterr()
    assert "! Here is a comment" in output
    assert "!$omp barrier" not in output

    filename = os.path.join(LFRIC_BASE_PATH,
                            "1_single_invoke_with_omp_dir.f90")
    main([filename, "-api", "lfric", "--keep-comments", "--keep-directives"])
    output, _ = capsys.readouterr()
    assert "! Here is a comment" in output
    assert "!$omp barrier" in output


def test_keep_comments_gocean(capsys):
    '''Test that the GOcean API correctly keeps comments and directives
    when applied the appropriate arguments.'''
    filename = os.path.join(GOCEAN_BASE_PATH, "single_invoke.f90")
    main([filename, "-api", "gocean"])
    output, _ = capsys.readouterr()
    assert "! Create fields on this grid" not in output

    main([filename, "-api", "gocean", "--keep-comments"])
    output, _ = capsys.readouterr()
    assert "! Create fields on this grid" in output


def test_config_flag():
    ''' Test that -c/--config take precedence over the configuration
        file references in the environment variable.
    '''
    filename = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                            "test_files", "lfric",
                            "1_single_invoke.f90")
    # dummy_config has a non-default REPORD_PAD_SIZE of 7
    config_name = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                               "test_files", "dummy_config.cfg")

    # Test with no option
    Config._HAS_CONFIG_BEEN_INITIALISED = False
    main([filename, "-api", "lfric"])
    assert Config.get().api == "lfric"
    assert Config.has_config_been_initialised() is True
    assert Config.get().reprod_pad_size == 8

    # Test with with --config
    Config._HAS_CONFIG_BEEN_INITIALISED = False
    main([filename, "--config", config_name, "-api", "lfric"])
    assert Config.get().api == "lfric"
    assert Config.has_config_been_initialised() is True
    assert Config.get().reprod_pad_size == 7

    # Test with with -c
    Config._HAS_CONFIG_BEEN_INITIALISED = False
    main([filename, "-c", config_name, "-api", "lfric"])
    assert Config.get().api == "lfric"
    assert Config.has_config_been_initialised() is True
    assert Config.get().reprod_pad_size == 7


def test_main_directory_arg(capsys):
    '''Test the -d option in main().'''

    # No -d option supplied
    filename = os.path.join(LFRIC_BASE_PATH, "1_single_invoke.f90")
    main([filename, "-api", "lfric"])
    # Invalid -d path supplied
    with pytest.raises(SystemExit):
        main([filename, "-api", "lfric", "-d", "invalid"])
    _, output = capsys.readouterr()
    assert "Kernel search path 'invalid' not found" in output
    # Multiple -d paths supplied
    main([filename, "-api", "lfric", "-d", LFRIC_BASE_PATH,
          "-d", NEMO_BASE_PATH])


def test_main_backend_arg(capsys):
    '''Test the --backend options in main().'''
    filename = os.path.join(LFRIC_BASE_PATH, "1_single_invoke.f90")
    # Make sure we get a default config instance
    Config._instance = None
    # Default is to have checks enabled.
    assert Config.get().backend_checks_enabled is True
    main([filename, "-api", "lfric", "--backend-disable-validation"])
    assert Config.get().backend_checks_enabled is False
    assert Config.get().backend_indentation_disabled is False
    Config._instance = None
    filename = os.path.join(NEMO_BASE_PATH, "explicit_do_long_line.f90")
    main([filename, "--backend-disable-indentation"])
    output, _ = capsys.readouterr()
    # None of the three DO loops should be indented.
    assert len(re.findall(r"^do j", output, re.MULTILINE)) == 3
    assert Config.get().backend_checks_enabled is True
    assert Config.get().backend_indentation_disabled is True
    Config._instance = None


def test_main_expected_fatal_error(capsys):
    '''Tests that we get the expected output and the code exits with an
    error when an expected fatal error is returned from the generate
    function.

    '''
    filename = (os.path.join(os.path.dirname(os.path.abspath(__file__)),
                             "test_files", "lfric",
                             "2_incorrect_number_of_args.f90"))
    with pytest.raises(SystemExit) as excinfo:
        main([filename, "-api", "lfric"])
    # the error code should be 1
    assert str(excinfo.value) == "1"
    _, output = capsys.readouterr()
    expected_output = ("Parse Error: Kernel 'testkern_type' called from the "
                       "algorithm layer with an insufficient number of "
                       "arguments as specified by the metadata. Expected at "
                       "least '5' but found '4'.\n")
    assert output == expected_output


def test_code_transformation_skip_files_error(tmpdir, capsys):
    ''' Test that applying recipes in the code-transformation mode skips the
    files marked as FILES_TO_SKIP '''
    code = '''
       ! This is a Fortran file with a funny syntax
       MoDUle             MYmod
       enD    Module      MYmod
    '''
    recipe = '''
FILES_TO_SKIP = ["funny_syntax.f90"]

def trans(psyir):
    assert False
    '''
    inputfile = str(tmpdir.join("funny_syntax.f90"))
    with open(inputfile, "w", encoding='utf-8') as my_file:
        my_file.write(code)
    recipefile = str(tmpdir.join("recipe.py"))
    with open(recipefile, "w", encoding='utf-8') as my_file:
        my_file.write(recipe)

    # Execute the recipe with FILES_TO_SKIP (it should not call the
    # recipe assert because the file is skipped)
    outputfile = str(tmpdir.join("output.f90"))
    main([inputfile, "-s", recipefile, "-o", outputfile])

    # We can also check that the output syntax has not been normalised
    with open(outputfile, "r", encoding='utf-8') as my_file:
        new_code = my_file.read()
    assert new_code == code

    # When doing the same but without a '-o' (output file), we just print
    # in stdout that the file was skipped.
    outputfile = str(tmpdir.join("output.f90"))
    main([inputfile, "-s", recipefile])
    output, _ = capsys.readouterr()
    assert ("funny_syntax.f90' skipped because it is listed in FILES_TO_SKIP."
            in output)


@pytest.mark.parametrize(
         "idx, value, output", [
          ("0", "False", "result = a + b + c"),
          # Indirect import is not resolved
          ("1", "True", "result = 1 + 1 + c"),
          ("2", "[\"module1\"]", "result = 1 + b + c"),
          ("3", "[\"module2\"]", "result = a + 1 + c"),
          # Indirect import resolved by name
          ("4", "[\"module1\",\"module3\"]", "result = 1 + b + 1"),
          # Now change both with case insensitive names
          ("5", "[\"mOdule1\",\"moduLe2\"]", "result = 1 + 1 + c")
          ])
def test_code_transformation_resolve_imports(tmpdir, capsys, monkeypatch,
                                             idx, value, output):
    ''' Test that applying recipes in the code-transformation mode follows the
    selected list of module names when generating the tree. '''

    module1 = '''
        module module1
            use module3
            integer :: a
        end module module1
    '''
    module2 = '''
        module module2
            integer :: b
        end module module2
    '''
    module3 = '''
        module module3
            integer :: c
        end module module3
    '''
    code = '''
        module test
            use module1
            use module2
            real :: result
        contains
            subroutine mytest()
                result = a + b + c
            end subroutine mytest
        end module test
    '''
    recipe = f'''
from psyclone.psyir.nodes import Reference, Literal
from psyclone.psyir.symbols import INTEGER_TYPE

RESOLVE_IMPORTS = {value}

def trans(psyir):
    # Replace all integer references with literal '1', it can only be done if
    # we have the type of the symbol (resolved from the module).
    for ref in psyir.walk(Reference):
        if ref.datatype == INTEGER_TYPE:
            ref.replace_with(Literal("1", INTEGER_TYPE))
    '''
    recipe_name = f"replace_integers_{idx}.py"
    for filename, content in [("module1.f90", module1),
                              ("module2.f90", module2),
                              ("module3.f90", module3),
                              ("code.f90", code),
                              (recipe_name, recipe)]:
        with open(tmpdir.join(filename), "w", encoding='utf-8') as my_file:
            my_file.write(content)

    # Execute the recipe (no -I needed as we have everything at the same place)
    monkeypatch.chdir(tmpdir)
    ModuleManager._instance = None
    main(["code.f90", "-s", recipe_name])
    captured = capsys.readouterr()

    # Compare the generated output to the parametrised expected output
    assert output in str(captured), str(captured)


def test_code_transformation_trans(tmpdir):
    ''' Test that applying recipes that have a trans, and are not listed
    in the FILES_TO_SKIP, executes the recipe transformations. '''
    code = '''
       ! This is a Fortran file with a funny syntax
       MoDUle             MYmod
       enD    Module      MYmod
    '''
    recipe = '''
def trans(psyir):
    psyir.children[0].name = "newname"
    '''
    inputfile = str(tmpdir.join("funny_syntax.f90"))
    with open(inputfile, "w", encoding='utf-8') as my_file:
        my_file.write(code)
    recipefile = str(tmpdir.join("change_name.py"))
    with open(recipefile, "w", encoding='utf-8') as my_file:
        my_file.write(recipe)
    outputfile = str(tmpdir.join("output.f90"))
    main([inputfile, "-s", recipefile, "-o", outputfile])
    # We will get the normalise syntax and the recipe code change
    with open(outputfile, "r", encoding='utf-8') as my_file:
        new_code = my_file.read()
    assert "module newname\n" in new_code


def test_code_transformation_free_form(tmpdir, capsys):
    '''Test that the free-form option works for code transformation.'''
    code = '''
    subroutine test
    integer :: n
    n = 3 + 4
    end subroutine'''
    # Using a fixed format file extension to check the --free-form
    # option is correctly overriding the default behaviour.
    inputfile = str(tmpdir.join("free_form.f"))
    with open(inputfile, "w", encoding='utf-8') as my_file:
        my_file.write(code)
    main([inputfile, "--free-form"])
    captured, _ = capsys.readouterr()
    correct = """subroutine test()
  integer :: n

  n = 3 + 4

end subroutine test"""
    assert correct in captured


def test_code_transformation_fixed_form(tmpdir, capsys, caplog):
    ''' Test that the fixed-form option works for code transformation.'''
    code = '''
      subroutine test
c     Comment here.
      integer n

      n = 3 +
     &4
      end subroutine'''
    inputfile = str(tmpdir.join("fixed_form.f90"))
    with open(inputfile, "w", encoding='utf-8') as my_file:
        my_file.write(code)
    main([inputfile, "--fixed-form"])
    captured, _ = capsys.readouterr()
    correct = """subroutine test()
  integer :: n

  n = 3 + 4

end subroutine test"""
    assert correct in captured

    with pytest.raises(SystemExit) as error:
        main([inputfile])
    with open(inputfile, "w", encoding='utf-8') as my_file:
        my_file.write(code)
    assert error.value.code == 1
    _, err = capsys.readouterr()
    assert "Failed to create PSyIR from file " in err
    assert "File was treated as free form" in err

    # Check that if we use a fixed form file extension we get the expected
    # behaviour.
    code = '''
      subroutine test
c     Comment here.
      integer n

      n = 3 +
     &4
      end subroutine'''
    inputfile = str(tmpdir.join("fixed_form.f"))
    with open(inputfile, "w", encoding='utf-8') as my_file:
        my_file.write(code)
    main([inputfile])
    captured, _ = capsys.readouterr()
    correct = """subroutine test()
  integer :: n

  n = 3 + 4

end subroutine test"""
    assert correct in captured

    caplog.clear()
    # Check an unknown file extension gives a log message and fails for a
    # fixed form input.
    with caplog.at_level(logging.INFO, logger="psyclone.generator"):
        inputfile = str(tmpdir.join("fixed_form.1s2"))
        with open(inputfile, "w", encoding='utf-8') as my_file:
            my_file.write(code)
        with pytest.raises(SystemExit) as error:
            main([inputfile])
        assert error.value.code == 1
        _, err = capsys.readouterr()
        assert "Failed to create PSyIR from file " in err
        assert ("' doesn't end with a recognised file extension. Assuming "
                "free form." in caplog.text)


@pytest.mark.parametrize("validate", [True, False])
def test_code_transformation_backend_validation(validate: bool,
                                                monkeypatch) -> None:
    '''
    Test that the backend validation flag is passed to
    the Fortran writer when using generic code transformations.
    '''

    # Create a dummy Fortran writer, which we use to check
    # the values passed in
    def dummy_fortran_writer(check_global_constraints: bool,
                             disable_copy: bool,
                             indent_string: Optional[str] = None):
        # pylint: disable=unused-argument
        """A dummy function used to test that the FortranWriter
        gets the backend-validation flag as intended.
        """
        assert check_global_constraints is validate
        # The writer must returns some string
        return lambda x: "some-string-doesn't-matter"

    monkeypatch.setattr(generator, "FortranWriter", dummy_fortran_writer)

    # The input file doesn't really matter, so just use a
    # kernel file from gocean:
    input_file = Path(get_base_path("gocean")) / "test27_loop_swap.f90"

    if validate:
        options = []
    else:
        options = ["--backend-disable-validation"]
    main([str(input_file)] + options)
    # The actual assert is in the dummy_fortran_writer function above


def test_code_transformation_parse_failure(tmpdir, caplog, capsys):
    '''
    Test the error handling in the code_transformation_mode() method when
    there is invalid Fortran in the supplied file.

    '''
    code = '''
    prog invalid
      ! This is not valid Fortran
    end prog invalid
    '''
    inputfile = str(tmpdir.join("funny_syntax.f90"))
    with open(inputfile, "w", encoding='utf-8') as my_file:
        my_file.write(code)
    with caplog.at_level(logging.ERROR, logger="psyclone.generator"):
        with pytest.raises(SystemExit):
            code_transformation_mode(inputfile, None, None, False, False,
                                     False)
        _, err = capsys.readouterr()
        assert "Failed to create PSyIR from file '" in err
        assert "Is the input valid Fortran" in caplog.text


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
        "  use constants_mod\n"
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
        main([filename, "-api", "lfric"])
    # the error code should be 1
    assert str(excinfo.value) == "1"
    _, output = capsys.readouterr()
    assert ("The invoke call argument 'setval_c' has been used as the "
            "Algorithm routine name. This is not allowed." in output)


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
    alg, _ = generate(filename, api="lfric")
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
                             "test_files", "lfric",
                             "1_single_invoke.f90"))
    with pytest.raises(SystemExit) as excinfo:
        main([filename, "-api", "lfric"])
    # the error code should be 1
    assert str(excinfo.value) == "1"
    _, output = capsys.readouterr()
    assert ("Error, unexpected exception, please report to the authors:"
            in output)
    assert "Traceback (most recent call last):" in output
    # Python >= 3.14 uses "is not a container or iterable",
    # so we split the assertion for cross-version support
    assert "TypeError: argument of type 'int' is not " in output
    assert "iterable" in output


def test_main_fort_line_length_off(capsys):
    '''Tests that the Fortran line-length limiting is off by default and
    is also disabled by `-l off`. One of the generated psy-layer lines
    should be longer than 132 characters.

    '''
    filename = (os.path.join(os.path.dirname(os.path.abspath(__file__)),
                             "test_files", "lfric",
                             "10.3_operator_different_spaces.f90"))
    main([filename, '-api', 'lfric'])
    output, _ = capsys.readouterr()
    assert not all(len(line) <= 132 for line in output.split('\n'))

    main([filename, '-api', 'lfric', '-l', 'off'])
    output, _ = capsys.readouterr()
    assert not all(len(line) <= 132 for line in output.split('\n'))

    alg_filename = os.path.join(NEMO_BASE_PATH, "explicit_do_long_line.f90")
    main([alg_filename])
    output, _ = capsys.readouterr()
    assert not all(len(line) <= 132 for line in output.split('\n'))

    main([alg_filename, '-l', 'off'])
    output, _ = capsys.readouterr()
    assert not all(len(line) <= 132 for line in output.split('\n'))


def test_main_fort_line_length_output_only(capsys):
    '''Check that the '-l output' option still processes the long lines but
    limits the line lengths in the output.
    '''
    filename = (os.path.join(os.path.dirname(os.path.abspath(__file__)),
                             "test_files", "lfric",
                             "10.3_operator_different_spaces.f90"))
    main([filename, '-api', 'lfric', '-l', 'output'])
    output, _ = capsys.readouterr()
    assert all(len(line) <= 132 for line in output.split('\n'))

    alg_filename = os.path.join(NEMO_BASE_PATH, "explicit_do_long_line.f90")
    main([alg_filename, '-l', 'output'])
    output, _ = capsys.readouterr()
    assert all(len(line) <= 132 for line in output.split('\n'))


def test_main_fort_line_length_all(capsys):
    '''Tests that the Fortran line length object works correctly. With
    the '-l all' option the input Fortran file is checked to verify that
    it complies with the 132 characters standard limit.

    '''
    filename = (os.path.join(os.path.dirname(os.path.abspath(__file__)),
                             "test_files", "lfric",
                             "10.3_operator_different_spaces.f90"))
    with pytest.raises(SystemExit):
        main([filename, '-api', 'lfric', '-l', 'all'])
    _, output = capsys.readouterr()
    assert ("does not conform to the specified 132 line-length limit"
            in output)
    # And for code transformations
    filename = os.path.join(NEMO_BASE_PATH, "explicit_do_long_line.f90")
    with pytest.raises(SystemExit):
        main([filename, '-l', 'all'])
    _, output = capsys.readouterr()
    assert ("does not conform to the specified 132 line-length limit"
            in output)


def test_main_no_invoke_alg_stdout(capsys):
    '''Tests that the main() function outputs the original algorithm input
    file to stdout when the algorithm file does not contain an invoke
    and that it does not produce any psy output.

    '''
    # pass in a kernel file as that has no invokes in it
    kern_filename = (os.path.join(os.path.dirname(os.path.abspath(__file__)),
                                  "test_files", "lfric",
                                  "testkern_mod.F90"))
    main([kern_filename, "-api", "lfric"])
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
                                 "test_files", "lfric",
                                 "1_single_invoke.f90"))

    psy_filename = str(tmpdir.join("psy.f90"))

    main([alg_filename, '-api', 'lfric', '-opsy', psy_filename])

    # check psy file is created
    assert os.path.isfile(psy_filename)

    # extract psy file content
    with open(psy_filename, encoding="utf8") as psy_file:
        psy_str = psy_file.read()
        # check content of generated psy file by comparing it with stdout
        main([alg_filename, '-api', 'lfric'])
        stdout, _ = capsys.readouterr()
        assert psy_str in stdout


def test_main_no_invoke_alg_file(capsys, tmpdir):
    '''Tests that the main() function outputs the original algorithm input
    file to file when the algorithm file does not contain an invoke
    and that it does not produce any psy output.

    '''
    # pass in a kernel file as that has no invokes in it
    kern_filename = (os.path.join(os.path.dirname(os.path.abspath(__file__)),
                                  "test_files", "lfric",
                                  "testkern_mod.F90"))

    alg_filename = str(tmpdir.join("alg.f90"))
    psy_filename = str(tmpdir.join("psy.f90"))
    # no need to delete the files as they have not been created

    main([kern_filename, '-api', 'lfric',
          '-oalg', alg_filename, '-opsy', psy_filename])
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
                                 "test_files", "lfric",
                                 "1_single_invoke.f90"))
    with pytest.raises(SystemExit) as err:
        main([alg_filename, '-api', 'lfric', '-okern', "/does/not/exist"])
    assert str(err.value) == "1"
    _, output = capsys.readouterr()
    assert ("Specified kernel output directory (/does/not/exist) does not "
            "exist" in output)


def test_main_kern_output_no_write(tmpdir, capsys):
    '''Test for when the specified output directory (for transformed
    kernels) cannot be written to.

    '''
    alg_filename = (os.path.join(os.path.dirname(os.path.abspath(__file__)),
                                 "test_files", "lfric",
                                 "1_single_invoke.f90"))
    # Create a new directory and make it readonly
    new_dir = os.path.join(str(tmpdir), "no_write_access")
    os.mkdir(new_dir)
    os.chmod(new_dir, stat.S_IREAD)
    with pytest.raises(SystemExit) as err:
        main([alg_filename, '-api', 'lfric', '-okern', str(new_dir)])
    assert str(err.value) == "1"
    _, output = capsys.readouterr()
    assert (f"Cannot write to specified kernel output directory "
            f"({str(new_dir)})" in output)


def test_main_kern_output_dir(tmpdir):
    '''Test that we can specify a valid kernel output directory.'''

    alg_filename = (os.path.join(os.path.dirname(os.path.abspath(__file__)),
                                 "test_files", "lfric",
                                 "1_single_invoke.f90"))
    main([alg_filename, '-api', 'lfric', '-okern', str(tmpdir)])
    # The specified kernel output directory should have been stored in
    # the configuration object
    assert Config.get().kernel_output_dir == str(tmpdir)

    # If no kernel_output_dir is set, it should default to the
    # current directory
    Config.get().kernel_output_dir = None
    assert Config.get().kernel_output_dir == str(os.getcwd())


def test_enable_cache_flag(tmpdir, monkeypatch):
    ''' Check that if the --enable-cache flag is provided, resolve imports will
    create .psycache files for each imported module.

    '''
    module1 = '''
        module module1
            integer :: a
        end module module1
    '''
    module2 = '''
        module module2
            integer :: b
        end module module2
    '''
    code = '''
        module test
            use module1
            use module2
            real :: result
        contains
            subroutine mytest()
                result = a + b
            end subroutine mytest
        end module test
    '''
    recipe = '''

RESOLVE_IMPORTS = True

def trans(psyir):
    pass
    '''
    recipe_name = "test_cache.py"
    for filename, content in [("module1.f90", module1),
                              ("module2.f90", module2),
                              ("code.f90", code),
                              (recipe_name, recipe)]:
        with open(tmpdir.join(filename), "w", encoding='utf-8') as my_file:
            my_file.write(content)

    # If enable-cache not used, no .psycache files exist
    monkeypatch.chdir(tmpdir)
    ModuleManager._instance = None
    main(["code.f90", "-s", recipe_name])
    assert not os.path.isfile("module1.psycache")
    assert not os.path.isfile("module2.psycache")
    assert not ModuleManager.get()._cache_active

    # If enable-cache is used, it will generate .psycache files for each module
    ModuleManager._instance = None
    main(["code.f90", "-s", recipe_name, "--enable-cache"])
    assert os.path.isfile(tmpdir.join("module1.psycache"))
    assert os.path.isfile(tmpdir.join("module2.psycache"))
    assert ModuleManager.get()._cache_active

    ModuleManager._instance = None


def test_main_include_invalid(capsys, tmpdir):
    '''Check that the main function complains if a non-existent location
    is specified as a search path for INCLUDE files.

    '''
    alg_file = (os.path.join(os.path.dirname(os.path.abspath(__file__)),
                             "nemo", "test_files", "include_stmt.f90"))
    fake_path = tmpdir.join('does_not_exist')
    with pytest.raises(SystemExit) as err:
        main([alg_file, '-I', fake_path.strpath])
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
    with pytest.raises(GenerationError) as err:
        main([alg_file])
    assert ("Found an unresolved Fortran INCLUDE file 'local_mpi.h' while"
            in str(err.value))
    # Now specify two locations to search with only the second containing
    # the necessary header file
    inc_path1 = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                             "test_files")
    inc_path2 = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                             "nemo", "test_files", "include_files")
    main([alg_file, '-I', str(inc_path1), '-I', str(inc_path2)])
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
          "-api", "gocean", "-oalg", algfile])
    # We only check the algorithm layer since we generate the PSy
    # layer from scratch in this API (and thus it contains no
    # non-ASCII characters).
    with open(algfile, "r", encoding="utf8") as afile:
        alg = afile.read().lower()
        assert "max reachable coeff" in alg
        assert "call invoke_0_kernel_utf" in alg
    # Check without PSyKAl DSLs
    test_file = os.path.join(NEMO_BASE_PATH, "utf_char.f90")
    tmp_file = os.path.join(str(tmpdir), "test_psy.f90")
    main(["-o", tmp_file, test_file])
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
    successfully if no script is specified for the lfric (LFRic)
    api. This test uses the new PSyIR approach to modify the algorithm
    layer which is currently in development so is protected by a
    switch. This switch is turned on in this test by monkeypatching.

    '''
    monkeypatch.setattr(generator, "LFRIC_TESTING", True)
    alg, _ = generate(
        os.path.join(BASE_PATH, "lfric", "1_single_invoke.f90"),
        api="lfric")
    # new call replaces invoke
    assert "use single_invoke_psy, only : invoke_0_testkern_type" in alg
    assert "call invoke_0_testkern_type(a, f1, f2, m1, m2)" in alg
    # functor symbol is removed
    assert " testkern_type" not in alg
    # module symbol is removed
    assert "testkern_mod" not in alg
    # _psyclone_builtins symbol (that was added by PSyclone) is removed
    assert "use _psyclone_builtins" not in alg


def test_script_lfric_new(monkeypatch, script_factory):
    '''Test that the generate function in generator.py returns
    successfully if a script (containing both trans_alg() and trans()
    functions) is specified. This test uses the new PSyIR approach to
    modify the algorithm layer which is currently in development so is
    protected by a switch. This switch is turned on in this test by
    monkeypatching.

    '''
    alg_script = script_factory("""
def trans_alg(psyir):
    pass

def trans(psyir):
    pass
    """)
    monkeypatch.setattr(generator, "LFRIC_TESTING", True)
    alg, _ = generate(
        os.path.join(BASE_PATH, "lfric", "1_single_invoke.f90"),
        api="lfric", script_name=alg_script)
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
        os.path.join(BASE_PATH, "lfric",
                     "15.1.2_builtin_and_normal_kernel_invoke.f90"),
        api="lfric")
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
            os.path.join(BASE_PATH, "lfric", "testkern_mod.F90"),
            api="lfric")
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
        f"  use constants_mod\n"
        f"  use field_mod, only : field_type\n"
        f"  type(field_type) :: field1, field2, field3, field4\n"
        f"  real(kind=r_def) :: scalar\n"
        f"  {invoke}(testkern_type(scalar, field1, field2, field3, field4))\n"
        f"end subroutine some_kernel\n"
        f"end module some_kernel_mod\n")
    alg_filename = str(tmpdir.join("alg.f90"))
    with open(alg_filename, "w", encoding='utf-8') as my_file:
        my_file.write(code)
    kern_filename = os.path.join(LFRIC_BASE_PATH, "testkern_mod.F90")
    shutil.copyfile(kern_filename, str(tmpdir.join("testkern_mod.F90")))
    with pytest.raises(GenerationError) as info:
        _, _ = generate(alg_filename, api="lfric")
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
        _, _ = generate(alg_filename, api="gocean")
    assert ("Kernel functor 'compute_cu' in routine 'some_kernel' from "
            "algorithm file '" in str(info.value))
    assert ("alg.f90' must be named in a use statement (found "
            "['kind_params_mod', 'grid_mod', 'field_mod', 'module_mod'])."
            in str(info.value))


@pytest.mark.usefixtures("clear_module_manager_instance")
def test_ignore_pattern():
    '''Checks that we can pass ignore patterns to the module manager.
    '''
    alg = os.path.join(get_base_path("lfric"), "1_single_invoke.f90")
    main(["-api", "lfric", alg,
          "--modman-file-ignore", "abc1",
          "--modman-file-ignore", "abc2"])

    mod_man = ModuleManager.get()
    assert mod_man._ignore_files == set(["abc1", "abc2"])


def test_intrinsic_control_settings(tmpdir):
    '''Checks that the intrinsic output control settings update the config
    correctly'''
    # Create dummy piece of code.
    code = """program test
    end program"""
    filename = str(tmpdir.join("test.f90"))
    with open(filename, "w", encoding='utf-8') as my_file:
        my_file.write(code)
    main([filename, "--backend-add-all-intrinsic-arg-names"])
    assert Config.get().backend_intrinsic_named_kwargs is True


def test_config_overwrite() -> None:
    ''' Test that configuration settings can be overwritten.
    '''

    # First make sure that the default values are as expected:
    assert Config.get().reprod_pad_size == 8
    filename = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                            "test_files", "lfric",
                            "1_single_invoke.f90")

    # Overwrite the config file's reprod_pad_size setting:
    main([filename, "--config-opts", "reprod_pad_size=27"])
    assert Config.get().reprod_pad_size == 27

    # Check error handling
    with pytest.raises(ConfigurationError) as err:
        main([filename, "--config-opts", "DOES_NOT_EXIST=27"])
    assert ("Attempt to overwrite unknown configuration option: "
            "'DOES_NOT_EXIST=27'" in str(err.value))
