# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2021, Science and Technology Facilities Council.
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
# Authors R. W. Ford and A. R. Porter, STFC Daresbury Lab

'''A module to perform pytest tests on the code in the main.py file
within the psyad directory.

'''
from __future__ import print_function, absolute_import
import logging
import six
import pytest
from psyclone.psyad import main

# main function


# -h description
def test_main_h_option(capsys):
    '''Test that the -h script option works as expected'''

    with pytest.raises(SystemExit) as info:
        main(["-h", "filename"])
    assert str(info.value) == "0"
    output, error = capsys.readouterr()
    assert error == ""
    # The name of the executable is replaced with either pytest or -c
    # when using pytest, therefore we split this test into sections.
    expected1 = "usage: "
    expected2 = (
        "[-h] [-oad OAD] [-v] -a ACTIVE [ACTIVE ...] -- filename\n\n"
        "Run the PSyclone adjoint code generator on an LFRic tangent-linear "
        "kernel file\n\n"
        "positional arguments:\n"
        "  filename              LFRic tangent-linear kernel source\n\n"
        "optional arguments:\n"
        "  -h, --help            show this help message and exit\n"
        "  -a ACTIVE [ACTIVE ...], --active ACTIVE [ACTIVE ...]\n"
        "                        active variable names\n"
        "  -oad OAD              filename for the transformed code\n"
        "  -v, --verbose         increase the verbosity of the output\n")
    assert expected1 in output
    assert expected2 in output


# no args
def test_main_no_args(capsys):
    '''Test that the main() function raises an exception if the required
    arguments are not supplied.

    '''
    with pytest.raises(SystemExit) as info:
        main([])
    assert str(info.value) == "2"
    output, error = capsys.readouterr()
    assert output == ""
    # Python2 returns a different message to Python3. Also, the name
    # of the executable is replaced with either pytest or -c when
    # using pytest, therefore we split the test into sections.
    expected1 = "usage: "
    expected2 = "[-h] [-oad OAD] [-v] -a ACTIVE [ACTIVE ...] -- filename"
    if six.PY2:
        expected3 = "error: too few arguments\n"
    else:
        expected3 = ("error: the following arguments are required: "
                     "-a/--active, filename")
    assert expected1 in error
    assert expected2 in error
    assert expected3 in error


# no -a
def test_main_no_a_arg(capsys):
    '''Test that the main() function raises an exception if the -a
    argument is not supplied.

    '''
    with pytest.raises(SystemExit) as info:
        main(["file"])
    assert str(info.value) == "2"
    output, error = capsys.readouterr()
    assert output == ""
    expected = ("error: the following arguments are required: "
                "-a/--active")
    assert expected in error


# no -a arg arg
def test_main_no_a_arg_arg(capsys):
    '''Test that the main() function raises an exception if an argument to
    the -a argument is not supplied.

    '''
    with pytest.raises(SystemExit) as info:
        main(["file", "-a"])
    assert str(info.value) == "2"
    output, error = capsys.readouterr()
    assert output == ""
    expected = ("error: argument -a/--active: expected at least one "
                "argument")
    assert expected in error


# no filename
def test_main_no_filename(capsys):
    '''Test that the main() function raises an exception if no filename is
    supplied.

    '''
    with pytest.raises(SystemExit) as info:
        main(["-a", "var"])
    assert str(info.value) == "2"
    output, error = capsys.readouterr()
    assert output == ""
    expected = "error: the following arguments are required: filename\n"
    assert expected in error


# invalid filename
def test_main_invalid_filename(capsys):
    '''Test that the the main() function raises an exception if the
    file specified by filename does not exist.

    '''
    # FileNotFoundError does not exist in Python2
    try:
        FileNotFoundError
    except NameError:
        # pylint: disable=redefined-builtin
        FileNotFoundError = IOError
    with pytest.raises(SystemExit):
        main(["-a", "arg", "--", "does_not_exist.f90"])
    output, error = capsys.readouterr()
    assert output == ""
    expected = "psyad: error: file 'does_not_exist.f90', not found."
    assert expected in error


# writing to stdout
def test_main_stdout(tmpdir, capsys):
    '''Test that the the main() function returns its output to stdout by
    default.

    '''
    tl_code = (
        "program test\n"
        "integer :: a\n"
        "a = 0.0\n"
        "end program test\n")
    expected = (
        "program test\n"
        "  integer :: a\n\n"
        "  a = 0.0\n\n"
        "end program test\n")
    filename = six.text_type(tmpdir.join("tl.f90"))
    with open(filename, "a") as my_file:
        my_file.write(tl_code)
    main(["-a", "a", "--", filename])
    output, error = capsys.readouterr()
    assert error == ""
    assert expected in output


# using -oad
def test_main_fileout(tmpdir, capsys):
    '''Test that the the main() function returns its output to a file if
    specified.

    '''
    tl_code = (
        "program test\n"
        "integer :: a\n"
        "a = 0.0\n"
        "end program test\n")
    expected = (
        "program test\n"
        "  integer :: a\n\n"
        "  a = 0.0\n\n"
        "end program test\n")
    filename_in = str(tmpdir.join("tl.f90"))
    filename_out = str(tmpdir.join("ad.f90"))
    with open(filename_in, "a") as my_file:
        my_file.write(tl_code)
    main([filename_in, "-oad", filename_out, "-a", "a"])
    output, error = capsys.readouterr()
    assert error == ""
    assert output == ""
    with open(filename_out, 'r') as my_file:
        data = my_file.read()
    assert expected in data


# -v output
@pytest.mark.xfail(reason="issue #1235: caplog returns an empty string in "
                   "github actions.", strict=False)
def test_main_verbose(tmpdir, capsys, caplog):
    '''Test that the the main() function outputs additional information if
    the -v flag is set. Actually -v seems to have no effect here as
    pytest takes control of the logging and we have to set it to the
    required level manually.

    '''
    tl_code = (
        "program test\n"
        "integer :: a\n"
        "a = 0.0\n"
        "end program test\n")
    filename_in = str(tmpdir.join("tl.f90"))
    filename_out = str(tmpdir.join("ad.f90"))
    with open(filename_in, "a") as my_file:
        my_file.write(tl_code)
    with caplog.at_level(logging.DEBUG):
        main([filename_in, "-v", "-oad", filename_out, "-a", "a"])

    output, error = capsys.readouterr()
    assert error == ""
    assert output == ""
    assert ("INFO     psyclone.psyad.main:main.py:89 Reading kernel file /"
            in caplog.text)
    assert "/tl.f90" in caplog.text
    assert "/tl.f90" in caplog.text
    assert ("INFO     psyclone.psyad.main:main.py:109 Writing adjoint of "
            "kernel to file /" in caplog.text)
    assert "/ad.f90" in caplog.text


def test_active(tmpdir, capsys):
    '''Test that active variable names provided via the -a option are
    passed on to to the generate_adjoint_str function and that the
    code aborts with the expected messages when they are not found in
    the code or result in invalid tangent linear code.

    '''
    tl_code = (
        "program test\n"
        "integer :: a, b\n"
        "a = b\n"
        "end program test\n")
    filename_in = str(tmpdir.join("tl.f90"))
    with open(filename_in, "a") as my_file:
        my_file.write(tl_code)
    # invalid active variable
    with pytest.raises(SystemExit):
        main([filename_in, "-a", "c"])
    output, error = capsys.readouterr()
    assert output == ""
    assert "psyad: error: \"Could not find 'c' in the Symbol Table.\"" in error
    # invalid tangent linear code
    with pytest.raises(SystemExit):
        main([filename_in, "-a", "b"])
    output, error = capsys.readouterr()
    assert output == ""
    assert (
        "psyad: error: TangentLinearError: Assignment node 'a = b\n' has "
        "active variables on its RHS but its LHS 'a' is not an active "
        "variable." in error)
