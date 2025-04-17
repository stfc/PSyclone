# Copyright (c) 2019 Science and Technology Facilities Council
#
# All rights reserved.
#
# Modifications made as part of the fparser project are distributed
# under the following license:
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are
# met:
#
# 1. Redistributions of source code must retain the above copyright
# notice, this list of conditions and the following disclaimer.
#
# 2. Redistributions in binary form must reproduce the above copyright
# notice, this list of conditions and the following disclaimer in the
# documentation and/or other materials provided with the distribution.
#
# 3. Neither the name of the copyright holder nor the names of its
# contributors may be used to endorse or promote products derived from
# this software without specific prior written permission.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
# "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
# LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
# A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
# HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
# SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
# LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
# DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
# THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
# (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
# OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

"""Test fparser one scripts

"""

import os
import sys
from fparser.scripts import parse

# Section 1: tests for pytest.py:runner


def test_runner(capsys, tmpdir, monkeypatch):
    """Test that the main function works as expected."""
    # Create a temporary file containing Fortran code.
    my_file = tmpdir.mkdir("sub").join("hello.f90")
    my_file.write("program hello\nend program hello\n")
    # Use monkeypatch to spoof the command-line argument
    monkeypatch.setattr(sys, "argv", ["read", str(my_file)])
    # run the relevant script method (main())
    parse.main()
    # capture the output and check that the code has been output
    stdout, _ = capsys.readouterr()
    print(stdout)
    assert (
        "    Program\n"
        "      blocktype='program'\n"
        "      name='hello'\n"
        "      item=Line('program hello',(1, 1),None,None,<reader>)\n"
        "      content:\n"
        "    EndProgram\n"
        "      blocktype='program'\n"
        "      name='hello'\n"
        "      item=Line('end program hello',"
        "(2, 2),None,None,<reader>)"
    ) in stdout


def test_log(caplog, monkeypatch):
    """Test that logging is enabled and works as expected."""
    my_file = os.path.join(os.path.dirname(os.path.abspath(__file__)), "bad_char.f90")
    # Use monkeypatch to spoof the command-line argument
    monkeypatch.setattr(sys, "argv", ["read", str(my_file)])
    # run the relevant script method (main())
    parse.main()
    # Check the log messages
    for record in caplog.records:
        assert record.levelname != "CRITICAL"
    assert (
        "Skipped bad character in input file. Error returned was " "'utf"
    ) in caplog.text
    # Output can be utf8 or utf-8 so split test in two.
    assert "8' codec can't decode byte " in caplog.text
    # Can't check the actual value as some versions of Python3 return
    # a different value to the one above.
    assert "in position 1815: invalid continuation byte." in caplog.text
