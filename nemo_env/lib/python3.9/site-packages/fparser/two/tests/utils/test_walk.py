# Copyright (c) 2020 Science and Technology Facilities Council.

# All rights reserved.

# Modifications made as part of the fparser project are distributed
# under the following license:

# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are
# met:

# 1. Redistributions of source code must retain the above copyright
# notice, this list of conditions and the following disclaimer.

# 2. Redistributions in binary form must reproduce the above copyright
# notice, this list of conditions and the following disclaimer in the
# documentation and/or other materials provided with the distribution.

# 3. Neither the name of the copyright holder nor the names of its
# contributors may be used to endorse or promote products derived from
# this software without specific prior written permission.

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

""" Test the walk() routine provided by utils.py. """

import pytest
from fparser.api import get_reader
from fparser.two.utils import walk
from fparser.two import Fortran2003
from fparser.common.readfortran import FortranStringReader
from fparser.two.parser import ParserFactory
from fparser.two.Fortran2003 import Name, Block_Nonlabel_Do_Construct


@pytest.mark.usefixtures("f2003_create")
def test_walk():
    """Test the walk() utility."""
    reader = get_reader(
        "program hello\n"
        "write(*,*) 'hello'\n"
        "write(*,*) 'goodbye'\n"
        "end program hello\n"
    )
    main = Fortran2003.Program(reader)
    # Check that walk produces the same result whether or not the
    # starting node is in a list.
    all_nodes = walk(main)
    all_nodes2 = walk([main])
    assert all_nodes == all_nodes2
    # Check that we can pull out nodes of a particular type
    all_writes = walk(main, types=Fortran2003.Write_Stmt)
    assert len(all_writes) == 2
    assert "hello" in str(all_writes[0])
    assert "goodbye" in str(all_writes[1])
    # Walk from a list of sibling nodes
    io_units = walk(all_writes, types=Fortran2003.Io_Unit)
    assert len(io_units) == 2
    # Should get empty list if no matching nodes found
    node_list = walk(all_writes, Fortran2003.Execution_Part)
    assert node_list == []


@pytest.mark.usefixtures("f2003_create")
def test_walk_debug(capsys):
    """Test the debug output of the walk() utility."""
    reader = get_reader(
        "program just_a_test\n"
        "if(.true.)then\n"
        "  b = 1\n"
        "end if\n"
        "end program just_a_test\n"
    )
    main = Fortran2003.Program(reader)
    _ = walk(main, debug=True)
    stdout, _ = capsys.readouterr()
    assert stdout.startswith("child type = ")
    assert "Main_Program" in stdout
    assert "If_Construct" in stdout
    assert "Assignment" in stdout
    assert "Int_Literal_Constant" in stdout
    assert "End_If_Stmt" in stdout
    # Walk only part of the tree and specify an indent
    if_constructs = walk(main, Fortran2003.If_Construct)
    _ = walk(if_constructs[0], indent=4, debug=True)
    stdout, _ = capsys.readouterr()
    assert stdout.startswith(8 * " " + "child type =")
    assert "Program" not in stdout


def test_walk_tuples():
    """Check that the walk utility properly visits tuples.
    See related issue : https://github.com/stfc/fparser/issues/367
    and pull request: https://github.com/stfc/fparser/pull/368
    """
    source_str = """\
program test
    integer :: iterator, size
    integer, dimension(0:10) :: A, B, C
    size = 10
    do iterator = 0,size
      A(iterator) = B(iterator) * C(iterator)
    enddo
end program test
"""
    expected = {"A", "B", "C", "iterator", "size"}
    reader = FortranStringReader(source_str, ignore_comments=True)
    f_parser = ParserFactory().create(std="f2008")
    parse_tree = f_parser(reader)
    doloops = walk(parse_tree, Block_Nonlabel_Do_Construct)
    assert len(doloops) == 1
    doloop = doloops[0]
    allNames = walk(doloop, Name, 0)
    assert len(allNames) == 8
    identifierSet = set(map(lambda x: x.tostr(), allNames))
    assert identifierSet == expected
