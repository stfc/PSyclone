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

""" Module containing pytest tests for the fparser2 Base class. """

import pytest
from fparser.api import get_reader
from fparser.two import Fortran2003
from fparser.two.utils import walk


TEST_CODE = (
    "program hello\n"
    "  implicit none\n"
    "  integer :: var1, ji\n"
    "  real(wp), dimension(10,10) :: var2\n"
    "  write(*,*) 'Guten Tag'\n"
    "  do ji = 1, var1\n"
    "    var2(ji, 5) = -1.0\n"
    "  end do\n"
    "  if(var1 < 3)then\n"
    "    call a_routine(var2)\n"
    "  end if\n"
    "end program hello\n"
)


@pytest.mark.usefixtures("f2003_create")
def test_parent_info():
    """Check that parent information is correctly set-up in the
    parse tree."""
    from fparser.two.utils import Base

    reader = get_reader(TEST_CODE)
    main = Fortran2003.Program(reader)
    node_list = walk(main)

    # Root node in the parse tree has no parent
    parent_prog = node_list[0]
    assert parent_prog.parent is None

    # Check connectivity of all non-string nodes
    for node in node_list[1:]:
        if isinstance(node, Base):
            for child in node.children:
                if isinstance(child, Base):
                    assert child.parent is node
                    assert child.get_root() is parent_prog


@pytest.mark.usefixtures("f2003_create")
def test_children_property():
    """Test that the children property of Base returns the correct
    results for both statements and expressions."""
    reader = get_reader(TEST_CODE)
    main = Fortran2003.Program(reader)
    # Check that children returns items when we have an expression
    writes = walk(main, Fortran2003.Write_Stmt)
    assert writes[0].children is writes[0].items
    assert len(writes[0].children) == 2
    # Check that it returns content when we have a statement
    do_stmts = walk(main, Fortran2003.Block_Nonlabel_Do_Construct)
    assert do_stmts[0].children is do_stmts[0].content
    assert len(do_stmts[0].children) == 3
