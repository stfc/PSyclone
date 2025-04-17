# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2023, Science and Technology Facilities Council.
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

"""
    Module containing tests for Fortran2003 where_construct_stmt rule R745.

    R745 where-construct-stmt is [where-construct-name:] WHERE ( mask-expr )

"""
import pytest

from fparser.common.sourceinfo import FortranFormat
from fparser.one.parsefortran import FortranParser
from fparser.common.readfortran import FortranStringReader


@pytest.mark.parametrize("name", ["", "name"])
def test_where_construct_stmt(name):
    """Test that the Fortran2003 where_construct_stmt rule R745 works
    correctly. Test with and without the optional
    where-construct-name.

    R745 where-construct-stmt is [where-construct-name:] WHERE ( mask-expr )

    """
    space = ""
    colon = ""
    if name:
        colon = ":"
        space = " "
    code = (
        f"  SUBROUTINE columnwise_code()\n"
        f"    {name}{colon}{space}WHERE ( col_mat(:,:,cell) /= 0.0_r_solve )\n"
        f"      col_mat(:,:,cell) = 1.0_r_solver/col_mat(:,:,cell)\n"
        f"    END WHERE{space}{name}\n"
        f"  END SUBROUTINE columnwise_code"
    )
    reader = FortranStringReader(code)
    reader.set_format(FortranFormat(True, False))
    parser = FortranParser(reader)
    parser.parse()
    mod = parser.block.content[0]
    assert str(mod) == code
