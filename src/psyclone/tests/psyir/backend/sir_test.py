# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2019, Science and Technology Facilities Council.
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
# Author R. W. Ford, STFC Daresbury Lab
# -----------------------------------------------------------------------------

'''Performs pytest tests on the psyclone.psyir.backend.sir module'''

import pytest
from fortran_test import create_schedule
from psyclone.psyir.backend.sir import gen_stencil
from psyclone.psyir.backend.base import VisitorError


# [1/3] function gen_stencil
def test_gen_stencil_1():
    '''Check the gen_stencil function produces the expected dimension
    strings.

    '''
    for form, expected in [("i,j,k,l,m", "[0,0,0,0,0]"),
                           ("i+1,j-1", "[1,-1]"),
                           ("m+7", "[7]"),
                           (" i + 1 , j , k - 1 ", "[1,0,-1]"),
                           ("i+1,j-2,k+3,l-4", "[1,-2,3,-4]"),
                           ("i+(1), j-(2)", "[1,-2]")]:
        code = (
            "module test\n"
            "contains\n"
            "  subroutine tmp()\n"
            "    real :: a(1,1,1)\n"
            "    integer :: i,j,k,l,m\n"
            "    a({0})=1.0\n"
            "  end subroutine tmp\n"
            "end module test\n".format(form))
        schedule = create_schedule(code)
        assignment = schedule.children[0]
        array_reference = assignment.children[0]
        result = gen_stencil(array_reference)
        assert result == expected


# [2/3] function gen_stencil
def test_gen_stencil_2():
    '''Check the gen_stencil function raises an exception when
    a node of the wrong type is provided.

    '''
    code = (
        "module test\n"
        "contains\n"
        "  subroutine tmp()\n"
        "  end subroutine tmp\n"
        "end module test\n")
    schedule = create_schedule(code)
    with pytest.raises(VisitorError) as excinfo:
        _ = gen_stencil(schedule)
    assert "gen_stencil expected an Array as input" in str(excinfo.value)


# [3/3] function gen_stencil
def test_gen_stencil_3():
    '''Check the gen_stencil function raises an exception when an
    unsupported form of indexing is found. Currently only "var +/-
    int" is supported.

    '''
    for form in ["1", "1+i", "-1+i", "i+j", "i+1+1", "i+(1+1)", "i*2"]:
        code = (
            "module test\n"
            "contains\n"
            "  subroutine tmp()\n"
            "    real :: a(1,1)\n"
            "    integer :: i,j\n"
            "    a({0})=1.0\n"
            "  end subroutine tmp\n"
            "end module test\n".format(form))
        schedule = create_schedule(code)
        assignment = schedule.children[0]
        array_reference = assignment.children[0]
        with pytest.raises(VisitorError) as excinfo:
            result = gen_stencil(array_reference)
        if form in ["1"]:
            error = "unsupported (non-stencil) index found"
        elif form in ["i*2"]:
            error = "unsupported stencil operator found"
        else:
            error = "unsupported stencil index found"
        assert error in str(excinfo.value)
