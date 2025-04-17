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

"""Test Fortran 2008 rule R814_2

    block-do-construct is do-stmt
                          do-block
                          end-do

The implementation in fparser2 actually implements the case when
do-stmt is a nonlabel-do-stmt (hence the name R814_2). R814_1 implements
the case when do-stmt is a label-do-stmt.

The only difference to F2003 rule R835 is that we force this rule to
use the F2008 version of nonlabel-do-stmt

"""
import pytest

from fparser.api import get_reader
from fparser.two.Fortran2008 import Block_Nonlabel_Do_Construct


@pytest.mark.usefixtures("f2008_create")
def test_concurrent():
    """Test that the Fortran2008 version supports do concurrent."""
    code = "DO CONCURRENT (i = 1 : 20)\n  a(i) = 0.0\nEND DO"
    reader = get_reader(code)
    obj = Block_Nonlabel_Do_Construct(reader)
    assert isinstance(obj, Block_Nonlabel_Do_Construct)
    assert str(obj) == code


def test_functional(f2008_parser):
    """The 2008 version of the Block_Nonlabel_Do_Construct class is only
    added to make sure that that an non-labelled do concurrent (where
    end do is used) is parsed in f2008. Therefore add a functional
    test to make sure this class does its job.

    """
    code = (
        "PROGRAM test\n"
        "  INTEGER :: i\n"
        "  REAL :: a(20)\n"
        "  DO CONCURRENT (i = 1 : 20)\n"
        "    a(i) = 0.0\n"
        "  END DO\n"
        "END PROGRAM"
    )
    tree = f2008_parser(get_reader(code))
    assert str(tree) == code
