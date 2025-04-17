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

"""Test Fortran 2008 rule R816

    label-do-stmt is [ do-construct-name : ] DO label [ loop-control ]

The only difference to F2003 rule R828 is that we force this rule to
use the F2008 version of loop-control

"""
import pytest

from fparser.api import get_reader
from fparser.two.Fortran2008 import Label_Do_Stmt


@pytest.mark.usefixtures("f2008_create")
def test_concurrent():
    """Test that the Fortran2008 version supports do concurrent."""
    code = "DO 10 CONCURRENT (i = 1 : 20)"
    reader = get_reader(code)
    obj = Label_Do_Stmt(reader)
    assert isinstance(obj, Label_Do_Stmt)
    assert str(obj) == code


# The 2008 version of the Label_Do_Stmt class is only added to make
# sure that that a labelled do concurrent is parsed in f2008. There
# are already functional tests (called test_functional) in
# test_block_label_do_construct_r814_1.py and
# test_action_term_do_construct_r824.py which make sure this class
# does its job.
