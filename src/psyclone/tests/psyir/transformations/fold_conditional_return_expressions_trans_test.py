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
# Author S. Siso, STFC Daresbury Lab

'''Module containing tests for the FoldConditionalReturnExpressionsTrans
transformation.'''

from __future__ import absolute_import
import pytest

from psyclone.psyir.transformations import \
        FoldConditionalReturnExpressionsTrans
from psyclone.psyir.backend.fortran import FortranWriter
from psyclone.psyir.transformations import TransformationError


SUB_IN = (
    "subroutine sub1(i, a)\n"
    "real, intent(inout) :: a\n"
    "integer, intent(in) :: i\n"
    " if (i < 5) then\n"
    "    return\n"
    " endif\n"
    " if (i > 10) then\n"
    "    return\n"
    " endif\n"
    "a=0.0\n"
    "end subroutine\n")
SUB_OUT = (
    "subroutine sub1(i, a)\n"
    "real, intent(inout) :: a\n"
    "integer, intent(in) :: i\n"
    " if (.not.(i < 5)) then\n"
    "   if (.not.(i > 10)) then\n"
    "     a=0.0\n"
    "   endif\n"
    " endif\n"
    "end subroutine\n")


def test_description():
    ''' Check that the transformation returns the expected strings '''
    trans = FoldConditionalReturnExpressionsTrans()
    assert trans.name == "FoldConditionalReturnExpressionsTrans"
    assert str(trans) == \
        "Fold all conditional expressions with Return statements."


def test_validation():
    ''' Check that the transformation can only be applied to routine nodes '''
    trans = FoldConditionalReturnExpressionsTrans()
    with pytest.raises(TransformationError) as info:
        trans.apply(None)
    assert("Error in FoldConditionalReturnExpressionsTrans transformation. "
           "This transformation can only be applied to Routine nodes."
           in str(info.value))


@pytest.mark.parametrize("input_code, expected",
                         [(SUB_IN, SUB_OUT)])
def test_transformation(input_code, expected):
    ''' Check that the transformation works as expected. '''
    trans = FoldConditionalReturnExpressionsTrans()
    assert True
