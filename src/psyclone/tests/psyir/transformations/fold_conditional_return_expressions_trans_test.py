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

from fparser.common.readfortran import FortranStringReader
from psyclone.psyir.transformations import \
        FoldConditionalReturnExpressionsTrans
from psyclone.psyir.backend.fortran import FortranWriter
from psyclone.psyir.frontend.fparser2 import Fparser2Reader
from psyclone.psyir.transformations import TransformationError


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


SUB_IN1 = (
    "subroutine sub1(i, a)\n"
    "  real, intent(inout) :: a\n"
    "  integer, intent(in) :: i\n"
    "  if (i < 5) then\n"
    "    return\n"
    "  endif\n"
    "  if (i > 10) then\n"
    "    ! Comments do not matter\n"
    "    return\n"
    "    a=2.0 ! Dead code do not matter\n"
    "  endif\n"
    "  a=0.0\n"
    "end subroutine\n")
SUB_OUT1 = (
    "subroutine sub1(i, a)\n"
    "  real, intent(inout) :: a\n"
    "  integer, intent(in) :: i\n\n"
    "  if (.NOT.i < 5) then\n"
    "    if (.NOT.i > 10) then\n"
    "      a = 0.0\n"
    "    end if\n"
    "  end if\n\n"
    "end subroutine sub1\n")

# Tests with preceding code that is not part of the mask
SUB_IN2 = (
    "subroutine sub1(i, a)\n"
    "  real, intent(inout) :: a\n"
    "  integer, intent(in) :: i\n"
    "  {0}\n"
    "  if (i < 5) then\n"
    "    return\n"
    "  endif\n"
    "  if (i > 10) then\n"
    "    return\n"
    "  endif\n"
    "  a=0.0\n"
    "end subroutine\n")
SUB_IN2_1 = SUB_IN2.format("a = 0.0")
SUB_IN2_2 = SUB_IN2.format(
    "if (i > 20) then\n"
    "    a=0.0\n"
    "    return\n"
    "  end if")
SUB_IN2_3 = SUB_IN2.format(
    "if (i > 20) then\n"
    "    return\n"
    "  else\n"
    "    a=0.0\n"
    "  end if")
SUB_OUT2 = (
    "subroutine sub1(i, a)\n"
    "  real, intent(inout) :: a\n"
    "  integer, intent(in) :: i\n\n"
    "  {0}\n"
    "  if (.NOT.i < 5) then\n"
    "    if (.NOT.i > 10) then\n"
    "      a = 0.0\n"
    "    end if\n"
    "  end if\n\n"
    "end subroutine sub1\n")
SUB_OUT2_1 = SUB_OUT2.format("a = 0.0")
SUB_OUT2_2 = SUB_OUT2.format(
    "if (i > 20) then\n"
    "    a = 0.0\n"
    "    return\n"
    "  end if")
SUB_OUT2_3 = SUB_OUT2.format(
    "if (i > 20) then\n"
    "    return\n"
    "  else\n"
    "    a = 0.0\n"
    "  end if")

test_cases = [(SUB_IN1, SUB_OUT1), (SUB_IN2_1, SUB_OUT2_1),
              (SUB_IN2_2, SUB_OUT2_2), (SUB_IN2_3, SUB_OUT2_3)]


@pytest.mark.parametrize("test_case", [0, 1, 2, 3])
def test_transformation(parser, test_case):
    ''' Check that the transformation works as expected. '''
    input_code, expected = test_cases[test_case]
    trans = FoldConditionalReturnExpressionsTrans()
    processor = Fparser2Reader()
    reader = FortranStringReader(input_code)
    parse_tree = parser(reader)
    subroutine = processor.generate_psyir(parse_tree)
    trans.apply(subroutine)
    writer = FortranWriter()
    print(writer(subroutine))
    print(expected)
    assert writer(subroutine) == expected
