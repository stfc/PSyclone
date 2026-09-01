# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2021-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

'''Module containing tests for the FoldConditionalReturnExpressionsTrans
transformation.'''

import pytest

from psyclone.psyir.transformations import \
        FoldConditionalReturnExpressionsTrans
from psyclone.psyir.transformations import TransformationError


def test_description():
    ''' Check that the transformation returns the expected strings '''
    trans = FoldConditionalReturnExpressionsTrans()
    assert trans.name == "FoldConditionalReturnExpressionsTrans"
    assert str(trans) == ("Re-structure kernel statements to eliminate "
                          "conditional Return expressions.")


def test_validation():
    ''' Check that the transformation can only be applied to routine nodes '''
    trans = FoldConditionalReturnExpressionsTrans()
    with pytest.raises(TransformationError) as info:
        trans.apply(None)
    assert ("Error in FoldConditionalReturnExpressionsTrans transformation. "
            "This transformation can only be applied to 'Routine' nodes, but "
            "found 'NoneType'." in str(info.value))


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
    "    a=2.0 ! Dead code does not matter\n"
    "  endif\n"
    "  a=0.0\n"
    "  a=1.0\n"
    "  a=2.0\n"
    "end subroutine\n")
SUB_OUT1 = (
    "subroutine sub1(i, a)\n"
    "  real, intent(inout) :: a\n"
    "  integer, intent(in) :: i\n\n"
    "  if (.NOT.i < 5) then\n"
    "    if (.NOT.i > 10) then\n"
    "      a = 0.0\n"
    "      a = 1.0\n"
    "      a = 2.0\n"
    "    end if\n"
    "  end if\n\n"
    "end subroutine sub1\n")

# Tests with preceding code before the mask condition, this part of the code
# won't be folded. Note that this includes If blocks with return statements
# similar, but not exactly, like a conditional mask because:
#  SUB_IN2_2: has an execution statement before the return statement.
#  SUB_IN2_3: has an else conditional branch.
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
def test_transformation(fortran_reader, fortran_writer, test_case):
    ''' Check that the transformation works as expected. '''
    input_code, expected = test_cases[test_case]
    trans = FoldConditionalReturnExpressionsTrans()
    file_container = fortran_reader.psyir_from_source(input_code)
    subroutine = file_container.children[0]
    trans.apply(subroutine)
    assert fortran_writer(subroutine) == expected
