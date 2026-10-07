# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2020-23 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

''' Module containing pytest tests for the handling of the U/LBOUND intrinsics
in the PSyIR. '''

import pytest
from fparser.common.readfortran import FortranStringReader
from fparser.two.Fortran2003 import Execution_Part
from psyclone.psyir.nodes import (
    Schedule, Assignment, BinaryOperation, Reference, Literal, IntrinsicCall)
from psyclone.psyir.frontend.fparser2 import Fparser2Reader


@pytest.mark.usefixtures("parser", "disable_declaration_check")
@pytest.mark.parametrize("bound", ["ubound", "lbound"])
@pytest.mark.parametrize("expression", ["n = {0}(a, 3)",
                                        "n = {0}(a(:,:,:), 3)",
                                        "n = {0}(a, idx1 + 3)"])
def test_bound_intrinsics(bound, expression):
    ''' Basic test that the UBOUND and LBROUND intrinsics are recognised
    and represented in the PSyIR.

    TODO #754 fix test so that 'disable_declaration_check' fixture is not
    required.
    '''
    fake_parent = Schedule()
    processor = Fparser2Reader()
    reader = FortranStringReader(expression.format(bound))
    fp2intrinsic = Execution_Part(reader).content[0]
    processor.process_nodes(fake_parent, [fp2intrinsic])
    assert isinstance(fake_parent[0], Assignment)
    assert isinstance(fake_parent[0].rhs, IntrinsicCall)
    if bound == "ubound":
        assert fake_parent[0].rhs.intrinsic == IntrinsicCall.Intrinsic.UBOUND
    else:
        assert fake_parent[0].rhs.intrinsic == IntrinsicCall.Intrinsic.LBOUND
    assert isinstance(fake_parent[0].rhs.arguments[0], Reference)
    assert isinstance(fake_parent[0].rhs.arguments[1],
                      (Literal, BinaryOperation))
