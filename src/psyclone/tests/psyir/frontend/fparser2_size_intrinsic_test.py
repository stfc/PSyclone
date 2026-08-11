# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2019-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

''' Module containing pytest tests for the handling of the SIZE intrinsic
in the PSyIR. '''

import pytest
from fparser.common.readfortran import FortranStringReader
from fparser.two.Fortran2003 import Execution_Part
from psyclone.psyir.frontend.fparser2 import Fparser2Reader
from psyclone.psyir.nodes import Schedule, Assignment, IntrinsicCall, \
    Reference, Literal


@pytest.mark.parametrize("expression", ["n = SIZE(a, 3)",
                                        "n = SIZE(a(:,:,:), 3)"])
@pytest.mark.usefixtures("disable_declaration_check", "parser")
def test_size(expression):
    ''' Basic test that the SIZE intrinsic is recognised and represented
    in the PSyIR.

    TODO #754 fix test so that 'disable_declaration_check' fixture is not
    required.
    '''
    fake_parent = Schedule()
    processor = Fparser2Reader()
    reader = FortranStringReader(expression)
    fp2intrinsic = Execution_Part(reader).content[0]
    processor.process_nodes(fake_parent, [fp2intrinsic])
    assert isinstance(fake_parent[0], Assignment)
    assert isinstance(fake_parent[0].rhs, IntrinsicCall)
    assert fake_parent[0].rhs.intrinsic == IntrinsicCall.Intrinsic.SIZE
    assert isinstance(fake_parent[0].rhs.arguments[0], Reference)
    assert isinstance(fake_parent[0].rhs.arguments[1], Literal)
