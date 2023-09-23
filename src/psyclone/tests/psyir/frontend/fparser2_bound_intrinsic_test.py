# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2020-23, Science and Technology Facilities Council.
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
# Author: A. R. Porter, STFC Daresbury Laboratory
# Modified: S. Siso, STFC Daresbury Laboratory

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
    assert isinstance(fake_parent[0].rhs.children[0], Reference)
    assert isinstance(fake_parent[0].rhs.children[1],
                      (Literal, BinaryOperation))
