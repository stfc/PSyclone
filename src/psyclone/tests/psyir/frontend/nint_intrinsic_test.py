# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2019, Science and Technology Facilities Council
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
# Author A. R. Porter, STFC Daresbury Laboratory

''' Module containing pytest tests for the handling of the NINT intrinsic
in the PSyIR fparser2 frontend. '''

from __future__ import absolute_import

import pytest
from psyclone.psyir.frontend.fparser2 import Fparser2Reader
from fparser.common.readfortran import FortranStringReader


@pytest.mark.parametrize("expression", ["n = NINT(a)",
                                        "n = NINT(a(1,1))",
                                        "n = NINT(a, my_kind)"])
def test_size(expression, parser):
    ''' Basic test that the NINT intrinsic is recognised and represented
    in the PSyIR. '''
    from fparser.two.Fortran2003 import Execution_Part
    from psyclone.psyGen import Schedule, Assignment, UnaryOperation, \
        Reference
    fake_parent = Schedule()
    processor = Fparser2Reader()
    reader = FortranStringReader(expression)
    fp2intrinsic = Execution_Part(reader).content[0]
    processor.process_nodes(fake_parent, [fp2intrinsic], None)
    assert isinstance(fake_parent[0], Assignment)
    assert isinstance(fake_parent[0].rhs, UnaryOperation)
    assert isinstance(fake_parent[0].rhs.children[0], Reference)
#    assert isinstance(fake_parent[0].rhs.children[1], Literal)
