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

''' Module containing pytest tests for the handling of the WHERE
construct in the PSyIR. '''

from __future__ import absolute_import

from fparser.common.readfortran import FortranStringReader
from psyclone.psyGen import Fparser2ASTProcessor, KernelSchedule


def test_where(parser):
    ''' Basic test that a WHERE construct is correctly translated into
    a canonical form in the PSyIR. '''
    from fparser.two.Fortran2003 import Where_Construct
    from psyclone.psyGen import Loop
    fake_parent = KernelSchedule("dummy_schedule")
    processor = Fparser2ASTProcessor()
    reader = FortranStringReader("WHERE (ptsu(:, :, :) /= 0._wp)\n"
                                 "  z1_st(:, :, :) = 1._wp / ptsu(:, :, :)\n"
                                 "ELSEWHERE\n"
                                 "  z1_st(:, :, :) = 0._wp\n"
                                 "END WHERE\n")
    fparser2spec = Where_Construct(reader)
    processor.process_nodes(fake_parent, [fparser2spec], None)
    assert len(fake_parent.children) == 1
    assert isinstance(fake_parent.children[0], Loop)
