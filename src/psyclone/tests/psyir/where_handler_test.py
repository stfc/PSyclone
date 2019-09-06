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

import pytest
from fparser.common.readfortran import FortranStringReader
from fparser.two.Fortran2003 import Where_Construct
from psyclone.psyGen import Fparser2ASTProcessor, KernelSchedule


def test_where_broken_tree(parser):
    ''' Check that we raise the expected exceptions if the fparser2 parse
    tree does not have the correct structure. '''
    from psyclone.psyGen import InternalError
    
    fake_parent = KernelSchedule("dummy_schedule")
    processor = Fparser2ASTProcessor()
    reader = FortranStringReader("WHERE (ptsu(:, :, :) /= 0._wp)\n"
                                 "  z1_st(:, :, :) = 1._wp / ptsu(:, :, :)\n"
                                 "ELSEWHERE\n"
                                 "  z1_st(:, :, :) = 0._wp\n"
                                 "END WHERE\n")
    fparser2spec = Where_Construct(reader)
    # Break the parse tree by removing the end-where statement
    del fparser2spec.content[-1]
    with pytest.raises(InternalError) as err:
        processor.process_nodes(fake_parent, [fparser2spec], None)
    assert "Failed to find closing end where statement" in str(err.value)
    # Now remove the opening where statement
    del fparser2spec.content[0]
    with pytest.raises(InternalError) as err:
        processor.process_nodes(fake_parent, [fparser2spec], None)
    assert "Failed to find opening where construct " in str(err.value)


def test_missing_array_notation(parser):
    ''' Check that we raise the expected exception if array notation is
    not used in the logical expression. '''
    fake_parent = KernelSchedule("dummy_schedule")
    processor = Fparser2ASTProcessor()
    reader = FortranStringReader("WHERE (ptsu /= 0._wp)\n"
                                 "  z1_st(:, :, :) = 1._wp / ptsu(:, :, :)\n"
                                 "ELSEWHERE\n"
                                 "  z1_st(:, :, :) = 0._wp\n"
                                 "END WHERE\n")
    fparser2spec = Where_Construct(reader)
    with pytest.raises(NotImplementedError) as err:
        processor.process_nodes(fake_parent, [fparser2spec], None)
    assert ("Explicit array notation must be used in the logical-array-expr "
            "of a WHERE construct but found: " in str(err.value))


def test_where(parser):
    ''' Basic test that a WHERE construct is correctly translated into
    a canonical form in the PSyIR. '''
    from psyclone.psyGen import Loop, IfBlock
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
    # Check that we have a triply-nested loop
    loop = fake_parent.children[0]
    assert isinstance(loop, Loop)
    assert isinstance(loop.loop_body[0], Loop)
    assert isinstance(loop.loop_body[0].loop_body[0], Loop)
    # Check that we have an IF block within the innermost loop
    assert isinstance(loop.loop_body[0].loop_body[0].loop_body[0], IfBlock)
