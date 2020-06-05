# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2020, Science and Technology Facilities Council.
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
# Author: R. W. Ford STFC Daresbury Lab

'''Module containing pytest tests for the nemo.py file'''

from __future__ import absolute_import
import pytest
from fparser.common.readfortran import FortranStringReader
from psyclone.nemo import NemoFparser2Reader
from psyclone.errors import InternalError


def test_loop_var_exception(parser):
    '''Checks that the expected exception is raised if a loop variable is
    not declared and there is no unqualified use statement.

    '''
    code = ('''
      subroutine test()
        do i=1,10
        end do
      end subroutine test
    ''')
    reader = FortranStringReader(code)
    fparser_tree = parser(reader)
    fparser2psyir = NemoFparser2Reader()
    with pytest.raises(InternalError) as excinfo:
        _ = fparser2psyir.generate_schedule("test", fparser_tree)
    assert (
        "Loop-variable name 'i' is not declared and there are no unqualified "
        "use statements. This is currently unsupported."
        in str(excinfo.value))
