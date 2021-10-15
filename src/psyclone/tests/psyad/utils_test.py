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
# Authors: R. W. Ford and A. R. Porter, STFC Daresbury Lab

'''A module to perform pytest tests on the code in the
utils.py file within the psyad directory.

'''
from psyclone.psyad.utils import active, passive


# active and passive functions
def test_active_passive(fortran_reader):
    '''Test that the active function returns True if an active variable
    exists in the node or its descendants and False if not. Also test
    that the passive function returns the opposite results.

    '''
    code = (
        "program test\n"
        "real :: a, b, c\n"
        "a = b\n"
        "end program test\n")
    tl_psyir = fortran_reader.psyir_from_source(code)
    symbol_table = tl_psyir.children[0].symbol_table
    symbol_a = symbol_table.lookup("a")
    symbol_b = symbol_table.lookup("b")
    symbol_c = symbol_table.lookup("c")
    assignment = tl_psyir.children[0][0]

    assert active(assignment, [symbol_a])
    assert not passive(assignment, [symbol_a])
    assert active(assignment, [symbol_b])
    assert not passive(assignment, [symbol_b])
    assert active(assignment, [symbol_a, symbol_b])
    assert not passive(assignment, [symbol_a, symbol_b])
    assert active(assignment, [symbol_a, symbol_b, symbol_c])
    assert not passive(assignment, [symbol_a, symbol_b, symbol_c])

    assert passive(assignment, [])
    assert not active(assignment, [])
    assert passive(assignment, [symbol_c])
    assert not active(assignment, [symbol_c])
