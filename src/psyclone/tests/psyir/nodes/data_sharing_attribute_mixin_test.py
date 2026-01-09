# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2026, Science and Technology Facilities Council.
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
# Authors: S. Siso, STFC Daresbury Lab
# -----------------------------------------------------------------------------

''' Test the DataSharingAttributeMixin. '''

from psyclone.psyir.nodes import Loop, Assignment, OMPParallelDoDirective
from psyclone.psyir.symbols import Symbol


def test_explicitly_private_symbols_attibute(fortran_reader):
    ''' Check that the explicitly_private_symbols functionality works '''
    code = '''
    subroutine basic_loop()
      integer, parameter :: jpi=16, jpj=16
      integer :: ji, jj
      real :: a(jpi, jpj), fconst
      do jj = 1, jpj
        do ji = 1, jpi
          a(ji) = b(ji, jj)
        end do
      end do
    end subroutine basic_loop
    '''
    psyir = fortran_reader.psyir_from_source(code)
    loops = psyir.walk(Loop)
    a_ref = psyir.walk(Assignment)[0].lhs
    b_ref = psyir.walk(Assignment)[0].rhs

    # Insert OMPParallelDoDirective before the outer loop
    parent = loops[0].parent
    directive = OMPParallelDoDirective(children=[loops[0].detach()])
    parent.addchild(directive)

    # By default the directive has no explict private symbols
    assert len(directive.explicitly_private_symbols) == 0

    # Add A as explicitly private symbol
    directive.explicitly_private_symbols.add(a_ref.symbol)
    assert len(directive.explicitly_private_symbols) == 1
    assert a_ref.symbol in directive.explicitly_private_symbols
    assert b_ref.symbol not in directive.explicitly_private_symbols

    # Check that the copy method appropriately updates the symbol references
    # (calling the 'replace_symbols_using' with the new symbol table)
    new_psyir = psyir.copy()
    new_directive = new_psyir.walk(OMPParallelDoDirective)[0]
    new_a_ref = new_psyir.walk(Assignment)[0].lhs
    assert new_a_ref.symbol is not a_ref.symbol
    assert a_ref.symbol not in new_directive.explicitly_private_symbols
    assert new_a_ref.symbol in new_directive.explicitly_private_symbols

    # The 'replace_symbols_using' can also be called with a Symbol
    previous_sym = new_a_ref.symbol
    a_sym = Symbol("a")
    new_directive.replace_symbols_using(a_sym)
    assert previous_sym not in new_directive.explicitly_private_symbols
    assert a_sym in new_directive.explicitly_private_symbols
    # Inner references are also updated because the method is recursive
    assert new_a_ref.symbol is a_sym
