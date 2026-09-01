# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
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
    # Having symbols that are not in the symbol_table doesn't make it fail
    directive.explicitly_private_symbols.add(Symbol("non_existant"))
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
