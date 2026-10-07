# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2020-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

''' This module contains pytest tests for the AtomicDirectiveMixin class. '''

from psyclone.psyir.nodes import AtomicDirectiveMixin


def test_is_valid_atomic_statement(fortran_reader):
    '''Test the is_valid_atomic_statement functionality of the
    AtomicDirectiveMixin class.'''

    code = """subroutine x(a, b)
    use some_mod
    integer :: a
    integer, dimension(:) :: b
    a = a + 1
    a = max(a, b(1))
    a = 3
    b(1) = 3
    call something()
    b(:) = 1
    end subroutine"""
    psyir = fortran_reader.psyir_from_source(code)
    routine = psyir.children[0]

    # a = a + 1 is a valid statement.
    assert AtomicDirectiveMixin.is_valid_atomic_statement(routine.children[0])
    # a = max(a, b(1)) is a valid statement
    assert AtomicDirectiveMixin.is_valid_atomic_statement(routine.children[1])
    # a = 3 is a valid statement (atomic write)
    assert AtomicDirectiveMixin.is_valid_atomic_statement(routine.children[2])
    # b(a) = 1 is a valid statement (atomic write)
    assert AtomicDirectiveMixin.is_valid_atomic_statement(routine.children[3])
    # a call is not a valid statement
    assert (
        not AtomicDirectiveMixin.is_valid_atomic_statement(
            routine.children[4]
        )
    )
    # An array write is not a valid statement
    assert (
        not AtomicDirectiveMixin.is_valid_atomic_statement(
            routine.children[5]
        )
    )

    # Test that a subclass can override the allowed operators or intrinsics.
    class DummyAtomic(AtomicDirectiveMixin):
        # Not allowed operators or Intrinsics.
        _VALID_OPERATORS = ()
        _VALID_INTRINSICS = ()

    assert not DummyAtomic.is_valid_atomic_statement(routine.children[0])
    assert not DummyAtomic.is_valid_atomic_statement(routine.children[1])
