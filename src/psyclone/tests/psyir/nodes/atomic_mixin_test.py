# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2020-2025, Science and Technology Facilities Council.
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
# Author: A. B. G. Chalk, STFC Daresbury Lab
# -----------------------------------------------------------------------------

''' This module contains pytest tests for the AtomicDirectiveMixin class. '''

from psyclone.psyir.nodes import AtomicDirectiveMixin


def test_is_valid_atomic_statement(fortran_reader):
    '''Test the is_valid_atomic_statement functionality of the AtomicDirectiveMixin
    class.'''

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
