# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2022-2025, Science and Technology Facilities Council.
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
# Author: A. B. G. Chalk - STFC Darebury Lab
# -----------------------------------------------------------------------------

"""
This module contains pytest tests for the reference_accesses for the
IntrinsicCall class and related functions.

TODO #2341 - tests need to be added for all of the supported intrinsics.

"""

import pytest

from psyclone.core import AccessType
from psyclone.psyir.nodes import (
#    ArrayReference,
#    Literal,
    Reference,
#    Schedule,
    Assignment,
)
from psyclone.psyir.nodes.intrinsic_call import (
    IntrinsicCall,
    _convert_argument_to_type_info,
    _reference_accesses_all_reads_with_optional_kind,
)
from psyclone.psyir.symbols import (
#    ArrayType,
    DataSymbol,
    INTEGER_TYPE,
#    IntrinsicSymbol,
#    REAL_TYPE,
#    BOOLEAN_TYPE,
#    CHARACTER_TYPE,
#    ScalarType,
#    UnresolvedType,
#    NoType
)


def test_convert_argument_to_type_info():
    """Test the _convert_argument_to_type_info helper function."""
    # Test that if we supply a Read-only Reference it results in a TYPE_INFO.
    symbol = DataSymbol("a", INTEGER_TYPE)
    ref = Reference(symbol)
    accesses = ref.reference_accesses()
    sig, _ = ref.get_signature_and_indices()
    assert accesses[sig].is_read_only
    _convert_argument_to_type_info(ref, accesses)
    assert accesses[sig][0].access_type == AccessType.TYPE_INFO

    # Test if we supply a mixed read/write reference we don't get a TYPE_INFO.
    assign = Assignment.create(Reference(symbol), Reference(symbol))
    accesses = assign.reference_accesses()
    _convert_argument_to_type_info(assign.lhs, accesses)
    sig, _ = assign.lhs.get_signature_and_indices()
    for access in accesses[sig]:
        assert access.access_type != AccessType.TYPE_INFO


def test_reference_accesses_all_reads_with_optional_kind(fortran_reader):
    """Test the _reference_accesses_all_reads_with_optional_kind helper
    function."""
    code = """subroutine test
    use external_mod, only: wp
    integer :: i, j
    j = INT(i)
    j = INT(i, kind=wp)
    end subroutine test"""
    psyir = fortran_reader.psyir_from_source(code)
    intrinsics = psyir.walk(IntrinsicCall)

    refs = _reference_accesses_all_reads_with_optional_kind(intrinsics[0])
    sig, _ = intrinsics[0].arguments[0].get_signature_and_indices()
    # All results should be reads.
    for ref in refs[sig]:
        assert ref.access_type == AccessType.READ

    refs = _reference_accesses_all_reads_with_optional_kind(intrinsics[1])
    # First result should be a READ, the kind should be a TYPE_INFO
    sig, _ = intrinsics[1].arguments[0].get_signature_and_indices()
    assert refs[sig][0].access_type == AccessType.READ
    sig, _ = intrinsics[1].arguments[1].get_signature_and_indices()
    assert refs[sig][0].access_type == AccessType.TYPE_INFO
