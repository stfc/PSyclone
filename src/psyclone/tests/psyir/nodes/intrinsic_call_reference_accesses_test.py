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

# import pytest

from psyclone.core import AccessType, VariablesAccessMap
from psyclone.psyir.nodes import (
    ArrayReference,
    Literal,
    Reference,
    Assignment,
    BinaryOperation
)
from psyclone.psyir.nodes.intrinsic_call import (
    IntrinsicCall,
    _convert_argument_to_type_info,
    _reference_accesses_all_reads_with_optional_kind,
    _add_read_argument,
    _add_write_argument,
    _add_readwrite_argument,
    _add_typeinfo_argument,
    _add_inquiry_argument,
)
from psyclone.psyir.symbols import (
    ArrayType,
    DataSymbol,
    INTEGER_TYPE,
    # IntrinsicSymbol,
    # REAL_TYPE,
    # BOOLEAN_TYPE,
    # CHARACTER_TYPE,
    # ScalarType,
    # UnresolvedType,
    # NoType
)


def test_add_read_argument():
    """ Test the _add_read_argument helper function."""
    # Test we get expected behaviour for a Reference input.
    symbol = DataSymbol("a", INTEGER_TYPE)
    vam = VariablesAccessMap()
    ref = Reference(symbol)
    _add_read_argument(ref, vam)

    sig, _ = ref.get_signature_and_indices()
    assert len(vam) == 1
    assert len(vam[sig]) == 1
    assert vam[sig][0].access_type == AccessType.READ

    # Test we get expected behaviour for a Literal input.
    vam = VariablesAccessMap()
    lit = Literal("1", INTEGER_TYPE)
    _add_read_argument(lit, vam)
    assert len(vam) == 0

    # Test we get expected behaviour for a Binop with 2 References.
    symbol2 = DataSymbol("b", INTEGER_TYPE)
    ref1 = Reference(symbol)
    ref2 = Reference(symbol2)
    binop = BinaryOperation.create(
        BinaryOperation.Operator.ADD,
        ref1, ref2
    )
    vam = VariablesAccessMap()
    _add_read_argument(binop, vam)
    assert len(vam) == 2
    sig, _ = ref1.get_signature_and_indices()
    assert len(vam[sig]) == 1
    assert vam[sig][0].access_type == AccessType.READ
    sig, _ = ref2.get_signature_and_indices()
    assert len(vam[sig]) == 1
    assert vam[sig][0].access_type == AccessType.READ

    # Test we get expected behaviour for an ArrayReference with Reference
    # index
    symbol = DataSymbol("c", ArrayType(INTEGER_TYPE, [2]))
    ref3 = Reference(symbol2)
    ref = ArrayReference.create(symbol, [ref3])
    vam = VariablesAccessMap()
    _add_read_argument(ref, vam)
    sig, _ = ref.get_signature_and_indices()
    assert len(vam[sig]) == 1
    assert vam[sig][0].access_type == AccessType.READ
    sig, _ = ref3.get_signature_and_indices()
    # This is the same behaviour as an ArrayReference itself would have.
    assert len(vam[sig]) == 1
    assert vam[sig][0].access_type == AccessType.READ

def test_add_write_argument():
    """ Test the _add_write_argument helper function."""
    # Test we get expected behaviour for a Reference input.
    symbol = DataSymbol("a", INTEGER_TYPE)
    vam = VariablesAccessMap()
    ref = Reference(symbol)
    _add_write_argument(ref, vam)

    sig, _ = ref.get_signature_and_indices()
    assert len(vam) == 1
    assert len(vam[sig]) == 1
    assert vam[sig][0].access_type == AccessType.WRITE

    symbol = DataSymbol("c", ArrayType(INTEGER_TYPE, [2]))
    aref = ArrayReference.create(symbol, [ref])
    vam = VariablesAccessMap()
    _add_write_argument(aref, vam)
    sig, _ = aref.get_signature_and_indices()
    assert len(vam[sig]) == 1
    assert vam[sig][0].access_type == AccessType.WRITE
    sig, _ = ref.get_signature_and_indices()
    assert len(vam[sig]) == 1
    assert vam[sig][0].access_type == AccessType.READ


def test_add_readwrite_argument():
    """ Test the _add_readwrite_argument helper function."""
    # Test we get expected behaviour for a Reference input.
    symbol = DataSymbol("a", INTEGER_TYPE)
    vam = VariablesAccessMap()
    ref = Reference(symbol)
    _add_readwrite_argument(ref, vam)

    sig, _ = ref.get_signature_and_indices()
    assert len(vam) == 1
    assert len(vam[sig]) == 1
    assert vam[sig][0].access_type == AccessType.READWRITE

    symbol = DataSymbol("c", ArrayType(INTEGER_TYPE, [2]))
    aref = ArrayReference.create(symbol, [ref])
    vam = VariablesAccessMap()
    _add_readwrite_argument(aref, vam)
    sig, _ = aref.get_signature_and_indices()
    assert len(vam[sig]) == 1
    assert vam[sig][0].access_type == AccessType.READWRITE
    sig, _ = ref.get_signature_and_indices()
    assert len(vam[sig]) == 1
    assert vam[sig][0].access_type == AccessType.READ


def test_add_typeinfo_argument():
    """ Test the _add_typeinfo_argument helper function."""
    # Test we get expected behaviour for a Reference input.
    symbol = DataSymbol("a", INTEGER_TYPE)
    vam = VariablesAccessMap()
    ref = Reference(symbol)
    _add_typeinfo_argument(ref, vam)

    sig, _ = ref.get_signature_and_indices()
    assert len(vam) == 1
    assert len(vam[sig]) == 1
    assert vam[sig][0].access_type == AccessType.TYPE_INFO

    # Test we skip for a Literal
    vam = VariablesAccessMap()
    lit = Literal("1", INTEGER_TYPE)
    _add_typeinfo_argument(lit, vam)
    assert len(vam) == 0


def test_add_inquiry_argument():
    """ Test the _add_inquiry_argument helper function."""
    symbol = DataSymbol("a", INTEGER_TYPE)
    vam = VariablesAccessMap()
    ref = Reference(symbol)
    _add_inquiry_argument(ref, vam)

    sig, _ = ref.get_signature_and_indices()
    assert len(vam) == 1
    assert len(vam[sig]) == 1
    assert vam[sig][0].access_type == AccessType.INQUIRY
    

# FIXME Test _compute_reference_accesses
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
