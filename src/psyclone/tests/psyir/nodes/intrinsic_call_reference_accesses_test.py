# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2022-2026, Science and Technology Facilities Council.
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

from psyclone.core import AccessType, VariablesAccessMap
from psyclone.psyir.nodes import (
    ArrayReference,
    Literal,
    Reference,
    BinaryOperation,
    Call
)
from psyclone.psyir.nodes.intrinsic_call import (
    IntrinsicCall,
    _reference_accesses_all_reads_with_optional_kind,
    _add_argument_of_access_type,
    _compute_reference_accesses,
)
from psyclone.psyir.symbols import (
    ArrayType,
    DataSymbol,
    INTEGER_TYPE,
    RoutineSymbol
)


def test_add_argument_of_access_type_read(fortran_reader):
    """ Test the _add_argument_of_access_type helper function with a READ."""
    # Test we get expected behaviour for a Reference input.
    symbol = DataSymbol("a", INTEGER_TYPE)
    vam = VariablesAccessMap()
    ref = Reference(symbol)
    _add_argument_of_access_type(ref, vam, AccessType.READ)

    sig, _ = ref.get_signature_and_indices()
    assert len(vam) == 1
    assert len(vam[sig]) == 1
    assert vam[sig][0].access_type == AccessType.READ

    # Test we get expected behaviour for a Literal input.
    vam = VariablesAccessMap()
    lit = Literal("1", INTEGER_TYPE)
    _add_argument_of_access_type(lit, vam, AccessType.READ)
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
    _add_argument_of_access_type(binop, vam, AccessType.READ)
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
    _add_argument_of_access_type(ref, vam, AccessType.READ)
    sig, _ = ref.get_signature_and_indices()
    assert len(vam[sig]) == 1
    assert vam[sig][0].access_type == AccessType.READ
    sig, _ = ref3.get_signature_and_indices()
    # This is the same behaviour as an ArrayReference itself would have.
    assert len(vam[sig]) == 1
    assert vam[sig][0].access_type == AccessType.READ

    code = """subroutine test
    use some_mod
    integer :: a, b, c
    integer, dimension(100) :: d
    integer, parameter :: wp = 8

    a = MAX(a, c + b + some_func(d) + 1_wp)
    end subroutine test"""
    psyir = fortran_reader.psyir_from_source(code)
    intrinsic = psyir.walk(IntrinsicCall)[0]
    vam = VariablesAccessMap()
    _add_argument_of_access_type(intrinsic.arguments[1], vam, AccessType.READ)
    sigs = vam.all_signatures
    assert str(sigs[0]) == "b"
    assert len(vam[sigs[0]]) == 1
    assert vam[sigs[0]][0].access_type == AccessType.READ
    assert str(sigs[1]) == "c"
    assert len(vam[sigs[1]]) == 1
    assert vam[sigs[1]][0].access_type == AccessType.READ
    assert str(sigs[2]) == "d"
    assert len(vam[sigs[2]]) == 1
    assert vam[sigs[2]][0].access_type == AccessType.READWRITE
    assert str(sigs[3]) == "some_func"
    assert len(vam[sigs[3]]) == 1
    assert vam[sigs[3]][0].access_type == AccessType.UNKNOWN
    assert str(sigs[4]) == "wp"
    assert len(vam[sigs[4]]) == 1
    assert vam[sigs[4]][0].access_type == AccessType.CONSTANT


def test_add_argument_of_access_type_write(fortran_reader):
    """ Test the _add_argument_of_access_type helper function with a WRITE."""
    # Test we get expected behaviour for a Reference input.
    symbol = DataSymbol("a", INTEGER_TYPE)
    vam = VariablesAccessMap()
    ref = Reference(symbol)
    _add_argument_of_access_type(ref, vam, AccessType.WRITE)

    sig, _ = ref.get_signature_and_indices()
    assert len(vam) == 1
    assert len(vam[sig]) == 1
    assert vam[sig][0].access_type == AccessType.WRITE

    symbol = DataSymbol("c", ArrayType(INTEGER_TYPE, [2]))
    aref = ArrayReference.create(symbol, [ref])
    vam = VariablesAccessMap()
    _add_argument_of_access_type(aref, vam, AccessType.WRITE)
    sig, _ = aref.get_signature_and_indices()
    assert len(vam[sig]) == 1
    assert vam[sig][0].access_type == AccessType.WRITE
    sig, _ = ref.get_signature_and_indices()
    assert len(vam[sig]) == 1
    assert vam[sig][0].access_type == AccessType.READ

    code = """subroutine test
    use some_mod
    integer :: a, b, c
    integer, dimension(100) :: d
    integer, parameter :: wp = 8

    CALL RANDOM_NUMBER( d(somefunc(b) + 1.0_wp))
    end subroutine test"""
    psyir = fortran_reader.psyir_from_source(code)
    call = psyir.walk(Call)[0]
    # RANDOM_NUMBER doesn't create an IntrinsicCall in the
    # PSyIR tree, so convert the resultant call into the
    # IntrinsicCall we need to test.
    intrinsic = IntrinsicCall.create(
            IntrinsicCall.Intrinsic.RANDOM_NUMBER,
            [arg.copy() for arg in call.arguments]
    )
    vam = VariablesAccessMap()
    _add_argument_of_access_type(
            intrinsic.arguments[0], vam, AccessType.WRITE
    )
    sigs = vam.all_signatures
    assert str(sigs[0]) == "b"
    assert len(vam[sigs[0]]) == 1
    assert vam[sigs[0]][0].access_type == AccessType.READWRITE
    assert str(sigs[1]) == "d"
    assert len(vam[sigs[1]]) == 1
    assert vam[sigs[1]][0].access_type == AccessType.WRITE
    assert str(sigs[2]) == "somefunc"
    assert len(vam[sigs[2]]) == 1
    assert vam[sigs[2]][0].access_type == AccessType.UNKNOWN
    assert str(sigs[3]) == "wp"
    assert len(vam[sigs[3]]) == 1
    assert vam[sigs[3]][0].access_type == AccessType.CONSTANT


def test_add_argument_of_access_type_readwrite():
    """ Test the _add_argument_of_access_type helper function with a
    READWRITE."""
    # Test we get expected behaviour for a Reference input.
    symbol = DataSymbol("a", INTEGER_TYPE)
    vam = VariablesAccessMap()
    ref = Reference(symbol)
    _add_argument_of_access_type(ref, vam, AccessType.READWRITE)

    sig, _ = ref.get_signature_and_indices()
    assert len(vam) == 1
    assert len(vam[sig]) == 1
    assert vam[sig][0].access_type == AccessType.READWRITE

    symbol = DataSymbol("c", ArrayType(INTEGER_TYPE, [2]))
    aref = ArrayReference.create(symbol, [ref])
    vam = VariablesAccessMap()
    _add_argument_of_access_type(aref, vam, AccessType.READWRITE)
    sig, _ = aref.get_signature_and_indices()
    assert len(vam[sig]) == 1
    assert vam[sig][0].access_type == AccessType.READWRITE
    sig, _ = ref.get_signature_and_indices()
    assert len(vam[sig]) == 1
    assert vam[sig][0].access_type == AccessType.READ


def test_add_argument_of_access_type_constant():
    """ Test the _add_argument_of_access_type helper function with a
    CONSTANT."""
    # Test we get expected behaviour for a Reference input.
    symbol = DataSymbol("a", INTEGER_TYPE)
    vam = VariablesAccessMap()
    ref = Reference(symbol)
    _add_argument_of_access_type(ref, vam, AccessType.CONSTANT)

    sig, _ = ref.get_signature_and_indices()
    assert len(vam) == 1
    assert len(vam[sig]) == 1
    assert vam[sig][0].access_type == AccessType.CONSTANT

    # Test we skip for a Literal
    vam = VariablesAccessMap()
    lit = Literal("1", INTEGER_TYPE)
    _add_argument_of_access_type(lit, vam, AccessType.CONSTANT)
    assert len(vam) == 0


def test_add_argument_of_access_type_inquiry():
    """ Test the _add_argument_of_access_type helper function with an
    INQUIRY."""
    symbol = DataSymbol("a", INTEGER_TYPE)
    vam = VariablesAccessMap()
    ref = Reference(symbol)
    _add_argument_of_access_type(ref, vam, AccessType.INQUIRY)

    sig, _ = ref.get_signature_and_indices()
    assert len(vam) == 1
    assert len(vam[sig]) == 1
    assert vam[sig][0].access_type == AccessType.INQUIRY

    # Test we skip for a Literal
    vam = VariablesAccessMap()
    lit = Literal("1", INTEGER_TYPE)
    _add_argument_of_access_type(lit, vam, AccessType.INQUIRY)
    assert len(vam) == 0

    symbol = DataSymbol("c", ArrayType(INTEGER_TYPE, [2]))
    aref = ArrayReference.create(symbol, [ref])
    vam = VariablesAccessMap()
    _add_argument_of_access_type(aref, vam, AccessType.INQUIRY)
    sig, _ = aref.get_signature_and_indices()
    assert len(vam[sig]) == 1
    assert vam[sig][0].access_type == AccessType.INQUIRY
    sig, _ = ref.get_signature_and_indices()
    assert len(vam[sig]) == 1
    assert vam[sig][0].access_type == AccessType.READ


def test_compute_reference_accesses():
    """ Test the _compute_reference_accesses helper function."""
    # Create some References to use to test functionality.
    a_sym = DataSymbol("a", INTEGER_TYPE)
    b_sym = DataSymbol("b", INTEGER_TYPE)
    c_sym = DataSymbol("c", INTEGER_TYPE)
    d_sym = DataSymbol("d", INTEGER_TYPE)
    e_sym = DataSymbol("e", INTEGER_TYPE)
    f_sym = DataSymbol("f", INTEGER_TYPE)
    g_sym = DataSymbol("g", INTEGER_TYPE)
    h_sym = DataSymbol("h", INTEGER_TYPE)
    i_sym = DataSymbol("i", INTEGER_TYPE)
    j_sym = DataSymbol("j", INTEGER_TYPE)
    a_ref = Reference(a_sym)
    b_ref = Reference(b_sym)
    c_ref = Reference(c_sym)
    d_ref = Reference(d_sym)
    e_ref = Reference(e_sym)
    f_ref = Reference(f_sym)
    g_ref = Reference(g_sym)
    h_ref = Reference(h_sym)
    i_ref = Reference(i_sym)
    j_ref = Reference(j_sym)

    # Create some general call to test the function.
    call = Call.create(
            Reference(RoutineSymbol("myname")),
            [a_ref, b_ref, c_ref, d_ref, e_ref,
             ("read", f_ref), ("write", g_ref),
             ("readwrite", h_ref),
             ("constant", i_ref),
             ("inquiry", j_ref),
             ]
    )
    varaccesses = _compute_reference_accesses(
            call,
            read_indices=[0],
            write_indices=[1],
            readwrite_indices=[2],
            constant_indices=[3],
            inquiry_indices=[4],
            read_named_args=["read", "not_present_1"],
            write_named_args=["write", "not_present_2"],
            readwrite_named_args=["readwrite", "not_present_3"],
            constant_named_args=["constant", "not_present_4"],
            inquiry_named_args=["inquiry", "not_present_5"],
    )
    # We should only get the 10 accesses present in the Call.
    assert len(varaccesses) == 10

    sig, _ = a_ref.get_signature_and_indices()
    assert len(varaccesses[sig]) == 1
    assert varaccesses[sig][0].access_type == AccessType.READ
    sig, _ = b_ref.get_signature_and_indices()
    assert len(varaccesses[sig]) == 1
    assert varaccesses[sig][0].access_type == AccessType.WRITE
    sig, _ = c_ref.get_signature_and_indices()
    assert len(varaccesses[sig]) == 1
    assert varaccesses[sig][0].access_type == AccessType.READWRITE
    sig, _ = d_ref.get_signature_and_indices()
    assert len(varaccesses[sig]) == 1
    assert varaccesses[sig][0].access_type == AccessType.CONSTANT
    sig, _ = e_ref.get_signature_and_indices()
    assert len(varaccesses[sig]) == 1
    assert varaccesses[sig][0].access_type == AccessType.INQUIRY
    sig, _ = f_ref.get_signature_and_indices()
    assert len(varaccesses[sig]) == 1
    assert varaccesses[sig][0].access_type == AccessType.READ
    sig, _ = g_ref.get_signature_and_indices()
    assert len(varaccesses[sig]) == 1
    assert varaccesses[sig][0].access_type == AccessType.WRITE
    sig, _ = h_ref.get_signature_and_indices()
    assert len(varaccesses[sig]) == 1
    assert varaccesses[sig][0].access_type == AccessType.READWRITE
    sig, _ = i_ref.get_signature_and_indices()
    assert len(varaccesses[sig]) == 1
    assert varaccesses[sig][0].access_type == AccessType.CONSTANT
    sig, _ = j_ref.get_signature_and_indices()
    assert len(varaccesses[sig]) == 1
    assert varaccesses[sig][0].access_type == AccessType.INQUIRY


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
    # First result should be a READ, the kind should be a CONSTANT
    sig, _ = intrinsics[1].arguments[0].get_signature_and_indices()
    assert refs[sig][0].access_type == AccessType.READ
    sig, _ = intrinsics[1].arguments[1].get_signature_and_indices()
    assert refs[sig][0].access_type == AccessType.CONSTANT
