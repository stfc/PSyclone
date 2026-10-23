# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------
'''This module contains tests to check that the DefinitionUseChains return
the same result for multiple inputs vs those inputs individually.'''

import pytest
from psyclone.psyir.nodes import (
    Assignment,
    Reference
)
from psyclone.psyir.tools.definition_use_chains import DefinitionUseChain


@pytest.mark.parametrize("code", [
  """integer :: a, b
  a = b
  b = 2 + a
  a = 2
    """,
  """integer :: a, b, i
  do i = 1, 100
  a = b
  b = 3
  end do
    """,
  """integer :: a, b
    logical :: x
    a = b
    if (x) then
        a = b + a
    else
        b = a * b
    end if
    a = 1
    b = 1
    """,
  """integer :: a, b
    a = b
    b = 2
    if (b > 1) then
       a = 1
    end if
    a = 2 * b""",
  """integer :: a, b
    a = b + a
    b = 2
    if (b > 1) then
       a = 1
    end if
    a = 2 * b""",
    ])
def test_duc_forward_equivalence(code, fortran_reader):
    '''Test the DUCs give the same results for multiple inputs as each of
    the inputs individually.
    '''
    code = f"subroutine test\n{code}\nend subroutine test"
    psyir = fortran_reader.psyir_from_source(code)

    assign = psyir.walk(Assignment)[0]

    all_refs = assign.walk(Reference)
    lhs_ref = all_refs[0]  # First Reference is lhs a
    rhs_ref = all_refs[1]  # Second Reference is always rhs b
    assert lhs_ref.symbol.name == "a"
    assert rhs_ref.symbol.name == "b"
    duc1 = DefinitionUseChain(all_refs)
    duc2 = DefinitionUseChain(lhs_ref)
    duc3 = DefinitionUseChain(rhs_ref)
    res1 = duc1.find_forward_accesses()
    res2 = duc2.find_forward_accesses()
    res3 = duc3.find_forward_accesses()

    lhs_sig = lhs_ref.get_signature_and_indices()[0]
    rhs_sig = rhs_ref.get_signature_and_indices()[0]

    assert len(res2[lhs_sig]) == len(res1[lhs_sig])
    for i, result in enumerate(res2[lhs_sig]):
        assert result is res1[lhs_sig][i]
    assert len(res3[rhs_sig]) == len(res1[rhs_sig])
    for i, result in enumerate(res3[rhs_sig]):
        assert result is res1[rhs_sig][i]


@pytest.mark.parametrize("code", [
  """integer :: a, b
  a = 2
  b = 2 + a
  a = b
    """,
  """integer :: a, b, i
  do i = 1, 100
  b = 3
  a = b
  end do
    """,
  """integer :: a, b
    logical :: x
    a = 1
    b = 1
    if (x) then
        a = b + a
    else
        b = a * b
    end if
    a = b
    """,
  """integer :: a, b
    a = 2 * b
    if (b > 1) then
       a = 1
    end if
    b = 2
    a = b""",
  """integer :: a, b
    a = 2 * b
    if (b > 1) then
       a = 1
    end if
    b = 2
    a = b + a""",
    ])
def test_duc_backward_equivalence(code, fortran_reader):
    '''Test the DUCs give the same results for multiple inputs as each
    of the inputs individually.
    '''
    code = f"subroutine test\n{code}\nend subroutine test"
    psyir = fortran_reader.psyir_from_source(code)

    assign = psyir.walk(Assignment)[-1]

    all_refs = assign.walk(Reference)
    lhs_ref = all_refs[0]  # First Reference is lhs a
    rhs_ref = all_refs[1]  # Second Reference is always rhs b
    assert lhs_ref.symbol.name == "a"
    assert rhs_ref.symbol.name == "b"
    duc1 = DefinitionUseChain(all_refs)
    duc2 = DefinitionUseChain(lhs_ref)
    duc3 = DefinitionUseChain(rhs_ref)
    res1 = duc1.find_backward_accesses()
    res2 = duc2.find_backward_accesses()
    res3 = duc3.find_backward_accesses()

    lhs_sig = lhs_ref.get_signature_and_indices()[0]
    rhs_sig = rhs_ref.get_signature_and_indices()[0]
    assert len(res2[lhs_sig]) == len(res1[lhs_sig])
    for i, result in enumerate(res2[lhs_sig]):
        assert result is res1[lhs_sig][i]
    assert len(res3[rhs_sig]) == len(res1[rhs_sig])
    for i, result in enumerate(res3[rhs_sig]):
        assert result is res1[rhs_sig][i]
