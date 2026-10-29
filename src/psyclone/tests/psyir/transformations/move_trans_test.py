# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------
'''Contains the tests for the MoveTrans transformation.'''

import pytest

from psyclone.psyir.nodes import (
    Assignment,
)
from psyclone.psyir.transformations import (
    MoveTrans,
    TransformationError
)


def test_move_trans_properties():
    '''Test the basic properties of the MoveTrans node.'''
    assert str(MoveTrans()) == "Move a node to a different location"
    assert MoveTrans().name == "MoveTrans"


def test_move_trans_validate(fortran_reader, monkeypatch):
    '''Test the validate method of the MoveTrans.'''

    with pytest.raises(TransformationError) as err:
        MoveTrans().validate("a", None)
    assert ("The node argument to MoveTrans should be a Node but got "
            "'str'." in str(err.value))

    code = """subroutine a
    integer :: i, j, k

        j = 2
        k = j
        j = i + 1
    end subroutine a"""

    psyir = fortran_reader.psyir_from_source(code)
    assign = psyir.walk(Assignment)[1]
    assign2 = psyir.walk(Assignment)[2]
    # Monkeypatch is_valid_location so we can check it easier.
    monkeypatch.setattr(assign2, "is_valid_location", lambda x,
                        position: False)
    with pytest.raises(TransformationError) as err:
        MoveTrans().validate(assign2, assign, position="before")
    assert ("In MoveTrans, data dependencies forbid the move to the new "
            "location" in str(err.value))


def test_move_trans_apply(fortran_reader, fortran_writer):
    '''Test the apply method of the MoveTrans.'''

    code = """subroutine a
    integer :: i, j, k

        i = 1
        j = 2
        k = 3
    end subroutine a"""
    psyir = fortran_reader.psyir_from_source(code)

    assigns = psyir.walk(Assignment)
    j_assign = assigns[1]
    k_assign = assigns[2]

    mtrans = MoveTrans()

    mtrans.apply(j_assign, k_assign, position="after")

    correct = """k = 3
  j = 2"""
    assert correct in fortran_writer(psyir)

    mtrans.apply(j_assign, k_assign, position="before")

    correct = """j = 2
  k = 3"""
    assert correct in fortran_writer(psyir)

    # TODO #2668: Check the options dictionary still works until removal.
    mtrans.apply(j_assign, k_assign, options={"position": "after"})
    correct = """k = 3
  j = 2"""
    assert correct in fortran_writer(psyir)
