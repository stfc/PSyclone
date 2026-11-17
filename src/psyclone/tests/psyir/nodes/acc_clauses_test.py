# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2023-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

''' This module contains the pytest test battery for the various OpenACC
 Directive Clause nodes.'''

import pytest

from psyclone.errors import GenerationError
from psyclone.psyir.nodes import (
    ACCAsyncQueueClause, ACCCopyClause, ACCCopyInClause, ACCCopyOutClause,
    Literal, Reference, Return)
from psyclone.psyir.symbols import DataSymbol, ScalarType, Symbol


def test_acc_async_queue_clause():
    '''
    Tests for the ACCAsyncQueueClause class.
    '''
    clause = ACCAsyncQueueClause()
    # By default, there is no queue specified by the clause.
    assert clause.queue is None
    # The clause is only permitted to have a single child which must be
    # a DataNode.
    with pytest.raises(GenerationError) as err:
        clause.addchild(Return())
    assert ("Item 'Return' can't be child 0 of 'ACCAsyncQueueClause'"
            in str(err.value))
    sym = Symbol("some_value")
    clause.addchild(Reference(sym))
    with pytest.raises(GenerationError) as err:
        clause.addchild(Reference(sym))
    assert ("Item 'Reference' can't be child 1 of 'ACCAsyncQueueClause'"
            in str(err.value))
    # Check that the `queue` property now returns the expected value.
    assert clause.queue.symbol is sym


@pytest.mark.parametrize("cls, string", [(ACCCopyClause, "copy"),
                                         (ACCCopyInClause, "copyin"),
                                         (ACCCopyOutClause, "copyout")])
def test_acccopyclause(cls, string):
    '''
    Test the various ACCCopyXXXX clause nodes.
    '''
    clause = cls()
    assert clause._children_valid_format == "Reference"
    assert clause.clause_string == string
    # The only permitted child type is Reference.
    assert not clause._validate_child(
        0, Literal("1", ScalarType.integer_type()))
    assert clause._validate_child(
        0, Reference(DataSymbol("var", ScalarType.integer_type())))
