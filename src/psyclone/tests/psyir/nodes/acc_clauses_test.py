# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2023-2025, Science and Technology Facilities Council.
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
# Author: A. R. Porter, STFC Daresbury Lab
# -----------------------------------------------------------------------------

''' This module contains the pytest test battery for the various OpenACC
 Directive Clause nodes.'''

import pytest

from psyclone.errors import GenerationError
from psyclone.psyir.nodes import (
    ACCAsyncQueueClause, ACCCopyClause, ACCCopyInClause, ACCCopyOutClause,
    Literal, Reference, Return)
from psyclone.psyir.symbols import DataSymbol, INTEGER_TYPE, Symbol


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
    assert not clause._validate_child(0, Literal("1", INTEGER_TYPE))
    assert clause._validate_child(0, Reference(DataSymbol("var",
                                                          INTEGER_TYPE)))
