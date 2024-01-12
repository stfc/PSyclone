# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2021-2024, Science and Technology Facilities Council.
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
# Author S. Siso, STFC Daresbury Lab
# -----------------------------------------------------------------------------

''' Performs py.test tests on CommentableMixin PSyIR nodes. '''

from __future__ import absolute_import
import pytest
from psyclone.psyir.nodes import Return, Routine, Container


def test_statement_comment_properties():
    ''' Check the Statement comment properties inherited from the Mixin'''

    # Use a Return node as an example of statement
    statement = Return()

    # Check the default value
    assert statement.preceding_comment == ""
    assert statement.inline_comment == ""

    # Check the setters with a proper value
    statement.preceding_comment = "My preceding comment"
    statement.inline_comment = "My inline comment"
    assert statement.preceding_comment == "My preceding comment"
    assert statement.inline_comment == "My inline comment"

    # Check the setters with a value of the wrong type
    with pytest.raises(TypeError) as err:
        statement.preceding_comment = 1
    assert "The preceding_comment must be a string but found 'int'." \
        in str(err.value)

    with pytest.raises(TypeError) as err:
        statement.inline_comment = 2
    assert "The inline_comment must be a string but found 'int'." \
        in str(err.value)


def test_other_commentable_nodes():
    ''' Check that other nodes with the CommentableMixin have the
    expected functionality.'''

    routine = Routine("my_routine")
    container = Container("my_container")

    # Check the setters with a proper value
    routine.preceding_comment = "My preceding comment 1"
    routine.inline_comment = "My inline comment 2"
    container.preceding_comment = "My preceding comment 3"
    container.inline_comment = "My inline comment 4"
    assert routine.preceding_comment == "My preceding comment 1"
    assert routine.inline_comment == "My inline comment 2"
    assert container.preceding_comment == "My preceding comment 3"
    assert container.inline_comment == "My inline comment 4"


def test_comment_attributes_copy():
    ''' Check that the comment attributes are copied as expected. '''
    statement = Return()
    statement.preceding_comment = "My preceding comment"
    statement.inline_comment = "My inline comment"

    # Make a copy of the node and replace the comments on the original
    statement2 = statement.copy()
    statement.preceding_comment = "Replace previous comment"
    statement.inline_comment = "Replace previous comment"
    assert statement2.preceding_comment == "My preceding comment"
    assert statement2.inline_comment == "My inline comment"
