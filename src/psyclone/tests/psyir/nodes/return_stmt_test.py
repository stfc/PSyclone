# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2019-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

''' Performs py.test tests on the Return PSyIR node. '''

import pytest
from psyclone.psyir.nodes import Return
from psyclone.errors import GenerationError
from psyclone.utils import colored


def test_return_node_str():
    ''' Check the node_str method of the Return class.'''
    return_stmt = Return()
    coloredtext = colored("Return", Return._colour)
    assert coloredtext+"[]" in return_stmt.node_str()


def test_return_can_be_printed():
    '''Test that a Return instance can always be printed (i.e. is
    initialised fully)'''
    return_stmt = Return()
    assert "Return[]" in str(return_stmt)


def test_return_children_validation():
    '''Test that children added to Return are validated. A Return node does
    not accept any children.

    '''
    return_stmt = Return()
    return_stmt1 = Return()
    with pytest.raises(GenerationError) as excinfo:
        return_stmt.addchild(return_stmt1)
    assert ("Item 'Return' can't be child 0 of 'Return'. Return is a"
            " LeafNode and doesn't accept children.") in str(excinfo.value)
