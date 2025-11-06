# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017-2025, Science and Technology Facilities Council.
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
# Authors A. B. G. Chalk, STFC Daresbury Lab

'''This module contains the tests for the MaximalParallelRegionTrans.'''

import pytest

from psyclone.psyir.nodes import (
    Assignment,
    IfBlock,
    Routine,
    OMPParallelDirective,
)
from psyclone.psyir.transformations import (
    MaximalParallelRegionTrans,
    TransformationError
)
from psyclone.transformations import OMPParallelTrans


# Dummy class to test MaxParallelRegionTrans' functionality.
class MaxParTrans(MaximalParallelRegionTrans):
    # The apply function will do OMPParallelTrans around allowed regions.
    _parallel_transformation = OMPParallelTrans
    # We're only allowing assignment because its straightforward to test with.
    _allowed_nodes = (Assignment, )


@pytest.mark.parametrize(
    "statement,expected",
    [
        ("i = 1", True),
        ("call a_function()", False),
        ("do i = 1, 100\nj = j + 1\nend do", True),
        ("do i = 1, 100\ncall a_function()\nend do", False),
        ("if (.true.) then\nj=3\nend if", True),
        ("if(.true.) then\nj=3\nelse\nj=3\nend if", True),
        ("if(.true.) then\ncall a_function()\nelse\nj=3\nendif", False),
        ("if(.true.) then\nj=3\nelse\ncall a_function()\nendif", False),
    ]
)
def test_can_be_in_parallel_region(fortran_reader, statement, expected):
    '''Test the _can_be_in_parallel_region function of
    MaxParallelRegionTrans.'''
    code = f"""
    subroutine test
        use some_module
        integer :: i, j
        {statement}
    end subroutine test
    """
    psyir = fortran_reader.psyir_from_source(code)
    routine = psyir.walk(Routine)[0]
    trans = MaxParTrans()
    assert trans._can_be_in_parallel_region(routine.children[0]) == expected


def test_validate(fortran_reader):
    '''Test the validate function of MaxParallelRegionTrans.'''
    code = """
    subroutine test
    integer :: i, j
    i = 1
    j = 1
    k = i + 1
    if(.true.) then
        k = i + j
    end if
    end subroutine test"""
    psyir = fortran_reader.psyir_from_source(code)
    routine = psyir.walk(Routine)[0]
    trans = MaxParTrans()
    # Validate should allow us to give the full children
    trans.validate(routine.children)

    # Validate should not allow non consecutive children
    with pytest.raises(TransformationError) as err:
        trans.validate([routine.children[0], routine.children[2]])
    assert ("Children are not consecutive children of one parent: child "
            "'k = i + 1' has position 2, but previous child had position 0."
            in str(err.value))

    # Validate should not allow children of different parents.
    with pytest.raises(TransformationError) as err:
        trans.validate([routine.children[0],
                        routine.children[3].if_body.children[0]])
    assert ("Error in MaxParTrans transformation: supplied nodes are not "
            "children of the same parent" in str(err.value))


def test_apply(fortran_reader):
    '''Test the apply function of MaxParallelRegionTrans.'''
    code = """
    subroutine test
    use some_module
    integer :: i, j
    i = 1
    j = 1
    call a_function()
    if(.true.) then
        i = 1
    end if
    j = 1
    end subroutine test
    """
    psyir = fortran_reader.psyir_from_source(code)
    routine = psyir.walk(Routine)[0]
    trans = MaxParTrans()
    trans.apply(routine)
    # The result should be two OMPParallelDirectives, one containing
    # i = 1 and j = 1, and another containing the IFBlock and the second j = 1
    dirs = routine.walk(OMPParallelDirective)
    assert len(dirs) == 2

    assert len(dirs[0].dir_body.children) == 2
    assert dirs[0].dir_body.children[0].debug_string() == "i = 1\n"
    assert dirs[0].dir_body.children[1].debug_string() == "j = 1\n"

    assert isinstance(dirs[1].dir_body.children[0], IfBlock)
    assert dirs[1].dir_body.children[1].debug_string() == "j = 1\n"
