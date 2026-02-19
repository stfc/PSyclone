# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2026, Science and Technology Facilities Council.
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

'''This module contains the tests for the MaximalRegionTrans.'''

import pytest

from psyclone.psyGen import Transformation
from psyclone.psyir.nodes import (
    Assignment,
    IfBlock,
    Routine,
    OMPParallelDirective,
)
from psyclone.psyir.transformations import (
    MaximalRegionTrans,
    TransformationError,
    OMPParallelTrans
)


# Dummy class to test MaxParallelRegionTrans' functionality.
class MaxParTrans(MaximalRegionTrans):
    # The apply function will do OMPParallelTrans around allowed regions.
    _transformation = OMPParallelTrans
    # We're only allowing assignment because its straightforward to test with.
    _allowed_contiguous_statements = (Assignment, )
    # Should parallelise any found region that contains an assignment.
    _required_nodes = (Assignment, )


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
def test_can_be_in_region(fortran_reader, statement, expected):
    '''Test the _can_be_in_region function of
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
    assert trans._can_be_in_region(routine.children[0]) == expected


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
    mtrans = MaxParTrans()
    mtrans.apply(routine)
    # The result should be two OMPParallelDirectives, one containing
    # i = 1 and j = 1, and another containing the IFBlock and the second j = 1
    dirs = routine.walk(OMPParallelDirective)
    assert len(dirs) == 2

    assert len(dirs[0].dir_body.children) == 2
    assert dirs[0].dir_body.children[0].debug_string() == "i = 1\n"
    assert dirs[0].dir_body.children[1].debug_string() == "j = 1\n"

    assert isinstance(dirs[1].dir_body.children[0], IfBlock)
    assert dirs[1].dir_body.children[1].debug_string() == "j = 1\n"

    code = """subroutine x
    integer :: i, j, k, l

    i = 1
    j = 2
    k = 3
    l = 4
    end subroutine x"""
    psyir = fortran_reader.psyir_from_source(code)
    assigns = psyir.walk(Assignment)
    mtrans.apply(assigns)
    assert len(psyir.walk(OMPParallelDirective)) == 1
    pdir = psyir.walk(OMPParallelDirective)[0]
    # All the assignments should be in the parallel directive.
    for assign in assigns:
        assert assign.parent.parent is pdir

    code = """subroutine x
    integer :: i, j, k, l

    i = 1
    do j = 2, 3
        k = 1
    end do
    if (j == 2) then
      k = 4
    end if
    do while(j < 3)
        j = j + 1
    end do
    i = 4
    end subroutine x
    """
    psyir = fortran_reader.psyir_from_source(code)
    nodes = psyir.walk(Routine)[0].children[:]
    mtrans.apply(nodes)
    assert len(psyir.walk(OMPParallelDirective)) == 1
    pdir = psyir.walk(OMPParallelDirective)[0]
    # All of the blocks here should be in the same ParallelDirective
    for node in nodes:
        assert node.parent.parent is pdir

    code = """subroutine x
    use some_mod
    integer :: i

    i = 1
    call something()
    i = 2
    end subroutine x"""
    psyir = fortran_reader.psyir_from_source(code)
    nodes = psyir.walk(Routine)[0].children[:]
    mtrans.apply(nodes)
    pdirs = psyir.walk(OMPParallelDirective)
    assert len(pdirs) == 2
    # All of the blocks here should be in the same ParallelDirective
    assert nodes[0].parent.parent is pdirs[0]
    assert not nodes[1].ancestor(OMPParallelDirective)
    assert nodes[2].parent.parent is pdirs[1]

    code = """subroutine x
    use some_mod
    integer :: i, j

    if(i == 1) then
        call something()
        i = 2
    else
        i = 3
    end if

    do i = 1,5
       call something()
       j = 2
    end do

    do while(j == 3)
      call something()
      j = j + 2
    end do
    end subroutine x"""
    # Each of the nodes should contain OMPParallels inside them and there
    # should be no top level OMPParallelDirective
    psyir = fortran_reader.psyir_from_source(code)
    nodes = psyir.walk(Routine)[0].children[:]
    mtrans.apply(nodes)
    assert len(psyir.walk(OMPParallelDirective)) == 4
    assert len(nodes[0].walk(OMPParallelDirective)) == 2
    assert len(nodes[0].if_body.children) == 2
    assert isinstance(nodes[0].if_body.children[1], OMPParallelDirective)
    assert isinstance(nodes[0].else_body.children[0], OMPParallelDirective)

    # Dummy class to test failing validation.
    class Faketrans(Transformation):
        '''Dummy transformation to test failing validation.'''
        def __init__(self, *args, **kwargs):
            super().__init__(*args, **kwargs)
            self._validate_count = 0

        def validate(self, node, **kwargs):
            if self._validate_count < 1:
                self._validate_count = self._validate_count + 1
                return
            raise TransformationError("")

        def apply(self, node, **kwargs):
            self._validate_count = 0
            OMPParallelTrans().apply(node, **kwargs)

    class OneParTrans(MaximalRegionTrans):
        '''Dummy MaximalRegionTrans that uses our FakeTrans'''
        _transformation = Faketrans
        _allowed_contiguous_statements = (Assignment, )
        _required_nodes = (Assignment, )

    code = """subroutine x
    use some_mod
    integer :: i, j

    if(i == 1) then
        call something()
        i = 2
    else
        i = 3
    end if

    do i = 1,5
       call something()
       j = 2
    end do

    do while(j == 3)
      call something()
      j = j + 2
    end do
    end subroutine x"""
    # Each of the nodes should contain OMPParallels inside them and there
    # should be no top level OMPParallelDirective
    psyir = fortran_reader.psyir_from_source(code)
    nodes = psyir.walk(Routine)[0].children[:]
    mtrans = OneParTrans()
    mtrans.apply(nodes)
    # Validate fails on all but the first try so we only get one resulting
    # OMPParallelDirective
    assert len(psyir.walk(OMPParallelDirective)) == 1


def test_validation_failure_during_compute_transformable_sections(
    fortran_reader
):
    '''Test that if validation fails during compute transformable section
    we still get a section containing the nodes that did pass validation.'''

    # Create a transformation which fails validation if the lhs symbol's name
    # for the assignment isn't a
    # Dummy class to test failing validation.
    class Faketrans(Transformation):
        '''Dummy transformation to test failing validation.'''
        def validate(self, node_list: list[Assignment], **kwargs):
            for node in node_list:
                if node.lhs.symbol.name != "a":
                    raise TransformationError("Isn't a")

        def apply(self, node: Assignment, **kwargs):
            OMPParallelTrans().apply(node, **kwargs)

    class OneParTrans(MaximalRegionTrans):
        '''Dummy MaximalRegionTrans that uses our FakeTrans'''
        _transformation = Faketrans
        _allowed_contiguous_statements = (Assignment, )
        _required_nodes = (Assignment, )

    code = """subroutine test
    integer :: a
    integer :: b

    a = 1
    b = 2
    a = 3
    end subroutine test"""
    psyir = fortran_reader.psyir_from_source(code)
    routine = psyir.walk(Routine)[0]
    nodes = routine.children[:]
    mtrans = OneParTrans()
    mtrans.apply(nodes)
    assert len(psyir.walk(OMPParallelDirective)) == 2
    assert isinstance(routine.children[0], OMPParallelDirective)
    assert isinstance(routine.children[1], Assignment)
    assert isinstance(routine.children[2], OMPParallelDirective)
