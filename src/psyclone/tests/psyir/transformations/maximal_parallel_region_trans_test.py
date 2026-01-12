# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2025-2026, Science and Technology Facilities Council.
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
    Assignment, IfBlock, Call, Loop, OMPParallelDirective, Routine
)
from psyclone.psyir.transformations.maximal_parallel_region_trans import (
    MaximalParallelRegionTrans
)
from psyclone.psyir.transformations.transformation_error import (
    TransformationError
)
from psyclone.transformations import OMPParallelTrans


class DummyMaxTrans(MaximalParallelRegionTrans):
    '''Test class to test the functionality of the MaximalParallelRegionTrans
    '''
    _parallel_transformation = OMPParallelTrans
    _allowed_nodes = (Assignment, IfBlock)
    _required_nodes = (Assignment)


def test_can_be_in_parallel_region(fortran_reader):
    '''Test the can_be_in_parallel_region function of the
    MaximalParallelRegionTrans.'''
    code = """subroutine x
    use some_mod
    integer :: i, j, k, l, m

    i = 2
    if(k == 3) then
        call something()
    end if
    call something()
    do j = k, l
        i = 3
    end do
    do j = k, l
        call something()
    end do
    end subroutine x"""
    psyir = fortran_reader.psyir_from_source(code)
    mtrans = DummyMaxTrans()
    # First case - Assignment is in _allowed_nodes so should be True
    assert mtrans._can_be_in_parallel_region(psyir.walk(Assignment)[0])
    # Second case - IfBlock is in _allowed_nodes so should be True
    assert mtrans._can_be_in_parallel_region(psyir.walk(IfBlock)[0])
    # Third case - Call is not in _allowed_nodes so should be False
    assert not mtrans._can_be_in_parallel_region(psyir.walk(Call)[0])
    # Fourth case - Loop containing only nodes in _allowed_nodes so should be
    # true.
    assert mtrans._can_be_in_parallel_region(psyir.walk(Loop)[0])
    # Fifth case - Loop containing a node not in _allowed_nodes so should be
    # False.
    assert not mtrans._can_be_in_parallel_region(psyir.walk(Loop)[1])


def test_validate(fortran_reader):
    '''Test the validate function of the MaximalParallelRegionTrans.'''
    code = """subroutine x
    integer :: i, j, k, l, m
    i = 1
    if(i == 3) then
        i = 2
    end if
    i = 3
    end subroutine x"""
    psyir = fortran_reader.psyir_from_source(code)
    assigns = psyir.walk(Assignment)
    mtrans = DummyMaxTrans()

    with pytest.raises(TransformationError) as err:
        mtrans.validate([assigns[0], assigns[1]])
    assert ("Error in DummyMaxTrans transformation: supplied nodes are not "
            "children of the same parent." in str(err.value))

    with pytest.raises(TransformationError) as err:
        mtrans.validate([assigns[0], assigns[2]])
    assert ("Children are not consecutive children of one parent: "
            "child 'i = 3' has position 2, but previous child had position 0."
            in str(err.value))


class DummyMaxTrans2(MaximalParallelRegionTrans):
    '''Test class to test the functionality of the MaximalParallelRegionTrans
    apply method
    '''
    _parallel_transformation = OMPParallelTrans
    _allowed_nodes = (Assignment)
    _required_nodes = (Assignment)


def test_apply(fortran_reader):
    '''Test the apply function of the MaximalParallelRegionTrans.'''
    mtrans = DummyMaxTrans2()

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
