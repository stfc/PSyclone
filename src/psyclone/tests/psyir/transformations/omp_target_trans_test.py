# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2018-2025, Science and Technology Facilities Council.
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
# ----------------------------------------------------------------------------
# Author: S. Siso, STFC Daresbury Lab

''' Tests for the OMPTargetTrans transformation. '''

import pytest
from psyclone.psyir.nodes import Loop, Schedule, OMPTargetDirective, Routine
from psyclone.psyir.transformations import OMPTargetTrans, TransformationError


@pytest.fixture(name="sample_psyir")
def sample_psyir_fixture(fortran_reader):
    ''' Snippet of code converted to PSyIR to use during the tests. '''
    code = '''
    subroutine my_subroutine()
        integer, dimension(10, 10) :: A
        integer :: i
        integer :: j
        do i = 1, 10
            do j = 1, 10
                A(i, j) = 0
            end do
        end do
        do i = 1, 10
            do j = 1, 10
                A(i, j) = 0
            end do
        end do
    end subroutine
    '''
    return fortran_reader.psyir_from_source(code)


def test_omptargettrans(sample_psyir):
    ''' Test OMPTargetTrans works as expected with the different options. '''

    # Insert a OMPTarget just on the first loop
    omptargettrans = OMPTargetTrans()
    tree = sample_psyir.copy()
    loops = tree.walk(Loop, stop_type=Loop)
    omptargettrans.apply(loops[0])
    assert isinstance(loops[0].parent, Schedule)
    assert isinstance(loops[0].parent.parent, OMPTargetDirective)
    assert isinstance(tree.children[0].children[0], OMPTargetDirective)
    assert tree.children[0].children[0] is loops[0].parent.parent
    assert not isinstance(loops[1].parent.parent, OMPTargetDirective)
    assert len(tree.walk(Routine)[0].children) == 2

    # Insert a combined OMPTarget in both loops (providing a list of nodes)
    tree = sample_psyir.copy()
    loops = tree.walk(Loop, stop_type=Loop)
    omptargettrans.apply(tree.children[0].children)
    assert isinstance(loops[0].parent, Schedule)
    assert isinstance(loops[0].parent.parent, OMPTargetDirective)
    assert isinstance(loops[1].parent, Schedule)
    assert isinstance(loops[1].parent.parent, OMPTargetDirective)
    assert len(tree.walk(Routine)[0].children) == 1
    assert loops[0].parent.parent is loops[1].parent.parent

    # Insert a combined OMPTarget in both loops (now providing a Schedule)
    tree = sample_psyir.copy()
    loops = tree.walk(Loop, stop_type=Loop)
    omptargettrans.apply(tree.children[0])
    assert isinstance(loops[0].parent, Schedule)
    assert isinstance(loops[0].parent.parent, OMPTargetDirective)
    assert isinstance(loops[1].parent, Schedule)
    assert isinstance(loops[1].parent.parent, OMPTargetDirective)
    assert len(tree.walk(Routine)[0].children) == 1
    assert loops[0].parent.parent is loops[1].parent.parent


def test_omptargettrans_validate(fortran_reader):
    ''' Test that OMPTargetTrans validation fails if it contains non-allowed
    constructs. '''

    omptargettrans = OMPTargetTrans()

    code = '''
    function myfunc(a)
        integer :: a
        integer :: myfunc
        do i = 1, 1
            myfunc = a
        enddo
    end function
    subroutine my_subroutine()
        integer, dimension(10, 10) :: A
        integer :: i
        integer :: j
        do i = 1, 10
            do j = 1, 10
                A(i, j) = myfunc(3)
            end do
        end do
        do i = 1, 10
            do j = 1, 10
                char = 'a' // 'b'
            end do
        end do
    end subroutine
    '''
    psyir = fortran_reader.psyir_from_source(code)
    loops = psyir.walk(Loop, stop_type=Loop)

    with pytest.raises(TransformationError) as err:
        omptargettrans.validate(loops[0])
    assert ("OpenMP Target cannot enclose a region that has a function "
            "return value symbol, but found one in 'myfunc'."
            in str(err.value))

    with pytest.raises(TransformationError) as err:
        omptargettrans.validate(loops[1])
    assert ("'myfunc' is not available on the accelerator device, and "
            "therefore it cannot be called from within an OMP Target region."
            in str(err.value))

    with pytest.raises(TransformationError) as err:
        omptargettrans.validate(loops[2])
    assert ("Nodes of type 'CodeBlock' cannot be enclosed by a OMPTarget"
            "Trans transformation" in str(err.value))
