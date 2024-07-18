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
# Authors R. W. Ford, A. R. Porter and S. Siso, STFC Daresbury Lab
#         A. B. G. Chalk, STFC Daresbury Lab
# Modified I. Kavcic, Met Office
# -----------------------------------------------------------------------------

''' Performs py.test tests on the OpenMP PSyIR Directive nodes. '''

import os
import pytest
from psyclone.psyir.nodes import Schedule, \
    Loop, OMPTaskDirective, OMPPrivateClause, OMPFirstprivateClause, \
    OMPSharedClause, OMPDependClause, DynamicOMPTaskDirective, \
    OMPSingleDirective
from psyclone.errors import GenerationError
from psyclone.transformations import OMPSingleTrans, \
    OMPParallelTrans

BASE_PATH = os.path.join(os.path.dirname(os.path.dirname(os.path.dirname(
    os.path.abspath(__file__)))), "test_files", "dynamo0p3")
GOCEAN_BASE_PATH = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                                os.pardir, os.pardir, "test_files",
                                "gocean1p0")


def test_omp_task_directive_validate_global_constraints():
    ''' Test the validate_global_constraints method of the
    OMPTaskDirective'''
    node = OMPTaskDirective()
    with pytest.raises(GenerationError) as excinfo:
        node.validate_global_constraints()
    assert ("OMPTaskDirective must be inside an OMP Single region but could"
            " not find an ancestor node.") in str(excinfo.value)
    parent = OMPSingleDirective(nowait=True)
    parent.children[0].addchild(node)
    with pytest.raises(GenerationError) as excinfo:
        node.validate_global_constraints()
    assert ("OMPTaskDirective found inside an OMP Single region with nowait "
            "attached. This means we can't guarantee correctness with other "
            "potential Single regions so is forbidden with PSyclone."
            in str(excinfo.value))


def test_omp_task_validate_child():
    ''' Test the validate_child method of the OMPTaskDirective'''
    assert OMPTaskDirective._validate_child(0, Schedule()) is True
    assert OMPTaskDirective._validate_child(1, OMPPrivateClause()) is True
    assert OMPTaskDirective._validate_child(2, OMPFirstprivateClause()) is True
    assert OMPTaskDirective._validate_child(3, OMPSharedClause()) is True
    assert OMPTaskDirective._validate_child(4, OMPDependClause()) is True
    assert OMPTaskDirective._validate_child(5, OMPDependClause()) is True
    assert OMPTaskDirective._validate_child(6, OMPDependClause()) is False
    assert OMPTaskDirective._validate_child(0, "string") is False
    assert OMPTaskDirective._validate_child(1, "string") is False
    assert OMPTaskDirective._validate_child(2, "string") is False
    assert OMPTaskDirective._validate_child(3, "string") is False
    assert OMPTaskDirective._validate_child(4, "string") is False
    assert OMPTaskDirective._validate_child(5, "string") is False


def test_omp_task_directive_clause_accessors(fortran_reader):
    ''' Test the input_depend_clause and output_depend_clause methods.'''
    code = '''
    subroutine my_subroutine()
        integer, dimension(10, 10) :: A
        integer, dimension(10, 10) :: B
        integer :: i
        integer :: j
        do i = 1, 10
            do j = 1, 10
                A(i, j) = B(i, j) + 1
            end do
        end do
        do i = 1, 10
            do j = 1, 10
                A(i, j) = 0
            end do
        end do
    end subroutine
    '''
    tree = fortran_reader.psyir_from_source(code)
    ptrans = OMPParallelTrans()
    strans = OMPSingleTrans()
    tdir = DynamicOMPTaskDirective()
    loops = tree.walk(Loop, stop_type=Loop)
    loop = loops[0]
    parent = loop.parent
    loop.detach()
    tdir.children[0].addchild(loop)
    parent.addchild(tdir, index=0)
    strans.apply(parent.children)
    ptrans.apply(parent.children)

    copy = tree.lower_to_language_level()
    task_dir = copy.walk(OMPTaskDirective)[0]
    assert isinstance(task_dir.input_depend_clause, OMPDependClause)
    assert (task_dir.input_depend_clause._operand ==
            OMPDependClause.DependClauseTypes.IN)
    assert isinstance(task_dir.output_depend_clause, OMPDependClause)
    assert (task_dir.output_depend_clause._operand ==
            OMPDependClause.DependClauseTypes.OUT)


def test_omp_task_directive_begin_end_string():
    ''' Test the begin_string and end_string methods of OMPTaskDirective. '''
    node = OMPTaskDirective()
    assert node.begin_string() == "omp task"
    assert node.end_string() == "omp end task"
