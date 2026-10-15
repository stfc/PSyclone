# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2021-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

''' Performs py.test tests on the OpenMP PSyIR Directive nodes. '''

import os
import pytest
from psyclone.psyir.nodes import Schedule, \
    Loop, OMPTaskDirective, OMPPrivateClause, OMPFirstprivateClause, \
    OMPSharedClause, OMPDependClause, DynamicOMPTaskDirective, \
    OMPSingleDirective
from psyclone.errors import GenerationError
from psyclone.transformations import OMPSingleTrans
from psyclone.psyir.transformations import OMPParallelTrans

BASE_PATH = os.path.join(os.path.dirname(os.path.dirname(os.path.dirname(
    os.path.abspath(__file__)))), "test_files", "lfric")
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
    assert (task_dir.input_depend_clause.operator ==
            OMPDependClause.DependClauseTypes.IN)
    assert isinstance(task_dir.output_depend_clause, OMPDependClause)
    assert (task_dir.output_depend_clause.operator ==
            OMPDependClause.DependClauseTypes.OUT)


def test_omp_task_directive_begin_end_string():
    ''' Test the begin_string and end_string methods of OMPTaskDirective. '''
    node = OMPTaskDirective()
    assert node.begin_string() == "omp task"
    assert node.end_string() == "omp end task"
