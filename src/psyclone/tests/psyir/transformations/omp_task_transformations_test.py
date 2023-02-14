# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2021-2022, Science and Technology Facilities Council.
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
# Authors: A. B. G. Chalk, STFC Daresbury Lab
'''
API-agnostic tests for OpenMP task transformation class.
'''
from __future__ import absolute_import, print_function
import os
import pytest

from psyclone.errors import GenerationError
from psyclone.parse.algorithm import parse
from psyclone.psyGen import PSyFactory
from psyclone.psyir.nodes import Loop, CodeBlock
from psyclone.psyir.transformations import TransformationError
from psyclone.transformations import OMPParallelTrans, \
    OMPSingleTrans
from psyclone.psyir.transformations import OMPTaskTrans

GOCEAN_BASE_PATH = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                                os.pardir, os.pardir, "test_files",
                                "gocean1p0")


def test_omptask_trans_str():
    '''Test the __str__ and name method of the OMPTaskTrans'''
    trans = OMPTaskTrans()
    assert str(trans) == "Adds an 'OMP TASK' directive to a statement"
    assert trans.name == "OMPTaskTrans"


def test_omptask_directive_fails():
    ''' Test the _directive method of the OMPTaskTrans fails if a collapse
    value is supplied. '''
    trans = OMPTaskTrans()
    with pytest.raises(TransformationError) as excinfo:
        trans._directive([], True)
    assert ("Collapse attribute should not be set for OMPTaskTrans" in
            str(excinfo.value))


def test_omptask_validate(fortran_reader):
    '''Test the validate method of the OMPTaskTrans fails when supplied
    a loop containing a codeblock'''
    code = '''
    subroutine sub()
        integer :: ji, jj, n
        integer, dimension(10, 10) :: t
        integer, dimension(10, 10) :: s
        do jj = 1, 10
            do ji = 1, 10
                t(ji, jj) = s(ji, jj)
            end do
        end do
    end subroutine sub
    '''
    psyir = fortran_reader.psyir_from_source(code)
    trans = OMPTaskTrans()
    loops = psyir.walk(Loop)
    loops[1].children[3].children.append(CodeBlock([], None))
    with pytest.raises(GenerationError) as excinfo:
        trans.apply(loops[1])
    assert ("OMPTaskTransformation cannot be applied to a region containing "
            "a code block" in str(excinfo.value))


def test_omptask_apply():
    ''' Test the apply method of the OMPTaskTrans. '''
    _, invoke_info = parse(os.path.join(GOCEAN_BASE_PATH, "single_invoke.f90"),
                           api="gocean1.0")
    taskt = OMPTaskTrans()
    master = OMPSingleTrans()
    parallel = OMPParallelTrans()
    psy = PSyFactory("gocean1.0", distributed_memory=False).\
        create(invoke_info)
    schedule = psy.invokes.invoke_list[0].schedule

    taskt.apply(schedule.children[0])
    master.apply(schedule.children[0])
    parallel.apply(schedule.children[0])

    code = str(psy.gen)
    assert (
        "    !$omp parallel default(shared), private(i,j)\n" +
        "      !$omp single\n" +
        "      !$omp task private(j,i), shared(cu_fld,p_fld,u_fld), depend(" +
        "in: cu_fld,p_fld%data(:,:),u_fld%data(:,:)), depend(" +
        "out: cu_fld%data(:,:))" +
        "\n" + "      DO" in code)
    assert (
        "      END DO\n" +
        "      !$omp end task\n" +
        "      !$omp end single\n" +
        "      !$omp end parallel" in code)
