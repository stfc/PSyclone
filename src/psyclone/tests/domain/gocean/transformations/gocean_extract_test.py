# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2019, Science and Technology Facilities Council
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
# Author I. Kavcic, Met Office
# Modified by A. R. Porter, STFC Daresbury Lab
# -----------------------------------------------------------------------------

''' Module containing tests for PSyclone GOceanExtractTrans
transformations.
'''

from __future__ import absolute_import

import pytest

from psyclone.domain.gocean.transformations import GOceanExtractTrans
from psyclone.psyir.nodes import ExtractNode, PSyDataNode
from psyclone.psyGen import Loop, NameSpace
from psyclone.psyir.transformations import TransformationError
from psyclone.tests.utilities import get_invoke

# API names
GOCEAN_API = "gocean1.0"


@pytest.fixture(scope="function", autouse=True)
def clear_psydata_namespace():
    '''This function is called at the before any test function. It
    creates a new NameSpace manager, which is responsible to create
    unique region names - this makes sure the test works if the order
    or number of tests run is changed, otherwise the created region
    names will change.'''
    PSyDataNode._namespace = NameSpace()

# --------------------------------------------------------------------------- #
# ================== Extract Transformation tests =========================== #
# --------------------------------------------------------------------------- #


def test_gocean_extract_trans():
    '''Tests basic functions in ExtractTrans.'''
    etrans = GOceanExtractTrans()
    assert str(etrans) == "Create a sub-tree of the PSyIR that has " \
                          "GOceanExtractNode at its root."
    assert etrans.name == "GOceanExtractTrans"


def test_kern_builtin_no_loop():
    ''' Test that applying Extract Transformation on a Kernel or Built-in
    call without its parent Loop raises a TransformationError. '''

    gocetrans = GOceanExtractTrans()
    _, invoke = get_invoke("single_invoke_three_kernels.f90",
                           GOCEAN_API, idx=0, dist_mem=False)
    schedule = invoke.schedule
    # Test Kernel call
    kernel_call = schedule.children[0].loop_body[0].loop_body[0]
    with pytest.raises(TransformationError) as excinfo:
        _, _ = gocetrans.apply(kernel_call)
    assert ("Extraction of a Kernel or a Built-in call without its "
            "parent Loop is not allowed.") in str(excinfo.value)


def test_no_outer_loop_gocean1p0():
    ''' Test that applying GOceanExtractTrans on an inner Loop without
    its parent outer Loop in GOcean1.0 API raises a TransformationError. '''
    etrans = GOceanExtractTrans()
    _, invoke = get_invoke("single_invoke_three_kernels.f90",
                           GOCEAN_API, idx=0, dist_mem=False)
    schedule = invoke.schedule

    # Try to extract the region between the outer and the inner Loop
    inner_loop = schedule[0].loop_body
    with pytest.raises(TransformationError) as excinfo:
        _, _ = etrans.apply(inner_loop)
    assert ("GOcean1.0 API: Extraction of an inner Loop without its "
            "ancestor outer Loop is not allowed.") in str(excinfo.value)


def test_no_parent_accdirective():
    ''' Test that applying Extract Transformation on an orphaned
    ACCLoopDirective without its ancestor ACCParallelDirective
    when optimisations are applied raises a TransformationError. '''
    from psyclone.transformations import ACCParallelTrans, ACCEnterDataTrans, \
        ACCLoopTrans

    etrans = GOceanExtractTrans()
    acclpt = ACCLoopTrans()
    accpara = ACCParallelTrans()
    accdata = ACCEnterDataTrans()

    _, invoke = get_invoke("single_invoke_three_kernels.f90",
                           GOCEAN_API, idx=0, dist_mem=False)
    schedule = invoke.schedule

    # Apply the OpenACC Loop transformation to every loop in the Schedule
    for child in schedule.children:
        if isinstance(child, Loop):
            schedule, _ = acclpt.apply(child)
    # Enclose all of these loops within a single ACC Parallel region
    schedule, _ = accpara.apply(schedule.children)
    # Add a mandatory ACC enter-data directive
    schedule, _ = accdata.apply(schedule)

    orphaned_directive = schedule.children[1].children[0]
    with pytest.raises(TransformationError) as excinfo:
        _, _ = etrans.apply(orphaned_directive)
    assert ("Extraction of Nodes enclosed within a thread-parallel "
            "region is not allowed.") in str(excinfo.value)

# --------------------------------------------------------------------------- #
# ================== ExtractNode tests ====================================== #
# --------------------------------------------------------------------------- #


def test_extract_node_position():
    ''' Test that Extract Transformation inserts the ExtractNode
    at the position of the first Node a Schedule in the Node list
    marked for extraction. '''

    # Test GOcean1.0 API for extraction of a single Node
    gocetrans = GOceanExtractTrans()
    _, invoke = get_invoke("single_invoke_three_kernels.f90",
                           GOCEAN_API, idx=0, dist_mem=False)
    schedule = invoke.schedule
    # Apply Extract transformation to the second Node and assert that
    # position and the absolute position of the ExtractNode are the same as
    # respective positions of the second Node before the transformation.
    pos = 1
    child = schedule.children[pos]
    abspos = child.abs_position
    dpth = child.depth
    schedule, _ = gocetrans.apply(child)
    extract_node = schedule.walk(ExtractNode)
    # The result is only one ExtractNode in the list with position 1
    assert extract_node[0].position == pos
    assert extract_node[0].abs_position == abspos
    assert extract_node[0].depth == dpth


def test_single_node_ompparalleldo_gocean1p0():
    ''' Test that applying Extract Transformation on a Node enclosed
    within an OMP Parallel DO Directive produces the correct result
    in GOcean1.0 API. '''
    from psyclone.transformations import GOceanOMPParallelLoopTrans

    etrans = GOceanExtractTrans()
    otrans = GOceanOMPParallelLoopTrans()

    # Test a Loop nested within the OMP Parallel DO Directive
    psy, invoke = get_invoke("single_invoke_three_kernels.f90",
                             GOCEAN_API, idx=0, dist_mem=False)
    schedule = invoke.schedule

    # Apply GOceanOMPParallelLoopTrans to the second Loop
    schedule, _ = otrans.apply(schedule.children[1])
    # Now enclose the parallel region within an ExtractNode (inserted
    # at the previous location of the OMPParallelDoDirective
    schedule, _ = etrans.apply(schedule.children[1])

    code = str(psy.gen)
    output = """      ! ExtractStart
      !
      CALL psy_data%PreStart("compute_cv_mod", "compute_cv_code", 2, 3)
      CALL psy_data%PreDeclareVariable("p_fld", p_fld)
      CALL psy_data%PreDeclareVariable("v_fld", v_fld)
      CALL psy_data%PreDeclareVariable("cv_fld_post", cv_fld)
      CALL psy_data%PreDeclareVariable("i_post", i)
      CALL psy_data%PreDeclareVariable("j_post", j)
      CALL psy_data%PreEndDeclaration
      CALL psy_data%ProvideVariable("p_fld", p_fld)
      CALL psy_data%ProvideVariable("v_fld", v_fld)
      CALL psy_data%PreEnd
      !$omp parallel do default(shared), private(i,j), schedule(static)
      DO j=2,jstop+1
        DO i=2,istop
          CALL compute_cv_code(i, j, cv_fld%data, p_fld%data, v_fld%data)
        END DO 
      END DO 
      !$omp end parallel do
      CALL psy_data%PostStart
      CALL psy_data%ProvideVariable("cv_fld_post", cv_fld)
      CALL psy_data%ProvideVariable("i_post", i)
      CALL psy_data%ProvideVariable("j_post", j)
      CALL psy_data%PostEnd
      !
      ! ExtractEnd"""

    assert output in code


def test_node_list_ompparallel_gocean1p0():
    ''' Test that applying Extract Transformation on a list of Nodes
    enclosed within an OMP Parallel Region produces the correct result
    in GOcean1.0 API. '''
    from psyclone.transformations import GOceanOMPLoopTrans, OMPParallelTrans

    etrans = GOceanExtractTrans()
    ltrans = GOceanOMPLoopTrans()
    otrans = OMPParallelTrans()

    # Test a Loop nested within the OMP Parallel DO Directive
    psy, invoke = get_invoke("single_invoke_three_kernels.f90",
                             GOCEAN_API, idx=0, dist_mem=False)
    schedule = invoke.schedule

    # Apply GOceanOMPParallelLoopTrans to the first two Loops
    schedule, _ = ltrans.apply(schedule.children[0])
    schedule, _ = ltrans.apply(schedule.children[1])
    # and enclose them within a parallel region
    schedule, _ = otrans.apply(schedule.children[0:2])
    # Now enclose the parallel region within an ExtractNode (inserted
    # at the previous location of the OMPParallelDirective
    schedule, _ = etrans.apply(schedule.children[0])

    code = str(psy.gen)
    output = """
      ! ExtractStart
      !
      CALL psy_data%PreStart("compute_cu_mod", "compute_cu_code", 3, 4)
      CALL psy_data%PreDeclareVariable("p_fld", p_fld)
      CALL psy_data%PreDeclareVariable("u_fld", u_fld)
      CALL psy_data%PreDeclareVariable("v_fld", v_fld)
      CALL psy_data%PreDeclareVariable("cu_fld_post", cu_fld)
      CALL psy_data%PreDeclareVariable("cv_fld_post", cv_fld)
      CALL psy_data%PreDeclareVariable("i_post", i)
      CALL psy_data%PreDeclareVariable("j_post", j)
      CALL psy_data%PreEndDeclaration
      CALL psy_data%ProvideVariable("p_fld", p_fld)
      CALL psy_data%ProvideVariable("u_fld", u_fld)
      CALL psy_data%ProvideVariable("v_fld", v_fld)
      CALL psy_data%PreEnd
      !$omp parallel default(shared), private(i,j)
      !$omp do schedule(static)
      DO j=2,jstop
        DO i=2,istop+1
          CALL compute_cu_code(i, j, cu_fld%data, p_fld%data, u_fld%data)
        END DO 
      END DO 
      !$omp end do
      !$omp do schedule(static)
      DO j=2,jstop+1
        DO i=2,istop
          CALL compute_cv_code(i, j, cv_fld%data, p_fld%data, v_fld%data)
        END DO 
      END DO 
      !$omp end do
      !$omp end parallel
      CALL psy_data%PostStart
      CALL psy_data%ProvideVariable("cu_fld_post", cu_fld)
      CALL psy_data%ProvideVariable("cv_fld_post", cv_fld)
      CALL psy_data%ProvideVariable("i_post", i)
      CALL psy_data%ProvideVariable("j_post", j)
      CALL psy_data%PostEnd
      !
      ! ExtractEnd"""

    assert output in code
