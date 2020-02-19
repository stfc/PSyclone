# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2019-2020, Science and Technology Facilities Council
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
# Modified by J. Henrichs, Bureau of Meteorology
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
    '''This function is called before any test function. It
    creates a new NameSpace manager, which is responsible to create
    unique region names - this makes sure the test works if the order
    or number of tests run is changed, otherwise the created region
    names will change.'''
    PSyDataNode._namespace = NameSpace()


def ordered_lines_in_text(lines, text):
    '''Verifies that the specified lines occur in text in the
    specified same order, though not necessarily consecutive.
    If not, a ValueError will be raised.
    :param lines: The lines that must occur in this order in the text.
    :type lines: list of str
    :param str text: The text in which the lines must occur.

    :raises ValueError: if a line is not found in the text, or the
        lines occur in a different order.
    '''
    indx = 0
    for line in lines:
        # index will raise a ValueException if the string is not found
        new_index = text.index(line, indx)
        indx = new_index + len(line)

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
    at the position of the first node in the Node list
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
    assert extract_node[0].dag_name == "gocean_extract_1"


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
      CALL psy_data%PreStart("psy_single_invoke_three_kernels", """ \
      """"invoke_0:compute_cv_code:r0", 2, 3)
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
      CALL psy_data%PreStart("psy_single_invoke_three_kernels", """ \
      """"invoke_0:r0", 3, 4)
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


# -----------------------------------------------------------------------------
# Testing driver generation

@pytest.mark.parametrize("create_driver", [None, False, True])
def test_driver_generation_flag(tmpdir, create_driver):
    '''Test that driver generation can be enabled and disabled, and
    that it is disabled by default. If create_driver is None, the
    default behaviour (don't create driver) is tested.

    '''
    # Use tmpdir so that the driver is created in tmp
    tmpdir.chdir()

    etrans = GOceanExtractTrans()
    psy, invoke = get_invoke("driver_test.f90",
                             GOCEAN_API, idx=0, dist_mem=False)
    schedule = invoke.schedule

    if create_driver is None:
        schedule, _ = etrans.apply(schedule.children[0:2])
    else:
        schedule, _ = etrans.apply(schedule.children[0:2],
                                   {'create-driver': create_driver})
    # We are only interested in the potentially triggered driver-creation.
    str(psy.gen)

    driver = tmpdir.join("driver-psy_extract_example_with_various_variable_"
                         "access_patterns-invoke_0_compute_kernel:compute_"
                         "kernel_code:r0.f90")
    # When create_driver is None, as a default no driver should be created.
    # Since "None or False" is "False", this simple test can be used in all
    # three cases.
    assert driver.isfile() == (create_driver or False)


def test_driver_creation(tmpdir):
    '''Test that driver is created correctly for all variable access \
    modes (input, input-output, output).

    '''
    # Use tmpdir so that the driver is created in tmp
    tmpdir.chdir()

    etrans = GOceanExtractTrans()
    psy, invoke = get_invoke("driver_test.f90",
                             GOCEAN_API, idx=0, dist_mem=False)
    schedule = invoke.schedule

    schedule, _ = etrans.apply(schedule.children[0],
                               {'create-driver': True})
    # We are only interested in the driver, so ignore results.
    str(psy.gen)

    driver = tmpdir.join("driver-psy_extract_example_with_various_variable_"
                         "access_patterns-invoke_0_compute_kernel:compute_"
                         "kernel_code:r0.f90")
    assert driver.isfile()

    with driver.open("r") as driver_file:
        driver_code = driver_file.read()

    # This is an excerpt of the code that should get created.
    # It is tested line by line since there is other code in between
    # which is not important, and the order might also change.
    expected = '''      IMPLICIT NONE
      REAL(KIND=8), allocatable, dimension(:,:) :: u_fld
      REAL(KIND=8), allocatable, dimension(:,:) :: p_fld_post
      REAL(KIND=8), allocatable, dimension(:,:) :: p_fld
      REAL(KIND=8), allocatable, dimension(:,:) :: cu_fld_post
      REAL(KIND=8), allocatable, dimension(:,:) :: cu_fld
      TYPE(PSyDataType) psy_data
      CALL psy_data%OpenRead("psy_extract_example_with_various_variable''' \
      '''_access_patterns", "invoke_0_compute_kernel:compute_kernel_code:r0")
      CALL psy_data%ReadVariable("cu_fld_post", cu_fld_post)
      ALLOCATE (cu_fld, mold=cu_fld_post)
      cu_fld = 0.0
      CALL psy_data%ReadVariable("p_fld", p_fld)
      CALL psy_data%ReadVariable("p_fld_post", p_fld_post)
      CALL psy_data%ReadVariable("u_fld", u_fld)
      ! RegionStart
      DO j=2,jstop
        DO i=2,istop+1
          CALL compute_kernel_code(i, j, cu_fld, p_fld, u_fld)
        END DO
      END DO
      ! RegionEnd
      !
      ! Check cu_fld
      ! Check i
      ! Check j
      ! Check p_fld'''

    expected_lines = expected.split("\n")

    for line in expected_lines:
        assert line in driver_code


def test_driver_loop_variables(tmpdir):
    '''Test that loop variables are not stored. ATM this test
    fails because of #641 but also because of #644 (scalars are considered
    to be arrays)

    '''
    # Use tmpdir so that the driver is created in tmp
    tmpdir.chdir()

    etrans = GOceanExtractTrans()
    psy, invoke = get_invoke("driver_test.f90",
                             GOCEAN_API, idx=0, dist_mem=False)
    schedule = invoke.schedule

    schedule, _ = etrans.apply(schedule.children[0],
                               {'create-driver': True})
    # We are only interested in the driver, so ignore results.
    str(psy.gen)

    driver = tmpdir.join("driver-psy_extract_example_with_various_variable_"
                         "access_patterns-invoke_0_compute_kernel:compute_"
                         "kernel_code:r0.f90")

    assert driver.isfile()

    with open(str(driver), "r") as driver_file:
        driver_code = driver_file.read()

    # Since atm types are not handled, scalars are actually considered
    # to be arrays. Once this is fixed, none of those lines should be
    # in the code anymore (j_post should be declared as scalar):
    unexpected = '''      REAL(KIND=8), allocatable, dimension(:,:) :: j_post
      ALLOCATE (j, mold=j_post)'''
    unexpected_lines = unexpected.split("\n")

    for line in unexpected_lines:
        if line in driver_code:
            pytest.xfail("#641 Loop variables are stored.")
    assert False, "X-failing test working: #641 Loop variables."


def test_driver_scalars(tmpdir):
    '''
    This tests the extraction and driver scalars.
    '''
    # Use tmpdir so that the driver is created in tmp
    tmpdir.chdir()

    etrans = GOceanExtractTrans()
    psy, invoke = get_invoke("single_invoke_scalar_float_arg.f90",
                             GOCEAN_API, idx=0, dist_mem=False)

    etrans.apply(invoke.schedule.children[0], {'create-driver': True})

    # First test extraction code
    # --------------------------
    extract_code = str(psy.gen)

    # Test the handling of scalar parameter in extraction code:
    expected_lines = ['CALL psy_data%PreDeclareVariable("a_scalar", '
                      'a_scalar)',
                      'CALL psy_data%ProvideVariable("a_scalar", a_scalar)']

    # Check that the above lines occur in the same order. There might be
    # other lines between the expected lines, which will be ignored in
    # 'ordered_linex_in_text'.
    ordered_lines_in_text(expected_lines, extract_code)

    # Now test the created driver:
    # ----------------------------
    driver_name = tmpdir.join("driver-psy_single_invoke_scalar_float_test-"
                              "invoke_0_bc_ssh:bc_ssh_code:r0.f90")
    with open(str(driver_name), "r") as driver_file:
        driver_code = driver_file.read()

    expected_lines = ['REAL(KIND=8) :: a_scalar',
                      'CALL psy_data%OpenRead("kernel_scalar_float", '
                      '"bc_ssh_code")',
                      'CALL psy_data%ReadVariable("a_scalar", a_scalar)']

    # Check that the above lines occur in the same order. There might be
    # other lines between the expected lines, which will be ignored in
    # 'ordered_linex_in_text'.
    with pytest.raises(ValueError):
        ordered_lines_in_text(expected_lines, driver_code)
    pytest.xfail("#644 Scalars not supported yet.")


@pytest.mark.xfail(reason="Grid properties not yet supported - #638")
def test_driver_grid_properties(tmpdir):
    '''
    This tests the extraction and driver generated for grid properties.
    '''
    # Use tmpdir so that the driver is created in tmp
    tmpdir.chdir()

    etrans = GOceanExtractTrans()
    psy, invoke = get_invoke("single_invoke_scalar_float_arg.f90",
                             GOCEAN_API, idx=0, dist_mem=False)

    etrans.apply(invoke.schedule.children[0], {'create-driver': True})

    # First test extraction code
    # --------------------------
    extract_code = str(psy.gen)

    # Test the handling of scalar and array grid properties
    expected_lines = ['CALL psy_data%PreDeclareVariable("ssh_fld%grid%'
                      'subdomain%internal%xstop", ssh_fld%grid%subdomain%'
                      'internal%xstop)',
                      'CALL psy_data%PreDeclareVariable("ssh_fld%grid%tmask", '
                      'ssh_fld%grid%tmask)',
                      'CALL psy_data%ProvideVariable("ssh_fld%grid%subdomain%'
                      'internal%xstop", ssh_fld%grid%subdomain%internal%'
                      'xstop)',
                      'CALL psy_data%ProvideVariable("ssh_fld%grid%tmask", '
                      'ssh_fld%grid%tmask)']

    # Check that the above lines occur in the same order. There might be
    # other lines between the expected lines, which will be ignored in
    # 'ordered_linex_in_text'.
    ordered_lines_in_text(expected_lines, extract_code)

    # Now test the created driver:
    # ----------------------------
    driver_name = tmpdir.join("driver-psy_single_invoke_scalar_float_test-"
                              "invoke_0_bc_ssh:bc_ssh_code:r0.f90")
    with open(str(driver_name), "r") as driver_file:
        driver_code = driver_file.read()

    expected_lines = ['REAL(KIND=8), allocatable, dimension(:,:) :: tmask',
                      'INTEGER :: xstop',
                      'CALL psy_data%OpenRead(',
                      '"psy_single_invoke_scalar_float_test", '
                      '"invoke_0_bc_ssh:bc_ssh_code:r0")',
                      'CALL psy_data%ReadVariable("ssh_fld%grid%subdomain%'
                      'internal%xstop", xstop)',
                      'CALL psy_data%ReadVariable("ssh_fld%grid%tmask", '
                      'tmask)']
    # Check that the above lines occur in the same order. There might be
    # other lines between the expected lines, which will be ignored in
    # 'ordered_linex_in_text'.
    ordered_lines_in_text(expected_lines, driver_code)
