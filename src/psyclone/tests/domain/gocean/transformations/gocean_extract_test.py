# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2019-2024, Science and Technology Facilities Council
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
# Modified by A. R. Porter and S. Siso, STFC Daresbury Lab
# Modified by J. Henrichs, Bureau of Meteorology
# -----------------------------------------------------------------------------

''' Module containing tests for PSyclone GOceanExtractTrans
transformations.
'''

from pathlib import Path

import pytest

from psyclone.configuration import Config
from psyclone.domain.gocean.transformations import GOceanExtractTrans
from psyclone.psyir.nodes import ExtractNode, Loop, OMPDirective
from psyclone.psyir.transformations import PSyDataTrans, TransformationError
from psyclone.transformations import (ACCParallelTrans, ACCEnterDataTrans,
                                      ACCLoopTrans, GOceanOMPLoopTrans,
                                      GOceanOMPParallelLoopTrans,
                                      OMPParallelTrans)
from psyclone.domain.gocean.transformations import GOConstLoopBoundsTrans
from psyclone.tests.utilities import get_invoke

# API names
GOCEAN_API = "gocean1.0"


@pytest.fixture(scope="function", autouse=True)
def clear_region_name_cache():
    '''All PSyData nodes keep a list of used region names as class variables
    to avoid name clashes. This needs to be cleared, otherwise the indices
    used when creating unique region identifier will change depending on the
    order in which tests are run.
    '''
    PSyDataTrans._used_kernel_names = {}
    yield
    PSyDataTrans._used_kernel_names = {}


# ============================================================================
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
    assert str(etrans) == "Create a sub-tree of the PSyIR that has a " \
                          "node of type ExtractNode at its root."
    assert etrans.name == "GOceanExtractTrans"


# -----------------------------------------------------------------------------
def test_gocean_extract_distributed_memory():
    '''Test that distributed memory must be disabled.'''

    _, invoke = get_invoke("single_invoke_three_kernels.f90",
                           GOCEAN_API, idx=0, dist_mem=True)
    etrans = GOceanExtractTrans()
    with pytest.raises(TransformationError) as excinfo:
        etrans.apply(invoke.schedule.children[3])
    assert ("Error in GOceanExtractTrans: Distributed memory is "
            "not supported.") in str(excinfo.value)


# -----------------------------------------------------------------------------
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
        gocetrans.apply(kernel_call)
    assert "Error in GOceanExtractTrans: Application to a Kernel or a " \
           "Built-in call without its parent Loop is not allowed." \
           in str(excinfo.value)


# -----------------------------------------------------------------------------
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
        etrans.apply(inner_loop)
    assert "Error in GOceanExtractTrans: Application to an inner Loop " \
           "without its ancestor outer Loop is not allowed." \
           in str(excinfo.value)


# -----------------------------------------------------------------------------
def test_no_parent_accdirective():
    ''' Test that applying Extract Transformation on an orphaned
    ACCLoopDirective without its ancestor ACCParallelDirective
    when optimisations are applied raises a TransformationError. '''

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
            acclpt.apply(child)
    # Enclose all of these loops within a single ACC Parallel region
    accpara.apply(schedule.children)
    # Add a mandatory ACC enter-data directive
    accdata.apply(schedule)

    orphaned_directive = schedule.children[1].children[0]
    with pytest.raises(TransformationError) as excinfo:
        etrans.apply(orphaned_directive)
    assert "Error in GOceanExtractTrans: Application to Nodes enclosed " \
           "within a thread-parallel region is not allowed." \
           in str(excinfo.value)

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
    gocetrans.apply(child)
    extract_node = schedule.walk(ExtractNode)
    # The result is only one ExtractNode in the list with position 1
    assert extract_node[0].position == pos
    assert extract_node[0].abs_position == abspos
    assert extract_node[0].depth == dpth


# -----------------------------------------------------------------------------
def test_single_node_ompparalleldo_gocean1p0():
    ''' Test that applying Extract Transformation on a Node enclosed
    within an OMP Parallel DO Directive produces the correct result
    in GOcean1.0 API.

    '''
    etrans = GOceanExtractTrans()
    otrans = GOceanOMPParallelLoopTrans()

    # Test a Loop nested within the OMP Parallel DO Directive
    psy, invoke = get_invoke("single_invoke_three_kernels.f90",
                             GOCEAN_API, idx=0, dist_mem=False)
    schedule = invoke.schedule

    # Apply GOceanOMPParallelLoopTrans to the second Loop
    otrans.apply(schedule.children[1])

    # Now enclose the parallel region within an ExtractNode (inserted
    # at the previous location of the OMPParallelDoDirective
    etrans.apply(schedule.children[1])

    code = str(psy.gen)
    output = """
      CALL extract_psy_data % PreStart("psy_single_invoke_three_kernels", """ \
      """"invoke_0:compute_cv_code:r0", 6, 3)
      CALL extract_psy_data % PreDeclareVariable("cv_fld%internal%xstart", """ \
                                               """cv_fld % internal % xstart)
      CALL extract_psy_data % PreDeclareVariable("cv_fld%internal%xstop", """ \
                                               """cv_fld % internal % xstop)
      CALL extract_psy_data % PreDeclareVariable("cv_fld%internal%ystart", """ \
                                               """cv_fld % internal % ystart)
      CALL extract_psy_data % PreDeclareVariable("cv_fld%internal%ystop", """ \
                                               """cv_fld % internal % ystop)
      CALL extract_psy_data % PreDeclareVariable("p_fld", p_fld)
      CALL extract_psy_data % PreDeclareVariable("v_fld", v_fld)
      CALL extract_psy_data % PreDeclareVariable("cv_fld_post", cv_fld)
      CALL extract_psy_data % PreDeclareVariable("i_post", i)
      CALL extract_psy_data % PreDeclareVariable("j_post", j)
      CALL extract_psy_data % PreEndDeclaration
      CALL extract_psy_data % ProvideVariable("cv_fld%internal%xstart", """ \
                                            """cv_fld % internal % xstart)
      CALL extract_psy_data % ProvideVariable("cv_fld%internal%xstop", """ \
                                            """cv_fld % internal % xstop)
      CALL extract_psy_data % ProvideVariable("cv_fld%internal%ystart", """ \
                                            """cv_fld % internal % ystart)
      CALL extract_psy_data % ProvideVariable("cv_fld%internal%ystop", """ \
                                            """cv_fld % internal % ystop)
      CALL extract_psy_data % ProvideVariable("p_fld", p_fld)
      CALL extract_psy_data % ProvideVariable("v_fld", v_fld)
      CALL extract_psy_data % PreEnd
      !$omp parallel do default(shared), private(i,j), schedule(static)
      DO j = cv_fld%internal%ystart, cv_fld%internal%ystop, 1
        DO i = cv_fld%internal%xstart, cv_fld%internal%xstop, 1
          CALL compute_cv_code(i, j, cv_fld%data, p_fld%data, v_fld%data)
        END DO
      END DO
      !$omp end parallel do
      CALL extract_psy_data % PostStart
      CALL extract_psy_data % ProvideVariable("cv_fld_post", cv_fld)
      CALL extract_psy_data % ProvideVariable("i_post", i)
      CALL extract_psy_data % ProvideVariable("j_post", j)
      CALL extract_psy_data % PostEnd
      """
    assert output in code


# -----------------------------------------------------------------------------
def test_single_node_ompparalleldo_gocean1p0_const_loop():
    ''' Test that applying Extract Transformation on a Node enclosed
    within an OMP Parallel DO Directive produces the correct result
    in GOcean1.0 API. This test is mostly identical to the previous one,
    but uses const loop bounds.

    '''
    etrans = GOceanExtractTrans()
    otrans = GOceanOMPParallelLoopTrans()
    ctrans = GOConstLoopBoundsTrans()

    # Test a Loop nested within the OMP Parallel DO Directive
    psy, invoke = get_invoke("single_invoke_three_kernels.f90",
                             GOCEAN_API, idx=0, dist_mem=False)
    schedule = invoke.schedule

    # Apply Constant Loop Bounds transformation
    ctrans.apply(schedule)

    # Apply GOceanOMPParallelLoopTrans to the second Loop
    otrans.apply(schedule.children[3])

    # Now enclose the parallel region within an ExtractNode (inserted
    # at the previous location of the OMPParallelDoDirective
    etrans.apply(schedule.children[3])

    code = str(psy.gen)
    output = """
      CALL extract_psy_data % PreStart("psy_single_invoke_three_kernels", """ \
      """"invoke_0:compute_cv_code:r0", 4, 3)
      CALL extract_psy_data % PreDeclareVariable("istop", istop)
      CALL extract_psy_data % PreDeclareVariable("jstop", jstop)
      CALL extract_psy_data % PreDeclareVariable("p_fld", p_fld)
      CALL extract_psy_data % PreDeclareVariable("v_fld", v_fld)
      CALL extract_psy_data % PreDeclareVariable("cv_fld_post", cv_fld)
      CALL extract_psy_data % PreDeclareVariable("i_post", i)
      CALL extract_psy_data % PreDeclareVariable("j_post", j)
      CALL extract_psy_data % PreEndDeclaration
      CALL extract_psy_data % ProvideVariable("istop", istop)
      CALL extract_psy_data % ProvideVariable("jstop", jstop)
      CALL extract_psy_data % ProvideVariable("p_fld", p_fld)
      CALL extract_psy_data % ProvideVariable("v_fld", v_fld)
      CALL extract_psy_data % PreEnd
      !$omp parallel do default(shared), private(i,j), schedule(static)
      DO j = 2, jstop + 1, 1
        DO i = 2, istop, 1
          CALL compute_cv_code(i, j, cv_fld%data, p_fld%data, v_fld%data)
        END DO
      END DO
      !$omp end parallel do
      CALL extract_psy_data % PostStart
      CALL extract_psy_data % ProvideVariable("cv_fld_post", cv_fld)
      CALL extract_psy_data % ProvideVariable("i_post", i)
      CALL extract_psy_data % ProvideVariable("j_post", j)
      CALL extract_psy_data % PostEnd
      """
    assert output in code


# -----------------------------------------------------------------------------
def test_node_list_ompparallel_gocean1p0():
    ''' Test that applying Extract Transformation on a list of Nodes
    enclosed within an OMP Parallel Region produces the correct result
    in GOcean1.0 API. '''

    etrans = GOceanExtractTrans()
    ltrans = GOceanOMPLoopTrans()
    otrans = OMPParallelTrans()
    ctrans = GOConstLoopBoundsTrans()

    # Test a Loop nested within the OMP Parallel DO Directive
    psy, invoke = get_invoke("single_invoke_three_kernels.f90",
                             GOCEAN_API, idx=0, dist_mem=False)
    schedule = invoke.schedule

    # Apply GOConstLoopBoundsTrans
    ctrans.apply(schedule)
    # Apply GOceanOMPParallelLoopTrans to the first two Loops
    outerloops = schedule.walk(Loop, stop_type=Loop)
    ltrans.apply(outerloops[0])
    ltrans.apply(outerloops[1])
    # and enclose them within a parallel region
    directives = schedule.walk(OMPDirective, stop_type=OMPDirective)
    otrans.apply(directives)
    # Now enclose the parallel region within an ExtractNode (inserted
    # at the previous location of the OMPParallelDirective
    directive = schedule.walk(OMPDirective, stop_type=OMPDirective)
    etrans.apply(directive)

    code = str(psy.gen)
    output = """
      CALL extract_psy_data % PreStart("psy_single_invoke_three_kernels", """ \
      """"invoke_0:r0", 5, 4)
      CALL extract_psy_data % PreDeclareVariable("istop", istop)
      CALL extract_psy_data % PreDeclareVariable("jstop", jstop)
      CALL extract_psy_data % PreDeclareVariable("p_fld", p_fld)
      CALL extract_psy_data % PreDeclareVariable("u_fld", u_fld)
      CALL extract_psy_data % PreDeclareVariable("v_fld", v_fld)
      CALL extract_psy_data % PreDeclareVariable("cu_fld_post", cu_fld)
      CALL extract_psy_data % PreDeclareVariable("cv_fld_post", cv_fld)
      CALL extract_psy_data % PreDeclareVariable("i_post", i)
      CALL extract_psy_data % PreDeclareVariable("j_post", j)
      CALL extract_psy_data % PreEndDeclaration
      CALL extract_psy_data % ProvideVariable("istop", istop)
      CALL extract_psy_data % ProvideVariable("jstop", jstop)
      CALL extract_psy_data % ProvideVariable("p_fld", p_fld)
      CALL extract_psy_data % ProvideVariable("u_fld", u_fld)
      CALL extract_psy_data % ProvideVariable("v_fld", v_fld)
      CALL extract_psy_data % PreEnd
      !$omp parallel default(shared), private(i,j)
      !$omp do schedule(static)
      DO j = 2, jstop, 1
        DO i = 2, istop + 1, 1
          CALL compute_cu_code(i, j, cu_fld%data, p_fld%data, u_fld%data)
        END DO
      END DO
      !$omp end do
      !$omp do schedule(static)
      DO j = 2, jstop + 1, 1
        DO i = 2, istop, 1
          CALL compute_cv_code(i, j, cv_fld%data, p_fld%data, v_fld%data)
        END DO
      END DO
      !$omp end do
      !$omp end parallel
      CALL extract_psy_data % PostStart
      CALL extract_psy_data % ProvideVariable("cu_fld_post", cu_fld)
      CALL extract_psy_data % ProvideVariable("cv_fld_post", cv_fld)
      CALL extract_psy_data % ProvideVariable("i_post", i)
      CALL extract_psy_data % ProvideVariable("j_post", j)
      CALL extract_psy_data % PostEnd
      """
    assert output in code


# -----------------------------------------------------------------------------
# Testing driver generation

@pytest.mark.usefixtures("change_into_tmpdir")
@pytest.mark.parametrize("create_driver", [None, False, True])
def test_driver_generation_flag(create_driver):
    '''Test that driver generation can be enabled and disabled, and
    that it is disabled by default. If create_driver is None, the
    default behaviour (don't create driver) is tested.

    '''
    # Use tmpdir so that the driver is created in tmp

    etrans = GOceanExtractTrans()
    psy, invoke = get_invoke("driver_test.f90",
                             GOCEAN_API, idx=0, dist_mem=False)
    schedule = invoke.schedule

    if create_driver is None:
        etrans.apply(schedule.children[0:2])
    else:
        etrans.apply(schedule.children[0:2],
                     {'create_driver': create_driver})
    # We are only interested in the potentially triggered driver-creation.
    str(psy.gen)

    driver = Path("driver-psy_extract_example_with_various_"
                  "variable_access_patterns-invoke_0_compute_"
                  "kernel:compute_kernel_code:r0.f90")
    # When create_driver is None, as a default no driver should be created.
    # Since "None or False" is "False", this simple test can be used in all
    # three cases.
    assert driver.is_file() == (create_driver or False)


# -----------------------------------------------------------------------------
@pytest.mark.usefixtures("change_into_tmpdir")
def test_driver_loop_variables():
    '''Test that loop variables are not stored. ATM this test
    fails because of #641.

    '''
    etrans = GOceanExtractTrans()
    psy, invoke = get_invoke("driver_test.f90",
                             GOCEAN_API, idx=0, dist_mem=False)
    schedule = invoke.schedule

    etrans.apply(schedule.children[0], {'create_driver': True})
    # We are only interested in the driver, so ignore results.
    str(psy.gen)

    driver = Path("driver-psy_extract_example_with_various_"
                  "variable_access_patterns-invoke_0_compute_"
                  "kernel:compute_kernel_code:r0.f90")

    assert driver.is_file()

    with open(driver, "r", encoding="utf-8") as driver_file:
        driver_code = driver_file.read()

    # Since atm types are not handled, scalars are actually considered
    # to be arrays. Once this is fixed, none of those lines should be
    # in the code anymore (j_post should be declared as scalar):
    unexpected_lines = ['  integer :: j_post', 'j = 0']

    for line in unexpected_lines:
        if line in driver_code:
            pytest.xfail("#641 Loop variables are stored.")
    assert False, "X-failing test working: #641 Loop variables."


# -----------------------------------------------------------------------------
@pytest.mark.usefixtures("change_into_tmpdir")
def test_driver_scalars(fortran_writer):
    '''
    This tests the extraction and driver generated for scalars.

    '''
    # Use tmpdir so that the driver is created in tmp

    etrans = GOceanExtractTrans()
    psy, invoke = get_invoke("single_invoke_scalar_float_arg.f90",
                             GOCEAN_API, idx=0, dist_mem=False)

    etrans.apply(invoke.schedule.children[0], {'create_driver': True})

    # First test extraction code
    # --------------------------
    extract_code = fortran_writer(psy.container)

    # Test the handling of scalar parameter in extraction code:
    expected_lines = ['use extract_psy_data_mod, only : '
                      'extract_PSyDataType',
                      'CALL extract_psy_data % PreDeclareVariable('
                      '"a_scalar", a_scalar)',
                      'CALL extract_psy_data % ProvideVariable('
                      '"a_scalar", a_scalar)']

    # Check that the above lines occur in the same order. There might be
    # other lines between the expected lines, which will be ignored in
    # 'ordered_linex_in_text'.
    ordered_lines_in_text(expected_lines, extract_code)

    # Now test the created driver:
    # ----------------------------
    driver_name = ("driver-psy_single_invoke_scalar_float_test-"
                   "invoke_0_bc_ssh:bc_ssh_code:r0.f90")
    with open(str(driver_name), "r", encoding="utf-8") as driver_file:
        driver_code = driver_file.read()

    expected_lines = ['use extract_psy_data_mod, only : '
                      'extract_PSyDataType',
                      'type(extract_psydatatype) extract_psy_data',
                      'INTEGER :: xstop',
                      'REAL(KIND=8) :: a_scalar',
                      'CALL extract_psy_data%OpenRead("'
                      'kernel_scalar_float", "bc_ssh_code")',
                      'CALL extract_psy_data%ReadVariable("a_scalar", '
                      'a_scalar)']

    # Check that the above lines occur in the same order. There might be
    # other lines between the expected lines, which will be ignored in
    # 'ordered_linex_in_text'.
    with pytest.raises(ValueError):
        ordered_lines_in_text(expected_lines, driver_code)
    pytest.xfail("#644 Scalars not supported yet.")


# -----------------------------------------------------------------------------
@pytest.mark.usefixtures("change_into_tmpdir")
def test_driver_grid_properties(fortran_writer):
    '''
    This tests the extraction and driver generated for grid properties.

    '''
    # Use tmpdir so that the driver is created in tmp

    etrans = GOceanExtractTrans()
    psy, invoke = get_invoke("single_invoke_scalar_float_arg.f90",
                             GOCEAN_API, idx=0, dist_mem=False)

    etrans.apply(invoke.schedule.children[0], {'create_driver': True})

    # First test extraction code
    # --------------------------
    extract_code = fortran_writer(psy.container)

    # Test the handling of scalar and array grid properties
    expected_lines = ['CALL extract_psy_data % PreDeclareVariable("'
                      'ssh_fld%grid%subdomain%internal%xstop", '
                      'ssh_fld % grid % '
                      'subdomain % internal % xstop)',
                      'CALL extract_psy_data % PreDeclareVariable('
                      '"ssh_fld%grid%tmask", ssh_fld % grid % tmask)',
                      'CALL extract_psy_data % ProvideVariable('
                      '"ssh_fld%grid%subdomain%internal%xstop", '
                      'ssh_fld % grid % subdomain % internal % xstop)',
                      'CALL extract_psy_data % ProvideVariable('
                      '"ssh_fld%grid%tmask", ssh_fld % grid % tmask)']

    # Check that the above lines occur in the same order. There might be
    # other lines between the expected lines, which will be ignored in
    # 'ordered_linex_in_text'.
    ordered_lines_in_text(expected_lines, extract_code)

    # Now test the created driver:
    # ----------------------------
    driver_name = ("driver-psy_single_invoke_scalar_float_test-"
                   "invoke_0_bc_ssh:bc_ssh_code:r0.f90")
    with open(str(driver_name), "r", encoding="utf-8") as driver_file:
        driver_code = driver_file.read()

    expected_lines = ['integer :: ssh_fld_grid_subdomain_internal_xstop',
                      'integer, allocatable, dimension(:,:) :: '
                      'ssh_fld_grid_tmask',
                      'call extract_psy_data%OpenRead(',
                      '\'psy_single_invoke_scalar_float_test\', '
                      '\'invoke_0_bc_ssh:bc_ssh_code:r0\')',
                      'call extract_psy_data%ReadVariable('
                      '\'ssh_fld%grid%subdomain%internal%xstop\', '
                      'ssh_fld_grid_subdomain_internal_xstop)',
                      'call extract_psy_data%ReadVariable('
                      '\'ssh_fld%grid%tmask\', ssh_fld_grid_tmask)']

    # Check that the above lines occur in the same order. There might be
    # other lines between the expected lines, which will be ignored in
    # 'ordered_linex_in_text'.
    ordered_lines_in_text(expected_lines, driver_code)


# -----------------------------------------------------------------------------
@pytest.mark.usefixtures("change_into_tmpdir")
def test_rename_region():
    '''
    This tests that an extract region can be renamed, and that the created
    driver will use the new names.

    '''
    # Use tmpdir so that the driver is created in tmp

    etrans = GOceanExtractTrans()
    psy, invoke = get_invoke("single_invoke_scalar_float_arg.f90",
                             GOCEAN_API, idx=0, dist_mem=False)

    etrans.apply(invoke.schedule.children[0],
                 {'create_driver': True,
                  'region_name': ("main", "update")})

    # Test that the extraction code contains the right names
    assert 'CALL extract_psy_data % PreStart("main", "update", 8, 3)' \
        in str(psy.gen)

    # Now test if the created driver has the right name, and will open the
    # right file:
    driver_name = "driver-main-update.f90"
    with open(driver_name, "r", encoding="utf-8") as driver_file:
        driver_code = driver_file.read()
    assert ("call extract_psy_data%OpenRead('main', 'update')"
            in driver_code)


# -----------------------------------------------------------------------------
@pytest.mark.usefixtures("change_into_tmpdir")
def test_change_prefix(monkeypatch):
    '''
    This tests that the prefix of a gocean extract transformation
    can be changed, and that the new prefix is also used in the
    created driver.

    '''
    # Use tmpdir so that the driver is created in tmp

    psy, invoke = get_invoke("single_invoke_scalar_float_arg.f90",
                             GOCEAN_API, idx=0, dist_mem=False)

    # In order to use a different prefix, this prefix needs to be valid.
    # So monkeypatch the valid prefix names in the config object:
    config = Config.get()
    monkeypatch.setattr(config, "_valid_psy_data_prefixes", ["NEW"])

    etrans = GOceanExtractTrans()
    etrans.apply(invoke.schedule.children[0],
                 {'create_driver': True, 'region_name': ("main", "update"),
                  'prefix': "NEW"})

    # Test that the extraction code contains the new prefix:
    gen = str(psy.gen)
    assert 'CALL NEW_psy_data % PreStart("main", "update", 8, 3)' \
        in gen

    # Now test if the created driver has the right prefix:
    driver_name = "driver-main-update.f90"
    with open(str(driver_name), "r", encoding="utf-8") as driver_file:
        driver_code = driver_file.read()
    assert "call NEW_psy_data%OpenRead('main', 'update')" in driver_code
