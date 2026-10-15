# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2019-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

''' Module containing tests for PSyclone ExtractTrans.
'''

import pytest

from psyclone.domain.lfric.transformations import LFRicExtractTrans
from psyclone.psyir.nodes import Loop
from psyclone.psyir.transformations import (
    ACCLoopTrans, ExtractTrans, TransformationError)
from psyclone.tests.utilities import get_invoke
from psyclone.transformations import (ACCParallelTrans,
                                      LFRicOMPParallelLoopTrans)


# --------------------------------------------------------------------------- #
# ================== Extract Transformation tests =========================== #
# --------------------------------------------------------------------------- #


def test_extract_trans():
    '''Tests basic functions in ExtractTrans.'''
    etrans = ExtractTrans()
    assert str(etrans) == "Create a sub-tree of the PSyIR that has " \
                          "a node of type ExtractNode at its root."
    assert etrans.name == "ExtractTrans"

    ltrans = LFRicExtractTrans()
    assert str(ltrans) == "Create a sub-tree of the PSyIR that has " \
                          "a node of type ExtractNode at its root."


# -----------------------------------------------------------------------------
def test_extract_validate():
    '''Test that the validate function can successfully finish.'''

    _, invoke = get_invoke("single_invoke_three_kernels.f90",
                           "gocean", idx=0, dist_mem=False)
    etrans = ExtractTrans()
    etrans.validate(invoke.schedule.children)


# -----------------------------------------------------------------------------
def test_extract_halo_exchange():
    '''Test that if distributed memory is enabled, halo exchanges are
    not allowed to be included.'''

    _, invoke = get_invoke("single_invoke_three_kernels.f90",
                           "gocean", idx=0, dist_mem=True)
    etrans = ExtractTrans()
    with pytest.raises(TransformationError) as excinfo:
        etrans.validate(invoke.schedule)
    assert ("Nodes of type 'GOHaloExchange' cannot be enclosed by a "
            "ExtractTrans transformation" in str(excinfo.value))


# -----------------------------------------------------------------------------
def test_extract_kern_builtin_no_loop():
    ''' Test that applying Extract Transformation on a Kernel or Built-in
    call without its parent Loop raises a TransformationError. '''

    gocetrans = ExtractTrans()
    _, invoke = get_invoke("single_invoke_three_kernels.f90",
                           "gocean", idx=0, dist_mem=False)
    schedule = invoke.schedule
    # Test Kernel call
    kernel_call = schedule.children[0].loop_body[0].loop_body[0]
    with pytest.raises(TransformationError) as excinfo:
        gocetrans.validate([kernel_call])
    assert "Error in ExtractTrans: Application to a Kernel or a " \
           "Built-in call without its parent Loop is not allowed." \
           in str(excinfo.value)


# -----------------------------------------------------------------------------
def test_extract_loop_no_directive_lfric():
    ''' Test that applying Extract Transformation on a Loop without its
    parent Directive when optimisations are applied in LFRic API
    raises a TransformationError. '''
    etrans = LFRicExtractTrans()

    # Test a Loop nested within the OMP Parallel DO Directive
    _, invoke = get_invoke("4.13_multikernel_invokes_w3_anyd.f90",
                           "lfric", idx=0, dist_mem=False)
    schedule = invoke.schedule
    # Apply LFRicOMPParallelLoopTrans to the second Loop
    otrans = LFRicOMPParallelLoopTrans()
    otrans.apply(schedule[1])
    loop = schedule.children[1].dir_body[0]
    # Try extracting the Loop inside the OMP Parallel DO region
    with pytest.raises(TransformationError) as excinfo:
        etrans.validate([loop])
    assert "Error in LFRicExtractTrans: Application to a Loop without its " \
           "parent Directive is not allowed." in str(excinfo.value)


# -----------------------------------------------------------------------------
def test_extract_directive_no_loop():
    ''' Test that applying Extract Transformation on an orphaned
    ACCLoopDirective without its ancestor ACCParallelDirective
    when optimisations are applied raises a TransformationError. '''

    etrans = ExtractTrans()
    acclpt = ACCLoopTrans()
    accpara = ACCParallelTrans()

    _, invoke = get_invoke("single_invoke_three_kernels.f90",
                           "gocean", idx=0, dist_mem=False)
    schedule = invoke.schedule

    # Apply the OpenACC Loop transformation to every loop in the Schedule
    for child in schedule.children:
        if isinstance(child, Loop):
            acclpt.apply(child)
    # Enclose all of these loops within a single ACC Parallel region
    accpara.apply(schedule.children)

    orphaned_directive = schedule.children[0].children[0]
    with pytest.raises(TransformationError) as excinfo:
        etrans.validate(orphaned_directive)
    assert "Error in ExtractTrans: Application to Nodes enclosed " \
           "within a thread-parallel region is not allowed." \
           in str(excinfo.value)
