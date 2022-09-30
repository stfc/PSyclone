# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2022, Science and Technology Facilities Council.
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
# Authors R. W. Ford, A. R. Porter, S. Siso and N. Nobre, STFC Daresbury Lab
# Modified I. Kavcic and A. Coughtrie, Met Office
#          C.M. Maynard, Met Office / University of Reading
# Modified J. Henrichs, Bureau of Meteorology
# Modified A. B. G. Chalk, STFC Daresbury Lab

''' Tests of the KernelModuleInlineTrans PSyIR transformation. '''

import pytest
from psyclone.configuration import Config
from psyclone.psyir.transformations import TransformationError
from psyclone.tests.gocean_build import GOceanBuild
from psyclone.tests.utilities import count_lines, get_invoke
from psyclone.domain.common.transformations import KernelModuleInlineTrans


def test_module_inline(tmpdir):
    ''' Test that we can successfully inline a basic kernel subroutine
    routine into the PSy layer module by directly setting inline to
    true for the specified kernel. '''
    psy, invoke = get_invoke("single_invoke_three_kernels.f90", "gocean1.0",
                             idx=0, dist_mem=False)
    schedule = invoke.schedule
    kern_call = schedule.children[0].loop_body[0].loop_body[0]
    kern_call.module_inline = True
    gen = str(psy.gen)
    # check that the subroutine has been inlined correctly
    expected = (
        "    END SUBROUTINE invoke_0\n"
        "    SUBROUTINE compute_cu_code(i, j, cu, p, u)\n")
    assert expected in gen
    # check that the associated use no longer exists
    assert 'USE compute_cu_mod, ONLY: compute_cu_code' not in gen
    assert GOceanBuild(tmpdir).code_compiles(psy)


def test_module_inline_with_transformation(tmpdir):
    ''' Test that we can succesfully inline a basic kernel subroutine
    routine into the PSy layer module using a transformation '''
    psy, invoke = get_invoke("single_invoke_three_kernels.f90", "gocean1.0",
                             idx=0, dist_mem=False)
    schedule = invoke.schedule
    kern_call = schedule.children[1].loop_body[0].loop_body[0]
    inline_trans = KernelModuleInlineTrans()
    inline_trans.apply(kern_call)
    gen = str(psy.gen)
    # check that the subroutine has been inlined
    assert 'SUBROUTINE compute_cv_code(i, j, cv, p, v)' in gen
    # check that the associated use no longer exists
    assert 'USE compute_cv_mod, ONLY: compute_cv_code' not in gen
    assert GOceanBuild(tmpdir).code_compiles(psy)


def test_transformation_inline_error_if_not_kernel():
    ''' Test that the inline transformation fails if the object being
    passed is not a kernel'''
    _, invoke = get_invoke("single_invoke_three_kernels.f90", "gocean1.0",
                           idx=0, dist_mem=False)
    schedule = invoke.schedule
    kern_call = schedule.children[0].loop_body[0]
    inline_trans = KernelModuleInlineTrans()
    with pytest.raises(TransformationError):
        inline_trans.apply(kern_call)


def test_module_inline_with_sub_use(tmpdir):
    ''' Test that we can module inline a kernel subroutine which
    contains a use statement'''
    psy, invoke = get_invoke("single_invoke_scalar_int_arg.f90", "gocean1.0",
                             idx=0, dist_mem=False)
    schedule = invoke.schedule
    kern_call = schedule.children[0].loop_body[0].loop_body[0]
    inline_trans = KernelModuleInlineTrans()
    inline_trans.apply(kern_call)
    gen = str(psy.gen)
    # check that the subroutine has been inlined
    assert 'SUBROUTINE bc_ssh_code(ji, jj, istep, ssha, tmask)' in gen
    # check that the use within the subroutine exists
    assert 'USE grid_mod' in gen
    # check that the associated psy use does not exist
    assert 'USE bc_ssh_mod, ONLY: bc_ssh_code' not in gen
    assert GOceanBuild(tmpdir).code_compiles(psy)


def test_module_inline_same_kernel(tmpdir):
    '''Tests that correct results are obtained when an invoke that uses
    the same kernel subroutine more than once has that kernel
    inlined'''
    psy, invoke = get_invoke("test14_module_inline_same_kernel.f90",
                             "gocean1.0", idx=0)
    schedule = invoke.schedule
    kern_call = schedule.coded_kernels()[0]
    inline_trans = KernelModuleInlineTrans()
    inline_trans.apply(kern_call)
    gen = str(psy.gen)
    # check that the subroutine has been inlined
    assert 'SUBROUTINE compute_cu_code(' in gen
    # check that the associated psy "use" does not exist
    assert 'USE compute_cu_mod, ONLY: compute_cu_code' not in gen
    # check that the subroutine has only been inlined once
    count = count_lines(gen, "SUBROUTINE compute_cu_code(")
    assert count == 1, "Expecting subroutine to be inlined once"
    assert GOceanBuild(tmpdir).code_compiles(psy)


def test_module_inline_dynamo(monkeypatch, annexed, dist_mem):
    '''Tests that correct results are obtained when a kernel is inlined
    into the psy-layer in the dynamo0.3 API. All previous tests use GOcean
    for testing.

    We also test when annexed is False and True as it affects how many halo
    exchanges are generated.

    '''
    config = Config.get()
    dyn_config = config.api_conf("dynamo0.3")
    monkeypatch.setattr(dyn_config, "_compute_annexed_dofs", annexed)
    psy, invoke = get_invoke("4.6_multikernel_invokes.f90", "dynamo0.3",
                             name="invoke_0", dist_mem=dist_mem)
    schedule = invoke.schedule
    if dist_mem:
        if annexed:
            index = 6
        else:
            index = 8
    else:
        index = 1
    kern_call = schedule.children[index].loop_body[0]
    inline_trans = KernelModuleInlineTrans()
    inline_trans.apply(kern_call)
    gen = str(psy.gen)
    # check that the subroutine has been inlined
    assert 'SUBROUTINE ru_code(' in gen
    # check that the associated psy "use" does not exist
    assert 'USE ru_kernel_mod, only : ru_code' not in gen
