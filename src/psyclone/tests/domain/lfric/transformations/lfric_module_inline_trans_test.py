# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017-2022, Science and Technology Facilities Council.
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

'''Module containing pytest tests for the KernelModuleInlineTrans applied to
the LFRic API.'''

from psyclone.configuration import Config
from psyclone.tests.utilities import get_invoke
from psyclone.transformations import KernelModuleInlineTrans


# The version of the API that the tests in this file
# exercise.
TEST_API = "dynamo0.3"


def test_module_inline(monkeypatch, annexed, dist_mem):
    '''Tests that correct results are obtained when a kernel is inlined
    into the psy-layer in the dynamo0.3 API. More in-depth tests can
    be found in the gocean1p0_transformations.py file. We also test
    when annexed is False and True as it affects how many halo
    exchanges are generated.

    '''
    config = Config.get()
    dyn_config = config.api_conf(TEST_API)
    monkeypatch.setattr(dyn_config, "_compute_annexed_dofs", annexed)
    psy, invoke = get_invoke("4.6_multikernel_invokes.f90", TEST_API,
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


def test_module_inline_mixed_precision():
    '''
    '''
    psy, invoke = get_invoke("4.6_multikernel_invokes.f90", TEST_API,
                             name="invoke_0", dist_mem=False)

