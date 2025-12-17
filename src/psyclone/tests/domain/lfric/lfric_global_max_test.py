# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2025, Science and Technology Facilities Council.
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
# Author A. R. Porter, STFC Daresbury Lab
# -----------------------------------------------------------------------------

'''Module containing pytest tests for the LFRicGlobalMax class.'''

from psyclone.domain.lfric.lfric_global_reductions import LFRicGlobalMax
from psyclone.psyGen import Kern
from psyclone.tests.utilities import get_invoke

TEST_API = "lfric"


def test_lgmax_in_invoke():
    '''
    Test the construction of an LFRicGlobalMax object.

    This is complicated by the need to supply it with an LFRicKernelArgument
    and therefore we use a full example to get hold of a suitable argument.
    '''
    psy, invoke = get_invoke("1.9_single_invoke_2_real_scalars.f90",
                             TEST_API, dist_mem=True,
                             idx=0)
    sched = invoke.schedule

    # Find a suitable kernel argument (real scalar).
    kernel = sched.walk(Kern)[0]
    for arg in kernel.args:
        if arg.is_scalar and arg.intrinsic_type == "real":
            break

    lgm = LFRicGlobalMax(operand=arg)
    assert isinstance(lgm, LFRicGlobalMax)
    assert lgm.operand is not arg
    assert lgm.operand.name == arg.name

    sched.addchild(lgm)
    output = psy.gen
    assert "use lfric_mpi_mod, only : lfric_mpi_type" in output, output
    assert "type(lfric_mpi_type) :: mpi" in output, output
    assert "real(kind=r_def) :: glob_a" in output, output
    assert "mpi = f1_proxy%get_mpi()" in output, output
    assert '''\
    ! Perform global max
    call mpi%global_max(a, glob_a)
    a = glob_a''' in output, output

    # Can't compile this because we're assigning to a read-only scalar
    # argument.
