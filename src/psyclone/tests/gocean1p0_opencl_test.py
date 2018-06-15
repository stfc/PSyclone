# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2018, Science and Technology Facilities Council
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
# Author A. R. Porter, STFC Daresbury Lab
from __future__ import print_function

'''Tests for OpenCL PSy-layer code generation that are specific to the
GOcean 1.0 API.'''

import os
import pytest
from psyclone.parse import parse
from psyclone.psyGen import PSyFactory
from psyclone.generator import GenerationError, ParseError

API = "gocean1.0"


def test_use_stmts():
    ''' Test that generating code for OpenCL results in the correct
    module use statements '''
    _, invoke_info = parse(os.path.join(os.path.
                                        dirname(os.path.
                                                abspath(__file__)),
                                        "test_files", "gocean1p0",
                                        "single_invoke.f90"),
                           api=API)
    psy = PSyFactory(API).create(invoke_info)
    sched = psy.invokes.invoke_list[0].schedule
    from psyclone.transformations import OCLTrans
    otrans = OCLTrans()
    otrans.apply(sched)
    generated_code = str(psy.gen)
    print(generated_code)
    expected = '''\
    SUBROUTINE invoke_0_compute_cu(cu_fld, p_fld, u_fld)
      USE compute_cu_mod, ONLY: compute_cu_code
      USE clfortran
      USE iso_c_binding'''
    assert expected in generated_code


def test_set_kern_args():
    ''' Check that we generate the necessary code to set kernel arguments '''
    _, invoke_info = parse(os.path.join(os.path.
                                        dirname(os.path.
                                                abspath(__file__)),
                                        "test_files", "gocean1p0",
                                        "single_invoke_two_kernels.f90"),
                           api=API)
    psy = PSyFactory(API).create(invoke_info)
    sched = psy.invokes.invoke_list[0].schedule
    from psyclone.transformations import OCLTrans
    otrans = OCLTrans()
    otrans.apply(sched)
    generated_code = str(psy.gen)
    print(generated_code)
    assert generated_code.count("SUBROUTINE compute_cu_code_set_args(cu_fld, "
                                "p_fld, u_fld)") == 1
    assert generated_code.count("SUBROUTINE time_smooth_code_set_args(u_fld, "
                                "unew_fld, uold_fld)") == 1
    assert ("CALL compute_cu_code_set_args(cu_fld%data, p_fld%data, "
            "u_fld%data)" in generated_code)
