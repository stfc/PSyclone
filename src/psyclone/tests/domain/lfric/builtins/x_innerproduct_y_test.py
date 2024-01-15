# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017-2024, Science and Technology Facilities Council.
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
# Author: A. R. Porter, STFC Daresbury Lab
# Modified: I. Kavcic, Met Office
# Modified: R. W. Ford and N. Nobre, STFC Daresbury Lab
# Modified: by J. Henrichs, Bureau of Meteorology

''' Module containing pytest tests of the LFRicXInnerproductYKern built-in
    (inner product of two real-valued fields).'''

import os
from psyclone.domain.lfric.kernel import LFRicKernelMetadata
from psyclone.domain.lfric.lfric_builtins import LFRicXInnerproductYKern
from psyclone.parse.algorithm import parse
from psyclone.psyGen import PSyFactory
from psyclone.psyir.nodes import Assignment, Loop
from psyclone.tests.lfric_build import LFRicBuild

# Constants
BASE_PATH = os.path.join(
    os.path.dirname(os.path.dirname(os.path.dirname(os.path.dirname(
        os.path.abspath(__file__))))),
    "test_files", "dynamo0p3")

# The PSyclone API under test
API = "dynamo0.3"


def test_x_innerproduct_y(tmpdir, dist_mem):
    '''
    Test that 1) the '__str__' method of 'LFRicXInnerproductYKern'
    returns the expected string and 2) we generate correct code
    for the built-in operation which calculates inner product of
    real-valued fields 'X' and 'Y' as 'innprod = innprod +
    X(:)*Y(:)'. 3) Also test the 'metadata()' method.

    '''
    # Test metadata
    metadata = LFRicXInnerproductYKern.metadata()
    assert isinstance(metadata, LFRicKernelMetadata)
    _, invoke_info = parse(
        os.path.join(BASE_PATH,
                     "15.9.1_X_innerproduct_Y_builtin.f90"),
        api=API)
    psy = PSyFactory(API,
                     distributed_memory=dist_mem).create(invoke_info)

    # Test '__str__' method
    first_invoke = psy.invokes.invoke_list[0]
    kern = first_invoke.schedule.children[0].loop_body[0]
    assert str(kern) == "Built-in: X_innerproduct_Y (real-valued fields)"

    # Test code generation
    code = str(psy.gen)
    output = (
            "      ! Zero summation variables\n"
            "      !\n"
            "      asum = 0.0_r_def\n"
            "      !\n"
            "      DO df=loop0_start,loop0_stop\n"
            "        asum = asum + f1_data(df) * f2_data(df)\n"
            "      END DO")
    assert output in code

    if not dist_mem:
        assert "loop0_stop = undf_aspc1_f1\n" in code
    else:
        assert "USE scalar_mod, ONLY: scalar_type\n" in code
        assert "TYPE(scalar_type) global_sum\n" in code
        assert "loop0_stop = f1_proxy%vspace%get_last_dof_owned()\n" in code
        output = (
            "      global_sum%value = asum\n"
            "      asum = global_sum%get_sum()")
        assert output in code

    # Test compilation of generated code
    assert LFRicBuild(tmpdir).code_compiles(psy)


def test_x_innerproduct_y_lowering(fortran_writer):
    '''
    Test that the lower_to_language_level() method of X_innerproduct_Y
    built-in works as expected.

    '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                           "15.9.1_X_innerproduct_Y_builtin.f90"), api=API)
    psy = PSyFactory(API,
                     distributed_memory=True).create(invoke_info)
    first_invoke = psy.invokes.invoke_list[0]
    kern = first_invoke.schedule.children[0].loop_body[0]
    parent = kern.parent
    lowered = kern.lower_to_language_level()
    assert parent.children[0] is lowered
    assert isinstance(parent.children[0], Assignment)

    loop = first_invoke.schedule.walk(Loop)[0]
    code = fortran_writer(loop)
    assert ("do df = loop0_start, loop0_stop, 1\n"
            "  asum = asum + f1_data(df) * f2_data(df)\n"
            "enddo") in code
