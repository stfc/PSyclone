# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2023, Science and Technology Facilities Council.
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
# Author: O. Brunt, Met Office

''' Module containing pytest tests of the LFRicRealToRealXKern built-in
    (converting real-valued field element of precision r_<prec> to 
    real-valued field elements of r_<prec>).'''

import os
import re
import pytest

from psyclone.configuration import Config
from psyclone.domain.lfric.kernel import LFRicKernelMetadata
from psyclone.domain.lfric.lfric_builtins import LFRicRealToRealXKern
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


def test_real_to_real_x(tmpdir, monkeypatch, annexed, dist_mem):
    '''
    Test that 1) the '__str__' method of 'LFRicRealToRealXKern' returns the
    expected string and 2) we generate correct code for the built-in
    operation 'Y = REAL(X, kind=r_<prec>)' where 'Y' is a real-valued
    field of kind 'r_<prec>' and 'X' is the real-valued field being
    converted. Test with and without annexed DoFs being computed as this
    affects the generated code. 3) Also test the 'metadata()' method.

    '''
    # Test metadata
    metadata = LFRicRealToRealXKern.metadata()
    assert isinstance(metadata, LFRicKernelMetadata)
    api_config = Config.get().api_conf(API)
    # Test with and without annexed DoFs
    monkeypatch.setattr(api_config, "_compute_annexed_dofs", annexed)
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "15.29.1_real_to_real_X_builtin.f90"),
                           api=API)
    psy = PSyFactory(API, distributed_memory=dist_mem).create(invoke_info)

    # Test '__str__' method
    first_invoke = psy.invokes.invoke_list[0]
    kern = first_invoke.schedule.children[0].loop_body[0]
    assert str(kern) == ("Built-in: real_to_real_X (convert a real-valued to a "
                         "real-valued field)")

    # Test code generation
    code = str(psy.gen)

    # Check that the correct field types and constants are used
    output = (
        "    USE constants_mod, ONLY: r_def, i_def\n"
        "    USE field_mod, ONLY: field_type, field_proxy_type\n")
    assert output in code

    # Check built-in loop
    output = (
        "      DO df=loop0_start,loop0_stop\n"
        "        f2_data(df) = REAL(f1_data(df), kind=r_def)\n"
        "      END DO\n")
    assert output in code

    if not dist_mem:
        assert "undf_aspc1_f2 = f2_proxy%vspace%get_undf()\n" in code
        assert "loop0_stop = undf_aspc1_f2\n" in code
    else:
        output_dm = "loop0_stop = f2_proxy%vspace%get_last_dof_annexed()\n"
        assert output in code
        assert "CALL f2_proxy%set_dirty()\n" in code
        if not annexed:
            output_dm = output_dm.replace("dof_annexed", "dof_owned")
        assert output_dm in code

    # Test compilation of generated code
    assert LFRicBuild(tmpdir).code_compiles(psy)


@pytest.mark.parametrize("kind_name", ["r_solver", "r_tran", "r_bl", "r_phys"])
def test_real_to_real_x_precision(tmpdir, kind_name):
    '''
    Test that the built-in picks up and creates correct code for field
    data with precision that is not the default, i.e. not 'r_def'. Try with
    all supported LFRic real-valued field precisions to make sure all work.

    '''
    # Note that monkeypatching required to change field type, field proxy
    # type and the field data precisions is extensive and complicated.
    # Modifying the test algorithm is easier and more effective.
    with open(os.path.join(BASE_PATH, "15.29.1_real_to_real_X_builtin.f90"),
              "r", encoding='utf-8') as alg_file:
        alg_code = alg_file.read()

    # Modify the 'real'-valued field type and precision, and store the
    # modified temporary algorithm
    pattern = re.compile(r"\bfield_")
    alg_code = re.sub(pattern, f"{kind_name}_field_", alg_code)
    os.mkdir(str(tmpdir.join("tmp")))
    tmp_fname = str(tmpdir.join("tmp", f"real_{kind_name}_X_builtin_alg.f90"))
    with open(tmp_fname, "w", encoding='utf-8') as tmp_file:
        tmp_file.write(alg_code)
    tmp_file.close()

    # Read and parse the modified algorithm
    with open(tmp_fname, "r", encoding='utf-8') as alg_file:
        _, invoke_info = parse(alg_file, api=API)
    psy = PSyFactory(API).create(invoke_info)
    code = str(psy.gen)

    # Test code generation
    assert f"USE constants_mod, ONLY: {kind_name}, i_def" in code
    assert (f"REAL(KIND={kind_name}), pointer, dimension(:) :: "
            "f2_data => null()") in code
    assert f"f2_data(df) = REAL(f1_data(df), kind={kind_name})" in code


def test_real_to_real_x_lowering(fortran_writer):
    '''
    Test that the lower_to_language_level() method works as expected.

    '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "15.29.1_real_to_real_X_builtin.f90"),
                           api=API)
    psy = PSyFactory(API,
                     distributed_memory=False).create(invoke_info)
    first_invoke = psy.invokes.invoke_list[0]
    kern = first_invoke.schedule.children[0].loop_body[0]
    parent = kern.parent
    lowered = kern.lower_to_language_level()
    assert parent.children[0] is lowered
    assert isinstance(parent.children[0], Assignment)

    loop = first_invoke.schedule.walk(Loop)[0]
    code = fortran_writer(loop)
    assert ("do df = loop0_start, loop0_stop, 1\n"
            "  f2_data(df) = REAL(f1_data(df), kind=r_def)\n"
            "enddo") in code
