# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2024, Science and Technology Facilities Council.
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
    (converting real-valued field element of precision 'r_<prec>' to
    real-valued field elements of 'r_<prec>').
'''

import os
import pytest

from psyclone.configuration import Config
from psyclone.domain.lfric.kernel import LFRicKernelMetadata
from psyclone.domain.lfric.lfric_builtins import LFRicRealToRealXKern
from psyclone.parse.algorithm import parse
from psyclone.psyGen import PSyFactory
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
                                        "15.10.8_real_to_real_X_builtin.f90"),
                           api=API)
    psy = PSyFactory(API, distributed_memory=dist_mem).create(invoke_info)

    # Test '__str__' method
    first_invoke = psy.invokes.invoke_list[0]
    kern = first_invoke.schedule.children[0].loop_body[0]
    assert str(kern) == ("Built-in: real_to_real_X (convert a real-valued to "
                         "a real-valued field)")

    # Test code generation
    code = str(psy.gen)

    # Check that the correct field types and constants are used
    output = (
        "    USE constants_mod, ONLY: r_tran, r_solver, r_def, i_def\n"
        "    USE field_mod, ONLY: field_type, field_proxy_type\n"
        "    USE r_solver_field_mod, ONLY: r_solver_field_type, "
        "r_solver_field_proxy_type\n"
        "    USE r_tran_field_mod, ONLY: r_tran_field_type, "
        "r_tran_field_proxy_type\n"
        )
    assert output in code

    # Check built-in loop for 'r_def'
    output = (
        "      DO df=loop1_start,loop1_stop\n"
        "        f1_data(df) = REAL(f3_data(df), kind=r_def)\n"
        "      END DO\n"
        )
    assert output in code

    # Check built-in loop for 'r_tran'
    output = (
        "      DO df=loop0_start,loop0_stop\n"
        "        f2_data(df) = REAL(f1_data(df), kind=r_tran)\n"
        "      END DO\n"
        )
    assert output in code

    # Check built-in loop for 'r_solver'
    output = (
        "      DO df=loop2_start,loop2_stop\n"
        "        f3_data(df) = REAL(f2_data(df), kind=r_solver)\n"
        "      END DO\n"
        )
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


@pytest.mark.parametrize("kind_name", ["r_bl", "r_phys", "r_um"])
def test_real_to_real_x_lowering(monkeypatch, kind_name):
    '''
    Test that the lower_to_language_level() method works as expected.

    '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "15.10.8_real_to_real_X_builtin.f90"),
                           api=API)
    psy = PSyFactory(API,
                     distributed_memory=False).create(invoke_info)
    first_invoke = psy.invokes.invoke_list[0]
    table = first_invoke.schedule.symbol_table
    arg = first_invoke.schedule.children[0].loop_body[0].args[0]
    # Set 'f2_data' to another 'r_<prec>'
    sym_kern = table.lookup_with_tag(f"{arg.name}:data")
    monkeypatch.setattr(arg, "_precision", f"{kind_name}")
    monkeypatch.setattr(sym_kern.datatype.partial_datatype.precision,
                        "_name", f"{kind_name}")

    # Test limited code generation (no equivalent field type)
    code = str(psy.gen)

    # Due to the reverse alphabetical ordering performed by PSyclone,
    # different cases will arise depending on the substitution
    if kind_name < 'r_def':
        assert f"USE constants_mod, ONLY: r_solver, r_def, {kind_name}" in code
    elif 'r_solver' > kind_name > 'r_def':
        assert f"USE constants_mod, ONLY: r_solver, {kind_name}, r_def" in code
    else:
        assert f"USE constants_mod, ONLY: {kind_name}, r_solver, r_def" in code

    # Assert correct type is set
    assert (f"REAL(KIND={kind_name}), pointer, dimension(:) :: "
            "f2_data => null()") in code
    assert f"f2_data(df) = REAL(f1_data(df), kind={kind_name})" in code
