# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017-2021, Science and Technology Facilities Council.
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
# Modified I. Kavcic, Met Office
# Modified R. W. Ford, STFC Daresbury Lab

''' This module tests the support for built-in operations in the Dynamo 0.3 API
    using pytest. Currently all built-in operations are 'pointwise' in that
    they iterate over DOFs. However this may change in the future. '''

# imports
from __future__ import absolute_import, print_function
import os
import pytest

from psyclone.parse.algorithm import parse
from psyclone.parse.utils import ParseError
from psyclone.psyGen import PSyFactory
from psyclone.errors import GenerationError, InternalError
from psyclone.configuration import Config
from psyclone import dynamo0p3_builtins

from psyclone.tests.lfric_build import LFRicBuild

# Constants
BASE_PATH = os.path.join(
    os.path.dirname(os.path.dirname(os.path.dirname(
        os.path.abspath(__file__)))),
    "test_files", "dynamo0p3")
# The PSyclone API under test
API = "dynamo0.3"


@pytest.fixture(scope="module", autouse=True)
def setup():
    '''Make sure that all tests here use LFRic (Dynamo0.3) as API.'''
    Config.get().api = "dynamo0.3"


# ------------- Adding (scaled) integer fields ------------------------------ #


def test_int_X_plus_Y(tmpdir, monkeypatch, annexed, dist_mem):
    ''' Test that 1) the str method of LFRicIntXPlusYKern returns the expected
    string and 2) we generate correct code for the built-in Z = X + Y
    where X and Y are integer fields. Also check that we generate correct bounds
    when Config.api_conf(API)._compute_annexed_dofs is False and True.

    '''
    api_config = Config.get().api_conf(API)
    monkeypatch.setattr(api_config, "_compute_annexed_dofs", annexed)
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "15.21.1_int_X_plus_Y_builtin.f90"),
                           api=API)
    psy = PSyFactory(API,
                     distributed_memory=dist_mem).create(invoke_info)
    # Test string method
    first_invoke = psy.invokes.invoke_list[0]
    kern = first_invoke.schedule.children[0].loop_body[0]
    assert str(kern) == "Built-in: Add integer-valued fields"
    # Test code generation
    code = str(psy.gen)

    assert LFRicBuild(tmpdir).code_compiles(psy)

    # Check for the correct field data declarations
    output = (
        "      TYPE(integer_field_type), intent(in) :: f3, f1, f2\n"
        "      INTEGER df\n"
        "      TYPE(integer_field_proxy_type) f3_proxy, f1_proxy, f2_proxy\n")
    assert output in code

    if not dist_mem:
        # The value of _compute_annexed_dofs should make no difference
        output = (
            "      f3_proxy = f3%get_proxy()\n"
            "      f1_proxy = f1%get_proxy()\n"
            "      f2_proxy = f2%get_proxy()\n"
            "      !\n"
            "      ! Initialise number of DoFs for aspc1_f3\n"
            "      !\n"
            "      undf_aspc1_f3 = f3_proxy%vspace%get_undf()\n"
            "      !\n"
            "      ! Call our kernels\n"
            "      !\n"
            "      DO df=1,undf_aspc1_f3\n"
            "        f3_proxy%data(df) = f1_proxy%data(df) + "
            "f2_proxy%data(df)\n"
            "      END DO")
        assert output in code
    else:
        output_dm_2 = (
            "      !\n"
            "      ! Call kernels and communication routines\n"
            "      !\n"
            "      DO df=1,f3_proxy%vspace%get_last_dof_annexed()\n"
            "        f3_proxy%data(df) = f1_proxy%data(df) + "
            "f2_proxy%data(df)\n"
            "      END DO\n"
            "      !\n"
            "      ! Set halos dirty/clean for fields modified in the "
            "above loop\n"
            "      !\n"
            "      CALL f3_proxy%set_dirty()\n"
            "      !\n")
        if not annexed:
            # Only compute owned dofs if _compute_annexed_dofs is False
            output_dm_2 = output_dm_2.replace("annexed", "owned")
        assert output_dm_2 in code
