# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2021-2022, Science and Technology Facilities Council.
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
# Author: R. W. Ford, STFC Daresbury Lab

'''This module tests the DynamoPSy class, currently located within the
dynamo0.3.py file.'''

from collections import OrderedDict
import os

from psyclone.dynamo0p3 import DynamoPSy, DynamoInvokes
from psyclone.parse.algorithm import parse
from psyclone.psyGen import PSy

BASE_PATH = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                         os.pardir, os.pardir, "test_files", "dynamo0p3")


class DummyInvokeInfo():
    '''A dummy class that contains the minimum information required to
    allow DynamoPSy to be called.

    :param str name: the name to use for this dummy invoke. Defaults \
        to an empty string.

    '''
    def __init__(self, name=""):
        self.name = name
        self.calls = []


def test_dynamopsy():
    ''' Check that an instance of DynamoPSy can be created successfully.'''

    invoke_info = DummyInvokeInfo()
    dynamo_psy = DynamoPSy(invoke_info)
    assert isinstance(dynamo_psy, DynamoPSy)
    assert issubclass(DynamoPSy, PSy)
    assert isinstance(dynamo_psy._invokes, DynamoInvokes)
    infrastructure_modules = dynamo_psy._infrastructure_modules
    assert len(infrastructure_modules) == 5
    assert isinstance(infrastructure_modules, OrderedDict)
    assert infrastructure_modules["constants_mod"] == ["i_def"]
    assert infrastructure_modules["field_mod"] == set()
    assert infrastructure_modules["r_solver_field_mod"] == set()
    assert infrastructure_modules["integer_field_mod"] == set()
    assert infrastructure_modules["operator_mod"] == set()


def test_dynamopsy_kind():
    '''Check that an instance of DynamoPSy captures any precision (kind
    values) for literals.

    '''
    # 1: no literal kind value gives the default r_def (even though it
    # is not required).
    _, invoke_info = parse(os.path.join(
        BASE_PATH, "15.12.3_single_pointwise_builtin.f90"), api="dynamo0.3")
    dynamo_psy = DynamoPSy(invoke_info)
    result = str(dynamo_psy.gen)
    assert "USE constants_mod, ONLY: r_def, i_def" in result
    assert "f1_proxy%data(df) = 0.0\n" in result
    # 2: Literal kind value is declared
    invoke_info.calls[0].kcalls[0].args[1]._text = "0.0_r_solver"
    invoke_info.calls[0].kcalls[0].args[1]._datatype = ("real", "r_solver")
    dynamo_psy = DynamoPSy(invoke_info)
    result = str(dynamo_psy.gen)
    assert "USE constants_mod, ONLY: r_solver, i_def" in result
    assert "f1_proxy%data(df) = 0.0_r_solver" in result


def test_dynamopsy_names():
    '''Check that the name() and orig_name() methods of DynamoPSy behave as
    expected.

    '''
    supplied_name = "hello"
    invoke_info = DummyInvokeInfo(name=supplied_name)
    dynamo_psy = DynamoPSy(invoke_info)
    assert dynamo_psy.name == supplied_name + "_psy"
    assert dynamo_psy.orig_name == supplied_name


def test_dynamopsy_inf_modules():
    '''Check that the infrastructure_modules() method of DynamoPSy behaves
    as expected.

    '''
    dynamo_psy = DynamoPSy(DummyInvokeInfo())
    assert (dynamo_psy.infrastructure_modules is
            dynamo_psy._infrastructure_modules)


def test_dynamopsy_gen_no_invoke():
    '''Check that the gen() method of DynamoPSy behaves as expected for a
    minimal psy-layer with no invoke calls.

    '''
    expected_result = (
        "  MODULE hello_psy\n"
        "    USE constants_mod, ONLY: i_def\n"
        "    IMPLICIT NONE\n"
        "    CONTAINS\n"
        "  END MODULE hello_psy")
    dynamo_psy = DynamoPSy(DummyInvokeInfo(name="hello"))
    result = dynamo_psy.gen
    assert str(result) == expected_result


def test_dynamopsy_gen():
    '''Check that the gen() method of DynamoPSy behaves as expected for a
    psy-layer with invoke calls. Simply check the invoke calls are
    generated as we check the rest of the generation for the
    DynamoPSy() gen() method in the previous test.

    '''
    _, invoke_info = parse(
        os.path.join(
            BASE_PATH, "15.14.4_builtin_and_normal_kernel_invoke.f90"),
        api="dynamo0.3")
    dynamo_psy = DynamoPSy(invoke_info)
    result = str(dynamo_psy.gen)
    assert (
        "      DO cell=1,mesh%get_last_halo_cell(1)\n"
        "        !\n"
        "        CALL testkern_code(nlayers, ginger, f1_proxy%data, "
        "f2_proxy%data, m1_proxy%data, m2_proxy%data, ndf_w1, undf_w1, "
        "map_w1(:,cell), ndf_w2, undf_w2, map_w2(:,cell), ndf_w3, undf_w3, "
        "map_w3(:,cell))\n"
        "      END DO\n"
        "      !\n"
        "      ! Set halos dirty/clean for fields modified in the above loop\n"
        "      !\n"
        "      CALL f1_proxy%set_dirty()\n"
        "      !\n"
        "      DO df=1,f1_proxy%vspace%get_last_dof_owned()\n"
        "        f1_proxy%data(df) = 0.0_r_def\n"
        "      END DO\n" in result)
