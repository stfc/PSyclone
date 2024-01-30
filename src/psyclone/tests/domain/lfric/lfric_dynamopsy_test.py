# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2021-2024, Science and Technology Facilities Council.
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
# Modifications: A. R. Porter, STFC Daresbury Lab


'''This module tests the DynamoPSy class, currently located within the
dynamo0.3.py file.'''

from collections import OrderedDict
import os

from psyclone.configuration import Config
from psyclone.domain.lfric import LFRicConstants
from psyclone.dynamo0p3 import DynamoPSy, LFRicInvokes
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
    assert isinstance(dynamo_psy._invokes, LFRicInvokes)
    infrastructure_modules = dynamo_psy._infrastructure_modules
    assert isinstance(infrastructure_modules, OrderedDict)
    assert list(infrastructure_modules["constants_mod"]) == ["i_def"]
    const = LFRicConstants()
    names = set(item["module"] for item in const.DATA_TYPE_MAP.values())
    assert len(names)+1 == len(infrastructure_modules)
    for module_name in names:
        assert infrastructure_modules[module_name] == set()


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
    assert "f1_data(df) = 0.0\n" in result
    # 2: Literal kind value is declared (trying with two cases to check)
    for kind_name in ["r_solver", "r_tran"]:
        invoke_info.calls[0].kcalls[0].args[1]._text = f"0.0_{kind_name}"
        invoke_info.calls[0].kcalls[0].args[1]._datatype = ("real", kind_name)
        dynamo_psy = DynamoPSy(invoke_info)
        result = str(dynamo_psy.gen).lower()
        assert f"use constants_mod, only: {kind_name}, r_def, i_def" in result
        assert f"f1_data(df) = 0.0_{kind_name}" in result


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
    '''Check that the infrastructure_modules() method of DynamoPSy (which
    is implemented as a property) behaves as expected. In this case we
    check that it returns the values set up in the initialisation of
    an instance of DynamoPSy.

    '''
    dynamo_psy = DynamoPSy(DummyInvokeInfo())
    assert (dynamo_psy.infrastructure_modules is
            dynamo_psy._infrastructure_modules)


def test_dynamopsy_gen_no_invoke():
    '''Check that the gen() method of DynamoPSy behaves as expected for a
    minimal psy-layer when the algorithm layer does not contain any
    invoke calls.

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


def test_dynamopsy_gen(monkeypatch):
    '''Check that the gen() method of DynamoPSy behaves as expected when
    generating a psy-layer from an algorithm layer containing invoke
    calls. Simply check that the PSy-layer code for the invoke call is
    generated as we check the rest of the generation for the
    DynamoPSy() gen() method in the previous test.

    '''
    # Since we're testing the DynamoPSy constructor directly, we have to
    # monkeypatch the Config object in order to guarantee that distributed
    # memory is enabled.
    monkeypatch.setattr(Config.get(), "_distributed_mem", True)

    _, invoke_info = parse(
        os.path.join(
            BASE_PATH, "15.14.4_builtin_and_normal_kernel_invoke.f90"),
        api="dynamo0.3")
    # Make sure we have distributed memory enabled, otherwise we can
    # get errors in parallel builds if a previous jobs leave this
    # to be false.
    config = Config.get()
    config.distributed_memory = True
    dynamo_psy = DynamoPSy(invoke_info)
    result = str(dynamo_psy.gen)
    assert (
        "      DO cell=loop0_start,loop0_stop\n"
        "        !\n"
        "        CALL testkern_code(nlayers, ginger, f1_data, "
        "f2_data, m1_data, m2_data, ndf_w1, undf_w1, "
        "map_w1(:,cell), ndf_w2, undf_w2, map_w2(:,cell), ndf_w3, undf_w3, "
        "map_w3(:,cell))\n"
        "      END DO\n"
        "      !\n"
        "      ! Set halos dirty/clean for fields modified in the above loop\n"
        "      !\n"
        "      CALL f1_proxy%set_dirty()\n"
        "      !\n"
        "      DO df=loop1_start,loop1_stop\n"
        "        f1_data(df) = 0.0_r_def\n"
        "      END DO\n" in result)
