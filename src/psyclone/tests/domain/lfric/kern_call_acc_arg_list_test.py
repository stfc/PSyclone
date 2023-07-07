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
# Authors: J. Henrichs, Bureau of Meteorology
#          R. W. Ford and A. R. Porter, STFC Daresbury Lab
#          I. Kavcic, Met Office

''' This module tests the LFric KernCallAccArgList class.'''

import os

import pytest

from psyclone.core import VariablesAccessInfo
from psyclone.domain.lfric import (KernCallAccArgList, LFRicConstants,
                                   LFRicSymbolTable, LFRicTypes)
from psyclone.errors import GenerationError, InternalError
from psyclone.dynamo0p3 import DynKern
from psyclone.parse.algorithm import parse
from psyclone import psyGen
from psyclone.psyir.nodes import Literal, Loop, Reference, UnaryOperation
from psyclone.psyir.symbols import ArrayType, ScalarType
from psyclone.tests.utilities import get_base_path, get_invoke
from psyclone.transformations import ACCParallelTrans, ACCEnterDataTrans

# constants
BASE_PATH = os.path.join(os.path.dirname(os.path.dirname(os.path.dirname(
    os.path.abspath(__file__)))),
                         "test_files", "dynamo0p3")
TEST_API = "dynamo0.3"


def test_acc_arg_list_single_coarse_mesh(dist_mem, monkeypatch):
    '''Tests that an exception is raised as expected if a kernel has more than
    one coarse mesh.
    '''
    # We need a DynKern in order to construct an instance of KernCallAccArgList
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "22.0_intergrid_prolong.f90"),
                           api=TEST_API)
    psy = psyGen.PSyFactory(TEST_API,
                            distributed_memory=dist_mem).create(invoke_info)
    schedule = psy.invokes.invoke_list[0].schedule
    kern = schedule.walk(DynKern)[0]
    arg_list = KernCallAccArgList(kern)
    # Rather than attempt to break the kernel itself, we monkeypatch the
    # args_filter routine so that it returns all args rather than just the
    # one requested.
    monkeypatch.setattr(psyGen, "args_filter", lambda args, arg_meshes: args)
    with pytest.raises(InternalError) as err:
        arg_list.cell_map()
    assert ("An LFRic intergrid kernel should have only one coarse mesh but "
            "'prolong_test_kernel_code' has 2" in str(err.value))


def test_lfric_acc():
    '''Check variable usage detection when OpenACC is used.

    '''
    # Use the OpenACC transforms to enclose the kernels
    # with OpenACC directives.
    acc_par_trans = ACCParallelTrans()
    acc_enter_trans = ACCEnterDataTrans()
    _, invoke = get_invoke("1_single_invoke.f90", "dynamo0.3",
                           name="invoke_0_testkern_type", dist_mem=False)
    sched = invoke.schedule
    acc_par_trans.apply(sched.children)
    acc_enter_trans.apply(sched)

    # Find the first kernel:
    kern = invoke.schedule.walk(psyGen.CodedKern)[0]
    create_acc_arg_list = KernCallAccArgList(kern)
    var_accesses = VariablesAccessInfo()
    create_acc_arg_list.generate(var_accesses=var_accesses)
    var_info = str(var_accesses)
    assert "f1: READ+WRITE" in var_info
    assert "f2: READ" in var_info
    assert "m1: READ" in var_info
    assert "m2: READ" in var_info
    assert "undf_w1: READ" in var_info
    assert "map_w1: READ" in var_info
    assert "undf_w2: READ" in var_info
    assert "map_w2: READ" in var_info
    assert "undf_w3: READ" in var_info
    assert "map_w3: READ" in var_info


def test_lfric_acc_operator():
    '''Check variable usage detection when OpenACC is used with
    a kernel that uses an operator.

    '''
    # Use the OpenACC transforms to enclose the kernels
    # with OpenACC directives.
    acc_par_trans = ACCParallelTrans()
    acc_enter_trans = ACCEnterDataTrans()
    _, invoke = get_invoke("20.0_cma_assembly.f90", "dynamo0.3",
                           idx=0, dist_mem=False)
    sched = invoke.schedule
    acc_par_trans.apply(sched.children)
    acc_enter_trans.apply(sched)

    # Find the first kernel:
    kern = invoke.schedule.walk(psyGen.CodedKern)[0]
    create_acc_arg_list = KernCallAccArgList(kern)
    var_accesses = VariablesAccessInfo()
    create_acc_arg_list.generate(var_accesses=var_accesses)
    var_info = str(var_accesses)
    assert "lma_op1_proxy%ncell_3d: READ" in var_info
    assert "lma_op1_proxy%local_stencil: READ" in var_info
    assert "cma_op1_matrix: WRITE" in var_info


def test_lfric_stencil():
    '''Check variable usage detection when OpenACC is used with a
    kernel that uses a stencil.

    '''
    # Use the OpenACC transforms to create the required kernels
    acc_par_trans = ACCParallelTrans()
    acc_enter_trans = ACCEnterDataTrans()
    _, invoke = get_invoke("14.4_halo_vector.f90", "dynamo0.3",
                           idx=0, dist_mem=False)
    sched = invoke.schedule
    acc_par_trans.apply(sched.children)
    acc_enter_trans.apply(sched)

    # Find the first kernel:
    kern = invoke.schedule.walk(psyGen.CodedKern)[0]
    create_acc_arg_list = KernCallAccArgList(kern)
    var_accesses = VariablesAccessInfo()
    create_acc_arg_list.generate(var_accesses=var_accesses)
    var_info = str(var_accesses)
    assert "f1: READ+WRITE" in var_info
    assert "f2: READ" in var_info
    assert "f2_stencil_dofmap: READ" in var_info
