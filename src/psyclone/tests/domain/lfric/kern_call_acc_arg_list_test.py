# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2023-2024, Science and Technology Facilities Council.
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
# Authors:  J. Henrichs, Bureau of Meteorology
#           R. W. Ford and A. R. Porter, STFC Daresbury Lab
#           I. Kavcic, Met Office
# Modified: L. Turner, Met Office

''' This module tests the LFric KernCallAccArgList class.'''

import os

import pytest

from psyclone.core import VariablesAccessInfo
from psyclone.domain.lfric import (FunctionSpace, KernCallAccArgList,
                                   LFRicKern)
from psyclone.errors import InternalError
from psyclone.parse.algorithm import parse
from psyclone import psyGen
from psyclone.tests.utilities import get_base_path, get_invoke
from psyclone.transformations import ACCParallelTrans, ACCEnterDataTrans

# constants
TEST_API = "dynamo0.3"
BASE_PATH = get_base_path(TEST_API)


def test_acc_arg_list_cell_map(dist_mem, monkeypatch):
    '''Test the cell_map() method.'''
    # We need a LFRicKern in order to construct an instance of
    # KernCallAccArgList
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "22.0_intergrid_prolong.f90"),
                           api=TEST_API)
    psy = psyGen.PSyFactory(TEST_API,
                            distributed_memory=dist_mem).create(invoke_info)
    schedule = psy.invokes.invoke_list[0].schedule
    kern = schedule.walk(LFRicKern)[0]
    arg_list = KernCallAccArgList(kern)
    # Check that the cell_map method works as expected.
    arg_list.cell_map()
    assert "cell_map_field2" in arg_list._arglist
    # Rather than attempt to break the kernel itself, we monkeypatch the
    # args_filter routine so that it returns all args rather than just the
    # one requested.
    monkeypatch.setattr(psyGen, "args_filter", lambda args, arg_meshes: args)
    with pytest.raises(InternalError) as err:
        arg_list.cell_map()
    assert ("An LFRic intergrid kernel should have only one coarse mesh but "
            "'prolong_test_kernel_code' has 2" in str(err.value))


def test_stencil_2d():
    '''Test for the stencil_2d() and stencil_2d_unknown_extent methods.'''
    _, invoke = get_invoke("19.7_multiple_stencils.f90", TEST_API,
                           name="invoke_0_testkern_stencil_multi_type",
                           dist_mem=False)
    sched = invoke.schedule
    kern = sched.walk(LFRicKern)[0]
    arg_list = KernCallAccArgList(kern)
    arg_list.stencil_2d(kern.arguments._args[1])
    # This should result in the whole stencil dofmap being added as an arg.
    assert arg_list._arglist == ['f2_stencil_dofmap']
    arg_list.stencil_2d_unknown_extent(kern.arguments._args[1])
    # This should result in the stencil extent being added as an arg.
    assert arg_list._arglist == ['f2_stencil_dofmap', 'f2_stencil_size']


def test_fs_compulsory_field_no_cell_column():
    '''Check that the fs_compulsory_field method does nothing if the kernel
    does not iterate over cell columns.'''
    _, invoke = get_invoke("25.0_domain.f90", TEST_API,
                           name="invoke_0_testkern_domain_type",
                           dist_mem=False)
    sched = invoke.schedule
    kern = sched.walk(LFRicKern)[0]
    arg_list = KernCallAccArgList(kern)
    fspace = FunctionSpace("w3", kern.arguments)
    arg_list.fs_compulsory_field(fspace)
    assert arg_list._arglist == []


def test_fs_intergrid():
    '''Test the fs_intergrid() method.'''
    _, invoke = get_invoke("22.2_intergrid_3levels.f90", TEST_API,
                           name="invoke_0", dist_mem=False)
    sched = invoke.schedule
    kernels = sched.walk(LFRicKern)
    prolong_kern = kernels[0]
    restrict_kern = kernels[2]
    fspace = FunctionSpace("any_space_1", restrict_kern.arguments)
    arg_list = KernCallAccArgList(restrict_kern)
    arg_list.fs_intergrid(fspace)
    # For the coarse mesh we need undf and the dofmap for the column.
    assert arg_list._arglist == ['undf_aspc1_fld_m', 'map_aspc1_fld_m']
    fspace = FunctionSpace("w1", prolong_kern.arguments)
    arg_list = KernCallAccArgList(prolong_kern)
    arg_list.fs_intergrid(fspace)
    # For the fine mesh we just need the whole dofmap.
    assert arg_list._arglist == ['map_w1']


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
    assert "f1_data: READ+WRITE" in var_info
    assert "f2_data: READ" in var_info
    assert "m1_data: READ" in var_info
    assert "m2_data: READ" in var_info
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
    assert "lma_op1_local_stencil: READ" in var_info
    assert "cma_op1_cma_matrix: WRITE" in var_info


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
