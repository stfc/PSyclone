# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017-2026, Science and Technology Facilities Council.
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
# Authors: R. W. Ford, A. R. Porter, S. Siso and N. Nobre, STFC Daresbury Lab
# Modified: I. Kavcic, L. Turner, O. Brunt and J. G. Wallwork, Met Office
# Modified: A. B. G. Chalk, STFC Daresbury Lab
# -----------------------------------------------------------------------------

''' Performs py.test tests on the GlobalReduction class. '''

import pytest

from psyclone.core import AccessType
from psyclone.errors import GenerationError, InternalError
from psyclone.domain.common.psylayer import GlobalReduction
from psyclone.psyir.nodes import Literal
from psyclone.psyir.nodes.node import colored
from psyclone.psyir.symbols import INTEGER_TYPE
from psyclone.tests.utilities import get_invoke


def test_globalreduction_node_str_and_dag_name():
    '''test the node_str and dag_name methods in the GlobalReduction class. The
    simplest way to do this is to use an LFRic builtin example which contains a
    scalar Argument and then use that to construct a GlobalReduction.

    '''
    _, invoke = get_invoke("15.9.1_X_innerproduct_Y_builtin.f90",
                           api="lfric",
                           dist_mem=True, idx=0)
    gsum = None
    for child in invoke.schedule.children:
        if isinstance(child, GlobalReduction):
            gsum = child
            break
    assert gsum
    gred = GlobalReduction(gsum.scalar)
    output = gred.node_str()
    expected_output = (colored("GlobalReduction", GlobalReduction._colour) +
                       "[scalar='asum']")
    assert expected_output in output
    assert gred.dag_name == "GlobalReduction(asum)_0"


def test_globalreduction_children_validation():
    '''Test that a GlobalReduction does not accept any children.'''
    _, invoke = get_invoke("15.9.1_X_innerproduct_Y_builtin.f90", api="lfric",
                           idx=0, dist_mem=True)
    gsum = None
    for child in invoke.schedule.children:
        if isinstance(child, GlobalReduction):
            gsum = child
            break
    with pytest.raises(GenerationError) as excinfo:
        gsum.addchild(Literal("2", INTEGER_TYPE))
    assert ("Item 'Literal' can't be child 0 of 'LFRicGlobalSum'. "
            "LFRicGlobalSum is a LeafNode and doesn't accept children."
            in str(excinfo.value))


def test_globalsum_nodm_error():
    ''' Check that an instance of the GlobalReduction class raises an
    exception if it is instantiated with no distributed memory enabled
    (dm=False). We use the LFRic API to test this.

    '''
    # Get an instance of a real scalar
    _, invoke = get_invoke("1.9_single_invoke_2_real_scalars.f90",
                           api="lfric", dist_mem=False, idx=0)
    schedule = invoke.schedule
    loop = schedule.children[0]
    kernel = loop.loop_body[0]
    argument = kernel.arguments.args[0]
    with pytest.raises(GenerationError) as err:
        _ = GlobalReduction(argument)
    assert ("Refusing to create a GlobalReduction object because "
            "distributed memory is not enabled (dm=False)."
            in str(err.value))


def test_globalreduction_unsupported_argument():
    ''' Check that an instance of the GlobalReduction class raises an
    exception for an unsupported argument type. '''
    # Get an instance of a non-scalar argument
    _, invoke = get_invoke("1.6.1_single_invoke_1_int_scalar.f90",
                           api="lfric", dist_mem=True, idx=0)
    schedule = invoke.schedule
    loop = schedule.children[4]
    kernel = loop.loop_body[0]
    argument = kernel.arguments.args[0]
    with pytest.raises(InternalError) as err:
        _ = GlobalReduction(argument)
    assert ("GlobalReduction.init(): the 'scalar' argument should be a "
            "scalar but found argument of type 'gh_field'." in str(err.value))


def test_globalreduction_arg():
    ''' Check that the global-reduction argument is defined as gh_readwrite and
    points to the GlobalReduction node '''
    _, invoke = get_invoke("15.14.3_sum_setval_field_builtin.f90",
                           api="lfric", idx=0, dist_mem=True)
    schedule = invoke.schedule
    glob_sum = schedule.children[2]
    glob_sum_arg = glob_sum.scalar
    assert glob_sum_arg.access == AccessType.READWRITE
    assert glob_sum_arg.call == glob_sum


def test_globalreduction_args():
    '''Test that the globalreduction class args method returns the appropriate
    argument '''
    _, invoke = get_invoke("15.14.3_sum_setval_field_builtin.f90",
                           api="lfric", dist_mem=True, idx=0)
    schedule = invoke.schedule
    global_sum = schedule.children[2]
    assert len(global_sum.args) == 1
    assert global_sum.args[0] == global_sum.scalar


def test_globalreduction_reference_accesses():
    '''Test the reference_accesses method returns the appropriate
    information for a GlobalReduction.'''
    _, invoke = get_invoke("15.14.3_sum_setval_field_builtin.f90",
                           api="lfric", dist_mem=True, idx=0)
    schedule = invoke.schedule
    global_sum = schedule.children[2]
    vam = global_sum.reference_accesses()
    if len(vam.all_data_accesses) != 1:
        pytest.xfail(reason="Issue #3346: the scalar to be updated by a "
                     "GlobalReduction is held in a bespoke '_scalar' "
                     "property.")
    assert list(vam) == ["FIXME"]
