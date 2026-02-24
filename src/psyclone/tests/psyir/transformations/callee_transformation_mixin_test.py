# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2026, Science and Technology Facilities Council.
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
# -----------------------------------------------------------------------------

"""
Contains py.test tests for the KernelTransformationMixin class.

"""

import pytest

from psyclone.domain.common.transformations import KernelModuleInlineTrans
from psyclone.domain.common.transformations.kernel_transformation_mixin import\
    KernelTransformationMixin
from psyclone.psyGen import CodedKern
from psyclone.psyir.nodes import Call, Container
from psyclone.psyir.symbols import RoutineSymbol
from psyclone.psyir.transformations.transformation_error import (
    TransformationError)
from psyclone.tests.utilities import get_invoke
from psyclone.transformations import Transformation


class MyTransform(Transformation, KernelTransformationMixin):
    '''
    A dummy transformation for testing the KernelTransformationMixin.
    '''
    def apply(self, node):
        '''
        '''


def test_check_kernel_is_local():
    '''
    Tests for the _check_kernel_is_local method.

    '''
    my_trans = MyTransform()
    # Just returns None for anything other than a CodedKern
    assert my_trans._check_kernel_is_local(None) is None
    a_call = Call.create(RoutineSymbol("sub"))
    assert my_trans._check_kernel_is_local(a_call) is None
    psy, invoke = get_invoke("single_invoke_three_kernels.f90", api="gocean",
                             idx=0)
    kern = invoke.schedule.walk(CodedKern)[0]
    with pytest.raises(TransformationError) as err:
        my_trans._check_kernel_is_local(kern)
    assert ("Cannot transform this Kernel call to 'compute_cu_code' because "
            "its implementation resides in a different source file. Apply "
            "KernelModuleInlineTrans first to bring it into this module."
            in str(err.value))
    mod_inline_trans = KernelModuleInlineTrans()
    mod_inline_trans.apply(kern)
    # Check should now pass.
    my_trans._check_kernel_is_local(kern)
    # We now want to test the checks for edge cases.
    sym = kern.scope.symbol_table.lookup(kern.name)
    # Find the newly-inlined kernel routine.
    container = invoke.schedule.ancestor(Container)
    routine = container.find_routine_psyir(kern.name, allow_private=True)
    # Remove the routine but ensure its symbol can still be found.
    routine.detach()
    container.symbol_table.add(sym)
    with pytest.raises(TransformationError) as err:
        my_trans._check_kernel_is_local(kern)
    assert ("ancestor Container does not contain a Routine named "
            "'compute_cu_code'" in str(err.value))
    # Detach the invoke routine from its parent Container but make sure
    # we copy in the RoutineSymbol to get past the first check
    invoke.schedule.detach()
    invoke.schedule.symbol_table.add(sym)
    with pytest.raises(TransformationError) as err:
        my_trans._check_kernel_is_local(kern)
    assert "there is no ancestor Container in which to look" in str(err.value)
