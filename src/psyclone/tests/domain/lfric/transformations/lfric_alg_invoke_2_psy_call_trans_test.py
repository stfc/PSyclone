# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2022, Science and Technology Facilities Council.
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

''' Module containing pytest unit tests for the LFRicAlgInvoke2PSyCallTrans
transformation.

'''
import pytest

from psyclone.domain.common.algorithm import AlgorithmInvokeCall
from psyclone.domain.lfric.transformations import LFRicAlgTrans
from psyclone.domain.lfric.transformations import LFRicAlgInvoke2PSyCallTrans
from psyclone.psyir.nodes import Call
from psyclone.psyir.symbols import RoutineSymbol, DataTypeSymbol, \
    StructureType, Symbol, REAL_TYPE, DataSymbol, INTEGER_TYPE
from psyclone.psyir.transformations import TransformationError


def test_lfai2psycall_validate():
    ''' Test the validate() method of the LFRicAlgInvoke2PSyCallTrans
    class. '''
    trans = LFRicAlgInvoke2PSyCallTrans()
    with pytest.raises(TransformationError) as err:
        trans.validate(None)
    assert ("The supplied call argument should be an "
            "`LFRicAlgorithmInvokeCall` node but found 'NoneType'"
            in str(err.value))


def test_lfai2psycall_apply(fortran_reader):
    ''' Test the apply() method of the LFRicAlgInvoke2PSyCallTrans
    class. '''
    code = (
        "subroutine alg1()\n"
        "  use kern_mod, only : kern\n"
        "  use field_mod, only : field_type\n"
        "  type(field_type) :: field1\n"
        "  call invoke(kern(field1))\n"
        "end subroutine alg1\n")
    psyir = fortran_reader.psyir_from_source(code)
    alg_trans = LFRicAlgTrans()
    alg_trans.apply(psyir)
    aic = psyir.walk(AlgorithmInvokeCall)[0]
    trans = LFRicAlgInvoke2PSyCallTrans()
    trans.apply(aic)
    assert psyir.walk(AlgorithmInvokeCall) == []
    calls = psyir.walk(Call)
    assert len(calls) == 1
    assert isinstance(calls[0].routine, RoutineSymbol)
    assert calls[0].routine.name == "invoke_0_kern"
