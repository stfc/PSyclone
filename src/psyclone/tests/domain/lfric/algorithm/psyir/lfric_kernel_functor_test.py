# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2021-2022, Science and Technology Facilities Council
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
# Author R. W. Ford, STFC Daresbury Lab
# Modified: S. Siso and A. R. Porter, STFC Daresbury Lab

'''Module containing tests for the LFRicBuiltinFunctor and LFRicKernelFunctor
 LFRic algorithm-layer-specific nodes.

'''
import pytest

from psyclone.domain.lfric.algorithm.psyir import (
    LFRicAlgorithmInvokeCall, LFRicKernelFunctor,
    LFRicBuiltinFunctor, LFRicBuiltinFunctorFactory)
from psyclone.domain.lfric.lfric_builtins import BUILTIN_MAP
from psyclone.psyir.frontend.fortran import FortranReader
from psyclone.psyir.nodes import Routine
from psyclone.psyir.symbols import (RoutineSymbol, DataTypeSymbol,
                                    StructureType, REAL_TYPE)


def test_lfricbuiltinfunctor():
    '''test that an instance of LFRicBuiltinFunctor class can be created.

    '''
    routine = DataTypeSymbol("hello", StructureType())
    lbc = LFRicBuiltinFunctor(routine)
    assert isinstance(lbc, LFRicBuiltinFunctor)
    assert lbc._text_name == "LFRicBuiltinFunctor"


def test_lfrickernelfunctor():
    '''test that an instance of LFRicKernelFunctor class can be created.

    '''
    routine = DataTypeSymbol("hello", StructureType())
    lbc = LFRicKernelFunctor(routine)
    assert isinstance(lbc, LFRicKernelFunctor)
    assert lbc._text_name == "LFRicKernelFunctor"


@pytest.mark.parametrize("name", list(BUILTIN_MAP.keys()))
def test_lfric_auto_gen_builtin_functor(name):
    ''' Test that the auto-generated BuiltIn functor classes can be created
    and lowered and that an appropriate symbol is added to/removed from the
    supplied table. '''
    lfric_functor = LFRicBuiltinFunctorFactory()
    sched = Routine("my_prog", is_program=True)
    table = sched.symbol_table
    funky = lfric_functor.create(name, table, [])
    funky2 = lfric_functor.create(name, table, [])
    assert isinstance(funky, LFRicBuiltinFunctor)
    sym = table.lookup(name)
    assert isinstance(sym, DataTypeSymbol)
    routine = RoutineSymbol("hello")
    call = LFRicAlgorithmInvokeCall.create(routine, [funky, funky2], 0)
    sched.addchild(call)
    funky.lower_to_language_level()
    assert name not in table._symbols
    # Lowering the second built-in should not cause problems.
    funky2.lower_to_language_level()
