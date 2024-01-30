# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2021-2024, Science and Technology Facilities Council
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
from psyclone.errors import InternalError
from psyclone.psyir.nodes import Routine
from psyclone.psyir.symbols import (RoutineSymbol, DataTypeSymbol,
                                    StructureType, SymbolTable, Symbol,
                                    ArgumentInterface, UnresolvedInterface)


def test_lfrickernelfunctor():
    '''Test that an instance of LFRicKernelFunctor class can be created.

    '''
    routine = DataTypeSymbol("hello", StructureType())
    lbc = LFRicKernelFunctor(routine)
    assert isinstance(lbc, LFRicKernelFunctor)
    assert lbc._text_name == "LFRicKernelFunctor"


def test_lfricbuiltinfunctor():
    '''Test that an instance of LFRicBuiltinFunctor class can be created.

    '''
    routine = DataTypeSymbol("hello", StructureType())
    lbc = LFRicBuiltinFunctor(routine)
    assert isinstance(lbc, LFRicBuiltinFunctor)
    assert lbc._text_name == "LFRicBuiltinFunctor"


def test_functor_factory_singleton():
    '''Check that the functor factory is a singleton.'''
    factory = LFRicBuiltinFunctorFactory.get()
    factory2 = LFRicBuiltinFunctorFactory.get()
    assert factory2 is factory
    with pytest.raises(ValueError) as err:
        _ = LFRicBuiltinFunctorFactory()
    assert ("Only one instance of LFRicBuiltinFunctorFactory is permitted. "
            "Use the get() method to access it" in str(err.value))


def test_functor_factory_create_classes():
    '''Check that the _create_classes() method creates the dictionary of
    functor classes just once.
    '''
    factory = LFRicBuiltinFunctorFactory.get()
    factory._create_classes()
    assert "setval_x" in factory._builtin_functor_map
    setval_x = factory._get_builtin_class("setval_x")
    factory._create_classes()
    assert setval_x is factory._builtin_functor_map["setval_x"]
    factory._builtin_functor_map = {}


def test_functor_factory_get_builtin_class():
    '''Test that the _get_builtin_class() method of the factory causes the
    whole list of classes to be constructed.
    '''
    factory = LFRicBuiltinFunctorFactory.get()
    factory._builtin_functor_map = {}
    _ = factory._get_builtin_class("setval_x")
    assert "setval_c" in factory._builtin_functor_map


@pytest.mark.parametrize("name", list(BUILTIN_MAP.keys()))
def test_lfric_functor_factory(name):
    '''Test that the LFricBuiltinFunctorFactory can create a class for
    every supported builtin. These that instances of those classes
    can be lowered and that an appropriate symbol is added
    to/removed from the supplied table. '''
    factory = LFRicBuiltinFunctorFactory.get()
    sched = Routine("my_prog", is_program=True)
    table = sched.symbol_table
    funky = factory.create(name, table, [])
    funky2 = factory.create(name, table, [])
    assert isinstance(funky, LFRicBuiltinFunctor)
    sym = table.lookup(name)
    assert isinstance(sym, DataTypeSymbol)
    routine = RoutineSymbol("hello")
    call = LFRicAlgorithmInvokeCall.create(routine, [funky, funky2], 0)
    sched.addchild(call)
    funky.lower_to_language_level()
    assert name not in table._symbols
    # Lowering the second built-in should not cause problems, even though the
    # associated symbol will have been removed when the first builtin was
    # lowered.
    funky2.lower_to_language_level()


def test_lfricbuiltinfunctor_sym_specialise():
    '''Test that creating an LFRicBuiltinFunctor specialises any
    existing generic symbol that has the name of the builtin.
    '''
    factory = LFRicBuiltinFunctorFactory.get()
    sched = Routine("my_prog", is_program=True)
    table = sched.symbol_table
    table.new_symbol("setval_x", symbol_type=Symbol,
                     interface=UnresolvedInterface())
    factory.create("setval_x", table, [])
    sym = table.lookup("setval_x")
    # The symbol should have been specialised.
    assert isinstance(sym, DataTypeSymbol)


def test_lfric_functor_factory_invalid_name():
    '''Check that the factory raises the expected error if an unrecognised
    name is supplied or the name clashes with an entry in the symbol table.'''
    table = SymbolTable()
    factory = LFRicBuiltinFunctorFactory.get()
    with pytest.raises(KeyError):
        factory.create("not-a-builtin", table, [])
    table.new_symbol("setval_c", interface=ArgumentInterface())
    with pytest.raises(InternalError) as err:
        factory.create("setval_c", table, [])
    assert ("A symbol with the same name as builtin 'setval_c' exists but has "
            "an interface of 'Argument(Access.UNKNOWN)' instead of being "
            "unresolved" in str(err.value))
    table.new_symbol("setval_X", symbol_type=RoutineSymbol)
    with pytest.raises(InternalError) as err:
        factory.create("setval_X", table, [])
    assert ("PSyclone internal error: A symbol with the same name as builtin "
            "'setval_X' exists but it is a 'RoutineSymbol' and not a "
            "DataTypeSymbol" in str(err.value))
