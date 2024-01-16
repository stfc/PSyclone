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

'''Module containing tests for the LFRicAlgorithmInvokeCall
algorithm-layer-specific node. The tests include translation of PSyIR
to LFRic Algorithm PSyIR and from LFRic Algorithm PSyIR to language-level
PSyIR.

'''
import pytest

from psyclone.domain.lfric.algorithm.psyir import (
    LFRicAlgorithmInvokeCall, LFRicKernelFunctor,
    LFRicBuiltinFunctor)
from psyclone.domain.lfric.transformations import LFRicAlgTrans
from psyclone.psyir.frontend.fortran import FortranReader
from psyclone.psyir.symbols import RoutineSymbol, DataTypeSymbol, \
    StructureType, REAL_TYPE


def create_alg_psyir(code):
    '''Utility to create an LFRic Algorithm PSyIR tree from Fortran
    code.

    :param str code: Fortran algorithm code encoded as a string.

    :returns: LFRic Algorithm PSyIR tree representing the Fortran \
        code.
    :rtype: :py:class:`psyclone.psyir.nodes.Node`

    '''
    fortran_reader = FortranReader()
    psyir = fortran_reader.psyir_from_source(code)
    alg_trans = LFRicAlgTrans()
    alg_trans.apply(psyir)

    return psyir


def test_lfricalgorithminvokecall():
    '''Check that an instance of LFRicAlgorithmInvokeCall can be
    created.

    '''
    routine = RoutineSymbol("hello")
    index = 2
    call = LFRicAlgorithmInvokeCall(routine, index)
    assert call.routine is routine
    assert call._index == index
    assert call._children_valid_format == "[LFRicFunctor]*"
    assert call._text_name == "LFRicAlgorithmInvokeCall"


def test_validate_child():
    '''Check that the _validate_child method behaves as expected.'''

    lfric_kernel_functor = LFRicKernelFunctor(
        DataTypeSymbol("dummy1", REAL_TYPE))
    lfric_builtin_functor = LFRicBuiltinFunctor(
        DataTypeSymbol("dummy2", REAL_TYPE))
    assert LFRicAlgorithmInvokeCall._validate_child(0, lfric_kernel_functor)
    assert LFRicAlgorithmInvokeCall._validate_child(1, lfric_builtin_functor)
    assert not LFRicAlgorithmInvokeCall._validate_child(0, "Invalid")


class DummySubClass(LFRicAlgorithmInvokeCall):
    '''A dummy subclass of LFRicAlgorithmInvokeCall used for testing the
    behaviour of the create method in LFRicAlgorithmInvokeCall.

    '''


@pytest.mark.parametrize("cls", [LFRicAlgorithmInvokeCall, DummySubClass])
def test_lfricalgorithminvokecall_create(cls):
    '''Check that the LFRicAlgorithmInvokeCall create method creates the
    expected object.

    '''
    routine = RoutineSymbol("hello")
    klc = LFRicKernelFunctor.create(DataTypeSymbol("arg", StructureType()), [])
    call = cls.create(routine, [klc], 0, name="describing_an_invoke")
    assert call._name == "describing_an_invoke"
    assert call.routine is routine
    # pylint: disable=unidiomatic-typecheck
    assert type(call) is cls
    assert len(call.children) == 1
    assert call.children[0] == klc


def test_lfricalgorithminvokecall_create_noname():
    '''Check that the LFRicAlgorithmInvokeCall create method sets
    name to None if it is not provided.

    '''
    routine = RoutineSymbol("hello")
    call = LFRicAlgorithmInvokeCall.create(routine, [], 0)
    assert call._name is None


def test_aic_defcontainerrootname():
    '''Check that _def_container_root_name returns the expected value'''
    code = (
        "subroutine alg1()\n"
        "  use kern_mod, only : kern\n"
        "  use field_mod, only : field_type\n"
        "  type(field_type) :: field1\n"
        "  call invoke(kern(field1))\n"
        "end subroutine alg1\n")
    psyir = create_alg_psyir(code)
    invoke = psyir.children[0][0]
    assert isinstance(invoke, LFRicAlgorithmInvokeCall)
    routine_node = psyir.children[0]
    name = invoke._def_container_root_name(routine_node)
    assert name == "alg1_psy"


@pytest.mark.parametrize(
    "orig_string,new_string,expected_name",
    [("", "", "invoke_0_kern"),
     ("kern(field1)", "setval_c(field1, 0.0)", "invoke_0")])
def test_aic_defroutinerootname_single(
        orig_string, new_string, expected_name):
    '''Check that _def_routine_root_name returns the expected
    values. Test for when there is a single user kernel or builtin in
    an invoke, as the output will differ.

    '''
    code = (
        "subroutine alg1()\n"
        "  use kern_mod, only : kern\n"
        "  use field_mod, only : field_type\n"
        "  type(field_type) :: field1\n"
        "  call invoke(kern(field1))\n"
        "end subroutine alg1\n")
    code = code.replace(orig_string, new_string)
    psyir = create_alg_psyir(code)
    invoke = psyir.children[0][0]
    assert isinstance(invoke, LFRicAlgorithmInvokeCall)
    name = invoke._def_routine_root_name()
    assert name == expected_name
