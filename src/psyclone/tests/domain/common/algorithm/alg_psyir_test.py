# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2021, Science and Technology Facilities Council
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

'''Module containing tests for the translation of PSyIR to PSyclone
Algorithm PSyIR.

'''
from __future__ import absolute_import
import pytest

from fparser.two.parser import ParserFactory
from fparser.common.readfortran import FortranStringReader

from psyclone.psyir.frontend.fparser2 import Fparser2Reader
from psyclone.psyir.nodes import Reference, Node, ArrayReference, \
    BinaryOperation
from psyclone.psyir.nodes.node import colored
from psyclone.psyir.symbols import RoutineSymbol, TypeSymbol, \
    StructureType, Symbol, ContainerSymbol
from psyclone.domain.common.algorithm import AlgorithmInvokeCall, \
    KernelFunctor
from psyclone.errors import GenerationError
from psyclone.domain.common.transformations import AlgTrans


def create_alg_psyir(code):
    '''Utility to create a PSyclone Algorithm PSyIR tree from Fortran
    code.

    :param str code: Fortran algorithm code encoded as a string.

    :returns: PSyclone Algorithm PSyIR tree representing the Fortran \
        code.
    :rtype: :py:class:`psyclone.psyir.nodes.Node`

    '''
    fortran_reader = FortranStringReader(code)
    f2008_parser = ParserFactory().create(std="f2008")
    parse_tree = f2008_parser(fortran_reader)

    psyir_reader = Fparser2Reader()
    psyir = psyir_reader.generate_psyir(parse_tree)

    alg_trans = AlgTrans()
    alg_trans.apply(psyir)

    return psyir


def test_algorithminvokecall():
    '''Check that an instance of AlgorithmInvokeCall can be
    created.

    '''
    routine = RoutineSymbol("hello")
    call = AlgorithmInvokeCall(routine)
    assert call._text_name == "AlgorithmInvokeCall"
    assert call._colour == "green"
    assert call._routine_symbol is None
    assert call._container_symbol is None


def test_createlanguagelevelsymbols_error():
    '''Check that the create_language_level_symbols method in
    AlgorithmInvokeCall raises the expected exceptions if the index
    argument is invalid.

    '''
    routine = RoutineSymbol("hello")
    invoke = AlgorithmInvokeCall(routine)
    with pytest.raises(TypeError) as info:
        invoke.create_language_level_symbols("error")
    assert ("AlgorithmInvokeCall index argument should be an int but found "
            "'str'." in str(info.value))

    with pytest.raises(ValueError) as info:
        invoke.create_language_level_symbols(-1)
    assert ("AlgorithmInvokeCall index argument should be a non-negative "
            "integer but found -1." in str(info.value))


def test_createlanguagelevelsymbols():
    '''Check that the create_language_level_symbols method behaves in the
    expected way, i.e. creates a routine_symbol and a
    container_symbol.

    '''
    code = (
        "subroutine alg1()\n"
        "  use kern_mod, only : kern\n"
        "  use field_mod, only : field_type\n"
        "  type(field_type) :: field1\n"
        "  call invoke(kern(field1))\n"
        "end subroutine alg1\n")

    psyir = create_alg_psyir(code)
    invoke = psyir.children[0]
    assert isinstance(invoke, AlgorithmInvokeCall)
    assert invoke._routine_symbol is None
    assert invoke._container_symbol is None

    invoke.create_language_level_symbols(0)

    assert isinstance(invoke._routine_symbol, RoutineSymbol)
    assert invoke._routine_symbol.name == "invoke_0_kern"
    assert isinstance(invoke._container_symbol, ContainerSymbol)
    assert invoke._container_symbol.name == "invoke_0_kern_mod"


def test_lowertolanguagelevel_error():
    '''Check that the lower_to_language_level method raises the expected
    exception when the create_language_level_symbols method has not
    been called and when an unexpected argument is found.

    '''
    code = (
        "subroutine alg1()\n"
        "  use kern_mod\n"
        "  use field_mod, only : field_type\n"
        "  type(field_type) :: field\n"
        "  call invoke(kern(field*1.0))\n"
        "end subroutine alg1\n")

    psyir = create_alg_psyir(code)
    invoke = psyir.children[0]

    with pytest.raises(GenerationError) as info:
        invoke.lower_to_language_level()
    assert ("The 'create_language_level_symbols()' method must be called "
            "before calling 'lower_to_language_level()'." in str(info.value))

    invoke.create_language_level_symbols(0)

    with pytest.raises(GenerationError) as info:
        invoke.lower_to_language_level()
    assert ("Expected Algorithm-layer kernel arguments to be a literal, "
            "reference or array reference, but found 'BinaryOperation'."
            in str(info.value))


@pytest.mark.xfail(reason="Issue #753: expression comparisons need improving")
def test_lowertolanguagelevel_expr():
    '''Check that the lower_to_language_level method deals correctly with
    simple associative expresssions, i.e. i+1 is the same as 1+i.

    '''
    code = (
        "subroutine alg1()\n"
        "  use kern_mod\n"
        "  use field_mod, only : field_type\n"
        "  type(field_type) :: field(10)\n"
        "  integer :: i\n"
        "  call invoke(kern(field(i+1), field(1+i)))\n"
        "end subroutine alg1\n")

    psyir = create_alg_psyir(code)
    invoke = psyir.children[0]
    invoke.create_language_level_symbols(0)
    invoke.lower_to_language_level()
    assert len(psyir.children[0].children) == 1


def test_lowertolanguagelevel_single():
    '''Check that the lower_to_language_level method works as expected
    when it has a single kernel with multiple fields of the same name.

    '''
    code = (
        "subroutine alg1()\n"
        "  use kern_mod\n"
        "  use field_mod, only : field_type\n"
        "  integer :: i,j\n"
        "  type(field_type) :: field1, field2(10)\n"
        "  call invoke(kern1(field1, field1, field2(i), field2( j )))\n"
        "end subroutine alg1\n")

    psyir = create_alg_psyir(code)
    invoke = psyir.children[0]

    assert isinstance(invoke, AlgorithmInvokeCall)
    assert len(psyir.walk(AlgorithmInvokeCall)) == 1
    assert len(psyir.walk(KernelFunctor)) == 1

    invoke.create_language_level_symbols(0)
    invoke.lower_to_language_level()

    assert len(psyir.walk(AlgorithmInvokeCall)) == 0
    assert len(psyir.walk(KernelFunctor)) == 0
    call = psyir.children[0]
    assert call.routine.name == "invoke_0_kern1"
    assert call.routine.is_global
    assert call.routine.interface.container_symbol.name == "invoke_0_kern1_mod"
    args = call.children
    assert len(args) == 3
    assert isinstance(args[0], Reference)
    assert args[0].symbol.name == "field1"
    assert isinstance(args[1], ArrayReference)
    assert args[1].symbol.name == "field2"
    assert len(args[1].children) == 1
    assert args[1].children[0].symbol.name == "i"
    assert isinstance(args[2], ArrayReference)
    assert args[2].symbol.name == "field2"
    assert len(args[2].children) == 1
    assert args[2].children[0].symbol.name == "j"


def test_lowertolanguagelevel_multi():
    '''Check that the lower_to_language_level method works as expected
    when it has multiple kernels with fields of the same name.

    '''
    code = (
        "subroutine alg1()\n"
        "  use kern_mod\n"
        "  use precision_mod, only : r_def\n"
        "  use field_mod, only : field_type\n"
        "  integer :: i,j\n"
        "  type(field_type) :: field1, field2(10)\n"
        "  call invoke(kern1(field1), kern2(field1), kern3(field2(i)), &\n"
        "              kern1(field2(I)), kern2(field2( j )), &\n"
        "              kern3(field2(j+1)), kern1(1.0_r_def))\n"
        "end subroutine alg1\n")

    psyir = create_alg_psyir(code)
    invoke = psyir.children[0]

    assert isinstance(invoke, AlgorithmInvokeCall)
    assert len(psyir.walk(AlgorithmInvokeCall)) == 1
    assert len(psyir.walk(KernelFunctor)) == 7

    invoke.create_language_level_symbols(0)
    invoke.lower_to_language_level()

    assert len(psyir.walk(AlgorithmInvokeCall)) == 0
    assert len(psyir.walk(KernelFunctor)) == 0
    call = psyir.children[0]
    assert call.routine.name == "invoke_0"
    assert call.routine.is_global
    assert call.routine.interface.container_symbol.name == "invoke_0_mod"
    args = call.children
    assert len(args) == 4
    assert isinstance(args[0], Reference)
    assert args[0].symbol.name == "field1"
    assert isinstance(args[1], ArrayReference)
    assert args[1].symbol.name == "field2"
    assert len(args[1].children) == 1
    assert args[1].children[0].symbol.name == "i"
    assert isinstance(args[2], ArrayReference)
    assert args[2].symbol.name == "field2"
    assert len(args[2].children) == 1
    assert args[2].children[0].symbol.name == "j"
    assert isinstance(args[3], ArrayReference)
    assert args[3].symbol.name == "field2"
    assert len(args[3].children) == 1
    assert isinstance(args[3].children[0], BinaryOperation)


def test_kernelfunctor():
    '''Check that an instance of KernelFunctor class can be created. Also
    check that the symbol method works as expected.

    '''
    symbol = TypeSymbol("hello", StructureType())
    klr = KernelFunctor(symbol)
    assert klr._symbol == symbol
    assert klr.symbol == symbol
    assert klr._colour == "yellow"
    assert klr._text_name == "KernelFunctor"
    assert klr.parent is None


def test_kernelfunctor_parent():
    '''Check that the optional parent argument to a KernelFunctor class
    constructor is stored correctly.

    '''
    parent = Node()
    symbol = TypeSymbol("hello", StructureType())
    klr = KernelFunctor(symbol, parent=parent)
    assert klr.parent == parent


def test_kernelfunctor_invalid_symbol():
    '''Check KernelFunctor raises the expected exception if the type of
    the symbol argument is invalid.

    '''
    with pytest.raises(TypeError) as info:
        _ = KernelFunctor(Symbol("hello"))
    assert ("KernelFunctor symbol argument should be a TypeSymbol but "
            "found 'Symbol'." in str(info.value))


class SubClass(KernelFunctor):
    '''Utility subclass of KernelFunctor to test that the create method
    in KernelFunctor behaves as expected for subclasses.'''


@pytest.mark.parametrize("cls", [KernelFunctor, SubClass])
def test_kernelfunctor_create(cls):
    '''Check that the create method of KernelFunctor works as expected.

    '''
    symbol = TypeSymbol("hello", StructureType())
    klr = cls.create(symbol, [])
    # pylint: disable=unidiomatic-typecheck
    assert type(klr) is cls
    assert klr._symbol == symbol
    assert len(klr.children) == 0

    arg = Reference(Symbol("dummy"))
    klr = KernelFunctor.create(symbol, [arg])
    assert len(klr.children) == 1
    assert klr.children[0] == arg
    assert arg.parent == klr


def test_kernelfunctor_create_invalid_symbol():
    '''Check that the create method of KernelFunctor raises the expected
    exception if the provided symbol argument is not the correct type.

    '''
    symbol = Symbol("hello")
    with pytest.raises(GenerationError) as info:
        _ = KernelFunctor.create(symbol, [])
    assert ("KernelFunctor create() symbol argument should be a TypeSymbol "
            "but found 'Symbol'." in str(info.value))


def test_kernelfunctor_create_invalid_args1():
    '''Check that the create method of KernelFunctor raises the expected
    exception if the provided 'arguments' argument is not a list.

    '''
    symbol = TypeSymbol("hello", StructureType())
    with pytest.raises(GenerationError) as info:
        _ = KernelFunctor.create(symbol, "Not a list")
    assert ("KernelFunctor create() arguments argument should be a list "
            "but found 'str'." in str(info.value))


def test_kernelfunctor_invalid_args2():
    '''Check that the create method of KernelFunctor raises the expected
    exception if its supplied list of children are not the expected
    type (tests _validate_child method and _children_valid_format
    variable)

    '''
    symbol = TypeSymbol("hello", StructureType())
    with pytest.raises(GenerationError) as info:
        _ = KernelFunctor.create(symbol, ["hello"])
    assert("Item 'str' can't be child 0 of 'KernelFunctor'. The valid "
           "format is: '[DataNode]*'." in str(info.value))


def test_kernelfunctor_node_str():
    '''Check the node_str method of the KernelFunctor class.'''

    symbol = TypeSymbol("hello", StructureType())
    arg = Reference(Symbol("dummy"))
    klr = KernelFunctor.create(symbol, [arg])
    coloredtext = colored("KernelFunctor", KernelFunctor._colour)
    assert klr.node_str() == coloredtext+"[name='hello']"


def test_kernelfunctor_str():
    '''Check the str method of the KernelFunctor class.'''

    symbol = TypeSymbol("hello", StructureType())
    arg = Reference(Symbol("dummy"))
    klr = KernelFunctor.create(symbol, [arg])
    assert klr.__str__() == "KernelFunctor[name='hello']"
