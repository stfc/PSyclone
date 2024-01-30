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
# Modified by S. Siso, STFC Daresbury Lab
# Modified by J. Henrichs, Bureau of Meteorology

'''Module containing tests for the AlgorithmInvokeCall and
KernelFunctor algorithm-layer-specific nodes. The tests include
translation of PSyIR to PSyclone Algorithm PSyIR and PSyclone
Algorithm PSyIR to processed PSyIR.

'''
import pytest

from psyclone.domain.common.algorithm import (AlgorithmInvokeCall,
                                              KernelFunctor)
from psyclone.domain.common.transformations import AlgTrans
from psyclone.errors import InternalError, GenerationError
from psyclone.psyir.frontend.fortran import FortranReader
from psyclone.psyir.nodes import Reference, Node, Container
from psyclone.psyir.nodes.node import colored
from psyclone.psyir.symbols import RoutineSymbol, DataTypeSymbol, \
    StructureType, Symbol, REAL_TYPE


def create_alg_psyir(code):
    '''Utility to create a PSyclone Algorithm PSyIR tree from Fortran
    code.

    :param str code: Fortran algorithm code encoded as a string.

    :returns: PSyclone Algorithm PSyIR tree representing the Fortran \
        code.
    :rtype: :py:class:`psyclone.psyir.nodes.Node`

    '''
    fortran_reader = FortranReader()
    psyir = fortran_reader.psyir_from_source(code)

    alg_trans = AlgTrans()
    alg_trans.apply(psyir)

    return psyir


def _check_alg_names(invoke, module_name):
    '''Utility function to check that the
    create_psylayer_symbol_root_names method creates the expected
    names.

    :param invoke: the invoke call for which names are being created.
    :type invoke: \
        :py:class:`psyclone.domain.common.algorithm.psyir.AlgorithmInvokeCall`
    :param str module_name: the expected module name.

    '''
    assert isinstance(invoke, AlgorithmInvokeCall)
    assert invoke.psylayer_routine_root_name is None
    assert invoke.psylayer_container_root_name is None

    invoke.create_psylayer_symbol_root_names()

    assert invoke.psylayer_routine_root_name == "invoke_0_kern"
    assert invoke.psylayer_container_root_name == module_name

    # Check that the names are only created once.
    routine_root_name_tmp = invoke.psylayer_routine_root_name
    container_root_name_tmp = invoke.psylayer_container_root_name
    invoke.create_psylayer_symbol_root_names()
    assert invoke.psylayer_routine_root_name is routine_root_name_tmp
    assert invoke.psylayer_container_root_name is container_root_name_tmp


def test_algorithminvokecall():
    '''Check that an instance of AlgorithmInvokeCall can be
    created. Also check any optional arguments.

    '''
    routine = RoutineSymbol("hello")
    call = AlgorithmInvokeCall(routine, 2)
    assert call._children_valid_format == "[KernelFunctor]*"
    assert call._text_name == "AlgorithmInvokeCall"
    assert call._colour == "green"
    assert call.psylayer_routine_root_name is None
    assert call.psylayer_container_root_name is None
    assert call._index == 2
    assert call.parent is None
    assert call._name is None

    name = "description"
    parent = Container("container")
    call = AlgorithmInvokeCall(
        routine, 2, parent=parent, name=name)
    assert call.parent is parent
    assert call._name == name


def test_algorithminvokecall_error():
    '''Check that AlgorithmInvokeCall raises the expected exceptions if
    the invoke argument has an invalid value.

    '''
    routine = RoutineSymbol("hello")
    with pytest.raises(TypeError) as info:
        AlgorithmInvokeCall(routine, "error")
    assert ("AlgorithmInvokeCall index argument should be an int but found "
            "'str'." in str(info.value))

    with pytest.raises(ValueError) as info:
        AlgorithmInvokeCall(routine, -1)
    assert ("AlgorithmInvokeCall index argument should be a non-negative "
            "integer but found -1." in str(info.value))

    with pytest.raises(ValueError) as info:
        AlgorithmInvokeCall(routine, 1, name=routine)
    assert ("Error with AlgorithmInvokeCall name argument: A name should be "
            "a string, but found 'RoutineSymbol'." in str(info.value))

    with pytest.raises(ValueError) as info:
        AlgorithmInvokeCall(routine, 1, name="not valid")
    assert ("Error with AlgorithmInvokeCall name argument: Invalid Fortran "
            "name 'not valid' found." in str(info.value))


def test_aic_create():
    '''Check that the create method behaves as expected.'''

    kernel_functor = KernelFunctor(DataTypeSymbol("dummy", REAL_TYPE))
    routine = RoutineSymbol("hello")
    index = 10
    aic = AlgorithmInvokeCall.create(routine, [kernel_functor], index)
    assert isinstance(aic, AlgorithmInvokeCall)
    assert len(aic.children) == 1
    assert aic.children[0] is kernel_functor
    assert aic._routine is routine
    assert aic._index == index
    assert aic._name is None

    name = "desCription"
    aic = AlgorithmInvokeCall.create(
        routine, [kernel_functor.detach()], index, name=name)
    assert aic._name == name.lower()

    with pytest.raises(ValueError) as err:
        _ = AlgorithmInvokeCall.create(routine, [kernel_functor.detach()],
                                       index, name="oh deary me")
    assert "Invalid Fortran name 'oh deary me' found." in str(err.value)

    with pytest.raises(GenerationError) as info:
        AlgorithmInvokeCall.create(routine, kernel_functor, index)
    assert ("AlgorithmInvokeCall create arguments argument should be a list "
            "but found 'KernelFunctor'." in str(info.value))


class DummySubClass(AlgorithmInvokeCall):
    '''A dummy subclass of AlgorithmInvokeCall used for testing the
    behaviour of the create method in AlgorithmInvokeCall.

    '''


@pytest.mark.parametrize("cls", [AlgorithmInvokeCall, DummySubClass])
def test_aic_create_object(cls):
    '''Check that the AlgorithmInvokeCall create method creates the
    expected object (which could be a subclass of AlgorithmInvokeCall)

    '''
    routine = RoutineSymbol("hello")
    call = cls.create(routine, [], 0)
    assert call.routine is routine
    # pylint: disable=unidiomatic-typecheck
    assert type(call) is cls


def test_aic_validate_child():
    '''Check that the _validate_child method behaves as expected.'''

    kernel_functor = KernelFunctor(DataTypeSymbol("dummy", REAL_TYPE))
    assert AlgorithmInvokeCall._validate_child(0, kernel_functor)
    assert not AlgorithmInvokeCall._validate_child(0, "Invalid")

    routine = RoutineSymbol("hello")
    call = AlgorithmInvokeCall(routine, 0)
    with pytest.raises(GenerationError) as info:
        call.children = ["invalid"]
    assert ("Item 'str' can't be child 0 of 'AlgorithmInvokeCall'. The valid "
            "format is: '[KernelFunctor]*'." in str(info.value))
    call.children = [kernel_functor]


def test_aic_node_str():
    '''Check that the node_str method returns the expected representation
    of this node.

    '''
    routine = RoutineSymbol("hello")
    call = AlgorithmInvokeCall.create(
        routine, [], 0, name="describing_an_invoke")
    assert ("AlgorithmInvokeCall[name=\"describing_an_invoke\"]"
            in call.node_str(colour=False))


def test_aic_defroutinerootname():
    '''Check that the _def_routine_root_name() internal method behaves as
    expected.

    '''
    symbol_name = "dummy"
    kernel_functor = KernelFunctor(DataTypeSymbol(symbol_name, REAL_TYPE))
    routine = RoutineSymbol("hello")
    index = 3
    call = AlgorithmInvokeCall(routine, index)
    call.children = [kernel_functor]
    assert call._def_routine_root_name() == f"invoke_{index}_{symbol_name}"

    call.children.append(kernel_functor.copy())
    assert call._def_routine_root_name() == f"invoke_{index}"

    for name in [" a  description ", "' a__description '",
                 "\" a  description \""]:
        call._name = name
        assert call._def_routine_root_name() == "invoke_a__description"

    # lowering should not prepend "invoke" if the invoke call has a
    # name starting with "invoke"
    symbol_name = "dummy"
    kernel_functor = KernelFunctor(DataTypeSymbol(symbol_name, REAL_TYPE))
    routine = RoutineSymbol("hello")
    index = 3
    call = AlgorithmInvokeCall(routine, index, name="invoke_1")
    call.children = [kernel_functor]
    assert call._def_routine_root_name() == "invoke_1"


def test_aic_defroutineroot_name_error():
    '''Check that the _def_routine_root_name() internal method raises the
    expected exception if the supplied name is invalid.

    '''
    symbol_name = "dummy"
    kernel_functor = KernelFunctor(DataTypeSymbol(symbol_name, REAL_TYPE))
    routine = RoutineSymbol("hello")
    index = 3
    call = AlgorithmInvokeCall(routine, index)
    call.children = [kernel_functor]
    assert call._def_routine_root_name() == f"invoke_{index}_{symbol_name}"

    call.children.append(kernel_functor.copy())
    assert call._def_routine_root_name() == f"invoke_{index}"

    for name in ["1name", "name!", "nameʑ", "ʒʓʔʕʗʘʙʚʛʜʝʞ"]:
        call._name = name
        with pytest.raises(TypeError) as info:
            _ = call._def_routine_root_name()
        print(name)
        assert (f"AlgorithmInvokeCall:_def_routine_root_name() the (optional) "
                f"name of an invoke must be a string containing a valid name "
                f"(with any spaces replaced by underscores) but found "
                f"'{name}'." in str(info.value))


def test_aic_createpsylayersymbolrootnames():
    '''Check that the create_psylayer_symbol_root_names method behaves in
    the expected way when the name comes from a subroutine, a module
    and when it has a filecontainer, i.e. it creates and stores a root
    name for a routine symbol and a container symbol. Also check that
    it raises the expected exception if no FileContainer or Routine
    nodes are found in the tree.

    '''
    code = (
        "subroutine alg1()\n"
        "  use kern_mod, only : kern\n"
        "  use field_mod, only : field_type\n"
        "  type(field_type) :: field1\n"
        "  call invoke(kern(field1))\n"
        "end subroutine alg1\n")

    # FileContainer and Routine (subroutine)
    psyir = create_alg_psyir(code)
    invoke = psyir.children[0][0]
    _check_alg_names(invoke, "psy_alg1")

    # Routine, no FileContainer
    psyir = create_alg_psyir(code)
    psyir = psyir.children[0]
    psyir.detach()
    invoke = psyir[0]
    _check_alg_names(invoke, "psy_alg1")

    code = (
        "module my_mod\n"
        "contains\n"
        "subroutine alg1()\n"
        "  use kern_mod, only : kern\n"
        "  use field_mod, only : field_type\n"
        "  type(field_type) :: field1\n"
        "  call invoke(kern(field1))\n"
        "end subroutine alg1\n"
        "end module my_mod\n")

    # File container and module
    psyir = create_alg_psyir(code)
    invoke = psyir.children[0].children[0][0]
    _check_alg_names(invoke, "psy_my_mod")

    # Module, no FileContainer
    psyir = create_alg_psyir(code)
    psyir = psyir.children[0]
    psyir.detach()
    invoke = psyir.children[0][0]
    _check_alg_names(invoke, "psy_my_mod")

    # No modules or FileContainers (should not happen)
    invoke.psylayer_container_root_name = None
    invoke.detach()
    with pytest.raises(InternalError) as error:
        invoke.create_psylayer_symbol_root_names()
    assert "No Routine or Container node found." in str(error.value)


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
    assert isinstance(invoke, AlgorithmInvokeCall)
    routine_node = psyir.children[0]
    name = invoke._def_container_root_name(routine_node)
    assert name == "psy_alg1"


def test_kernelfunctor():
    '''Check that an instance of KernelFunctor class can be created. Also
    check that the symbol method works as expected.

    '''
    symbol = DataTypeSymbol("hello", StructureType())
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
    symbol = DataTypeSymbol("hello", StructureType())
    klr = KernelFunctor(symbol, parent=parent)
    assert klr.parent == parent


def test_kernelfunctor_invalid_symbol():
    '''Check KernelFunctor raises the expected exception if the type of
    the symbol argument is invalid.

    '''
    with pytest.raises(TypeError) as info:
        _ = KernelFunctor(Symbol("hello"))
    assert ("KernelFunctor symbol argument should be a DataTypeSymbol but "
            "found 'Symbol'." in str(info.value))


class SubClass(KernelFunctor):
    '''Utility subclass of KernelFunctor to test that the create method
    in KernelFunctor behaves as expected for subclasses.'''


@pytest.mark.parametrize("cls", [KernelFunctor, SubClass])
def test_kernelfunctor_create(cls):
    '''Check that the create method of KernelFunctor works as expected.

    '''
    symbol = DataTypeSymbol("hello", StructureType())
    klr = cls.create(symbol, [])
    # pylint: disable=unidiomatic-typecheck
    assert type(klr) is cls
    assert klr._symbol == symbol
    assert not klr.children

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
    assert ("KernelFunctor create() symbol argument should be a DataTypeSymbol"
            " but found 'Symbol'." in str(info.value))


def test_kernelfunctor_create_invalid_args1():
    '''Check that the create method of KernelFunctor raises the expected
    exception if the provided 'arguments' argument is not a list.

    '''
    symbol = DataTypeSymbol("hello", StructureType())
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
    symbol = DataTypeSymbol("hello", StructureType())
    with pytest.raises(GenerationError) as info:
        _ = KernelFunctor.create(symbol, ["hello"])
    assert ("Item 'str' can't be child 0 of 'KernelFunctor'. The valid "
            "format is: '[DataNode]*'." in str(info.value))


def test_kernelfunctor_node_str():
    '''Check the node_str method of the KernelFunctor class.'''

    symbol = DataTypeSymbol("hello", StructureType())
    arg = Reference(Symbol("dummy"))
    klr = KernelFunctor.create(symbol, [arg])
    coloredtext = colored("KernelFunctor", KernelFunctor._colour)
    assert klr.node_str() == coloredtext+"[name:'hello']"


def test_kernelfunctor_str():
    '''Check the str method of the KernelFunctor class.'''

    symbol = DataTypeSymbol("hello", StructureType())
    arg = Reference(Symbol("dummy"))
    klr = KernelFunctor.create(symbol, [arg])
    assert klr.__str__() == "KernelFunctor[name:'hello']"
