# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2022-2024, Science and Technology Facilities Council.
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
# Authors: R. W. Ford and A. R. Porter, STFC Daresbury Laboratory.

''' Module containing pytest unit tests for the LFRicAlgInvoke2PSyCallTrans
transformation.

'''
import pytest

from psyclone.domain.lfric.algorithm import (
    LFRicAlgorithmInvokeCall, LFRicFunctor, LFRicBuiltinFunctorFactory)
from psyclone.domain.lfric.kernel import (
    LFRicKernelMetadata, FieldArgMetadata, LFRicKernelContainer,
    MetaFuncsArgMetadata)
from psyclone.domain.lfric.lfric_builtins import BUILTIN_MAP
from psyclone.domain.lfric.transformations import (
    LFRicAlgTrans, LFRicAlgInvoke2PSyCallTrans)
from psyclone.errors import GenerationError
from psyclone.psyir.nodes import (
    Call, Routine, Reference, Container, FileContainer, Literal)
from psyclone.psyir.symbols import (
    RoutineSymbol, DataTypeSymbol, REAL_TYPE, Symbol, SymbolTable,
    INTEGER_TYPE)
from psyclone.psyir.transformations import TransformationError


def test_lfai2psycall_validate():
    '''Test the validate() method of the LFRicAlgInvoke2PSyCallTrans
    class.

    '''
    # Wrong arg type
    trans = LFRicAlgInvoke2PSyCallTrans()
    with pytest.raises(TransformationError) as err:
        trans.validate(None)
    assert ("The supplied call argument should be an "
            "`LFRicAlgorithmInvokeCall` node but found 'NoneType'"
            in str(err.value))

    # No metadata internal TypeError
    call = LFRicAlgorithmInvokeCall.create(
        RoutineSymbol("mysub"), [], 0)
    with pytest.raises(TransformationError) as err:
        trans.validate(call)
    assert ("A dictionary containing LFRic kernel PSyIR must be passed into "
            "the LFRicAlgInvoke2PSyCallTrans transformation but this was not "
            "found." in str(err.value))

    # No metadata internal KeyError
    with pytest.raises(TransformationError) as err:
        trans.validate(call, options={"dummy": "dummy"})
    assert ("A dictionary containing LFRic kernel PSyIR must be passed into "
            "the LFRicAlgInvoke2PSyCallTrans transformation but this was not "
            "found." in str(err.value))

    # kernels not a dictionary
    with pytest.raises(TransformationError) as err:
        trans.validate(call, options={"kernels": None})
    assert ("The value of 'kernels' in the options argument must be a "
            "dictionary but found 'NoneType'." in str(err.value))

    # kernels entry index not found
    data_type_symbol = DataTypeSymbol("kern_type", REAL_TYPE)
    arguments = [Reference(Symbol("arg1"))]
    kernel_functor = LFRicFunctor.create(data_type_symbol, arguments)
    call = LFRicAlgorithmInvokeCall.create(
        RoutineSymbol("mysub"), [kernel_functor], 0)
    with pytest.raises(TransformationError) as err:
        trans.validate(call, options={"kernels": {None: None}})
    assert ("The 'kernels' option must be a dictionary containing LFRic "
            "kernel PSyIR indexed by the id's of the associated kernel "
            "functors, but the id for kernel functor 'kern_type' was not "
            "found." in str(err.value))

    # kernels entry value invalid
    with pytest.raises(TransformationError) as err:
        trans.validate(call, options={"kernels": {id(kernel_functor): None}})
    assert ("A PSyIR Container (an LFRic kernel module) was expected, but "
            "found 'NoneType'." in str(err.value))

    # no LFRicKernelContainer in PSyIR
    psyir = Container.create("mymod", SymbolTable(), [])
    with pytest.raises(TransformationError) as err:
        trans.validate(call, options={"kernels": {id(kernel_functor): psyir}})
    assert ("LFRic kernel PSyIR should contain an LFRicKernelContainer as "
            "the root or first child of the root but this was not found."
            in str(err.value))

    # Invalid number of arguments (metadata expects 2 but kernel functor has 1)
    metadata = LFRicKernelMetadata(
        operates_on="cell_column",
        meta_args=[FieldArgMetadata("gh_real", "gh_write", "w3"),
                   FieldArgMetadata("gh_real", "gh_read", "w3")],
        procedure_name="kernel_code",
        name="kernel_type")
    psyir = LFRicKernelContainer.create(
        "kernel_mod", metadata, SymbolTable(), [])
    with pytest.raises(GenerationError) as err:
        trans.validate(call, options={"kernels": {id(kernel_functor): psyir}})
    assert ("The invoke kernel functor 'kern_type' has 1 arguments, but the "
            "kernel metadata expects there to be 2 arguments."
            in str(err.value))

    # OK
    metadata = LFRicKernelMetadata(
        operates_on="cell_column",
        meta_args=[FieldArgMetadata("gh_real", "gh_write", "w3")],
        procedure_name="kernel_code",
        name="kernel_type")
    psyir = LFRicKernelContainer.create(
        "kernel_mod", metadata, SymbolTable(), [])
    trans.validate(call, options={"kernels": {id(kernel_functor): psyir}})


def test_lfai2psycall_get_metadata():
    '''Test the _get_metadata() utility method.'''

    # Not PSyIR Container
    trans = LFRicAlgInvoke2PSyCallTrans()
    with pytest.raises(TransformationError) as info:
        trans._get_metadata(None)
    assert ("A PSyIR Container (an LFRic kernel module) was expected, but "
            "found 'NoneType'." in str(info.value))

    # Not LFRic Kernel
    with pytest.raises(TransformationError) as info:
        trans._get_metadata(Container.create("mymod", SymbolTable(), []))
    assert ("LFRic kernel PSyIR should contain an LFRicKernelContainer as "
            "the root or first child of the root but this was not found."
            in str(info.value))

    # OK root is an LFRicKernelContainer
    metadata = LFRicKernelMetadata(
        operates_on="cell_column",
        meta_args=[FieldArgMetadata("gh_real", "gh_write", "w3")],
        procedure_name="kernel_code",
        name="kernel_type")
    psyir = LFRicKernelContainer.create(
        "kernel_mod", metadata, SymbolTable(), [])
    trans._get_metadata(psyir)

    # OK root.children[0] is an  LFRicKernelContainer
    psyir = FileContainer.create("file", SymbolTable(), [psyir])
    trans._get_metadata(psyir)


# pylint: disable=too-many-statements
def test_lfai2psycall_get_arguments():
    '''Test the get_arguments() method.'''

    # check_arg=True: Invalid number of arguments (metadata expects 2
    # but kernel functor has 1)
    trans = LFRicAlgInvoke2PSyCallTrans()
    data_type_symbol = DataTypeSymbol("kern_type", REAL_TYPE)
    arguments = [Reference(Symbol("arg1"))]
    kernel_functor = LFRicFunctor.create(data_type_symbol, arguments)
    call = LFRicAlgorithmInvokeCall.create(
        RoutineSymbol("mysub"), [kernel_functor], 0)
    metadata = LFRicKernelMetadata(
        operates_on="cell_column",
        meta_args=[FieldArgMetadata("gh_real", "gh_write", "w3"),
                   FieldArgMetadata("gh_real", "gh_read", "w3")],
        procedure_name="kernel_code",
        name="kernel_type")
    psyir = LFRicKernelContainer.create(
        "kernel_mod", metadata, SymbolTable(), [])
    with pytest.raises(GenerationError) as err:
        trans.get_arguments(
            call, options={"kernels": {id(kernel_functor): psyir}},
            check_args=True)
    assert ("The invoke kernel functor 'kern_type' has 1 arguments, but the "
            "kernel metadata expects there to be 2 arguments."
            in str(err.value))

    # OK
    metadata.meta_args = [FieldArgMetadata("gh_real", "gh_write", "w3")]
    psyir = LFRicKernelContainer.create(
        "kernel_mod", metadata, SymbolTable(), [])
    args = trans.get_arguments(
        call, options={"kernels": {id(kernel_functor): psyir}})
    assert len(args) == 1
    assert isinstance(args[0], Reference)
    assert args[0].name == "arg1"

    # OK, multi-kern
    data_type_symbol = DataTypeSymbol("kern2_type", REAL_TYPE)
    arguments = [Reference(Symbol("arg1")), Reference(Symbol("arg2"))]
    kernel_functor2 = LFRicFunctor.create(data_type_symbol, arguments)
    kernel_functor1 = kernel_functor.copy()
    call = LFRicAlgorithmInvokeCall.create(
        RoutineSymbol("mysub"), [kernel_functor1, kernel_functor2], 0)
    metadata2 = LFRicKernelMetadata(
        operates_on="cell_column",
        meta_args=[FieldArgMetadata("gh_real", "gh_write", "w3"),
                   FieldArgMetadata("gh_real", "gh_read", "w3")],
        procedure_name="kernel2_code",
        name="kernel2_type")
    psyir2 = LFRicKernelContainer.create(
        "kernel2_mod", metadata2, SymbolTable(), [])
    args = trans.get_arguments(
        call, options={"kernels": {
            id(kernel_functor1): psyir, id(kernel_functor2): psyir2}})
    assert len(args) == 2
    assert isinstance(args[0], Reference)
    assert args[0].name == "arg1"
    assert isinstance(args[1], Reference)
    assert args[1].name == "arg2"

    # OK, stencil and qr
    arguments = [Reference(Symbol("arg1")), Reference(Symbol("arg2")),
                 Reference(Symbol("stencil1")), Reference(Symbol("stencil2")),
                 Reference(Symbol("qr")), Reference(Symbol("qr2"))]
    kernel_functor = LFRicFunctor.create(data_type_symbol, arguments)
    call = LFRicAlgorithmInvokeCall.create(
        RoutineSymbol("mysub"), [kernel_functor], 0)
    metadata.meta_args = [
        FieldArgMetadata("gh_real", "gh_write", "w3"),
        FieldArgMetadata("gh_real", "gh_read", "w3", stencil="xory1d")]
    metadata.meta_funcs = [MetaFuncsArgMetadata("w3", basis_function=True)]
    metadata.shapes = ["gh_quadrature_xyoz", "gh_quadrature_face"]
    psyir = LFRicKernelContainer.create(
        "kernel_mod", metadata, SymbolTable(), [])
    args = trans.get_arguments(
        call, options={"kernels": {id(kernel_functor): psyir}})
    assert len(args) == 6
    assert isinstance(args[0], Reference)
    assert args[0].name == "arg1"
    assert isinstance(args[1], Reference)
    assert args[1].name == "arg2"
    assert isinstance(args[2], Reference)
    assert args[2].name == "stencil1"
    assert isinstance(args[3], Reference)
    assert args[3].name == "stencil2"
    assert isinstance(args[4], Reference)
    assert args[4].name == "qr"
    assert isinstance(args[5], Reference)
    assert args[5].name == "qr2"

    # error stencil
    arguments = [Reference(Symbol("arg1")), Reference(Symbol("arg2")),
                 Reference(Symbol("stencil1")), Literal("1", INTEGER_TYPE),
                 Reference(Symbol("qr"))]
    kernel_functor = LFRicFunctor.create(data_type_symbol, arguments)
    call = LFRicAlgorithmInvokeCall.create(
        RoutineSymbol("mysub"), [kernel_functor], 0)
    with pytest.raises(GenerationError) as info:
        _ = trans.get_arguments(
            call, options={"kernels": {id(kernel_functor): psyir}})
    assert ("A literal is not a valid value for a stencil direction, but "
            "found '1' for field 'arg2'." in str(info.value))

    # OK builtin
    builtin_factory = LFRicBuiltinFunctorFactory.get()
    builtin_functor = builtin_factory.create(
        "setval_x", SymbolTable(),
        [Reference(Symbol("arg1")), Reference(Symbol("arg2"))])
    call = LFRicAlgorithmInvokeCall.create(
        RoutineSymbol("mysub"), [builtin_functor], 0)
    args = trans.get_arguments(call, options={"kernels": {}})
    assert len(args) == 2
    assert isinstance(args[0], Reference)
    assert args[0].name == "arg1"
    assert isinstance(args[1], Reference)
    assert args[1].name == "arg2"


def test_lfai2psycall_apply(fortran_reader):
    ''' Test the apply() method of the LFRicAlgInvoke2PSyCallTrans
    class. '''
    code = (
        "subroutine alg1()\n"
        "  use kern_mod, only : kernel_type\n"
        "  use field_mod, only : field_type\n"
        "  type(field_type) :: field1\n"
        "  call invoke(kernel_type(field1))\n"
        "end subroutine alg1\n")
    psyir = fortran_reader.psyir_from_source(code)
    alg_trans = LFRicAlgTrans()
    alg_trans.apply(psyir)
    aic = psyir.walk(LFRicAlgorithmInvokeCall)[0]
    trans = LFRicAlgInvoke2PSyCallTrans()
    metadata = LFRicKernelMetadata(
        operates_on="cell_column",
        meta_args=[FieldArgMetadata("gh_real", "gh_write", "w3")],
        procedure_name="kernel_code",
        name="kernel_type")
    kernel_psyir = LFRicKernelContainer.create(
        "kernel_mod", metadata, SymbolTable(), [])
    trans.apply(aic, options={"kernels": {id(aic.children[0]): kernel_psyir}})
    assert psyir.walk(LFRicAlgorithmInvokeCall) == []
    calls = psyir.walk(Call)
    assert len(calls) == 1
    assert isinstance(calls[0].routine, RoutineSymbol)
    assert calls[0].routine.name == "invoke_0_kernel_type"
    routine = psyir.walk(Routine)[0]
    assert "invoke" not in routine.symbol_table._symbols


def test_lfai2psycall_builtin_apply(fortran_reader):
    ''' Test the apply() method of the LFRicAlgInvoke2PSyCallTrans class
    when there is a builtin kernel. '''
    code = (
        "subroutine alg1()\n"
        "  use constants_mod, only: r_def\n"
        "  use kern_mod, only : kern\n"
        "  use field_mod, only : field_type\n"
        "  type(field_type) :: field1\n"
        "  call invoke(setval_c(field1, 1.0_r_def), kern(field1))\n"
        "end subroutine alg1\n")
    psyir = fortran_reader.psyir_from_source(code)
    alg_trans = LFRicAlgTrans()
    alg_trans.apply(psyir)
    aic = psyir.walk(LFRicAlgorithmInvokeCall)[0]
    trans = LFRicAlgInvoke2PSyCallTrans()

    setval_c_metadata = BUILTIN_MAP["setval_c"].metadata()
    kernel_psyir1 = LFRicKernelContainer.create(
        "setval_c_mod", setval_c_metadata, SymbolTable(), [])
    kern_metadata = LFRicKernelMetadata(
        operates_on="cell_column",
        meta_args=[FieldArgMetadata("gh_real", "gh_write", "w3")],
        procedure_name="kern_code",
        name="kern")
    kernel_psyir2 = LFRicKernelContainer.create(
        "kern_mod", kern_metadata, SymbolTable(), [])

    trans.apply(aic, options={"kernels": {
        id(aic.children[0]): kernel_psyir1,
        id(aic.children[1]): kernel_psyir2}})
    assert psyir.walk(LFRicAlgorithmInvokeCall) == []
    calls = psyir.walk(Call)
    assert len(calls) == 1
    assert isinstance(calls[0].routine, RoutineSymbol)
    assert calls[0].routine.name == "invoke_0"
    routine = psyir.walk(Routine)[0]
    # Check that both the 'invoke' and 'setval_c' symbols have been removed.
    assert "invoke" not in routine.symbol_table._symbols
    assert "setval_c" not in routine.symbol_table._symbols


def test_lfai2psycall_multi_invokes(fortran_reader):
    ''' Test the apply method when a routine contains more than one
    'invoke' call. '''
    code = (
        "subroutine alg1()\n"
        # The builtins_mod wildcard import permits us to use the generic
        # fparser2 frontend to the PSyIR to read this code. Otherwise
        # there's no place for the symbols representing the Builtin kernels
        # to be defined.
        "  use builtins_mod\n"
        "  use constants_mod, only: r_def\n"
        "  use kern_mod, only : kern\n"
        "  use field_mod, only : field_type\n"
        "  type(field_type) :: field1, field2\n"
        "  call invoke(setval_c(field1, 1.0_r_def), kern(field1))\n"
        "  call invoke(setval_c(field2, 0.0_r_def), kern(field2), "
        "setval_x(field1, field2))\n"
        "end subroutine alg1\n")
    psyir = fortran_reader.psyir_from_source(code)
    routine = psyir.walk(Routine)[0]
    alg_trans = LFRicAlgTrans()
    alg_trans.apply(psyir)
    invokes = psyir.walk(LFRicAlgorithmInvokeCall)
    # Apply the transformation to the second invoke.
    trans = LFRicAlgInvoke2PSyCallTrans()

    setval_c_metadata = BUILTIN_MAP["setval_c"].metadata()
    kernel_psyir1 = LFRicKernelContainer.create(
        "setval_c_mod", setval_c_metadata, SymbolTable(), [])
    kern_metadata = LFRicKernelMetadata(
        operates_on="cell_column",
        meta_args=[FieldArgMetadata("gh_real", "gh_write", "w3")],
        procedure_name="kern_code",
        name="kern")
    kernel_psyir2 = LFRicKernelContainer.create(
        "kern_mod", kern_metadata, SymbolTable(), [])
    setval_x_metadata = BUILTIN_MAP["setval_x"].metadata()
    kernel_psyir3 = LFRicKernelContainer.create(
        "setval_x_mod", setval_x_metadata, SymbolTable(), [])

    trans.apply(invokes[1], options={"kernels": {
        id(invokes[1].children[0]): kernel_psyir1,
        id(invokes[1].children[1]): kernel_psyir2,
        id(invokes[1].children[2]): kernel_psyir3}})

    invokes = psyir.walk(LFRicAlgorithmInvokeCall)
    assert len(invokes) == 1
    assert "invoke" in routine.symbol_table._symbols
    assert "setval_c" in routine.symbol_table._symbols
    assert "setval_x" not in routine.symbol_table._symbols

    # Apply the transformation to the one remaining invoke.
    trans.apply(invokes[0], options={"kernels": {
        id(invokes[0].children[0]): kernel_psyir1,
        id(invokes[0].children[1]): kernel_psyir2}})

    assert not psyir.walk(LFRicAlgorithmInvokeCall)
    assert "invoke" not in routine.symbol_table._symbols
    assert "setval_c" not in routine.symbol_table._symbols
