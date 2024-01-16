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

''' Module containing pytest unit tests for the AlgInvoke2PSyCallTrans
transformation.

'''
import pytest

from psyclone.errors import InternalError
from psyclone.domain.common.algorithm import AlgorithmInvokeCall, KernelFunctor
from psyclone.domain.common.transformations import AlgTrans
from psyclone.domain.common.transformations import AlgInvoke2PSyCallTrans
from psyclone.domain.gocean.transformations import GOceanAlgInvoke2PSyCallTrans
from psyclone.psyir.nodes import (
    Call, Loop, Literal, Container, Reference, ArrayReference, BinaryOperation,
    CodeBlock, UnaryOperation)
from psyclone.psyir.symbols import (
    RoutineSymbol, DataSymbol, INTEGER_TYPE, REAL_TYPE, ArrayType,
    DataTypeSymbol)
from psyclone.psyir.transformations import TransformationError


def check_call(call, routine_name, container_name, args_info):
    '''Utility function to check the contents of a processed invoke call.

    :param invoke: the call node that is being checked.
    :type invoke: :py:class:`psyclone.psyir.nodes.Call`
    :param str routine_name: the name of the call node.
    :param str container_name: the name of the container containing \
        the call node.
    ;param args_info: information to check the call arguments.
    :type args_info: \
        List[Tuple[:py:class:`psyclone.psyir.nodes.Reference`, str]] | \
        List[Tuple[:py:class:`psyclone.psyir.nodes.ArrayReference`, str, \
                   str | :py:class:`psyclone.psyir.nodes.BinaryOperation`]]

    '''
    assert isinstance(call.routine, RoutineSymbol)
    assert call.routine.name == routine_name
    assert call.routine.is_import
    assert call.routine.interface.container_symbol.name == container_name
    args = call.children
    assert len(args) == len(args_info)
    for idx, arg_info in enumerate(args_info):
        arg_type = arg_info[0]
        name = arg_info[1]
        assert isinstance(args[idx], arg_type)
        assert args[idx].symbol.name == name
        if arg_type is ArrayReference:
            indices_info = arg_info[2]
            indices = args[idx].children
            assert len(indices) == len(indices_info)
            for idx2, index_info in enumerate(indices_info):
                if isinstance(index_info, str):
                    assert indices[idx2].symbol.name == index_info
                else:  # BinaryOperation
                    assert isinstance(indices[idx2], BinaryOperation)


# pylint: disable=abstract-class-instantiated
def test_abstract():
    '''Test that AlgInvoke2PSyCallTrans is abstract.'''
    with pytest.raises(TypeError) as info:
        _ = AlgInvoke2PSyCallTrans()
    # Python >= 3.9 spots that 'method' should be singular. Prior to this it
    # was plural. Python >= 3.12 tweaks the error message yet again to mention
    # the lack of an implementation and to quote the method name.
    # We split the check to accomodate for this.
    assert ("Can't instantiate abstract class AlgInvoke2PSyCallTrans with"
            in str(info.value))
    assert ("abstract method" in str(info.value))
    assert ("get_arguments" in str(info.value))


# pylint: enable=abstract-class-instantiated
def test_ai2psycall_validate_argtype():
    '''Test the validate() method of the AlgorithmInvoke2PSyCallTrans
    class. Use GOceanAlgInvoke2PSyCallTrans as AlgInvoke2PSyCallTrans
    is abstract.

    '''
    trans = GOceanAlgInvoke2PSyCallTrans()
    with pytest.raises(TransformationError) as err:
        trans.validate(None)
    assert ("The supplied call argument should be an `AlgorithmInvokeCall` "
            "node but found 'NoneType'" in str(err.value))


def test_ai2psycall_validate_no_invoke_sym(fortran_reader):
    '''Check that the validate() method raises the expected exception when
    no invoke symbol is found in the PSyIR. Use
    GOceanAlgInvoke2PSyCallTrans as AlgInvoke2PSyCallTrans is
    abstract.

    '''
    code = (
        "subroutine alg1()\n"
        "  use kern_mod\n"
        "  use field_mod, only : field_type\n"
        "  type(field_type) :: field\n"
        "  call invoke(kern(field))\n"
        "end subroutine alg1\n")

    psyir = fortran_reader.psyir_from_source(code)
    AlgTrans().apply(psyir)
    invoke = psyir.children[0].children[0]
    symbol_table = invoke.scope.symbol_table
    invoke_symbol = symbol_table.lookup("invoke")
    symbol_table.remove(invoke_symbol)
    trans = GOceanAlgInvoke2PSyCallTrans()

    with pytest.raises(InternalError) as info:
        trans.validate(invoke)
    assert ("No 'invoke' symbol found despite there still being at least one "
            "AlgorithmInvokeCall node present." in str(info.value))


def test_ai2psycall_apply(fortran_reader):
    '''Test the apply() method of the AlgorithmInvoke2PSyCallTrans
    class. Use GOceanAlgInvoke2PSyCallTrans as AlgInvoke2PSyCallTrans
    is abstract.

    '''
    code = (
        "subroutine alg1()\n"
        "  use kern_mod, only : kern\n"
        "  use field_mod, only : field_type\n"
        "  type(field_type) :: field1\n"
        "  call invoke(kern(field1))\n"
        "end subroutine alg1\n")
    psyir = fortran_reader.psyir_from_source(code)
    alg_trans = AlgTrans()
    alg_trans.apply(psyir)
    aic = psyir.walk(AlgorithmInvokeCall)[0]
    trans = GOceanAlgInvoke2PSyCallTrans()
    trans.apply(aic)
    assert psyir.walk(AlgorithmInvokeCall) == []
    calls = psyir.walk(Call)
    assert len(calls) == 1
    assert isinstance(calls[0].routine, RoutineSymbol)
    assert calls[0].routine.name == "invoke_0_kern"


def test_ai2psycall_apply_error(fortran_reader):
    '''Check that the apply() method raises the expected exception when an
    unexpected argument is found. Use GOceanAlgInvoke2PSyCallTrans as
    AlgInvoke2PSyCallTrans is abstract.

    '''
    code = (
        "subroutine alg1()\n"
        "  use kern_mod\n"
        "  use field_mod, only : field_type\n"
        "  type(field_type) :: field\n"
        "  call invoke(kern(field*1.0))\n"
        "end subroutine alg1\n")
    psyir = fortran_reader.psyir_from_source(code)
    AlgTrans().apply(psyir)
    invoke = psyir.children[0].children[0]
    trans = GOceanAlgInvoke2PSyCallTrans()
    with pytest.raises(TypeError) as info:
        trans.apply(invoke)
    assert ("Expected Algorithm-layer kernel arguments to be a Literal, "
            "Reference or CodeBlock, but 'field * 1.0' passed to kernel "
            "'kern' is of type 'BinaryOperation'." in str(info.value))


def test_ai2psycall_invalid_name(fortran_reader):
    '''Check that an invalid invoke name is rejected by the transformation.'''
    code = (
        "subroutine alg1()\n"
        "  use kern_mod\n"
        "  use field_mod, only : field_type\n"
        "  type(field_type) :: field(10)\n"
        "  integer :: i\n"
        "  call invoke(kern(field), name='invalid name')\n"
        "end subroutine alg1\n")
    psyir = fortran_reader.psyir_from_source(code)
    with pytest.raises(TransformationError) as err:
        AlgTrans().apply(psyir)
    assert ("Problem with invoke name: Invalid Fortran "
            "name 'invalid name' found" in str(err.value))


def test_ai2psycall_apply_expr(fortran_reader):
    '''Check that the apply() method deals correctly with simple
    associative expressions, i.e. i+1 is the same as 1+i. Use
    GOceanAlgInvoke2PSyCallTrans as AlgInvoke2PSyCallTrans is
    abstract.

    '''
    code = (
        "subroutine alg1()\n"
        "  use kern_mod\n"
        "  use field_mod, only : field_type\n"
        "  type(field_type) :: field(10)\n"
        "  integer :: i\n"
        "  call invoke(kern(field(i+1), field(1+i)))\n"
        "end subroutine alg1\n")
    psyir = fortran_reader.psyir_from_source(code)
    AlgTrans().apply(psyir)
    subroutine = psyir.children[0]
    invoke = subroutine.children[0]
    trans = GOceanAlgInvoke2PSyCallTrans()
    trans.apply(invoke)
    assert len(subroutine.children[0].children) == 1


def test_ai2psycall_apply_single(fortran_reader):
    '''Check that the apply() method works as expected when the invoke has
    a single kernel with multiple fields of the same name. Also check
    that the apply() method creates the required routine and container
    symbols if they have not already been created. Also check that the
    apply() method removes the invoke symbol from the appropriate
    symbol table. Use GOceanAlgInvoke2PSyCallTrans as
    AlgInvoke2PSyCallTrans is abstract.

    '''
    code = (
        "subroutine alg1()\n"
        "  use kern_mod\n"
        "  use field_mod, only : field_type\n"
        "  integer :: i,j\n"
        "  type(field_type) :: field1, field2(10)\n"
        "  call invoke(kern1(field1, field1, field2(i), field2( j )))\n"
        "end subroutine alg1\n")
    psyir = fortran_reader.psyir_from_source(code)
    AlgTrans().apply(psyir)
    invoke = psyir.children[0][0]

    assert isinstance(invoke, AlgorithmInvokeCall)
    assert len(psyir.walk(AlgorithmInvokeCall)) == 1
    assert len(psyir.walk(KernelFunctor)) == 1
    assert "invoke" in invoke.scope.symbol_table._symbols

    # Don't call create_psylayer_symbol_root_names() here. This is to
    # check that the transformation creates the names if needed.
    trans = GOceanAlgInvoke2PSyCallTrans()
    trans.apply(invoke)

    assert not psyir.walk(AlgorithmInvokeCall)
    assert not psyir.walk(KernelFunctor)
    invoke = psyir.children[0][0]
    assert "invoke" not in invoke.scope.symbol_table._symbols

    call = psyir.children[0][0]
    check_call(call, "invoke_0_kern1", "psy_alg1",
               [(Reference, "field1"),
                (ArrayReference, "field2", ["i"]),
                (ArrayReference, "field2", ["j"])])


def test_aipsycall_apply_multi(fortran_reader):
    '''Check that the apply() method works as expected when it has
    multiple kernels with fields of the same name. Also check that an
    invoke name is supported. Use GOceanAlgInvoke2PSyCallTrans as
    AlgInvoke2PSyCallTrans is abstract.

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
        "              kern3(field2(j+1)), kern1(1.0_r_def), &\n"
        "              name=\"multi_kern_invoke\")\n"
        "end subroutine alg1\n")
    psyir = fortran_reader.psyir_from_source(code)
    AlgTrans().apply(psyir)
    invoke = psyir.children[0][0]

    assert isinstance(invoke, AlgorithmInvokeCall)
    assert len(psyir.walk(AlgorithmInvokeCall)) == 1
    assert len(psyir.walk(KernelFunctor)) == 7

    # Explicitly create the language level root names before transforming
    # to make sure the transformation works if they have already been created.
    invoke.create_psylayer_symbol_root_names()
    trans = GOceanAlgInvoke2PSyCallTrans()
    trans.apply(invoke)

    assert not psyir.walk(AlgorithmInvokeCall)
    assert not psyir.walk(KernelFunctor)

    call = psyir.children[0][0]
    check_call(call, "invoke_multi_kern_invoke", "psy_alg1",
               [(Reference, "field1"),
                (ArrayReference, "field2", ["i"]),
                (ArrayReference, "field2", ["j"]),
                (ArrayReference, "field2", [BinaryOperation])])


def test_ai2psycall_apply_multi_invoke(fortran_reader):
    '''Check that the apply() method works as expected when it has
    multiple invoke's. Also purposely add existing algorithm names
    that clash with root names to make sure generated names are
    correct. Also check that the apply() method removes the invoke
    symbol from the appropriate symbol table when the second invoke is
    lowered. Use GOceanAlgInvoke2PSyCallTrans as
    AlgInvoke2PSyCallTrans is abstract.

    '''
    code = (
        "subroutine alg1()\n"
        "  use kern_mod, only : kern1, kern2\n"
        "  use field_mod, only : field_type\n"
        "  type(field_type) :: field1\n"
        "  real :: invoke_0_kern1, psy_alg1\n"
        "  call invoke(kern1(field1))\n"
        "  call invoke(kern2(field1))\n"
        "end subroutine alg1\n")
    psyir = fortran_reader.psyir_from_source(code)
    AlgTrans().apply(psyir)
    invoke1 = psyir.children[0][0]
    invoke2 = psyir.children[0][1]

    assert isinstance(invoke1, AlgorithmInvokeCall)
    assert isinstance(invoke2, AlgorithmInvokeCall)
    assert len(psyir.walk(AlgorithmInvokeCall)) == 2
    assert len(psyir.walk(KernelFunctor)) == 2
    assert "invoke" in invoke1.scope.symbol_table._symbols
    assert "invoke" in invoke2.scope.symbol_table._symbols

    # Don't call create_psylayer_symbol_root_names() here. This is to
    # check that the transformation creates the names if needed.
    trans = GOceanAlgInvoke2PSyCallTrans()

    # Just transform one of the invoke's. The 'invoke' symbol should still
    # exist.
    trans.apply(invoke1)
    call1 = psyir.children[0][0]
    assert not isinstance(call1, AlgorithmInvokeCall)
    assert isinstance(invoke2, AlgorithmInvokeCall)
    assert len(psyir.walk(AlgorithmInvokeCall)) == 1
    assert len(psyir.walk(KernelFunctor)) == 1
    assert "invoke" in call1.scope.symbol_table._symbols
    assert "invoke" in invoke2.scope.symbol_table._symbols

    # Now transform the second invoke. The 'invoke' symbol should be removed.
    trans.apply(invoke2)
    call2 = psyir.children[0][1]

    assert not psyir.walk(AlgorithmInvokeCall)
    assert not psyir.walk(KernelFunctor)
    assert "invoke" not in call1.scope.symbol_table._symbols
    assert "invoke" not in call2.scope.symbol_table._symbols

    assert call1.routine.name == "invoke_0_kern1_1"
    assert call1.routine.interface.container_symbol.name == "psy_alg1_1"

    assert call2.routine.name == "invoke_1_kern2"
    assert call2.routine.interface.container_symbol.name == "psy_alg1_1"


def test_ai2psycall_apply_invoke_symbols(fortran_reader):
    '''Check that the apply() method removes the appropriate invoke
    symbols when these symbols are within symbol tables that are
    connected to different parts of the PSyIR tree, including where
    the nodes are ancestors of each other. Use
    GOceanAlgInvoke2PSyCallTrans as AlgInvoke2PSyCallTrans is
    abstract.

    '''
    code = (
        "module mod1\n"
        "contains\n"
        "subroutine alg1()\n"
        "  use kern_mod, only : kern1\n"
        "  use field_mod, only : field_type\n"
        "  type(field_type) :: field\n"
        "  call invoke(kern1(field))\n"
        "end subroutine alg1\n"
        "subroutine alg2()\n"
        "  use kern_mod, only : kern2\n"
        "  use field_mod, only : field_type\n"
        "  type(field_type) :: field\n"
        "  call invoke(kern2(field))\n"
        "end subroutine alg2\n"
        "end module\n")
    psyir = fortran_reader.psyir_from_source(code)
    AlgTrans().apply(psyir)
    # Add an invoke symbol to the module (Container node)
    container = psyir.children[0]
    assert isinstance(container, Container)
    container.symbol_table.add(RoutineSymbol("invoke"))

    invoke1 = container.children[0][0]
    assert isinstance(invoke1, AlgorithmInvokeCall)
    invoke2 = container.children[1][0]
    assert isinstance(invoke2, AlgorithmInvokeCall)

    assert "invoke" in invoke1.scope.symbol_table._symbols
    assert "invoke" in invoke2.scope.symbol_table._symbols

    trans = GOceanAlgInvoke2PSyCallTrans()
    trans.apply(invoke1)
    trans.apply(invoke2)

    call1 = container.children[0][0]
    call2 = container.children[1][0]
    assert "invoke" not in call1.scope.symbol_table._symbols
    assert "invoke" not in call2.scope.symbol_table._symbols

    container = psyir.children[0]
    assert "invoke" in container.symbol_table._symbols


def test_ai2psycall_apply_invoke_symbols_scope(fortran_reader):
    '''Check that the apply() method removes the appropriate invoke symbol
    when it is in a different scope to the invoke call.  Use
    GOceanAlgInvoke2PSyCallTrans as AlgInvoke2PSyCallTrans is
    abstract.

    '''
    code = (
        "subroutine alg()\n"
        "  use kern_mod, only : kern\n"
        "  use field_mod, only : field_type\n"
        "  type(field_type) :: field\n"
        "  call invoke(kern(field))\n"
        "end subroutine alg\n")
    psyir = fortran_reader.psyir_from_source(code)
    AlgTrans().apply(psyir)
    # Move the invoke so the invoke symbol is in a different scope to
    # the invoke call.
    invoke = psyir.children[0][0]
    assert isinstance(invoke, AlgorithmInvokeCall)
    symbol = invoke.scope.symbol_table.new_symbol(
        root_name="i", symbol_type=DataSymbol, datatype=INTEGER_TYPE)
    loop = Loop.create(
        symbol, Literal("0", INTEGER_TYPE), Literal("1", INTEGER_TYPE),
        Literal("1", INTEGER_TYPE), [invoke.detach()])
    psyir.children[0].children.append(loop)

    assert "invoke" not in invoke.scope.symbol_table._symbols
    assert "invoke" in loop.scope.symbol_table._symbols

    trans = GOceanAlgInvoke2PSyCallTrans()
    trans.apply(invoke)

    assert not psyir.walk(AlgorithmInvokeCall)
    assert not psyir.walk(KernelFunctor)
    loop = psyir.children[0][0]
    invoke = loop.loop_body[0]
    assert "invoke" not in invoke.scope.symbol_table._symbols
    assert "invoke" not in loop.scope.symbol_table._symbols


def test_ai2psycall_add_arg():
    '''Test the _add_arg() utility method.'''

    # Invalid argument exception (not a Node)
    with pytest.raises(TypeError) as info:
        AlgInvoke2PSyCallTrans._add_arg(None, [])
    assert ("Expected Algorithm-layer kernel arguments to be a Literal, "
            "Reference or CodeBlock, but found 'NoneType'."
            in str(info.value))

    # Invalid argument exception (Node parent is not a KernelFunctor)
    arg = UnaryOperation.create(
        UnaryOperation.Operator.PLUS, Literal("1.0", REAL_TYPE))
    with pytest.raises(TypeError) as info:
        AlgInvoke2PSyCallTrans._add_arg(arg, [])
    assert ("Expected Algorithm-layer kernel arguments to be a Literal, "
            "Reference or CodeBlock, but '+1.0' is of type 'UnaryOperation'."
            in str(info.value))

    # Invalid argument exception (Node parent is a KernelFunctor)
    _ = KernelFunctor.create(
        DataTypeSymbol("my_kernel", REAL_TYPE), [arg])
    with pytest.raises(TypeError) as info:
        AlgInvoke2PSyCallTrans._add_arg(arg, [])
    assert ("Expected Algorithm-layer kernel arguments to be a Literal, "
            "Reference or CodeBlock, but '+1.0' passed to kernel "
            "'my_kernel' is of type 'UnaryOperation'." in str(info.value))

    # literal (nothing added)
    args = []
    AlgInvoke2PSyCallTrans._add_arg(Literal("1.0", REAL_TYPE), args)
    assert args == []

    # reference (arg added)
    name = "hello1"
    AlgInvoke2PSyCallTrans._add_arg(
        Reference(DataSymbol(name, REAL_TYPE)), args)
    assert len(args) == 1
    assert isinstance(args[0], Reference)
    assert args[0].name == name

    # array reference (arg added)
    name = "hello2"
    AlgInvoke2PSyCallTrans._add_arg(ArrayReference.create(DataSymbol(
        name, ArrayType(REAL_TYPE, [10])), [Literal("1", INTEGER_TYPE)]), args)
    assert len(args) == 2
    assert isinstance(args[1], ArrayReference)
    assert args[1].name == name

    # arg, same name, not added
    name = "hello1"
    AlgInvoke2PSyCallTrans._add_arg(
        Reference(DataSymbol(name, REAL_TYPE)), args)
    assert len(args) == 2

    # codeblock arg
    AlgInvoke2PSyCallTrans._add_arg(CodeBlock([], None), args)
    assert len(args) == 3
    assert isinstance(args[2], CodeBlock)


def test_ai2psycall_remove_imported_symbols(fortran_reader):
    '''Check that the remove_imported_symbols() method removes the kernel
    functor symbol and the associated container at the appropriate
    time when it is in a different scope to the invoke call and the
    kernel is used in multiple locations. Use
    GOceanAlgInvoke2PSyCallTrans as AlgInvoke2PSyCallTrans is
    abstract.

    '''
    code = (
        "module alg_mod\n"
        "use kern_mod, only : kern\n"
        "contains\n"
        "subroutine alg1()\n"
        "  use field_mod, only : field_type\n"
        "  type(field_type) :: field\n"
        "  call invoke(kern(field))\n"
        "end subroutine alg1\n"
        "subroutine alg2()\n"
        "  use field_mod, only : field_type\n"
        "  type(field_type) :: field\n"
        "  call invoke(kern(field))\n"
        "end subroutine alg2\n"
        "end module\n")
    psyir = fortran_reader.psyir_from_source(code)
    AlgTrans().apply(psyir)
    module = psyir.children[0]
    assert module.name == "alg_mod"

    assert "kern" in module.symbol_table._symbols
    assert "kern_mod" in module.symbol_table._symbols

    invokes = psyir.walk(AlgorithmInvokeCall)
    assert len(invokes) == 2
    trans = GOceanAlgInvoke2PSyCallTrans()

    trans.apply(invokes[0])
    assert "kern" in module.symbol_table._symbols
    assert "kern_mod" in module.symbol_table._symbols

    trans.apply(invokes[1])
    assert "kern" not in module.symbol_table._symbols
    assert "kern_mod" not in module.symbol_table._symbols
