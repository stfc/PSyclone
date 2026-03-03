# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2021-2026, Science and Technology Facilities Council
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
# Modified: A. R. Porter,  S. Siso and A. B. G. Chalk,  STFC Daresbury Lab

'''Module containing tests for the translation of PSyIR to PSyclone
Algorithm PSyIR.

'''
import pytest

from psyclone.psyir.frontend.fortran import FortranReader
from psyclone.psyir.transformations import TransformationError
from psyclone.psyir.nodes import Call, CodeBlock, Reference, \
    Literal, BinaryOperation
from psyclone.psyir.symbols import RoutineSymbol, DataTypeSymbol, Symbol, \
    StructureType

from psyclone.domain.common.algorithm import \
    AlgorithmInvokeCall, KernelFunctor
from psyclone.domain.common.transformations import RaisePSyIR2AlgTrans


def check_reference(klr, name, arg_name):
    '''Utility routine that checks that the kernel layer metadata
    reference argument has the expected structure if its argument is a
    reference.

    :param klr: the KernelFunctor node being tested.
    :type klr: :py:class:`psyclone.domain.common.algorithm.KernelFunctor`
    :param str name: the name of the symbol within a reference that is \
        an argument to klr.
    :param str arg_name: the name of the argument passed to the ..
        an argument to klr.

    '''
    assert isinstance(klr, KernelFunctor)
    assert klr.symbol.name == name
    assert len(klr.children) == 1
    arg = klr.children[0]
    assert isinstance(arg, Reference)
    assert arg.symbol.name == arg_name


def check_literal(klr, name, arg_value):
    '''Utility routine that checks that the kernel layer metadata
    reference argument has the expected structure if its argument is a
    literal.

    :param klr: the KernelFunctor node being tested.
    :type klr: :py:class:`psyclone.domain.common.algorithm.KernelFunctor`

    :param str value: the value of the literal that is an argument to klr.

    '''
    assert isinstance(klr, KernelFunctor)
    assert klr.symbol.name == name
    assert len(klr.children) == 1
    arg = klr.children[0]
    assert isinstance(arg, Literal)
    assert arg.value == arg_value


def test_init():
    '''Check that an RaisePSyIR2AlgTrans instance can be created correctly,
    has the expected defaults, deals with any __init__ arguments and
    its name method returns the expected value.

    '''
    invoke_trans = RaisePSyIR2AlgTrans()
    assert invoke_trans.name == "RaisePSyIR2AlgTrans"
    assert isinstance(invoke_trans, RaisePSyIR2AlgTrans)
    assert invoke_trans._call_name is None


def test_specialise_symbol():
    '''Test that the specialise_symbol method work as expected.

    '''
    symbol = Symbol("hello")

    # Check that a Symbol is specialised
    RaisePSyIR2AlgTrans._specialise_symbol(symbol)
    assert isinstance(symbol, DataTypeSymbol)
    # pylint: disable=no-member
    assert isinstance(symbol.datatype, StructureType)

    # Check that something that is not a symbol is ignored
    test = "hello"
    RaisePSyIR2AlgTrans._specialise_symbol(test)
    assert isinstance(test, str)
    assert test == "hello"


@pytest.mark.parametrize("string", ["error='hello'", "name=0"])
def test_named_arg_error(string):
    '''Test that the validation method raises an exception if a named
    argument has an unsupported format.

    '''
    code = (
        f"subroutine alg()\n"
        f"  use kern_mod\n"
        f"  call invoke({string})\n"
        f"end subroutine alg\n")

    reader = FortranReader()
    psyir = reader.psyir_from_source(code)
    invoke_trans = RaisePSyIR2AlgTrans()

    invoke = psyir.children[0][0]
    with pytest.raises(TransformationError) as info:
        invoke_trans.validate(invoke)
    assert (f"Error in RaisePSyIR2AlgTrans transformation. If there is a "
            f"named argument, it must take the form name='str', but found "
            f"'call invoke({string})\n'." in str(info.value))


def test_multi_named_arg_error():
    '''Test that the validation method raises an exception if more than
    one named argument is specified in an invoke call. Also check that
    the apply method calls the validate method.

    '''
    code = (
        "subroutine alg()\n"
        "  use kern_mod\n"
        "  call invoke(name1='first', name2='second')\n"
        "end subroutine alg\n")

    reader = FortranReader()
    psyir = reader.psyir_from_source(code)
    invoke_trans = RaisePSyIR2AlgTrans()
    invoke = psyir.children[0][0]

    with pytest.raises(TransformationError) as info:
        invoke_trans.validate(invoke)
    assert ("Error in RaisePSyIR2AlgTrans transformation. There should be "
            "at most one named argument in an invoke, but there are 2 in "
            "'call invoke(name1='first', name2='second')\n'."
            in str(info.value))

    invoke_trans._call_name = None
    with pytest.raises(TransformationError) as info:
        invoke_trans.apply(invoke, 0)
    assert ("Error in RaisePSyIR2AlgTrans transformation. There should be "
            "at most one named argument in an invoke, but there are 2 in "
            "'call invoke(name1='first', name2='second')\n'."
            in str(info.value))


def test_codeblock_invalid():
    '''Test that the expected exception is raised if unsupported content
    is found within a codeblock.

    '''
    code = (
        "subroutine alg()\n"
        "  use kern_mod\n"
        "  call invoke('xx'//'xx')\n"
        "end subroutine alg\n")

    reader = FortranReader()
    psyir = reader.psyir_from_source(code)
    invoke = psyir.children[0][0]
    code_block = invoke.arguments[0]
    assert isinstance(code_block, CodeBlock)

    invoke_trans = RaisePSyIR2AlgTrans()

    with pytest.raises(TransformationError) as info:
        invoke_trans.validate(invoke)
    assert ("Error in RaisePSyIR2AlgTrans transformation. The arguments to "
            "this invoke call are expected to be kernel calls which are "
            "represented in generic PSyIR as Calls, but ''xx' // 'xx'' is "
            "of type 'CodeBlock'." in str(info.value))


def test_call_error():
    '''Test that the expected exception is raised in the validate method
    when the supplied node is the wrong type. Also check that the
    validate method from within the apply method.

    '''
    invoke_trans = RaisePSyIR2AlgTrans()
    with pytest.raises(TransformationError) as info:
        invoke_trans.validate("hello")
    assert ("Error in RaisePSyIR2AlgTrans transformation. The supplied call "
            "argument should be a `Call` node but found 'str'."
            in str(info.value))

    # Check that validate is called via the apply method
    with pytest.raises(TransformationError) as info:
        invoke_trans.apply("hello", 0)
    assert ("Error in RaisePSyIR2AlgTrans transformation. The supplied call "
            "argument should be a `Call` node but found 'str'."
            in str(info.value))


def test_invoke_error():
    '''Test that the expected exception is raised in the validate method
    when the supplied node is a call but its name is not the expected
    'invoke' name.

    '''
    invoke_trans = RaisePSyIR2AlgTrans()
    with pytest.raises(TransformationError) as info:
        invoke_trans.validate(Call.create(RoutineSymbol("hello")))
    assert ("Error in RaisePSyIR2AlgTrans transformation. The supplied call "
            "argument should be a `Call` node with name 'invoke' but "
            "found 'hello'." in str(info.value))


def test_invoke_kern_functor_argument(fortran_reader):
    '''Test that the validate method does not raise an exception if a
    PSyIR Call (representing a kern functor) is found.

    '''
    code = (
        "subroutine alg()\n"
        "  use kern_mod\n"
        "  use field_mod, only : r2d_field\n"
        "  type(r2d_field) :: field\n"
        "  call invoke(kern(field))\n"
        "end subroutine alg\n")

    psyir = fortran_reader.psyir_from_source(code)
    subroutine = psyir.children[0]
    assert isinstance(subroutine[0].arguments[0], Call)
    invoke_trans = RaisePSyIR2AlgTrans()
    invoke_trans.validate(subroutine[0])


@pytest.mark.parametrize("arg", ["0", "'hello'"])
def test_arg_error(fortran_reader, arg):
    '''Test that the validate method raises an exception if unexpected
    content is found as an argument to an invoke.

    '''
    code = (
        f"subroutine alg()\n"
        f"  use kern_mod\n"
        f"  use field_mod, only : r2d_field\n"
        f"  type(r2d_field) :: field\n"
        f"  call invoke({arg})\n"
        f"end subroutine alg\n")

    psyir = fortran_reader.psyir_from_source(code)
    invoke_trans = RaisePSyIR2AlgTrans()
    with pytest.raises(TransformationError) as info:
        invoke_trans.validate(psyir.children[0][0])
    if arg == "alg(field)":
        assert ("Error in RaisePSyIR2AlgTrans transformation. The invoke "
                "call argument 'alg' has been used as the Algorithm routine "
                "name. This is not allowed." in str(info.value))
    else:
        assert (
            f"The arguments to this invoke call are expected to be kernel "
            f"calls which are represented in generic PSyIR as Calls, "
            f"but '{arg}' is of type 'Literal'."
            in str(info.value))


def test_apply_call(fortran_reader):
    '''Test that an invoke with an Call argument is uplifted
    to AlgorithmInvokeCall and KernelFunctor classes.

    '''
    code = (
        "subroutine alg()\n"
        "  use kern_mod, only: kern, kern1, kern2\n"
        "  use field_mod, only: r2d_field\n"
        "  type(r2d_field) :: field\n"
        "  call invoke(kern(field))\n"
        "  call invoke(kern1(0.0), kern2(1.0), name='an_invoke')\n"
        "end subroutine alg\n")

    psyir = fortran_reader.psyir_from_source(code)
    subroutine = psyir.children[0]
    assert len(subroutine[0].arguments) == 1
    assert isinstance(subroutine[0].arguments[0], Call)

    invoke_trans = RaisePSyIR2AlgTrans()
    invoke_trans.apply(subroutine[0], 1)

    invoke = subroutine[0]
    assert isinstance(invoke, AlgorithmInvokeCall)
    assert invoke._index == 1
    assert len(invoke.arguments) == 1
    check_reference(invoke.arguments[0], "kern", "field")

    invoke_trans.apply(subroutine[1], 2)
    invoke = subroutine.children[1]
    assert isinstance(invoke, AlgorithmInvokeCall)
    assert invoke._name == "an_invoke"
    assert invoke._index == 2
    assert len(invoke.arguments) == 2
    check_literal(invoke.arguments[0], "kern1", "0.0")
    check_literal(invoke.arguments[1], "kern2", "1.0")


def test_apply_expr(fortran_reader):
    '''Test that an invoke is always parsed as a Call and after applying
    the RaisePSyIR2AlgTrans this are uplifted to AlgorithmInvokeCall and
    KernelFunctor classes.

    '''
    code = (
        "subroutine alg()\n"
        "  use kern_mod, only: kern\n"
        "  use field_mod, only: r2d_field\n"
        "  type(r2d_field) :: field\n"
        "  call invoke(kern((field+field)/2), kern((field+field)/2,1.0))\n"
        "end subroutine alg\n")

    psyir = fortran_reader.psyir_from_source(code)
    subroutine = psyir.children[0]
    assert len(subroutine[0].arguments) == 2
    assert isinstance(subroutine[0].arguments[0], Call)
    assert isinstance(subroutine[0].arguments[1], Call)

    invoke_trans = RaisePSyIR2AlgTrans()
    invoke_trans.apply(subroutine[0], 5)

    invoke = subroutine[0]
    assert isinstance(invoke, AlgorithmInvokeCall)
    assert invoke._index == 5
    assert len(invoke.arguments) == 2

    klr = invoke.arguments[0]
    assert isinstance(klr, KernelFunctor)
    assert klr.symbol.name == "kern"
    assert len(klr.children) == 1
    arg = klr.children[0]
    assert isinstance(arg, BinaryOperation)

    klr = invoke.arguments[1]
    assert isinstance(klr, KernelFunctor)
    assert klr.symbol.name == "kern"
    assert len(klr.children) == 2
    arg = klr.children[0]
    assert isinstance(arg, BinaryOperation)


def test_multi_name():
    '''Check that the expected exception is raised if a name is provided
    more than once.

    '''
    code = (
        "subroutine alg()\n"
        "  use kern_mod\n"
        "  call invoke(name='Sancho', name2='Fernandes')\n"
        "end subroutine alg\n")

    reader = FortranReader()
    psyir = reader.psyir_from_source(code)
    invoke_trans = RaisePSyIR2AlgTrans()

    with pytest.raises(TransformationError) as info:
        invoke_trans.validate(psyir.children[0][0])
    assert ("There should be at most one named argument in an invoke, but "
            "there are 2 in 'call invoke(name='Sancho', name2='Fernandes')\n'."
            in str(info.value))


def test_apply_keep_comments():
    '''Test that comment nodes are correctly kept on an invoke when the
    RaisePSyIR2AlgTrans is applied.
    '''
    code = (
        "subroutine alg()\n"
        "  use kern_mod, only: kern\n"
        "  use field_mod, only: r2d_field\n"
        "  type(r2d_field) :: field\n"
        " ! preceding comment\n"
        "  call invoke(kern(field)) !inline comment\n"
        "end subroutine alg\n")

    fortran_reader = FortranReader(ignore_comments=False)
    psyir = fortran_reader.psyir_from_source(code)
    subroutine = psyir.children[0]
    assert len(subroutine[0].arguments) == 1
    assert isinstance(subroutine[0].arguments[0], Call)

    invoke_trans = RaisePSyIR2AlgTrans()
    invoke_trans.apply(subroutine[0], 1)

    invoke = subroutine[0]
    assert invoke.preceding_comment == "preceding comment"
    assert invoke.inline_comment == "inline comment"
