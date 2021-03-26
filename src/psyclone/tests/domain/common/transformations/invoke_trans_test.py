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
from psyclone.domain.common.algorithm import \
    AlgorithmInvokeCall, KernelFunctor
from psyclone.domain.common.transformations import InvokeTrans
from psyclone.psyir.transformations import TransformationError
from psyclone.psyir.nodes import Call, CodeBlock, Literal, Reference, \
    ArrayReference, Literal, BinaryOperation
from psyclone.psyir.symbols import RoutineSymbol, TypeSymbol, Symbol, \
    StructureType
from psyclone.psyir.nodes.node import ChildrenList


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
    assert type(klr) == KernelFunctor
    assert klr.symbol.name == name
    assert len(klr.children) == 1
    arg = klr.children[0]
    assert type(arg) == Reference
    assert arg.symbol.name == arg_name


def check_literal(klr, name, arg_value):
    '''Utility routine that checks that the kernel layer metadata
    reference argument has the expected structure if its argument is a
    literal.

    :param klr: the KernelFunctor node being tested.
    :type klr: :py:class:`psyclone.domain.common.algorithm.KernelFunctor`

    :param str value: the value of the literal that is an argument to klr.

    '''
    assert type(klr) == KernelFunctor
    assert klr.symbol.name == name
    assert len(klr.children) == 1
    arg = klr.children[0]
    assert type(arg) == Literal
    assert arg.value == arg_value


def create_psyir(code):
    ''' Utility to create a PSyIR tree from Fortran code.
    
    :param str code: Fortran code encoded as a string

    :returns: psyir tree representing the Fortran code
    :rtype: :py:class:`psyclone.psyir.nodes.Node`

    '''
    fortran_reader = FortranStringReader(code)
    f2008_parser = ParserFactory().create(std="f2008")
    parse_tree = f2008_parser(fortran_reader)

    psyir_reader = Fparser2Reader()
    psyir = psyir_reader.generate_psyir(parse_tree)

    return psyir


def test_init():
    '''Check that an InvokeTrans instance can be created correctly, has
    the expected defaults, deals with any __init__ arguments and its
    name method returns the expected value.

    '''
    invoke_trans = InvokeTrans()
    assert invoke_trans._invoke_name == "invoke"
    assert invoke_trans.name == "InvokeTrans"
    invoke_trans = InvokeTrans(invoke_name="test")
    assert invoke_trans._invoke_name == "test"


def test_parse_args_get_symbol():
    '''Test that the parse_args and get_symbol methods work as
    expected.

    '''
    code = (
        "subroutine alg()\n"
        "  use kern_mod\n"
        "  call invoke(kern(1.0))\n"
        "end subroutine alg\n")

    psyir = create_psyir(code)
    code_block = psyir[0].children[0]
    assert isinstance(code_block, CodeBlock)

    # Check expected output from parse_args
    nodes = InvokeTrans._parse_args(code_block, code_block._fp2_nodes[0])
    assert isinstance(nodes, ChildrenList)
    assert len(nodes) == 1
    assert isinstance(nodes[0], Literal)
    assert nodes[0].value == "1.0"

    # Check expected output from get_symbol when no symbol exists
    with pytest.raises(KeyError):
        _ = code_block.scope.symbol_table.lookup("kern")
    symbol = InvokeTrans._get_symbol(code_block, code_block._fp2_nodes[0])
    assert isinstance(symbol, TypeSymbol)
    assert symbol.name == "kern"
    symbol2 = code_block.scope.symbol_table.lookup("kern")
    assert symbol2 is symbol

    # Check expected output from get_symbol when symbol already exists
    symbol3 = InvokeTrans._get_symbol(code_block, code_block._fp2_nodes[0])
    assert symbol3 is symbol

    
def test_specialise_symbol():
    '''Test that the specialise_symbol method work as expected.

    '''
    symbol = Symbol("hello")

    # Check that a Symbol is specialised
    InvokeTrans._specialise_symbol(symbol)
    assert isinstance(symbol, TypeSymbol)
    assert isinstance(symbol.datatype, StructureType)

    # Check that something that is not a symbol is ignored
    test = "hello"
    InvokeTrans._specialise_symbol(test)
    assert isinstance(test, str)
    assert test == "hello"


def test_call_error():
    '''Test that the expected exception is raised in the validate method
    when the supplied node is the wrong type. Also check that the
    validate method from within the apply method.

    '''
    invoke_trans = InvokeTrans()
    with pytest.raises(TransformationError) as info:
        invoke_trans.validate("hello")
    assert ("Error in InvokeTrans transformation. The supplied call argument "
            "should be a `Call` node but found 'str'." in str(info.value))

    # Check that validate is called via the apply method
    with pytest.raises(TransformationError) as info:
        invoke_trans.apply("hello")
    assert ("Error in InvokeTrans transformation. The supplied call argument "
            "should be a `Call` node but found 'str'." in str(info.value))


def test_invoke_error():
    '''Test that the expected exception is raised in the validate method
    when the supplied node is a call but its name is not the expected
    'invoke' name.

    '''
    invoke_trans = InvokeTrans()
    with pytest.raises(TransformationError) as info:
        invoke_trans.validate(Call(RoutineSymbol("hello")))
    assert ("Error in InvokeTrans transformation. The supplied call argument "
            "should be a `Call` node with name 'invoke' but found 'hello'."
            in str(info.value))


def test_array_reference():
    '''Test that the validate method does not raise an exception if a
    PSyIR ArrayReference is found.

    '''
    code = (
        "subroutine alg()\n"
        "  use kern_mod\n"
        "  use field_mod, only : r2d_field\n"
        "  type(r2d_field) :: field\n"
        "  call invoke(kern(field))\n"
        "end subroutine alg\n")

    psyir = create_psyir(code)
    assert isinstance(psyir[0].children[0], ArrayReference)
    invoke_trans = InvokeTrans()
    invoke_trans.validate(psyir[0])


def test_code_block_error():
    '''Test that the validate method raises an exception if unexpected
    content is found in a CodeBlock.

    '''
    code = (
        "subroutine alg()\n"
        "  use kern_mod\n"
        "  use field_mod, only : r2d_field\n"
        "  type(r2d_field) :: field\n"
        "  call invoke(field=field)\n"
        "end subroutine alg\n")

    psyir = create_psyir(code)
    invoke_trans = InvokeTrans()
    with pytest.raises(TransformationError) as info:
        invoke_trans.validate(psyir[0])
    assert ("The supplied call argument contains a CodeBlock with content "
            "(Actual_Arg_Spec) which is not a StructureConstructor."
            in str(info.value))


def test_arg_error():
    '''Test that the validate method raises an exception if unexpected
    content is found as an argument to an invoke.

    '''
    code = (
        "subroutine alg()\n"
        "  use kern_mod\n"
        "  use field_mod, only : r2d_field\n"
        "  type(r2d_field) :: field\n"
        "  call invoke('hello')\n"
        "end subroutine alg\n")

    psyir = create_psyir(code)
    invoke_trans = InvokeTrans()
    with pytest.raises(TransformationError) as info:
        invoke_trans.validate(psyir[0])
    assert ("The arguments to this invoke call are expected to be a "
            "CodeBlock or an ArrayReference, but found 'Literal'."
            in str(info.value))


def test_apply_arrayref():
    '''Test that an invoke with an array reference argument is transformed
    into PSyclone-specific AlgorithmInvokeCall and KernelFunctor
    classes.

    '''
    code = (
        "subroutine alg()\n"
        "  use kern_mod, only: kern\n"
        "  use field_mod, only: r2d_field\n"
        "  type(r2d_field) :: field\n"
        "  call invoke(kern(field))\n"
        "end subroutine alg\n")

    psyir = create_psyir(code)
    assert len(psyir[0].children) == 1
    assert isinstance(psyir[0].children[0], ArrayReference)

    invoke_trans = InvokeTrans()
    invoke_trans.apply(psyir[0])

    invoke = psyir.children[0]
    assert type(invoke) == AlgorithmInvokeCall
    assert len(invoke.children) == 1
    check_reference(invoke.children[0], "kern", "field")


def test_apply_codeblock():
    '''Test that an invoke with a code block argument is transformed
    into PSyclone-specific AlgorithmInvokeCall and KernelFunctor
    classes.

    '''
    code = (
        "subroutine alg()\n"
        "  use kern_mod, only: kern\n"
        "  call invoke(kern(0.0))\n"
        "end subroutine alg\n")

    psyir = create_psyir(code)
    assert len(psyir[0].children) == 1
    assert isinstance(psyir[0].children[0], CodeBlock)

    invoke_trans = InvokeTrans()
    invoke_trans.apply(psyir[0])

    invoke = psyir.children[0]
    assert type(invoke) == AlgorithmInvokeCall
    assert len(invoke.children) == 1
    check_literal(invoke.children[0], "kern", "0.0")


def test_apply_codeblocks():
    '''Test that an invoke with a code block argument containing multiple
    structure constructors is transformed into PSyclone-specific
    AlgorithmInvokeCall and KernelFunctor classes.

    '''
    code = (
        "subroutine alg()\n"
        "  use kern_mod, only: kern\n"
        "  call invoke(kern(0.0), kern(1.0))\n"
        "end subroutine alg\n")

    psyir = create_psyir(code)
    assert len(psyir[0].children) == 1
    assert isinstance(psyir[0].children[0], CodeBlock)

    invoke_trans = InvokeTrans()
    invoke_trans.apply(psyir[0])

    invoke = psyir.children[0]
    assert type(invoke) == AlgorithmInvokeCall
    assert len(invoke.children) == 2
    check_literal(invoke.children[0], "kern", "0.0")
    check_literal(invoke.children[1], "kern", "1.0")


def test_apply_mixed():
    '''Test that an invoke with a mixture of code block and array
    reference arguments is transformed into PSyclone-specific
    AlgorithmInvokeCall and KernelFunctor classes.

    '''
    code = (
        "subroutine alg()\n"
        "  use kern_mod, only: kern\n"
        "  use field_mod, only: r2d_field\n"
        "  type(r2d_field) :: field\n"
        "  call invoke(kern(0.0), kern(1.0), kern(field), kern(2.0))\n"
        "end subroutine alg\n")

    psyir = create_psyir(code)
    assert len(psyir[0].children) == 3
    assert isinstance(psyir[0].children[0], CodeBlock)
    assert isinstance(psyir[0].children[1], ArrayReference)
    assert isinstance(psyir[0].children[2], CodeBlock)

    invoke_trans = InvokeTrans()
    invoke_trans.apply(psyir[0])

    invoke = psyir.children[0]
    assert type(invoke) == AlgorithmInvokeCall
    assert len(invoke.children) == 4
    check_literal(invoke.children[0], "kern", "0.0")
    check_literal(invoke.children[1], "kern", "1.0")
    check_reference(invoke.children[2], "kern", "field")
    check_literal(invoke.children[3], "kern", "2.0")


def test_apply_expr():
    '''Test that an invoke with a mixture of code block and array
    reference arguments as expresssions is transformed into PSyclone-specific
    AlgorithmInvokeCall and KernelFunctor classes.

    '''
    code = (
        "subroutine alg()\n"
        "  use kern_mod, only: kern\n"
        "  use field_mod, only: r2d_field\n"
        "  type(r2d_field) :: field\n"
        "  call invoke(kern((field+field)/2), kern((field+field)/2,1.0))\n"
        "end subroutine alg\n")

    psyir = create_psyir(code)
    assert len(psyir[0].children) == 2
    assert isinstance(psyir[0].children[0], ArrayReference)
    assert isinstance(psyir[0].children[1], CodeBlock)

    invoke_trans = InvokeTrans()
    invoke_trans.apply(psyir[0])

    invoke = psyir.children[0]
    assert type(invoke) == AlgorithmInvokeCall
    assert len(invoke.children) == 2

    klr = invoke.children[0]
    assert type(klr) == KernelFunctor
    assert klr.symbol.name == "kern"
    assert len(klr.children) == 1
    arg = klr.children[0]
    assert isinstance(arg, BinaryOperation)

    klr = invoke.children[1]
    assert type(klr) == KernelFunctor
    assert klr.symbol.name == "kern"
    assert len(klr.children) == 2
    arg0 = klr.children[0]
    assert isinstance(arg, BinaryOperation)
