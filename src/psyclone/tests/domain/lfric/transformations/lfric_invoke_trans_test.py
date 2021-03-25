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
    AlgorithmInvokeCall, KernelLayerRef
from psyclone.domain.common.transformations import InvokeTrans
from psyclone.domain.lfric.transformations import LFRicInvokeTrans
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

    :param klr: the KernelLayerRef node being tested.
    :type klr: :py:class:`psyclone.domain.common.algorithm.KernelLayerRef`
    :param str name: the name of the symbol within a reference that is \
        an argument to klr.
    :param str arg_name: the name of the argument passed to the ..
        an argument to klr.

    '''
    assert type(klr) == KernelLayerRef
    assert klr.symbol.name == name
    assert len(klr.children) == 1
    arg = klr.children[0]
    assert type(arg) == Reference
    assert arg.symbol.name == arg_name


def check_literal(klr, name, arg_value):
    '''Utility routine that checks that the kernel layer metadata
    reference argument has the expected structure if its argument is a
    literal.

    :param klr: the KernelLayerRef node being tested.
    :type klr: :py:class:`psyclone.domain.common.algorithm.KernelLayerRef`

    :param str value: the value of the literal that is an argument to klr.

    '''
    assert type(klr) == KernelLayerRef
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
    '''Check that an LFRicInvokeTrans instance can be created correctly, has
    the expected defaults, deals with any __init__ arguments and its
    name method returns the expected value.

    '''
    invoke_trans = LFRicInvokeTrans()
    assert invoke_trans._invoke_name == "invoke"
    assert invoke_trans.name == "LFRicInvokeTrans"
    invoke_trans = LFRicInvokeTrans(invoke_name="test")
    assert invoke_trans._invoke_name == "test"


def test_structure_contructor():
    '''Test that validation does not raise an exception if the fparser2
    node is a structure constructor.

    '''
    code = (
        "subroutine alg()\n"
        "  use kern_mod\n"
        "  call invoke(kern(1.0))\n"
        "end subroutine alg\n")

    psyir = create_psyir(code)
    lfric_invoke_trans = LFRicInvokeTrans()

    lfric_invoke_trans.validate(psyir[0])
    lfric_invoke_trans._validate_fp2_node(
        psyir[0].children[0]._fp2_nodes[0])


@pytest.mark.parametrize("string", ["error = 'hello'", "name = 0"])
def test_named_arg_error(string):
    '''Test that the validation method raises an exception if a named
    argument has an unsupported format.

    '''
    code = (
        "subroutine alg()\n"
        "  use kern_mod\n"
        "  call invoke({0})\n"
        "end subroutine alg\n".format(string))

    psyir = create_psyir(code)
    lfric_invoke_trans = LFRicInvokeTrans()

    with pytest.raises(TransformationError) as info:
        lfric_invoke_trans.validate(psyir[0])
    assert ("Error in LFRicInvokeTrans transformation. If there is a named argument, it must take the form "
            "name='str', but found '{0}'.".format(string) in str(info.value))
    
    with pytest.raises(TransformationError) as info:
        lfric_invoke_trans._validate_fp2_node(
            psyir[0].children[0]._fp2_nodes[0])
    assert ("Error in LFRicInvokeTrans transformation. If there is a named argument, it must take the form "
            "name='str', but found '{0}'.".format(string) in str(info.value))


def test_multi_named_arg_error():
    '''Test that the validation method raises an exception if more than
    one named argument is specified in an invoke call. Also check that
    the apply method calls the validate method.

    '''
    code = (
        "subroutine alg()\n"
        "  use kern_mod\n"
        "  call invoke(name='first', name='second')\n"
        "end subroutine alg\n")

    psyir = create_psyir(code)
    lfric_invoke_trans = LFRicInvokeTrans()

    with pytest.raises(TransformationError) as info:
        lfric_invoke_trans.validate(psyir[0])
    assert ("Error in LFRicInvokeTrans transformation. There should be at "
            "most one named argument in an invoke, but there are at least "
            "two: 'first' and 'second'." in str(info.value))

    with pytest.raises(TransformationError) as info:
        lfric_invoke_trans.apply(psyir[0])
    assert ("Error in LFRicInvokeTrans transformation. There should be at "
            "most one named argument in an invoke, but there are at least "
            "two: 'first' and 'second'." in str(info.value))


def test_codeblock_invalid(monkeypatch):
    '''Test that the expected exception is raised if unsupported content
    is found within a codeblock. Use monkeypatch to sabotage the
    codeblock to cause the exception.

    '''
    code = (
        "subroutine alg()\n"
        "  use kern_mod\n"
        "  call invoke(name='tallulah')\n"
        "end subroutine alg\n")

    psyir = create_psyir(code)
    code_block = psyir[0].children[0]
    assert isinstance(code_block, CodeBlock)
    monkeypatch.setattr(code_block, "_fp2_nodes", [None])

    lfric_invoke_trans = LFRicInvokeTrans()

    with pytest.raises(TransformationError) as info:
        lfric_invoke_trans.validate(psyir[0])
    assert ("Expecting an algorithm invoke codeblock to contain either "
            "Structure-Constructor or actual-arg-spec, but found "
            "'NoneType'." in str(info.value))

# CodedKern - a) arrayref, b) structureconst
def test_apply_codedkern_arrayref():
    ''' xxx '''
    code = (
        "subroutine alg()\n"
        "  use kern_mod\n"
        "  use field_mod, only : field\n"
        "  type(field) :: field1\n"
        "  call invoke(kern(field1))\n"
        "end subroutine alg\n")

    psyir = create_psyir(code)
    lfric_invoke_trans = LFRicInvokeTrans()

    lfric_invoke_trans.apply(psyir[0])

    psyir.view()
    exit(1)

# BuiltinKern- a) arrayref, b) structureconst
# name
# mixture
