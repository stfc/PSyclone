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
from psyclone.domain.common.transformations import InvokeTrans, AlgPSyIRTrans
from psyclone.psyir.transformations import TransformationError
from psyclone.psyir.nodes import Call, CodeBlock, Literal, Reference, \
    ArrayReference, Literal, BinaryOperation, Routine, Container
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
    '''Check that an InvokeTrans instance can be created correctly, has
    the expected defaults, deals with any __init__ arguments and its
    name method returns the expected value.

    '''
    alg_trans = AlgPSyIRTrans()
    assert alg_trans._invoke_name == "invoke"
    assert alg_trans.name == "AlgPSyIRTrans"
    alg_trans = AlgPSyIRTrans(invoke_name="test")
    assert alg_trans._invoke_name == "test"
    assert isinstance(alg_trans._invoke_trans, InvokeTrans)


def test_validate_node_error():
    '''Test that the validate method raises the expected exception if an
    invalid node argument is provided, or the node is not the root
    node. Also test that the apply method calls the validate method.

    '''
    code = (
        "module alg_mod\n"
        "  contains\n"
        "  subroutine alg()\n"
        "    use kern_mod\n"
        "    call invoke(kern(1.0))\n"
        "  end subroutine alg\n"
        "end module alg_mod\n")

    psyir = create_psyir(code)

    alg_trans = AlgPSyIRTrans()
    with pytest.raises(TransformationError) as info:
        alg_trans.validate("hello")
    assert ("The supplied call argument should be a Routine or Container "
            "node but found 'str'." in str(info.value))

    with pytest.raises(TransformationError) as info:
        alg_trans.apply("hello")
    assert ("The supplied call argument should be a Routine or Container "
            "node but found 'str'." in str(info.value))

    with pytest.raises(TransformationError) as info:
        alg_trans.validate(psyir.children[0])
    assert ("The supplied node should be the root of a PSyIR tree but this "
            "node has a parent." in str(info.value))

    alg_trans.validate(psyir)


# applied to all invoke calls, not anything else
def test_apply():
    '''Test that the apply method behaves as expected.

    '''
    code = (
        "module alg_mod\n"
        "  use kern_mod\n"
        "  contains\n"
        "  subroutine alg1()\n"
        "    call invoke(kern(1.0))\n"
        "    call not_invoke(kern(1.0))\n"
        "    call invoke(kern(1.0))\n"
        "  end subroutine alg1\n"
        "  subroutine alg2()\n"
        "    call invoke(kern(1.0))\n"
        "  end subroutine alg2\n"
        "end module alg_mod\n")

    psyir = create_psyir(code)
    alg_trans = AlgPSyIRTrans()
    assert len(psyir.walk(Call)) == 4
    assert len(psyir.walk(AlgorithmInvokeCall)) == 0
    assert len(psyir.walk(KernelLayerRef)) == 0

    alg_trans.apply(psyir)

    assert len(psyir.walk(Call)) == 4
    assert len(psyir.walk(AlgorithmInvokeCall)) == 3
    assert len(psyir.walk(KernelLayerRef)) == 3
