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

'''Module containing tests for the translation of PSyIR to LFRic
Algorithm PSyIR.

'''
from __future__ import absolute_import
import pytest

from fparser.two.parser import ParserFactory
from fparser.common.readfortran import FortranStringReader
from psyclone.psyir.frontend.fparser2 import Fparser2Reader
from psyclone.psyir.nodes import Reference, Literal
from psyclone.domain.lfric.algorithm import \
    psyir_to_algpsyir, AlgorithmInvokeCall, CodedCall, BuiltinCall


def check_kernel(call):
    ''' xxx '''
    assert type(call) ==  CodedCall
    assert call.routine.name == "kern_type"
    assert len(call.children) == 1
    arg = call.children[0]
    assert type(arg) == Reference
    assert arg.symbol.name == "field"


def check_builtin(call):
    ''' xxx '''
    assert type(call) ==  BuiltinCall
    assert call.routine.name == "setval_c"
    assert len(call.children) == 2
    arg0 = call.children[0]
    assert type(arg0) == Reference
    assert arg0.symbol.name == "field"
    arg1 = call.children[1]
    assert type(arg1) == Literal
    assert arg1.value == "0.0"


@pytest.mark.parametrize("name", [None, "'invoke with kernel'"])
def test_kern(name):
    '''Test that an invoke containing a kernel within an LFRic algorithm
    layer is transformed into LFRic-specific AlgorithmInvokeCall and
    CodedCall classes. Also test that the optional name='xxx' argument
    is captured correctly.

    '''
    name_arg=""
    if name:
        name_arg = ", name={0}".format(name)
    code = (
        "module algorithm_mod\n"
        "use kern_mod, only: kern_type\n"
        "use field_mod, only: field_type\n"
        "contains\n"
        "  subroutine alg()\n"
        "    type(field_type) :: field\n"
        "    call invoke(kern_type(field){0})\n"
        "  end subroutine alg\n"
        "end module algorithm_mod\n".format(name_arg))

    fortran_reader = FortranStringReader(code)
    f2008_parser = ParserFactory().create(std="f2008")
    parse_tree = f2008_parser(fortran_reader)

    psyir_reader = Fparser2Reader()
    psyir = psyir_reader.generate_psyir(parse_tree)

    psyir_to_algpsyir(psyir)

    invoke = psyir.children[0][0]
    assert type(invoke) == AlgorithmInvokeCall
    assert invoke._description == name
    assert len(invoke.children) == 1
    check_kernel(invoke.children[0])


@pytest.mark.parametrize("name", [None, "'invoke with builtin'"])
def test_builtin(name):
    '''Test that an invoke containing a builtin within an LFRic algorithm
    layer is transformed into LFRic-specific AlgorithmInvokeCall and
    BuiltinCall classes. Also test that the optional name='xxx'
    argument is captured correctly.

    '''
    name_arg=""
    if name:
        name_arg = ", name={0}".format(name)
    code = (
        "module algorithm_mod\n"
        "use field_mod, only: field_type\n"
        "use psyclone_builtins, only : setval_c\n"
        "contains\n"
        "  subroutine alg()\n"
        "    type(field_type) :: field\n"
        "    call invoke(setval_c(field, 0.0){0})\n"
        "  end subroutine alg\n"
        "end module algorithm_mod\n".format(name_arg))

    fortran_reader = FortranStringReader(code)
    f2008_parser = ParserFactory().create(std="f2008")
    parse_tree = f2008_parser(fortran_reader)

    psyir_reader = Fparser2Reader()
    psyir = psyir_reader.generate_psyir(parse_tree)

    psyir_to_algpsyir(psyir)

    invoke = psyir.children[0][0]
    assert type(invoke) == AlgorithmInvokeCall
    assert invoke._description == name
    assert len(invoke.children) == 1
    check_builtin(invoke.children[0])


def test_mixed_multi_invoke():
    '''Test that multiple invokes containing a mixture of kernels and
    builtins within an LFRic algorithm layer are transformed into
    LFRic-specific AlgorithmInvokeCall, KernelCall and BuiltinCall
    classes. Also test that the optional name='xxx' argument is
    captured correctly.

    '''
    code = (
        "module algorithm_mod\n"
        "use field_mod, only: field_type\n"
        "use kern_mod, only: kern_type\n"
        "use psyclone_builtins, only : setval_c\n"
        "contains\n"
        "  subroutine alg()\n"
        "    type(field_type) :: field\n"
        "    call invoke(setval_c(field, 0.0), &\n"
        "                kern_type(field))\n"
        "    call invoke(name='named invoke', &\n"
        "                setval_c(field, 0.0), &\n"
        "                kern_type(field))\n"
        "  end subroutine alg\n"
        "end module algorithm_mod\n")

    fortran_reader = FortranStringReader(code)
    f2008_parser = ParserFactory().create(std="f2008")
    parse_tree = f2008_parser(fortran_reader)

    psyir_reader = Fparser2Reader()
    psyir = psyir_reader.generate_psyir(parse_tree)

    psyir_to_algpsyir(psyir)

    for index, name in enumerate([None, "'named invoke'"]):
        invoke = psyir.children[0][index]
        assert type(invoke) == AlgorithmInvokeCall
        assert invoke._description == name
        assert len(invoke.children) == 2
        check_builtin(invoke.children[0])
        check_kernel(invoke.children[1])


#Errors ...
