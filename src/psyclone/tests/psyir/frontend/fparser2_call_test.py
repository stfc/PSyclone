# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2021-2022, Science and Technology Facilities Council.
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


''' Performs py.test tests on the support for calls in the fparser2
    PSyIR front-end '''

from __future__ import absolute_import
import pytest
from fparser.common.readfortran import FortranStringReader
from fparser.two import Fortran2003
from psyclone.psyir.frontend.fparser2 import Fparser2Reader
from psyclone.psyir.symbols import RoutineSymbol, UnresolvedInterface, \
    ImportInterface, NoType
from psyclone.psyir.nodes import Literal, CodeBlock, Schedule, Call, \
    Reference, BinaryOperation
from psyclone.errors import GenerationError


@pytest.mark.usefixtures("f2008_parser")
def test_call_noargs():
    '''Test that fparser2reader transforms a Fortran subroutine call with
    no arguments into the equivalent PSyIR Call node. Also test that a
    new RoutineSymbol is added to the symbol table (with an unresolved
    interface) when one does not already exist. Also test that the
    Call node ast property is set to reference the original fparser2
    call node.

    '''
    reader = FortranStringReader(" call kernel()")
    ast = Fortran2003.Call_Stmt(reader)
    fake_parent = Schedule()
    processor = Fparser2Reader()
    processor.process_nodes(fake_parent, [ast])

    call_node = fake_parent.children[0]
    assert isinstance(call_node, Call)
    assert not call_node.children

    routine_symbol = call_node.routine
    assert isinstance(routine_symbol, RoutineSymbol)
    assert isinstance(routine_symbol.interface, UnresolvedInterface)
    assert routine_symbol.name == "kernel"
    assert routine_symbol in call_node.scope.symbol_table.symbols
    assert isinstance(routine_symbol.datatype, NoType)

    assert (str(call_node)) == "Call[name='kernel']"

    assert call_node.ast == ast


def test_call_declared_routine(f2008_parser):
    '''Test that fparser2reader transforms a Fortran subroutine call into
     the equivalent PSyIR Call node when the call name has already
     been declared. The example includes the call twice as the first
     time the symbol needs to be specialised to a RoutineSymbol and
     the second time it should already be a RoutineSymbol.

    '''
    test_code = (
        "subroutine test()\n"
        "use my_mod, only : kernel\n"
        "  call kernel()\n"
        "  call kernel()\n"
        "end subroutine")
    reader = FortranStringReader(test_code)
    ptree = f2008_parser(reader)
    processor = Fparser2Reader()
    sched = processor.generate_schedule("test", ptree)
    for call_node in [sched.children[0], sched.children[1]]:
        routine_symbol = call_node.routine
        assert isinstance(routine_symbol, RoutineSymbol)
        assert isinstance(routine_symbol.interface, ImportInterface)
        assert routine_symbol.name == "kernel"
        assert routine_symbol in call_node.scope.symbol_table.symbols
        assert isinstance(routine_symbol.datatype, NoType)


def test_call_incorrect_type(f2008_parser):
    '''Test that fparser2reader raises the expected exception if the name
    of the call is already declared as an incompatible symbol
    type. Note, fparser2 should really pick this up but currently its
    consistency checks are limited.

    '''
    test_code = (
        "subroutine test()\n"
        "real :: kernel\n"
        "  call kernel()\n"
        "end subroutine")
    reader = FortranStringReader(test_code)
    ptree = f2008_parser(reader)
    processor = Fparser2Reader()
    with pytest.raises(GenerationError) as info:
        _ = processor.generate_schedule("test", ptree)
    assert ("Expecting the symbol 'kernel', to be of type 'Symbol' or "
            "'RoutineSymbol', but found 'DataSymbol'." in str(info.value))


@pytest.mark.parametrize("args,arg_names", [
    ("1.0, a, (a+b)*2.0", [None, None, None]),
    ("1.0, arg2=a, arg3=(a+b)*2.0", [None, "arg2", "arg3"])])
def test_call_args(f2008_parser, args, arg_names):
    '''Test that fparser2reader transforms a Fortran subroutine call with
    arguments into the equivalent PSyIR Call node. Test with and
    without named arguments.

    '''
    test_code = (
        f"subroutine test()\n"
        f"use my_mod, only : kernel\n"
        f"real :: a,b\n"
        f"  call kernel({args})\n"
        f"end subroutine")
    reader = FortranStringReader(test_code)
    ptree = f2008_parser(reader)
    processor = Fparser2Reader()
    sched = processor.generate_schedule("test", ptree)

    call_node = sched.children[0]
    assert isinstance(call_node, Call)
    assert len(call_node._argument_names) == len(call_node.children)
    for idx, child in enumerate(call_node.children):
        assert call_node._argument_names[idx] == (id(child), arg_names[idx])
    assert call_node.argument_names == arg_names
    assert len(call_node.children) == 3
    assert isinstance(call_node.children[0], Literal)
    assert call_node.children[0].value == "1.0"
    assert isinstance(call_node.children[1], Reference)
    assert call_node.children[1].name == "a"
    assert isinstance(call_node.children[2], BinaryOperation)

    routine_symbol = call_node.routine
    assert isinstance(routine_symbol, RoutineSymbol)
    assert call_node.routine.name == "kernel"
    assert isinstance(routine_symbol.interface, ImportInterface)
    assert routine_symbol is call_node.scope.symbol_table.lookup("kernel")

    assert (str(call_node)) == "Call[name='kernel']"


@pytest.mark.usefixtures("f2008_parser")
def test_labelled_call():
    '''Test that fparser2reader transforms a labelled Fortran subroutine call
    into a CodeBlock.

    '''
    reader = FortranStringReader("99 call kernel()")
    ast = Fortran2003.Call_Stmt(reader)
    fake_parent = Schedule()
    processor = Fparser2Reader()
    processor.process_nodes(fake_parent, [ast])
    assert isinstance(fake_parent[0], CodeBlock)


def test_call_codeblock_args(fortran_reader):
    ''' Test that we get one CodeBlock for each (unrecognised) argument rather
    than a single CodeBlock containing all of them. '''
    test_code = (
        "subroutine test()\n"
        "  use my_mod, only : kernel\n"
        "  real :: a, b\n"
        "  call kernel(a, 'not'//'nice', 'at'//'all', b)\n"
        "end subroutine")
    psyir = fortran_reader.psyir_from_source(test_code)
    call_node = psyir.walk(Call)[0]
    assert isinstance(call_node, Call)
    assert len(call_node.children) == 4
    assert isinstance(call_node.children[0], Reference)
    assert call_node.children[0].name == "a"
    assert isinstance(call_node.children[1], CodeBlock)
    assert isinstance(call_node.children[2], CodeBlock)
    assert isinstance(call_node.children[3], Reference)
    assert call_node.children[3].name == "b"
