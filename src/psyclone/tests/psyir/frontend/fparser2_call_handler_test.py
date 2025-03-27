# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2021-2025, Science and Technology Facilities Council.
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


'''Performs py.test tests on the support for calls within the
    _call_handler method in the fparser2 PSyIR front-end

'''

import pytest

from fparser.common.readfortran import FortranStringReader
from fparser.two import Fortran2003

from psyclone.errors import GenerationError
from psyclone.psyir.frontend.fparser2 import Fparser2Reader
from psyclone.psyir.nodes import (
    CodeBlock, Schedule, Call, Reference, StructureReference,
    ArrayOfStructuresReference, Routine)
from psyclone.psyir.symbols import (
    RoutineSymbol, UnresolvedInterface, ImportInterface, NoType)


@pytest.mark.usefixtures("f2008_parser")
def test_call_noargs():
    '''Test that fparser2reader _call_handler method transforms a Fortran
    subroutine call with no arguments into the equivalent PSyIR Call
    node. Also test that a new RoutineSymbol is added to the symbol
    table (with an unresolved interface) when one does not already
    exist. Also test that the Call node ast property is set to
    reference the original fparser2 call node, which indicates that
    the _process_args method is called.

    '''
    reader = FortranStringReader(" call kernel()")
    ast = Fortran2003.Call_Stmt(reader)
    fake_parent = Routine.create('dummy')
    processor = Fparser2Reader()
    processor.process_nodes(fake_parent, [ast])

    call_node = fake_parent.children[0]
    assert isinstance(call_node, Call)
    assert not call_node.arguments

    routine_ref = call_node.routine
    assert isinstance(routine_ref, Reference)
    routine_symbol = routine_ref.symbol
    assert isinstance(routine_symbol, RoutineSymbol)
    assert isinstance(routine_symbol.interface, UnresolvedInterface)
    assert routine_symbol.name == "kernel"
    assert routine_symbol in call_node.scope.symbol_table.symbols
    assert isinstance(routine_symbol.datatype, NoType)

    assert (str(call_node)) == "Call[name='kernel']"

    assert call_node.ast == ast


def test_call_declared_routine(f2008_parser):
    '''Test that fparser2reader _call_handler method transforms a Fortran
     subroutine call into the equivalent PSyIR Call node when the call
     name has already been declared. The example includes the call
     twice as the first time the symbol needs to be specialised to a
     RoutineSymbol and the second time it should already be a
     RoutineSymbol.

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
    psyir = processor.generate_psyir(ptree)

    for call_node in psyir.walk(Call):
        routine_symbol = call_node.routine.symbol
        assert isinstance(routine_symbol, RoutineSymbol)
        assert isinstance(routine_symbol.interface, ImportInterface)
        assert routine_symbol.name == "kernel"
        assert routine_symbol in call_node.scope.symbol_table.symbols
        assert isinstance(routine_symbol.datatype, NoType)
        assert (str(call_node)) == "Call[name='kernel']"


def test_call_type_bound_expression(f2008_parser):
    '''Test that fparser2reader _call_handler method transforms a Fortran
     subroutine call into the equivalent PSyIR Call node when the call
     is a complex type-bound expression.
    '''
    test_code = (
        "subroutine test()\n"
        "  use my_mod\n"
        "  ! Simple type-bound call with 1 argument\n"
        "  call struct%method(3)\n"
        "  ! Type-bound call with a middle array member and a named argument\n"
        "  call struct%field(3 + var)%method(arg_name=4)\n"
        "  ! Array Type-bound call and no arguments\n"
        "  call struct(3)%method()\n"
        "  ! Array Type-bound call with a middle field and no arguments\n"
        "  call struct(i)%field%method()\n"
        "end subroutine")
    reader = FortranStringReader(test_code)
    ptree = f2008_parser(reader)
    processor = Fparser2Reader()
    psyir = processor.generate_psyir(ptree)

    calls = psyir.walk(Call)
    assert len(calls) == 4

    # Check that all children are fully captured at their expected position
    assert isinstance(calls[0].children[0], StructureReference)
    assert calls[0].routine.debug_string() == "struct%method"
    assert calls[0].arguments[0].debug_string() == "3"
    assert calls[0].argument_names[0] is None

    assert isinstance(calls[1].children[0], StructureReference)
    assert calls[1].routine.debug_string() == "struct%field(3 + var)%method"
    assert calls[1].arguments[0].debug_string() == "4"
    assert calls[1].argument_names[0] == "arg_name"

    assert isinstance(calls[2].children[0], ArrayOfStructuresReference)
    assert calls[2].routine.debug_string() == "struct(3)%method"
    assert len(calls[2].arguments) == 0
    assert len(calls[2].argument_names) == 0

    assert isinstance(calls[3].children[0], ArrayOfStructuresReference)
    assert calls[3].routine.debug_string() == "struct(i)%field%method"
    assert len(calls[3].arguments) == 0
    assert len(calls[3].argument_names) == 0


def test_call_incorrect_type(f2008_parser):
    '''Test that fparser2reader _call_handler method raises the expected
    exception if the name of the call is already declared as an
    incompatible symbol type. Note, fparser2 should really pick this
    up but currently its consistency checks are limited.

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
        _ = processor.generate_psyir(ptree)
    assert ("Expecting the symbol 'kernel', to be of type 'Symbol' or "
            "'RoutineSymbol', but found 'DataSymbol'." in str(info.value))


@pytest.mark.usefixtures("f2008_parser")
def test_labelled_call():
    '''Test that fparser2reader method transforms a labelled Fortran
    subroutine call into a CodeBlock. This is not done by
    _call_handler but is related to calls so is added in this file.

    '''
    reader = FortranStringReader("99 call kernel()")
    ast = Fortran2003.Call_Stmt(reader)
    fake_parent = Schedule()
    processor = Fparser2Reader()
    processor.process_nodes(fake_parent, [ast])
    assert isinstance(fake_parent[0], CodeBlock)


def test_unresolved_shadowed_routine(fortran_reader):
    '''
    For the situation where we can't be certain of the source of the target
    of a call, check that an unresolved RoutineSymbol is created.
    '''
    code = '''\
    module this_mod
      implicit none
    contains
      subroutine do_it()
        use my_mod
        integer :: old_my_sub
        real, dimension(10) :: a
        ! This call could be to the routine defined in the same module *or*
        ! to one of the same name brought in by the wildcard import from
        ! `my_mod`
        call my_sub(a)
      end subroutine do_it
      subroutine my_sub()
        real, dimension(10) :: b
        b = 1.0
      end subroutine my_sub
    end module this_mod
    '''
    psyir = fortran_reader.psyir_from_source(code)
    do_it = psyir.children[0].find_routine_psyir("do_it")
    calls = psyir.walk(Call)
    assert calls[0].routine.symbol.is_unresolved
    assert isinstance(calls[0].routine.symbol, RoutineSymbol)
    # Symbol should be in the table of the Routine containing the call.
    assert calls[0].routine.symbol.find_symbol_table(calls[0]).node is do_it
