# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2020-2021, Science and Technology Facilities Council.
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
# Author: A. R. Porter, STFC Daresbury Lab
# -----------------------------------------------------------------------------

''' Performs py.test tests on the fparser2 PSyIR front-end support for
    derived types. '''

from __future__ import absolute_import
import pytest
from psyclone.psyir.nodes import KernelSchedule, CodeBlock, Assignment, \
    ArrayOfStructuresReference, StructureReference, Member, StructureMember, \
    ArrayOfStructuresMember, ArrayMember, Literal, Reference, Range
from psyclone.psyir.symbols import SymbolError, DeferredType, StructureType, \
    TypeSymbol, ScalarType, RoutineSymbol, Symbol, ArrayType, \
    UnknownFortranType
from psyclone.psyir.frontend.fparser2 import Fparser2Reader
from fparser.two import Fortran2003
from fparser.common.readfortran import FortranStringReader


@pytest.mark.usefixtures("f2008_parser")
def test_deferred_derived_type():
    ''' Check that we get a symbol with a type given by a TypeSymbol (which
    itself is of DeferredType) for a declaration using an unresolved derived
    type. '''
    fake_parent = KernelSchedule("dummy_schedule")
    symtab = fake_parent.symbol_table
    processor = Fparser2Reader()
    reader = FortranStringReader("use my_mod\n"
                                 "type(my_type) :: var")
    fparser2spec = Fortran2003.Specification_Part(reader)
    processor.process_declarations(fake_parent, fparser2spec.content, [])
    vsym = symtab.lookup("var")
    assert isinstance(vsym.datatype, TypeSymbol)
    assert isinstance(vsym.datatype.datatype, DeferredType)
    tsym = symtab.lookup("my_type")
    assert isinstance(tsym, TypeSymbol)


@pytest.mark.usefixtures("f2008_parser")
def test_missing_derived_type():
    ''' Check that the fronted raises an error if it encounters a variable
    of a derived type that cannot be resolved. '''
    fake_parent = KernelSchedule("dummy_schedule")
    processor = Fparser2Reader()
    reader = FortranStringReader("type(my_type) :: var")
    fparser2spec = Fortran2003.Specification_Part(reader)
    # This should raise an error because there's no Container from which
    # the definition of 'my_type' can be brought into scope.
    with pytest.raises(SymbolError) as err:
        processor.process_declarations(fake_parent, fparser2spec.content, [])
    assert "No Symbol found for name 'my_type'" in str(err.value)


def test_name_clash_derived_type(f2008_parser):
    ''' Check that the frontend raises an error if it encounters a reference
    to a derived type that clashes with another symbol. '''
    fake_parent = KernelSchedule("dummy_schedule")
    symtab = fake_parent.symbol_table
    # Add a RoutineSymbol to the symbol table that clashes with the name
    # of the derived type.
    symtab.add(RoutineSymbol("my_type"))
    processor = Fparser2Reader()
    reader = FortranStringReader("subroutine my_sub()\n"
                                 "  type(my_type) :: some_var\n"
                                 "end subroutine my_sub\n")
    fparser2spec = f2008_parser(reader)
    # This should raise an error because the Container symbol table should
    # already contain a RoutineSymbol named 'my_type'
    with pytest.raises(SymbolError) as err:
        processor.process_declarations(fake_parent, fparser2spec.content, [])
    assert ("Search for a TypeSymbol named 'my_type' (required by declaration"
            " 'TYPE(my_type) :: some_var') found a 'RoutineSymbol' "
            "instead" in str(err.value))


def test_name_clash_derived_type_def(f2008_parser):
    ''' Check that the frontend raises an error if it encounters a definition
    of a derived type with a name that clashes with another symbol. '''
    fake_parent = KernelSchedule("dummy_schedule")
    symtab = fake_parent.symbol_table
    # Add a RoutineSymbol to the symbol table that will clash with the name
    # of the derived type.
    symtab.add(RoutineSymbol("my_type"))
    # Add a TypeSymbol to the symbol table. Make it appear that we've already
    # seen a definition of this symbol by making it of UnknownFortranType.
    symtab.new_symbol("my_type2", symbol_type=TypeSymbol,
                      datatype=UnknownFortranType("huh"))
    processor = Fparser2Reader()
    fparser2spec = f2008_parser(
        FortranStringReader("subroutine my_sub()\n"
                            "  type :: my_type\n"
                            "    integer :: flag\n"
                            "  end type my_type\n"
                            "end subroutine my_sub\n"))
    # This should raise an error because the Container symbol table will
    # already contain a RoutineSymbol named 'my_type'
    with pytest.raises(SymbolError) as err:
        processor.process_declarations(fake_parent, fparser2spec.content, [])
    assert ("Error processing definition of derived type 'my_type'. The "
            "symbol table already contains an entry with this name but it is a"
            " 'RoutineSymbol' when it should be a 'TypeSymbol' (for the "
            "derived-type definition 'TYPE :: my_type\n  INTEGER :: flag\n"
            "END TYPE my_type')" in str(err.value))
    # Repeat but with a derived-type name that will clash with our existing
    # TypeSymbol
    fparser2spec = f2008_parser(
        FortranStringReader("subroutine my_sub2()\n"
                            "  type :: my_type2\n"
                            "    integer :: flag\n"
                            "  end type my_type2\n"
                            "end subroutine my_sub2\n"))
    with pytest.raises(SymbolError) as err:
        processor.process_declarations(fake_parent, fparser2spec.content, [])
    assert ("Error processing definition of derived type 'my_type2'. The "
            "symbol table already contains a TypeSymbol with this name but it "
            "is of type 'UnknownFortranType' when it should be of "
            "'DeferredType'" in str(err.value))


@pytest.mark.usefixtures("f2008_parser")
@pytest.mark.parametrize("use_stmt", ["use grid_mod, only: grid_type",
                                      "use grid_mod"])
def test_parse_derived_type(use_stmt):
    ''' Check that the fronted correctly creates a TypeSymbol of type
    StructureType from the declaration of a derived type. '''
    fake_parent = KernelSchedule("dummy_schedule")
    symtab = fake_parent.symbol_table
    processor = Fparser2Reader()
    reader = FortranStringReader("{0}\n"
                                 "type :: my_type\n"
                                 "  integer :: flag\n"
                                 "  type(grid_type), private :: grid\n"
                                 "  real, dimension(3) :: posn\n"
                                 "end type my_type\n"
                                 "type(my_type) :: var\n".format(use_stmt))
    fparser2spec = Fortran2003.Specification_Part(reader)
    processor.process_declarations(fake_parent, fparser2spec.content, [])
    sym = symtab.lookup("my_type")
    assert isinstance(sym, TypeSymbol)
    assert isinstance(sym.datatype, StructureType)
    flag = sym.datatype.lookup("flag")
    assert isinstance(flag.datatype, ScalarType)
    assert flag.visibility == Symbol.Visibility.PUBLIC
    grid = sym.datatype.lookup("grid")
    assert isinstance(grid.datatype, TypeSymbol)
    assert isinstance(grid.datatype.datatype, DeferredType)
    assert grid.visibility == Symbol.Visibility.PRIVATE
    posn = sym.datatype.lookup("posn")
    assert isinstance(posn.datatype, ArrayType)
    var = symtab.lookup("var")
    assert var.datatype is sym


@pytest.mark.usefixtures("f2008_parser")
def test_derived_type_self_ref():
    ''' Test that we can parse a derived type that contains a pointer
    reference of that same type. The 'pointer' attribute is not supported
    so we get a TypeSymbol of UnknownFortranType. '''
    fake_parent = KernelSchedule("dummy_schedule")
    symtab = fake_parent.symbol_table
    processor = Fparser2Reader()
    reader = FortranStringReader("type :: my_type\n"
                                 "  type(my_type), pointer :: next => null()\n"
                                 "  integer :: flag\n"
                                 "end type my_type\n"
                                 "type(my_type) :: var\n")
    fparser2spec = Fortran2003.Specification_Part(reader)
    processor.process_declarations(fake_parent, fparser2spec.content, [])
    sym = symtab.lookup("my_type")
    assert isinstance(sym, TypeSymbol)
    assert isinstance(sym.datatype, UnknownFortranType)
    assert symtab.lookup("var").datatype is sym


@pytest.mark.usefixtures("f2008_parser")
def test_derived_type_accessibility():
    '''
    Check that accessibility statements/attributes within a derived type
    are handled correctly.
    '''
    fake_parent = KernelSchedule("dummy_schedule")
    symtab = fake_parent.symbol_table
    processor = Fparser2Reader()
    reader = FortranStringReader("type :: my_type\n"
                                 "  private\n"
                                 "  integer :: flag\n"
                                 "  real, public :: scale\n"
                                 "end type my_type\n")
    fparser2spec = Fortran2003.Specification_Part(reader)
    processor.process_declarations(fake_parent, fparser2spec.content, [])
    sym = symtab.lookup("my_type")
    assert isinstance(sym, TypeSymbol)
    flag = sym.datatype.lookup("flag")
    assert flag.visibility == Symbol.Visibility.PRIVATE
    scale = sym.datatype.lookup("scale")
    assert scale.visibility == Symbol.Visibility.PUBLIC


def test_derived_type_ref(f2008_parser):
    ''' Check that the frontend handles a references to a member of
    a derived type. '''
    processor = Fparser2Reader()
    reader = FortranStringReader(
        "subroutine my_sub()\n"
        "  use some_mod, only: my_type\n"
        "  type(my_type) :: var\n"
        "  var%flag = 0\n"
        "  var%region%start = 1\n"
        "  var%region%subgrid(3)%stop = 1\n"
        "  var%region%subgrid(3)%data(:) = 1.0\n"
        "  var%region%subgrid(3)%data(var%start:var%stop) = 1.0\n"
        "end subroutine my_sub\n")
    fparser2spec = f2008_parser(reader)
    sched = processor.generate_schedule("my_sub", fparser2spec)
    assert not sched.walk(CodeBlock)
    assignments = sched.walk(Assignment)
    # var%flag
    assign = assignments[0]
    assert isinstance(assign.lhs, StructureReference)
    assert isinstance(assign.lhs.children[0], Member)
    assert assign.lhs.children[0].name == "flag"
    # var%region%start
    assign = assignments[1]
    assert isinstance(assign.lhs, StructureReference)
    assert isinstance(assign.lhs.children[0], StructureMember)
    assert assign.lhs.children[0].name == "region"
    assert isinstance(assign.lhs.children[0].children[0], Member)
    assert assign.lhs.children[0].children[0].name == "start"
    # var%region%subgrid(3)%stop
    assign = assignments[2]
    amem = assign.lhs.children[0].children[0]
    assert isinstance(amem, ArrayOfStructuresMember)
    assert isinstance(amem.children[0], Member)
    assert isinstance(amem.children[1], Literal)
    assert amem.children[0].name == "stop"
    # var%region%subgrid(3)%data(:)
    assign = assignments[3]
    amem = assign.lhs.member.member
    assert isinstance(amem, ArrayOfStructuresMember)
    assert isinstance(amem.member, ArrayMember)
    assert isinstance(amem.member.indices[0], Range)
    assign = assignments[4]
    # var%region%subgrid(3)%data(var%start:var%stop)
    amem = assign.lhs.member.member.member
    assert isinstance(amem, ArrayMember)
    assert isinstance(amem.indices[0], Range)
    assert isinstance(amem.indices[0].children[0], StructureReference)
    assert isinstance(amem.indices[0].children[0].member, Member)
    assert amem.indices[0].children[0].member.name == "start"


def test_array_of_derived_type_ref(f2008_parser):
    ''' Test that the frontend handles a reference to a member of an element
    of an array of derived types. '''
    processor = Fparser2Reader()
    reader = FortranStringReader("subroutine my_sub()\n"
                                 "  use some_mod, only: my_type\n"
                                 "  type(my_type), dimension(3) :: var\n"
                                 "  integer :: idx = 2\n"
                                 "  integer :: start = 2, stop = 3\n"
                                 "  var(1)%flag = 0\n"
                                 "  var(idx)%flag = 0\n"
                                 "  var(start:stop)%flag = 1\n"
                                 "  var(1)%region%start = 1\n"
                                 "  var(1)%region%subgrid(3)%stop = 1\n"
                                 "  var(1)%region%subgrid(3)%data(:) = 1.0\n"
                                 "end subroutine my_sub\n")
    fparser2spec = f2008_parser(reader)
    sched = processor.generate_schedule("my_sub", fparser2spec)
    assert not sched.walk(CodeBlock)
    assignments = sched.walk(Assignment)
    # var(1)%flag
    assign = assignments[0]
    assert isinstance(assign.lhs, ArrayOfStructuresReference)
    assert isinstance(assign.lhs.children[0], Member)
    assert assign.lhs.children[0].name == "flag"
    assert isinstance(assign.lhs.children[1], Literal)
    # var(idx)%flag
    assign = assignments[1]
    assert isinstance(assign.lhs, ArrayOfStructuresReference)
    assert isinstance(assign.lhs.children[0], Member)
    assert isinstance(assign.lhs.children[1], Reference)
    assert assign.lhs.children[1].symbol.name == "idx"
    # var(start:stop)%flag
    assign = assignments[2]
    assert isinstance(assign.lhs, ArrayOfStructuresReference)
    assert isinstance(assign.lhs.member, Member)
    assert isinstance(assign.lhs.indices[0], Range)
    assert isinstance(assign.lhs.indices[0].children[0], Reference)
    assert assign.lhs.indices[0].children[0].symbol.name == "start"
    # var(1)%region%start
    assign = assignments[3]
    assert isinstance(assign.lhs, ArrayOfStructuresReference)
    assert isinstance(assign.lhs.member, StructureMember)
    assert isinstance(assign.lhs.indices[0], Literal)
    assert assign.lhs.member.name == "region"
    assert isinstance(assign.lhs.member.member, Member)
    # var(1)%region%subgrid(3)%stop
    assign = assignments[4]
    assert isinstance(assign.lhs, ArrayOfStructuresReference)
    assert isinstance(assign.lhs.member.member, ArrayOfStructuresMember)
    assert isinstance(assign.lhs.member.member.member, Member)
    assert isinstance(assign.lhs.member.member.indices[0], Literal)
    # var(1)%region%subgrid(3)%data(:)
    assign = assignments[5]
    assert isinstance(assign.lhs, ArrayOfStructuresReference)
    amem = assign.lhs.member.member
    assert isinstance(amem, ArrayOfStructuresMember)
    assert isinstance(amem.member, ArrayMember)
    assert isinstance(amem.indices[0], Literal)
    assert isinstance(amem.member.indices[0], Range)


def test_derived_type_codeblocks(f2008_parser):
    ''' Check that we create a CodeBlock if we encounter unsupported
    entries in a parse tree describing a derived-type access. We have
    to test with invalid content in two locations. '''
    code = ("subroutine my_sub()\n"
            "  use some_mod, only: my_type\n"
            "  type(my_type), dimension(3) :: var\n"
            "  var(1)%region%subgrid(3)%stop = 1\n"
            "end subroutine my_sub\n")
    processor = Fparser2Reader()
    # First create a valid parse tree.
    reader = FortranStringReader(code)
    fparser2spec = f2008_parser(reader)
    dref = Fortran2003.walk(fparser2spec, Fortran2003.Data_Ref)[0]
    # Now break the Data_Ref instance by modifying its first child. Requesting
    # a subset of items from a tuple appears to generate a new tuple so
    # explicitly create a list and then create a tuple from that.
    item_list = ["hello"] + list(dref.items[1:])
    dref.items = tuple(item_list)
    sched = processor.generate_schedule("my_sub", fparser2spec)
    cblocks = sched.walk(CodeBlock)
    assert len(cblocks) == 1
    assert isinstance(cblocks[0].parent, Assignment)
    # Repeat but this time break the Data_Ref by modifying its second child.
    reader = FortranStringReader(code)
    fparser2spec = f2008_parser(reader)
    dref = Fortran2003.walk(fparser2spec, Fortran2003.Data_Ref)[0]
    dref.items = (dref.items[0], "hello")
    sched = processor.generate_schedule("my_sub", fparser2spec)
    cblocks = sched.walk(CodeBlock)
    assert len(cblocks) == 1
    assert isinstance(cblocks[0].parent, Assignment)
