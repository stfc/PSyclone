# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2020-2024, Science and Technology Facilities Council.
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
# Authors: A. R. Porter, N. Nobre and S. Siso, STFC Daresbury Lab
# Modified: R. W. Ford, STFC Daresbury Lab
# -----------------------------------------------------------------------------

''' Performs py.test tests on the fparser2 PSyIR front-end support for
    derived types. '''

import pytest

from fparser.two import Fortran2003
from fparser.two.utils import walk
from fparser.common.readfortran import FortranStringReader

from psyclone.errors import InternalError
from psyclone.psyir.nodes import (
    KernelSchedule, CodeBlock, Assignment, ArrayOfStructuresReference,
    StructureReference, Member, StructureMember, ArrayOfStructuresMember,
    ArrayMember, Literal, Reference, Range, IntrinsicCall)
from psyclone.psyir.symbols import (
    SymbolError, UnresolvedType, StructureType, DataTypeSymbol, ScalarType,
    RoutineSymbol, Symbol, ArrayType, UnsupportedFortranType, DataSymbol,
    INTEGER_TYPE, ContainerSymbol, ImportInterface)
from psyclone.psyir.frontend.fparser2 import (
    Fparser2Reader, _create_struct_reference)


def test_create_struct_reference():
    ''' Tests for the _create_struct_reference() utility. '''
    one = Literal("1", INTEGER_TYPE)
    with pytest.raises(InternalError) as err:
        _create_struct_reference(None, StructureReference, Symbol("fake"),
                                 ["hello", 1], [])
    assert ("List of members must contain only strings or tuples but found "
            "entry of type 'int'" in str(err.value))
    with pytest.raises(NotImplementedError) as err:
        _create_struct_reference(None, StructureType, Symbol("fake"),
                                 ["hello"], [])
    assert "Cannot create structure reference for type '" in str(err.value)
    with pytest.raises(InternalError) as err:
        _create_struct_reference(None, StructureReference,
                                 DataSymbol("fake", UnresolvedType()),
                                 ["hello"], [one.copy()])
    assert ("Creating a StructureReference but array indices have been "
            "supplied" in str(err.value))
    with pytest.raises(InternalError) as err:
        _create_struct_reference(None, ArrayOfStructuresReference,
                                 DataSymbol("fake", UnresolvedType()),
                                 ["hello"], [])
    assert ("Cannot create an ArrayOfStructuresReference without one or more "
            "index expressions" in str(err.value))
    ref = _create_struct_reference(None, StructureReference,
                                   DataSymbol("fake", UnresolvedType()),
                                   ["hello"], [])
    assert isinstance(ref, StructureReference)
    assert isinstance(ref.member, Member)
    assert ref.member.name == "hello"
    # Check that we can create an ArrayOfStructuresReference and that any
    # PSyIR nodes are copied.
    idx_var = one.copy()
    idx_var2 = one.copy()
    aref = _create_struct_reference(None, ArrayOfStructuresReference,
                                    DataSymbol("fake", UnresolvedType()),
                                    ["a", ("b", [idx_var2])], [idx_var])
    assert isinstance(aref, ArrayOfStructuresReference)
    assert isinstance(aref.member, StructureMember)
    assert aref.member.name == "a"
    assert aref.member.member.name == "b"
    assert len(aref.member.member.indices) == 1
    assert aref.member.member.indices[0] is not idx_var2
    assert len(aref.indices) == 1
    assert aref.indices[0] is not idx_var


@pytest.mark.usefixtures("f2008_parser")
@pytest.mark.parametrize("type_name", ["my_type", "MY_TYPE", "mY_type"])
def test_deferred_derived_type(type_name):
    ''' Check that we get a symbol with a type given by a DataTypeSymbol (which
    itself is of UnresolvedType) for a declaration using an unresolved derived
    type. '''
    fake_parent = KernelSchedule("dummy_schedule")
    symtab = fake_parent.symbol_table
    processor = Fparser2Reader()
    reader = FortranStringReader(f"use my_mod\n"
                                 f"type({type_name}) :: var")
    fparser2spec = Fortran2003.Specification_Part(reader)
    processor.process_declarations(fake_parent, fparser2spec.content, [])
    vsym = symtab.lookup("var")
    assert isinstance(vsym.datatype, DataTypeSymbol)
    assert isinstance(vsym.datatype.datatype, UnresolvedType)
    tsym = symtab.lookup("my_type")
    assert isinstance(tsym, DataTypeSymbol)


@pytest.mark.usefixtures("f2008_parser")
def test_missing_derived_type():
    '''Check that the fronted does not raise an error if it encounters a
    variable of a derived type that cannot be resolved. This is the
    back-end's responsibility.

    '''
    fake_parent = KernelSchedule("dummy_schedule")
    processor = Fparser2Reader()
    reader = FortranStringReader("type(my_type) :: var")
    fparser2spec = Fortran2003.Specification_Part(reader)
    processor.process_declarations(fake_parent, fparser2spec.content, [])


@pytest.mark.parametrize("type_name", ["my_type", "MY_TYPE", "mY_type"])
def test_name_clash_derived_type(f2008_parser, type_name):
    ''' Check that the frontend raises an error if it encounters a reference
    to a derived type that clashes with another symbol. '''
    fake_parent = KernelSchedule("dummy_schedule")
    symtab = fake_parent.symbol_table
    # Add a RoutineSymbol to the symbol table that clashes with the name
    # of the derived type.
    symtab.add(RoutineSymbol("my_type"))
    processor = Fparser2Reader()
    reader = FortranStringReader(f"subroutine my_sub()\n"
                                 f"  type({type_name}) :: some_var\n"
                                 f"end subroutine my_sub\n")
    fparser2spec = f2008_parser(reader)
    spec_part = walk(fparser2spec, Fortran2003.Specification_Part)[0]
    # This should raise an error because the Container symbol table should
    # already contain a RoutineSymbol named 'my_type'
    with pytest.raises(SymbolError) as err:
        processor.process_declarations(fake_parent, spec_part.children, [])
    assert (f"Search for a DataTypeSymbol named '{type_name}' (required by "
            f"specification 'TYPE({type_name})') found a 'RoutineSymbol' "
            f"instead" in str(err.value))


def test_name_clash_derived_type_def(f2008_parser):
    ''' Check that the frontend raises an error if it encounters a definition
    of a derived type with a name that clashes with another symbol. '''
    fake_parent = KernelSchedule("dummy_schedule")
    symtab = fake_parent.symbol_table
    # Add a RoutineSymbol to the symbol table that will clash with the name
    # of the derived type.
    symtab.add(RoutineSymbol("my_type"))
    # Add a DataTypeSymbol to the symbol table. Make it appear that we've
    # already seen a definition of this symbol by making it of
    # UnsupportedFortranType.
    symtab.new_symbol("my_type2", symbol_type=DataTypeSymbol,
                      datatype=UnsupportedFortranType("huh"))
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
            " 'RoutineSymbol' when it should be a 'DataTypeSymbol' (for the "
            "derived-type definition 'TYPE :: my_type\n  INTEGER :: flag\n"
            "END TYPE my_type')" in str(err.value))
    # Repeat but with a derived-type name that will clash with our existing
    # DataTypeSymbol
    fparser2spec = f2008_parser(
        FortranStringReader("subroutine my_sub2()\n"
                            "  type :: my_type2\n"
                            "    integer :: flag\n"
                            "  end type my_type2\n"
                            "end subroutine my_sub2\n"))
    with pytest.raises(SymbolError) as err:
        processor.process_declarations(fake_parent, fparser2spec.content, [])
    assert ("Error processing definition of derived type 'my_type2'. The "
            "symbol table already contains a DataTypeSymbol with this name but"
            " it is of type 'UnsupportedFortranType' when it should be of "
            "'UnresolvedType'" in str(err.value))


def test_existing_symbol_derived_type_def(f2008_parser):
    ''' Check that a new DataTypeSymbol is created in place of an existing
    Symbol if it is used in a type declaration. '''
    fake_parent = KernelSchedule("dummy_schedule")
    symtab = fake_parent.symbol_table
    csym = symtab.new_symbol("some_mod", symbol_type=ContainerSymbol)
    # Add a generic Symbol to the symbol table for what will be the type
    # definition.
    symtab.add(Symbol("my_type", visibility=Symbol.Visibility.PRIVATE,
                      interface=ImportInterface(csym)))

    processor = Fparser2Reader()
    fparser2spec = f2008_parser(
        FortranStringReader("subroutine my_sub()\n"
                            "  type(my_type) :: var\n"
                            "end subroutine my_sub\n"))
    type_specs = walk(fparser2spec, types=Fortran2003.Declaration_Type_Spec)
    typ, prec = processor._process_type_spec(fake_parent, type_specs[0])
    assert prec is None
    assert isinstance(typ, DataTypeSymbol)
    # Check that the visibility has been preserved from the original Symbol.
    assert typ.visibility == Symbol.Visibility.PRIVATE
    assert typ.name == "my_type"
    assert isinstance(typ.interface, ImportInterface)


@pytest.mark.usefixtures("f2008_parser")
@pytest.mark.parametrize("use_stmt", ["use grid_mod, only: grid_type",
                                      "use grid_mod, only: GRID_TYPE",
                                      "use grid_mod"])
@pytest.mark.parametrize("type_name", ["GRID_TYPE", "grid_type"])
def test_parse_derived_type(use_stmt, type_name):
    ''' Check that the fronted correctly creates a DataTypeSymbol of type
    StructureType from the declaration of a derived type. '''
    fake_parent = KernelSchedule("dummy_schedule")
    symtab = fake_parent.symbol_table
    processor = Fparser2Reader()
    reader = FortranStringReader(f"{use_stmt}\n"
                                 f"type :: my_type\n"
                                 f"  integer :: flag\n"
                                 f"  type({type_name}), private :: grid\n"
                                 f"  real, dimension(3) :: posn\n"
                                 f"end type my_type\n"
                                 f"type(my_type) :: var\n")
    fparser2spec = Fortran2003.Specification_Part(reader)
    processor.process_declarations(fake_parent, fparser2spec.content, [])
    sym = symtab.lookup("my_type")
    assert isinstance(sym, DataTypeSymbol)
    assert isinstance(sym.datatype, StructureType)
    flag = sym.datatype.lookup("flag")
    assert isinstance(flag.datatype, ScalarType)
    assert flag.visibility == Symbol.Visibility.PUBLIC
    grid = sym.datatype.lookup("grid")
    assert isinstance(grid.datatype, DataTypeSymbol)
    assert isinstance(grid.datatype.datatype, UnresolvedType)
    assert grid.visibility == Symbol.Visibility.PRIVATE
    posn = sym.datatype.lookup("posn")
    assert isinstance(posn.datatype, ArrayType)
    var = symtab.lookup("var")
    assert var.datatype is sym


@pytest.mark.usefixtures("f2008_parser")
def test_derived_type_contains():
    ''' Check that we get a DataTypeSymbol of UnsupportedFortranType if a
    derived-type definition has a CONTAINS section. '''
    fake_parent = KernelSchedule("dummy_schedule")
    symtab = fake_parent.symbol_table
    processor = Fparser2Reader()
    reader = FortranStringReader("type my_type\n"
                                 "  integer :: flag\n"
                                 "  real, dimension(3) :: posn\n"
                                 "contains\n"
                                 "  procedure :: init => obesdv_setup\n"
                                 "end type my_type\n"
                                 "type(my_type) :: var\n")
    fparser2spec = Fortran2003.Specification_Part(reader)
    processor.process_declarations(fake_parent, fparser2spec.content, [])
    sym = symtab.lookup("my_type")
    # It should still be a DataTypeSymbol but its type is unknown.
    assert isinstance(sym, DataTypeSymbol)
    assert isinstance(sym.datatype, UnsupportedFortranType)
    assert sym.datatype.declaration == '''\
TYPE :: my_type
  INTEGER :: flag
  REAL, DIMENSION(3) :: posn
  CONTAINS
  PROCEDURE :: init => obesdv_setup
END TYPE my_type'''


@pytest.mark.usefixtures("f2008_parser")
@pytest.mark.parametrize("type_name", ["my_type", "MY_TYPE", "mY_type"])
def test_derived_type_self_ref(type_name):
    ''' Test that we can parse a derived type that contains a pointer
    reference of that same type. The 'pointer' attribute is not supported
    so we get a DataTypeSymbol of UnsupportedFortranType. '''
    fake_parent = KernelSchedule("dummy_schedule")
    symtab = fake_parent.symbol_table
    processor = Fparser2Reader()
    reader = FortranStringReader(f"type :: my_type\n"
                                 f"  type({type_name}), "
                                 f"pointer :: next => null()\n"
                                 f"  integer :: flag\n"
                                 f"end type my_type\n"
                                 f"type({type_name}) :: var\n")
    fparser2spec = Fortran2003.Specification_Part(reader)
    processor.process_declarations(fake_parent, fparser2spec.content, [])
    sym = symtab.lookup("my_type")
    assert isinstance(sym, DataTypeSymbol)
    assert isinstance(sym.datatype, UnsupportedFortranType)
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
    assert isinstance(sym, DataTypeSymbol)
    flag = sym.datatype.lookup("flag")
    assert flag.visibility == Symbol.Visibility.PRIVATE
    scale = sym.datatype.lookup("scale")
    assert scale.visibility == Symbol.Visibility.PUBLIC


def test_derived_type_ref(f2008_parser, fortran_writer):
    ''' Check that the frontend handles references to a member of
    a derived type. '''
    processor = Fparser2Reader()
    reader = FortranStringReader(
        "subroutine my_sub()\n"
        "  use some_mod, only: my_type\n"
        "  type(my_type) :: var, vars(3)\n"
        "  var%flag = 0\n"
        "  var%region%start = 1\n"
        "  var%region%subgrid(3)%stop = 1\n"
        "  var%region%subgrid(3)%data(:) = 1.0\n"
        "  var%region%subgrid(3)%data(var%start:var%stop) = 1.0\n"
        "  vars(1)%region%subgrid(3)%data(:) = 1.0\n"
        "  vars(1)%region%subgrid(:)%data(1) = 1.0\n"
        "  vars(:)%region%subgrid(3)%xstop = 1.0\n"
        "end subroutine my_sub\n")
    fparser2spec = f2008_parser(reader)
    sched = processor.generate_psyir(fparser2spec)
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
    lbound = amem.member.indices[0].children[0]
    assert isinstance(lbound, IntrinsicCall)
    assert lbound.intrinsic == IntrinsicCall.Intrinsic.LBOUND
    assert lbound.children[0].symbol.name == "var"
    assert isinstance(lbound.children[0], StructureReference)
    # The argument to the LBOUND intrinsic must ultimately resolve down
    # to a Member access, not an ArrayMember access.
    assert isinstance(lbound.children[0].member.member.member, Member)
    assert not isinstance(lbound.children[0].member.member.member, ArrayMember)
    ubound = amem.member.indices[0].children[1]
    assert isinstance(ubound, IntrinsicCall)
    assert ubound.intrinsic == IntrinsicCall.Intrinsic.UBOUND
    # The argument to the UBOUND intrinsic must ultimately resolve down
    # to a Member access, not an ArrayMember access.
    assert isinstance(ubound.children[0].member.member.member, Member)
    assert not isinstance(ubound.children[0].member.member.member, ArrayMember)
    gen = fortran_writer(amem)
    assert gen == "subgrid(3)%data(:)"
    # var%region%subgrid(3)%data(var%start:var%stop)
    assign = assignments[4]
    amem = assign.lhs.member.member.member
    assert isinstance(amem, ArrayMember)
    assert isinstance(amem.indices[0], Range)
    assert isinstance(amem.indices[0].children[0], StructureReference)
    assert isinstance(amem.indices[0].children[0].member, Member)
    assert amem.indices[0].children[0].member.name == "start"
    # vars(1)%region%subgrid(3)%data(:) = 1.0
    assign = assignments[5]
    amem = assign.lhs
    data_node = amem.member.member.member
    lbound = data_node.children[0].children[0]
    assert isinstance(lbound, IntrinsicCall)
    assert lbound.intrinsic == IntrinsicCall.Intrinsic.LBOUND
    # Argument to LBOUND must be a Member, not an ArrayMember
    assert isinstance(lbound.children[0].member.member.member, Member)
    assert not isinstance(lbound.children[0].member.member.member, ArrayMember)
    ubound = data_node.children[0].children[1]
    assert isinstance(ubound, IntrinsicCall)
    assert ubound.intrinsic == IntrinsicCall.Intrinsic.UBOUND
    # Argument to UBOUND must be a Member, not an ArrayMember
    assert isinstance(ubound.children[0].member.member.member, Member)
    assert not isinstance(ubound.children[0].member.member.member, ArrayMember)
    # vars(1)%region%subgrid(:)%data(1) = 1.0
    assign = assignments[6]
    amem = assign.lhs
    assert isinstance(amem.member.member.children[1], Range)
    lbound = amem.member.member.children[1].children[0]
    assert isinstance(lbound, IntrinsicCall)
    assert lbound.intrinsic == IntrinsicCall.Intrinsic.LBOUND
    assert lbound.children[0].member.member.name == "subgrid"
    assert isinstance(lbound.children[0].member.member, Member)
    assert not isinstance(lbound.children[0].member.member, ArrayMember)
    assert amem.member.member.member.name == "data"
    assert isinstance(amem.member.member.member, ArrayMember)
    # vars(:)%region%subgrid(3)%xstop
    assign = assignments[7]
    amem = assign.lhs
    lbound = amem.children[1].children[0]
    assert isinstance(lbound, IntrinsicCall)
    assert lbound.intrinsic == IntrinsicCall.Intrinsic.LBOUND
    assert isinstance(lbound.children[0], Reference)
    assert lbound.children[0].symbol.name == "vars"


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
    sched = processor.generate_psyir(fparser2spec)
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
    sched = processor.generate_psyir(fparser2spec)
    cblocks = sched.walk(CodeBlock)
    assert len(cblocks) == 1
    assert isinstance(cblocks[0].parent, Assignment)
    # Repeat but this time break the Data_Ref by modifying its second child.
    reader = FortranStringReader(code)
    fparser2spec = f2008_parser(reader)
    dref = Fortran2003.walk(fparser2spec, Fortran2003.Data_Ref)[0]
    dref.items = (dref.items[0], "hello")
    sched = processor.generate_psyir(fparser2spec)
    cblocks = sched.walk(CodeBlock)
    assert len(cblocks) == 1
    assert isinstance(cblocks[0].parent, Assignment)
