# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2020, Science and Technology Facilities Council.
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
from psyclone.psyir.nodes import KernelSchedule
from psyclone.psyir.symbols import SymbolError, DeferredType, StructureType, \
    TypeSymbol, ScalarType, RoutineSymbol, Symbol, ArrayType
from psyclone.psyir.frontend.fparser2 import Fparser2Reader
from fparser.two.Fortran2003 import Specification_Part
from fparser.common.readfortran import FortranStringReader


@pytest.mark.usefixtures("f2008_parser")
def test_deferred_derived_type():
    ''' Check that we get a symbol with DeferredType for a declaration using
    an unresolved derived type. '''
    fake_parent = KernelSchedule("dummy_schedule")
    symtab = fake_parent.symbol_table
    processor = Fparser2Reader()
    reader = FortranStringReader("use my_mod\n"
                                 "type(my_type) :: var")
    fparser2spec = Specification_Part(reader)
    processor.process_declarations(fake_parent, fparser2spec.content, [])
    vsym = symtab.lookup("var")
    assert isinstance(vsym.datatype, DeferredType)
    tsym = symtab.lookup("my_type")
    assert isinstance(tsym, TypeSymbol)


@pytest.mark.usefixtures("f2008_parser")
def test_missing_derived_type():
    ''' Check that the fronted raises an error if it encounters a variable
    of a derived type that cannot be resolved. '''
    fake_parent = KernelSchedule("dummy_schedule")
    processor = Fparser2Reader()
    reader = FortranStringReader("type(my_type) :: var")
    fparser2spec = Specification_Part(reader)
    # This should raise an error because there's no Container from which
    # the definition of 'my_type' can be brought into scope.
    with pytest.raises(SymbolError) as err:
        processor.process_declarations(fake_parent, fparser2spec.content, [])
    assert "No Symbol found for name 'my_type'" in str(err.value)


def test_name_clash_derived_type(f2008_parser):
    ''' Check that the fronted raises an error if it encounters a reference
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
    assert ("Search for a TypeSymbol named 'my_type' found a RoutineSymbol "
            "instead" in str(err.value))


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
                                 "end type my_type\n".format(use_stmt))
    fparser2spec = Specification_Part(reader)
    processor.process_declarations(fake_parent, fparser2spec.content, [])
    sym = symtab.lookup("my_type")
    assert isinstance(sym, TypeSymbol)
    assert isinstance(sym.datatype, StructureType)
    flag = sym.datatype.lookup("flag")
    assert isinstance(flag.datatype, ScalarType)
    assert flag.visibility == Symbol.Visibility.PUBLIC
    grid = sym.datatype.lookup("grid")
    assert isinstance(grid.datatype, DeferredType)
    assert grid.visibility == Symbol.Visibility.PRIVATE
    posn = sym.datatype.lookup("posn")
    assert isinstance(posn.datatype, ArrayType)


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
    fparser2spec = Specification_Part(reader)
    processor.process_declarations(fake_parent, fparser2spec.content, [])
    sym = symtab.lookup("my_type")
    assert isinstance(sym, TypeSymbol)
    flag = sym.datatype.lookup("flag")
    assert flag.visibility == Symbol.Visibility.PRIVATE
    scale = sym.datatype.lookup("scale")
    assert scale.visibility == Symbol.Visibility.PUBLIC
