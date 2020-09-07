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
from psyclone.psyGen import KernelSchedule
from psyclone.psyir.symbols import (SymbolError, DeferredType, StructureType,
                                    TypeSymbol, DataSymbol, ScalarType)
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


@pytest.mark.usefixtures("f2008_parser")
def test_missing_derived_type():
    ''' Check that the fronted raises an error if it encounters a variable
    of a derived type that cannot be resolved. '''
    fake_parent = KernelSchedule("dummy_schedule")
    symtab = fake_parent.symbol_table
    processor = Fparser2Reader()
    reader = FortranStringReader("type(my_type) :: var")
    fparser2spec = Specification_Part(reader)
    # This should raise an error because there's no Container from which
    # the definition of 'my_type' can be brought into scope.
    with pytest.raises(SymbolError) as err:
        processor.process_declarations(fake_parent, fparser2spec.content, [])
    assert "No Symbol found for name 'my_type'" in str(err.value)


@pytest.mark.xfail(reason="Needs support for RoutineSymbols added in #872")
def test_name_clash_derived_type(f2008_parser):
    ''' Check that the fronted raises an error if it encounters a reference
    to a derived type that clashes with another symbol. '''
    fake_parent = KernelSchedule("dummy_schedule")
    symtab = fake_parent.symbol_table
    processor = Fparser2Reader()
    reader = FortranStringReader("module my_mod\n"
                                 "  type :: my_type\n"
                                 "    integer :: flag\n"
                                 "  end type my_type\n"
                                 "contains\n"
                                 "  subroutine my_type()\n"
                                 "    integer :: flag2\n"
                                 "  end subroutine my_type\n"
                                 "end module my_mod\n")
    fparser2spec = f2008_parser(reader)
    # This should raise an error because there's no Container from which
    # the definition of 'my_type' can be brought into scope.
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
                                 "  type(grid_type) :: grid\n"
                                 "end type my_type\n".format(use_stmt))
    fparser2spec = Specification_Part(reader)
    processor.process_declarations(fake_parent, fparser2spec.content, [])
    sym = symtab.lookup("my_type")
    assert isinstance(sym, TypeSymbol)
    assert isinstance(sym.datatype, StructureType)
    flag_sym = sym.datatype.symbol_table.lookup("flag")
    assert isinstance(flag_sym, DataSymbol)
    assert isinstance(flag_sym.datatype, ScalarType)
    grid_sym = sym.datatype.symbol_table.lookup("grid")
    assert isinstance(grid_sym, DataSymbol)
    assert isinstance(grid_sym.datatype, DeferredType)
