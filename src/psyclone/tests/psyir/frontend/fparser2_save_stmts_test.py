# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2023, Science and Technology Facilities Council.
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

''' Tests Fortran save statements in the fparser2 PSyIR front-end '''

import pytest

from psyclone.errors import GenerationError
from psyclone.psyir.nodes import Routine
from psyclone.psyir.symbols import (StaticInterface, DefaultModuleInterface,
                                    AutomaticInterface, UnknownFortranType)


def test_save_statement_module(fortran_reader):
    '''Tests that save statement and attribute are correctly captured when
    they appear in a module, irrespective of ordering.'''
    code = '''
      module my_mod
        save :: var3
        integer :: var1
        integer, save :: var2
        save :: var1
        integer :: var3, var4
      end module my_mod'''
    psyir = fortran_reader.psyir_from_source(code)
    symtab = psyir.children[0].symbol_table
    var1 = symtab.lookup("var1")
    assert var1.is_constant is False
    assert isinstance(var1.interface, StaticInterface)
    var2 = symtab.lookup("var2")
    assert isinstance(var2.interface, StaticInterface)
    var3 = symtab.lookup("var3")
    assert isinstance(var3.interface, StaticInterface)
    var4 = symtab.lookup("var4")
    assert isinstance(var4.interface, DefaultModuleInterface)


def test_default_save_module(fortran_reader):
    '''
    Test that all Symbols declared in a module are given a StaticInterface if
    there is a SAVE statement.
    '''
    code = '''
      module my_mod
        integer :: var1, var4
        integer :: var2
        save
        integer :: var3
      end module my_mod'''
    psyir = fortran_reader.psyir_from_source(code)
    symtab = psyir.children[0].symbol_table
    for var in ["var1", "var2", "var3", "var4"]:
        varsym = symtab.lookup(var)
        assert isinstance(varsym.interface, StaticInterface)


def test_save_invalid_fortran(fortran_reader):
    '''
    The Fortran standard (2008) says:

    C580: If a SAVE statement with an omitted saved entity list appears in a
    scoping unit, no other appearance of the SAVE attr-spec or SAVE statement
    is permitted in that scoping unit.

    This should be picked up by fparser2 but isn't so we catch it in PSyclone.
    '''
    code = '''
      module my_mod
        integer :: var2
        save
        integer, save :: var3
      end module my_mod'''
    with pytest.raises(GenerationError) as err:
        _ = fortran_reader.psyir_from_source(code)
    assert ("'INTEGER, SAVE :: var3'. Symbol 'sym_name' is the subject of a "
            "SAVE statement but also has a SAVE attribute on its declaration"
            in str(err.value))
    code = '''
      module my_mod
        integer :: var2, var3
        save
        save :: var3
      end module my_mod'''
    with pytest.raises(GenerationError) as err:
        _ = fortran_reader.psyir_from_source(code)
    assert ("Supplied nodes contain a SAVE without a saved-entity list plus "
            "one or more SAVES *with* saved-entity lists (naming ['var3']). "
            "This is not valid Fortran" in str(err.value))


def test_save_statement_subroutine(fortran_reader):
    '''Tests that save statements and attributes are correctly captured when
    they appear in a subroutine.'''
    code = '''
      subroutine my_sub()
        save :: var3
        integer :: var1, var4
        integer, save :: var2
        save :: var1
        integer :: var3
        write (*,*) var1, var2, var3
      end subroutine my_sub
'''
    psyir = fortran_reader.psyir_from_source(code)
    symtab = psyir.children[0].symbol_table
    for var in ["var1", "var2", "var3"]:
        varsym = symtab.lookup(var)
        assert isinstance(varsym.interface, StaticInterface)
    sym = symtab.lookup("var4")
    assert isinstance(sym.interface, AutomaticInterface)


def test_save_common_module(fortran_reader):
    '''
    Check that SAVE statements involving a named Common Block in a module
    and a subroutine are correctly captured.
    '''
    code = '''
      module my_mod
        save :: var3, /my_common/
        common /my_common/ a, b
        integer :: var1
        integer :: a, b
        integer :: var3
      contains
        subroutine my_sub()
          save :: /some_other_common/
          common /some_other_common/ igorina
          real :: igorina
        end subroutine my_sub
      end module my_mod'''
    psyir = fortran_reader.psyir_from_source(code)
    symtab = psyir.children[0].symbol_table
    var3 = symtab.lookup("var3")
    assert isinstance(var3.interface, StaticInterface)
    ufsym = symtab.lookup("/my_common/")
    assert isinstance(ufsym.datatype, UnknownFortranType)
    assert ufsym.datatype._declaration == "SAVE :: /my_common/"
    sub = psyir.walk(Routine)[0]
    other = sub.symbol_table.lookup("/some_other_common/")
    assert isinstance(other.datatype, UnknownFortranType)
    assert other.datatype._declaration == "SAVE :: /some_other_common/"
