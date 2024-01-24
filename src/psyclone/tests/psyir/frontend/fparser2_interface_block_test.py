# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2021-2024, Science and Technology Facilities Council.
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
# Modified: R. W. Ford, STFC Daresbury Lab
# -----------------------------------------------------------------------------

''' Performs py.test tests on the handling of interface blocks in the fparser2
    PSyIR front-end. '''

import pytest

from psyclone.psyir.nodes import Container
from psyclone.psyir.symbols import (
    RoutineSymbol, Symbol, UnsupportedFortranType, DataTypeSymbol)


def test_named_interface(fortran_reader):
    ''' Test that the frontend creates a RoutineSymbol of
    UnsupportedFortranType for a named interface block.'''
    dummy_module = '''
    module dummy_mod
      private
      public eos
      interface eos
        module procedure eos_insitu, eos_insitu_2d
      end interface
    contains
      subroutine eos_insitu(f1, f2)
          real, intent(in)  :: f1
          real, intent(out) :: f2
          f2 = f1 + 1
      end subroutine eos_insitu
      subroutine eos_insitu_2d(f1, f2)
          real, dimension(:,:), intent(in)  :: f1
          real, dimension(:,:), intent(out) :: f2
          f2(:,:) = f1(:,:) + 1
      end subroutine eos_insitu_2d
    end module dummy_mod
    '''
    file_container = fortran_reader.psyir_from_source(dummy_module)
    container = file_container.children[0]
    assert isinstance(container, Container)
    assert container.symbol_table.lookup("eos_insitu")
    eos = container.symbol_table.lookup("eos")
    assert isinstance(eos, RoutineSymbol)
    assert isinstance(eos.datatype, UnsupportedFortranType)
    assert (eos.datatype.declaration == "interface eos\n"
            "  module procedure eos_insitu, eos_insitu_2d\n"
            "end interface")
    assert eos.visibility == Symbol.Visibility.PUBLIC


def test_named_interface_declared(fortran_reader):
    ''' Test that the frontend creates a RoutineSymbol of
    UnsupportedFortranType for a named interface block when the symbol
    name has already been declared. '''
    test_module = '''
    module test_mod
      type test
      end type test
      interface test
        module procedure test_code
      end interface test
    contains
      real function test_code()
      end function test_code
    end module test_mod
    '''
    file_container = fortran_reader.psyir_from_source(test_module)
    container = file_container.children[0]
    assert isinstance(container, Container)
    # type symbol
    assert container.symbol_table.lookup("test")
    test_symbol = container.symbol_table.lookup("test")
    assert isinstance(test_symbol, DataTypeSymbol)
    # interface symbol
    assert container.symbol_table.lookup("_psyclone_internal_test")
    test_symbol = container.symbol_table.lookup("_psyclone_internal_test")
    assert isinstance(test_symbol, RoutineSymbol)
    assert isinstance(test_symbol.datatype, UnsupportedFortranType)
    assert (test_symbol.datatype.declaration == "interface test\n"
            "  module procedure test_code\n"
            "end interface test")
    assert test_symbol.visibility == Symbol.Visibility.PUBLIC


GENERIC_INTERFACE_CODE = '''
module dummy_mod
  interface
    subroutine ext1(x, y, z)
      real, dimension(100, 100) :: x, y, z
    end subroutine ext1
    subroutine ext2(x, z)
      real x
      complex (kind = 4) z(2000)
    end subroutine ext2
    function ext3(p, q)
      logical ext3
      integer p(1000)
      logical q(1000)
    end function ext3
  end interface
end module dummy_mod
'''

OPERATOR_INTERFACE_CODE = '''
module dummy_mod
  interface operator ( * )
    function boolean_and (b1, b2)
      logical, intent (in) :: b1 (:), b2 (size (b1))
      logical :: boolean_and (size (b1))
    end function boolean_and
  end interface operator ( * )
end module dummy_mod
'''

ASSIGNMENT_INTERFACE_CODE = '''
module dummy_mod
  interface assignment ( = )
    subroutine logical_to_numeric (n, b)
      integer, intent (out) :: n
      logical, intent (in) :: b
    end subroutine logical_to_numeric
    subroutine char_to_string (s, c)
      use string_module
      ! contains definition of type string
      type (string), intent (out) :: s ! a variable-length string
      character (*), intent (in) :: c
    end subroutine char_to_string
  end interface assignment ( = )
end module dummy_mod
'''

ABSTRACT_INTERFACE_CODE = '''
module dummy_mod
  abstract interface
    subroutine update_interface(field_name, field_proxy, times, xios_id)
      import i_def, field_proxy_type
      character(len=*),        intent(in) :: field_name
      type(field_proxy_type),  intent(inout) :: field_proxy
      integer(i_def),          intent(in) :: times(:)
      character(len=*),        intent(in) :: xios_id
    end subroutine update_interface
  end interface
end module dummy_mod
'''


@pytest.mark.parametrize("code, start, end", [
    (GENERIC_INTERFACE_CODE, "interface", "end interface"),
    (OPERATOR_INTERFACE_CODE,
     "interface operator(*)", "end interface operator(*)"),
    (ASSIGNMENT_INTERFACE_CODE,
     "interface assignment(=)", "end interface assignment(=)"),
    (ABSTRACT_INTERFACE_CODE, "abstract interface", "end interface")])
def test_unnamed_interface(fortran_reader, code, start, end):
    ''' Test that the frontend creates a RoutineSymbol of
    UnsupportedFortranType for the supplied unnamed interfaces. '''
    file_container = fortran_reader.psyir_from_source(code)
    container = file_container.children[0]
    assert isinstance(container, Container)
    # interface symbol
    interface_symbol = container.symbol_table.lookup(
        "_psyclone_internal_interface")
    assert isinstance(interface_symbol, RoutineSymbol)
    assert isinstance(interface_symbol.datatype, UnsupportedFortranType)
    assert interface_symbol.datatype.declaration.startswith(start)
    assert interface_symbol.datatype.declaration.endswith(end)
    assert interface_symbol.visibility == Symbol.Visibility.PUBLIC
