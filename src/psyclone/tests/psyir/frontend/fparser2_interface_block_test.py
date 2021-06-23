# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2021, Science and Technology Facilities Council.
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

''' Performs py.test tests on the handling of interface blocks in the fparser2
    PSyIR front-end. '''

from __future__ import absolute_import
from psyclone.psyir.nodes import Container, CodeBlock
from psyclone.psyir.symbols import DataSymbol, Symbol, UnknownFortranType


def test_named_interface(fortran_reader):
    ''' Test that the frontend creates a symbol of UnknownFortranType for
        a named interface block.'''
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
    container = fortran_reader.psyir_from_source(dummy_module)
    assert isinstance(container, Container)
    assert container.symbol_table.lookup("eos_insitu")
    eos = container.symbol_table.lookup("eos")
    assert isinstance(eos, DataSymbol)
    assert isinstance(eos.datatype, UnknownFortranType)
    assert (eos.datatype.declaration == "interface eos\n"
            "  module procedure eos_insitu, eos_insitu_2d\n"
            "end interface")
    assert eos.visibility == Symbol.Visibility.PUBLIC


def test_generic_interface(fortran_reader):
    ''' Test that we create a CodeBlock if a module contains an interface block
        that defines interfaces to external routines. '''
    dummy_module = '''
    module dummy_mod
      INTERFACE
      SUBROUTINE EXT1 (X, Y, Z)
      REAL, DIMENSION (100, 100) :: X, Y, Z
      END SUBROUTINE EXT1
      SUBROUTINE EXT2 (X, Z)
      REAL X
      COMPLEX (KIND = 4) Z (2000)
      END SUBROUTINE EXT2
      FUNCTION EXT3 (P, Q)
      LOGICAL EXT3
      INTEGER P (1000)
      LOGICAL Q (1000)
      END FUNCTION EXT3
      END INTERFACE
    end module dummy_mod
    '''
    container = fortran_reader.psyir_from_source(dummy_module)
    assert isinstance(container, CodeBlock)


def test_operator_interface(fortran_reader):
    ''' Test that we create a CodeBlock if a module contains an interface block
    that defines an operator. '''
    dummy_module = '''
    module dummy_mod
      INTERFACE OPERATOR ( * )
      FUNCTION BOOLEAN_AND (B1, B2)
        LOGICAL, INTENT (IN) :: B1 (:), B2 (SIZE (B1))
        LOGICAL :: BOOLEAN_AND (SIZE (B1))
      END FUNCTION BOOLEAN_AND
      END INTERFACE OPERATOR ( * )
    end module dummy_mod
    '''
    container = fortran_reader.psyir_from_source(dummy_module)
    assert isinstance(container, CodeBlock)


def test_assignment_interface(fortran_reader):
    ''' Test that we create a CodeBlock if a module contains an interface block
    defining an assignment. '''
    dummy_module = '''
    module dummy_mod
    INTERFACE ASSIGNMENT ( = )
    SUBROUTINE LOGICAL_TO_NUMERIC (N, B)
    INTEGER, INTENT (OUT) :: N
    LOGICAL, INTENT (IN) :: B
    END SUBROUTINE LOGICAL_TO_NUMERIC
    SUBROUTINE CHAR_TO_STRING (S, C)
    USE STRING_MODULE
    ! Contains definition of type STRING
    TYPE (STRING), INTENT (OUT) :: S ! A variable-length string
    CHARACTER (*), INTENT (IN) :: C
    END SUBROUTINE CHAR_TO_STRING
    END INTERFACE ASSIGNMENT ( = )
    end module dummy_mod
    '''
    container = fortran_reader.psyir_from_source(dummy_module)
    assert isinstance(container, CodeBlock)
