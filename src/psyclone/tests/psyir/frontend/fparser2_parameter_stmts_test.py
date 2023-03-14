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
# Author: S. Siso, STFC Daresbury Lab
# -----------------------------------------------------------------------------

''' Tests Fortran parameter statements in the fparser2 PSyIR front-end '''

import pytest
from fparser.common.readfortran import FortranStringReader
from fparser.two.Fortran2003 import Specification_Part
from psyclone.psyir.frontend.fparser2 import Fparser2Reader
from psyclone.psyir.nodes import Routine, Literal, BinaryOperation, \
    Container, CodeBlock
from psyclone.psyir.symbols import Symbol


@pytest.mark.usefixtures("f2008_parser")
def test_parameter_statements_work():
    '''Tests that parameter statements are correctly captured in the
    constant_value symbol attribute. '''

    # Create a dummy test routine
    routine = Routine("test_routine")
    symtab = routine.symbol_table
    processor = Fparser2Reader()

    # Test with a single parameter
    reader = FortranStringReader('''
        integer :: var1
        parameter (var1=3)''')
    fparser2spec = Specification_Part(reader)
    processor.process_declarations(routine, fparser2spec.content, [])
    newsymbol = symtab.lookup("var1")
    assert newsymbol.is_constant
    assert isinstance(newsymbol.constant_value, Literal)
    assert newsymbol.constant_value.value == "3"

    # Test with a single parameter with an expression
    reader = FortranStringReader('''
        integer :: var_expr
        parameter (var_expr=10+4)''')
    fparser2spec = Specification_Part(reader)
    processor.process_declarations(routine, fparser2spec.content, [])
    newsymbol1 = symtab.lookup("var_expr")
    assert newsymbol1.is_constant
    assert isinstance(newsymbol1.constant_value, BinaryOperation)
    assert newsymbol1.constant_value.children[0].value == "10"

    # Test with multiple parameters of different types
    reader = FortranStringReader('''
        integer :: var2
        real :: var3
        logical :: var4
        character :: var5
        parameter (var2=1, var3=3.14, var4=.TRUE., var5='a')''')
    fparser2spec = Specification_Part(reader)
    processor.process_declarations(routine, fparser2spec.content, [])
    newsymbol2 = symtab.lookup("var2")
    newsymbol3 = symtab.lookup("var3")
    newsymbol4 = symtab.lookup("var4")
    newsymbol5 = symtab.lookup("var5")
    assert newsymbol2.is_constant
    assert newsymbol3.is_constant
    assert newsymbol4.is_constant
    assert newsymbol5.is_constant
    assert isinstance(newsymbol2.constant_value, Literal)
    assert newsymbol2.constant_value.value == "1"
    assert isinstance(newsymbol3.constant_value, Literal)
    assert newsymbol3.constant_value.value == "3.14"
    assert isinstance(newsymbol4.constant_value, Literal)
    assert newsymbol4.constant_value.value == "true"
    assert isinstance(newsymbol5.constant_value, Literal)
    assert newsymbol5.constant_value.value == "a"


@pytest.mark.usefixtures("f2008_parser")
def test_parameter_statements_with_unsupported_symbols():
    '''Tests that when parameter statements fail, a NotImplementedError
    with an appropriate error message is produced.'''

    # Create a dummy test routine
    routine = Routine("test_routine")
    symtab = routine.symbol_table
    symtab.add(Symbol("var2"))
    processor = Fparser2Reader()

    # Test with a UnknownType declaration
    reader = FortranStringReader('''
        character*5 :: var1
        parameter (var1='hello')''')
    fparser2spec = Specification_Part(reader)

    with pytest.raises(NotImplementedError) as error:
        processor.process_declarations(routine, fparser2spec.content, [])
    assert ("Could not parse 'PARAMETER(var1 = 'hello')' because 'var1' has "
            "an UnknownType." in str(error.value))

    # Test with a symbol which is not a DataSymbol
    reader = FortranStringReader('''
        parameter (var2='hello')''')
    fparser2spec = Specification_Part(reader)

    with pytest.raises(NotImplementedError) as error:
        processor.process_declarations(routine, fparser2spec.content, [])
    assert ("Could not parse 'PARAMETER(var2 = 'hello')' because 'var2' is not"
            " a DataSymbol." in str(error.value))


def test_unsupported_parameter_statements_produce_codeblocks(fortran_reader,
                                                             fortran_writer):
    '''Tests that when parameter statements fail, the whole routine or module
    ends up in a CodeBlock.
    '''

    psyir = fortran_reader.psyir_from_source('''
        module my_mod
            contains
            subroutine my_sub()
                integer :: var
                character*5 :: var1
                parameter (var=3, var1='hello')
            end subroutine my_sub
        end module my_mod
        ''')
    assert isinstance(psyir.children[0], Container)
    assert isinstance(psyir.children[0].children[0], CodeBlock)

    psyir = fortran_reader.psyir_from_source('''
        module my_mod
            character*5 :: var1
            parameter (var1='hello')
            contains
            subroutine my_sub()
                integer :: var
                parameter (var=3)
            end subroutine my_sub
        end module my_mod
        ''')
    assert isinstance(psyir.children[0], CodeBlock)

    # An therefore, the backend is able to reproduce it without losing any
    # statements
    code = fortran_writer(psyir)
    assert code == '''\
MODULE my_mod
  CHARACTER*5 :: var1
  PARAMETER(var1 = 'hello')
  CONTAINS
  SUBROUTINE my_sub
    INTEGER :: var
    PARAMETER(var = 3)
  END SUBROUTINE my_sub
END MODULE my_mod
'''
