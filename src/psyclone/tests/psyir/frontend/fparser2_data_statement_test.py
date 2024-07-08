# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2024, Science and Technology Facilities Council.
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
# Author: J. Henrichs, Bureau of Meteorology
# -----------------------------------------------------------------------------

'''Tests Fortran data statements in the fparser2 PSyIR front-end. Data
statements are represented as an UnsupportedFortranType. The tests
here cover a few variations of the data statement, in case that at
some stage we need more information.
'''

import pytest
from fparser.common.readfortran import FortranStringReader
from fparser.two.Fortran2003 import Specification_Part
from psyclone.psyir.frontend.fparser2 import Fparser2Reader
from psyclone.psyir.nodes import Routine
from psyclone.psyir.symbols import UnsupportedFortranType


@pytest.mark.usefixtures("f2008_parser")
def test_simple_data_statement():
    ''' Test that data statements are correctly captured. '''

    # Create a dummy test routine
    routine = Routine("test_routine")
    symtab = routine.symbol_table
    processor = Fparser2Reader()

    # Test with a single namelist
    reader = FortranStringReader('''
        data a /1/''')
    fparser2spec = Specification_Part(reader)
    processor.process_declarations(routine, fparser2spec.content, [])
    # There is a data_stmt in the symbol table:
    data = symtab.lookup("_PSYCLONE_INTERNAL_DATA_STMT")
    assert isinstance(data.datatype, UnsupportedFortranType)
    assert data.datatype.declaration == "DATA a / 1 /"


@pytest.mark.usefixtures("f2008_parser")
def test_multiple_data_statement():
    ''' Test that data statements are correctly captured. '''

    # Create a dummy test routine
    routine = Routine("test_routine")
    symtab = routine.symbol_table
    processor = Fparser2Reader()

    # Test with a single namelist
    reader = FortranStringReader('''
        data a /1/, b/2/, c/3/''')
    fparser2spec = Specification_Part(reader)
    processor.process_declarations(routine, fparser2spec.content, [])
    # There is a data_stmt in the symbol table:
    data = symtab.lookup("_PSYCLONE_INTERNAL_DATA_STMT")
    assert isinstance(data.datatype, UnsupportedFortranType)
    assert data.datatype.declaration == "DATA a / 1 /, b / 2 /, c / 3 /"


@pytest.mark.usefixtures("f2008_parser")
def test_data_statement_implicit_loop():
    ''' Test that z data statement with an implicit loops is correctly
    captured. '''

    # Create a dummy test routine
    routine = Routine("test_routine")
    symtab = routine.symbol_table
    processor = Fparser2Reader()

    # Test with a single namelist
    reader = FortranStringReader(
        "DATA (es(ies),ies=    0, 5) / 0.966483e-02,0.966483e-02,"
        "0.984279e-02,0.100240e-01,0.102082e-01,0.103957e-01/")
    fparser2spec = Specification_Part(reader)
    processor.process_declarations(routine, fparser2spec.content, [])
    # There is a data_stmt in the symbol table:
    data = symtab.lookup("_PSYCLONE_INTERNAL_DATA_STMT")
    assert isinstance(data.datatype, UnsupportedFortranType)
    assert (data.datatype.declaration == "DATA (es(ies), ies = 0, 5) / "
            "0.966483E-02, 0.966483E-02, 0.984279E-02, 0.100240E-01, "
            "0.102082E-01, 0.103957E-01 /")


def test_data_statement_backend(fortran_reader, fortran_writer):
    '''Test the full process: parsing source code with data statements,
    converting them to PSyIR, and writing the out as Fortran.'''

    psyir = fortran_reader.psyir_from_source('''
        subroutine test()
            integer :: a, b, c(5)
            data a/1/, b/2/
            data (c(i), i=1,5) / 5, 4, 3, 2, 1/
        end subroutine test
      ''')
    code = fortran_writer(psyir)
    assert "DATA a / 1 /, b / 2 /" in code
    assert "DATA (c(i), i = 1, 5) / 5, 4, 3, 2, 1 /" in code
