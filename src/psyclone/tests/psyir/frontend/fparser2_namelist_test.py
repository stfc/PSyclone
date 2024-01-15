# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2023-2024, Science and Technology Facilities Council.
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
#         J. Henrichs, Bureau of Meteorology
# -----------------------------------------------------------------------------

''' Tests Fortran namelist statements in the fparser2 PSyIR front-end '''

import pytest
from fparser.common.readfortran import FortranStringReader
from fparser.two.Fortran2003 import Specification_Part
from psyclone.psyir.frontend.fparser2 import Fparser2Reader
from psyclone.psyir.nodes import Routine
from psyclone.psyir.symbols import NamelistInterface, UnknownFortranType


@pytest.mark.usefixtures("f2008_parser")
def test_named_namelist():
    ''' Test that namelists are correctly captured and the symbols
    they reference have a NamelistInterface. '''

    # Create a dummy test routine
    routine = Routine("test_routine")
    symtab = routine.symbol_table
    processor = Fparser2Reader()

    # Test with a single namelist
    reader = FortranStringReader('''
        integer :: a, b, c
        NAMELIST /name1/ a, b,  c''')
    fparser2spec = Specification_Part(reader)
    processor.process_declarations(routine, fparser2spec.content, [])

    # There is a name1 namelist symbol
    namelist = symtab.lookup("_PSYCLONE_INTERNAL_NAMELIST")
    assert isinstance(namelist.datatype, UnknownFortranType)
    assert namelist.datatype.declaration == "NAMELIST /name1/ a, b, c"

    # The variables have been updated to a namelist interface
    assert isinstance(symtab.lookup("a").interface, NamelistInterface)
    assert isinstance(symtab.lookup("b").interface, NamelistInterface)
    assert isinstance(symtab.lookup("c").interface, NamelistInterface)


@pytest.mark.usefixtures("f2008_parser")
def test_multiple_namelists_in_statement():
    ''' Test that namelist statements with multiple namelists are handled
    correctly.'''

    # Create a dummy test routine
    routine = Routine("test_routine")
    symtab = routine.symbol_table
    processor = Fparser2Reader()

    # And provide a namelist statement containing two namelists
    reader = FortranStringReader('''
        integer :: a, b, c, d
        namelist /name1/ a, b /name2/ c
        namelist /name2/ d''')
    fparser2spec = Specification_Part(reader)
    processor.process_declarations(routine, fparser2spec.content, [])

    # There is a UnknownFortranType symbol containing each the namelist
    namelist = symtab.lookup("_PSYCLONE_INTERNAL_NAMELIST")
    assert isinstance(namelist.datatype, UnknownFortranType)
    assert (namelist.datatype.declaration ==
            "NAMELIST /name1/ a, b, /name2/ c")
    namelist = symtab.lookup("_PSYCLONE_INTERNAL_NAMELIST_1")
    assert isinstance(namelist.datatype, UnknownFortranType)
    assert namelist.datatype.declaration == "NAMELIST /name2/ d"

    # The variables have been updated to a namelist interface
    assert isinstance(symtab.lookup("a").interface, NamelistInterface)
    assert isinstance(symtab.lookup("b").interface, NamelistInterface)
    assert isinstance(symtab.lookup("c").interface, NamelistInterface)
    assert isinstance(symtab.lookup("d").interface, NamelistInterface)


@pytest.mark.usefixtures("f2008_parser")
def test_namelist_with_posterior_declaration():
    ''' Test that namelists with symbols that are declared after the
    namelist statement are handled correctly.'''

    # Create a dummy test routine
    routine = Routine("test_routine")
    symtab = routine.symbol_table
    processor = Fparser2Reader()

    reader = FortranStringReader('''
        namelist /name1/ a, b
        integer :: a, b''')
    fparser2spec = Specification_Part(reader)
    processor.process_declarations(routine, fparser2spec.content, [])

    # There is an UnknownFortranType symbol containing the namelist
    namelist = symtab.lookup("_PSYCLONE_INTERNAL_NAMELIST")
    assert isinstance(namelist.datatype, UnknownFortranType)
    assert namelist.datatype.declaration == "NAMELIST /name1/ a, b"

    # The variables have been updated to a namelist interface
    assert isinstance(symtab.lookup("a").interface, NamelistInterface)
    assert isinstance(symtab.lookup("b").interface, NamelistInterface)


@pytest.mark.usefixtures("f2008_parser")
def test_namelist_undeclared_symbol():
    ''' Test that namelist of symbols that have not been declared
    produce NotImplementedError.'''

    # Create a dummy test routine
    routine = Routine("test_routine")
    processor = Fparser2Reader()

    # This is also valid Fortran, but currently not supported
    reader = FortranStringReader('''
        namelist /name1/ a, b
        integer :: a''')
    fparser2spec = Specification_Part(reader)
    with pytest.raises(NotImplementedError) as err:
        processor.process_declarations(routine, fparser2spec.content, [])
    assert ("The symbol interface of a namelist variable could not be "
            "updated because of \"Could not find 'b' in the Symbol Table.\"."
            in str(err.value))
