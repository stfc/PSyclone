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

''' Tests Fortran common blocks in the fparser2 PSyIR front-end '''

import pytest
from fparser.common.readfortran import FortranStringReader
from fparser.two.Fortran2003 import Specification_Part
from psyclone.psyir.frontend.fparser2 import Fparser2Reader
from psyclone.psyir.nodes import Routine
from psyclone.psyir.symbols import CommonBlockInterface, \
    UnknownFortranType


@pytest.mark.usefixtures("f2008_parser")
def test_named_common_block():
    ''' Test that named common blocks are correctly captured and the symbols
    they reference have a CommonBlockInterface. '''

    # Create a dummy test routine
    routine = Routine("test_routine")
    symtab = routine.symbol_table
    processor = Fparser2Reader()

    # Test with a single parameter
    reader = FortranStringReader('''
        integer :: a, b, c
        common /name1/ a, b, c''')
    fparser2spec = Specification_Part(reader)
    processor.process_declarations(routine, fparser2spec.content, [])

    # There is a name1 commonblock symbol
    commonblock = symtab.lookup("_PSYCLONE_COMMONBLOCK_name1")
    assert isinstance(commonblock.datatype, UnknownFortranType)
    assert commonblock.datatype.declaration == "COMMON /name1/ a, b, c"

    # The variables have been updated to a common block interface
    assert isinstance(symtab.lookup("a").interface, CommonBlockInterface)
    assert isinstance(symtab.lookup("b").interface, CommonBlockInterface)
    assert isinstance(symtab.lookup("c").interface, CommonBlockInterface)


@pytest.mark.usefixtures("f2008_parser")
def test_named_commonblock_with_posterior_declaration():
    ''' Test that commonblocks of symbols that have not been declared
    yet produce NotImplementedError'''

    # Create a dummy test routine
    routine = Routine("test_routine")
    processor = Fparser2Reader()

    # This is also valid Fortran, but currently not supported
    reader = FortranStringReader('''
        common /name1/ a, b
        integer :: a, b''')
    fparser2spec = Specification_Part(reader)
    with pytest.raises(NotImplementedError) as err:
        processor.process_declarations(routine, fparser2spec.content, [])
    assert ("The symbol interface of a common block variable could not be "
            "updated because of \"Could not find 'a' in the Symbol Table.\". "
            "Currently we only support commonblocks of symbols that have "
            "previously been declared." in str(err.value))


@pytest.mark.usefixtures("f2008_parser")
def test_unnamed_commonblock():
    ''' Test that unnamed common blocks produce NotImplementedError '''

    # Create a dummy test routine
    routine = Routine("test_routine")
    processor = Fparser2Reader()

    # Test with a single parameter
    reader = FortranStringReader('''
        integer :: a, b, c
        common a, b, c''')
    fparser2spec = Specification_Part(reader)

    with pytest.raises(NotImplementedError) as err:
        processor.process_declarations(routine, fparser2spec.content, [])
    assert ("Unammed Common blocks not supported, but found 'COMMON // a, b, "
            "c'." in str(err.value))
