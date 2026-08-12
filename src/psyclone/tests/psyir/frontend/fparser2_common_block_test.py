# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2023-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

''' Tests Fortran common blocks in the fparser2 PSyIR front-end '''

import pytest
from fparser.common.readfortran import FortranStringReader
from fparser.two.Fortran2003 import Specification_Part
from psyclone.psyir.frontend.fparser2 import Fparser2Reader
from psyclone.psyir.nodes import Routine
from psyclone.psyir.symbols import CommonBlockInterface, ScalarType


@pytest.mark.usefixtures("f2008_parser")
def test_named_common_block():
    ''' Test that named common blocks are correctly captured and the symbols
    they reference have a CommonBlockInterface. '''

    # Create a dummy test routine
    routine = Routine.create("test_routine")
    symtab = routine.symbol_table
    processor = Fparser2Reader()

    # Test with a single common block
    reader = FortranStringReader('''
        integer(kind=i_def) :: a, b, c
        common /name1/ a, b, c''')
    fparser2spec = Specification_Part(reader)
    processor.process_declarations(routine, fparser2spec.content, [])

    # The variables have been updated to a common block interface
    name1_cb = CommonBlockInterface('name1')
    assert symtab.lookup("a").interface == name1_cb
    assert symtab.lookup("b").interface == name1_cb
    assert symtab.lookup("c").interface == name1_cb

    # The same common block can also bring other variables in a separate
    # statement
    reader = FortranStringReader('''
        real :: d, e
        real(kind=wp) :: f
        common /name1/ d, e, f''')
    fparser2spec = Specification_Part(reader)
    processor.process_declarations(routine, fparser2spec.content, [])

    assert symtab.lookup("d").interface == name1_cb
    assert symtab.lookup("e").interface == name1_cb
    fsym = symtab.lookup("f")
    assert isinstance(fsym.interface, CommonBlockInterface)
    assert fsym.datatype.intrinsic is ScalarType.Intrinsic.REAL


@pytest.mark.usefixtures("f2008_parser")
def test_unnamed_commonblock():
    ''' Test that unnamed common blocks are handled correctly.'''

    # Create a dummy test routine
    routine = Routine.create("test_routine")
    symtab = routine.symbol_table
    processor = Fparser2Reader()

    # And provide an unnamed common block
    reader = FortranStringReader('''
        integer :: a, b, c
        common a, b, c''')
    fparser2spec = Specification_Part(reader)
    processor.process_declarations(routine, fparser2spec.content, [])

    # The variables have been updated to the unnamed common block interface
    unnamed_cb = CommonBlockInterface("")
    assert symtab.lookup("a").interface == unnamed_cb
    assert symtab.lookup("b").interface == unnamed_cb
    assert symtab.lookup("c").interface == unnamed_cb


@pytest.mark.usefixtures("f2008_parser")
def test_multiple_commonblocks_and_comments():
    ''' Test that common block statements with multiple common blocks
    and comments are handled correctly.'''

    # Create a dummy test routine
    routine = Routine.create("test_routine")
    symtab = routine.symbol_table
    processor = Fparser2Reader()

    # And provide a common block containing two named blocks
    code = ('''
        integer :: a, b, c, d
        ! This is the first common block
        common /name1/ a, b /name2/ c  ! Inline comment
        ! This is the second common block
        common /name2/ d  ! Inline comment
        ! Comment after
        ''')
    fparser2spec = processor.generate_parse_tree_from_source(
        code, partial_code="specs")
    processor.process_declarations(routine, fparser2spec.content, [])

    # The variables have been updated to a common block interface
    name1_cb = CommonBlockInterface('name1')
    name2_cb = CommonBlockInterface('name2')
    assert symtab.lookup("a").interface == name1_cb
    assert symtab.lookup("b").interface == name1_cb
    assert symtab.lookup("c").interface == name2_cb
    assert symtab.lookup("d").interface == name2_cb

    # The comments are currently discarded
    assert symtab.lookup("a").preceding_comment == ""


@pytest.mark.usefixtures("f2008_parser")
def test_named_commonblock_with_posterior_declaration():
    ''' Test that commonblocks with symbols that are declared after the
    commonblock statement are handled correctly.'''

    # Create a dummy test routine
    routine = Routine.create("test_routine")
    symtab = routine.symbol_table
    processor = Fparser2Reader()

    # This is also valid Fortran, but currently not supported
    reader = FortranStringReader('''
        common /name1/ a, b
        integer :: a, b''')
    fparser2spec = Specification_Part(reader)
    processor.process_declarations(routine, fparser2spec.content, [])

    # The variables have been updated to a common block interface
    assert isinstance(symtab.lookup("a").interface, CommonBlockInterface)
    assert isinstance(symtab.lookup("b").interface, CommonBlockInterface)


@pytest.mark.usefixtures("f2008_parser")
def test_undeclared_symbol():
    ''' Test that commonblocks of symbols that have not been declared
    produce NotImplementedError.'''

    # Create a dummy test routine
    routine = Routine.create("test_routine")
    processor = Fparser2Reader()

    # This is also valid Fortran, but currently not supported
    reader = FortranStringReader('''
        common /name1/ a, b
        integer :: a''')
    fparser2spec = Specification_Part(reader)
    with pytest.raises(NotImplementedError) as err:
        processor.process_declarations(routine, fparser2spec.content, [])
    assert ("The symbol interface of a common block variable could not be "
            "updated because of \"Could not find 'b' in the Symbol Table.\"."
            in str(err.value))


@pytest.mark.usefixtures("f2008_parser")
def test_commonblock_with_explicit_array_shape_symbol():
    ''' Test that commonblocks with an explicit-shape-spec-list
    produce NotImplementedError.'''

    # Create a dummy test routine
    routine = Routine.create("test_routine")
    processor = Fparser2Reader()

    # This is also valid Fortran, but currently not supported
    reader = FortranStringReader('''
        integer :: a
        common /name1/ a (10, 4)''')

    fparser2spec = Specification_Part(reader)
    with pytest.raises(NotImplementedError) as err:
        processor.process_declarations(routine, fparser2spec.content, [])
    assert ("The symbol interface of a common block variable could not be "
            "updated because of \"Could not find 'a(10, 4)' in the Symbol "
            "Table.\"." in str(err.value))


@pytest.mark.usefixtures("f2008_parser")
def test_commonblock_with_explicit_init_symbol():
    ''' Test that commonblocks containing a symbol declared with explicit
    initialisation produce NotImplementedError.'''

    # Create a dummy test routine
    routine = Routine.create("test_routine")
    processor = Fparser2Reader()

    # This is also invalid Fortran, but fparser2 doesn't notice.
    reader = FortranStringReader('''
        integer :: a = 10
        common /name1/ a''')
    fparser2spec = Specification_Part(reader)
    with pytest.raises(NotImplementedError) as err:
        processor.process_declarations(routine, fparser2spec.content, [])
    assert ("Symbol 'a' has an initial value (10) but appears in a common "
            "block." in str(err.value))
