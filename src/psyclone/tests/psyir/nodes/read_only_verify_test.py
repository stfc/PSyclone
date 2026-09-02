# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2021-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

''' Module containing pytest tests for the ReadOnlyVerifyNode. '''

from psyclone.psyir.nodes import ReadOnlyVerifyNode, CodeBlock, Routine, \
    Reference, Return, IfBlock, Schedule
from psyclone.psyir.symbols import DataSymbol, ScalarType


def test_read_only_verify_lower_to_language_level():
    ''' Test that the lowering methods works as expected. '''

    # Create a ReadOnlyVerify code with a read-only variable 'a'
    routine = Routine.create('my_routine')
    symbol = DataSymbol("a", ScalarType.integer_type())
    routine.symbol_table.add(symbol)
    node = ReadOnlyVerifyNode()
    routine.addchild(node)
    schedule = Schedule()
    node.addchild(schedule)
    schedule.addchild(IfBlock.create(Reference(symbol), [Return()]))

    node.lower_to_language_level()

    expected = ['CALL read_only_verify_psy_data % PreStart("my_routine", '
                '"r0", 1, 1)',
                'CALL read_only_verify_psy_data % PreDeclareVariable("a", a)',
                'CALL read_only_verify_psy_data % PreDeclareVariable("a", a)',
                'CALL read_only_verify_psy_data % PreEndDeclaration',
                'CALL read_only_verify_psy_data % ProvideVariable("a", a)',
                'CALL read_only_verify_psy_data % PreEnd',
                'CALL read_only_verify_psy_data % PostStart',
                'CALL read_only_verify_psy_data % ProvideVariable("a", a)',
                'CALL read_only_verify_psy_data % PostEnd']

    for codeblock, code in zip(routine.walk(CodeBlock), expected):
        assert codeblock.get_fortran_lines()[0] == code
