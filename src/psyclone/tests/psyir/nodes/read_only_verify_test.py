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
# Author: S. Siso, STFC Daresbury Laboratory
# -----------------------------------------------------------------------------

''' Module containing pytest tests for the ReadOnlyVerifyNode. '''

from __future__ import absolute_import
from psyclone.psyir.nodes import ReadOnlyVerifyNode, CodeBlock, Routine, \
    Reference, Return, IfBlock, Schedule
from psyclone.psyir.symbols import DataSymbol, INTEGER_TYPE


def test_read_only_verify_lower_to_language_level():
    ''' Test that the lowering methods works as expected. '''

    # Create a ReadOnlyVerify code with a read-only variable 'a'
    routine = Routine('my_routine')
    symbol = DataSymbol("a", INTEGER_TYPE)
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
        assert str(codeblock.ast) == code
