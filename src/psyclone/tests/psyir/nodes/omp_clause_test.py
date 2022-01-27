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
# Authors A. B. G. Chalk, STFC Daresbury Lab
# -----------------------------------------------------------------------------

''' Performs py.test tests on the OpenMP PSyIR Clause nodes. '''

from psyclone.psyir.nodes.omp_clauses import GrainsizeClause, NowaitClause,\
    NogroupClause, NumTasksClause
from psyclone.psyir.nodes.literal import Literal
from psyclone.psyir.symbols import INTEGER_TYPE


def test_nowait_clause():
    ''' Test the NowaitClause functionality. '''
    nowait = NowaitClause()
    assert nowait.clause_string == "nowait"
    assert NowaitClause._validate_child(0, nowait) is False


def test_grainsize_clause():
    ''' Test the GrainsizeClause functionality. '''

    nowait = GrainsizeClause(children=[Literal("32", INTEGER_TYPE)])
    assert nowait.clause_string == "grainsize"
    assert GrainsizeClause._validate_child(0, Literal("64", INTEGER_TYPE)) is\
        True
    assert GrainsizeClause._validate_child(1, Literal("64", INTEGER_TYPE)) is\
        False


def test_numtasks_clause():
    ''' Test the NumTasksClause functionality. '''
    nowait = NumTasksClause(children=[Literal("32", INTEGER_TYPE)])
    assert nowait.clause_string == "num_tasks"
    assert NumTasksClause._validate_child(0, Literal("64", INTEGER_TYPE)) is\
        True
    assert NumTasksClause._validate_child(1, Literal("64", INTEGER_TYPE)) is\
        False


def test_nogroup_clause():
    ''' Test the NogroupClause functionality. '''
    nowait = NogroupClause()
    assert nowait.clause_string == "nogroup"
    assert NogroupClause._validate_child(0, nowait) is False
