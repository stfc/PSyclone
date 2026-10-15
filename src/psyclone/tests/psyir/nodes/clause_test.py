# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2022-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

''' Performs py.test tests on the PSyIR Clause nodes. '''

from psyclone.psyir.nodes.clause import OperatorClause


def test_operator_clause(fortran_writer):
    '''
    Test the operator method of the operator clause.
    '''
    # Create a instanstiable class
    class OpTest(OperatorClause):
        ''' Temporary class for testing'''

    opt = OpTest()
    assert opt.operator is None
    assert fortran_writer(opt) == ""
