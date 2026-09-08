# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

'''Module containing tests for the LFRicColourAndOMPTrans transformation.'''

from psyclone.domain.lfric.transformations import LFRicColourAndOMPTrans


# so CI doesnt complain
def test_import():
    ''' Check the transformation can be imported. '''
    assert LFRicColourAndOMPTrans()
