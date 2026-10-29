# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2019-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

'''This module provides access to various classes used in determining
variable access information.
'''

from psyclone.core.access_sequence import AccessInfo, AccessSequence
from psyclone.core.variables_access_map import VariablesAccessMap
from psyclone.core.access_type import AccessType
from psyclone.core.signature import Signature
from psyclone.core.symbolic_maths import SymbolicMaths


# The entities in the __all__ list are made available to import directly from
# this package e.g. 'from psyclone.core import Signature'
__all__ = [
        'AccessInfo',
        'AccessSequence',
        'AccessType',
        'Signature',
        'SymbolicMaths',
        'VariablesAccessMap']
