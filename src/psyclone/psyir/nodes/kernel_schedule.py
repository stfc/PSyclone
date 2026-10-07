# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2020-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

''' This module contains the KernelSchedule node implementation.'''

from psyclone.psyir.nodes.routine import Routine


class KernelSchedule(Routine):
    '''
    A KernelSchedule is the parent node of the PSyIR for Kernel source code.

    '''
    _text_name = "KernelSchedule"


# For automatic documentation generation
__all__ = ["KernelSchedule"]
