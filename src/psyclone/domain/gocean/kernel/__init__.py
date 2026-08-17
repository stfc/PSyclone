# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2022-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

'''Module to capture GOcean-specific PSyIR for the Kernel
layer.

'''

from psyclone.domain.gocean.kernel.metadata import (
    GOceanArgDescriptor,
    GOceanFieldArgMetadata,
    GOceanGridPropertyArgMetadata,
    GOceanKernelMetadata,
    GOceanKernelProcedure,
    GOceanScalarArgMetadata,
    GOceanStencil,
    find_metadata_symbol,
)
from psyclone.domain.gocean.kernel.psyir import GOceanContainer

__all__ = [
    "GOceanArgDescriptor",
    "GOceanContainer",
    "GOceanFieldArgMetadata",
    "GOceanGridPropertyArgMetadata",
    "GOceanKernelMetadata",
    "GOceanKernelProcedure",
    "GOceanScalarArgMetadata",
    "GOceanStencil",
    "find_metadata_symbol",
]
