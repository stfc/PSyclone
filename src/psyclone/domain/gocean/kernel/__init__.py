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
    GOceanFieldArgMetadata,
    GOceanGridPropertyArgMetadata,
    GOceanKernelMetadata,
    GOceanScalarArgMetadata,
    GOceanStencilMetadata,
)
from psyclone.domain.gocean.kernel.psyir import GOceanContainer

__all__ = [
    "GOceanContainer",
    "GOceanFieldArgMetadata",
    "GOceanGridPropertyArgMetadata",
    "GOceanKernelMetadata",
    "GOceanScalarArgMetadata",
    "GOceanStencilMetadata",
]
