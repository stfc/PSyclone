# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2022-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

'''Module to capture GOcean-specific PSyIR for the Kernel
layer.

'''
from psyclone.domain.gocean.kernel.psyir import GOceanContainer, \
    GOceanKernelMetadata

__all__ = [
    "GOceanKernelMetadata",
]
