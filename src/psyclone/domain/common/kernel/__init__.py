# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2026 Science and Technology
#                         Facilities Council.
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

"""Common support for API-specific kernel metadata."""

from psyclone.domain.common.kernel.metadata import (
    KernelInfo,
    KernelMetadata,
    metadata_structure,
)
__all__ = [
    "KernelInfo",
    "KernelMetadata",
    "metadata_structure",
]
