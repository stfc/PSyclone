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
from psyclone.domain.common.kernel.source import (
    find_kernel_file,
    parse_fortran_file,
    parse_fortran_source,
)

__all__ = [
    "find_kernel_file",
    "KernelInfo",
    "KernelMetadata",
    "metadata_structure",
    "parse_fortran_file",
    "parse_fortran_source",
]
