# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2022-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

"""LFRic-specific Container class for a kernel."""

from psyclone.domain.common.kernel.psyir import KernelContainer


class LFRicKernelContainer(KernelContainer):
    """A kernel container holding LFRic-specific metadata.

    :param str name: the name of the container.
    :param metadata: the LFRic kernel metadata object.
    :type metadata: :py:class:`psyclone.domain.lfric.kernel.\
        LFRicKernelMetadata`
    :param kwargs: additional keyword arguments passed to the parent class.
    :type kwargs: unwrapped dict
    """


__all__ = ["LFRicKernelContainer"]
