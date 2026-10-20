# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2022-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

"""GOcean-specific Container class for a kernel."""

from psyclone.domain.common.kernel.psyir import KernelContainer


class GOceanKernelContainer(KernelContainer):
    """A kernel container holding GOcean-specific metadata.

    :param str name: the name of the container.
    :param metadata: the GOcean kernel metadata object.
    :type metadata: :py:class:`psyclone.domain.gocean.kernel.\
        GOceanKernelMetadata`
    :param kwargs: additional keyword arguments passed to the parent class.
    :type kwargs: unwrapped dict
    """


__all__ = ["GOceanKernelContainer"]
