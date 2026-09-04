# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2021-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

'''A package module for domain/common.

Public objects are loaded lazily so importing a lightweight common subsystem
does not pull in the code-generation dependency graph.
'''


def __getattr__(name):
    """Load legacy package exports on first use."""
    if name == "DriverCreator":
        from psyclone.domain.common.driver_creator import DriverCreator
        return DriverCreator
    raise AttributeError(name)


__all__ = ["DriverCreator"]
