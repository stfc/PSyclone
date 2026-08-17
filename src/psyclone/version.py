# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

''' Single location for the current version number of PSyclone. This is
    used in setup.py and
    doc/{user_guide,developer_guide,reference_guide/source}/conf.py

    It is NOT used in doc/reference_guide/doxygen.config'''

__MAJOR__ = 3
__MINOR__ = 3
__MICRO__ = 1

# Version suffix e.g. "-rc1", "-dev" or "" (for a full release)
_VERSION_SUFFIX = ""

__SHORT_VERSION__ = f"{__MAJOR__:d}.{__MINOR__:d}{_VERSION_SUFFIX}"
__VERSION__ = f"{__MAJOR__:d}.{__MINOR__:d}.{__MICRO__:d}{_VERSION_SUFFIX}"
