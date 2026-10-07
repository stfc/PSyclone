# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2019-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

'''This directory contains classes related to parsing Fortran.
'''

from psyclone.parse.file_info import FileInfo, FileInfoFParserError
from psyclone.parse.module_info import ModuleInfo, ModuleInfoError
from psyclone.parse.module_manager import ModuleManager


# For AutoAPI documentation generation.
__all__ = [
        'FileInfo',
        'FileInfoFParserError',
        'ModuleInfo',
        'ModuleInfoError',
        'ModuleManager'
        ]
