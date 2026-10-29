# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2019-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

'''This module provides the TransformationError class.
'''

from psyclone.errors import PSycloneError, LazyString


class TransformationError(PSycloneError):
    ''' Provides a PSyclone-specific error class for errors found during
        code transformation operations. '''

    def __init__(self, value):
        super().__init__(value)
        self.value = LazyString(
            lambda: f"Transformation Error: {value}")
