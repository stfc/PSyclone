# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2021-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

'''This module provides the TangentLinearError class.
'''

from psyclone.errors import PSycloneError

# Disable Python3-specific way of calling parent as support is still
# required for Python2.
# pylint: disable=super-with-arguments


class TangentLinearError(PSycloneError):
    '''Provides a PSyclone-specific error class for code that does not
    conform to the constraints required to be valid tangent linear
    code.

    :param str value: the message associated with the error.

    '''
    def __init__(self, value):

        super(TangentLinearError, self).__init__(value)
        self.value = "TangentLinearError: "+str(value)

    def __str__(self):
        return self.value
