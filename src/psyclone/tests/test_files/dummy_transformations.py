# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

import abc


class LocalTransformation():
    '''abstract baseclass for a transformation. Use of abc means it can
    not be instantiated.

    '''
    __metaclass__ = abc.ABCMeta

    @abc.abstractmethod
    def name(self):
        ...


class TestTrans(LocalTransformation):
    ''' A placeholder test transformation '''

    def __init__(self):
        pass

    def __str__(self):
        ...

    @property
    def name(self):
        return "testTrans"
