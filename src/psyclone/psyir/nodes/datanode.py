# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2020-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

''' This module contains the DataNode abstract node implementation.'''

from typing import Optional

from psyclone.psyir.nodes.node import Node


class DataNode(Node):
    '''
    Abstract node representing a general PSyIR expression that represents a
    value, which has a datatype.

    '''
    @property
    def datatype(self):
        '''
        :returns: the data-type of this Node. Currently this base
            implementation just returns UnresolvedType(). If a sub-class can do
            better then it must override this method.
        :rtype: :py:class:`psyclone.psyir.symbols.UnresolvedType`
        '''
        # pylint: disable=import-outside-toplevel
        from psyclone.psyir.nodes.loop import Loop
        from psyclone.psyir.nodes.ranges import Range
        from psyclone.psyir.symbols.datatypes import (
            UnresolvedType, ScalarType)
        # If it is a direct child of Loop or Range, it can only be an Integer
        if self.parent and isinstance(self.parent, (Loop, Range)):
            return ScalarType.integer_type()
        return UnresolvedType()

    def is_character(self, unknown_as: Optional[bool] = None) -> bool:
        '''
        :param unknown_as: Determines behaviour in the case where it cannot be
            determined whether the DataNode is a character. Defaults to None,
            in which case an exception is raised.

        :returns: True if this DataNode is a character, otherwise False.

        :raises ValueError: if the intrinsic type cannot be determined.

        '''
        dtype = self.datatype
        if not hasattr(dtype, "intrinsic"):
            if unknown_as is None:
                raise ValueError(
                    "is_character could not resolve whether the expression"
                    f" '{self.debug_string()}' operates on characters."
                )
            return unknown_as
        # pylint: disable=import-outside-toplevel
        from psyclone.psyir.symbols.datatypes import ScalarType
        return dtype.intrinsic == ScalarType.Intrinsic.CHARACTER
