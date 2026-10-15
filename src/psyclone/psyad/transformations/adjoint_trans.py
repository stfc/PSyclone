# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2021-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

'''This module contains an abstract parent class for adjoint
transformations.

'''
from psyclone.psyGen import Transformation
from psyclone.psyir.symbols import DataSymbol

# AdjointTransformation is purposefully abstract. It does not
# implement the validate or apply methods and instead provides
# initialisation that all subclasses can benefit from. Therefore we
# disable the pylint warning here.
# pylint: disable=abstract-method

# We make use of the form of super that works with both Python2 and
# Python3 here so disable the pylint warning about using a Python3
# specific version of super.
# pylint: disable=super-with-arguments


class AdjointTransformation(Transformation):
    '''An abstract class for Adjoint transformations. Requires a list of
    active variables to be passed when creating an instance of the
    class. Also supports an optional writer argument.

    :param active_variables: a list of names of the active variables.
    :type active_variables: list of \
        :py:class:`psyclone.psyir.symbols.DataSymbol`

    :raises TypeError: if the active_variables are of the wrong type.

    '''
    def __init__(self, active_variables):
        super(AdjointTransformation, self).__init__()

        if not isinstance(active_variables, list):
            raise TypeError(
                f"The active variables argument should be a list, but found "
                f"'{type(active_variables).__name__}'.")

        if not active_variables:
            raise TypeError("There should be at least one active variable.")

        for active_variable in active_variables:
            if not isinstance(active_variable, DataSymbol):
                raise TypeError(
                    f"Active variables should be of type DataSymbol, but "
                    f"found '{type(active_variable).__name__}'.")

        # A list of active variables.
        self._active_variables = active_variables


# =============================================================================
# Documentation utils: The list of module members that we wish AutoAPI to
# generate documentation for.
__all__ = ["AdjointTransformation"]
