# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2022-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

'''Module containing the ColumnwiseOperatorArgMetadata class which captures
the metadata associated with a columnwise operator argument. Supports the
creation, modification and Fortran output of a ColumnwiseOperator argument.

'''
from psyclone.domain.lfric.kernel.operator_arg_metadata import \
    OperatorArgMetadata


class ColumnwiseOperatorArgMetadata(OperatorArgMetadata):
    '''Class to capture LFRic kernel metadata information for a Columnwise
    operator argument.

    '''
    # The name used to specify a columnwise operator argument in LFRic
    # metadata.
    form = "gh_columnwise_operator"
    # The name to use for any exceptions.
    check_name = "columnwise-operator"


__all__ = ["ColumnwiseOperatorArgMetadata"]
