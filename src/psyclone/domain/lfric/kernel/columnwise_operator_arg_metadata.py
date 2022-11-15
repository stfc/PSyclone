# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2022, Science and Technology Facilities Council
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
#
# * Redistributions of source code must retain the above copyright notice, this
#   list of conditions and the following disclaimer.
#
# * Redistributions in binary form must reproduce the above copyright notice,
#   this list of conditions and the following disclaimer in the documentation
#   and/or other materials provided with the distribution.
#
# * Neither the name of the copyright holder nor the names of its
#   contributors may be used to endorse or promote products derived from
#   this software without specific prior written permission.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
# "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
# LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
# FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
# COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
# INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
# BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
# LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
# CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
# LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
# ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
# POSSIBILITY OF SUCH DAMAGE.
# -----------------------------------------------------------------------------
# Author R. W. Ford, STFC Daresbury Lab

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

    @staticmethod
    def create_from_fparser2(fparser2_tree):
        '''Create an instance of this class from an fparser2 tree.

        :param fparser2_tree: fparser2 tree capturing the metadata for a \
            columnwise operator argument.
        :type fparser2_tree: :py:class:`fparser.two.Fortran2003.Part_Ref`

        :returns: an instance of ColumnwiseOperatorArgMetadata.
        :rtype: :py:class:`psyclone.domain.lfric.kernel.\
            ColumnwiseOperatorArgMetadata`

        '''
        ColumnwiseOperatorArgMetadata.check_fparser2_arg(
            fparser2_tree, "arg_type")
        ColumnwiseOperatorArgMetadata.check_nargs(fparser2_tree, 5)
        ColumnwiseOperatorArgMetadata.check_first_arg(
            fparser2_tree, "columnwise-operator")
        datatype, access = ColumnwiseOperatorArgMetadata.get_type_and_access(
            fparser2_tree)
        function_space_to = ColumnwiseOperatorArgMetadata.get_arg(
            fparser2_tree,
            ColumnwiseOperatorArgMetadata.function_space_to_arg_index)
        function_space_from = ColumnwiseOperatorArgMetadata.get_arg(
            fparser2_tree,
            ColumnwiseOperatorArgMetadata.function_space_from_arg_index)
        ColumnwiseOperatorArgMetadata.check_remaining_args(
            fparser2_tree, datatype, access, function_space_to,
            function_space_from)
        return ColumnwiseOperatorArgMetadata(
            datatype, access, function_space_to, function_space_from)


__all__ = ["ColumnwiseOperatorArgMetadata"]
