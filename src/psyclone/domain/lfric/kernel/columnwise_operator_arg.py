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

'''Module containing the ColumnwiseOperatorArg class which captures
the metadata associated with a columnwise operator argument. Supports the
creation, modification and Fortran output of a ColumnwiseOperator argument.

'''
from psyclone.domain.lfric.kernel.operator_arg import OperatorArg


class ColumnwiseOperatorArg(OperatorArg):
    '''Class to capture LFRic kernel metadata information for a Columnwise
    operator argument.

    :param Optional[str] datatype: the datatype of this Columnwise \
        Operator (GH_INTEGER, ...).
    :param Optional[str] access: the way the kernel accesses this \
        operator (GH_WRITE, ...).
    :param Optional[str] function_space_to: the function space that this \
        operator maps to (W0, ...).
    :param Optional[str] function_space_from: the function space that this \
        operator maps from (W0, ...).

    '''
    form = "GH_COLUMNWISE_OPERATOR"

    @staticmethod
    def create_from_fparser2(fparser2_tree):
        '''Create an instance of this class from an fparser2 tree.

        :param fparser2_tree: fparser2 tree capturing the metadata for a \
            columnwise operator argument.
        :type fparser2_tree: :py:class:`fparser.two.Fortran2003.Part_Ref`

        :returns: an instance of ColumnwiseOperatorArg.
        :rtype: :py:class:`psyclone.domain.lfric.kernel.ColumnwiseOperatorArg`

        '''
        ColumnwiseOperatorArg.check_fparser2(fparser2_tree, nargs=5)
        ColumnwiseOperatorArg.check_first_arg(fparser2_tree, "ColumnwiseOperator")
        datatype, access = ColumnwiseOperatorArg.get_type_and_access(
            fparser2_tree, ColumnwiseOperatorArg.datatype_arg_index,
            ColumnwiseOperatorArg.access_arg_index)
        function_space_to = ColumnwiseOperatorArg.get_arg(
            fparser2_tree, ColumnwiseOperatorArg.function_space_to_arg_index)
        function_space_from = ColumnwiseOperatorArg.get_arg(
            fparser2_tree, ColumnwiseOperatorArg.function_space_from_arg_index)
        ColumnwiseOperatorArg.check_remaining_args(
            fparser2_tree, datatype, access, function_space_to,
            function_space_from)
        return ColumnwiseOperatorArg(
            datatype, access, function_space_to, function_space_from)
