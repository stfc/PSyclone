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
from psyclone.domain.lfric.kernel import OperatorArg


class ColumnwiseOperatorArg(OperatorArg):
    '''Class to capture LFRic kernel metadata information for a Columnwise
    operator argument.

    :param Optional[str] datatype: the datatype of this Columnwise \
        Operator (GH_INTEGER, ...).
    :param Optional[str] access: the way the kernel accesses this \
        field (GH_WRITE, ...).
    :param Optional[str] function_space1: the function space that this \
        field maps from (W0, ...).
    :param Optional[str] function_space2: the function space that this \
        field maps to (W0, ...).

    '''
    def __init__(self, datatype=None, access=None, function_space1=None,
                 function_space2=None):
        super().__init__(datatype, access, function_space1, function_space2)
        self._form = "GH_COLUMNWISE_OPERATOR"

    @staticmethod
    def create_from_psyir(psyir):
        '''Create an instance of this class from generic PSyIR. At this moment
        this information is captured in an fparser2 tree.

        :param psyir: fparser2 tree containing the PSyIR for a \
            columnwise operator argument.
        :type psyir: :py:class:`fparser.two.Fortran2003.Part_Ref`

        :returns: an instance of ColumnwiseOperatorArg.
        :rtype: :py:class:`psyclone.domain.lfric.kernel.ColumnwiseOperatorArg`

        '''
        ColumnwiseOperatorArg.check_psyir(psyir, nargs=5)
        datatype = psyir.children[1].children[1].tostr()
        access = psyir.children[1].children[2].tostr()
        function_space1 = psyir.children[1].children[3].tostr()
        function_space2 = psyir.children[1].children[4].tostr()
        return ColumnwiseOperatorArg(
            datatype, access, function_space1, function_space2)
