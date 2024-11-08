# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2022-2024, Science and Technology Facilities Council
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

'''Module containing the OperatesOnMetadata class which captures
the values for the LFRic kernel OPERATES_ON metadata.

'''
from psyclone.domain.lfric import LFRicConstants
from psyclone.domain.lfric.kernel.common_declaration_metadata import \
    CommonDeclarationMetadata


class OperatesOnMetadata(CommonDeclarationMetadata):
    '''Class to capture the values of the LFRic kernel
    OPERATES_ON metadata. This class supports the creation,
    modification and Fortran output of this metadata.

    OPERATES_ON metadata specifies that the Kernel has been written to
    expect data in the specified form, i.e. 'cell_column' means a column
    of cells and 'domain' means all cells.

    :param str operates_on: the value of operates_on.

    '''
    def __init__(self, operates_on):
        super().__init__()
        self.operates_on = operates_on

    def fortran_string(self):
        '''
         :returns: the operates_on metadata as Fortran.
         :rtype: str
        '''
        return OperatesOnMetadata.scalar_declaration_string(
            "INTEGER", "OPERATES_ON", self._operates_on)

    @staticmethod
    def create_from_fparser2(fparser2_tree):
        '''Create an instance of OperatesOnMetadata from an fparser2
        tree.

        :param fparser2_tree: fparser2 tree capturing the operates_on \
            metadata.
        :type fparser2_tree: :py:class:`fparser.two.Fortran2003.\
            Data_Component_Def_Stmt`

        LFRic operates_on metadata is in scalar form:

        integer :: operates_on = cell_column

        :returns: an instance of OperatesOnMetadata.
        :rtype: :py:class:`psyclone.domain.lfric.kernel.\
            OperatesOnMetadata`

        '''
        const = LFRicConstants()
        valid_values = const.USER_KERNEL_ITERATION_SPACES
        value = OperatesOnMetadata.get_intrinsic_scalar_declaration(
            fparser2_tree, "INTEGER", "OPERATES_ON", valid_values)
        return OperatesOnMetadata(value)

    @property
    def operates_on(self):
        '''
        :returns: the operates_on value.
        :rtype: str
        '''
        return self._operates_on

    @operates_on.setter
    def operates_on(self, value):
        '''
        :param str value: sets the operates_on metadata to the \
            supplied value.
        '''
        const = LFRicConstants()
        OperatesOnMetadata.validate_scalar_value(
            value, const.VALID_ITERATION_SPACES, "OPERATES_ON")
        self._operates_on = value.lower()


__all__ = ["OperatesOnMetadata"]
