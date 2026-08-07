# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2022-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

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
