# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2022-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

'''Module containing the EvaluatorTargetsMetadata class which captures
the values for the LFRic kernel GH_EVALUATOR_TARGETS metadata.

'''
from psyclone.domain.lfric import LFRicConstants
from psyclone.domain.lfric.kernel.common_declaration_metadata import \
    CommonDeclarationMetadata


class EvaluatorTargetsMetadata(CommonDeclarationMetadata):
    '''Class to capture the values of the LFRic kernel
    GH_EVALUATOR_TARGETS metadata.  This class supports the creation,
    modification and Fortran output of this metadata.

    if an evaluator is required for multiple function spaces then
    this is specified using the gh_evaluator_targets
    metadata.

    :param evaluator_targets: a list of function-space names.
    :type evaluator_targets: List[str]

    '''
    def __init__(self, evaluator_targets):
        super().__init__()
        self.evaluator_targets = evaluator_targets

    def fortran_string(self):
        '''
         :returns: the evaluator_targets metadata as Fortran.
         :rtype: str
        '''
        return EvaluatorTargetsMetadata.array_declaration_string(
            "INTEGER", "GH_EVALUATOR_TARGETS", self._evaluator_targets)

    @staticmethod
    def create_from_fparser2(fparser2_tree):
        '''Create an instance of EvaluatorTargetsMetadata from an fparser2
        tree.

        LFRic evaluator targets metadata is in array form. Two
        versions of the array form are supported:

        integer :: gh_evaluator_targets(2) = (/ w0, w1 /)
        integer, dimension(2) :: gh_shape = (/ w0, w1 /)

        :param fparser2_tree: fparser2 tree capturing the evaluator \
            targets metadata.
        :type fparser2_tree: :py:class:`fparser.two.Fortran2003.\
            Data_Component_Def_Stmt`

        :returns: an instance of EvaluatorTargetsMetadata.
        :rtype: :py:class:`psyclone.domain.lfric.kernel.\
            EvaluatorTargetsMetadata`

        '''
        const = LFRicConstants()
        valid_values = const.VALID_FUNCTION_SPACES
        values_list = EvaluatorTargetsMetadata.\
            get_intrinsic_array_declaration(
                fparser2_tree, "INTEGER", "GH_EVALUATOR_TARGETS", valid_values)
        return EvaluatorTargetsMetadata(values_list)

    @property
    def evaluator_targets(self):
        '''
        :returns: a list of evaluator targets values (names of \
            function spaces).
        :rtype: List[str]
        '''
        return self._evaluator_targets[:]

    @evaluator_targets.setter
    def evaluator_targets(self, values):
        '''
        :param values: set the evaluator_targets metadata to the \
            supplied list of values.
        :type values: List[str]
        '''
        const = LFRicConstants()
        EvaluatorTargetsMetadata.validate_list(values, str)
        for value in values:
            EvaluatorTargetsMetadata.validate_scalar_value(
                value, const.VALID_FUNCTION_SPACES, "evaluator_targets")
        # Take a copy of the list so that it can't be modified
        # externally. Also make all values lower case.
        self._evaluator_targets = [value.lower() for value in values]


__all__ = ["EvaluatorTargetsMetadata"]
