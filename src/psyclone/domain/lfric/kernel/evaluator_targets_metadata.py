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
