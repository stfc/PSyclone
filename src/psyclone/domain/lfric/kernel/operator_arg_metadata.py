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

'''Module containing the OperatorArgMetadata class which captures the metadata
associated with an operator argument. Supports the creation, modification
and Fortran output of a Operator argument.

'''
from psyclone.domain.lfric import LFRicConstants
from psyclone.domain.lfric.kernel.scalar_arg_metadata import ScalarArgMetadata


class OperatorArgMetadata(ScalarArgMetadata):
    '''Class to capture LFRic kernel metadata information for an operator
    argument.

    :param str datatype: the datatype of this operator (GH_INTEGER, ...).
    :param str access: the way the kernel accesses this operator \
        (GH_WRITE, ...).
    :param str function_space_to: the function space that this operator \
        maps to (W0, ...).
    :param str function_space_from: the function space that this \
        operator maps from (W0, ...).

    '''
    # The name used to specify an operator argument in LFRic metadata.
    form = "gh_operator"
    # The relative positions of LFRic function-space-to and
    # function-space-from metadata. Metadata for an operator argument
    # is provided in the following format 'arg_type(form, datatype,
    # access, function_space_to, function_space_from)'. Therefore, for
    # example, the index of the function_space_to argument
    # (function_space_to_arg_index) is 3. Index values not provided
    # here are common to the parent classes and are inherited from
    # them.
    function_space_to_arg_index = 3
    function_space_from_arg_index = 4
    # The name to use for any exceptions.
    check_name = "operator"
    # The number of arguments in the language-level metadata.
    nargs = 5

    def __init__(self, datatype, access, function_space_to,
                 function_space_from):
        super().__init__(datatype, access)
        self.function_space_to = function_space_to
        self.function_space_from = function_space_from

    @classmethod
    def _get_metadata(cls, fparser2_tree):
        '''Extract the required metadata from the fparser2 tree and return it
        as strings. Also check that the metadata is in the expected
        form (but do not check the metadata values as that is done
        separately).

        :param fparser2_tree: fparser2 tree containing the metadata \
            for this argument.
        :type fparser2_tree: :py:class:`fparser.two.Fortran2003.Part_Ref`

        :returns: a tuple containing the datatype, access \
            function_space_to and function_space_from metadata.
        :rtype: Tuple[str, str, str, str]

        '''
        datatype, access = super()._get_metadata(fparser2_tree)
        function_space_to = cls.get_arg(
            fparser2_tree, cls.function_space_to_arg_index)
        function_space_from = cls.get_arg(
            fparser2_tree, cls.function_space_from_arg_index)
        return (datatype, access, function_space_to, function_space_from)

    def fortran_string(self):
        '''
        :returns: the metadata represented by this class as Fortran.
        :rtype: str
        '''
        return (f"arg_type({self.form}, {self.datatype}, {self.access}, "
                f"{self.function_space_to}, {self.function_space_from})")

    @staticmethod
    def check_datatype(value):
        '''
        :param str value: the datatype to check for validity.
        '''
        const = LFRicConstants()
        OperatorArgMetadata.validate_scalar_value(
            value, const.VALID_OPERATOR_DATA_TYPES, "datatype descriptor")

    @staticmethod
    def check_access(value):
        '''
        :param str value: the access descriptor to validate.
        '''
        const = LFRicConstants()
        OperatorArgMetadata.validate_scalar_value(
            value, const.VALID_OPERATOR_ACCESS_TYPES, "access descriptor")

    @property
    def function_space_to(self):
        '''
        :returns: the first function space for this operator \
            argument (that this operator maps to).
        :rtype: str
        '''
        return self._function_space_to

    @function_space_to.setter
    def function_space_to(self, value):
        '''
        :param str value: set the function space to the \
            specified value.

        raises ValueError: if the provided value is not a valid \
            function space.

        '''
        const = LFRicConstants()
        self.validate_scalar_value(
            value, const.VALID_FUNCTION_SPACE_NAMES, "function_space_to")
        self._function_space_to = value.lower()

    @property
    def function_space_from(self):
        '''
        :returns: the second function space for this operator \
            argument (that this operator maps from).
        :rtype: str
        '''
        return self._function_space_from

    @function_space_from.setter
    def function_space_from(self, value):
        '''
        :param str value: set the function space to the \
            specified value.
        '''
        const = LFRicConstants()
        self.validate_scalar_value(
            value, const.VALID_FUNCTION_SPACE_NAMES, "function_space_from")
        self._function_space_from = value.lower()


__all__ = ["OperatorArgMetadata"]
