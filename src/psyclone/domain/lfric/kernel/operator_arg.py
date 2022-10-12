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

'''Module containing the OperatorArg class which captures the metadata
associated with an operator argument. Supports the creation, modification
and Fortran output of a Operator argument.

'''
from psyclone.domain.lfric import LFRicConstants
from psyclone.domain.lfric.kernel.scalar_arg import ScalarArg


class OperatorArg(ScalarArg):
    '''Class to capture LFRic kernel metadata information for an operator
    argument.

    :param Optional[str] datatype: the datatype of this operator \
        (GH_INTEGER, ...).
    :param Optional[str] access: the way the kernel accesses this \
        operator (GH_WRITE, ...).
    :param Optional[str] function_space_to: the function space that \
        this operator maps to (W0, ...).
    :param Optional[str] function_space_from: the function space that \
        this operator maps from (W0, ...).

    '''
    # The name used to specify an operator argument in LFRic metadata.
    form = "GH_OPERATOR"
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

    def __init__(self, datatype=None, access=None, function_space_to=None,
                 function_space_from=None):
        super().__init__(datatype, access)
        if function_space_to is None:
            self._function_space_to = function_space_to
        else:
            self.function_space_to = function_space_to
        if function_space_from is None:
            self._function_space_from = function_space_from
        else:
            self.function_space_from = function_space_from

    @staticmethod
    def create_from_fparser2(fparser2_tree):
        '''Create an instance of this class from an fparser2 tree.

        :param fparser2_tree: fparser2 tree capturing the metadata for \
            an operator argument.
        :type fparser2_tree: :py:class:`fparser.two.Fortran2003.Part_Ref`

        :returns: an instance of OperatorArg.
        :rtype: :py:class:`psyclone.domain.lfric.kernel.OperatorArg`

        '''
        OperatorArg.check_fparser2(fparser2_tree, "arg_type")
        OperatorArg.check_nargs(fparser2_tree, nargs=5)
        OperatorArg.check_first_arg(fparser2_tree, "Operator")
        datatype, access = OperatorArg.get_type_and_access(fparser2_tree)
        function_space_to = OperatorArg.get_arg(
            fparser2_tree, OperatorArg.function_space_to_arg_index)
        function_space_from = OperatorArg.get_arg(
            fparser2_tree, OperatorArg.function_space_from_arg_index)
        OperatorArg.check_remaining_args(
            fparser2_tree, datatype, access, function_space_to,
            function_space_from)
        return OperatorArg(
            datatype, access, function_space_to, function_space_from)

    @classmethod
    def create_from_fortran_string(cls, fortran_string):
        '''Create an instance of this class from Fortran.

        :param str fortran_string: a string containing the metadata in \
            Fortran.

        :returns: an instance of cls.
        :rtype: subclass of :py:class:`psyclone.domain.lfric.kernel.common_arg`

        '''
        fparser2_tree = cls.create_fparser2(fortran_string)
        return cls.create_from_fparser2(fparser2_tree)

    def fortran_string(self):
        ''':returns: the metadata represented by this class as Fortran.
        :rtype: str

        :raises ValueError: if one or more of the datatype, access, \
            function_space_to or function_space_from values have not \
            been set.

        '''
        if not (self.datatype and self.access and self.function_space_to and
                self.function_space_from):
            raise ValueError(
                f"Values for datatype, access, function_space_to and "
                f"function_space_from must be provided before calling the "
                f"fortran_string method, but found '{self.datatype}', "
                f"'{self.access}', '{self.function_space_to}' and "
                f"'{self.function_space_from}', respectively.")

        return (f"arg_type({self.form}, {self.datatype}, {self.access}, "
                f"{self.function_space_to}, {self.function_space_from})")

    @staticmethod
    def check_access(value):
        '''
        :param str value: the access descriptor to validate.

        :raises ValueError: if the provided value is not a valid \
            access type.
        '''

        const = LFRicConstants()
        if not value or value.lower() not in const.VALID_OPERATOR_ACCESS_TYPES:
            raise ValueError(
                f"The access descriptor metadata for an operator should be "
                f"one of {const.VALID_OPERATOR_ACCESS_TYPES}, but found "
                f"'{value}'.")

    @staticmethod
    def check_datatype(value):
        '''
        :param str value: the datatype to check for validity.

        :raises ValueError: if the provided value is not a valid \
            datatype descriptor.

        '''
        const = LFRicConstants()
        if not value or value.lower() not in const.VALID_OPERATOR_DATA_TYPES:
            raise ValueError(
                f"The datatype descriptor metadata for an operator should be "
                f"one of {const.VALID_OPERATOR_DATA_TYPES}, but found "
                f"'{value}'.")

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
        if not value or value.lower() not in const.VALID_FUNCTION_SPACES:
            raise ValueError(
                f"The function_space_to metadata for an operator should be "
                f"one of {const.VALID_FUNCTION_SPACES}, but found '{value}'.")
        self._function_space_to = value

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

        raises ValueError: if the provided value is not a valid \
            function space.

        '''
        const = LFRicConstants()
        if not value or value.lower() not in const.VALID_FUNCTION_SPACES:
            raise ValueError(
                f"The function_space_from metadata for an operator should be "
                f"one of {const.VALID_FUNCTION_SPACES}, but found '{value}'.")
        self._function_space_from = value
