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

'''Module containing the ScalarArg class which captures the metadata
associated with a scalar argument. Supports the creation, modification
and Fortran output of a Scalar argument.

'''
from psyclone.domain.lfric import LFRicConstants
from psyclone.domain.lfric.kernel.common_arg import CommonArg


class ScalarArg(CommonArg):
    '''Class to capture LFRic kernel metadata information for a scalar
    argument.

    :param Optional[str] datatype: the datatype of this scalar \
        (GH_INTEGER, ...).
    :param Optional[str] access: the way the kernel accesses this \
        scalar (GH_WRITE, ...).

    '''
    form = "GH_SCALAR"

    @staticmethod
    def create_from_fparser2(fparser2_tree):
        '''Create an instance of this class from an fparser2 tree.

        :param fparser2_tree: fparser2 tree containing the metadata \
            for a scalar argument.
        :type fparser2_tree: :py:class:`fparser.two.Fortran2003.Part_Ref`

        :returns: an instance of ScalarArg.
        :rtype: :py:class:`psyclone.domain.lfric.kernel.ScalarArg`

        '''
        datatype_arg_index = 1
        access_arg_index = 2
        ScalarArg.check_fparser2(fparser2_tree, nargs=3)
        datatype, access = ScalarArg.get_type_and_access(
            fparser2_tree, datatype_arg_index, access_arg_index)
        return ScalarArg(datatype, access)

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
        '''
        :returns: the metadata represented by this class as Fortran.
        :rtype: str

        :raises ValueError: if one or more of the datatype or access \
            values have not been set.

        '''
        if not (self.datatype and self.access):
            raise ValueError(
                f"Values for datatype and access must be "
                f"provided before calling the fortran_string method, but "
                f"found '{self.datatype}' and '{self.access}', respectively.")

        return f"arg_type({self.form}, {self.datatype}, {self.access})"

    @property
    def datatype(self):
        '''
        :returns: the datatype for this scalar argument.
        :rtype: str
        '''
        return self._datatype

    @staticmethod
    def check_datatype(value):
        '''
        :param str value: the datatype to check for validity.

        :raises ValueError: if the provided value is not a valid \
            datatype descriptor.

        '''
        const = LFRicConstants()
        if not value or value.lower() not in const.VALID_SCALAR_DATA_TYPES:
            raise ValueError(
                f"The datatype descriptor metadata for a scalar should be one "
                f"of {const.VALID_SCALAR_DATA_TYPES}, but found '{value}'.")

    @datatype.setter
    def datatype(self, value):
        '''
        :param str value: set the datatype to the \
            specified value.
        '''
        self.check_datatype(value)
        self._datatype = value

    @property
    def access(self):
        '''
        :returns: the access descriptor for this scalar \
            argument.
        :rtype: str
        '''
        return self._access

    @staticmethod
    def check_access(value):
        '''
        :param str value: the access descriptor to validate.

        :raises ValueError: if the provided value is not a valid \
            access type.
        '''

        const = LFRicConstants()
        if not value or value.lower() not in const.VALID_SCALAR_ACCESS_TYPES:
            raise ValueError(
                f"The access descriptor metadata for a scalar should be one "
                f"of {const.VALID_SCALAR_ACCESS_TYPES}, but found '{value}'.")

    @access.setter
    def access(self, value):
        '''
        :param str value: set the access descriptor to the \
            specified value.

        '''
        self.check_access(value)
        self._access = value
