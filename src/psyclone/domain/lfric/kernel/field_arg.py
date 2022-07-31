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

'''Module containing the FieldArg class which captures the metadata
associated with a field argument. Supports the creation, modification
and Fortran output of a Field argument.

'''
from fparser.common.readfortran import FortranStringReader
from fparser.two import Fortran2003
from fparser.two.parser import ParserFactory

from psyclone.domain.lfric import LFRicConstants


class FieldArg():
    '''Class to capture LFRic kernel metadata information for a field
    argument.

    :param Optional[str] datatype: the datatype of this field \
        (GH_INTEGER, ...).
    :param Optional[str] access: the way the kernel accesses this \
        field (GH_WRITE, ...).
    :param Optional[str] function_space: the function space that this \
        field is on (W0, ...).

    '''
    def __init__(self, datatype=None, access=None, function_space=None):
        self._form = "GH_FIELD"
        if datatype is None:
            self._datatype = datatype
        else:
            self.datatype = datatype
        if access is None:
            self._access = access
        else:
            self.access = access
        if function_space is None:
            self._function_space = function_space
        else:
            self.function_space = function_space

    @staticmethod
    def create_part_ref(fortran_string):
        '''Creates an fparser tree from a Fortran string. Expects the parent
        node of the tree to be a Part_Ref object (as it represents a field
        argument).

        :param str fortran_string: a string containing the metadata in \
           Fortran.

        :returns: an fparser2 tree containing the PSyIR for a field \
            argument.
        :rtype: :py:class:`fparser.two.Fortran2003.Part_Ref`

        :raises ValueError: if the Fortran string is not in the \
            expected form.

        '''
        _ = ParserFactory().create(std="f2003")
        reader = FortranStringReader(fortran_string)
        part_ref = Fortran2003.Part_Ref(reader)
        if not part_ref:
            raise ValueError(
                f"Expected kernel metadata to be a Fortran part reference, "
                f"with the form 'arg_type(...)' but found '{fortran_string}'.")
        return part_ref

    @staticmethod
    def create_from_fortran_string(fortran_string):
        '''Create an instance of this class from a Fortran string.

        :param str fortran_string: a string containing the metadata in \
            Fortran.

        :returns: an instance of FieldArg.
        :rtype: :py:class:`psyclone.domain.lfric.kernel.FieldArg`

        '''
        part_ref = FieldArg.create_part_ref(fortran_string)
        return FieldArg.create_from_psyir(part_ref)

    @staticmethod
    def check_psyir(psyir):
        '''Checks that the psyir argument is valid.

        :param psyir: fparser2 tree containing the PSyIR for a field \
            argument.
        :type psyir: :py:class:`fparser.two.Fortran2003.Part_Ref`

        :raises TypeError: if the psyir argument is not an fparser2 \
            Part_Ref object.
        :raises ValueError: if the field arg kernel metadata is not in \
            the form arg_type(...).
        :raises ValueError: if the field arg kernel metadata does not \
            contain 4 arguments.

        '''
        if not isinstance(psyir, Fortran2003.Part_Ref):
            raise TypeError(
                f"Expected field arg kernel metadata to be encoded as a "
                f"Fortran Part_Ref object but found type "
                f"'{type(psyir).__name__}' with value '{str(psyir)}'.")
        if not psyir.children[0].tostr().lower() == "arg_type":
            raise ValueError(
                f"Expected field arg kernel metadata to have the name "
                f"'arg_type' and be in the form 'arg_type(...)', but found "
                f"'{str(psyir)}'.")
        if not len(psyir.children[1].children) == 4:
            raise ValueError(
                f"Expected field arg kernel metadata to have 4 arguments, "
                f"but found {len(psyir.children[1].children)} in "
                f"'{str(psyir)}'.")

    @staticmethod
    def create_from_psyir(psyir):
        '''Create an instance of this class from generic PSyIR. At this moment
        this information is captured in an fparser2 tree.

        :param psyir: fparser2 tree containing the PSyIR for a field \
            argument.
        :type psyir: :py:class:`fparser.two.Fortran2003.Part_Ref`

        :returns: an instance of FieldArg.
        :rtype: :py:class:`psyclone.domain.lfric.kernel.FieldArg`

        '''
        FieldArg.check_psyir(psyir)
        datatype = psyir.children[1].children[1].tostr()
        access = psyir.children[1].children[2].tostr()
        function_space = psyir.children[1].children[3].tostr()
        return FieldArg(datatype, access, function_space)

    def fortran_string(self):
        '''
        :returns: the metadata represented by this class as a \
            Fortran string.
        :rtype: str

        raises ValueError: if one or more of the datatype, access or \
            function_space values have not been set.

        '''
        if not (self.datatype and self.access and self.function_space):
            raise ValueError(
                f"Values for datatype, access and function_space must be "
                f"provided before calling the fortran_string method, but "
                f"found '{self.datatype}', '{self.access}' and "
                f"'{self.function_space}'.")

        return (f"arg_type({self.form}, {self.datatype}, {self.access}, "
                f"{self.function_space})")

    @property
    def form(self):
        '''
        :returns: the form descriptor for this field \
            argument. This is always GH_FIELD.
        :rtype: str
        '''
        return self._form

    @property
    def datatype(self):
        '''
        :returns: the datatype for this field \
            argument.
        :rtype: str
        '''
        return self._datatype

    @datatype.setter
    def datatype(self, value):
        '''
        :param str value: set the datatype to the \
            specified value.

        :raises ValueError: if the provided value is not a valid \
            datatype descriptor.

        '''
        const = LFRicConstants()
        if not value or value.lower() not in const.VALID_FIELD_DATA_TYPES:
            raise ValueError(
                f"The second metadata entry for a field argument should "
                f"be a recognised datatype descriptor (one of "
                f"{const.VALID_FIELD_DATA_TYPES}), but found '{value}'.")
        self._datatype = value

    @property
    def access(self):
        '''
        :returns: the access descriptor for this field \
            argument.
        :rtype: str
        '''
        return self._access

    @access.setter
    def access(self, value):
        '''
        :param str value: set the access descriptor to the \
            specified value.

        :raises ValueError: if the provided value is not a valid \
            access type.

        '''
        const = LFRicConstants()
        if not value or value.lower() not in const.VALID_ACCESS_TYPES:
            raise ValueError(
                f"The third metadata entry for a field argument should "
                f"be a recognised datatype descriptor (one of "
                f"{const.VALID_ACCESS_TYPES}), but found '{value}'.")
        self._access = value

    @property
    def function_space(self):
        '''
        :returns: the function space for this field \
            argument.
        :rtype: str
        '''
        return self._function_space

    @function_space.setter
    def function_space(self, value):
        '''
        :param str value: set the access descriptor to the \
            specified value.

        raises ValueError: if the provided value is not a valid \
            function space.

        '''
        const = LFRicConstants()
        if not value or value.lower() not in const.VALID_FUNCTION_SPACES:
            raise ValueError(
                f"The fourth metadata entry for a field argument should "
                f"be a recognised datatype descriptor (one of "
                f"{const.VALID_FUNCTION_SPACES}), but found '{value}'.")
        self._function_space = value
