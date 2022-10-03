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

'''Module containing the InterGridVectorArg class which captures the
metadata associated with a intergrid vector argument. Supports the
creation, modification and Fortran output of an InterGridVector
argument.

'''
from fparser.two import Fortran2003
from psyclone.domain.lfric.kernel.inter_grid_arg import InterGridArg


class InterGridVectorArg(InterGridArg):
    '''Class to capture LFRic kernel metadata information for an
    InterGridVector argument.

    :param Optional[str] datatype: the datatype of this \
        InterGridVector argument (GH_INTEGER, ...).
    :param Optional[str] access: the way the kernel accesses this \
        InterGridVector argument (GH_WRITE, ...).
    :param Optional[str] function_space: the function space that this \
        InterGridVector argument is on (W0, ...).
    :param Optional[str] vector_length: the size of the vector.

    '''
    vector_length_arg_index = 0

    def __init__(self, datatype=None, access=None, function_space=None,
                 mesh_arg=None, vector_length=None):
        super().__init__(datatype, access, function_space, mesh_arg)

        if vector_length is None:
            self._vector_length = vector_length
        else:
            self.vector_length = vector_length

    @staticmethod
    def create_from_fparser2(fparser2_tree):
        '''Create an instance of this class from an fparser2 tree.

        :param fparser2_tree: fparser2 tree capturing the metadata for \
            an InterGrid vector argument.
        :type fparser2_tree: :py:class:`fparser.two.Fortran2003.Part_Ref`

        :returns: an instance of InterGridVectorArg.
        :rtype: :py:class:`psyclone.domain.lfric.kernel.InterGridArg`

        raises ValueError: if the metadata is not in the correct form.

        '''
        InterGridVectorArg.check_fparser2(
            fparser2_tree, nargs=5, encoding=Fortran2003.Structure_Constructor)
        InterGridArg.check_first_arg(
            fparser2_tree, "InterGridVector", vector=True)
        vector_length = InterGridVectorArg.get_and_check_vector_length(
            fparser2_tree, InterGridVectorArg.vector_length_arg_index)
        datatype, access, function_space = \
            InterGridVectorArg.get_type_access_and_fs(
                fparser2_tree, InterGridVectorArg.datatype_arg_index,
                InterGridVectorArg.access_arg_index,
                InterGridVectorArg.function_space_arg_index)
        mesh_arg = InterGridVectorArg.get_mesh_arg(fparser2_tree)
        InterGridVectorArg.check_remaining_args(
            fparser2_tree, datatype, access, function_space, mesh_arg,
            vector_length)
        return InterGridVectorArg(
            datatype, access, function_space, mesh_arg, vector_length)

    def fortran_string(self):
        '''
        :returns: the metadata represented by this class as Fortran.
        :rtype: str

        :raises ValueError: if one or more of the datatype, access, \
            function_space, mesh_arg or vecgtor_length values have not \
            been set.

        '''
        if not (self.datatype and self.access and self.function_space and
                self.mesh_arg and self.vector_length):
            raise ValueError(
                f"Values for datatype, access, function_space, mesh_arg "
                f"and vector_length must be provided before calling the "
                f"fortran_string method, but found '{self.datatype}', "
                f"'{self.access}', '{self.function_space}', "
                f"'{self.mesh_arg}' and '{self.vector_length}', respectively.")

        return (f"arg_type({self.form}*{self.vector_length}, "
                f"{self.datatype}, {self.access}, {self.function_space}, "
                f"mesh_arg={self.mesh_arg})")

    @property
    def vector_length(self):
        '''
        :returns: the vector length of this intergrid vector \
            argument.
        :rtype: str
        '''
        return self._vector_length

    @vector_length.setter
    def vector_length(self, value):
        '''

        :param str value: set the intergrid vector length to the specified \
            value.

        :raises TypeError: if the provided value is not of type str.
        :raises ValueError: if the provided value is not greater than 1.

        '''
        if not isinstance(value, str):
            raise TypeError(f"The vector size should be a string but found "
                            f"{type(value).__name__}.")
        int_value = int(value)
        if int_value <= 1:
            raise ValueError(f"The vector size should be an integer greater "
                             f"than 1 but found {value}.")
        self._vector_length = value
