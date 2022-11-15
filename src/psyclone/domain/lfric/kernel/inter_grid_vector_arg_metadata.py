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

'''Module containing the InterGridVectorArgMetadata class which captures the
metadata associated with a intergrid vector argument. Supports the
creation, modification and Fortran output of an InterGridVector
argument.

'''
from fparser.two import Fortran2003
from psyclone.domain.lfric.kernel.inter_grid_arg_metadata import \
    InterGridArgMetadata


class InterGridVectorArgMetadata(InterGridArgMetadata):
    '''Class to capture LFRic kernel metadata information for an
    InterGridVector argument.

    :param str datatype: the datatype of this InterGridVector argument \
        (GH_INTEGER, ...).
    :param str access: the way the kernel accesses this InterGridVector \
        argument (GH_WRITE, ...).
    :param str function_space: the function space that this \
        InterGridVector argument is on (W0, ...).
    :param str vector_length: the size of the vector.

    '''
    # The relative position of LFRic vector length metadata. Metadata
    # for an inter-grid vector argument is provided in the following
    # format 'arg_type(form*vector_length, datatype, access,
    # function_space, mesh)'. Therefore, the index of the
    # vector_length argument (vector_length_arg_index) is 0. Index
    # values not provided here are common to the parent classes and
    # are inherited from them.
    vector_length_arg_index = 0

    def __init__(self, datatype, access, function_space, mesh_arg,
                 vector_length):
        super().__init__(datatype, access, function_space, mesh_arg)
        self.vector_length = vector_length

    @staticmethod
    def create_from_fparser2(fparser2_tree):
        '''Create an instance of this class from an fparser2 tree.

        :param fparser2_tree: fparser2 tree capturing the metadata for \
            an InterGrid vector argument.
        :type fparser2_tree: :py:class:`fparser.two.Fortran2003.Part_Ref`

        :returns: an instance of InterGridVectorArgMetadata.
        :rtype: :py:class:`psyclone.domain.lfric.kernel.InterGridArg`

        '''
        InterGridVectorArgMetadata.check_fparser2_arg(
            fparser2_tree, "arg_type",
            encoding=Fortran2003.Structure_Constructor)
        InterGridVectorArgMetadata.check_nargs(fparser2_tree, 5)
        InterGridVectorArgMetadata.check_first_arg(
            fparser2_tree, "inter-grid-vector", vector=True)
        vector_length = InterGridVectorArgMetadata.get_vector_length(
            fparser2_tree)
        datatype, access, function_space = \
            InterGridVectorArgMetadata.get_type_access_and_fs(fparser2_tree)
        mesh_arg = InterGridVectorArgMetadata.get_mesh_arg(fparser2_tree)
        InterGridVectorArgMetadata.check_remaining_args(
            fparser2_tree, datatype, access, function_space, mesh_arg,
            vector_length)
        return InterGridVectorArgMetadata(
            datatype, access, function_space, mesh_arg, vector_length)

    def fortran_string(self):
        '''
        :returns: the metadata represented by this class as Fortran.
        :rtype: str
        '''
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
        :raises ValueError: if the provided value is not a string \
            containing an integer.
        :raises ValueError: if the provided value is not greater than 1.

        '''
        if not isinstance(value, str):
            raise TypeError(f"The vector size should be a string but found "
                            f"{type(value).__name__}.")
        try:
            int_value = int(value)
        except ValueError as info:
            raise ValueError(
                f"The vector size should be a string containing an integer, "
                f"but found '{value}'.") from info

        if int_value <= 1:
            raise ValueError(f"The vector size should be an integer greater "
                             f"than 1 but found {value}.")
        self._vector_length = value


__all__ = ["InterGridVectorArgMetadata"]
