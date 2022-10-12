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

'''Module containing the InterGridArg class which captures the metadata
associated with an intergrid argument. Supports the creation, modification
and Fortran output of an intergrid argument.

'''
from fparser.two import Fortran2003

from psyclone.domain.lfric import LFRicConstants
from psyclone.domain.lfric.kernel.field_arg import FieldArg


class InterGridArg(FieldArg):
    '''Class to capture LFRic kernel metadata information for an intergrid
    argument.

    :param Optional[str] datatype: the datatype of this InterGrid \
        argument (GH_INTEGER, ...).
    :param Optional[str] access: the way the kernel accesses this \
        intergrid argument (GH_WRITE, ...).
    :param Optional[str] function_space: the function space that this \
        InterGrid is on (W0, ...).
    :param Optional[str] mesh_arg: the type of mesh that this \
        InterGrid arg is on (coarse or fine).

    '''
    # The relative position of LFRic mesh metadata. Metadata for an
    # inter-grid argument is provided in the following format
    # 'arg_type(form, datatype, access, function_space,
    # mesh)'. Therefore, the index of the mesh argument
    # (mesh_arg_index) is 4. Index values not provided here are common
    # to the parent classes and are inherited from them.
    mesh_arg_index = 4

    def __init__(self, datatype=None, access=None, function_space=None,
                 mesh_arg=None):
        super().__init__(datatype, access, function_space)
        if mesh_arg is None:
            self._mesh_arg = mesh_arg
        else:
            self.mesh_arg = mesh_arg

    @staticmethod
    def create_from_fparser2(fparser2_tree):
        '''Create an instance of this class from an fparser2 tree.

        :param fparser2_tree: fparser2 tree capturing the metadata for \
            an InterGrid argument.
        :type fparser2_tree: \
            :py:class:`fparser.two.Fortran2003.Structure_Constructor`

        :returns: an instance of InterGridArg.
        :rtype: :py:class:`psyclone.domain.lfric.kernel.InterGridArg`

        '''
        InterGridArg.check_fparser2(
            fparser2_tree, "arg_type",
            encoding=Fortran2003.Structure_Constructor)
        InterGridArg.check_nargs(fparser2_tree, nargs=5)
        InterGridArg.check_first_arg(fparser2_tree, "InterGrid")
        datatype, access, function_space = \
            InterGridArg.get_type_access_and_fs(fparser2_tree)
        mesh_arg = InterGridArg.get_mesh_arg(fparser2_tree)
        InterGridArg.check_remaining_args(
            fparser2_tree, datatype, access, function_space, mesh_arg)
        return InterGridArg(datatype, access, function_space, mesh_arg)

    @staticmethod
    def get_mesh_arg(fparser2_tree):
        '''Retrieves the mesh_arg metadata value from the supplied fparser2
        tree.

        :param fparser2_tree: fparser2 tree capturing the metadata for \
            an InterGrid argument.
        :type fparser2_tree: \
            :py:class:`fparser.two.Fortran2003.Structure_Constructor`

        :returns: the metadata mesh value extracted from the fparser2 tree.
        :rtype: str

        raises ValueError: if the lhs of the assignment "mesh_arg = \
            value" is not "mesh_arg".

        '''
        mesh_arg_lhs = fparser2_tree.children[1].\
            children[InterGridArg.mesh_arg_index].children[0].tostr()
        if not mesh_arg_lhs.lower() == "mesh_arg":
            raise ValueError(
                f"At argument index {InterGridArg.mesh_arg_index} for "
                f"metadata '{fparser2_tree}' expected the left hand side "
                f"to be MESH_ARG but found '{mesh_arg_lhs}'.")
        mesh_arg = fparser2_tree.children[1].\
            children[InterGridArg.mesh_arg_index].children[1].tostr()
        return mesh_arg

    @classmethod
    def create_from_fortran_string(cls, fortran_string):
        '''Create an instance of this class from Fortran.

        :param str fortran_string: a string containing the metadata in \
            Fortran.

        :returns: an instance of cls.
        :rtype: subclass of :py:class:`psyclone.domain.lfric.kernel.common_arg`

        '''
        fparser2_tree = cls.create_fparser2(
            fortran_string, encoding=Fortran2003.Structure_Constructor)
        return cls.create_from_fparser2(fparser2_tree)

    def fortran_string(self):
        '''
        :returns: the metadata represented by this class as Fortran.
        :rtype: str

        :raises ValueError: if one or more of the datatype, access, \
            function_space or mesh_arg values have not been set.

        '''
        if not (self.datatype and self.access and self.function_space and
                self.mesh_arg):
            raise ValueError(
                f"Values for datatype, access, function_space and mesh_arg "
                f"must be provided before calling the fortran_string method, "
                f"but found '{self.datatype}', '{self.access}', "
                f"'{self.function_space}' and '{self.mesh_arg}', "
                f"respectively.")

        return (f"arg_type({self.form}, {self.datatype}, {self.access}, "
                f"{self.function_space}, mesh_arg={self.mesh_arg})")

    @property
    def mesh_arg(self):
        '''

        :returns: the mesh type for this intergrid argument.
        :rtype: str

        '''
        return self._mesh_arg

    @mesh_arg.setter
    def mesh_arg(self, value):
        '''
        :param str value: set the mesh type to the \
            specified value.

        raises ValueError: if the provided value is not a valid \
            mesh_argument type (gh_coarse or gh_fine).

        '''
        const = LFRicConstants()
        if not value or value.lower() not in const.VALID_MESH_TYPES:
            raise ValueError(
                f"The mesh_arg metadata for a mesh should be one of "
                f"{const.VALID_MESH_TYPES}, but found '{value}'.")
        self._mesh_arg = value
