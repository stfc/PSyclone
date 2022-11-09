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

'''Module containing the InterGridArgMetadata class which captures the metadata
associated with an intergrid argument. Supports the creation, modification
and Fortran output of an intergrid argument.

'''
from fparser.two import Fortran2003

from psyclone.domain.lfric import LFRicConstants
from psyclone.domain.lfric.kernel.field_arg_metadata import FieldArgMetadata


class InterGridArgMetadata(FieldArgMetadata):
    '''Class to capture LFRic kernel metadata information for an intergrid
    argument.

    :param str datatype: the datatype of this InterGrid argument \
        (GH_INTEGER, ...).
    :param str access: the way the kernel accesses this intergrid \
        argument (GH_WRITE, ...).
    :param str function_space: the function space that this \
        InterGrid is on (W0, ...).
    :param str mesh_arg: the type of mesh that this InterGrid arg \
        is on (coarse or fine).

    '''
    # The relative position of LFRic mesh metadata. Metadata for an
    # inter-grid argument is provided in the following format
    # 'arg_type(form, datatype, access, function_space,
    # mesh)'. Therefore, the index of the mesh argument
    # (mesh_arg_index) is 4. Index values not provided here are common
    # to the parent classes and are inherited from them.
    mesh_arg_index = 4

    # The fparser2 class that captures this metadata.
    fparser2_class = Fortran2003.Structure_Constructor

    def __init__(self, datatype, access, function_space, mesh_arg):
        super().__init__(datatype, access, function_space)
        self.mesh_arg = mesh_arg

    @staticmethod
    def create_from_fparser2(fparser2_tree):
        '''Create an instance of this class from an fparser2 tree.

        :param fparser2_tree: fparser2 tree capturing the metadata for \
            an InterGrid argument.
        :type fparser2_tree: \
            :py:class:`fparser.two.Fortran2003.Structure_Constructor`

        :returns: an instance of InterGridArgMetadata.
        :rtype: :py:class:`psyclone.domain.lfric.kernel.InterGridArgMetadata`

        '''
        InterGridArgMetadata.check_fparser2_arg(
            fparser2_tree, "arg_type",
            encoding=Fortran2003.Structure_Constructor)
        InterGridArgMetadata.check_nargs(fparser2_tree, 5)
        InterGridArgMetadata.check_first_arg(fparser2_tree, "InterGrid")
        datatype, access, function_space = \
            InterGridArgMetadata.get_type_access_and_fs(fparser2_tree)
        mesh_arg = InterGridArgMetadata.get_mesh_arg(fparser2_tree)
        InterGridArgMetadata.check_remaining_args(
            fparser2_tree, datatype, access, function_space, mesh_arg)
        return InterGridArgMetadata(datatype, access, function_space, mesh_arg)

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
            children[InterGridArgMetadata.mesh_arg_index].children[0].tostr()
        if not mesh_arg_lhs.lower() == "mesh_arg":
            raise ValueError(
                f"At argument index {InterGridArgMetadata.mesh_arg_index} for "
                f"metadata '{fparser2_tree}' expected the left hand side "
                f"to be MESH_ARG but found '{mesh_arg_lhs}'.")
        mesh_arg = fparser2_tree.children[1].\
            children[InterGridArgMetadata.mesh_arg_index].children[1].tostr()
        return mesh_arg

    def fortran_string(self):
        '''
        :returns: the metadata represented by this class as Fortran.
        :rtype: str
        '''
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
        '''
        const = LFRicConstants()
        InterGridArgMetadata.validate_scalar_value(
            value, const.VALID_MESH_TYPES, "mesh_arg")
        self._mesh_arg = value.lower()
