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
    :param Optional[str] stencil: the type of stencil used by the \
        kernel when accessing this InterGrid arg.

    '''
    # The relative position of LFRic mesh metadata. Metadata for an
    # inter-grid argument is provided in the following format
    # 'arg_type(form, datatype, access, function_space, [stencil],
    # mesh)'. The stencil argument is optional and its index
    # (stencil_arg_index) is therefore 4 if it exists and the index of
    # the mesh argument is 4 or 5 depending on whether there is a
    # stencil argument. As the mesh argument index value is not known
    # beforehand, it is not set. Fixed index values not provided here
    # are common to the parent classes and are inherited from them.
    stencil_arg_index = 4
    # The name to use for any exceptions.
    check_name = "inter-grid"
    # The number of arguments in the language-level metadata (min and
    # max values).
    nargs = (5, 6)

    # The fparser2 class that captures this metadata.
    fparser2_class = Fortran2003.Structure_Constructor

    def __init__(self, datatype, access, function_space, mesh_arg,
                 stencil=None):
        super().__init__(datatype, access, function_space, stencil=stencil)
        self.mesh_arg = mesh_arg

    @classmethod
    def _get_metadata(cls, fparser2_tree):
        '''Extract the required metadata from the fparser2 tree and return it
        as strings. Also check that the metadata is in the expected
        form (but do not check the metadata values as that is done
        separately).

        :param fparser2_tree: fparser2 tree containing the metadata \
            for this argument.
        :type fparser2_tree: \
            :py:class:`fparser.two.Fortran2003.Structure_Constructor`

        :returns: a tuple containing the datatype, access, function \
            space, mesh and stencil metadata.
        :rtype: Tuple[str, str, str, str, Optional[str]]

        '''
        datatype, access = cls._get_datatype_access_metadata(fparser2_tree)
        function_space = cls.get_arg(
            fparser2_tree, cls.function_space_arg_index)
        try:
            stencil = cls.get_stencil(fparser2_tree)
            mesh_arg = cls.get_mesh_arg(fparser2_tree, 5)
        except TypeError:
            stencil = None
            mesh_arg = cls.get_mesh_arg(fparser2_tree, 4)

        return (datatype, access, function_space, mesh_arg, stencil)

    @staticmethod
    def get_mesh_arg(fparser2_tree, mesh_arg_index):
        '''Retrieves the mesh_arg metadata value from the supplied fparser2
        tree.

        :param fparser2_tree: fparser2 tree capturing the metadata for \
            an InterGrid argument.
        :type fparser2_tree: \
            :py:class:`fparser.two.Fortran2003.Structure_Constructor`
        :param int mesh_arg_index: the index at which to find the metadata.

        :returns: the metadata mesh value extracted from the fparser2 tree.
        :rtype: str

        raises ValueError: if the metadata is not in the form \
            'mesh_arg = <value>'.

        '''
        try:
            mesh_arg_lhs = fparser2_tree.children[1].\
                children[mesh_arg_index].children[0].tostr()
        except IndexError as info:
            raise ValueError(
                f"At argument index {mesh_arg_index} for metadata "
                f"'{fparser2_tree}' expected the metadata to be in the form "
                f"'mesh_arg=value' but found "
                f"'{fparser2_tree.children[1].children[mesh_arg_index]}'.") \
                from info

        if not mesh_arg_lhs.lower() == "mesh_arg":
            raise ValueError(
                f"At argument index {mesh_arg_index} for metadata "
                f"'{fparser2_tree}' expected the left hand side "
                f"to be 'mesh_arg' but found '{mesh_arg_lhs}'.")
        mesh_arg = fparser2_tree.children[1].\
            children[mesh_arg_index].children[1].tostr()
        return mesh_arg

    def fortran_string(self):
        '''
        :returns: the metadata represented by this class as Fortran.
        :rtype: str
        '''
        if self.stencil:
            return (f"arg_type({self.form}, {self.datatype}, {self.access}, "
                    f"{self.function_space}, stencil({self.stencil}), "
                    f"mesh_arg={self.mesh_arg})")
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


__all__ = ["InterGridArgMetadata"]
