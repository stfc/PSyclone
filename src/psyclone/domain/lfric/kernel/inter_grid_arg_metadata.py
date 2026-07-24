# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2022-2026, Science and Technology Facilities Council
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
from typing import Optional
from fparser.two import Fortran2003

from psyclone.domain.lfric import LFRicConstants
from psyclone.domain.lfric.kernel.field_arg_metadata import FieldArgMetadata


class InterGridArgMetadata(FieldArgMetadata):
    '''Class to capture LFRic kernel metadata information for an intergrid
    argument.

    :param datatype: the datatype of this InterGrid argument (GH_INTEGER, ...).
    :param access: the way the kernel accesses this intergrid
        argument (GH_WRITE, ...).
    :param function_space: the function space that this
        InterGrid is on (W0, ...).
    :param mesh_arg: the type of mesh that this InterGrid arg
        is on (coarse or fine).
    :param stencil: the type of stencil used by the
        kernel when accessing this InterGrid arg.

    '''
    # The name to use for any exceptions.
    check_name = "inter-grid"
    # The number of arguments in the language-level metadata (min and
    # max values).
    nargs = (5, 8)

    # The fparser2 class that captures this metadata.
    fparser2_class = Fortran2003.Structure_Constructor

    def __init__(self,
                 datatype: str,
                 access: str,
                 function_space: str,
                 mesh_arg: str,
                 stencil: Optional[str] = None,
                 nlevels: Optional[str] = None,
                 ndata: Optional[str] = "1"):
        super().__init__(datatype, access, function_space, stencil=stencil,
                         nlevels=nlevels, ndata=ndata)
        self.mesh_arg = mesh_arg

    @classmethod
    def _get_metadata(
            cls,
            fparser2_tree: Fortran2003.Structure_Constructor
    ) -> tuple[str, str, str, str,
               Optional[str], Optional[str], Optional[str]]:
        '''Extract the required metadata from the fparser2 tree and return it
        as strings. Also check that the metadata is in the expected form (but
        do not check the metadata values as that is done separately).

        :param fparser2_tree: fparser2 tree containing the metadata
            for this argument.

        :returns: a tuple containing the datatype, access, function
            space, mesh, stencil, nlevels and ndata metadata.

        '''
        datatype, access = cls._get_datatype_access_metadata(fparser2_tree)
        function_space = cls.get_arg(fparser2_tree,
                                     cls.function_space_arg_index)

        cls._validate_named_args(fparser2_tree, ["mesh_arg", "nlevels",
                                                 "ndata"])
        stencil = cls.get_stencil(fparser2_tree)

        mesh_arg = cls.get_named_arg(fparser2_tree, "mesh_arg")
        nlevels = cls.get_named_arg(fparser2_tree, "nlevels")
        ndata = cls.get_named_arg(fparser2_tree, "ndata")

        return (datatype, access, function_space, mesh_arg, stencil, nlevels,
                ndata)

    def fortran_string(self) -> str:
        '''
        :returns: the metadata represented by this class as Fortran.
        '''
        result = super().fortran_string()
        # Have to remove closing ')' before adding mesh_arg info.
        return (f"{result[:-1]}, mesh_arg={self.mesh_arg})")

    @property
    def mesh_arg(self) -> str:
        '''
        :returns: the mesh type for this intergrid argument.
        '''
        return self._mesh_arg

    @mesh_arg.setter
    def mesh_arg(self, value: str) -> None:
        '''
        :param value: set the mesh type to the specified value.
        '''
        const = LFRicConstants()
        InterGridArgMetadata.validate_scalar_value(
            value, const.VALID_MESH_TYPES, "mesh_arg")
        self._mesh_arg = value.lower()


__all__ = ["InterGridArgMetadata"]
