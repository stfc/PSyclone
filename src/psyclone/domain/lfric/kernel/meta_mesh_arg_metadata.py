# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2022-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

'''Module containing the MetaMeshArgMetadata class which
captures the argument values for the LFRic kernel
META_MESH metadata.

'''
from fparser.two import Fortran2003

from psyclone.domain.lfric import LFRicConstants
from psyclone.domain.lfric.kernel.common_arg_metadata import CommonArgMetadata


class MetaMeshArgMetadata(CommonArgMetadata):
    '''Class to capture the LFRic kernel metadata information for a
    meta_mesh argument.

    :param str mesh: the name of the mesh property.

    '''
    fparser2_class = Fortran2003.Part_Ref

    def __init__(self, mesh):
        super().__init__()
        self.mesh = mesh

    @staticmethod
    def create_from_fparser2(fparser2_tree):
        '''Create an instance of this class from an fparser2 tree.

        :param fparser2_tree: fparser2 tree containing the metadata \
            for a meta_mesh argument.
        :type fparser2_tree: :py:class:`fparser.two.Fortran2003.Part_Ref`

        :returns: an instance of this class.
        :rtype: :py:class:`psyclone.domain.lfric.kernel.MetaMeshArgMetadata`

        '''
        MetaMeshArgMetadata.check_fparser2_arg(
            fparser2_tree, type_name="mesh_data_type")
        MetaMeshArgMetadata.check_nargs(fparser2_tree, 1)
        mesh = MetaMeshArgMetadata.get_arg(fparser2_tree, 0)
        return MetaMeshArgMetadata(mesh)

    def fortran_string(self):
        '''
        :returns: the metadata represented by this class as Fortran.
        :rtype: str
        '''
        return f"mesh_data_type({self.mesh})"

    @property
    def mesh(self):
        '''
        :returns: the mesh property for this meta_mesh argument.
        :rtype: str
        '''
        return self._mesh

    @mesh.setter
    def mesh(self, value):
        '''
        :param str value: set the mesh property to the specified value.
        '''
        const = LFRicConstants()
        self.validate_scalar_value(
            value, const.VALID_MESH_NAMES, "mesh property")
        self._mesh = value.lower()


__all__ = ["MetaMeshArgMetadata"]
