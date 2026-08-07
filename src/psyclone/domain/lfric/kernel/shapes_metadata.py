# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2022-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

'''Module containing the ShapesMetadata class which captures the
values for the LFRic kernel GH_SHAPE metadata.

'''
from fparser.two import Fortran2003

from psyclone.domain.lfric import LFRicConstants
from psyclone.domain.lfric.kernel.common_declaration_metadata import \
    CommonDeclarationMetadata


class ShapesMetadata(CommonDeclarationMetadata):
    '''Class to capture the values of the LFRic kernel GH_SHAPE metadata.
    This class supports the creation, modification and Fortran output
    of this metadata.

    If an LFRic kernel requires basis or differential-basis functions
    then the metadata must also specify the set of points on which
    these functions are required. This information is provided by the
    GH_SHAPE component of the metadata.

    :param shapes: a list of shape values
    :type shapes: List[str]

    '''
    def __init__(self, shapes):
        super().__init__()
        self.shapes = shapes

    def fortran_string(self):
        '''
        :returns: the shapes metadata as Fortran.
        :rtype: str
        '''
        if len(self.shapes) == 1:
            return ShapesMetadata.scalar_declaration_string(
                "INTEGER", "GH_SHAPE", self.shapes[0])
        return ShapesMetadata.array_declaration_string(
            "INTEGER", "GH_SHAPE", self.shapes)

    @staticmethod
    def create_from_fparser2(fparser2_tree):
        '''Create an instance of ShapesMetadata from an fparser2 tree.

        LFRic shape metadata can have a scalar and array form. Two
        versions of the array form are supported:
        ::

            integer :: gh_shape = gh_quadrature_face
            integer :: gh_shape(2) = (/ gh_quadrature_face, gh_evaluator /)
            integer, dimension(2) :: gh_shape = &
                     (/ gh_quadrature_face, gh_evaluator /)

        :param fparser2_tree: fparser2 tree capturing the shapes metadata
        :type fparser2_tree: :py:class:`fparser.two.Fortran2003.\
            Data_Component_Def_Stmt`

        :returns: an instance of ShapesMetadata.
        :rtype: :py:class:`psyclone.domain.lfric.kernel.ShapesMetadata`

        '''
        # As both scalar and array forms are supported we need the
        # validation from both get_intrinsic_array_declaration and
        # get_intrinsic_scalar_declaration. However, we can't call
        # these functions separately as both might raise an exception
        # and we won't know which exception to return. Instead we call
        # the validation that is common to both first and then test
        # for an array declaration to determine whether to call the
        # array or scalar validation.
        ShapesMetadata.validate_node(
            fparser2_tree, Fortran2003.Data_Component_Def_Stmt)
        ShapesMetadata.validate_name_value(
            fparser2_tree, "GH_SHAPE")

        const = LFRicConstants()
        valid_values = const.VALID_EVALUATOR_SHAPES

        component_decl_list = fparser2_tree.children[2]
        gh_shape_declaration = component_decl_list.children[0]
        if fparser2_tree.children[1] or gh_shape_declaration.children[1]:
            # This is not the scalar form so check for the array form.
            shapes_list = ShapesMetadata.get_intrinsic_array_declaration(
                fparser2_tree, "INTEGER", "GH_SHAPE", valid_values)
        else:
            # Check for the scalar form.
            shapes_value = ShapesMetadata.\
                get_intrinsic_scalar_declaration(
                    fparser2_tree, "INTEGER", "GH_SHAPE", valid_values)
            shapes_list = [shapes_value]

        return ShapesMetadata(shapes_list)

    @property
    def shapes(self):
        '''
        :returns: a list of shape values
        :rtype: List[str]
        '''
        # Return a copy of the list so it can't be modified
        # externally.
        return self._shapes[:]

    @shapes.setter
    def shapes(self, values):
        '''
        :param values: set the shapes metadata to the supplied list of \
            values.
        :type values: List[str]
        '''
        const = LFRicConstants()
        ShapesMetadata.validate_list(values, str)
        for value in values:
            ShapesMetadata.validate_scalar_value(
                value, const.VALID_EVALUATOR_SHAPES, "shape")
        # Take a copy of the list so that it can't be modified
        # externally. Also make all values lower case.
        self._shapes = [value.lower() for value in values]


__all__ = ["ShapesMetadata"]
