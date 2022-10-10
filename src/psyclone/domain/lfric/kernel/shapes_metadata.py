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

'''Module containing the ShapesMetadata class which captures the
values for the LFRic kernel GH_SHAPE metadata.

        The fparser2 class hierarchy for the different flavours
        of valid gh_shape metadata above is given below:

        Data_Component_Def_Stmt
            [0] Intrinsic_Type_Spec
                [0] <str> INTEGER
                [1] <NoneType>
            [1] Component_Attr_Spec_List or <NoneType>
                ...
                Dimension_Component_Attr_Spec
                    [0] <str> DIMENSION
                    [1] Explicit_Shape_Spec_List
                        ...
                        Explicit_Shape_Spec
                            [0] <NoneType>
                            [1] Int_Literal_Constant
                                [0] <str> 1
                                [1] <NoneType>
                        ...
                ...
            [2] Component_Decl_List
                ...
                Component_Decl
                    [0] Name.string: gh_shape
                    [1] Explicit_Shape_Spec_List or <NoneType>
                        [0] <NoneType>
                        [1] Int_Literal_Constant
                            [0] <str> 1
                            [1] <NoneType>
                    [2] <NoneType>
                    [3] Component_Initialization
                        [0] <str> =
                        [1] Name.string: gh_quadrature_face
                        *OR*
                        [1] Array_Constructor (/ ... /)
                            [0] <str> (/
                            [1] Ac_Value_List
                                Name.string: gh_quadrature_face
                                ...
                            [2] <str> /)
                ...
'''
from fparser.two import Fortran2003

from psyclone.domain.lfric import LFRicConstants
from psyclone.domain.lfric.kernel.common_declaration_metadata import \
    CommonDeclarationMetadata
from psyclone.parse.utils import ParseError


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
        self.shapes = shapes

    def fortran_string(self):
        '''
         :returns: the shapes metadata as Fortran.
         :rtype: str
        '''
        if len(self.shapes) == 1:
            return ShapesMetadata.scalar_declaration_string(
                "INTEGER", "GH_SHAPE", self.shapes[0])
        else:
            return ShapesMetadata.array_declaration_string(
                "INTEGER", "GH_SHAPE", self.shapes)

    @staticmethod
    def create_from_fparser2(fparser2_tree):
        '''Create an instance of ShapesMetadata from an fparser2 tree.

        LFRic shape metadata can have a scalar and array form. Two
        versions of the array form are supported:

        integer :: gh_shape = gh_quadrature_face
        integer :: gh_shape(2) = (/ gh_quadrature_face, gh_evaluator /)
        integer, dimension(2) :: gh_shape = (/ gh_quadrature_face, gh_evaluator /)

        :param fparser2_tree: fparser2 tree capturing the shapes metadata
        :type fparser2_tree: :py:class:`fparser.two.Fortran2003.\
            Data_Component_Def_Stmt`

        :returns: an instance of ShapesMetadata.
        :rtype: :py:class:`psyclone.domain.lfric.kernel.ShapesMetadata`

        '''
        ShapesMetadata.validate_node(
            fparser2_tree, Fortran2003.Data_Component_Def_Stmt)
        ShapesMetadata.validate_datatype_name_value(
            fparser2_tree, "INTEGER", "GH_SHAPE")

        const = LFRicConstants()
        valid_values = const.VALID_EVALUATOR_SHAPES

        component_decl_list = fparser2_tree.children[2]
        gh_shape_declaration = component_decl_list.children[0]
        if fparser2_tree.children[1] or gh_shape_declaration.children[1]:
            # This is probably an array
            shapes_list = ShapesMetadata.validate_array_declaration(
                fparser2_tree, "INTEGER", "GH_SHAPE", valid_values)
        else:
            shapes_value = ShapesMetadata.validate_scalar_declaration(
                fparser2_tree, "INTEGER", "GH_SHAPE", valid_values)
            shapes_list = [shapes_value]

        return ShapesMetadata(shapes_list)

    @property
    def shapes(self):
        '''
        :returns: a list of shape values
        :rtype: List[str]
        '''
        return self._shapes[:]

    @shapes.setter
    def shapes(self, values):
        '''
        :param values: set the shapes metdata to the supplied list of \
            values.
        :type values: List[str]

        raises TypeError: if the supplied value is not a list.
        raises TypeError: if the supplied value is an empty list.
        raises TypeError: if any entry in the list is not of the \
            required type.

        '''
        if not isinstance(values, list):
            raise TypeError(f"shape values should be provided as a list but "
                            f"found '{type(values).__name__}'.")
        if not values:
            raise TypeError(
                "The shapes list should contain at least one entry, but "
                "it is empty.")
        const = LFRicConstants()
        for value in values:
            if not isinstance(value, str):
                raise TypeError(
                    f"shapes should be a list of str, "
                    f"but found '{type(value).__name__}'.")
            if value.lower() not in const.VALID_EVALUATOR_SHAPES:
                raise ValueError(
                    f"The shape metadata should be a recognised "
                    f"value (one of {const.VALID_EVALUATOR_SHAPES}) "
                    f"but found '{value}'.")
        # Take a copy of the list so that it can't be modified
        # externally. Also make all values lower case.
        self._shapes = [value.lower() for value in values]
