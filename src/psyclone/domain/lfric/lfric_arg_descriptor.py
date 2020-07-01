# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017-2020, Science and Technology Facilities Council.
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
# Authors R. W. Ford, A. R. Porter and S. Siso, STFC Daresbury Lab
# Modified I. Kavcic, Met Office

'''
This module contains the LFRicArgDescriptor class and related constants
and properties.
'''

# Imports
from __future__ import print_function, absolute_import
import os
from psyclone.parse.kernel import Descriptor, get_stencil, get_mesh
from psyclone.parse.utils import ParseError
import psyclone.expression as expr
from psyclone.configuration import Config
from psyclone.core.access_type import AccessType
from psyclone.domain.lfric import FunctionSpace
from psyclone.errors import InternalError

# API configuration
API = "dynamo0.3"


class LFRicArgDescriptor(Descriptor):
    '''
    This class captures the information specified in one of LFRic API argument
    descriptors (scalars, fields and operators).

    :param arg_type: LFRic API valid argument type (scalar, \
                     field or operator).
    :type arg_type: :py:class:`psyclone.expression.FunctionVar` or \
                    :py:class:`psyclone.expression.BinaryOperator`
    :param iterates_over: iteration space from the parsed kernel metadata.
    :type iterates_over: str

    :raises ParseError: if a 'meta_arg' entry is not of 'arg_type' type.
    :raises ParseError: if a 'meta_arg' entry has fewer than 2 args.
    :raises ParseError: if an argument type is not one of LFRic API \
                        valid argument types.
    :raises ParseError: if the second 'meta_arg' entry is not a valid \
                        access descriptor.
    :raises InternalError: if the iteration space from the parsed kernel \
                           metadata is not over 'cells' or 'dofs'.
                "'{0}'.".format(self._iterates_over))
    :raises InternalError: if all the metadata checks fail to catch an \
                           invalid argument type.

    '''

    # ---------- LFRicArgDescriptor class constants  ------------------------ #
    # Supported LFRic API argument types (scalars, fields, operators)
    VALID_SCALAR_NAMES = ["gh_integer", "gh_real"]
    VALID_FIELD_NAMES = ["gh_field"]
    VALID_OPERATOR_NAMES = ["gh_operator", "gh_columnwise_operator"]
    VALID_ARG_TYPE_NAMES = VALID_FIELD_NAMES + VALID_OPERATOR_NAMES + \
        VALID_SCALAR_NAMES

    # Supported LFRic API stencil types and directions
    VALID_STENCIL_TYPES = ["x1d", "y1d", "xory1d", "cross", "region"]
    # Note, can't use VALID_STENCIL_DIRECTIONS at all locations in this
    # file as it causes failures with py.test 2.8.7. Therefore some parts
    # of the code do not use the VALID_STENCIL_DIRECTIONS variable.
    VALID_STENCIL_DIRECTIONS = ["x_direction", "y_direction"]
    # Note, xory1d does not have a direct mapping in STENCIL_MAPPING as it
    # indicates either x1d or y1d.
    # Note, the LFRic infrastructure currently does not have 'region' as
    # an option in stencil_dofmap_mod.F90 so it is not included in
    # STENCIL_MAPPING (TODO #194: Add support for region stencils).
    STENCIL_MAPPING = {"x1d": "STENCIL_1DX", "y1d": "STENCIL_1DY",
                       "cross": "STENCIL_CROSS"}

    # Supported LFRic API mesh types that may be specified for a field
    # using the mesh_arg=... meta-data element (for inter-grid kernels that
    # perform prolongation/restriction).
    VALID_MESH_TYPES = ["gh_coarse", "gh_fine"]
    # ----------------------------------------------------------------------- #

    def __init__(self, arg_type, iterates_over):
        self._arg_type = arg_type
        # Initialise properties
        self._type = None
        self._function_space_to = None
        self._function_space_from = None
        self._function_space = None
        self._function_spaces = []
        # Set vector size to 1 (scalars set it to 0 in their validation)
        self._vector_size = 1
        # Initialise other internal arguments
        self._access_type = None
        self._function_space1 = None
        self._function_space2 = None
        self._stencil = None
        self._mesh = None
        self._iterates_over = iterates_over

        # Check for correct type descriptor
        if arg_type.name != 'arg_type':
            raise ParseError(
                "In the LFRic API each 'meta_arg' entry must be of type "
                "'arg_type', but found '{0}'.".format(arg_type.name))

        # We require at least 2 args
        if len(arg_type.args) < 2:
            raise ParseError(
                "In the LFRic API each 'meta_arg' entry must have at "
                "least 2 args, but found '{0}'.".format(len(arg_type.args)))

        # Check the first argument descriptor. If it is a binary operator
        # then it has to be a field vector with an "*n" appended where "*"
        # is a binary operator and "n > 1" is a vector size. If it is a
        # variable then it can be one of the other allowed argument types.
        argtype = None
        separator = None
        if isinstance(arg_type.args[0], expr.BinaryOperator):
            argtype = arg_type.args[0].toks[0]
            separator = arg_type.args[0].toks[1]
        else:
            argtype = arg_type.args[0]

        # First check for a valid argument type. It has to be a variable
        # (FunctionVar expression) and have a valid LFRic API argument name.
        if isinstance(argtype, expr.FunctionVar) and argtype.name in \
           LFRicArgDescriptor.VALID_ARG_TYPE_NAMES:
            self._type = argtype.name
        else:
            raise ParseError(
                "In the LFRic API the 1st argument of a 'meta_arg' "
                "entry should be a valid argument type (one of {0}), but "
                "found '{1}' in '{2}'.".
                format(LFRicArgDescriptor.VALID_ARG_TYPE_NAMES,
                       argtype, arg_type))

        # Check for a valid vector size in case of a binary
        # operator expression
        if separator:
            self._validate_vector_size(separator, arg_type)

        # The 2nd arg is an access descriptor. Permitted accesses for each
        # argument type are dealt with in the related _validate methods.
        # Convert from GH_* names to the generic access type
        api_config = Config.get().api_conf(API)
        access_mapping = api_config.get_access_mapping()
        try:
            self._access_type = access_mapping[arg_type.args[1].name]
        except KeyError:
            valid_names = api_config.get_valid_accesses_api()
            raise ParseError(
                "In the LFRic API the 2nd argument of a 'meta_arg' entry "
                "must be a valid access descriptor (one of {0}), but found "
                "'{1}' in '{2}'.".format(valid_names,
                                         arg_type.args[1].name, arg_type))

        # Check for the allowed iteration spaces from the parsed kernel or
        # built-in metadata
        if self._iterates_over not in ["cells", "dofs"]:
            raise InternalError(
                "LFRicArgDescriptor.__init__(): expected iteration space "
                "over 'cells' or 'dofs' in the kernel metadata but got "
                "'{0}'.".format(self._iterates_over))

        # FIELD, OPERATOR and SCALAR argument type descriptors and checks
        if self._type in LFRicArgDescriptor.VALID_FIELD_NAMES:
            # Validate field arguments
            self._validate_field(arg_type)

        elif self._type in LFRicArgDescriptor.VALID_OPERATOR_NAMES:
            # Validate operator arguments
            self._validate_operator(arg_type)

        elif self._type in LFRicArgDescriptor.VALID_SCALAR_NAMES:
            # Validate scalar arguments
            self._validate_scalar(arg_type)

        else:
            # We should never get to here if the checks are tight enough
            raise InternalError(
                "LFRicArgDescriptor.__init__(): failed argument validation "
                "for the 'meta_arg' entry '{0}', should not get to here.".
                format(arg_type))

        # Initialise the parent class
        super(LFRicArgDescriptor,
              self).__init__(self._access_type, self._function_space1,
                             stencil=self._stencil, mesh=self._mesh)

    def _validate_vector_size(self, separator, arg_type):
        '''
        Validates descriptors for field vector arguments and populates
        vector properties accordingly.

        :param str separator: operator in a binary expression.
        :param arg_type: LFRic API field (vector) argument type.
        :type arg_type: :py:class:`psyclone.expression.FunctionVar`

        :raises ParseError: if the field vector notation does not use \
                            the '*' operator.
        :raises ParseError: if the field vector notation is not in the \
                            correct format '(field*n)' where 'n' is \
                            an integer.
        :raises ParseError: if the field vector notation is used for the \
                            vector size of less than 2.
        :raises ParseError: if the field vector notation is used for an \
                            argument that is not a field.

        '''
        # Check that the operator is correct
        if separator != "*":
            raise ParseError(
                "In the LFRic API the 1st argument of a 'meta_arg' "
                "entry may be a field vector but if so must use '*' as "
                "the separator in the format 'field*n', but found "
                "'{0}' in '{1}'.".format(separator, arg_type))

        # Now try to find the vector size for a field vector and return
        # an error if it is not an integer number...
        try:
            vectsize = int(arg_type.args[0].toks[2])
        except TypeError:
            raise ParseError(
                "In the LFRic API, the field vector notation must be in "
                "the format 'field*n' where 'n' is an integer, but the "
                "following '{0}' was found in '{1}'.".
                format(str(arg_type.args[0].toks[2]), arg_type))
        # ... or it is less than 2 (1 is the default for all fields)...
        if vectsize < 2:
            raise ParseError(
                "In the LFRic API the 1st argument of a 'meta_arg' entry "
                "may be a field vector with format 'field*n' where n is "
                "an integer > 1. However, found n = {0} in '{1}'.".
                format(vectsize, arg_type))
        # ... and set the vector size if all checks pass
        self._vector_size = vectsize

        # Check that no other arguments than fields use vector notation
        if self._type not in \
           LFRicArgDescriptor.VALID_FIELD_NAMES and self._vector_size:
            raise ParseError(
                "In the LFRic API, vector notation is only supported "
                "for {0} argument types but found '{1}'.".
                format(LFRicArgDescriptor.VALID_FIELD_NAMES,
                       arg_type.args[0]))

    def _validate_field(self, arg_type):
        '''
        Validates metadata descriptors for field arguments and
        populates argument properties accordingly.

        :param arg_type: LFRic API field (vector) argument type.
        :type arg_type: :py:class:`psyclone.expression.FunctionVar`

        :raises InternalError: if argument type other than a field is \
                               passed in.
        :raises ParseError: if there are fewer than 3 metadata arguments.
        :raises ParseError: if there are more than 4 metadata arguments.
        :raises ParseError: if the 3rd argument is not a valid function space.
        :raises ParseError: if the optional 4th argument is not a stencil \
                            specification or a mesh identifier (for \
                            inter-grid kernels).
        :raises ParseError: if a field on a discontinuous function space \
                            has 'gh_inc' access.
        :raises ParseError: if a field on a continuous function space has \
                            'gh_readwrite' access.
        :raises ParseError: if a field on 'any_space' function space has \
                            'gh_readwrite' access.
        :raises ParseError: if a field with a stencil access is not read only.

        '''
        # Check whether something other than a field is passed in
        if self._type not in LFRicArgDescriptor.VALID_FIELD_NAMES:
            raise InternalError(
                "LFRicArgDescriptor._validate_field(): expecting a field "
                "argument but got an argument of type '{0}'.".
                format(arg_type.args[0]))

        # There must be at least 3 arguments
        if len(arg_type.args) < 3:
            raise ParseError(
                "In the LFRic API each 'meta_arg' entry must have at "
                "least 3 arguments if its first argument is of {0} type, "
                "but found {1} in '{2}'.".
                format(LFRicArgDescriptor.VALID_FIELD_NAMES,
                       len(arg_type.args), arg_type))
        # There must be at most 4 arguments
        if len(arg_type.args) > 4:
            raise ParseError(
                "In the LFRic API each 'meta_arg' entry must have at "
                "most 4 arguments if its first argument is of {0} type, "
                "but found {1} in '{2}'.".
                format(LFRicArgDescriptor.VALID_FIELD_NAMES,
                       len(arg_type.args), arg_type))

        # The 3rd argument must be a valid function space name
        if arg_type.args[2].name not in \
           FunctionSpace.VALID_FUNCTION_SPACE_NAMES:
            raise ParseError(
                "In the LFRic API the 3rd argument of a 'meta_arg' "
                "entry must be a valid function space name (one of {0}) if "
                "its first argument is of {1} type, but found '{2}' in "
                "'{3}'.".format(FunctionSpace.VALID_FUNCTION_SPACE_NAMES,
                                LFRicArgDescriptor.VALID_FIELD_NAMES,
                                arg_type.args[2].name, arg_type))
        self._function_space1 = arg_type.args[2].name

        # The optional 4th argument is either a stencil specification
        # or a mesh identifier (for inter-grid kernels)
        if len(arg_type.args) == 4:
            try:
                if "stencil" in str(arg_type.args[3]):
                    self._stencil = get_stencil(
                        arg_type.args[3],
                        LFRicArgDescriptor.VALID_STENCIL_TYPES)
                elif "mesh" in str(arg_type.args[3]):
                    self._mesh = get_mesh(arg_type.args[3],
                                          LFRicArgDescriptor.VALID_MESH_TYPES)
                else:
                    raise ParseError("Unrecognised metadata entry")
            except ParseError as err:
                raise ParseError(
                    "In the LFRic API the 4th argument of a 'meta_arg' "
                    "field entry must be either a valid stencil specification"
                    "or a mesh identifier (for inter-grid kernels). However, "
                    "entry '{0}' raised the following error: {1}.".
                    format(arg_type, str(err)))

        # Test allowed accesses for fields
        field_disc_accesses = [AccessType.READ, AccessType.WRITE,
                               AccessType.READWRITE]
        field_cont_accesses = [AccessType.READ, AccessType.INC]
        # TODO in issue #138: Allowed accesses for fields on
        # continuous function spaces for the user-defined kernels that loop
        # over cells should only be [AccessType.READ, AccessType.INC].
        field_cont_accesses_tmp = field_cont_accesses + [AccessType.WRITE]
        # Convert generic access types to GH_* names for error messages
        api_config = Config.get().api_conf(API)
        rev_access_mapping = api_config.get_reverse_access_mapping()

        # Check fields on discontinuous function spaces
        fld_disc_acc_msg = [rev_access_mapping[acc] for acc in
                            field_disc_accesses]
        if self._function_space1.lower() in \
           FunctionSpace.VALID_DISCONTINUOUS_NAMES \
           and self._access_type not in field_disc_accesses:
            raise ParseError(
                "In the LFRic API, allowed accesses for a field on a "
                "discontinuous function space '{0}' are {1}, but found "
                "'{2}' in '{3}'.".
                format(self._function_space1.lower(), fld_disc_acc_msg,
                       rev_access_mapping[self._access_type], arg_type))

        # Check fields on continuous function spaces
        fld_cont_acc_msg = [rev_access_mapping[acc] for acc in
                            field_cont_accesses]
        fld_cont_acc_msg_tmp = [rev_access_mapping[acc] for acc in
                                field_cont_accesses_tmp]
        # As said above, allowed accesses for fields on continuous function
        # spaces is dealt with in #138
        if self._function_space1.lower() in \
           FunctionSpace.CONTINUOUS_FUNCTION_SPACES \
           and self._access_type not in field_cont_accesses_tmp:
            raise ParseError(
                "In the LFRic API, allowed accesses for a field on a "
                "continuous function space '{0}' are {1}, but found "
                "'{2}' in '{3}'.".
                format(self._function_space1.lower(), fld_cont_acc_msg_tmp,
                       rev_access_mapping[self._access_type], arg_type))

        if self._function_space1.lower() in \
           FunctionSpace.VALID_ANY_SPACE_NAMES:
           if self._iterates_over == "cells" and \
              self._access_type not in field_cont_accesses_tmp:
                raise ParseError(
                    "In the LFRic API, allowed accesses for a field on "
                    "'any_space' that loops over cells are {0}, but found "
                    "'{1}' in '{2}'.".
                    format(fld_cont_acc_msg_tmp,
                           rev_access_mapping[self._access_type], arg_type))
           if self._iterates_over == "dofs" and \
              self._access_type not in field_disc_accesses:
                raise ParseError(
                    "In the LFRic API, allowed accesses for a field on "
                    "'any_space' that loops over DoFs are {0}, but found "
                    "'{1}' in '{2}'.".
                    format(fld_disc_acc_msg,
                           rev_access_mapping[self._access_type], arg_type))


        # Test allowed accesses for fields that have stencil specification
        if self._stencil and self._access_type != AccessType.READ:
            raise ParseError(
                "In the LFRic API a field with a stencil access must be "
                "read-only ('{0}'), but found '{1}' in '{2}'.".
                format(rev_access_mapping[AccessType.READ],
                       rev_access_mapping[self._access_type], arg_type))

    def _validate_operator(self, arg_type):
        '''
        Validates metadata descriptors for operator arguments and
        populates argument properties accordingly.

        :param arg_type: LFRic API operator argument type.
        :type arg_type: :py:class:`psyclone.expression.FunctionVar`

        :raises InternalError: if argument type other than an operator is \
                               passed in.
        :raises ParseError: if there are not exactly 4 metadata arguments.
        :raises ParseError: if the function space to- is not one of the \
                            valid function spaces.
        :raises ParseError: if the function space from- is not one of the \
                            valid function spaces.
        :raises ParseError: if the operator argument has an invalid access.

        '''
        # Check whether something other than an operator is passed in
        if self._type not in LFRicArgDescriptor.VALID_OPERATOR_NAMES:
            raise InternalError(
                "LFRicArgDescriptor._validate_operator(): expecting an "
                "operator argument but got an argument of type '{0}'.".
                format(self._type))

        # We expect 4 arguments with the 3rd and 4th each being a
        # function space
        if len(arg_type.args) != 4:
            raise ParseError(
                "In the LFRic API each 'meta_arg' entry must have 4 "
                "arguments if its first argument is an operator (one "
                "of {0}), but found {1} in '{2}'.".
                format(LFRicArgDescriptor.VALID_OPERATOR_NAMES,
                       len(arg_type.args), arg_type))

        # Operator arguments need to have valid to- and from- function spaces
        if arg_type.args[2].name not in \
           FunctionSpace.VALID_FUNCTION_SPACE_NAMES:
            raise ParseError(
                "In the LFRic API the 3rd argument of a 'meta_arg' "
                "operator entry must be a valid function space name (one of "
                "{0}), but found '{1}' in '{2}'.".
                format(FunctionSpace.VALID_FUNCTION_SPACE_NAMES,
                       arg_type.args[2].name, arg_type))
        self._function_space1 = arg_type.args[2].name
        if arg_type.args[3].name not in \
           FunctionSpace.VALID_FUNCTION_SPACE_NAMES:
            raise ParseError(
                "In the LFRic API the 4th argument of a 'meta_arg' "
                "operator entry must be a valid function space name (one "
                "of {0}), but found '{1}' in '{2}'.".
                format(FunctionSpace.VALID_FUNCTION_SPACE_NAMES,
                       arg_type.args[3].name, arg_type))
        self._function_space2 = arg_type.args[3].name

        # Test allowed accesses for operators
        operator_accesses = [AccessType.READ, AccessType.WRITE,
                             AccessType.READWRITE]
        # Convert generic access types to GH_* names for error messages
        api_config = Config.get().api_conf(API)
        rev_access_mapping = api_config.get_reverse_access_mapping()
        op_acc_msg = [rev_access_mapping[acc] for acc in operator_accesses]
        if self._access_type not in operator_accesses:
            raise ParseError(
                "In the LFRic API, allowed accesses for operators are {0} "
                "because they behave as discontinuous quantities, but found "
                "'{1}' in '{2}'.".
                format(op_acc_msg, rev_access_mapping[self._access_type],
                       arg_type))

    def _validate_scalar(self, arg_type):
        '''
        Validates metadata descriptors for scalar arguments and
        populates argument properties accordingly.

        :param arg_type: LFRic API scalar argument type.
        :type arg_type: :py:class:`psyclone.expression.FunctionVar`

        :raises InternalError: if argument type other than a scalar is \
                               passed in.
        :raises ParseError: if there are not exactly 2 metadata arguments.
        :raises ParseError: if scalar arguments do not have a read-only or
                            a reduction access.
        :raises ParseError: if a scalar argument that is not a real \
                            scalar has a reduction access.

        '''
        # Check whether something other than a scalar is passed in
        if self._type not in LFRicArgDescriptor.VALID_SCALAR_NAMES:
            raise InternalError(
                "LFRicArgDescriptor._validate_scalar(): expecting a scalar "
                "argument but got an argument of type '{0}'.".
                format(arg_type.args[0]))

        # There must be at least 2 arguments to describe a scalar
        if len(arg_type.args) != 2:
            raise ParseError(
                "In the LFRic API each 'meta_arg' entry must have 2 "
                "arguments if its first argument is 'gh_{{r,i}}scalar', but "
                "found {0} in '{1}'.".format(len(arg_type.args), arg_type))

        # Test allowed accesses for scalars (read_only or reduction)
        scalar_accesses = [AccessType.READ] + \
            AccessType.get_valid_reduction_modes()
        # Convert generic access types to GH_* names for error messages
        api_config = Config.get().api_conf(API)
        rev_access_mapping = api_config.get_reverse_access_mapping()
        if self._access_type not in scalar_accesses:
            api_specific_name = rev_access_mapping[self._access_type]
            valid_reductions = AccessType.get_valid_reduction_names()
            raise ParseError(
                "In the LFRic API scalar arguments must have read-only "
                "('gh_read') or a reduction {0} access but found '{1}' "
                "in '{2}'.".format(valid_reductions, api_specific_name,
                                   arg_type))
        # Reduction access is currently only valid for real scalar arguments
        if self._type != "gh_real" and self._access_type in \
           AccessType.get_valid_reduction_modes():
            raise ParseError(
                "In the LFRic API a reduction access '{0}' is only valid "
                "with a real scalar argument, but '{1}' was found in '{2}'.".
                format(self._access_type.api_specific_name(),
                       self._type, arg_type))

        # Scalars don't have vector size
        self._vector_size = 0

    @property
    def type(self):
        '''
        :returns: the type of the argument (gh_field, gh_operator, ...).
        :rtype: str

        '''
        return self._type

    @property
    def function_space_to(self):
        '''
        Returns the "to" function space for an operator. This is
        the first function space specified in the metadata.

        :returns: "to" function space for an operator.
        :rtype: str

        :raises InternalError: if this is not an operator.

        '''
        if self._type in LFRicArgDescriptor.VALID_OPERATOR_NAMES:
            return self._function_space1
        raise InternalError(
            "In the LFRic API 'function_space_to' only makes sense "
            "for one of {0}, but this is a '{1}'.".
            format(LFRicArgDescriptor.VALID_OPERATOR_NAMES, self._type))

    @property
    def function_space_from(self):
        '''
        Returns the "from" function space for an operator. This is
        the second function space specified in the metadata.

        :returns: "from" function space for an operator.
        :rtype: str

        :raises InternalError: if this is not an operator.

        '''
        if self._type in LFRicArgDescriptor.VALID_OPERATOR_NAMES:
            return self._function_space2
        raise InternalError(
            "In the LFRic API 'function_space_from' only makes sense "
            "for one of {0}, but this is a '{1}'.".
            format(LFRicArgDescriptor.VALID_OPERATOR_NAMES, self._type))

    @property
    def function_space(self):
        '''
        Returns the function space name related to this kernel argument
        depending on the argument type: a single function space for a field,
        function_space_from for an operator and nothing for a scalar.

        :returns: function space relating to this kernel argument or \
                  None (for a scalar).
        :rtype: str or NoneType

        :raises InternalError: if an invalid argument type is passed in.

        '''
        if self._type in LFRicArgDescriptor.VALID_FIELD_NAMES:
            return self._function_space1
        if self._type in LFRicArgDescriptor.VALID_OPERATOR_NAMES:
            return self._function_space2
        if self._type in LFRicArgDescriptor.VALID_SCALAR_NAMES:
            return None
        raise InternalError("LFRicArgDescriptor.function_space(), should "
                            "not get to here.")

    @property
    def function_spaces(self):
        '''
        Returns the function space names related to this kernel argument
        as a list depending on the argument type: one function space for
        a field, both function spaces ("to"- and then "from"-) for an
        operator and an empty list for a scalar.

        :returns: function space names related to this kernel argument.
        :rtype: list of str

        :raises InternalError: if an invalid argument type is passed in.

        '''
        if self._type in LFRicArgDescriptor.VALID_FIELD_NAMES:
            return [self.function_space]
        if self._type in LFRicArgDescriptor.VALID_OPERATOR_NAMES:
            # return to before from to maintain expected ordering
            return [self.function_space_to, self.function_space_from]
        if self._type in LFRicArgDescriptor.VALID_SCALAR_NAMES:
            return []
        raise InternalError("LFRicArgDescriptor.function_spaces(), should "
                            "not get to here.")

    @property
    def vector_size(self):
        '''
        Returns the vector size of the argument. This will be 1 if *n
        has not been specified for all argument types except scalars
        (their vector size is set to 0).

        :returns: vector size of the argument.
        :rtype: int

        '''
        return self._vector_size

    def __str__(self):
        '''
        Creates a string representation of the argument descriptor. This
        is type and access for scalars with the addition of function
        space(s) for fields and operators.

        :returns: string representation of the argument descriptor.
        :rtype: str

        :raises InternalError: if an invalid argument type is passed in.

        '''
        res = "LFRicArgDescriptor object" + os.linesep
        res += "  argument_type[0]='{0}'".format(self._type)
        if self._vector_size > 1:
            res += "*"+str(self._vector_size)
        res += os.linesep
        res += "  access_descriptor[1]='{0}'"\
               .format(self._access_type.api_specific_name())\
               + os.linesep
        if self._type in LFRicArgDescriptor.VALID_FIELD_NAMES:
            res += "  function_space[2]='{0}'".format(self._function_space1) \
                   + os.linesep
        elif self._type in LFRicArgDescriptor.VALID_OPERATOR_NAMES:
            res += "  function_space_to[2]='{0}'".\
                   format(self._function_space1) + os.linesep
            res += "  function_space_from[3]='{0}'".\
                   format(self._function_space2) + os.linesep
        elif self._type in LFRicArgDescriptor.VALID_SCALAR_NAMES:
            pass  # we have nothing to add if we're a scalar
        else:  # we should never get to here
            raise InternalError("LFRicArgDescriptor.__str__(), should not "
                                "get to here.")
        return res


# Documentation utils: The list of module members that we wish AutoAPI to
# generate documentation for. (See https://psyclone-ref.readthedocs.io)
__all__ = [
    'LFRicArgDescriptor']
