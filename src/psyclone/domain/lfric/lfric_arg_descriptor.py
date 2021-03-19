# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017-2021, Science and Technology Facilities Council.
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
# Modified I. Kavcic and A. Coughtrie, Met Office

'''
This module contains the LFRicArgDescriptor class and related constants
and properties.
'''

# Imports
from __future__ import print_function, absolute_import
import os
import six
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
    :param operates_on: value of operates_on from the parsed kernel metadata \
                        (used for validation).
    :type operates_on: str

    :raises ParseError: if a 'meta_arg' entry is not of 'arg_type' type.
    :raises ParseError: if the first argument of a 'meta_arg' entry is not \
                        one of LFRic API valid argument types.
    :raises ParseError: if the second argument of a 'meta_arg' entry is not \
                        one of LFRic API valid data types.
    :raises ParseError: if a 'meta_arg' entry has fewer than 3 args.
    :raises ParseError: if the third 'meta_arg' entry is not a valid \
                        access descriptor.
    :raises InternalError: if the operates_on from the parsed kernel \
                           metadata is not 'cell_column' or 'dof'.
    :raises InternalError: if all the metadata checks fail to catch an \
                           invalid argument type.

    '''

    # ---------- LFRicArgDescriptor class constants  ------------------------ #
    # Supported LFRic API argument types (scalars, fields, operators)
    # TODO in #874: Remove support for the old-style scalar metadata
    #               (["gh_integer", "gh_real"]).
    VALID_SCALAR_NAMES = ["gh_scalar", "gh_real", "gh_integer"]
    VALID_FIELD_NAMES = ["gh_field"]
    VALID_OPERATOR_NAMES = ["gh_operator", "gh_columnwise_operator"]
    VALID_ARG_TYPE_NAMES = VALID_FIELD_NAMES + VALID_OPERATOR_NAMES + \
        VALID_SCALAR_NAMES

    # Supported API argument data types (real and integer for now)
    VALID_ARG_DATA_TYPES = ["gh_real", "gh_integer"]
    VALID_SCALAR_DATA_TYPES = VALID_ARG_DATA_TYPES
    VALID_FIELD_DATA_TYPES = VALID_ARG_DATA_TYPES
    VALID_OPERATOR_DATA_TYPES = ["gh_real"]

    # Supported LFRic API stencil types and directions
    VALID_STENCIL_TYPES = ["x1d", "y1d", "xory1d", "cross", "region",
                           "cross2d"]
    # Note, can't use VALID_STENCIL_DIRECTIONS at all locations in this
    # file as it causes failures with py.test 2.8.7. Therefore some parts
    # of the code do not use the VALID_STENCIL_DIRECTIONS variable.
    VALID_STENCIL_DIRECTIONS = ["x_direction", "y_direction"]
    # Note, xory1d does not have a direct mapping in STENCIL_MAPPING as it
    # indicates either x1d or y1d.
    STENCIL_MAPPING = {"x1d": "STENCIL_1DX", "y1d": "STENCIL_1DY",
                       "cross": "STENCIL_CROSS", "cross2d": "STENCIL_2D_CROSS",
                       "region": "STENCIL_REGION"}

    # Supported LFRic API mesh types that may be specified for a field
    # using the mesh_arg=... meta-data element (for inter-grid kernels that
    # perform prolongation/restriction).
    VALID_MESH_TYPES = ["gh_coarse", "gh_fine"]
    # ----------------------------------------------------------------------- #

    def __init__(self, arg_type, operates_on):
        self._arg_type = arg_type
        # Initialise properties
        self._argument_type = None
        self._data_type = None
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
        self._nargs = 0
        # Initialise temporary "offset" internal argument required
        # to support the old and the current argument metadata style.
        # TODO in #874: Remove support the for the old-style metadata
        #               as well as this temporary argument.
        self._offset = 0

        # Check for correct type descriptor
        if arg_type.name != 'arg_type':
            raise ParseError(
                "In the LFRic API each 'meta_arg' entry must be of type "
                "'arg_type', but found '{0}'.".format(arg_type.name))

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
            self._argument_type = argtype.name
        else:
            raise ParseError(
                "In the LFRic API the 1st argument of a 'meta_arg' "
                "entry should be a valid argument type (one of {0}), "
                "but found '{1}' in '{2}'.".
                format(LFRicArgDescriptor.VALID_ARG_TYPE_NAMES,
                       argtype, arg_type))

        # Check for a valid vector size in case of a binary
        # operator expression
        if separator:
            self._validate_vector_size(separator, arg_type)

        # The 2nd arg is the Fortran primitive type of their data.
        # TODO in #874: Remove support for the old-style metadata.
        # Note: Here we also set internal "offset" argument required to
        #       support the old and the current argument metadata style.
        dtype = arg_type.args[1].name
        # ParseError for an invalid data type is raised here for scalars
        # as we know their argtype. This condition will be removed in
        # #874 and this metadata check will be valid for all argument types.
        if self._argument_type == "gh_scalar":
            if dtype in LFRicArgDescriptor.VALID_ARG_DATA_TYPES:
                self._data_type = dtype
                self._offset = 1
            else:
                raise ParseError(
                    "In the LFRic API the 2nd argument of a 'meta_arg' "
                    "entry should be a valid data type (one of {0}), "
                    "but found '{1}' in '{2}'.".
                    format(LFRicArgDescriptor.VALID_ARG_DATA_TYPES,
                           dtype, arg_type))
        # TODO in # 874: Remove this temporary ParseError check for invalid
        #      data type for field and operator arguments.
        #      The old-style metadata has access type as the second argument
        #      and no data type. The new metadata has access type as the
        #      third argument and data type as the second.
        fld_or_op_arg = (
            self._argument_type in LFRicArgDescriptor.VALID_FIELD_NAMES or
            self._argument_type in LFRicArgDescriptor.VALID_OPERATOR_NAMES)
        if fld_or_op_arg:
            api_config = Config.get().api_conf(API)
            access_mapping = api_config.get_access_mapping()
            if arg_type.args[2].name in access_mapping:
                if dtype in LFRicArgDescriptor.VALID_ARG_DATA_TYPES:
                    self._data_type = dtype
                    self._offset = 1
                else:
                    raise ParseError(
                        "In the LFRic API the 2nd argument of a 'meta_arg' "
                        "entry should be a valid data type (one of {0}), "
                        "but found '{1}' in '{2}'.".
                        format(LFRicArgDescriptor.VALID_ARG_DATA_TYPES,
                               dtype, arg_type))

        # Check number of args (in general and also for scalar arguments).
        # We require at least three (two for old-style metadata).
        # TODO in issue #874: Remove offset and restore this check below
        #                     the first check for the correct 'arg_type'
        #                     descriptor name.
        self._nargs = len(arg_type.args)
        min_nargs = 2 + self._offset
        if self._nargs < min_nargs:
            raise ParseError(
                "In the LFRic API each 'meta_arg' entry must have at least "
                "{0} args, but found {1} in '{2}'.".
                format(min_nargs, self._nargs, arg_type))

        # The 3rd arg is an access descriptor. Permitted accesses for each
        # argument type are dealt with in the related _init methods.
        # Convert from GH_* names to the generic access type
        api_config = Config.get().api_conf(API)
        access_mapping = api_config.get_access_mapping()
        prop_ind = 1 + self._offset
        try:
            self._access_type = access_mapping[arg_type.args[prop_ind].name]
        except KeyError as err:
            valid_names = api_config.get_valid_accesses_api()
            six.raise_from(ParseError(
                "In the LFRic API argument {0} of a 'meta_arg' entry "
                "must be a valid access descriptor (one of {1}), but found "
                "'{2}' in '{3}'.".
                format(prop_ind+1, valid_names, arg_type.args[prop_ind].name,
                       arg_type)), err)

        # Check for the allowed iteration spaces from the parsed kernel
        # metadata
        from psyclone.dynamo0p3 import VALID_ITERATION_SPACES
        if operates_on not in VALID_ITERATION_SPACES:
            raise InternalError(
                "Expected operates_on in the kernel metadata to be one of "
                "{0} but got '{1}'.".format(
                    VALID_ITERATION_SPACES, operates_on))

        # FIELD, OPERATOR and SCALAR argument type descriptors and checks
        if self._argument_type in LFRicArgDescriptor.VALID_FIELD_NAMES:
            # Validate field arguments
            self._init_field(arg_type, operates_on)

        elif self._argument_type in LFRicArgDescriptor.VALID_OPERATOR_NAMES:
            # Validate operator arguments
            self._init_operator(arg_type)

        elif self._argument_type in LFRicArgDescriptor.VALID_SCALAR_NAMES:
            # Validate scalar arguments
            self._init_scalar(arg_type)

        else:
            # We should never get to here if the checks are tight enough
            raise InternalError(
                "Failed argument validation for the 'meta_arg' entry '{0}', "
                "should not get to here.".format(arg_type))

        # Initialise the parent class
        super(LFRicArgDescriptor,
              self).__init__(self._access_type, self._function_space1,
                             stencil=self._stencil, mesh=self._mesh,
                             argument_type=self._argument_type)

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
        except TypeError as err:
            six.raise_from(ParseError(
                "In the LFRic API, the field vector notation must be in "
                "the format 'field*n' where 'n' is an integer, but the "
                "following '{0}' was found in '{1}'.".
                format(str(arg_type.args[0].toks[2]), arg_type)), err)
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
        if self._argument_type not in \
           LFRicArgDescriptor.VALID_FIELD_NAMES and self._vector_size:
            raise ParseError(
                "In the LFRic API, vector notation is only supported "
                "for {0} argument types but found '{1}'.".
                format(LFRicArgDescriptor.VALID_FIELD_NAMES,
                       arg_type.args[0]))

    def _init_field(self, arg_type, operates_on):
        '''
        Validates metadata descriptors for field arguments and
        initialises field argument properties accordingly.

        :param arg_type: LFRic API field (vector) argument type.
        :type arg_type: :py:class:`psyclone.expression.FunctionVar`
        :param operates_on: value of operates_on from the parsed kernel \
                            metadata (used for validation).
        :type operates_on: str

        :raises InternalError: if argument type other than a field is \
                               passed in.
        :raises ParseError: if there are fewer than 4 metadata arguments.
        :raises ParseError: if there are more than 5 metadata arguments.
        :raises InternalError: if a field argument has an invalid data type.
        :raises ParseError: if the 4th argument is not a valid function space.
        :raises ParseError: if the optional 5th argument is not a stencil \
                            specification or a mesh identifier (for \
                            inter-grid kernels).
        :raises ParseError: if a field passed to a kernel that operates on \
                            DoFs does not have a valid access \
                            (one of [READ, WRITE, READWRITE]).
        :raises ParseError: if a field on a discontinuous function space \
                            passed to a kernel that operates on cell-columns \
                            does not have a valid access (one of \
                            [READ, WRITE, READWRITE]).
        :raises ParseError: if a field on a continuous function space \
                            passed to a kernel that operates on cell-columns \
                            does not have a valid access (one of [READ, INC]).
        :raises ParseError: if the kernel operates on the domain and is \
                            passed a field on a continuous space.
        :raises InternalError: if an invalid value for operates_on is \
                               passed in.
        :raises ParseError: if a field with a stencil access is not read-only.
        :raises ParseError: if a field with a stencil access is passed to a \
                            kernel that operates on the domain.

        '''
        # Check whether something other than a field is passed in
        if self._argument_type not in LFRicArgDescriptor.VALID_FIELD_NAMES:
            raise InternalError(
                "Expected a field argument but got an argument of type "
                "'{0}'.".format(arg_type.args[0]))

        # TODO in #874: Remove support for the old-style field descriptors
        #               throughout this routine.
        # There must be at least 4 arguments
        nargs_field_min = 3 + self._offset
        if self._nargs < nargs_field_min:
            raise ParseError(
                "In the LFRic API each 'meta_arg' entry must have at "
                "least {0} arguments if its first argument is of {1} type, "
                "but found {2} in '{3}'.".
                format(nargs_field_min, LFRicArgDescriptor.VALID_FIELD_NAMES,
                       self._nargs, arg_type))
        # There must be at most 5 arguments
        nargs_field_max = 4 + self._offset
        if self._nargs > nargs_field_max:
            raise ParseError(
                "In the LFRic API each 'meta_arg' entry must have at "
                "most {0} arguments if its first argument is of {1} type, "
                "but found {2} in '{3}'.".
                format(nargs_field_max, LFRicArgDescriptor.VALID_FIELD_NAMES,
                       self._nargs, arg_type))

        # Check whether an invalid data type for a field argument is passed in.
        # TODO in #874: Remove the support for old-style field metadata that
        #               prescribes the data type.
        if not self._data_type and self._offset == 0:
            self._data_type = "gh_real"
        if (self._data_type not in
                LFRicArgDescriptor.VALID_FIELD_DATA_TYPES):
            raise InternalError(
                "Expected one of {0} as the field data type but got '{1}'.".
                format(LFRicArgDescriptor.VALID_FIELD_DATA_TYPES,
                       self._data_type))

        # The 4th argument must be a valid function-space name
        prop_ind = 2 + self._offset
        if arg_type.args[prop_ind].name not in \
           FunctionSpace.VALID_FUNCTION_SPACE_NAMES:
            raise ParseError(
                "In the LFRic API argument {0} of a 'meta_arg' field entry "
                "must be a valid function-space name (one of {1}) if its "
                "first argument is of {2} type, but found '{3}' in '{4}'.".
                format(prop_ind+1, FunctionSpace.VALID_FUNCTION_SPACE_NAMES,
                       LFRicArgDescriptor.VALID_FIELD_NAMES,
                       arg_type.args[prop_ind].name, arg_type))
        self._function_space1 = arg_type.args[prop_ind].name

        # The optional 5th argument is either a stencil specification
        # or a mesh identifier (for inter-grid kernels)
        prop_ind = 3 + self._offset
        if self._nargs == nargs_field_max:
            try:
                if "stencil" in str(arg_type.args[prop_ind]):
                    self._stencil = get_stencil(
                        arg_type.args[prop_ind],
                        LFRicArgDescriptor.VALID_STENCIL_TYPES)
                elif "mesh" in str(arg_type.args[prop_ind]):
                    self._mesh = get_mesh(arg_type.args[prop_ind],
                                          LFRicArgDescriptor.VALID_MESH_TYPES)
                else:
                    raise ParseError("Unrecognised metadata entry")
            except ParseError as err:
                six.raise_from(ParseError(
                    "In the LFRic API argument {0} of a 'meta_arg' field "
                    "entry must be either a valid stencil specification"
                    "or a mesh identifier (for inter-grid kernels). However, "
                    "entry '{1}' raised the following error: {2}.".
                    format(prop_ind+1, arg_type, str(err))), err)

        # Test allowed accesses for fields
        field_disc_accesses = [AccessType.READ, AccessType.WRITE,
                               AccessType.READWRITE]
        field_cont_accesses = [AccessType.READ, AccessType.INC]
        # Convert generic access types to GH_* names for error messages
        api_config = Config.get().api_conf(API)
        rev_access_mapping = api_config.get_reverse_access_mapping()
        # Create a list of allowed accesses for use in error messages
        fld_disc_acc_msg = [rev_access_mapping[acc] for acc in
                            field_disc_accesses]
        fld_cont_acc_msg = [rev_access_mapping[acc] for acc in
                            field_cont_accesses]
        # Joint lists of valid function spaces for continuous fields
        fld_cont_spaces = (FunctionSpace.CONTINUOUS_FUNCTION_SPACES +
                           FunctionSpace.VALID_ANY_SPACE_NAMES)

        # Check accesses for kernels that operate on DoFs
        if operates_on == "dof":
            if self._access_type not in field_disc_accesses:
                raise ParseError(
                    "In the LFRic API, allowed field accesses for a "
                    "kernel that operates on DoFs are {0}, but found "
                    "'{1}' for '{2}' in '{3}'.".
                    format(fld_disc_acc_msg,
                           rev_access_mapping[self._access_type],
                           self._function_space1.lower(), arg_type))
        # Check accesses for kernels that operate on cell-columns or the
        # domain
        elif operates_on in ["cell_column", "domain"]:
            # Fields on discontinuous function spaces
            if (self._function_space1.lower() in
                    FunctionSpace.VALID_DISCONTINUOUS_NAMES and
                    self._access_type not in field_disc_accesses):
                raise ParseError(
                    "In the LFRic API, allowed accesses for fields on "
                    "discontinuous function spaces that are arguments to "
                    "kernels that operate on either cell-columns or the domain"
                    " are {0}, but found '{1}' for '{2}' in '{3}'.".
                    format(fld_disc_acc_msg,
                           rev_access_mapping[self._access_type],
                           self._function_space1.lower(), arg_type))
            # Fields on continuous function spaces
            if self._function_space1.lower() in fld_cont_spaces:
                if operates_on == "domain":
                    raise ParseError(
                        "In the LFRic API, kernels that operate on the domain "
                        "only accept field arguments on discontinuous function"
                        " spaces but found '{0}' in '{1}'".format(
                            self._function_space1.lower(), arg_type))

                if self._access_type not in field_cont_accesses:
                    raise ParseError(
                        "In the LFRic API, allowed accesses for fields on "
                        "continuous function spaces that are arguments to "
                        "kernels that operate on cell-columns are {0}, but "
                        "found '{1}' for '{2}' in '{3}'.".format(
                            fld_cont_acc_msg,
                            rev_access_mapping[self._access_type],
                            self._function_space1.lower(), arg_type))
        # Raise an InternalError for an invalid value of operates-on
        else:
            from psyclone.dynamo0p3 import VALID_ITERATION_SPACES
            raise InternalError(
                "Invalid operates_on '{0}' in the kernel metadata (expected "
                "one of {1}).".format(operates_on, VALID_ITERATION_SPACES))

        # Test allowed accesses for fields that have stencil specification
        if self._stencil:
            if self._access_type != AccessType.READ:
                raise ParseError(
                    "In the LFRic API a field with a stencil access must be "
                    "read-only ('{0}'), but found '{1}' in '{2}'.".
                    format(rev_access_mapping[AccessType.READ],
                           rev_access_mapping[self._access_type], arg_type))
            if operates_on == "domain":
                raise ParseError(
                    "In the LFRic API, kernels that operate on the domain "
                    "are not permitted to have arguments with a stencil "
                    "access but found: '{0}'".format(arg_type))

    def _init_operator(self, arg_type):
        '''
        Validates metadata descriptors for operator arguments and
        initialises operator argument properties accordingly.

        :param arg_type: LFRic API operator argument type.
        :type arg_type: :py:class:`psyclone.expression.FunctionVar`

        :raises InternalError: if argument type other than an operator is \
                               passed in.
        :raises ParseError: if there are not exactly 5 metadata arguments.
        :raises ParseError: if an operator argument has an invalid data \
                            type (the only permitted data type is 'gh_real').
        :raises ParseError: if the function space to- is not one of the \
                            valid function spaces.
        :raises ParseError: if the function space from- is not one of the \
                            valid function spaces.
        :raises ParseError: if the operator argument has an invalid access.

        '''
        # Check whether something other than an operator is passed in
        if self._argument_type not in LFRicArgDescriptor.VALID_OPERATOR_NAMES:
            raise InternalError(
                "Expected an operator argument but got an argument of type "
                "'{0}'.".format(self._argument_type))

        # TODO in #874: Remove support for the old-style operator descriptors
        #               throughout this routine.
        # We expect 5 arguments with the 4th and 5th each being a
        # function space
        nargs_operator = 4 + self._offset
        if self._nargs != nargs_operator:
            raise ParseError(
                "In the LFRic API each 'meta_arg' entry must have {0} "
                "arguments if its first argument is an operator (one "
                "of {1}), but found {2} in '{3}'.".
                format(nargs_operator, LFRicArgDescriptor.VALID_OPERATOR_NAMES,
                       self._nargs, arg_type))

        # Check whether an invalid data type for an operator argument is passed
        # in. The only valid data type for operators in LFRic API is "gh_real".
        # TODO in #874: Remove the support for old-style operator metadata that
        #               prescribes the data type.
        if not self._data_type and self._offset == 0:
            self._data_type = "gh_real"
        if (self._data_type not in
                LFRicArgDescriptor.VALID_OPERATOR_DATA_TYPES):
            raise ParseError(
                "In the LFRic API the permitted data types for operator "
                "arguments are one of {0}, but found '{1}' in '{2}'.".
                format(LFRicArgDescriptor.VALID_OPERATOR_DATA_TYPES,
                       self._data_type, arg_type))

        # Operator arguments need to have valid to- and from- function spaces
        # Check for a valid to- function space
        prop_ind = 2 + self._offset
        if arg_type.args[prop_ind].name not in \
           FunctionSpace.VALID_FUNCTION_SPACE_NAMES:
            raise ParseError(
                "In the LFRic API argument {0} of a 'meta_arg' operator "
                "entry must be a valid function-space name (one of "
                "{1}), but found '{2}' in '{3}'.".
                format(prop_ind+1, FunctionSpace.VALID_FUNCTION_SPACE_NAMES,
                       arg_type.args[prop_ind].name, arg_type))
        self._function_space1 = arg_type.args[prop_ind].name
        # Check for a valid from- function space
        prop_ind = 3 + self._offset
        if arg_type.args[prop_ind].name not in \
           FunctionSpace.VALID_FUNCTION_SPACE_NAMES:
            raise ParseError(
                "In the LFRic API argument {0} of a 'meta_arg' operator "
                "entry must be a valid function-space name (one of "
                "{1}), but found '{2}' in '{3}'.".
                format(prop_ind+1, FunctionSpace.VALID_FUNCTION_SPACE_NAMES,
                       arg_type.args[prop_ind].name, arg_type))
        self._function_space2 = arg_type.args[prop_ind].name

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

    def _init_scalar(self, arg_type):
        '''
        Validates metadata descriptors for scalar arguments and
        initialises scalar argument properties accordingly.

        :param arg_type: LFRic API scalar argument type.
        :type arg_type: :py:class:`psyclone.expression.FunctionVar`

        :raises InternalError: if argument type other than a scalar is \
                               passed in.
        :raises ParseError: if there are not exactly 3 metadata arguments.
        :raises InternalError: if a scalar argument has an invalid data type.
        :raises ParseError: if scalar arguments do not have a read-only or
                            a reduction access.
        :raises ParseError: if a scalar argument that is not a real \
                            scalar has a reduction access.

        '''
        # Check whether something other than a scalar is passed in
        if self._argument_type not in LFRicArgDescriptor.VALID_SCALAR_NAMES:
            raise InternalError(
                "Expected a scalar argument but got an argument of type "
                "'{0}'.".format(arg_type.args[0]))

        # There must be 3 argument descriptors to describe a scalar.
        # TODO in #874: Remove support for the old-style 2 descriptors.
        nargs_scalar = 2 + self._offset
        if self._nargs != nargs_scalar:
            raise ParseError(
                "In the LFRic API each 'meta_arg' entry must have {0} "
                "arguments if its first argument is 'gh_{{r,i}}scalar', but "
                "found {1} in '{2}'.".
                format(nargs_scalar, self._nargs, arg_type))

        # Check whether an invalid data type for a scalar argument is passed
        # in. Valid data types for scalars are valid data types in LFRic API.
        # TODO in #874: Remove the support for old-style scalar metadata that
        #               assigns the data type from the scalar name (the 1st
        #               argument).
        if not self._data_type and self._offset == 0:
            self._data_type = arg_type.args[0].name
            # Translate the old-style argument type into the current one
            self._argument_type = "gh_scalar"
        if (self._data_type not in
                LFRicArgDescriptor.VALID_SCALAR_DATA_TYPES):
            raise InternalError(
                "Expected one of {0} as the scalar data type but got '{1}'.".
                format(LFRicArgDescriptor.VALID_SCALAR_DATA_TYPES,
                       self._data_type))

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
        if self._data_type != "gh_real" and self._access_type in \
           AccessType.get_valid_reduction_modes():
            raise ParseError(
                "In the LFRic API a reduction access '{0}' is only valid "
                "with a real scalar argument, but a scalar argument with "
                "'{1}' data type was found in '{2}'.".
                format(self._access_type.api_specific_name(),
                       self._data_type, arg_type))

        # Scalars don't have vector size
        self._vector_size = 0

    @property
    def data_type(self):
        '''
        :returns: intrinsic Fortran (primitive) type of the argument data.
        :rtype: str

        '''
        return self._data_type

    @property
    def function_space_to(self):
        '''
        Returns the "to" function space for an operator. This is
        the first function space specified in the metadata.

        :returns: "to" function space for an operator.
        :rtype: str

        :raises InternalError: if this is not an operator.

        '''
        if self._argument_type in LFRicArgDescriptor.VALID_OPERATOR_NAMES:
            return self._function_space1
        raise InternalError(
            "In the LFRic API 'function_space_to' only makes sense "
            "for one of {0}, but this is a '{1}'.".
            format(LFRicArgDescriptor.VALID_OPERATOR_NAMES,
                   self._argument_type))

    @property
    def function_space_from(self):
        '''
        Returns the "from" function space for an operator. This is
        the second function space specified in the metadata.

        :returns: "from" function space for an operator.
        :rtype: str

        :raises InternalError: if this is not an operator.

        '''
        if self._argument_type in LFRicArgDescriptor.VALID_OPERATOR_NAMES:
            return self._function_space2
        raise InternalError(
            "In the LFRic API 'function_space_from' only makes sense "
            "for one of {0}, but this is a '{1}'.".
            format(LFRicArgDescriptor.VALID_OPERATOR_NAMES,
                   self._argument_type))

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
        if self._argument_type in LFRicArgDescriptor.VALID_FIELD_NAMES:
            return self._function_space1
        if self._argument_type in LFRicArgDescriptor.VALID_OPERATOR_NAMES:
            return self._function_space2
        if self._argument_type in LFRicArgDescriptor.VALID_SCALAR_NAMES:
            return None
        raise InternalError("Expected a valid argument type but got '{0}'.".
                            format(self._argument_type))

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
        if self._argument_type in LFRicArgDescriptor.VALID_FIELD_NAMES:
            return [self.function_space]
        if self._argument_type in LFRicArgDescriptor.VALID_OPERATOR_NAMES:
            # Return to before from to maintain expected ordering
            return [self.function_space_to, self.function_space_from]
        if self._argument_type in LFRicArgDescriptor.VALID_SCALAR_NAMES:
            return []
        raise InternalError("Expected a valid argument type but got '{0}'.".
                            format(self._argument_type))

    @property
    def vector_size(self):
        '''
        Returns the vector size of the argument. This will be 1 if ``*n``
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
        res += "  argument_type[0]='{0}'".format(self._argument_type)
        if self._vector_size > 1:
            res += "*"+str(self._vector_size)
        res += os.linesep
        res += "  data_type[1]='{0}'".format(self._data_type)\
               + os.linesep
        res += "  access_descriptor[2]='{0}'"\
               .format(self._access_type.api_specific_name())\
               + os.linesep
        if self._argument_type in LFRicArgDescriptor.VALID_FIELD_NAMES:
            res += "  function_space[3]='{0}'".format(self._function_space1) \
                   + os.linesep
        elif self._argument_type in LFRicArgDescriptor.VALID_OPERATOR_NAMES:
            res += "  function_space_to[3]='{0}'".\
                   format(self._function_space1) + os.linesep
            res += "  function_space_from[4]='{0}'".\
                   format(self._function_space2) + os.linesep
        elif self._argument_type in LFRicArgDescriptor.VALID_SCALAR_NAMES:
            pass  # We have nothing to add if we're a scalar
        else:  # We should never get to here
            raise InternalError("Expected a valid argument type but got "
                                "'{0}'.".format(self._argument_type))
        return res


# Documentation utils: The list of module members that we wish AutoAPI to
# generate documentation for (see https://psyclone-ref.readthedocs.io).
__all__ = [
    'LFRicArgDescriptor']
