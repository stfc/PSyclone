# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017-2024, Science and Technology Facilities Council.
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
# Authors R. W. Ford, A. R. Porter, S. Siso and N. Nobre, STFC Daresbury Lab
# Modified I. Kavcic and A. Coughtrie, Met Office
# Modified by J. Henrichs, Bureau of Meteorology

'''
This module contains the LFRicArgDescriptor class and related constants
and properties.
'''

# Imports

import os

from psyclone.configuration import Config
from psyclone.core.access_type import AccessType
# Importing from psyclone.domain.lfric only creates circular
# import with __init__
from psyclone.domain.lfric.lfric_constants import LFRicConstants
from psyclone.errors import InternalError
import psyclone.expression as expr
from psyclone.parse.kernel import Descriptor, get_stencil, get_mesh
from psyclone.parse.utils import ParseError

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
    :param str operates_on: value of operates_on from the parsed kernel \
                            metadata (used for validation).
    :param int metadata_index: position of this argument in the list of \
                               arguments specified in the metadata.

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
    # pylint: disable=too-many-instance-attributes

    # ----------------------------------------------------------------------- #

    def __init__(self, arg_type, operates_on, metadata_index):
        # pylint: disable=too-many-branches, too-many-statements
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

        # Check for the correct argument type descriptor
        if arg_type.name != 'arg_type':
            raise ParseError(
                f"In the LFRic API each 'meta_arg' entry must be of type "
                f"'arg_type', but found '{arg_type.name}'.")

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

        const = LFRicConstants()
        # First check for a valid argument type. It has to be a variable
        # (FunctionVar expression) and have a valid LFRic API argument name.
        if isinstance(argtype, expr.FunctionVar) and argtype.name in \
           const.VALID_ARG_TYPE_NAMES:
            self._argument_type = argtype.name
        else:
            raise ParseError(
                f"In the LFRic API the 1st argument of a 'meta_arg' entry "
                f"should be a valid argument type (one of "
                f"{const.VALID_ARG_TYPE_NAMES}), but found '{argtype}' in "
                f"'{arg_type}'.")

        # Check for a valid vector size in case of a binary
        # operator expression
        if separator:
            self._validate_vector_size(separator, arg_type)

        # Check number of args - we require at least three
        self._nargs = len(arg_type.args)
        min_nargs = 3
        if self._nargs < min_nargs:
            raise ParseError(
                f"In the LFRic API each 'meta_arg' entry must have at least "
                f"{min_nargs} args, but found {self._nargs} in '{arg_type}'.")

        # The 2nd arg is the Fortran primitive type of the argument data
        dtype = arg_type.args[1].name
        if dtype in const.VALID_ARG_DATA_TYPES:
            self._data_type = dtype
        else:
            raise ParseError(
                f"In the LFRic API the 2nd argument of a 'meta_arg' entry "
                f"should be a valid data type (one of "
                f"{const.VALID_ARG_DATA_TYPES}), but found '{dtype}' in "
                f"'{arg_type}'.")

        # The 3rd arg is an access descriptor. Allowed accesses for each
        # argument type are dealt with in the related _init methods.
        # Convert from GH_* names to the generic access type
        api_config = Config.get().api_conf(API)
        access_mapping = api_config.get_access_mapping()
        prop_ind = 2
        try:
            self._access_type = access_mapping[arg_type.args[prop_ind].name]
        except KeyError as err:
            valid_names = api_config.get_valid_accesses_api()
            raise ParseError(
                f"In the LFRic API argument {prop_ind+1} of a 'meta_arg' entry"
                f" must be a valid access descriptor (one of {valid_names}), "
                f"but found '{arg_type.args[prop_ind].name}' in "
                f"'{arg_type}'.") from err

        # Check for the allowed iteration spaces from the parsed kernel
        # metadata
        if operates_on not in const.VALID_ITERATION_SPACES:
            raise InternalError(
                f"Expected operates_on in the kernel metadata to be one of "
                f"{const.VALID_ITERATION_SPACES} but got '{operates_on}'.")

        # FIELD, OPERATOR and SCALAR argument type descriptors and checks
        if self._argument_type in const.VALID_FIELD_NAMES:
            # Validate field arguments
            self._init_field(arg_type, operates_on)

        elif self._argument_type in const.VALID_OPERATOR_NAMES:
            # Validate operator arguments
            self._init_operator(arg_type)

        elif self._argument_type in const.VALID_SCALAR_NAMES:
            # Validate scalar arguments
            self._init_scalar(arg_type)

        else:
            # We should never get to here if the checks are tight enough
            raise InternalError(
                f"Failed argument validation for the 'meta_arg' entry "
                f"'{arg_type}', should not get to here.")

        # Initialise the parent class
        super().__init__(self._access_type, self._function_space1,
                         metadata_index, stencil=self._stencil,
                         mesh=self._mesh,
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
                f"In the LFRic API the 1st argument of a 'meta_arg' "
                f"entry may be a field vector but if so must use '*' as "
                f"the separator in the format 'field*n', but found "
                f"'{separator}' in '{arg_type}'.")

        # Now try to find the vector size for a field vector and return
        # an error if it is not an integer number...
        try:
            vectsize = int(arg_type.args[0].toks[2])
        except TypeError as err:
            raise ParseError(
                f"In the LFRic API, the field vector notation must be in the "
                f"format 'field*n' where 'n' is an integer, but the following "
                f"'{arg_type.args[0].toks[2]}' was found in "
                f"'{arg_type}'.") from err
        # ... or it is less than 2 (1 is the default for all fields)...

        const = LFRicConstants()
        if vectsize < 2:
            raise ParseError(
                f"In the LFRic API the 1st argument of a 'meta_arg' entry may "
                f"be a field vector with format 'field*n' where n is an "
                f"integer > 1. However, found n = {vectsize} in '{arg_type}'.")
        # ... and set the vector size if all checks pass
        self._vector_size = vectsize

        # Check that no other arguments than fields use vector notation
        if self._argument_type not in \
           const.VALID_FIELD_NAMES and self._vector_size:
            raise ParseError(
                f"In the LFRic API, vector notation is only supported for "
                f"{const.VALID_FIELD_NAMES} argument types but found "
                f"'{arg_type.args[0]}'.")

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
        :raises ParseError: if a field argument has an invalid data type.
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
                            does not have a valid access (one of [READ, WRITE,\
                            INC, READINC]).
        :raises ParseError: if the kernel operates on the domain and is \
                            passed a field on a continuous space.
        :raises InternalError: if an invalid value for operates_on is \
                               passed in.
        :raises ParseError: if a field with a stencil access is not read-only.
        :raises ParseError: if a field with a stencil access is passed to a \
                            kernel that operates on the domain.

        '''
        # pylint: disable=too-many-locals, too-many-statements
        # pylint: disable=too-many-branches
        const = LFRicConstants()

        # Check whether something other than a field is passed in
        if self._argument_type not in const.VALID_FIELD_NAMES:
            raise InternalError(
                f"Expected a field argument but got an argument of type "
                f"'{arg_type.args[0]}'.")

        # There must be at least 4 arguments
        nargs_field_min = 4
        if self._nargs < nargs_field_min:
            raise ParseError(
                "In the LFRic API each 'meta_arg' entry must have at least "
                f"{nargs_field_min} arguments if its first argument is of "
                f"{const.VALID_FIELD_NAMES} type, but found {self._nargs} in "
                f"'{arg_type}'.")
        # There must be at most 5 arguments
        nargs_field_max = 5
        if self._nargs > nargs_field_max:
            raise ParseError(
                f"In the LFRic API each 'meta_arg' entry must have at most "
                f"{nargs_field_max} arguments if its first argument is of "
                f"{const.VALID_FIELD_NAMES} type, but found {self._nargs} in "
                f"'{arg_type}'.")

        # Check whether an invalid data type for a field argument is passed in.
        if self._data_type not in const.VALID_FIELD_DATA_TYPES:
            raise ParseError(
                f"In the LFRic API the allowed data types for field arguments "
                f"are one of {const.VALID_FIELD_DATA_TYPES}, but found "
                f"'{self._data_type}' in '{arg_type}'.")

        # The 4th argument must be a valid function-space name
        prop_ind = 3
        if arg_type.args[prop_ind].name not in \
           const.VALID_FUNCTION_SPACE_NAMES:
            raise ParseError(
                f"In the LFRic API argument {prop_ind+1} of a 'meta_arg' "
                f"field entry must be a valid function-space name (one of "
                f"{const.VALID_FUNCTION_SPACE_NAMES}) if its first argument "
                f"is of {const.VALID_FIELD_NAMES} type, but found "
                f"'{arg_type.args[prop_ind].name}' in '{arg_type}'.")
        self._function_space1 = arg_type.args[prop_ind].name

        # The optional 5th argument is either a stencil specification
        # or a mesh identifier (for inter-grid kernels)
        prop_ind = 4
        if self._nargs == nargs_field_max:
            try:
                if "stencil" in str(arg_type.args[prop_ind]):
                    self._stencil = get_stencil(
                        arg_type.args[prop_ind],
                        const.VALID_STENCIL_TYPES)
                elif "mesh" in str(arg_type.args[prop_ind]):
                    self._mesh = get_mesh(arg_type.args[prop_ind],
                                          const.VALID_MESH_TYPES)
                else:
                    raise ParseError("Unrecognised metadata entry")
            except ParseError as err:
                raise ParseError(
                    f"In the LFRic API argument {prop_ind+1} of a 'meta_arg' "
                    f"field entry must be either a valid stencil specification"
                    f" or a mesh identifier (for inter-grid kernels). However,"
                    f" entry '{arg_type}' raised the following error: "
                    f"{err}.") from err

        # Test allowed accesses for fields
        field_disc_accesses = [AccessType.READ, AccessType.WRITE,
                               AccessType.READWRITE]
        # Note that although WRITE is permitted for fields on continuous
        # function spaces, kernels that specify this must guarantee to write
        # the same value to any given shared entity, independent of iteration.
        field_cont_accesses = [AccessType.READ, AccessType.WRITE,
                               AccessType.INC, AccessType.READINC]
        # Convert generic access types to GH_* names for error messages
        api_config = Config.get().api_conf(API)
        rev_access_mapping = api_config.get_reverse_access_mapping()
        # Create a list of allowed accesses for use in error messages
        fld_disc_acc_msg = [rev_access_mapping[acc] for acc in
                            field_disc_accesses]
        fld_cont_acc_msg = [rev_access_mapping[acc] for acc in
                            field_cont_accesses]
        # Joint lists of valid function spaces for continuous fields
        fld_cont_spaces = (const.CONTINUOUS_FUNCTION_SPACES +
                           const.VALID_ANY_SPACE_NAMES)

        # Check accesses for kernels that operate on DoFs
        if operates_on == "dof":
            if self._access_type not in field_disc_accesses:
                raise ParseError(
                    f"In the LFRic API, allowed field accesses for a kernel "
                    f"that operates on DoFs are {fld_disc_acc_msg}, but found "
                    f"'{rev_access_mapping[self._access_type]}' for "
                    f"'{self._function_space1.lower()}' in '{arg_type}'.")
        # Check accesses for kernels that operate on cell-columns or the
        # domain
        elif operates_on in ["cell_column", "domain"]:
            # Fields on discontinuous function spaces
            if (self._function_space1.lower() in
                    const.VALID_DISCONTINUOUS_NAMES and
                    self._access_type not in field_disc_accesses):
                raise ParseError(
                    f"In the LFRic API, allowed accesses for fields on "
                    f"discontinuous function spaces that are arguments to "
                    f"kernels that operate on either cell-columns or the "
                    f"domain are {fld_disc_acc_msg}, but found "
                    f"'{rev_access_mapping[self._access_type]}' for "
                    f"'{self._function_space1.lower()}' in '{arg_type}'.")
            # Fields on continuous function spaces
            if self._function_space1.lower() in fld_cont_spaces:
                if operates_on == "domain":
                    raise ParseError(
                        f"In the LFRic API, kernels that operate on the domain"
                        f" only accept field arguments on discontinuous "
                        f"function spaces but found "
                        f"'{self._function_space1.lower()}' in '{arg_type}'")

                if self._access_type not in field_cont_accesses:
                    raise ParseError(
                        f"In the LFRic API, allowed accesses for fields on "
                        f"continuous function spaces that are arguments to "
                        f"kernels that operate on cell-columns are "
                        f"{fld_cont_acc_msg}, but found "
                        f"'{rev_access_mapping[self._access_type]}' for "
                        f"'{self._function_space1.lower()}' in '{arg_type}'.")
        # Raise an InternalError for an invalid value of operates-on
        else:
            raise InternalError(
                f"Invalid operates_on '{operates_on}' in the kernel metadata "
                f"(expected one of {const.VALID_ITERATION_SPACES}).")

        # Test allowed accesses for fields that have stencil specification
        if self._stencil:
            if self._access_type != AccessType.READ:
                raise ParseError(
                    f"In the LFRic API a field with a stencil access must be "
                    f"read-only ('{rev_access_mapping[AccessType.READ]}'), "
                    f"but found '{rev_access_mapping[self._access_type]}' in "
                    f"'{arg_type}'.")
            if operates_on == "domain":
                raise ParseError(
                    f"In the LFRic API, kernels that operate on the domain "
                    f"are not permitted to have arguments with a stencil "
                    f"access but found: '{arg_type}'")

    def _init_operator(self, arg_type):
        '''
        Validates metadata descriptors for operator arguments and
        initialises operator argument properties accordingly.

        :param arg_type: LFRic API operator argument type.
        :type arg_type: :py:class:`psyclone.expression.FunctionVar`

        :raises InternalError: if argument type other than an operator is \
                               passed in.
        :raises ParseError: if there are not exactly 5 metadata arguments.
        :raises ParseError: if an operator argument has an invalid data type.
        :raises ParseError: if the function space to- is not one of the \
                            valid function spaces.
        :raises ParseError: if the function space from- is not one of the \
                            valid function spaces.
        :raises ParseError: if the operator argument has an invalid access.

        '''
        const = LFRicConstants()
        # Check whether something other than an operator is passed in
        if self._argument_type not in const.VALID_OPERATOR_NAMES:
            raise InternalError(
                f"Expected an operator argument but got an argument of type "
                f"'{self._argument_type}'.")

        # We expect 5 arguments with the 4th and 5th each being a
        # function space
        nargs_operator = 5
        if self._nargs != nargs_operator:
            raise ParseError(
                f"In the LFRic API each 'meta_arg' entry must have "
                f"{nargs_operator} arguments if its first argument is an "
                f"operator (one of {const.VALID_OPERATOR_NAMES}), but found "
                f"{self._nargs} in '{arg_type}'.")

        # Check whether an invalid data type for an operator argument is passed
        # in. The only valid data type for operators in LFRic API is "gh_real".
        if self._data_type not in const.VALID_OPERATOR_DATA_TYPES:
            raise ParseError(
                f"In the LFRic API the allowed data types for operator "
                f"arguments are one of {const.VALID_OPERATOR_DATA_TYPES}, but "
                f"found '{self._data_type}' in '{arg_type}'.")

        # Operator arguments need to have valid to- and from- function spaces
        # Check for a valid to- function space
        prop_ind = 3
        if arg_type.args[prop_ind].name not in \
           const.VALID_FUNCTION_SPACE_NAMES:
            raise ParseError(
                f"In the LFRic API argument {prop_ind+1} of a 'meta_arg' "
                f"operator entry must be a valid function-space name (one of "
                f"{const.VALID_FUNCTION_SPACE_NAMES}), but found "
                f"'{arg_type.args[prop_ind].name}' in '{arg_type}'.")
        self._function_space1 = arg_type.args[prop_ind].name
        # Check for a valid from- function space
        prop_ind = 4
        if arg_type.args[prop_ind].name not in \
           const.VALID_FUNCTION_SPACE_NAMES:
            raise ParseError(
                f"In the LFRic API argument {prop_ind+1} of a 'meta_arg' "
                f"operator entry must be a valid function-space name (one of "
                f"{const.VALID_FUNCTION_SPACE_NAMES}), but found "
                f"'{arg_type.args[prop_ind].name}' in '{arg_type}'.")
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
                f"In the LFRic API, allowed accesses for operators are "
                f"{op_acc_msg} because they behave as discontinuous "
                f"quantities, but found "
                f"'{rev_access_mapping[self._access_type]}' in '{arg_type}'.")

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
        const = LFRicConstants()
        # Check whether something other than a scalar is passed in
        if self._argument_type not in const.VALID_SCALAR_NAMES:
            raise InternalError(
                f"Expected a scalar argument but got an argument of type "
                f"'{arg_type.args[0]}'.")

        # There must be 3 argument descriptors to describe a scalar.
        nargs_scalar = 3
        if self._nargs != nargs_scalar:
            raise ParseError(
                f"In the LFRic API each 'meta_arg' entry must have "
                f"{nargs_scalar} arguments if its first argument is "
                f"'gh_scalar', but found {self._nargs} in '{arg_type}'.")

        # Check whether an invalid data type for a scalar argument is passed
        # in. Valid data types for scalars are valid data types in LFRic API.
        if self._data_type not in const.VALID_SCALAR_DATA_TYPES:
            raise InternalError(
                f"Expected one of {const.VALID_SCALAR_DATA_TYPES} as the "
                f"scalar data type but got '{self._data_type}'.")

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
                f"In the LFRic API scalar arguments must have read-only "
                f"('gh_read') or a reduction {valid_reductions} access but "
                f"found '{api_specific_name}' in '{arg_type}'.")
        # Reduction access is currently only valid for real scalar arguments
        if self._data_type != "gh_real" and self._access_type in \
           AccessType.get_valid_reduction_modes():
            raise ParseError(
                f"In the LFRic API a reduction access "
                f"'{self._access_type.api_specific_name()}' is only valid "
                f"with a real scalar argument, but a scalar argument with "
                f"'{self._data_type}' data type was found in '{arg_type}'.")

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
        const = LFRicConstants()
        if self._argument_type in const.VALID_OPERATOR_NAMES:
            return self._function_space1
        raise InternalError(
            f"In the LFRic API 'function_space_to' only makes sense for one "
            f"of {const.VALID_OPERATOR_NAMES}, but this is a "
            f"'{self._argument_type}'.")

    @property
    def function_space_from(self):
        '''
        Returns the "from" function space for an operator. This is
        the second function space specified in the metadata.

        :returns: "from" function space for an operator.
        :rtype: str

        :raises InternalError: if this is not an operator.

        '''
        const = LFRicConstants()
        if self._argument_type in const.VALID_OPERATOR_NAMES:
            return self._function_space2
        raise InternalError(
            f"In the LFRic API 'function_space_from' only makes sense for one "
            f"of {const.VALID_OPERATOR_NAMES}, but this is a "
            f"'{self._argument_type}'.")

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
        const = LFRicConstants()
        if self._argument_type in const.VALID_FIELD_NAMES:
            return self._function_space1
        if self._argument_type in const.VALID_OPERATOR_NAMES:
            return self._function_space2
        if self._argument_type in const.VALID_SCALAR_NAMES:
            return None
        raise InternalError(f"Expected a valid argument type but got "
                            f"'{self._argument_type}'.")

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
        const = LFRicConstants()
        if self._argument_type in const.VALID_FIELD_NAMES:
            return [self.function_space]
        if self._argument_type in const.VALID_OPERATOR_NAMES:
            # Return to before from to maintain expected ordering
            return [self.function_space_to, self.function_space_from]
        if self._argument_type in const.VALID_SCALAR_NAMES:
            return []
        raise InternalError(f"Expected a valid argument type but got "
                            f"'{self._argument_type}'.")

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
        const = LFRicConstants()
        res = "LFRicArgDescriptor object" + os.linesep
        res += f"  argument_type[0]='{self._argument_type}'"
        if self._vector_size > 1:
            res += "*" + str(self._vector_size)
        res += os.linesep
        res += f"  data_type[1]='{self._data_type}'" + os.linesep
        res += (f"  access_descriptor[2]="
                f"'{self._access_type.api_specific_name()}'"
                + os.linesep)
        if self._argument_type in const.VALID_FIELD_NAMES:
            res += (f"  function_space[3]='{self._function_space1}'"
                    + os.linesep)
        elif self._argument_type in const.VALID_OPERATOR_NAMES:
            res += (f"  function_space_to[3]='{self._function_space1}'"
                    + os.linesep)
            res += (f"  function_space_from[4]='{self._function_space2}'"
                    + os.linesep)
        elif self._argument_type in const.VALID_SCALAR_NAMES:
            pass  # We have nothing to add if we're a scalar
        else:  # We should never get to here
            raise InternalError(f"Expected a valid argument type but got "
                                f"'{self._argument_type}'.")
        return res


# Documentation utils: The list of module members that we wish AutoAPI to
# generate documentation for (see https://psyclone-ref.readthedocs.io).
__all__ = [
    'LFRicArgDescriptor']
