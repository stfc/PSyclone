# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017, Science and Technology Facilities Council
# However, it has been created with the help of the GungHo Consortium,
# whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
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
# Author R. Ford STFC Daresbury Lab

''' This module implements the PSyclone Dynamo 0.3 API by 1)
    specialising the required base classes in parser.py (Descriptor,
    KernelType) and adding a new class (DynFuncDescriptor03) to
    capture function descriptor metadata and 2) specialising the
    required base classes in psyGen.py (PSy, Invokes, Invoke, Schedule,
    Loop, Kern, Inf, Arguments and Argument). '''

# imports
import os
from parse import Descriptor, KernelType, ParseError
import expression as expr
import fparser
from psyGen import PSy, Invokes, Invoke, Schedule, Loop, Kern, \
    Arguments, KernelArgument, NameSpaceFactory, GenerationError, \
    FieldNotFoundError, HaloExchange, GlobalSum, FORTRAN_INTENT_NAMES
import psyGen
import config

# first section : Parser specialisations and classes

# constants
DISCONTINUOUS_FUNCTION_SPACES = ["w3"]
CONTINUOUS_FUNCTION_SPACES = ["w0", "w1", "w2", "wtheta", "w2h", "w2v"]
VALID_FUNCTION_SPACES = DISCONTINUOUS_FUNCTION_SPACES + \
    CONTINUOUS_FUNCTION_SPACES

VALID_ANY_SPACE_NAMES = ["any_space_1", "any_space_2", "any_space_3",
                         "any_space_4", "any_space_5", "any_space_6",
                         "any_space_7", "any_space_8", "any_space_9"]

VALID_FUNCTION_SPACE_NAMES = VALID_FUNCTION_SPACES + VALID_ANY_SPACE_NAMES

VALID_EVALUATOR_NAMES = ["gh_basis", "gh_diff_basis"]
VALID_METAFUNC_NAMES = VALID_EVALUATOR_NAMES + ["gh_orientation"]

VALID_EVALUATOR_SHAPES = ["quadrature_xyoz", "evaluator_xyz"]

VALID_SCALAR_NAMES = ["gh_real", "gh_integer"]
VALID_OPERATOR_NAMES = ["gh_operator", "gh_columnwise_operator"]
VALID_ARG_TYPE_NAMES = ["gh_field"] + VALID_OPERATOR_NAMES + \
                       VALID_SCALAR_NAMES

VALID_REDUCTION_NAMES = ["gh_sum"]
# List of all access types that involve writing to an argument
# in some form
GH_WRITE_ACCESSES = ["gh_write", "gh_inc"] + VALID_REDUCTION_NAMES
# List of all access types that only involve reading an argument
GH_READ_ACCESSES = ["gh_read"]
VALID_ACCESS_DESCRIPTOR_NAMES = GH_READ_ACCESSES + GH_WRITE_ACCESSES


VALID_STENCIL_TYPES = ["x1d", "y1d", "xory1d", "cross", "region"]
# Note, can't use VALID_STENCIL_DIRECTIONS at all locations in this
# file as it causes failures with py.test 2.8.7. Therefore some parts
# of the code do not use the VALID_STENCIL_DIRECTIONS variable.
VALID_STENCIL_DIRECTIONS = ["x_direction", "y_direction"]
# Note, xory1d does not have a direct mapping in STENCIL_MAPPING as it
# indicates either x1d or y1d.
# Note, the LFRic infrastructure currently does not have 'region' as
# an option in stencil_dofmap_mod.F90 so it is not included in
# STENCIL_MAPPING.
STENCIL_MAPPING = {"x1d": "STENCIL_1DX", "y1d": "STENCIL_1DY",
                   "cross": "STENCIL_CROSS"}

VALID_LOOP_BOUNDS_NAMES = ["start", "inner", "edge", "halo", "ncolour",
                           "ncolours", "cells", "dofs"]

# The mapping from meta-data strings to field-access types
# used in this API.
FIELD_ACCESS_MAP = {"write": "gh_write", "read": "gh_read",
                    "inc": "gh_inc"}

# Valid Dynamo loop types. The default is "" which is over cells (in the
# horizontal plane).
VALID_LOOP_TYPES = ["dofs", "colours", "colour", ""]

# Mappings used by non-API-Specific code in psyGen
psyGen.MAPPING_REDUCTIONS = {"sum": "gh_sum"}
psyGen.MAPPING_SCALARS = {"iscalar": "gh_integer", "rscalar": "gh_real"}
psyGen.MAPPING_ACCESSES = {"inc": "gh_inc", "write": "gh_write",
                           "read": "gh_read"}
psyGen.VALID_ARG_TYPE_NAMES = VALID_ARG_TYPE_NAMES
psyGen.VALID_ACCESS_DESCRIPTOR_NAMES = VALID_ACCESS_DESCRIPTOR_NAMES

# Functions


def get_fs_map_name(function_space):
    ''' Returns a dofmap name for the supplied FunctionSpace. '''
    return "map_" + function_space.mangled_name


def get_cbanded_map_name(function_space):
    ''' Returns the name of a column-banded dofmap for the supplied
    FunctionSpace. '''
    return "cbanded_map_" + function_space.mangled_name


def get_cma_indirection_map_name(function_space):
    ''' Returns the name of a CMA indirection dofmap for the supplied
    FunctionSpace. '''
    return "cma_indirection_map_" + function_space.mangled_name


def get_fs_ndf_name(function_space):
    ''' Returns a ndf name for this FunctionSpace object. '''
    return "ndf_" + function_space.mangled_name


def get_fs_undf_name(function_space):
    ''' Returns a undf name for this FunctionSpace object. '''
    return "undf_" + function_space.mangled_name


def get_fs_orientation_name(function_space):
    ''' Returns an orientation name for a function space with the
    supplied name '''
    return "orientation" + "_" + function_space.mangled_name


def get_fs_basis_name(function_space):
    ''' Returns a name for the basis function on this FunctionSpace.
    The name is unique to the function space, it is not the
    raw metadata value. '''
    return "basis" + "_" + function_space.mangled_name


def get_fs_diff_basis_name(function_space):
    ''' Returns a name for the differential basis function on the
    supplied FunctionSpace. The name is unique to the function space, it
    is not the raw metadata value. '''
    return "diff_basis" + "_" + function_space.mangled_name


def get_fs_operator_name(operator_name, function_space):
    ''' Returns the name of the specified operator for the supplied
    FunctionSpace. The name is unique to the function space, it
    is not the raw metadata value. '''
    if operator_name == "gh_orientation":
        return get_fs_orientation_name(function_space)
    elif operator_name == "gh_basis":
        return get_fs_basis_name(function_space)
    elif operator_name == "gh_diff_basis":
        return get_fs_diff_basis_name(function_space)
    else:
        raise GenerationError(
            "Unsupported name '{0}' found. Expected one of {1}".
            format(operator_name, VALID_METAFUNC_NAMES))


def mangle_fs_name(args, fs_name):
    ''' Construct the mangled version of a function-space name given
    a list of kernel arguments '''
    if fs_name not in VALID_ANY_SPACE_NAMES:
        # If the supplied function-space name is not any any-space then
        # we don't need to mangle the name
        return fs_name
    for arg in args:
        for fspace in arg.function_spaces:
            if fspace and fspace.orig_name.lower() == fs_name.lower():
                return fs_name.lower() + "_" + arg.name
    raise FieldNotFoundError("No kernel argument found for function space "
                             "'{0}'".format(fs_name))


def field_on_space(function_space, arguments):
    ''' Returns the corresponding argument if the supplied list of arguments
    contains a field that exists on the specified space. Otherwise
    returns None.'''
    if function_space.mangled_name in arguments.unique_fs_names:
        for arg in arguments.args:
            # First, test that arg is a field as some argument objects won't
            # have function spaces, e.g. scalars
            if arg.type == "gh_field" and \
               arg.function_space.orig_name == function_space.orig_name:
                return arg
    return None


def cma_on_space(function_space, arguments):
    ''' Returns the corresponding argument if the supplied list of arguments
    contains a cma operator that maps to/from the specified space. Otherwise
    returns None.'''
    if function_space.mangled_name in arguments.unique_fs_names:
        for arg in arguments.args:
            # First, test that arg is a CMA op as some argument objects won't
            # have function spaces, e.g. scalars
            if arg.type == "gh_columnwise_operator" and \
               function_space.orig_name in [arg.function_space_to.orig_name,
                                            arg.function_space_from.orig_name]:
                return arg
    return None

# Classes


class FunctionSpace(object):
    ''' Manages the name of a function space. If it is an any-space
    then its name is mangled such that it is unique within the scope
    of an Invoke '''

    def __init__(self, name, kernel_args):
        self._orig_name = name
        self._kernel_args = kernel_args
        if self._orig_name not in VALID_ANY_SPACE_NAMES:
            # We only need to name-mangle any-space spaces
            self._mangled_name = self._orig_name
        else:
            # We do not construct the name-mangled name at this point
            # as the full list of kernel arguments may still be under
            # construction.
            self._mangled_name = None

    @property
    def orig_name(self):
        ''' Returns the name of this function space as declared in the
        kernel meta-data '''
        return self._orig_name

    @property
    def mangled_name(self):
        ''' Returns the mangled name of this function space such that
        it is unique within the scope of an invoke. If the mangled
        name has not been generated then we do that the first time we're
        called. '''
        if self._mangled_name:
            return self._mangled_name
        else:
            # Cannot use kernel_args.field_on_space(x) here because that
            # routine itself requires the mangled name in order to identify
            # whether the space is present in the kernel call.
            self._mangled_name = mangle_fs_name(self._kernel_args.args,
                                                self._orig_name)
            return self._mangled_name


class DynFuncDescriptor03(object):
    ''' The Dynamo 0.3 API includes a function-space descriptor as
    well as an argument descriptor which is not supported by the base
    classes. This class captures the information specified in a
    function-space descriptor. '''

    def __init__(self, func_type):
        self._func_type = func_type
        if func_type.name != 'func_type':
            raise ParseError(
                "In the dynamo0.3 API each meta_func entry must be of type "
                "'func_type' but found '{0}'".format(func_type.name))
        if len(func_type.args) < 2:
            raise ParseError(
                "In the dynamo0.3 API each meta_func entry must have at "
                "least 2 args, but found '{0}'".format(len(func_type.args)))
        self._operator_names = []
        for idx, arg in enumerate(func_type.args):
            if idx == 0:  # first func_type arg
                if arg.name not in VALID_FUNCTION_SPACE_NAMES:
                    raise ParseError(
                        "In the dynamo0p3 API the 1st argument of a "
                        "meta_func entry should be a valid function space "
                        "name (one of {0}), but found '{1}' in '{2}'".format(
                            VALID_FUNCTION_SPACE_NAMES, arg.name, func_type))
                self._function_space_name = arg.name
            else:  # subsequent func_type args
                if arg.name not in VALID_METAFUNC_NAMES:
                    raise ParseError(
                        "In the dynamo0.3 API, the 2nd argument and all "
                        "subsequent arguments of a meta_func entry should "
                        "be one of {0}, but found "
                        "'{1}' in '{2}".format(VALID_METAFUNC_NAMES,
                                               arg.name, func_type))
                if arg.name in self._operator_names:
                    raise ParseError(
                        "In the dynamo0.3 API, it is an error to specify an "
                        "operator name more than once in a meta_func entry, "
                        "but '{0}' is replicated in '{1}".format(arg.name,
                                                                 func_type))
                self._operator_names.append(arg.name)
        self._name = func_type.name

    @property
    def function_space_name(self):
        ''' Returns the name of the descriptors function space '''
        return self._function_space_name

    @property
    def operator_names(self):
        ''' Returns a list of operators that are associated with this
        descriptors function space '''
        return self._operator_names

    def __repr__(self):
        return "DynFuncDescriptor03({0})".format(self._func_type)

    def __str__(self):
        res = "DynFuncDescriptor03 object" + os.linesep
        res += "  name='{0}'".format(self._name) + os.linesep
        res += "  nargs={0}".format(len(self._operator_names)+1) + os.linesep
        res += "  function_space_name[{0}] = '{1}'".\
               format(0, self._function_space_name) + os.linesep
        for idx, arg in enumerate(self._operator_names):
            res += "  operator_name[{0}] = '{1}'".format(idx+1, arg) + \
                   os.linesep
        return res


class DynArgDescriptor03(Descriptor):
    ''' This class captures the information specified in an argument
    descriptor.'''

    def __init__(self, arg_type):
        self._arg_type = arg_type
        if arg_type.name != 'arg_type':
            raise ParseError(
                "In the dynamo0.3 API each meta_arg entry must be of type "
                "'arg_type', but found '{0}'".format(arg_type.name))
        # we require at least 2 args
        if len(arg_type.args) < 2:
            raise ParseError(
                "In the dynamo0.3 API each meta_arg entry must have at least "
                "2 args, but found '{0}'".format(len(arg_type.args)))
        # the first arg is the type of field, possibly with a *n appended
        self._vector_size = 1
        if isinstance(arg_type.args[0], expr.BinaryOperator):
            # we expect 'field_type * n' to have been specified
            self._type = arg_type.args[0].toks[0].name
            operator = arg_type.args[0].toks[1]
            try:
                self._vector_size = int(arg_type.args[0].toks[2])
            except TypeError:
                raise ParseError(
                    "In the dynamo0.3 API vector notation expects the format "
                    "(field*n) where n is an integer, but the following was "
                    "found '{0}' in '{1}'.".
                    format(str(arg_type.args[0].toks[2]), arg_type))
            if self._type not in VALID_ARG_TYPE_NAMES:
                raise ParseError(
                    "In the dynamo0.3 API the 1st argument of a meta_arg "
                    "entry should be a valid argument type (one of {0}), but "
                    "found '{1}' in '{2}'".format(VALID_ARG_TYPE_NAMES,
                                                  self._type, arg_type))
            if self._type in VALID_SCALAR_NAMES and self._vector_size > 1:
                raise ParseError(
                    "In the dynamo0.3 API vector notation is not supported "
                    "for scalar arguments (found '{0}')".
                    format(arg_type.args[0]))
            if not operator == "*":
                raise ParseError(
                    "In the dynamo0.3 API the 1st argument of a meta_arg "
                    "entry may be a vector but if so must use '*' as the "
                    "separator in the format (field*n), but found '{0}' in "
                    "'{1}'".format(operator, arg_type))
            if not self._vector_size > 1:
                raise ParseError(
                    "In the dynamo0.3 API the 1st argument of a meta_arg "
                    "entry may be a vector but if so must contain a valid "
                    "integer vector size in the format (field*n where n>1), "
                    "but found '{0}' in '{1}'".format(self._vector_size,
                                                      arg_type))

        elif isinstance(arg_type.args[0], expr.FunctionVar):
            # we expect 'field_type' to have been specified
            if arg_type.args[0].name not in VALID_ARG_TYPE_NAMES:
                raise ParseError(
                    "In the dynamo0.3 API the 1st argument of a "
                    "meta_arg entry should be a valid argument type (one of "
                    "{0}), but found '{1}' in '{2}'".
                    format(VALID_ARG_TYPE_NAMES, arg_type.args[0].name,
                           arg_type))
            self._type = arg_type.args[0].name
        else:
            raise ParseError(
                "Internal error in DynArgDescriptor03.__init__, (1) should "
                "not get to here")
        # The 2nd arg is an access descriptor
        if arg_type.args[1].name not in VALID_ACCESS_DESCRIPTOR_NAMES:
            raise ParseError(
                "In the dynamo0.3 API the 2nd argument of a meta_arg entry "
                "must be a valid access descriptor (one of {0}), but found "
                "'{1}' in '{2}'".format(VALID_ACCESS_DESCRIPTOR_NAMES,
                                        arg_type.args[1].name, arg_type))
        self._access_descriptor = arg_type.args[1]
        # Reduction access descriptors are only valid for scalar arguments
        if self._type not in VALID_SCALAR_NAMES and \
           self._access_descriptor.name in VALID_REDUCTION_NAMES:
            raise ParseError(
                "In the dynamo0.3 API a reduction access '{0}' is only valid "
                "with a scalar argument, but '{1}' was found".
                format(self._access_descriptor.name, self._type))
        # Scalars can only be read_only or reductions
        if self._type in VALID_SCALAR_NAMES:
            if self._access_descriptor.name not in ["gh_read"] + \
               VALID_REDUCTION_NAMES:
                raise ParseError(
                    "In the dynamo0.3 API scalar arguments must be "
                    "read-only (gh_read) or a reduction ({0}) but found "
                    "'{1}' in '{2}'".format(VALID_REDUCTION_NAMES,
                                            self._access_descriptor.name,
                                            arg_type))
        stencil = None
        if self._type == "gh_field":
            if len(arg_type.args) < 3:
                raise ParseError(
                    "In the dynamo0.3 API each meta_arg entry must have at "
                    "least 3 arguments if its first argument is gh_field, but "
                    "found {0} in '{1}'".format(len(arg_type.args), arg_type)
                    )
            # There must be at most 4 arguments.
            if len(arg_type.args) > 4:
                raise ParseError(
                    "In the dynamo0.3 API each meta_arg entry must have at "
                    "most 4 arguments if its first argument is gh_field, but "
                    "found {0} in '{1}'".format(len(arg_type.args), arg_type))
            # The 3rd argument must be a function space name
            if arg_type.args[2].name not in VALID_FUNCTION_SPACE_NAMES:
                raise ParseError(
                    "In the dynamo0.3 API the 3rd argument of a meta_arg "
                    "entry must be a valid function space name if its first "
                    "argument is gh_field (one of {0}), but found '{1}' in "
                    "'{2}".format(VALID_FUNCTION_SPACE_NAMES,
                                  arg_type.args[2].name, arg_type))
            self._function_space1 = arg_type.args[2].name

            # The optional 4th argument is a stencil specification
            if len(arg_type.args) == 4:
                try:
                    stencil = self._get_stencil(arg_type.args[3],
                                                VALID_STENCIL_TYPES)
                except ParseError as err:
                    raise ParseError(
                        "In the dynamo0.3 API the 4th argument of a meta_arg "
                        "entry must be a valid stencil specification but "
                        "entry '{0}' raised the following error:".
                        format(arg_type) + str(err))

            if self._function_space1.lower() in DISCONTINUOUS_FUNCTION_SPACES \
               and self._access_descriptor.name.lower() == "gh_inc":
                raise ParseError(
                    "It does not make sense for a quantity on a discontinuous "
                    "space ({0}) to have a 'gh_inc' access".
                    format(self._function_space1.lower()))
            if stencil and self._access_descriptor.name.lower() != "gh_read":
                raise ParseError("a stencil must be read only so its access"
                                 "should be gh_read")

        elif self._type in VALID_OPERATOR_NAMES:
            # we expect 4 arguments with the 3rd and 4th each being a
            # function space
            if len(arg_type.args) != 4:
                raise ParseError(
                    "In the dynamo0.3 API each meta_arg entry must have 4 "
                    "arguments if its first argument is gh_operator or "
                    "gh_columnwise_operator, but "
                    "found {0} in '{1}'".format(len(arg_type.args), arg_type))
            if arg_type.args[2].name not in VALID_FUNCTION_SPACE_NAMES:
                raise ParseError(
                    "In the dynamo0.3 API the 3rd argument of a meta_arg "
                    "entry must be a valid function space name (one of {0}), "
                    "but found '{1}' in '{2}".
                    format(VALID_FUNCTION_SPACE_NAMES, arg_type.args[2].name,
                           arg_type))
            self._function_space1 = arg_type.args[2].name
            if arg_type.args[3].name not in VALID_FUNCTION_SPACE_NAMES:
                raise ParseError(
                    "In the dynamo0.3 API the 4th argument of a meta_arg "
                    "entry must be a valid function space name (one of {0}), "
                    "but found '{1}' in '{2}".
                    format(VALID_FUNCTION_SPACE_NAMES, arg_type.args[2].name,
                           arg_type))
            self._function_space2 = arg_type.args[3].name
        elif self._type in VALID_SCALAR_NAMES:
            if len(arg_type.args) != 2:
                raise ParseError(
                    "In the dynamo0.3 API each meta_arg entry must have 2 "
                    "arguments if its first argument is gh_{{r,i}}scalar, but "
                    "found {0} in '{1}'".format(len(arg_type.args), arg_type))
            # Scalars don't have a function space
            self._function_space1 = None
        else:  # we should never get to here
            raise ParseError(
                "Internal error in DynArgDescriptor03.__init__, (2) should "
                "not get to here")
        Descriptor.__init__(self, self._access_descriptor.name,
                            self._function_space1, stencil=stencil)

    @property
    def function_space_to(self):
        ''' Return the "to" function space for a gh_operator. This is
        the first function space specified in the metadata. Raise an
        error if this is not an operator. '''
        if self._type in VALID_OPERATOR_NAMES:
            return self._function_space1
        else:
            raise RuntimeError(
                "function_space_to only makes sense for one of {0}, but "
                "this is a '{1}'".format(VALID_OPERATOR_NAMES, self._type))

    @property
    def function_space_from(self):
        ''' Return the "from" function space for a gh_operator. This is
        the second function space specified in the metadata. Raise an
        error if this is not an operator. '''
        if self._type in VALID_OPERATOR_NAMES:
            return self._function_space2
        else:
            raise RuntimeError(
                "function_space_from only makes sense for one of {0}, but this"
                " is a '{1}'".format(VALID_OPERATOR_NAMES, self._type))

    @property
    def function_space(self):
        ''' Return the function space name that this instance operates
        on. In the case of a gh_operator/gh_columnwise_operator, where there
        are 2 function spaces, return function_space_from. '''
        if self._type == "gh_field":
            return self._function_space1
        elif self._type in VALID_OPERATOR_NAMES:
            return self._function_space2
        elif self._type in VALID_SCALAR_NAMES:
            return None
        else:
            raise RuntimeError(
                "Internal error, DynArgDescriptor03:function_space(), should "
                "not get to here.")

    @property
    def function_spaces(self):
        ''' Return the function space names that this instance operates
        on as a list. In the case of a gh_operator, where there are 2 function
        spaces, we return both. '''
        if self._type == "gh_field":
            return [self.function_space]
        elif self._type in VALID_OPERATOR_NAMES:
            # return to before from to maintain expected ordering
            return [self.function_space_to, self.function_space_from]
        elif self._type in VALID_SCALAR_NAMES:
            return []
        else:
            raise RuntimeError(
                "Internal error, DynArgDescriptor03:function_spaces(), should "
                "not get to here.")

    @property
    def vector_size(self):
        ''' Returns the vector size of the argument. This will be 1 if *n
        has not been specified. '''
        return self._vector_size

    @property
    def type(self):
        ''' returns the type of the argument (gh_field, gh_operator, ...). '''
        return self._type

    def __str__(self):
        res = "DynArgDescriptor03 object" + os.linesep
        res += "  argument_type[0]='{0}'".format(self._type)
        if self._vector_size > 1:
            res += "*"+str(self._vector_size)
        res += os.linesep
        res += "  access_descriptor[1]='{0}'".format(self._access_descriptor) \
               + os.linesep
        if self._type == "gh_field":
            res += "  function_space[2]='{0}'".format(self._function_space1) \
                   + os.linesep
        elif self._type in VALID_OPERATOR_NAMES:
            res += "  function_space_to[2]='{0}'".\
                   format(self._function_space1) + os.linesep
            res += "  function_space_from[3]='{0}'".\
                   format(self._function_space2) + os.linesep
        elif self._type in VALID_SCALAR_NAMES:
            pass  # we have nothing to add if we're a scalar
        else:  # we should never get to here
            raise ParseError("Internal error in DynArgDescriptor03.__str__")
        return res

    def __repr__(self):
        return "DynArgDescriptor03({0})".format(self._arg_type)


class DynKernMetadata(KernelType):
    ''' Captures the Kernel subroutine code and metadata describing
    the subroutine for the Dynamo 0.3 API. '''

    def __init__(self, ast, name=None):
        KernelType.__init__(self, ast, name=name)

        # The type of CMA operation this kernel performs (or None if
        # no CMA operators are involved)
        self._cma_operation = None

        # Query the meta-data for the evaluator shape (only required if
        # kernel uses quadrature or an evaluator). If it is not
        # present then eval_shape will be None.
        self._eval_shape = self.get_integer_variable('evaluator_shape')

        # parse the arg_type metadata
        self._arg_descriptors = []
        for arg_type in self._inits:
            self._arg_descriptors.append(DynArgDescriptor03(arg_type))
        # parse the func_type metadata if it exists
        found = False
        for line in self._ktype.content:
            if isinstance(line, fparser.typedecl_statements.Type):
                for entry in line.selector:
                    if entry == "func_type":
                        if line.entity_decls[0].split()[0].split("(")[0] == \
                                "meta_funcs":
                            found = True
                            break
        if not found:
            func_types = []
        else:
            # use the base class method to extract the information
            func_types = self.getkerneldescriptors(self._ktype,
                                                   var_name="meta_funcs")
        self._func_descriptors = []
        # populate a list of function descriptor objects which we
        # return via the func_descriptors method.
        arg_fs_names = []
        for descriptor in self._arg_descriptors:
            arg_fs_names.extend(descriptor.function_spaces)
        used_fs_names = []
        need_evaluator = False
        for func_type in func_types:
            descriptor = DynFuncDescriptor03(func_type)
            fs_name = descriptor.function_space_name
            # check that function space names in meta_funcs are specified in
            # meta_args
            if fs_name not in arg_fs_names:
                raise ParseError(
                    "In the dynamo0.3 API all function spaces specified in "
                    "meta_funcs must exist in meta_args, but '{0}' breaks "
                    "this rule in ...\n'{1}'.".
                    format(fs_name, self._ktype.content))
            if fs_name not in used_fs_names:
                used_fs_names.append(fs_name)
            else:
                raise ParseError(
                    "In the dynamo0.3 API function spaces specified in "
                    "meta_funcs must be unique, but '{0}' is replicated."
                    .format(fs_name))

            # Check that a valid evaluator shape has been specified if
            # this function space requires a basis or differential basis
            for name in descriptor.operator_names:
                if name in VALID_EVALUATOR_NAMES:
                    need_evaluator = True
                    if not self._eval_shape:
                        raise ParseError(
                            "In the dynamo0.3 API any kernel requiring "
                            "quadrature or an evaluator ({0}) must also "
                            "supply the shape of that evaluator by setting "
                            "'evaluator_shape' in the kernel meta-data but "
                            "this is missing for kernel '{1}'".
                            format(VALID_EVALUATOR_NAMES, self.name))
                    if self._eval_shape not in VALID_EVALUATOR_SHAPES:
                        raise ParseError(
                            "In the dynamo0.3 API a kernel requiring either "
                            "quadrature or an evaluator must request a valid "
                            "evaluator shape (one of {0}) but got '{1}' for "
                            "kernel '{2}'".
                            format(VALID_EVALUATOR_SHAPES, self._eval_shape,
                                   self.name))
            self._func_descriptors.append(descriptor)
        # Perform further checks that the meta-data we've parsed
        # conforms to the rules for this API
        self._validate(need_evaluator)

    def _validate(self, need_evaluator):
        ''' Check that the meta-data conforms to Dynamo 0.3 rules for a
        user-provided kernel or a built-in '''
        from dynamo0p3_builtins import BUILTIN_MAP
        # We must have at least one argument that is written to
        write_count = 0
        for arg in self._arg_descriptors:
            if arg.access != "gh_read":
                write_count += 1
                # We must not write to scalar arguments if it's not a
                # built-in
                if self.name not in BUILTIN_MAP and \
                   arg.type in VALID_SCALAR_NAMES:
                    raise ParseError(
                        "A user-supplied Dynamo 0.3 kernel must not "
                        "write/update a scalar argument but kernel {0} has "
                        "{1} with {2} access".format(self.name,
                                                     arg.type, arg.access))
        if write_count == 0:
            raise ParseError("A Dynamo 0.3 kernel must have at least one "
                             "argument that is updated (written to) but "
                             "found none for kernel {0}".format(self.name))

        # Check that no evaluator shape has been supplied if no basis or
        # differential basis functions are required for the kernel
        if not need_evaluator and self._eval_shape:
            raise ParseError(
                "Kernel '{0}' specifies an evaluator shape ({1}) but does not "
                "need an evaluator because no basis or differential basis "
                "functions are required".format(self.name, self._eval_shape))

        # If we have a columnwise operator as argument then we need to
        # identify the operation that this kernel performs (one of
        # assemble, apply/apply-inverse and matrix-matrix)
        cwise_ops = psyGen.args_filter(self._arg_descriptors,
                                       arg_types=["gh_columnwise_operator"])
        if cwise_ops:
            self._cma_operation = self._identify_cma_op(cwise_ops)

    def _identify_cma_op(self, cwise_ops):
        '''Identify and return the type of CMA-operator-related operation
        this kernel performs (one of "assemble", "apply" or "matrix-matrix")'''

        for arg in self._arg_descriptors:
            # No vector arguments are permitted
            if arg.vector_size > 1:
                raise ParseError(
                    "Kernel {0} takes a CMA operator but has a "
                    "vector argument ({1}). This is forbidden.".
                    format(self.name,
                           arg.type+"*"+str(arg.vector_size)))
            # No stencil accesses are permitted
            if arg.stencil:
                raise ParseError(
                    "Kernel {0} takes a CMA operator but has an argument "
                    "with a stencil access ({1}). This is forbidden.".
                    format(self.name, arg.stencil['type']))

        # Count the number of CMA operators that are written to
        write_count = 0
        for cop in cwise_ops:
            if cop.access not in GH_READ_ACCESSES:
                write_count += 1

        if write_count == 0:
            # This kernel only reads from CMA operators and must
            # therefore be an apply (or apply-inverse). It must
            # have one CMA operator, one read-only field and one
            # written field as arguments
            if len(cwise_ops) != 1:
                raise ParseError(
                    "In the Dynamo 0.3 API a kernel that applies a CMA "
                    "operator must only have one such operator in its "
                    "list of arguments but found {0} for kernel {1}".
                    format(len(cwise_ops), self.name))
            cma_op = cwise_ops[0]
            if len(self._arg_descriptors) != 3:
                raise ParseError(
                    "In the Dynamo 0.3 API a kernel that applies a CMA "
                    "operator must have 3 arguments (the operator and "
                    "two fields) but kernel {0} has {1} arguments".
                    format(self.name, len(self._arg_descriptors)))
            # Check that the other two arguments are fields
            farg_read = psyGen.args_filter(self._arg_descriptors,
                                           arg_types=["gh_field"],
                                           arg_accesses=GH_READ_ACCESSES)
            farg_write = psyGen.args_filter(self._arg_descriptors,
                                            arg_types=["gh_field"],
                                            arg_accesses=GH_WRITE_ACCESSES)
            if len(farg_read) != 1:
                raise ParseError(
                    "Kernel {0} has a read-only CMA operator. In order "
                    "to apply it the kernel must have one read-only field "
                    "argument.".format(self.name))
            if len(farg_write) != 1:
                raise ParseError(
                    "Kernel {0} has a read-only CMA operator. In order "
                    "to apply it the kernel must write to one field "
                    "argument.".format(self.name))
            # Check that the function spaces match up
            if farg_read[0].function_space != cma_op.function_space_from:
                raise ParseError(
                    "Kernel {0} applies a CMA operator but the function "
                    "space of the field argument it reads from ({1}) "
                    "does not match the 'from' space of the operator "
                    "({2})".format(self.name, farg_read[0].function_space,
                                   cma_op.function_space_from))
            if farg_write[0].function_space != cma_op.function_space_to:
                raise ParseError(
                    "Kernel {0} applies a CMA operator but the function "
                    "space of the field argument it writes to ({1}) "
                    "does not match the 'to' space of the operator "
                    "({2})".format(self.name, farg_write[0].function_space,
                                   cma_op.function_space_to))
            # This is a valid CMA-apply or CMA-apply-inverse kernel
            return "apply"

        elif write_count == 1:
            # This kernel writes to a single CMA operator and therefore
            # must either be assembling a CMA operator
            # or performing a matrix-matrix operation...
            # The kernel must not write to any args other than the CMA
            # operator
            write_args = psyGen.args_filter(self._arg_descriptors,
                                            arg_accesses=GH_WRITE_ACCESSES)
            if len(write_args) > 1:
                # Remove the one CMA operator from the list of arguments
                # that are written to so that we can produce a nice
                # error message
                for arg in write_args[:]:
                    if arg.type == 'gh_columnwise_operator':
                        write_args.remove(arg)
                        break
                raise ParseError(
                    "Kernel {0} writes to a column-wise operator but "
                    "also writes to {1} argument(s). This is not "
                    "allowed.".format(self.name,
                                      [arg.type for arg in write_args]))
            if len(cwise_ops) == 1:

                # If this is a valid assembly kernel then we need at least one
                # read-only LMA operator
                lma_read_ops = psyGen.args_filter(
                    self._arg_descriptors,
                    arg_types=["gh_operator"], arg_accesses=GH_READ_ACCESSES)
                if lma_read_ops:
                    return "assembly"
                else:
                    raise ParseError(
                        "Kernel {0} has a single column-wise operator "
                        "argument but does not conform to the rules for an "
                        "Assembly kernel because it does not have any read-"
                        "only LMA operator arguments".format(self.name))
            else:
                # A valid matrix-matrix kernel must only have CMA operators
                # and scalars as arguments.
                scalar_args = psyGen.args_filter(
                    self._arg_descriptors,
                    arg_types=["gh_real", "gh_integer"])
                if (len(scalar_args) + len(cwise_ops)) != \
                   len(self._arg_descriptors):
                    raise ParseError(
                        "A column-wise matrix-matrix kernel must have only "
                        "column-wise operators and scalars as arguments but "
                        "kernel {0} has: {1}.".
                        format(self.name,
                               [arg.type for arg in self._arg_descriptors]))
                return "matrix-matrix"
        else:
            raise ParseError(
                "A Dynamo 0.3 kernel cannot update more than one CMA "
                "(column-wise) operator but kernel {0} updates {1}".
                format(self.name, write_count))

    @property
    def func_descriptors(self):
        ''' Returns metadata about the function spaces within a
        Kernel. This metadata is provided within Kernel code via the
        meta_funcs variable. Information is returned as a list of
        DynFuncDescriptor03 objects, one for each function space. '''
        return self._func_descriptors

    @property
    def cma_operation(self):
        ''' Returns the type of CMA operation identified from the kernel
        meta-data (one of 'assembly', 'apply' or 'matrix-matrix') or
        "" if the kernel does not involve CMA operators '''
        return self._cma_operation

# Second section : PSy specialisations

# classes


class DynamoPSy(PSy):
    ''' The Dynamo specific PSy class. This creates a Dynamo specific
    invokes object (which controls all the required invocation calls).
    It also overrides the PSy gen method so that we generate dynamo
    specific PSy module code. '''

    def __init__(self, invoke_info):
        PSy.__init__(self, invoke_info)
        self._invokes = DynamoInvokes(invoke_info.calls)

    @property
    def name(self):
        '''Returns a name for the psy layer. This is used as the psy module
        name. We override the default value as the Met Office prefer
        _psy to be appended, rather than prepended'''
        return self._name + "_psy"

    @property
    def gen(self):
        '''
        Generate PSy code for the Dynamo0.3 api.

        :rtype: ast

        '''
        from f2pygen import ModuleGen, UseGen
        # create an empty PSy layer module
        psy_module = ModuleGen(self.name)
        # include required infrastructure modules
        psy_module.add(UseGen(psy_module, name="field_mod", only=True,
                              funcnames=["field_type", "field_proxy_type"]))
        psy_module.add(UseGen(psy_module, name="operator_mod", only=True,
                              funcnames=["operator_type",
                                         "operator_proxy_type",
                                         "columnwise_operator_type",
                                         "columnwise_operator_proxy_type"]))
        psy_module.add(UseGen(psy_module, name="quadrature_mod", only=True,
                              funcnames=["quadrature_type"]))
        psy_module.add(UseGen(psy_module, name="constants_mod", only=True,
                              funcnames=["r_def"]))
        # add all invoke specific information
        self.invokes.gen_code(psy_module)
        # inline kernels where requested
        self.inline(psy_module)
        # return the generated code
        return psy_module.root


class DynamoInvokes(Invokes):
    ''' The Dynamo specific invokes class. This passes the Dynamo
    specific invoke class to the base class so it creates the one we
    require. '''

    def __init__(self, alg_calls):
        self._name_space_manager = NameSpaceFactory().create()
        if False:  # pylint: disable=using-constant-test
            self._0_to_n = DynInvoke(None, None)  # for pyreverse
        Invokes.__init__(self, alg_calls, DynInvoke)


def stencil_extent_value(field):
    '''Returns the content of the stencil extent. This may be a literal
    value (a number) or a variable name. This function simplifies this
    problem by returning a string in either case'''
    if field.stencil.extent_arg.is_literal():
        extent = field.stencil.extent_arg.text
    else:
        extent = field.stencil.extent_arg.varName
    return extent


def stencil_unique_str(arg, context):
    '''Returns a string that uniquely identifies a stencil. As a stencil
    differs due to the function space it operates on, type of
    stencil and extent of stencil, we concatenate these things together
    to return a unique string '''
    unique = context
    unique += arg.function_space.mangled_name
    unique += arg.descriptor.stencil['type']
    if arg.descriptor.stencil['extent']:
        raise GenerationError(
            "found a stencil with an extent specified in the metadata. This "
            "is not coded for.")
    unique += arg.stencil.extent_arg.text.lower()
    if arg.descriptor.stencil['type'] == 'xory1d':
        unique += arg.stencil.direction_arg.text.lower()
    return unique


def stencil_map_name(arg):
    ''' returns a valid unique map name for a stencil in the PSy layer '''
    root_name = arg.name + "_stencil_map"
    unique = stencil_unique_str(arg, "map")
    name_space_manager = NameSpaceFactory().create()
    return name_space_manager.create_name(
        root_name=root_name, context="PSyVars", label=unique)


def stencil_dofmap_name(arg):
    ''' returns a valid unique dofmap name for a stencil in the PSy layer '''
    root_name = arg.name + "_stencil_dofmap"
    unique = stencil_unique_str(arg, "dofmap")
    name_space_manager = NameSpaceFactory().create()
    return name_space_manager.create_name(
        root_name=root_name, context="PSyVars", label=unique)


def stencil_size_name(arg):
    ''' returns a valid unique name for the size (in cells) of a stencil
    in the PSy layer '''
    root_name = arg.name + "_stencil_size"
    unique = stencil_unique_str(arg, "size")
    name_space_manager = NameSpaceFactory().create()
    return name_space_manager.create_name(
        root_name=root_name, context="PSyVars", label=unique)


class DynInvokeStencil(object):
    '''stencil information and code generation associated with a
    DynInvoke call'''

    def __init__(self, schedule):

        self._name_space_manager = NameSpaceFactory().create()
        # list of arguments which have an extent value passed to this
        # invoke routine from the algorithm layer. Duplicate argument
        # names are removed.
        self._unique_extent_args = []
        extent_names = []
        for call in schedule.calls():
            for arg in call.arguments.args:
                if arg.stencil:
                    # check for the existence of arg.extent here as in
                    # the future we plan to support kernels which
                    # specify the value of extent in metadata. If this
                    # is the case then an extent argument is not
                    # required.
                    if not arg.stencil.extent:
                        if not arg.stencil.extent_arg.is_literal():
                            if arg.stencil.extent_arg.text not in extent_names:
                                extent_names.append(
                                    arg.stencil.extent_arg.text)
                                self._unique_extent_args.append(arg)

        # a list of arguments that have a direction variable passed in
        # to this invoke routine from the algorithm layer. Duplicate
        # argument names are removed.
        self._unique_direction_args = []
        direction_names = []
        for call in schedule.calls():
            for idx, arg in enumerate(call.arguments.args):
                if arg.stencil and arg.stencil.direction_arg:
                    if arg.stencil.direction_arg.is_literal():
                        raise GenerationError(
                            "Kernel {0}, metadata arg {1}, a literal is not "
                            "a valid value for a stencil direction".
                            format(call.name, str(idx)))
                    if arg.stencil.direction_arg.text.lower() not in \
                       ["x_direction", "y_direction"]:
                        if arg.stencil.direction_arg.text not in \
                           direction_names:
                            direction_names.append(
                                arg.stencil.direction_arg.text)
                            self._unique_direction_args.append(arg)

        # list of stencil args with an extent variable passed in. The same
        # field name may occur more than once here from different kernels.
        self._kern_args = []
        for call in schedule.calls():
            for arg in call.arguments.args:
                if arg.stencil:
                    if not arg.stencil.extent:
                        self._kern_args.append(arg)

    @property
    def _unique_extent_vars(self):
        '''return a list of all the unique extent argument names in this
        invoke call. '''
        names = []
        for arg in self._unique_extent_args:
            names.append(arg.stencil.extent_arg.varName)
        return names

    def _declare_unique_extent_vars(self, parent):
        '''Declare all unique extent arguments as integers with intent in and
        add the declaration as a child of the parent argument passed
        in. The parent argument should be an appropriate f2pygen
        object. '''
        from f2pygen import DeclGen
        if self._unique_extent_vars:
            parent.add(DeclGen(parent, datatype="integer",
                               entity_decls=self._unique_extent_vars,
                               intent="in"))

    @property
    def _unique_direction_vars(self):
        '''return a list of all the unique direction argument names in this
        invoke call.'''
        names = []
        for arg in self._unique_direction_args:
            names.append(arg.stencil.direction_arg.varName)
        return names

    def _declare_unique_direction_vars(self, parent):
        '''Declare all unique direction arguments as integers with intent in
        and add the declaration as a child of the parent argument
        passed in. The parent argument should be an appropriate
        f2pygen object. '''
        from f2pygen import DeclGen
        if self._unique_direction_vars:
            parent.add(DeclGen(parent, datatype="integer",
                               entity_decls=self._unique_direction_vars,
                               intent="in"))

    @property
    def unique_alg_vars(self):
        '''returns a list of the names of the extent and direction arguments
        specified in the algorithm layer'''
        return self._unique_extent_vars + self._unique_direction_vars

    def declare_unique_alg_vars(self, parent):
        '''declares all extent and direction arguments passed into the PSy
        layer'''
        self._declare_unique_extent_vars(parent)
        self._declare_unique_direction_vars(parent)

    def initialise_stencil_maps(self, parent):
        '''adds in the required stencil dofmap code to the PSy layer'''
        from f2pygen import AssignGen, IfThenGen, TypeDeclGen, UseGen, \
            CommentGen, DeclGen
        if self._kern_args:
            parent.add(CommentGen(parent, ""))
            parent.add(CommentGen(parent, " Initialise stencil dofmaps"))
            parent.add(CommentGen(parent, ""))
            parent.add(UseGen(parent, name="stencil_dofmap_mod", only=True,
                              funcnames=["stencil_dofmap_type"]))
            stencil_map_names = []
            for arg in self._kern_args:
                map_name = stencil_map_name(arg)
                if map_name not in stencil_map_names:
                    # only initialise maps once
                    stencil_map_names.append(map_name)
                    parent.add(
                        TypeDeclGen(parent, pointer=True,
                                    datatype="stencil_dofmap_type",
                                    entity_decls=[map_name+" => null()"]))
                    stencil_type = arg.descriptor.stencil['type']
                    if stencil_type == "xory1d":
                        parent.add(UseGen(parent, name="flux_direction_mod",
                                          only=True,
                                          funcnames=["x_direction",
                                                     "y_direction"]))
                        parent.add(UseGen(parent, name="stencil_dofmap_mod",
                                          only=True,
                                          funcnames=["STENCIL_1DX",
                                                     "STENCIL_1DY"]))
                        direction_name = arg.stencil.direction_arg.varName
                        for direction in ["x", "y"]:
                            if_then = IfThenGen(parent, direction_name +
                                                " .eq. " + direction +
                                                "_direction")
                            if_then.add(
                                AssignGen(if_then, pointer=True,
                                          lhs=map_name, rhs=arg.proxy_name +
                                          "%vspace%get_stencil_dofmap("
                                          "STENCIL_1D" + direction.upper() +
                                          ","+stencil_extent_value(arg)+")"))
                            parent.add(if_then)
                    else:
                        try:
                            stencil_name = STENCIL_MAPPING[stencil_type]
                        except KeyError:
                            raise GenerationError(
                                "Unsupported stencil type '{0}' supplied. "
                                "Supported mappings are {1}".
                                format(arg.descriptor.stencil['type'],
                                       str(STENCIL_MAPPING)))
                        parent.add(UseGen(parent, name="stencil_dofmap_mod",
                                          only=True,
                                          funcnames=[stencil_name]))
                        parent.add(
                            AssignGen(parent, pointer=True, lhs=map_name,
                                      rhs=arg.proxy_name +
                                      "%vspace%get_stencil_dofmap(" +
                                      stencil_name + "," +
                                      stencil_extent_value(arg) + ")"))
                    parent.add(DeclGen(parent, datatype="integer",
                                       pointer=True,
                                       entity_decls=[stencil_dofmap_name(arg) +
                                                     "(:,:,:) => null()"]))
                    parent.add(AssignGen(parent, pointer=True,
                                         lhs=stencil_dofmap_name(arg),
                                         rhs=map_name + "%get_whole_dofmap()"))

                    # Add declaration and look-up of stencil size
                    parent.add(DeclGen(parent, datatype="integer",
                                       entity_decls=[stencil_size_name(arg)]))
                    parent.add(AssignGen(parent, lhs=stencil_size_name(arg),
                                         rhs=map_name + "%get_size()"))


class DynInvokeDofmaps(object):
    ''' Holds all information on the dofmaps (including column-banded and
    indirection) required by an invoke '''

    def __init__(self, schedule):

        self._name_space_manager = NameSpaceFactory().create()
        # Look at every kernel call in this invoke and generate a list
        # of the unique function spaces involved.
        # We create a dictionary whose keys are the map names and entries
        # are the corresponding field objects.
        self._unique_fs_maps = {}
        # We also create a dictionary of column-banded dofmaps. Entries
        # in this one are a list where the first item is the
        # corresponding CMA object and the second is whether the map
        # corresponds to the 'to' or 'from' space.
        self._unique_cbanded_maps = {}
        # A dictionary of required CMA indirection dofmaps. Each entry
        # is a list where the first item correponds to the CMA object
        # and the second is whether the map corresponds to the 'to' or
        # 'from' space.
        self._unique_indirection_maps = {}

        for call in schedule.calls():
            # We only need a dofmap if the kernel iterates over cells
            if call.iterates_over == "cells":
                for unique_fs in call.arguments.unique_fss:
                    # We only need a dofmap if there is a *field* on this
                    # function space. If there is then we use it to look
                    # up the dofmap.
                    fld_arg = field_on_space(unique_fs, call.arguments)
                    if fld_arg:
                        map_name = get_fs_map_name(unique_fs)
                        if map_name not in self._unique_fs_maps:
                            self._unique_fs_maps[map_name] = fld_arg
                if call.cma_operation == "assembly":
                    # A kernel that assembles a CMA operator requires
                    # column-banded dofmaps for its 'to' and 'from'
                    # function spaces
                    cma_args = psyGen.args_filter(
                        call.arguments.args,
                        arg_types=["gh_columnwise_operator"])
                    map_name = get_cbanded_map_name(
                        cma_args[0].function_space_to)
                    if map_name not in self._unique_cbanded_maps:
                        self._unique_cbanded_maps[map_name] = [cma_args[0],
                                                               "to"]
                    map_name = get_cbanded_map_name(
                        cma_args[0].function_space_from)
                    if map_name not in self._unique_cbanded_maps:
                        self._unique_cbanded_maps[map_name] = [cma_args[0],
                                                               "from"]
                elif call.cma_operation == "apply":
                    # A kernel that applies (or applies the inverse of) a
                    # CMA operator requires the indirection dofmaps for the
                    # to- and from-spaces of the operator.
                    cma_args = psyGen.args_filter(
                        call.arguments.args,
                        arg_types=["gh_columnwise_operator"])
                    map_name = get_cma_indirection_map_name(
                        cma_args[0].function_space_to)
                    if map_name not in self._unique_indirection_maps:
                        self._unique_indirection_maps[map_name] = [cma_args[0],
                                                                   "to"]
                    map_name = get_cma_indirection_map_name(
                        cma_args[0].function_space_from)
                    if map_name not in self._unique_indirection_maps:
                        self._unique_indirection_maps[map_name] = [cma_args[0],
                                                                   "from"]

    def initialise_dofmaps(self, parent):
        ''' Generates the calls to the LFRic infrastructure that
        look-up the necessary dofmaps. Adds these calls as children
        of the supplied parent node. This must be an appropriate
        f2pygen object. '''
        from f2pygen import CommentGen, AssignGen

        # If we've got no dofmaps then we do nothing
        if self._unique_fs_maps:
            parent.add(CommentGen(parent, ""))
            parent.add(CommentGen(parent,
                                  " Look-up dofmaps for each function space"))
            parent.add(CommentGen(parent, ""))

            for dmap, field in self._unique_fs_maps.items():
                parent.add(AssignGen(parent, pointer=True, lhs=dmap,
                                     rhs=field.proxy_name_indexed +
                                     "%" + field.ref_name() +
                                     "%get_whole_dofmap()"))
        if self._unique_cbanded_maps:
            parent.add(CommentGen(parent, ""))
            parent.add(CommentGen(parent,
                                  " Look-up required column-banded dofmaps"))
            parent.add(CommentGen(parent, ""))

            for dmap, cma in self._unique_cbanded_maps.items():
                parent.add(AssignGen(parent, pointer=True, lhs=dmap,
                                     rhs=cma[0].proxy_name_indexed +
                                     "%column_banded_dofmap_"+cma[1]))

        if self._unique_indirection_maps:
            parent.add(CommentGen(parent, ""))
            parent.add(CommentGen(parent,
                                  " Look-up required CMA indirection dofmaps"))
            parent.add(CommentGen(parent, ""))

            for dmap, cma in self._unique_indirection_maps.items():
                parent.add(AssignGen(parent, pointer=True, lhs=dmap,
                                     rhs=cma[0].proxy_name_indexed +
                                     "%indirection_dofmap_"+cma[1]))

    def declare_dofmaps(self, parent):
        ''' Declare all unique function space dofmaps as pointers to
        integer arrays of rank 2. The declarations are added as
        children of the supplied parent argument. This must be an
        appropriate f2pygen object. '''
        from f2pygen import DeclGen

        # Function space dofmaps
        decl_map_names = \
            [dmap+"(:,:) => null()" for dmap in self._unique_fs_maps]

        if decl_map_names:
            parent.add(DeclGen(parent, datatype="integer", pointer=True,
                               entity_decls=decl_map_names))

        # Column-banded dofmaps
        decl_bmap_names = \
            [dmap+"(:,:) => null()" for dmap in self._unique_cbanded_maps]
        if decl_bmap_names:
            parent.add(DeclGen(parent, datatype="integer", pointer=True,
                               entity_decls=decl_bmap_names))

        # CMA operator indirection dofmaps
        decl_ind_map_names = \
            [dmap+"(:) => null()" for dmap in self._unique_indirection_maps]
        if decl_ind_map_names:
            parent.add(DeclGen(parent, datatype="integer", pointer=True,
                               entity_decls=decl_ind_map_names))


class DynInvokeCMAOperators(object):
    ''' Holds all information on the CMA operators required by an invoke '''

    cma_same_fs_params = ["nrow", "bandwidth", "alpha",
                          "beta", "gamma_m", "gamma_p"]
    cma_diff_fs_params = ["nrow", "ncol", "bandwidth", "alpha",
                          "beta", "gamma_m", "gamma_p"]

    def __init__(self, schedule):

        self._name_space_manager = NameSpaceFactory().create()

        # Look at every kernel call in this invoke and generate a set of
        # the unique CMA operators involved. For each one we create a
        # dictionary entry. The key is the name of the CMA argument in the
        # PSy layer and the entry is itself another dictionary containing
        # two entries: the first 'arg' is the CMA argument object and the
        # second 'params' is the list of integer variables associated with
        # that CMA operator. The contents of this list depend on whether
        # or not the to/from function spaces of the CMA operator are the
        # same.
        self._cma_ops = {}
        for call in schedule.calls():
            if call.cma_operation:
                # Get a list of all of the CMA arguments to this call
                cma_args = psyGen.args_filter(
                    call.arguments.args,
                    arg_types=["gh_columnwise_operator"])
                # Create a dictionary entry for each argument that we
                # have not already seen
                for arg in cma_args:
                    if arg.name not in self._cma_ops:
                        if arg.function_space_to.orig_name != \
                           arg.function_space_from.orig_name:
                            self._cma_ops[arg.name] = {
                                "arg": arg,
                                "params": self.cma_diff_fs_params}
                        else:
                            self._cma_ops[arg.name] = {
                                "arg": arg,
                                "params": self.cma_same_fs_params}

    def initialise_cma_ops(self, parent):
        ''' Generates the calls to the LFRic infrastructure that look-up
        the various components of each CMA operator. Adds these as
        children of the supplied parent node. This must be an appropriate
        f2pygen object. '''
        from f2pygen import CommentGen, AssignGen

        # If we have no CMA operators then we do nothing
        if not self._cma_ops:
            return

        parent.add(CommentGen(parent, ""))
        parent.add(CommentGen(parent,
                              " Look-up information for each CMA operator"))
        parent.add(CommentGen(parent, ""))

        for op_name in self._cma_ops:
            # First create a pointer to the array containing the actual
            # matrix
            cma_name = self._name_space_manager.create_name(
                root_name=op_name+"_matrix",
                context="PSyVars",
                label=op_name+"_matrix")
            parent.add(AssignGen(parent, lhs=cma_name, pointer=True,
                                 rhs=self._cma_ops[op_name]["arg"].
                                 proxy_name_indexed+"%columnwise_matrix"))
            # Then make copies of the related integer parameters
            for param in self._cma_ops[op_name]["params"]:
                param_name = self._name_space_manager.create_name(
                    root_name=op_name+"_"+param,
                    context="PSyVars",
                    label=op_name+"_"+param)
                parent.add(AssignGen(parent, lhs=param_name,
                                     rhs=self._cma_ops[op_name]["arg"].
                                     proxy_name_indexed+"%"+param))

    def declare_cma_ops(self, parent):
        ''' Generate the necessary declarations for all column-wise operators
        and their associated parameters '''
        from f2pygen import DeclGen

        # If we have no CMA operators then we do nothing
        if not self._cma_ops:
            return

        for op_name in self._cma_ops:
            # Declare the matrix itself
            cma_name = self._name_space_manager.create_name(
                root_name=op_name+"_matrix", context="PSyVars",
                label=op_name+"_matrix")
            parent.add(DeclGen(parent, datatype="real", kind="r_def",
                               pointer=True,
                               entity_decls=[cma_name+"(:,:,:) => null()"]))
            # Declare the associated integer parameters
            param_names = []
            for param in self._cma_ops[op_name]["params"]:
                param_names.append(self._name_space_manager.create_name(
                    root_name=op_name+"_"+param,
                    context="PSyVars",
                    label=op_name+"_"+param))
            parent.add(DeclGen(parent, datatype="integer",
                               entity_decls=param_names))


class DynInvoke(Invoke):
    ''' The Dynamo specific invoke class. This passes the Dynamo
    specific schedule class to the base class so it creates the one we
    require.  Also overrides the gen_code method so that we generate
    dynamo specific invocation code. '''

    def __init__(self, alg_invocation, idx):
        if False:
            self._schedule = DynSchedule(None)  # for pyreverse
        reserved_names_list = []
        reserved_names_list.extend(STENCIL_MAPPING.values())
        reserved_names_list.extend(VALID_STENCIL_DIRECTIONS)
        reserved_names_list.extend(["omp_get_thread_num",
                                    "omp_get_max_threads"])
        Invoke.__init__(self, alg_invocation, idx, DynSchedule,
                        reserved_names=reserved_names_list)

        # The baseclass works out the algorithm code's unique argument
        # list and stores it in the self._alg_unique_args
        # list. However, the base class currently ignores any stencil and qr
        # arguments so we need to add them in.

        # initialise our invoke stencil information
        self.stencil = DynInvokeStencil(self.schedule)

        # Initialise the object holding all information on the dofmaps
        # required by this invoke.
        self.dofmaps = DynInvokeDofmaps(self.schedule)

        # Initialise the object holding all information on the column-
        # -matrix assembly operators required by this invoke.
        self.cma_ops = DynInvokeCMAOperators(self.schedule)

        # extend arg list
        self._alg_unique_args.extend(self.stencil.unique_alg_vars)

        # adding in qr arguments
        self._alg_unique_qr_args = []
        for call in self.schedule.calls():
            if call.qr_required:
                if call.qr_text not in self._alg_unique_qr_args:
                    self._alg_unique_qr_args.append(call.qr_text)
        self._alg_unique_args.extend(self._alg_unique_qr_args)
        # we also need to work out the names to use for the qr
        # arguments within the psy layer. These are stored in the
        # _psy_unique_qr_vars list
        self._psy_unique_qr_vars = []
        for call in self.schedule.calls():
            if call.qr_required:
                if call.qr_name not in self._psy_unique_qr_vars:
                    self._psy_unique_qr_vars.append(call.qr_name)

        # lastly, add in halo exchange calls and global sums if
        # required. We only need to add halo exchange calls for fields
        # since operators are assembled in place and scalars don't
        # have halos. We only need to add global sum calls for scalars
        # which have a gh_sum access.
        if config.DISTRIBUTED_MEMORY:
            # halo exchange calls
            # for the moment just add them before each loop as required
            for loop in self.schedule.loops():
                inc = loop.has_inc_arg()
                for halo_field in loop.unique_fields_with_halo_reads():
                    if halo_field.vector_size > 1:
                        # the range function below returns values from
                        # 1 to the vector size which is what we
                        # require in our Fortran code
                        for idx in range(1, halo_field.vector_size+1):
                            exchange = DynHaloExchange(
                                halo_field, parent=loop, vector_index=idx,
                                inc=inc)
                            loop.parent.children.insert(loop.position,
                                                        exchange)
                    else:
                        exchange = DynHaloExchange(halo_field, parent=loop,
                                                   inc=inc)
                        loop.parent.children.insert(loop.position, exchange)
            # global sum calls
            for loop in self.schedule.loops():
                for scalar in loop.args_filter(
                        arg_types=VALID_SCALAR_NAMES,
                        arg_accesses=VALID_REDUCTION_NAMES, unique=True):
                    if scalar.type.lower() == "gh_integer":
                        raise GenerationError(
                            "Integer reductions are not currently supported "
                            "by the LFRic infrastructure. Error found in "
                            "Kernel '{0}', argument '{1}'".format(
                                scalar.call.name, scalar.name))
                    global_sum = DynGlobalSum(scalar, parent=loop.parent)
                    loop.parent.children.insert(loop.position+1, global_sum)

    @property
    def qr_required(self):
        ''' Returns True if at least one of the kernels in this invoke
        requires QR, otherwise returns False. '''
        required = False
        for call in self.schedule.calls():
            if call.qr_required:
                required = True
                break
        return required

    def unique_proxy_declarations(self, datatype, access=None):
        ''' Returns a list of all required proxy declarations for the
        specified datatype.  If access is supplied (e.g. "gh_write")
        then only declarations with that access are returned. '''
        if datatype not in VALID_ARG_TYPE_NAMES:
            raise GenerationError(
                "unique_proxy_declarations called with an invalid datatype. "
                "Expected one of '{0}' but found '{1}'".
                format(str(VALID_ARG_TYPE_NAMES), datatype))
        if access and access not in VALID_ACCESS_DESCRIPTOR_NAMES:
            raise GenerationError(
                "unique_proxy_declarations called with an invalid access "
                "type. Expected one of '{0}' but got '{1}'".
                format(VALID_ACCESS_DESCRIPTOR_NAMES, access))
        declarations = []
        for call in self.schedule.calls():
            for arg in call.arguments.args:
                if not access or arg.access == access:
                    if arg.text and arg.type == datatype:
                        if arg.proxy_declaration_name not in declarations:
                            declarations.append(arg.proxy_declaration_name)
        return declarations

    def arg_for_funcspace(self, fspace):
        ''' Returns an argument object which is on the requested
        function space. Searches through all Kernel calls in this
        invoke. Currently the first argument object that is found is
        used. Throws an exception if no argument exists. '''
        for kern_call in self.schedule.calls():
            try:
                return kern_call.arguments.get_arg_on_space(fspace)
            except FieldNotFoundError:
                pass
        raise GenerationError(
            "No argument found on '{0}' space".format(fspace.mangled_name))

    def unique_fss(self):
        ''' Returns the unique function space *objects* over all kernel
        calls in this invoke. '''
        unique_fs = []
        unique_fs_names = []
        for kern_call in self.schedule.calls():
            kern_fss = kern_call.arguments.unique_fss
            for fspace in kern_fss:
                if fspace.mangled_name not in unique_fs_names:
                    unique_fs.append(fspace)
                    unique_fs_names.append(fspace.mangled_name)
        return unique_fs

    def basis_required(self, func_space):
        ''' Returns true if at least one of the kernels in this invoke
        requires a basis function for this function space, otherwise
        it returns False. '''
        # look in each kernel
        for kern_call in self.schedule.kern_calls():
            # is there a descriptor for this function space?
            if kern_call.fs_descriptors.exists(func_space):
                descriptor = kern_call.fs_descriptors.\
                    get_descriptor(func_space)
                # does this descriptor specify that a basis function
                # is required?
                if descriptor.requires_basis:
                    # found a kernel that requires a basis function
                    # for this function space
                    return True

        # none of my kernels require a basis function for this function space
        return False

    def diff_basis_required(self, func_space):
        ''' Returns true if at least one of the kernels in this invoke
        requires a differential basis function for this function
        space, otherwise it returns False.'''
        # look in each kernel
        for kern_call in self.schedule.kern_calls():
            # is there a descriptor for this function space?
            if kern_call.fs_descriptors.exists(func_space):
                descriptor = kern_call.fs_descriptors.\
                    get_descriptor(func_space)
                # does this descriptor specify that a basis function
                # is required?
                if descriptor.requires_diff_basis:
                    # found a kernel that requires a diff basis
                    # function for this function space
                    return True
        # none of my kernels require a diff basis function for this
        # function space
        return False

    def is_coloured(self):
        ''' Returns true if at least one of the loops in the
        schedule of this invoke has been coloured '''
        for loop in self.schedule.loops():
            if loop.loop_type == "colours":
                return True
        return False

    def get_fs_operator_name(self, operator_name, function_space):
        ''' A convenience method that returns an operator name for a
        particular operator on a particular function space. These
        names are specified in function_space_descriptors objects
        contained within Kernel objects. The first Kernel which uses
        the specified function space is used to return the name. If no
        Kernel using this function space exists in this invoke, an
        error is thrown. '''
        for kern_call in self.schedule.kern_calls():
            if kern_call.fs_descriptors.exists(function_space):
                return get_fs_operator_name(operator_name, function_space)
        raise GenerationError(
            "Dyn_invoke:get_fs_operator_name: no kern call with function "
            "space '{0}' and operator '{1}'".format(function_space,
                                                    operator_name))

    def field_on_space(self, func_space):
        ''' If a field exists on this space for any kernel in this
        invoke then return that field. Otherwise return None. '''
        for kern_call in self.schedule.calls():
            field = field_on_space(func_space, kern_call.arguments)
            if field:
                return field
        return None

    def gen_code(self, parent):
        ''' Generates Dynamo specific invocation code (the subroutine
        called by the associated invoke call in the algorithm
        layer). This consists of the PSy invocation subroutine and the
        declaration of its arguments. '''
        from f2pygen import SubroutineGen, TypeDeclGen, AssignGen, DeclGen, \
            AllocateGen, DeallocateGen, CallGen, CommentGen
        # Create a namespace manager so we can avoid name clashes
        self._name_space_manager = NameSpaceFactory().create()
        # Create the subroutine
        invoke_sub = SubroutineGen(parent, name=self.name,
                                   args=self.psy_unique_var_names +
                                   self.stencil.unique_alg_vars +
                                   self._psy_unique_qr_vars)

        # Add the subroutine argument declarations for real scalars
        scalar_args = self.unique_declns_by_intent("gh_real")
        for intent in FORTRAN_INTENT_NAMES:
            if scalar_args[intent]:
                invoke_sub.add(DeclGen(invoke_sub, datatype="real",
                                       kind="r_def",
                                       entity_decls=scalar_args[intent],
                                       intent=intent))

        # Add the subroutine argument declarations for integer scalars
        scalar_args = self.unique_declns_by_intent("gh_integer")
        for intent in FORTRAN_INTENT_NAMES:
            if scalar_args[intent]:
                invoke_sub.add(DeclGen(invoke_sub, datatype="integer",
                                       entity_decls=scalar_args[intent],
                                       intent=intent))

        # declare any stencil arguments
        self.stencil.declare_unique_alg_vars(invoke_sub)

        # Declare any dofmaps
        self.dofmaps.declare_dofmaps(invoke_sub)

        # Declare any CMA operators and associated parameters
        self.cma_ops.declare_cma_ops(invoke_sub)

        # Add the subroutine argument declarations for fields
        fld_args = self.unique_declns_by_intent("gh_field")
        for intent in FORTRAN_INTENT_NAMES:
            if fld_args[intent]:
                if intent == "out":
                    # The data part of a field might have intent(out) but
                    # in order to preserve the state of the whole derived-type
                    # object it must be declared as inout.
                    fort_intent = "inout"
                else:
                    fort_intent = intent
                invoke_sub.add(TypeDeclGen(invoke_sub, datatype="field_type",
                                           entity_decls=fld_args[intent],
                                           intent=fort_intent))

        # Add the subroutine argument declarations for operators that
        # are read or written (operators are always on discontinous spaces
        # and therefore are never 'inc')
        op_declarations_dict = self.unique_declns_by_intent("gh_operator")
        for intent in FORTRAN_INTENT_NAMES:
            if op_declarations_dict[intent]:
                if intent == "out":
                    # The data part of an operator might have intent(out) but
                    # in order to preserve the state of the whole derived-type
                    # object it must be declared as inout.
                    fort_intent = "inout"
                else:
                    fort_intent = intent
                invoke_sub.add(
                    TypeDeclGen(invoke_sub, datatype="operator_type",
                                entity_decls=op_declarations_dict[intent],
                                intent=fort_intent))

        # Add subroutine argument declarations for CMA operators that are
        # read or written (as with normal/LMA operators, they are never 'inc'
        # because they are discontinuous)
        cma_op_declarations_dict = self.unique_declns_by_intent(
            "gh_columnwise_operator")
        for intent in FORTRAN_INTENT_NAMES:
            if cma_op_declarations_dict[intent]:
                if intent == "out":
                    # The data part of an operator might have intent(out) but
                    # in order to preserve the state of the whole derived-type
                    # object it must be declared as inout.
                    fort_intent = "inout"
                else:
                    fort_intent = intent
                invoke_sub.add(
                    TypeDeclGen(invoke_sub,
                                datatype="columnwise_operator_type",
                                entity_decls=cma_op_declarations_dict[intent],
                                intent=fort_intent))

        # Add the subroutine argument declarations for qr (quadrature
        # rules)
        if len(self._psy_unique_qr_vars) > 0:
            invoke_sub.add(TypeDeclGen(invoke_sub, datatype="quadrature_type",
                                       entity_decls=self._psy_unique_qr_vars,
                                       intent="in"))

        # declare and initialise proxies for each of the (non-scalar)
        # arguments
        invoke_sub.add(CommentGen(invoke_sub, ""))
        invoke_sub.add(CommentGen(invoke_sub, " Initialise field proxies"))
        invoke_sub.add(CommentGen(invoke_sub, ""))
        for arg in self.psy_unique_vars:
            # We don't have proxies for scalars
            if arg.type in VALID_SCALAR_NAMES:
                continue
            if arg.vector_size > 1:
                # the range function below returns values from
                # 1 to the vector size which is what we
                # require in our Fortran code
                for idx in range(1, arg.vector_size+1):
                    invoke_sub.add(
                        AssignGen(invoke_sub,
                                  lhs=arg.proxy_name+"("+str(idx)+")",
                                  rhs=arg.name+"("+str(idx)+")%get_proxy()"))
            else:
                invoke_sub.add(AssignGen(invoke_sub, lhs=arg.proxy_name,
                                         rhs=arg.name+"%get_proxy()"))

        field_proxy_decs = self.unique_proxy_declarations("gh_field")
        if len(field_proxy_decs) > 0:
            invoke_sub.add(
                TypeDeclGen(invoke_sub,
                            datatype="field_proxy_type",
                            entity_decls=field_proxy_decs))
        op_proxy_decs = self.unique_proxy_declarations("gh_operator")
        if len(op_proxy_decs) > 0:
            invoke_sub.add(
                TypeDeclGen(invoke_sub,
                            datatype="operator_proxy_type",
                            entity_decls=op_proxy_decs))
        cma_op_proxy_decs = self.unique_proxy_declarations(
            "gh_columnwise_operator")
        if len(cma_op_proxy_decs) > 0:
            invoke_sub.add(
                TypeDeclGen(invoke_sub,
                            datatype="columnwise_operator_proxy_type",
                            entity_decls=cma_op_proxy_decs))

        # Initialise the number of layers
        invoke_sub.add(CommentGen(invoke_sub, ""))
        invoke_sub.add(CommentGen(invoke_sub, " Initialise number of layers"))
        invoke_sub.add(CommentGen(invoke_sub, ""))

        # Use the first argument that is not a scalar
        first_var = None
        cma_op = None
        for var in self.psy_unique_vars:
            if not first_var and var.type in ["gh_field", "gh_operator",
                                              "gh_columnwise_operator"]:
                first_var = var
            if var.type == "gh_columnwise_operator":
                cma_op = var
        if not first_var:
            raise GenerationError(
                "Cannot create an Invoke with no field/operator arguments")

        # Use our namespace manager to create a unique name unless
        # the context and label match and in this case return the
        # previous name
        nlayers_name = self._name_space_manager.create_name(
            root_name="nlayers", context="PSyVars", label="nlayers")
        invoke_sub.add(
            AssignGen(invoke_sub, lhs=nlayers_name,
                      rhs=first_var.proxy_name_indexed + "%" +
                      first_var.ref_name() + "%get_nlayers()"))
        invoke_sub.add(DeclGen(invoke_sub, datatype="integer",
                               entity_decls=[nlayers_name]))

        # If we have one or more CMA operators then we will need the number
        # of columns in the mesh
        if cma_op:
            invoke_sub.add(CommentGen(invoke_sub, ""))
            invoke_sub.add(CommentGen(invoke_sub,
                                      " Initialise number of cols"))
            invoke_sub.add(CommentGen(invoke_sub, ""))
            ncol_name = self._name_space_manager.create_name(
                root_name="ncell_2d", context="PSyVars", label="ncell_2d")
            invoke_sub.add(
                AssignGen(invoke_sub, lhs=ncol_name,
                          rhs=cma_op.proxy_name_indexed + "%ncell_2d"))
            invoke_sub.add(DeclGen(invoke_sub, datatype="integer",
                                   entity_decls=[ncol_name]))

        # declare and initialise a mesh object if required
        if config.DISTRIBUTED_MEMORY:
            from f2pygen import UseGen
            # we will need a mesh object for any loop bounds
            mesh_obj_name = self._name_space_manager.create_name(
                root_name="mesh", context="PSyVars", label="mesh")
            invoke_sub.add(UseGen(invoke_sub, name="mesh_mod", only=True,
                                  funcnames=["mesh_type"]))
            invoke_sub.add(
                TypeDeclGen(invoke_sub, datatype="mesh_type", pointer=True,
                            entity_decls=[mesh_obj_name+" => null()"]))
            rhs = first_var.name_indexed + "%get_mesh()"
            invoke_sub.add(CommentGen(invoke_sub, ""))
            invoke_sub.add(CommentGen(invoke_sub, " Create a mesh object"))
            invoke_sub.add(CommentGen(invoke_sub, ""))
            invoke_sub.add(AssignGen(invoke_sub, pointer=True,
                                     lhs=mesh_obj_name, rhs=rhs))

        if self.schedule.reductions(reprod=True):
            # we have at least one reproducible reduction so we need
            # to know the number of OpenMP threads
            from f2pygen import UseGen
            omp_function_name = "omp_get_max_threads"
            nthreads_name = self._name_space_manager.create_name(
                root_name="nthreads", context="PSyVars", label="nthreads")
            invoke_sub.add(UseGen(invoke_sub, name="omp_lib", only=True,
                                  funcnames=[omp_function_name]))
            invoke_sub.add(DeclGen(invoke_sub, datatype="integer",
                                   entity_decls=[nthreads_name]))
            invoke_sub.add(CommentGen(invoke_sub, ""))
            invoke_sub.add(CommentGen(
                invoke_sub, " Determine the number of OpenMP threads"))
            invoke_sub.add(CommentGen(invoke_sub, ""))
            invoke_sub.add(AssignGen(invoke_sub, lhs=nthreads_name,
                                     rhs=omp_function_name+"()"))

        # Initialise any stencil maps
        self.stencil.initialise_stencil_maps(invoke_sub)

        # Initialise dofmaps (one for each function space that is used
        # in this invoke)
        self.dofmaps.initialise_dofmaps(invoke_sub)

        # Initialise CMA operators and associated parameters
        self.cma_ops.initialise_cma_ops(invoke_sub)

        if self.qr_required:
            # declare and initialise qr values
            invoke_sub.add(CommentGen(invoke_sub, ""))
            invoke_sub.add(CommentGen(invoke_sub, " Initialise qr values"))
            invoke_sub.add(CommentGen(invoke_sub, ""))
            invoke_sub.add(
                DeclGen(invoke_sub, datatype="integer",
                        entity_decls=["nqp_h", "nqp_v"]))
            invoke_sub.add(
                DeclGen(invoke_sub, datatype="real", pointer=True,
                        kind="r_def", entity_decls=["xp(:,:) => null()"]))
            decl_list = ["zp(:) => null()", "wh(:) => null()",
                         "wv(:) => null()"]
            invoke_sub.add(
                DeclGen(invoke_sub, datatype="real", pointer=True,
                        kind="r_def", entity_decls=decl_list))
            if len(self._psy_unique_qr_vars) > 1:
                raise GenerationError(
                    "Oops, not yet coded for multiple qr values")
            qr_var_name = self._psy_unique_qr_vars[0]
            qr_ptr_vars = {"zp": "xqp_v", "xp": "xqp_h", "wh": "wqp_h",
                           "wv": "wqp_v"}
            qr_vars = ["nqp_h", "nqp_v"]
            for qr_var in qr_ptr_vars.keys():
                invoke_sub.add(
                    AssignGen(invoke_sub, pointer=True, lhs=qr_var,
                              rhs=qr_var_name + "%get_" +
                              qr_ptr_vars[qr_var] + "()"))
            for qr_var in qr_vars:
                invoke_sub.add(
                    AssignGen(invoke_sub, lhs=qr_var,
                              rhs=qr_var_name + "%get_" + qr_var + "()"))
        operator_declarations = []
        var_list = []
        var_dim_list = []
        # loop over all unique function spaces used by the kernels in this
        # invoke
        for function_space in self.unique_fss():
            # Initialise information associated with this function space
            invoke_sub.add(CommentGen(invoke_sub, ""))
            invoke_sub.add(
                CommentGen(invoke_sub, " Initialise sizes and "
                           "allocate any basis arrays for " +
                           function_space.mangled_name))
            invoke_sub.add(CommentGen(invoke_sub, ""))
            # Find an argument on this space to use to dereference
            arg = self.arg_for_funcspace(function_space)
            name = arg.proxy_name_indexed
            # initialise ndf for this function space and add name to
            # list to declare later
            ndf_name = get_fs_ndf_name(function_space)
            var_list.append(ndf_name)
            invoke_sub.add(AssignGen(invoke_sub, lhs=ndf_name,
                                     rhs=name +
                                     "%" + arg.ref_name(function_space) +
                                     "%get_ndf()"))
            # if there is a field on this space then initialise undf
            # for this function space and add name to list to declare
            # later
            if self.field_on_space(function_space):
                undf_name = get_fs_undf_name(function_space)
                var_list.append(undf_name)
                invoke_sub.add(AssignGen(invoke_sub, lhs=undf_name,
                                         rhs=name + "%" +
                                         arg.ref_name(function_space) +
                                         "%get_undf()"))
            if self.basis_required(function_space):
                # initialise 'dim' variable for this function space
                # and add name to list to declare later
                lhs = "dim_"+function_space.mangled_name
                var_dim_list.append(lhs)
                rhs = name+"%"+arg.ref_name(function_space)+"%get_dim_space()"
                invoke_sub.add(AssignGen(invoke_sub, lhs=lhs, rhs=rhs))
                # allocate the basis function variable
                alloc_args = "dim_" + function_space.mangled_name + ", " + \
                             get_fs_ndf_name(function_space) + ", nqp_h, nqp_v"
                op_name = self.get_fs_operator_name("gh_basis", function_space)
                invoke_sub.add(AllocateGen(invoke_sub,
                                           op_name+"("+alloc_args+")"))
                # add basis function variable to list to declare later
                operator_declarations.append(op_name+"(:,:,:,:)")
            if self.diff_basis_required(function_space):
                # initialise 'diff_dim' variable for this function
                # space and add name to list to declare later
                lhs = "diff_dim_" + function_space.mangled_name
                var_dim_list.append(lhs)
                rhs = name+"%" + arg.ref_name(function_space) + \
                    "%get_dim_space_diff()"
                invoke_sub.add(AssignGen(invoke_sub, lhs=lhs, rhs=rhs))
                # allocate the diff basis function variable
                alloc_args = ("diff_dim_" + function_space.mangled_name +
                              ", " + get_fs_ndf_name(function_space) +
                              ", nqp_h, nqp_v")
                op_name = self.get_fs_operator_name("gh_diff_basis",
                                                    function_space)
                invoke_sub.add(AllocateGen(invoke_sub,
                                           op_name+"("+alloc_args+")"))
                # add diff basis function variable to list to declare later
                operator_declarations.append(op_name+"(:,:,:,:)")
        if var_list:
            # declare ndf and undf for all function spaces
            invoke_sub.add(DeclGen(invoke_sub, datatype="integer",
                                   entity_decls=var_list))
        if var_dim_list:
            # declare dim and diff_dim for all function spaces
            invoke_sub.add(DeclGen(invoke_sub, datatype="integer",
                                   entity_decls=var_dim_list))
        if operator_declarations:
            # declare the basis function operators
            invoke_sub.add(DeclGen(invoke_sub, datatype="real",
                                   allocatable=True,
                                   kind="r_def",
                                   entity_decls=operator_declarations))

        if self.is_coloured():
            # Add declarations of the colour map and array holding the
            # no. of cells of each colour
            invoke_sub.add(DeclGen(parent, datatype="integer",
                                   pointer=True,
                                   entity_decls=["cmap(:,:)",
                                                 "ncp_colour(:)"]))
            # Declaration of variable to hold the number of colours
            invoke_sub.add(DeclGen(parent, datatype="integer",
                                   entity_decls=["ncolour"]))

        if self.qr_required:
            # add calls to compute the values of any basis arrays
            invoke_sub.add(CommentGen(invoke_sub, ""))
            invoke_sub.add(CommentGen(invoke_sub, " Compute basis arrays"))
            invoke_sub.add(CommentGen(invoke_sub, ""))
            # only look at function spaces that are used by the
            # kernels in this invoke
            for function_space in self.unique_fss():
                # see if a basis function is needed for this function space
                if self.basis_required(function_space):
                    # Create the argument list
                    args = []
                    op_name = self.get_fs_operator_name("gh_basis",
                                                        function_space)
                    args.append(op_name)
                    args.append(get_fs_ndf_name(function_space))
                    args.extend(["nqp_h", "nqp_v", "xp", "zp"])
                    # find an appropriate field to access
                    arg = self.arg_for_funcspace(function_space)
                    name = arg.proxy_name_indexed
                    # insert the basis array call
                    invoke_sub.add(CallGen(invoke_sub,
                                           name=name + "%" +
                                           arg.ref_name(function_space) +
                                           "%compute_basis_function",
                                           args=args))
                if self.diff_basis_required(function_space):
                    # Create the argument list
                    args = []
                    op_name = self.get_fs_operator_name("gh_diff_basis",
                                                        function_space)
                    args.append(op_name)
                    args.append(get_fs_ndf_name(function_space))
                    args.extend(["nqp_h", "nqp_v", "xp", "zp"])
                    # find an appropriate field to access
                    arg = self.arg_for_funcspace(function_space)
                    name = arg.proxy_name_indexed
                    # insert the diff basis array call
                    invoke_sub.add(
                        CallGen(invoke_sub, name=name + "%" +
                                arg.ref_name(function_space) +
                                "%compute_diff_basis_function", args=args))
        invoke_sub.add(CommentGen(invoke_sub, ""))
        if config.DISTRIBUTED_MEMORY:
            invoke_sub.add(CommentGen(invoke_sub, " Call kernels and "
                                      "communication routines"))
        else:
            invoke_sub.add(CommentGen(invoke_sub, " Call our kernels"))
        invoke_sub.add(CommentGen(invoke_sub, ""))
        # add content from the schedule
        self.schedule.gen_code(invoke_sub)
        if self.qr_required:
            # deallocate all allocated basis function arrays
            invoke_sub.add(CommentGen(invoke_sub, ""))
            invoke_sub.add(CommentGen(invoke_sub, " Deallocate basis arrays"))
            invoke_sub.add(CommentGen(invoke_sub, ""))
            func_space_var_names = []
            # loop over all function spaces used by the kernels in this invoke
            for function_space in self.unique_fss():
                if self.basis_required(function_space):
                    # add the basis array name to the list to use later
                    op_name = self.get_fs_operator_name("gh_basis",
                                                        function_space)
                    func_space_var_names.append(op_name)
                if self.diff_basis_required(function_space):
                    # add the diff_basis array name to the list to use later
                    op_name = self.get_fs_operator_name("gh_diff_basis",
                                                        function_space)
                    func_space_var_names.append(op_name)
            # add the required deallocate call
            invoke_sub.add(DeallocateGen(invoke_sub, func_space_var_names))
        invoke_sub.add(CommentGen(invoke_sub, ""))
        # finally, add me to my parent
        parent.add(invoke_sub)


class DynSchedule(Schedule):
    ''' The Dynamo specific schedule class. This passes the Dynamo-
    specific factories for creating kernel and infrastructure calls
    to the base class so it creates the ones we require. '''

    def __init__(self, arg):
        from dynamo0p3_builtins import DynBuiltInCallFactory
        Schedule.__init__(self, DynKernCallFactory, DynBuiltInCallFactory, arg)

    def view(self, indent=0):
        '''a method implemented by all classes in a schedule which display the
        tree in a textual form. This method overrides the default view
        method to include distributed memory information '''
        print self.indent(indent) + "Schedule[invoke='" + self.invoke.name + \
            "' dm="+str(config.DISTRIBUTED_MEMORY)+"]"
        for entity in self._children:
            entity.view(indent=indent + 1)


class DynGlobalSum(GlobalSum):
    ''' Dynamo specific global sum class which can be added to and
    manipulated in, a schedule '''
    def __init__(self, scalar, parent=None):
        if not config.DISTRIBUTED_MEMORY:
            raise GenerationError("It makes no sense to create a DynGlobalSum "
                                  "object when dm=False")
        # a list of scalar types that this class supports
        self._supported_scalars = ["gh_real"]
        if scalar.type not in self._supported_scalars:
            raise GenerationError("DynGlobalSum currently only supports "
                                  "'{0}', but found '{1}'.".
                                  format(self._supported_scalars, scalar.type))
        GlobalSum.__init__(self, scalar, parent=parent)

    def gen_code(self, parent):
        ''' Dynamo specific code generation for this class '''
        from f2pygen import AssignGen, TypeDeclGen, UseGen
        name = self._scalar.name
        name_space_manager = NameSpaceFactory().create()
        sum_name = name_space_manager.create_name(
            root_name="global_sum", context="PSyVars", label="global_sum")
        parent.add(UseGen(parent, name="scalar_mod", only=True,
                          funcnames=["scalar_type"]))
        parent.add(TypeDeclGen(parent, datatype="scalar_type",
                               entity_decls=[sum_name]))
        parent.add(AssignGen(parent, lhs=sum_name+"%value", rhs=name))
        parent.add(AssignGen(parent, lhs=name, rhs=sum_name+"%get_sum()"))


class DynHaloExchange(HaloExchange):

    ''' Dynamo specific halo exchange class which can be added to and
    manipulated in, a schedule '''

    def __init__(self, field, check_dirty=True, parent=None,
                 vector_index=None, inc=False):

        self._vector_index = vector_index
        if field.descriptor.stencil:
            halo_type = field.descriptor.stencil['type']
            halo_depth = field.descriptor.stencil['extent']
            if not halo_depth:
                # halo_depth is provided by the algorithm layer
                halo_depth = stencil_extent_value(field)
            else:
                halo_depth = str(halo_depth)
            if inc:
                # there is an inc writer which needs redundant
                # computation so our halo depth must be increased by 1
                halo_depth += "+1"
        else:
            halo_type = 'region'
            halo_depth = "1"
        HaloExchange.__init__(self, field, halo_type, halo_depth,
                              check_dirty, parent=parent)

    def gen_code(self, parent):
        ''' Dynamo specific code generation for this class '''
        from f2pygen import IfThenGen, CallGen, CommentGen
        if self._vector_index:
            ref = "(" + str(self._vector_index) + ")"
        else:
            ref = ""
        if self._check_dirty:
            if_then = IfThenGen(parent, self._field.proxy_name + ref +
                                "%is_dirty(depth=" + self._halo_depth +
                                ")")
            parent.add(if_then)
            halo_parent = if_then
        else:
            halo_parent = parent
        halo_parent.add(
            CallGen(
                halo_parent, name=self._field.proxy_name + ref +
                "%halo_exchange(depth=" + self._halo_depth + ")"))
        parent.add(CommentGen(parent, ""))


class DynLoop(Loop):
    ''' The Dynamo specific Loop class. This passes the Dynamo
    specific loop information to the base class so it creates the one
    we require.  Creates Dynamo specific loop bounds when the code is
    being generated. '''

    def __init__(self, parent=None, loop_type=""):
        Loop.__init__(self, parent=parent,
                      valid_loop_types=VALID_LOOP_TYPES)
        self.loop_type = loop_type
        self._kern = None

        # Get the namespace manager instance so we can look-up
        # the name of the nlayers and ndf variables
        self._name_space_manager = NameSpaceFactory().create()

        # set our variable name at initialisation as it might be
        # required by other classes before code generation
        if self._loop_type == "colours":
            self._variable_name = "colour"
        elif self._loop_type == "colour":
            self._variable_name = "cell"
        elif self._loop_type == "dofs":
            self._variable_name = self._name_space_manager.\
                create_name(root_name="df",
                            context="PSyVars",
                            label="dof_loop_idx")
        else:
            self._variable_name = "cell"

        # At this stage we don't know what our loop bounds are
        self._lower_bound_name = None
        self._lower_bound_index = None
        self._upper_bound_name = None
        self._upper_bound_index = None

    def view(self, indent=0):
        ''' Print out a textual representation of this loop. We override
        this method from the Loop class because, in Dynamo0.3, the
        function space is now an object and we need to call orig_name on
        it '''
        print self.indent(indent) +\
            "Loop[type='{0}',field_space='{1}',it_space='{2}']".\
            format(self._loop_type, self._field_space.orig_name,
                   self.iteration_space)
        for entity in self._children:
            entity.view(indent=indent + 1)

    def load(self, kern):
        ''' Load the state of this Loop using the supplied Kernel
        object. This method is provided so that we can individually
        construct Loop objects for a given kernel call. '''
        self._kern = kern

        self._field = kern.arguments.iteration_space_arg()
        self._field_name = self._field.name
        self._field_space = self._field.function_space
        self._iteration_space = kern.iterates_over  # cells etc.

        # Loop bounds
        self.set_lower_bound("start")

        from dynamo0p3_builtins import DynBuiltIn
        if isinstance(kern, DynBuiltIn):
            # If the kernel is a built-in/pointwise operation
            # then this loop must be over DoFs
            self.set_upper_bound("dofs")
        else:
            if config.DISTRIBUTED_MEMORY:
                if self._field.type in VALID_OPERATOR_NAMES:
                    # We always compute operators redundantly out to the L1
                    # halo
                    self.set_upper_bound("halo", index=1)
                elif (self.field_space.orig_name in
                      DISCONTINUOUS_FUNCTION_SPACES):
                    self.set_upper_bound("edge")
                elif self.field_space.orig_name in CONTINUOUS_FUNCTION_SPACES:
                    # Must iterate out to L1 halo for continuous quantities
                    self.set_upper_bound("halo", index=1)
                elif self.field_space.orig_name in VALID_ANY_SPACE_NAMES:
                    # We don't know whether any-space is continuous or not
                    # so we have to err on the side of caution and assume that
                    # it is.
                    self.set_upper_bound("halo", index=1)
                else:
                    raise GenerationError(
                        "Unexpected function space found. Expecting one of "
                        "{0} but found '{1}'".format(
                            str(VALID_FUNCTION_SPACES),
                            self.field_space.orig_name))
            else:  # sequential
                self.set_upper_bound("cells")

    def set_lower_bound(self, name, index=None):
        ''' Set the lower bounds of this loop '''
        if name not in VALID_LOOP_BOUNDS_NAMES:
            raise GenerationError(
                "The specified lower bound loop name is invalid")
        if name in ["inner", "halo"] and index < 1:
            raise GenerationError(
                "The specified index '{0}' for this lower loop bound is "
                "invalid".format(str(index)))
        self._lower_bound_name = name
        self._lower_bound_index = index

    def set_upper_bound(self, name, index=None):
        ''' Set the upper bounds of this loop '''
        if name not in VALID_LOOP_BOUNDS_NAMES:
            raise GenerationError(
                "The specified upper bound loop name is invalid")
        if name == "start":
            raise GenerationError("'start' is not a valid upper bound")
        if name in ["inner", "halo"] and index < 1:
            raise GenerationError(
                "The specified index '{0}' for this upper loop bound is "
                "invalid".format(str(index)))
        self._upper_bound_name = name
        self._upper_bound_index = index

    @property
    def upper_bound_name(self):
        ''' Returns the name of the upper loop bound '''
        return self._upper_bound_name

    @property
    def upper_bound_index(self):
        ''' Returns the index of the upper loop bound. Is None if upper
        bound name is not "inner" or "halo" '''
        return self._upper_bound_index

    def _lower_bound_fortran(self):
        ''' Create the associated fortran code for the type of lower bound '''
        if not config.DISTRIBUTED_MEMORY and self._lower_bound_name != "start":
            raise GenerationError(
                "The lower bound must be 'start' if we are sequential but "
                "found '{0}'".format(self._upper_bound_name))
        if self._lower_bound_name == "start":
            return "1"
        else:
            # the start of our space is the end of the previous space +1
            if self._lower_bound_name == "inner":
                prev_space_name = self._lower_bound_name
                prev_space_index_str = str(self._lower_bound_index + 1)
            elif self._lower_bound_name == "edge":
                prev_space_name = "inner"
                prev_space_index_str = "1"
            elif (self._lower_bound_name == "halo" and
                  self._lower_bound_index == 1):
                prev_space_name = "edge"
                prev_space_index_str = ""
            elif (self._lower_bound_name == "halo" and
                  self._lower_bound_index > 1):
                prev_space_name = self._lower_bound_name
                prev_space_index_str = str(self._lower_bound_index - 1)
            else:
                raise GenerationError("Unsupported lower bound name found")
            mesh_obj_name = self._name_space_manager.create_name(
                root_name="mesh", context="PSyVars", label="mesh")
            return mesh_obj_name + "%get_last_" + prev_space_name + "_cell(" \
                + prev_space_index_str + ")+1"

    def _upper_bound_fortran(self):
        ''' Create the associated fortran code for the type of upper bound '''
        if self._upper_bound_name == "ncolours":
            return "ncolour"
        elif self._upper_bound_name == "ncolour":
            return "ncp_colour(colour)"
        elif self._upper_bound_name == "dofs":
            if config.DISTRIBUTED_MEMORY:
                result = self.field.proxy_name_indexed + "%" + \
                    self.field.ref_name() + "%get_last_dof_owned()"
            else:
                result = self._kern.undf_name
            return result
        elif not config.DISTRIBUTED_MEMORY:
            if self._upper_bound_name == "cells":
                result = self.field.proxy_name_indexed + "%" + \
                    self.field.ref_name() + "%get_ncell()"
            else:
                raise GenerationError(
                    "For sequential/shared-memory code, the upper loop "
                    "bound must be one of ncolours, ncolour, cells or dofs "
                    "but got '{0}'".format(self._upper_bound_name))
            return result
        else:
            if self._upper_bound_name in ["inner", "halo"]:
                index = self._upper_bound_index
            else:
                index = ""
            mesh_obj_name = self._name_space_manager.create_name(
                root_name="mesh", context="PSyVars", label="mesh")
            return mesh_obj_name + "%get_last_" + self._upper_bound_name + \
                "_cell(" + str(index) + ")"

    def has_inc_arg(self, mapping=None):
        ''' Returns True if any of the Kernels called within this loop
        have an argument with INC access. Returns False otherwise. '''
        if mapping is not None:
            my_mapping = mapping
        else:
            my_mapping = FIELD_ACCESS_MAP
        return Loop.has_inc_arg(self, my_mapping)

    def unique_fields_with_halo_reads(self):
        ''' Returns all fields in this loop that require at least some
        of their halo to be clean to work correctly. '''

        unique_fields = []
        unique_field_names = []

        for call in self.calls():
            for arg in call.arguments.args:
                if self._halo_read_access(arg):
                    if arg.name not in unique_field_names:
                        unique_field_names.append(arg.name)
                        unique_fields.append(arg)
        return unique_fields

    def _halo_read_access(self, arg):
        '''Determines whether this argument reads from the halo for this
        loop'''
        if arg.descriptor.stencil:
            if self._upper_bound_name not in ["halo", "edge"]:
                raise GenerationError(
                    "Loop bounds other than halo and edge are currently "
                    "unsupported. Found '{0}'.".format(self._upper_bound_name))
            return self._upper_bound_name in ["halo", "edge"]
        if arg.type in VALID_SCALAR_NAMES:
            # scalars do not have halos
            return False
        elif arg.type in VALID_OPERATOR_NAMES:
            # operators do not have halos
            return False
        elif arg.discontinuous and arg.access.lower() == "gh_read":
            # there are no shared dofs so access to inner and edge are
            # local so we only care about reads in the halo
            return self._upper_bound_name == "halo"
        elif arg.access.lower() in ["gh_read", "gh_inc"]:
            # it is either continuous or we don't know (any_space_x)
            # and we need to assume it may be continuous for
            # correctness. There may be shared dofs so only access to
            # inner is local so we care about reads in both the edge
            # (annexed dofs) and the halo
            return self._upper_bound_name in ["halo", "edge"]
        else:
            # access is neither a read nor an inc so does not need halo
            return False

    def gen_code(self, parent):
        ''' Work out the appropriate loop bounds and variable name
        depending on the loop type and then call the base class to
        generate the code. '''

        # Check that we're not within an OpenMP parallel region if
        # we are a loop over colours.
        if self._loop_type == "colours" and self.is_openmp_parallel():
            raise GenerationError("Cannot have a loop over "
                                  "colours within an OpenMP "
                                  "parallel region.")

        # get fortran loop bounds
        self._start = self._lower_bound_fortran()
        self._stop = self._upper_bound_fortran()
        Loop.gen_code(self, parent)

        if config.DISTRIBUTED_MEMORY and self._loop_type != "colour":
            # Set halo dirty for all fields that are modified
            from f2pygen import CallGen, CommentGen
            fields = self.unique_modified_args(FIELD_ACCESS_MAP, "gh_field")
            if fields:
                parent.add(CommentGen(parent, ""))
                parent.add(CommentGen(parent,
                                      " Set halos dirty for fields modified "
                                      "in the above loop"))
                parent.add(CommentGen(parent, ""))
                from psyGen import OMPParallelDoDirective
                from f2pygen import DirectiveGen
                use_omp_master = False
                if self.is_openmp_parallel():
                    if not self.ancestor(OMPParallelDoDirective):
                        use_omp_master = True
                        # I am within an OpenMP Do directive so protect
                        # set_dirty() with OpenMP Master
                        parent.add(DirectiveGen(parent, "omp", "begin",
                                                "master", ""))
                for field in fields:
                    if field.vector_size > 1:
                        # the range function below returns values from
                        # 1 to the vector size which is what we
                        # require in our Fortran code
                        for index in range(1, field.vector_size+1):
                            parent.add(CallGen(parent, name=field.proxy_name +
                                               "(" + str(index) +
                                               ")%set_dirty()"))
                    else:
                        parent.add(CallGen(parent, name=field.proxy_name +
                                           "%set_dirty()"))
                if use_omp_master:
                    # I am within an OpenMP Do directive so protect
                    # set_dirty() with OpenMP Master
                    parent.add(DirectiveGen(parent, "omp", "end",
                                            "master", ""))
                parent.add(CommentGen(parent, ""))


class DynKern(Kern):
    ''' Stores information about Dynamo Kernels as specified by the
    Kernel metadata and associated algorithm call. Uses this
    information to generate appropriate PSy layer code for the Kernel
    instance or to generate a Kernel stub'''

    def __init__(self):
        if False:  # pylint: disable=using-constant-test
            self._arguments = DynKernelArguments(None, None)  # for pyreverse
        self._func_descriptors = None
        self._fs_descriptors = None
        self._qr_required = False
        self._qr_text = ""
        self._qr_name = ""
        self._qr_args = None
        self._name_space_manager = NameSpaceFactory().create()
        self._cma_operation = ""

    def load(self, call, parent=None):
        ''' sets up kernel information with the call object which is
        created by the parser. This object includes information about
        the invoke call and the associated kernel'''
        self._setup_qr(call.ktype.func_descriptors)
        self._setup(call.ktype, call.module_name, call.args, parent)

    def load_meta(self, ktype):
        ''' sets up kernel information with the kernel type object
        which is created by the parser. The object includes the
        metadata describing the kernel code '''

        # create a name for each argument
        from parse import Arg
        args = []
        for idx, descriptor in enumerate(ktype.arg_descriptors):
            pre = None
            if descriptor.type.lower() == "gh_operator":
                pre = "op_"
            elif descriptor.type.lower() == "gh_columnwise_operator":
                pre = "cma_op_"
            elif descriptor.type.lower() == "gh_field":
                pre = "field_"
            elif descriptor.type.lower() == "gh_real":
                pre = "rscalar_"
            elif descriptor.type.lower() == "gh_integer":
                pre = "iscalar_"
            else:
                raise GenerationError(
                    "load_meta expected one of '{0}' but "
                    "found '{1}'".format(VALID_ARG_TYPE_NAMES,
                                         descriptor.type))
            args.append(Arg("variable", pre+str(idx+1)))

            if descriptor.stencil:
                if not descriptor.stencil["extent"]:
                    # stencil size (in cells) is passed in
                    args.append(Arg("variable",
                                    pre+str(idx+1)+"_stencil_size"))
                if descriptor.stencil["type"] == "xory1d":
                    # direction is passed in
                    args.append(Arg("variable", pre+str(idx+1)+"_direction"))

        # initialise qr so we can test whether it is required
        self._setup_qr(ktype.func_descriptors)
        if self._qr_required:
            # it is required so add a qr algorithm argument
            args.append(Arg("variable", "qr"))
        self._setup(ktype, "dummy_name", args, None)

    def _setup_qr(self, func_descriptors):
        ''' initialisation of the qr information. This may be needed before
        general setup so is computed in a separate method. '''
        self._qr_required = False
        for descriptor in func_descriptors:
            if len(descriptor.operator_names) > 0:
                self._qr_required = True
                break

    def _setup(self, ktype, module_name, args, parent):
        ''' internal setup of kernel information. '''
        from parse import KernelCall
        Kern.__init__(self, DynKernelArguments,
                      KernelCall(module_name, ktype, args),
                      parent, check=False)
        self._func_descriptors = ktype.func_descriptors
        # Keep a record of the type of CMA kernel identified when
        # parsing the kernel meta-data
        self._cma_operation = ktype.cma_operation
        self._fs_descriptors = FSDescriptors(ktype.func_descriptors)
        # dynamo 0.3 api kernels require quadrature rule arguments to be
        # passed in if one or more basis functions are used by the kernel.
        self._qr_args = {"nh": "nqp_h", "nv": "nqp_v", "h": "wh", "v": "wv"}

        # if there is a quadrature rule, what is the name of the
        # algorithm argument?
        self._qr_text = ""
        self._qr_name = ""
        if self._qr_required:
            qr_arg = args[-1]
            self._qr_text = qr_arg.text
            # use our namespace manager to create a unique name unless
            # the context and label match and in this case return the
            # previous name. We use the full text of the original
            # as a label.
            self._qr_name = self._name_space_manager.create_name(
                root_name=qr_arg.varName, context="AlgArgs",
                label=self._qr_text)

    @property
    def cma_operation(self):
        ''' Returns the type of CMA operation performed by this kernel
        (one of 'assembly', 'apply' or 'matrix-matrix') or None if the
        the kernel does not involve CMA operators '''
        return self._cma_operation

    @property
    def fs_descriptors(self):
        ''' Returns a list of function space descriptor objects of
        type FSDescriptor which contain information about the function
        spaces. '''
        return self._fs_descriptors

    @property
    def qr_required(self):
        ''' Returns True if this kernel makes use of a quadrature
        rule, else returns False. '''
        return self._qr_required

    @property
    def qr_text(self):
        ''' Returns the QR argument-text used by the algorithm layer
        in the calling argument list. '''
        return self._qr_text

    @property
    def qr_name(self):
        ''' Returns a Quadrature-rule name for this Kernel. '''
        return self._qr_name

    @property
    def qr_args(self):
        '''Returns a dictionary of generic qr names mapped to specific
        dynamo0.3 names'''
        return self._qr_args

    def local_vars(self):
        ''' Returns the names used by the Kernel that vary from one
        invocation to the next and therefore require privatisation
        when parallelised. '''
        lvars = []
        # Orientation maps
        for unique_fs in self.arguments.unique_fss:
            if self._fs_descriptors.exists(unique_fs):
                fs_descriptor = self._fs_descriptors.get_descriptor(unique_fs)
                if fs_descriptor.requires_orientation:
                    lvars.append(get_fs_orientation_name(unique_fs))
        return lvars

    @property
    def gen_stub(self):
        ''' output a kernel stub '''
        from f2pygen import ModuleGen, SubroutineGen

        # remove "_code" from the name if it exists to determine the
        # base name which (if dynamo0.3 naming conventions are
        # followed) is used as the root for the module and subroutine
        # names.
        if self.name.lower().endswith("_code"):
            base_name = self.name[:-5]
        else:
            # TODO: add a warning here when logging is added
            base_name = self.name

        # create an empty PSy layer module
        psy_module = ModuleGen(base_name+"_mod")

        # create the subroutine
        sub_stub = SubroutineGen(psy_module, name=base_name+"_code",
                                 implicitnone=True)
        # create the arglist and declarations
        create_arg_list = KernStubArgList(self, sub_stub)
        create_arg_list.generate()
        arglist = create_arg_list.arglist
        # add the arglist
        sub_stub.args = arglist
        # add the subroutine to the parent module
        psy_module.add(sub_stub)
        return psy_module.root

    @property
    def incremented_arg(self):
        ''' Returns the argument corresponding to a field or operator that has
        INC access.  '''
        return Kern.incremented_arg(self, FIELD_ACCESS_MAP)

    @property
    def written_arg(self):
        ''' Returns the argument corresponding to a field or operator that has
        WRITE access '''
        return Kern.written_arg(self, FIELD_ACCESS_MAP)

    @property
    def updated_arg(self):
        ''' Returns the kernel argument that is updated (incremented or
        written to) '''
        arg = None
        try:
            arg = self.incremented_arg
        except FieldNotFoundError:
            arg = self.written_arg
        return arg

    def gen_code(self, parent):
        ''' Generates dynamo version 0.3 specific psy code for a call to
            the dynamo kernel instance. '''
        from f2pygen import CallGen, DeclGen, AssignGen, UseGen, CommentGen, \
            IfThenGen
        parent.add(DeclGen(parent, datatype="integer",
                           entity_decls=["cell"]))

        # Check whether this kernel reads from an operator
        op_args = self.parent.args_filter(arg_types=VALID_OPERATOR_NAMES,
                                          arg_accesses=["gh_read"])
        if op_args:
            # It does. We must check that our parent loop does not
            # go beyond the L1 halo.
            if self.parent.upper_bound_name == "halo" and \
               self.parent.upper_bound_index > 1:
                raise GenerationError(
                    "Kernel '{0}' reads from an operator and therefore "
                    "cannot be used for cells beyond the level 1 halo. "
                    "However the containing loop goes out to level {1}".
                    format(self._name, self.parent.upper_bound_index))

        # If this kernel is being called from within a coloured
        # loop then we have to look-up the colour map
        if self.is_coloured():

            # Find which argument object the kernel writes to (either GH_INC
            # or GH_WRITE) in order to look-up the colour map
            arg = self.updated_arg
            # TODO Check whether this arg is gh_inc and if not, Warn that
            # we're colouring a kernel that has no field object with INC access

            new_parent, position = parent.start_parent_loop()
            # Add the look-up of the colouring map for this kernel
            # call
            new_parent.add(CommentGen(new_parent, ""),
                           position=["before", position])
            new_parent.add(CommentGen(new_parent, " Look-up colour map"),
                           position=["before", position])
            new_parent.add(CommentGen(new_parent, ""),
                           position=["before", position])
            name = arg.proxy_name_indexed + \
                "%" + arg.ref_name() + "%get_colours"
            new_parent.add(CallGen(new_parent,
                                   name=name,
                                   args=["ncolour", "ncp_colour", "cmap"]),
                           position=["before", position])
            new_parent.add(CommentGen(new_parent, ""),
                           position=["before", position])

            # We must look-up the cell index using the colour map rather than
            # use the current cell index directly
            cell_index = "cmap(colour, cell)"
        else:
            # This kernel call has not been coloured
            #  - is it OpenMP parallel, i.e. are we a child of
            # an OpenMP directive?
            if self.is_openmp_parallel():
                try:
                    # It is OpenMP parallel - does it have an argument
                    # with INC access?
                    arg = self.incremented_arg
                except FieldNotFoundError:
                    arg = None
                if arg:
                    raise GenerationError("Kernel {0} has an argument with "
                                          "INC access and therefore must "
                                          "be coloured in order to be "
                                          "parallelised with OpenMP".
                                          format(self._name))
            cell_index = "cell"

        parent.add(CommentGen(parent, ""))

        # orientation arrays initialisation and their declarations
        orientation_decl_names = []
        for unique_fs in self.arguments.unique_fss:
            if self._fs_descriptors.exists(unique_fs):
                fs_descriptor = self._fs_descriptors.get_descriptor(unique_fs)
                if fs_descriptor.requires_orientation:
                    field = self._arguments.get_arg_on_space(unique_fs)
                    oname = get_fs_orientation_name(unique_fs)
                    orientation_decl_names.append(oname+"(:) => null()")
                    parent.add(
                        AssignGen(parent, pointer=True,
                                  lhs=oname,
                                  rhs=field.proxy_name_indexed + "%" +
                                  field.ref_name(unique_fs) +
                                  "%get_cell_orientation(" +
                                  cell_index + ")"))
        if orientation_decl_names:
            parent.add(DeclGen(parent, datatype="integer", pointer=True,
                               entity_decls=orientation_decl_names))
            parent.add(CommentGen(parent, ""))

        # dump = True
        # if dump:
        #    new_parent, position = parent.start_parent_loop()
        #    create_dump = DinoWriters(self, new_parent, position)
        #    create_dump.generate()

        create_arg_list = KernCallArgList(self, parent)
        create_arg_list.generate()
        arglist = create_arg_list.arglist

        # generate the kernel call and associated use statement
        parent.add(CallGen(parent, self._name, arglist))
        if not self.module_inline:
            parent.add(UseGen(parent, name=self._module_name,
                              only=True, funcnames=[self._name]))
        # 5: Fix for boundary_dofs array in matrix_vector_code
        if self.name == "matrix_vector_code":
            # Any call to this kernel must be followed by a call
            # to update boundary conditions (if the updated field
            # is not on a discontinuous space).
            # Rather than rely on knowledge of the interface to
            # matrix_vector kernel, look-up the argument that is
            # updated and then apply b.c.'s to that...
            enforce_bc_arg = self.updated_arg
            # We only need to call the enforce_bc kernel if the field is on
            # a vector function space
            space_names = ["w1", "w2", "w2h", "w2v"]
            kern_func_space_name = enforce_bc_arg.function_space
            ndf_name = get_fs_ndf_name(kern_func_space_name)
            undf_name = get_fs_undf_name(kern_func_space_name)
            map_name = get_fs_map_name(kern_func_space_name)
            proxy_name = enforce_bc_arg.proxy_name
            self._name_space_manager = NameSpaceFactory().create()
            fs_name = self._name_space_manager.create_name(root_name="fs")
            boundary_dofs_name = self._name_space_manager.create_name(
                root_name="boundary_dofs")
            parent.add(UseGen(parent, name="function_space_mod",
                              only=True, funcnames=space_names))
            parent.add(DeclGen(parent, datatype="integer", pointer=True,
                               entity_decls=[boundary_dofs_name +
                                             "(:,:) => null()"]))
            parent.add(DeclGen(parent, datatype="integer",
                               entity_decls=[fs_name]))
            new_parent, position = parent.start_parent_loop()
            new_parent.add(AssignGen(new_parent, lhs=fs_name,
                                     rhs=enforce_bc_arg.name +
                                     "%which_function_space()"),
                           position=["before", position])
            test_str = " .or. ".join(
                [fs_name + " == " + space_name for space_name in
                 space_names])
            if_then = IfThenGen(new_parent, test_str)
            new_parent.add(if_then, position=["before", position])
            if_then.add(AssignGen(if_then, pointer=True,
                                  lhs=boundary_dofs_name,
                                  rhs=proxy_name +
                                  "%vspace%get_boundary_dofs()"))
            parent.add(CommentGen(parent, ""))
            if_then = IfThenGen(parent, test_str)
            parent.add(if_then)
            nlayers_name = self._name_space_manager.create_name(
                root_name="nlayers", context="PSyVars", label="nlayers")
            parent.add(UseGen(parent, name="enforce_bc_kernel_mod", only=True,
                              funcnames=["enforce_bc_code"]))
            if_then.add(CallGen(if_then, "enforce_bc_code",
                                [nlayers_name,
                                 enforce_bc_arg.proxy_name+"%data",
                                 ndf_name, undf_name,
                                 map_name+"(:,"+cell_index+")",
                                 boundary_dofs_name]))
            parent.add(CommentGen(parent, ""))


class ArgOrdering(object):
    '''Base class capturing the arguments, type and ordering of data in
    a Kernel call.'''
    def __init__(self, kern):
        self._kern = kern

    def generate(self):
        '''specifies which arguments appear in an argument list, their type
        and their ordering. Calls methods for each type of argument
        that can be specialised by a child class for its particular need'''
        if self._kern.arguments.has_operator():
            # All operator types require the cell index to be provided
            self.cell_position()
        # Pass the number of layers in the mesh unless this kernel is
        # applying a CMA operator or doing a CMA matrix-matrix calculation
        if self._kern.cma_operation not in ["apply", "matrix-matrix"]:
            self.mesh_height()
        # Pass the number of cells in the mesh if this kernel has a
        # LMA operator argument
        # TODO this code should replace the code that currently includes
        # this quantity for *every* operator it encounters.
        # if self._kern.arguments.has_operator(op_type="gh_operator"):
        #     self.mesh_ncell3d()
        # Pass the number of columns in the mesh if this kernel has a CMA
        # operator argument
        if self._kern.arguments.has_operator(op_type="gh_columnwise_operator"):
            self.mesh_ncell2d()

        # for each argument in the order they are specified in the
        # kernel metadata, call particular methods depending on what
        # type of argument we find (field, field vector, operator or
        # scalar). If the argument is a field or field vector and also
        # has a stencil access then also call appropriate stencil
        # methods.
        for arg in self._kern.arguments.args:
            if arg.type == "gh_field":
                if arg.vector_size > 1:
                    self.field_vector(arg)
                else:
                    self.field(arg)
                if arg.descriptor.stencil:
                    if not arg.descriptor.stencil['extent']:
                        # stencil extent is not provided in the
                        # metadata so must be passed
                        self.stencil_unknown_extent(arg)
                    if arg.descriptor.stencil['type'] == "xory1d":
                        # if "xory1d is specified then the actual
                        # direction must be passed
                        self.stencil_unknown_direction(arg)
                    # stencil information that is always passed
                    self.stencil(arg)
            elif arg.type == "gh_operator":
                self.operator(arg)
            elif arg.type == "gh_columnwise_operator":
                self.cma_operator(arg)
            elif arg.type in VALID_SCALAR_NAMES:
                self.scalar(arg)
            else:
                raise GenerationError(
                    "Unexpected arg type found in dynamo0p3.py:"
                    "ArgOrdering:generate(). Expected one of '{0}' "
                    "but found '{1}'".format(VALID_ARG_TYPE_NAMES, arg.type))
        # For each function space (in the order they appear in the
        # metadata arguments)
        for unique_fs in self._kern.arguments.unique_fss:
            # Provide compulsory arguments common to operators and
            # fields on a space.
            self.fs_compulsory(unique_fs)
            # Provide additional compulsory arguments if there is a
            # field on this space
            if field_on_space(unique_fs, self._kern.arguments):
                self.fs_compulsory_field(unique_fs)
            cma_op = cma_on_space(unique_fs, self._kern.arguments)
            if cma_op:
                if self._kern.cma_operation == "assembly":
                    # CMA-assembly requires banded dofmaps
                    self.banded_dofmap(unique_fs)
                elif self._kern.cma_operation == "apply":
                    # Applying a CMA operator requires indirection dofmaps
                    self.indirection_dofmap(unique_fs, operator=cma_op)

            # Provide any optional arguments. These arguments are
            # associated with the keyword arguments (basis function,
            # differential basis function and orientation) for a
            # function space.
            if self._kern.fs_descriptors.exists(unique_fs):
                descriptors = self._kern.fs_descriptors
                descriptor = descriptors.get_descriptor(unique_fs)
                if descriptor.requires_basis:
                    self.basis(unique_fs)
                if descriptor.requires_diff_basis:
                    self.diff_basis(unique_fs)
                if descriptor.requires_orientation:
                    self.orientation(unique_fs)
            # Fix for boundary_dofs array to the boundary condition
            # kernel (enforce_bc_kernel) arguments
            if self._kern.name.lower() == "enforce_bc_code" and \
               unique_fs.orig_name.lower() == "any_space_1":
                self.bc_kernel(unique_fs)
        # Provide qr arguments if required
        if self._kern.qr_required:
            self.quad_rule()

    def cell_position(self):
        ''' add cell position information'''
        raise NotImplementedError(
            "Error: ArgOrdering.cell_position() must be implemented by "
            "subclass")

    def mesh_height(self):
        ''' add height information'''
        raise NotImplementedError(
            "Error: ArgOrdering.mesh_height() must be implemented by subclass")

    def mesh_ncell2d(self):
        ''' Add the number of columns in the mesh '''
        raise NotImplementedError(
            "Error: ArgOrdering.mesh_ncell2d() must be implemented by"
            "subclass")

    def cma_operator(self, arg):
        ''' Add information on the CMA operator '''
        raise NotImplementedError("Error: ArgOrdering.cma_operator() must "
                                  "be implemented by subclass")

    def field_vector(self, arg):
        ''' add field-vector information for this field-vector argument '''
        raise NotImplementedError(
            "Error: ArgOrdering.field_vector() must be implemented by "
            "subclass")

    def field(self, arg):
        ''' add field information for this field argument '''
        raise NotImplementedError(
            "Error: ArgOrdering.field() must be implemented by subclass")

    def stencil_unknown_extent(self, arg):
        ''' add stencil extent information for this stencil argument '''
        raise NotImplementedError(
            "Error: ArgOrdering.stencil_unknown_extent() must be implemented "
            "by subclass")

    def stencil_unknown_direction(self, arg):
        ''' add stencil direction information for this stencil argument '''
        raise NotImplementedError(
            "Error: ArgOrdering.stencil_unknown_direction() must be "
            "implemented by subclass")

    def stencil(self, arg):
        ''' add stencil information for this stencil argument '''
        raise NotImplementedError(
            "Error: ArgOrdering.stencil() must be implemented by subclass")

    def operator(self, arg):
        ''' add operator information for this operator argument '''
        raise NotImplementedError(
            "Error: ArgOrdering.operator() must be implemented by subclass")

    def scalar(self, arg):
        ''' add scalar information for this scalar argument '''
        raise NotImplementedError(
            "Error: ArgOrdering.scalar() must be implemented by subclass")

    def fs_compulsory(self, function_space):
        '''add compulsory information common to operators and fieldsfor this
        function space'''
        raise NotImplementedError(
            "Error: ArgOrdering.fs_compulsory() must be implemented by "
            "subclass")

    def fs_compulsory_field(self, function_space):
        '''add compulsory information for this function space'''
        raise NotImplementedError(
            "Error: ArgOrdering.fs_compulsory_field() must be implemented "
            "by subclass")

    def basis(self, function_space):
        '''add basis function information for this function space '''
        raise NotImplementedError(
            "Error: ArgOrdering.basis() must be implemented by subclass")

    def diff_basis(self, function_space):
        '''add differential basis function information for this function
        space'''
        raise NotImplementedError(
            "Error: ArgOrdering.diff_basis() must be implemented by subclass")

    def orientation(self, function_space):
        '''add orientation information for this function space'''
        raise NotImplementedError(
            "Error: ArgOrdering.orientation() must be implemented by subclass")

    def bc_kernel(self, function_space):
        '''add boundary condition information for this function space'''
        raise NotImplementedError(
            "Error: ArgOrdering.bc_kernel() must be implemented by subclass")

    def quad_rule(self):
        '''add qr information'''
        raise NotImplementedError(
            "Error: ArgOrdering.quad_rule() must be implemented by subclass")

    def banded_dofmap(self, function_space):
        ''' Add banded dofmap (required for CMA operator assembly) '''
        raise NotImplementedError("Error: ArgOrdering.banded_dofmap() must"
                                  " be implemented by subclass")

    def indirection_dofmap(self, arg, operator=None):
        ''' Add indirection dofmap required when applying a CMA operator '''
        raise NotImplementedError("Error: ArgOrdering.indirection_dofmap() "
                                  "must be implemented by subclass")


class KernCallArgList(ArgOrdering):
    '''Creates the argument list required to call kernel "kern" from the
    PSy-layer. The ordering and type of arguments is captured by the base
    class '''
    def __init__(self, kern, parent=None):
        ArgOrdering.__init__(self, kern)
        self._parent = parent
        self._arglist = []
        self._name_space_manager = NameSpaceFactory().create()

    def cell_position(self):
        ''' add a cell argument to the argument list'''
        self._arglist.append(self._cell_ref_name)

    def mesh_height(self):
        ''' add mesh height (nlayers) to the argument list'''
        nlayers_name = self._name_space_manager.create_name(
            root_name="nlayers", context="PSyVars", label="nlayers")
        self._arglist.append(nlayers_name)

    # TODO uncomment this method when ensuring we only pass ncell3d once
    # to any given kernel.
    # def mesh_ncell3d(self):
    #     ''' Add the number of cells in the full 3D mesh to the argument
    #     list '''
    #     ncell3d_name = self._name_space_manager.create_name(
    #         root_name="ncell_3d", context="PSyVars", label="ncell3d")
    #     self._arglist.append(ncell3d_name)

    def mesh_ncell2d(self):
        ''' Add the number of columns in the mesh to the argument list '''
        ncell2d_name = self._name_space_manager.create_name(
            root_name="ncell_2d", context="PSyVars", label="ncell_2d")
        self._arglist.append(ncell2d_name)

    def field_vector(self, argvect):
        '''add the field vector associated with the argument 'argvect' to the
        argument list '''
        # the range function below returns values from
        # 1 to the vector size which is what we
        # require in our Fortran code
        for idx in range(1, argvect.vector_size+1):
            text = argvect.proxy_name + "(" + str(idx) + ")%data"
            self._arglist.append(text)

    def field(self, arg):
        '''add the field array associated with the argument 'arg' to the
        argument list'''
        text = arg.proxy_name + "%data"
        self._arglist.append(text)

    def stencil_unknown_extent(self, arg):
        '''add stencil information to the argument list associated with the
        argument 'arg' if the extent is unknown'''
        # the extent is not specified in the metadata
        # so pass the value in
        name = stencil_size_name(arg)
        self._arglist.append(name)

    def stencil_unknown_direction(self, arg):
        '''add stencil information to the argument list associated with the
        argument 'arg' if the direction is unknown'''
        # the direction of the stencil is not known so pass the value in
        name = arg.stencil.direction_arg.varName
        self._arglist.append(name)

    def stencil(self, arg):
        '''add general stencil information associated with the argument 'arg'
        to the argument list'''
        # add in stencil dofmap
        var_name = stencil_dofmap_name(arg)
        name = var_name + "(:,:," + self._cell_ref_name + ")"
        self._arglist.append(name)

    def operator(self, arg):
        ''' add the operator arguments to the argument list '''
        # TODO we should only be including ncell_3d once in the argument
        # list but this adds it for every operator
        self._arglist.append(arg.proxy_name_indexed+"%ncell_3d")
        self._arglist.append(arg.proxy_name_indexed+"%local_stencil")

    def cma_operator(self, arg):
        ''' add the CMA operator and associated scalars to the argument
        list '''
        if arg.function_space_to.orig_name != \
           arg.function_space_from.orig_name:
            components = ["matrix"] + DynInvokeCMAOperators.cma_diff_fs_params
        else:
            components = ["matrix"] + DynInvokeCMAOperators.cma_same_fs_params
        for component in components:
            self._arglist.append(
                self._name_space_manager.create_name(
                    root_name=arg.name+"_"+component,
                    context="PSyVars",
                    label=arg.name+"_"+component))

    def scalar(self, scalar_arg):
        '''add the name associated with the scalar argument to the argument
        list'''
        self._arglist.append(scalar_arg.name)

    def fs_compulsory(self, function_space):
        '''add compulsory arguments common to operators and
        fields on a space.'''
        if self._kern.cma_operation not in ["matrix-matrix"]:
            # There is currently one compulsory argument: "ndf" but only
            # if this is not a CMA-related kernel
            ndf_name = get_fs_ndf_name(function_space)
            self._arglist.append(ndf_name)

    def fs_compulsory_field(self, function_space):
        '''add compulsory arguments to the argument list, when there is a
        field on this function space'''
        undf_name = get_fs_undf_name(function_space)
        self._arglist.append(undf_name)
        map_name = get_fs_map_name(function_space)
        self._arglist.append(map_name+"(:,"+self._cell_ref_name+")")

    def basis(self, function_space):
        '''add basis function information for this function space to the
        argument list'''
        basis_name = get_fs_basis_name(function_space)
        self._arglist.append(basis_name)

    def diff_basis(self, function_space):
        '''add differential basis information for the function space to the
        argument list'''
        diff_basis_name = get_fs_diff_basis_name(function_space)
        self._arglist.append(diff_basis_name)

    def orientation(self, function_space):
        '''add orientation information for this function space to the
        argument list'''
        orientation_name = get_fs_orientation_name(function_space)
        self._arglist.append(orientation_name)

    def bc_kernel(self, function_space):
        ''' implement the boundary_dofs array fix '''
        from f2pygen import DeclGen, AssignGen
        self._arglist.append("boundary_dofs")
        parent = self._parent
        parent.add(DeclGen(parent, datatype="integer",
                           pointer=True, entity_decls=[
                               "boundary_dofs(:,:) => null()"]))
        fspace = None
        for fspace in self._kern.arguments.unique_fss:
            if fspace.orig_name == "any_space_1":
                break
        farg = self._kern.arguments.get_arg_on_space(fspace)
        # Sanity check - expect the enforce_bc_code kernel to only have
        # a field argument.
        if farg.type != "gh_field":
            raise GenerationError(
                "Expected a gh_field from which to look-up boundary dofs "
                "for kernel {0} but got {1}".format(self._kern.name,
                                                    farg.type))
        new_parent, position = parent.start_parent_loop()
        new_parent.add(AssignGen(new_parent, pointer=True,
                                 lhs="boundary_dofs",
                                 rhs=farg.proxy_name +
                                 "%vspace%get_boundary_dofs()"),
                       position=["before", position])

    def quad_rule(self):
        ''' add qr information to the argument list'''
        self._arglist.extend([self._kern.qr_args["nh"],
                              self._kern.qr_args["nv"],
                              self._kern.qr_args["h"],
                              self._kern.qr_args["v"]])

    def banded_dofmap(self, function_space):
        ''' Add banded dofmap (required for CMA operator assembly) '''
        # Note that the necessary ndf values will already have been added
        # to the argument list as they are mandatory for every function
        # space that appears in the meta-data.
        self._arglist.append(get_cbanded_map_name(function_space))

    def indirection_dofmap(self, function_space, operator=None):
        ''' Add indirection dofmap required when applying a CMA operator '''
        self._arglist.append(get_cma_indirection_map_name(function_space))

    @property
    def arglist(self):
        '''return the kernel argument list. The generate function must be
        called first'''
        if not self._arglist:
            raise GenerationError(
                "Internal error. The argument list in KernCallArgList:"
                "arglist() is empty. Has the generate() method been called?")
        return self._arglist

    @property
    def _cell_ref_name(self):
        '''utility routine which determines whether to return the cell value
        or the colourmap lookup value '''
        if self._kern.is_coloured():
            return "cmap(colour, cell)"
        else:
            return "cell"


class KernStubArgList(ArgOrdering):
    '''Creates the argument list required to create and declare the
    required arguments for a kernel subroutine.  The ordering and type
    of the arguments is captured by the base class '''
    def __init__(self, kern, parent):

        from f2pygen import UseGen
        parent.add(UseGen(parent, name="constants_mod", only=True,
                          funcnames=["r_def"]))
        self._first_arg = True
        self._first_arg_decl = None
        ArgOrdering.__init__(self, kern)
        self._parent = parent
        self._arglist = []
        self._name_space_manager = NameSpaceFactory().create()

    def cell_position(self):
        ''' Add cell position to the argument list if required '''
        from f2pygen import DeclGen
        self._arglist.append("cell")
        self._parent.add(DeclGen(self._parent, datatype="integer", intent="in",
                                 entity_decls=["cell"]))

    def mesh_height(self):
        ''' add mesh height (nlayers) to the argument list if required '''
        from f2pygen import DeclGen
        self._arglist.append("nlayers")
        self._parent.add(DeclGen(self._parent, datatype="integer", intent="in",
                                 entity_decls=["nlayers"]))

    def mesh_ncell2d(self):
        ''' Add the number of columns in the mesh to the argument list if
        required '''
        from f2pygen import DeclGen
        self._arglist.append("ncell_2d")
        self._parent.add(DeclGen(self._parent, datatype="integer", intent="in",
                                 entity_decls=["ncell_2d"]))

    def field_vector(self, argvect):
        '''add the field vector associated with the argument 'argvect' to the
        argument list '''
        undf_name = get_fs_undf_name(argvect.function_space)
        from f2pygen import DeclGen
        # the range function below returns values from
        # 1 to the vector size which is what we
        # require in our Fortran code
        for idx in range(1, argvect.vector_size+1):
            text = (argvect.name + "_" +
                    argvect.function_space.mangled_name +
                    "_v" + str(idx))
            intent = argvect.intent
            decl = DeclGen(self._parent, datatype="real",
                           kind="r_def", dimension=undf_name,
                           intent=intent, entity_decls=[text])
            self._parent.add(decl)
            if self._first_arg:
                self._first_arg = False
                self._first_arg_decl = decl
            self._arglist.append(text)

    def field(self, arg):
        '''add the field associated with the argument 'arg' to the argument
        list'''
        from f2pygen import DeclGen
        undf_name = get_fs_undf_name(arg.function_space)
        text = arg.name + "_" + arg.function_space.mangled_name
        intent = arg.intent
        decl = DeclGen(self._parent, datatype="real",
                       kind="r_def", dimension=undf_name,
                       intent=intent, entity_decls=[text])
        self._parent.add(decl)
        if self._first_arg:
            self._first_arg = False
            self._first_arg_decl = decl
        self._arglist.append(text)

    def stencil_unknown_extent(self, arg):
        '''add stencil information associated with the argument 'arg' if the
        extent is unknown'''
        from f2pygen import DeclGen
        name = arg.name + "_stencil_size"
        self._parent.add(DeclGen(self._parent, datatype="integer", intent="in",
                                 entity_decls=[name]))
        self._arglist.append(name)

    def stencil_unknown_direction(self, arg):
        '''add stencil information associated with the argument 'arg' if the
        direction is unknown'''
        from f2pygen import DeclGen
        name = arg.name+"_direction"
        self._parent.add(DeclGen(self._parent, datatype="integer", intent="in",
                                 entity_decls=[name]))
        self._arglist.append(name)

    def stencil(self, arg):
        '''add general stencil information associated with the argument
        'arg' '''
        from f2pygen import DeclGen
        name = arg.name+"_stencil_map"
        ndf_name = get_fs_ndf_name(arg.function_space)
        self._parent.add(DeclGen(self._parent, datatype="integer", intent="in",
                                 dimension=",".join(
                                     [ndf_name, arg.name + "_stencil_size"]),
                                 entity_decls=[name]))
        self._arglist.append(name)

    def operator(self, arg):
        ''' add the operator arguments to the argument list '''
        from f2pygen import DeclGen
        size = arg.name + "_ncell_3d"
        self._arglist.append(size)
        decl = DeclGen(self._parent, datatype="integer", intent="in",
                       entity_decls=[size])
        self._parent.add(decl)
        # If this is the first argument in the kernel then keep a
        # note so that we can put subsequent declarations in the
        # correct location
        if self._first_arg:
            self._first_arg = False
            self._first_arg_decl = decl
        text = arg.name
        self._arglist.append(text)

        intent = arg.intent
        ndf_name_to = get_fs_ndf_name(arg.function_space_to)
        ndf_name_from = get_fs_ndf_name(arg.function_space_from)
        self._parent.add(DeclGen(self._parent, datatype="real", kind="r_def",
                                 dimension=",".join([ndf_name_to,
                                                     ndf_name_from, size]),
                                 intent=intent, entity_decls=[text]))

    def cma_operator(self, arg):
        ''' add the CMA operator arguments to the argument list '''
        from f2pygen import DeclGen
        # The CMA operator itself
        self._arglist.append(arg.name)
        # Associated scalar parameters
        nrow = arg.name + "_nrow"
        _local_args = [nrow]
        if arg.function_space_to.orig_name != \
           arg.function_space_from.orig_name:
            # If the to- and from-spaces are the same then so are ncol and
            # nrow so we only pass one of them
            ncol = arg.name + "_ncol"
            _local_args.append(ncol)
        bandwidth = arg.name + "_bandwidth"
        alpha = arg.name + "_alpha"
        beta = arg.name + "_beta"
        gamma_m = arg.name + "_gamma_m"
        gamma_p = arg.name + "_gamma_p"
        _local_args += [bandwidth, alpha, beta, gamma_m, gamma_p]
        self._arglist += _local_args

        intent = arg.intent
        # Declare the associated scalar arguments before the array because
        # some of them are used to dimension the latter (and some compilers
        # get upset if this ordering is not followed)
        self._parent.add(DeclGen(self._parent, datatype="integer",
                                 intent="in",
                                 entity_decls=_local_args))
        # Declare the array that holds the CMA operator
        # If this is the first argument in the kernel then keep a
        # note so that we can put subsequent declarations in the
        # correct location
        decl = DeclGen(self._parent, datatype="real", kind="r_def",
                       dimension=",".join([bandwidth,
                                           nrow, "ncell_2d"]),
                       intent=intent, entity_decls=[arg.name])
        self._parent.add(decl)
        if self._first_arg:
            self._first_arg = False
            self._first_arg_decl = decl

    def banded_dofmap(self, function_space):
        ''' Declare the banded dofmap required for a CMA operator
        that maps to/from the specified function space '''
        from f2pygen import DeclGen
        ndf = get_fs_ndf_name(function_space)
        dofmap = get_cbanded_map_name(function_space)
        self._parent.add(DeclGen(self._parent, datatype="integer",
                                 dimension=",".join([ndf, "nlayers"]),
                                 intent="in",
                                 entity_decls=[dofmap]))
        self._arglist.append(dofmap)

    def indirection_dofmap(self, function_space, operator=None):
        ''' Declare the indirection dofmaps required when applying a
        CMA operator '''
        from f2pygen import DeclGen
        if not operator:
            raise GenerationError("Internal error: no CMA operator supplied.")
        if operator.type != "gh_columnwise_operator":
            raise GenerationError(
                "Internal error: a CMA operator (gh_columnwise_operator) must "
                "be supplied but got {0}".format(operator.type))
        # If a kernel applies a CMA operator then it must only have a
        # single such operator amongst its arguments
        if operator.function_space_to.orig_name == function_space.orig_name:
            dim_name = operator.name + "_nrow"
        else:
            dim_name = operator.name + "_ncol"
        map_name = get_cma_indirection_map_name(function_space)
        self._parent.add(DeclGen(self._parent, datatype="integer",
                                 dimension=dim_name,
                                 intent="in",
                                 entity_decls=[map_name]))
        self._arglist.append(map_name)

    def scalar(self, arg):
        '''add the name associated with the scalar argument'''
        from f2pygen import DeclGen
        if arg.type == "gh_real":
            decl = DeclGen(self._parent, datatype="real", kind="r_def",
                           intent=arg.intent,
                           entity_decls=[arg.name])
        elif arg.type == "gh_integer":
            decl = DeclGen(self._parent, datatype="integer",
                           intent=arg.intent,
                           entity_decls=[arg.name])
        else:
            raise GenerationError(
                "Internal error: expected arg type to be one "
                "of '{0}' but got '{1}'".format(VALID_SCALAR_NAMES,
                                                arg.type))
        self._parent.add(decl)
        self._arglist.append(arg.name)

    def fs_compulsory(self, function_space):
        ''' Provide compulsory arguments common to operators and
        fields on a space. There is one: "ndf". The only exception to
        this are CMA-matrix-matrix kenels. '''
        from f2pygen import DeclGen
        if self._kern.cma_operation not in ["matrix-matrix"]:
            ndf_name = get_fs_ndf_name(function_space)
            self._arglist.append(ndf_name)
            self._parent.add(
                DeclGen(self._parent, datatype="integer", intent="in",
                        entity_decls=[ndf_name]),
                position=["before", self._first_arg_decl.root])

    def fs_compulsory_field(self, function_space):
        ''' Provide compulsory arguments if there is a field on this
        function space'''
        from f2pygen import DeclGen
        ndf_name = get_fs_ndf_name(function_space)
        undf_name = get_fs_undf_name(function_space)
        self._arglist.append(undf_name)
        map_name = get_fs_map_name(function_space)
        self._arglist.append(map_name)
        # ndf* declarations need to be before argument
        # declarations as some compilers don't like
        # declarations after they have been used. We place
        # ndf* before the first argument declaration
        # (field or operator) (rather than after nlayers)
        # as this keeps the declarations in the order
        # specified in the metadata and first used by
        # fields/operators.
        self._parent.add(DeclGen(self._parent, datatype="integer", intent="in",
                                 entity_decls=[undf_name]),
                         position=["before", self._first_arg_decl.root])
        self._parent.add(DeclGen(self._parent, datatype="integer", intent="in",
                                 dimension=ndf_name,
                                 entity_decls=[map_name]))

    def basis(self, function_space):
        ''' provide basis function information for the function space '''
        from f2pygen import DeclGen
        basis_name = get_fs_basis_name(function_space)
        ndf_name = get_fs_ndf_name(function_space)
        self._arglist.append(basis_name)
        # the size of the first dimension for a
        # basis array depends on the
        # function space. The values are
        # w0=1, w1=3, w2=3, w3=1, wtheta=1, w2h=3, w2v=3
        first_dim = None
        if function_space.orig_name.lower() in \
           ["w0", "w3", "wtheta"]:
            first_dim = "1"
        elif (function_space.orig_name.lower() in
              ["w1", "w2", "w2h", "w2v"]):
            first_dim = "3"
        else:
            raise GenerationError(
                "Unsupported space for basis function, "
                "expecting one of {0} but found "
                "'{1}'".format(VALID_FUNCTION_SPACES,
                               function_space.orig_name))
        self._parent.add(DeclGen(self._parent, datatype="real",
                                 kind="r_def", intent="in",
                                 dimension=",".join(
                                     [first_dim, ndf_name,
                                      self._kern.qr_args["nh"],
                                      self._kern.qr_args["nv"]]),
                                 entity_decls=[basis_name]))

    def diff_basis(self, function_space):
        '''provide differential basis function information for the function
        space'''
        from f2pygen import DeclGen
        ndf_name = get_fs_ndf_name(function_space)
        diff_basis_name = get_fs_diff_basis_name(function_space)
        self._arglist.append(diff_basis_name)
        # the size of the first dimension for a
        # differential basis array depends on the
        # function space. The values are
        # w0=3, w1=3, w2=1, w3=3, wtheta=3, w2h=1, w2v=1
        first_dim = None
        if function_space.orig_name.lower() in \
           ["w2", "w2h", "w2v"]:
            first_dim = "1"
        elif (function_space.orig_name.lower() in
              ["w0", "w1", "w3", "wtheta"]):
            first_dim = "3"
        else:
            raise GenerationError(
                "Unsupported space for differential basis "
                "function, expecting one of {0} but found "
                "'{1}'".format(VALID_FUNCTION_SPACES,
                               function_space.orig_name))
        self._parent.add(DeclGen(self._parent, datatype="real",
                                 kind="r_def", intent="in",
                                 dimension=",".join(
                                     [first_dim, ndf_name,
                                      self._kern.qr_args["nh"],
                                      self._kern.qr_args["nv"]]),
                                 entity_decls=[diff_basis_name]))

    def orientation(self, function_space):
        ''' provide orientation information for the function space '''
        from f2pygen import DeclGen
        ndf_name = get_fs_ndf_name(function_space)
        orientation_name = get_fs_orientation_name(function_space)
        self._arglist.append(orientation_name)
        self._parent.add(DeclGen(self._parent, datatype="integer",
                                 intent="in", dimension=ndf_name,
                                 entity_decls=[orientation_name]))

    def bc_kernel(self, function_space):
        ''' implement the boundary_dofs array fix '''
        from f2pygen import DeclGen
        self._arglist.append("boundary_dofs")
        ndf_name = get_fs_ndf_name(function_space)
        self._parent.add(DeclGen(self._parent, datatype="integer", intent="in",
                                 dimension=",".join([ndf_name, "2"]),
                                 entity_decls=["boundary_dofs"]))

    def quad_rule(self):
        ''' provide qr information '''
        from f2pygen import DeclGen
        self._arglist.extend([self._kern.qr_args["nh"],
                              self._kern.qr_args["nv"],
                              self._kern.qr_args["h"],
                              self._kern.qr_args["v"]])
        self._parent.add(DeclGen(self._parent, datatype="integer", intent="in",
                                 entity_decls=[self._kern.qr_args["nh"],
                                               self._kern.qr_args["nv"]]))
        self._parent.add(DeclGen(self._parent, datatype="real", kind="r_def",
                                 intent="in",
                                 dimension=self._kern.qr_args["nh"],
                                 entity_decls=[self._kern.qr_args["h"]]))
        self._parent.add(DeclGen(self._parent, datatype="real", kind="r_def",
                                 intent="in",
                                 dimension=self._kern.qr_args["nv"],
                                 entity_decls=[self._kern.qr_args["v"]]))

    @property
    def arglist(self):
        '''return the kernel argument list. The generate function must be
        called first'''
        if not self._arglist:
            raise GenerationError(
                "Internal error. The argument list in KernStubArgList:"
                "arglist() is empty. Has the generate() method been called?")
        return self._arglist


# class DinoWriters(ArgOrdering):
#    '''Creates the required writers to dump the state of a Kernel call
#    using dino. The integers are output first, followed by the fields,
#    arrays etc'''
#    def __init__(self, kern, parent=None, position=None):
#        ArgOrdering.__init__(self, kern)
#        self._parent = parent
#        self._position = position
#        self._name_space_manager = NameSpaceFactory().create()
#        self._scalar_position = None
#        self._array_position = None
#
#    def cell_position(self):
#        ''' get dino to output cell position information '''
#        # dino outputs a full field so we do not need cell index information
#        pass
#
#    def mesh_height(self):
#        ''' get dino to output the height of the mesh (nlayers)'''
#        nlayers_name = self._name_space_manager.create_name(
#            root_name="nlayers", context="PSyVars", label="nlayers")
#        self._add_dino_scalar(nlayers_name)
#
#    def field_vector(self, argvect):
#        '''get dino to output field vector data associated with the argument
#        'argvect' '''
#        # TBD
#        pass
#
#    def field(self, arg):
#        '''get dino to output field datat associated with the argument
#        'arg' '''
#        if arg.intent in ["in", "inout"]:
#            text = arg.proxy_name + "%data"
#            self._add_dino_array(text)
#
#    def stencil_unknown_extent(self, arg):
#        '''get dino to output stencil information associated with the argument
#        'arg' if the extent is unknown '''
#        # TBD
#        pass
#
#    def stencil_unknown_direction(self, arg):
#        '''get dino to output stencil information associated with the argument
#        'arg' if the direction is unknown '''
#        # TBD
#        pass
#
#    def stencil(self, arg):
#        '''get dino to output general stencil information associated with the
#        argument 'arg' '''
#        # TBD
#        pass
#
#    def operator(self, arg):
#        ''' get dino to output the operator arguments '''
#        # TBD
#        pass
#
#    def scalar(self, scalar_arg):
#        '''get dino to output the value of the scalar argument'''
#        if scalar_arg in ["in", "inout"]:
#            self._add_dino_scalar(scalar_arg.name)
#
#    def fs_compulsory(self, function_space):
#        '''get dino to output any compulsory arguments common to operators and
#        fields on a space. '''
#        # There is currently one: "ndf".
#        ndf_name = get_fs_ndf_name(function_space)
#        self._add_dino_scalar(ndf_name)
#
#    def fs_compulsory_field(self, function_space):
#        '''get dino to output compulsory arguments if there is a field on this
#        function space'''
#        undf_name = get_fs_undf_name(function_space)
#        self._add_dino_scalar(undf_name)
#
#    def basis(self, function_space):
#        '''get dino to output basis function information for the function
#        space'''
#        # TBD
#        pass
#
#    def diff_basis(self, function_space):
#        '''get dino to output differential basis function information for the
#        function space'''
#        # TBD
#         pass
#
#    def orientation(self, function_space):
#        '''get dino to output orientation information for the function
#        space'''
#        # TBD
#        pass
#
#    def bc_kernel(self, function_space):
#        '''get dino to output any boundary_dofs information for bc_kernel'''
#        # TBD
#        pass
#
#    def quad_rule(self):
#        '''get dino to output qr information '''
#        # TBD
#        pass
#
#    def generate(self):
#        '''perform any additional actions before and after kernel
#        argument-list based generation'''
#        from f2pygen import CommentGen
#        self._parent.add(CommentGen(self._parent, " dino output start"),
#                         position=["before", self._position])
#        scalar_comment = CommentGen(self._parent, " dino scalars")
#        self._parent.add(scalar_comment,
#                         position=["before", self._position])
#        array_comment = CommentGen(self._parent, " dino arrays")
#        self._parent.add(array_comment,
#                         position=["before", self._position])
#        self._scalar_position = scalar_comment.root
#        self._array_position = array_comment.root
#        self._parent.add(CommentGen(self._parent, " dino output end"),
#                         position=["before", self._position])
#        self._parent.add(CommentGen(self._parent, ""),
#                         position=["before", self._position])
#        ArgOrdering.generate(self)
#
#    def _add_dino_scalar(self, name):
#        ''' add a dino output call for a scalar variable '''
#        from f2pygen import CallGen
#        self._parent.add(CallGen(self._parent, name="dino%output_scalar",
#                                 args=[name]),
#                         position=["after", self._scalar_position])
#
#    def _add_dino_array(self, name):
#        ''' add a dino output call for an array variable '''
#        from f2pygen import CallGen
#        self._parent.add(CallGen(self._parent, name="dino%output_array",
#                                 args=[name]),
#                         position=["after", self._array_position])


class FSDescriptor(object):
    ''' Provides information about a particular function space used by
    a meta-funcs entry in the kernel metadata. '''

    def __init__(self, descriptor):
        self._descriptor = descriptor

    @property
    def requires_basis(self):
        ''' Returns True if a basis function is associated with this
        function space, otherwise it returns False. '''
        return "gh_basis" in self._descriptor.operator_names

    @property
    def requires_diff_basis(self):
        ''' Returns True if a differential basis function is
        associated with this function space, otherwise it returns
        False. '''
        return "gh_diff_basis" in self._descriptor.operator_names

    @property
    def requires_orientation(self):
        ''' Returns True if an orientation function is
        associated with this function space, otherwise it returns
        False. '''
        return "gh_orientation" in self._descriptor.operator_names

    @property
    def fs_name(self):
        ''' Returns the raw metadata value of this function space. '''
        return self._descriptor.function_space_name


class FSDescriptors(object):
    ''' Contains a collection of FSDescriptor objects and methods
    that provide information across these objects. We have one
    FSDescriptor for each meta-funcs entry in the kernel
    meta-data '''

    def __init__(self, descriptors):
        self._orig_descriptors = descriptors
        self._descriptors = []
        for descriptor in descriptors:
            self._descriptors.append(FSDescriptor(descriptor))

    def exists(self, fspace):
        ''' Return True if a descriptor with the specified function
        space exists, otherwise return False. '''
        for descriptor in self._descriptors:
            # FS descriptors hold information taken from the kernel
            # metadata and therefore it is the original name of
            # the supplied function space that we must look at
            if descriptor.fs_name == fspace.orig_name:
                return True
        return False

    def get_descriptor(self, fspace):
        ''' Return the descriptor with the specified function space
        name. If it does not exist raise an error.'''
        for descriptor in self._descriptors:
            if descriptor.fs_name == fspace.orig_name:
                return descriptor
        raise GenerationError(
            "FSDescriptors:get_descriptor: there is no descriptor for "
            "function space {0}".format(fspace.orig_name))


def check_args(call):
    '''checks that the kernel arguments provided via the invoke call are
    consistent with the information expected, as specified by the
    kernel metadata '''

    # stencil arguments
    stencil_arg_count = 0
    for arg_descriptor in call.ktype.arg_descriptors:
        if arg_descriptor.stencil:
            if not arg_descriptor.stencil['extent']:
                # an extent argument must be provided
                stencil_arg_count += 1
            if arg_descriptor.stencil['type'] == 'xory1d':
                # a direction argument must be provided
                stencil_arg_count += 1

    # qr_argument
    qr_required = False
    for descriptor in call.ktype.func_descriptors:
        if len(descriptor.operator_names) > 0:
            qr_required = True
    if qr_required:
        qr_arg_count = 1
    else:
        qr_arg_count = 0

    expected_arg_count = len(call.ktype.arg_descriptors) + \
        stencil_arg_count + qr_arg_count

    if expected_arg_count != len(call.args):
        raise GenerationError(
            "error: expected '{0}' arguments in the algorithm layer but "
            "found '{1}'. Expected '{2}' standard arguments, '{3}' "
            "stencil arguments and '{4}' qr_arguments'".format(
                expected_arg_count, len(call.args),
                len(call.ktype.arg_descriptors), stencil_arg_count,
                qr_arg_count))


class DynStencil(object):
    ''' Provides stencil information about a Dynamo argument '''
    def __init__(self, name):
        self._name = name
        self._extent = None
        self._extent_arg = None
        self._direction_arg = None

    @property
    def extent(self):
        '''Returns the extent of the stencil if it is known. It will be known
        if it is specified in the metadata.'''
        return self._extent

    @property
    def extent_arg(self):
        '''Returns the algorithm argument associated with the extent value if
        extent has not been provided in the metadata.'''
        return self._extent_arg

    @extent_arg.setter
    def extent_arg(self, value):
        ''' sets the extent_arg argument. '''
        self._extent_arg = value

    @property
    def direction_arg(self):
        '''returns the direction argument associated with the direction of
        the stencil if the direction of the stencil is not known'''
        return self._direction_arg

    @direction_arg.setter
    def direction_arg(self, value):
        ''' sets the direction_arg argument. '''
        self._direction_arg = value


class DynKernelArguments(Arguments):
    ''' Provides information about Dynamo kernel call arguments
    collectively, as specified by the kernel argument metadata. '''

    def __init__(self, call, parent_call):
        if False:  # pylint: disable=using-constant-test
            # For pyreverse
            self._0_to_n = DynKernelArgument(None, None, None, None)

        self._name_space_manager = NameSpaceFactory().create()

        Arguments.__init__(self, parent_call)

        # check that the arguments provided by the algorithm layer are
        # consistent with those expected by the kernel(s)
        check_args(call)

        # create our arguments and add in stencil information where
        # appropriate.
        self._args = []
        idx = 0
        for arg in call.ktype.arg_descriptors:

            dyn_argument = DynKernelArgument(self, arg, call.args[idx],
                                             parent_call)
            idx += 1
            if dyn_argument.descriptor.stencil:
                stencil = DynStencil(dyn_argument.descriptor.stencil['type'])
                if dyn_argument.descriptor.stencil['extent']:
                    raise GenerationError("extent metadata not yet supported")
                    # if supported we would add the following
                    # line. However, note there is currently no setter
                    # for extent in DynStencil so this would need to
                    # be added.  stencil.extent =
                    # dyn_argument.descriptor.stencil['extent']
                else:
                    # an extent argument has been added
                    stencil.extent_arg = call.args[idx]
                    # extent_arg is not a standard dynamo argument, it is
                    # an Arg object created by the parser. Therefore its
                    # name may clash. We register and update the name here.
                    unique_name = self._name_space_manager.create_name(
                        root_name=stencil.extent_arg.varName,
                        context="AlgArgs",
                        label=stencil.extent_arg.text)
                    stencil.extent_arg.varName = unique_name
                    idx += 1
                if dyn_argument.descriptor.stencil['type'] == 'xory1d':
                    # a direction argument has been added
                    stencil.direction_arg = call.args[idx]
                    if stencil.direction_arg.varName not in \
                       VALID_STENCIL_DIRECTIONS:
                        # direction_arg is not a standard dynamo
                        # argument, it is an Arg object created by the
                        # parser. Therefore its name may clash. We
                        # register and update the name here.
                        unique_name = self._name_space_manager.create_name(
                            root_name=stencil.direction_arg.varName,
                            context="AlgArgs",
                            label=stencil.direction_arg.text)
                        stencil.direction_arg.varName = unique_name
                    idx += 1
                dyn_argument.stencil = stencil
            self._args.append(dyn_argument)

        self._dofs = []

        # Generate a static list of unique function-space names used
        # by the set of arguments: store the mangled names as these
        # are what we use at the level of an Invoke
        self._unique_fs_names = []
        # List of corresponding unique function-space objects
        self._unique_fss = []
        for arg in self._args:
            for function_space in arg.function_spaces:
                # We check that function_space is not None because scalar
                # args don't have one and fields only have one (only
                # operators have two).
                if function_space and \
                   function_space.mangled_name not in self._unique_fs_names:
                    self._unique_fs_names.append(function_space.mangled_name)
                    self._unique_fss.append(function_space)

    def get_arg_on_space(self, func_space):
        '''Returns the first argument (field or operator) found that
        is on the specified function space. If no field or operator is
        found an exception is raised.'''
        for arg in self._args:
            for function_space in arg.function_spaces:
                if function_space and \
                   func_space.mangled_name == function_space.mangled_name:
                    return arg
        raise FieldNotFoundError("DynKernelArguments:get_arg_on_space: there "
                                 "is no field or operator with function space "
                                 "{0}".format(func_space.mangled_name))

    def has_operator(self, op_type=None):
        ''' Returns true if at least one of the arguments is an operator
        (either LMA or CMA). '''
        if op_type and op_type not in VALID_OPERATOR_NAMES:
            raise GenerationError(
                "If supplied, op_type must be a valid operator type (one "
                "of {0}) but got {1}".format(VALID_OPERATOR_NAMES, op_type))
        if not op_type:
            # If no operator type is specified then we match any type
            op_list = VALID_OPERATOR_NAMES
        else:
            op_list = [op_type]
        for arg in self._args:
            if arg.type in op_list:
                return True
        return False

    @property
    def unique_fss(self):
        ''' Returns a unique list of function space objects used by the
        arguments of this kernel '''
        return self._unique_fss

    @property
    def unique_fs_names(self):
        ''' Return the list of unique function space names used by the
        arguments of this kernel. The names are unmangled (i.e. as
        specified in the kernel metadata) '''
        return self._unique_fs_names

    def iteration_space_arg(self, mapping=None):
        '''Returns an argument we can use to dereference the iteration
        space. This can be a field or operator that is modified or
        alternatively a field that is read if one or more scalars
        are modified. If a kernel writes to more than one argument then
        that requiring the largest iteration space is selected.'''

        # Since we always compute operators out to the L1 halo we first
        # check whether this kernel writes to an operator
        op_args = psyGen.args_filter(self._args,
                                     arg_types=VALID_OPERATOR_NAMES,
                                     arg_accesses=GH_WRITE_ACCESSES)
        if op_args:
            return op_args[0]

        # This kernel does not write to an operator. We now check for
        # fields that are written to. We check first for any modified
        # field on a continuous function space, failing that we try
        # any_space function spaces (because we must assume such a
        # space is continuous) and finally we try discontinuous
        # function spaces. We do this because if a quantity on a
        # continuous FS is modified then our iteration space must be
        # larger (include L1 halo cells)
        fld_args = psyGen.args_filter(self._args,
                                      arg_types=["gh_field"],
                                      arg_accesses=GH_WRITE_ACCESSES)
        if fld_args:
            for spaces in [CONTINUOUS_FUNCTION_SPACES,
                           VALID_ANY_SPACE_NAMES,
                           DISCONTINUOUS_FUNCTION_SPACES]:
                for arg in fld_args:
                    if arg.function_space.orig_name in spaces:
                        return arg

        # No modified fields or operators. Check for unmodified fields...
        fld_args = psyGen.args_filter(self._args, arg_types=["gh_field"])
        if fld_args:
            return fld_args[0]

        # it is an error if we get to here
        raise GenerationError(
            "iteration_space_arg(). The dynamo0.3 api must have a modified "
            "field, a modified operator, or an unmodified field (in the case "
            "of a modified scalar). None of these were found.")

    @property
    def dofs(self):
        ''' Currently required for invoke base class although this
        makes no sense for dynamo. Need to refactor the invoke class
        and pull out dofs into the gunghoproto api. '''
        return self._dofs


class DynKernelArgument(KernelArgument):
    ''' Provides information about individual Dynamo kernel call
    arguments as specified by the kernel argument metadata. '''

    def __init__(self, kernel_args, arg_meta_data, arg_info, call):
        KernelArgument.__init__(self, arg_meta_data, arg_info, call)
        # Keep a reference to DynKernelArguments object that contains
        # this argument. This permits us to manage name-mangling for
        # any-space function spaces.
        self._kernel_args = kernel_args
        self._vector_size = arg_meta_data.vector_size
        self._type = arg_meta_data.type
        self._stencil = None

        # The list of function-space objects for this argument. Each
        # object can be queried for its original name and for the
        # mangled name (used to make any-space arguments distinct
        # within an invoke). The argument will only have more than
        # one function-space associated with it if it is an operator.
        fs1 = None
        fs2 = None

        if self._type in VALID_OPERATOR_NAMES:

            fs1 = FunctionSpace(arg_meta_data.function_space_to,
                                self._kernel_args)
            fs2 = FunctionSpace(arg_meta_data.function_space_from,
                                self._kernel_args)
        else:
            if arg_meta_data.function_space:
                fs1 = FunctionSpace(arg_meta_data.function_space,
                                    self._kernel_args)
        self._function_spaces = [fs1, fs2]

    @property
    def descriptor(self):
        ''' return a descriptor object which contains Kernel
        metadata about this argument '''
        return self._arg

    def ref_name(self, function_space=None):
        ''' Returns the name used to dereference this type of argument. '''
        if not function_space:
            if self._type in VALID_OPERATOR_NAMES:
                # For an operator we use the 'from' FS
                function_space = self._function_spaces[1]
            else:
                function_space = self._function_spaces[0]
        else:
            # Check that the supplied function space is valid for this
            # argument
            found = False
            for fspace in self.function_spaces:
                if fspace and fspace.orig_name == function_space.orig_name:
                    found = True
                    break
            if not found:
                raise GenerationError(
                    "DynKernelArgument:ref_name(fs). The supplied function "
                    "space (fs='{0}') is not one of the function spaces "
                    "associated with this argument (fss='{1}')".format(
                        function_space.orig_name,
                        self.function_space_names))
        if self._type == "gh_field":
            return "vspace"
        elif self._type in VALID_OPERATOR_NAMES:
            if function_space.orig_name == self.descriptor.function_space_from:
                return "fs_from"
            elif function_space.orig_name == self.descriptor.function_space_to:
                return "fs_to"
            else:
                raise GenerationError(
                    "ref_name: Error, function space '{0}' is one of the "
                    "gh_operator function spaces '{1}' but is not being "
                    "returned by either function_space from '{2}' or "
                    "function_space_to '{3}'".format(
                        function_space.orig_name, self.function_spaces,
                        self.descriptor.function_space_from,
                        self.descriptor.function_space_to))
        else:
            raise GenerationError(
                "ref_name: Error, unsupported arg type '{0}' found".
                format(self._type))

    @property
    def type(self):
        ''' Returns the type of this argument. '''
        return self._type

    @property
    def vector_size(self):
        ''' Returns the vector size of this argument as specified in
        the Kernel metadata. '''
        return self._vector_size

    @property
    def proxy_name(self):
        ''' Returns the proxy name for this argument. '''
        return self._name+"_proxy"

    @property
    def proxy_declaration_name(self):
        ''' Returns the proxy name for this argument with the array
        dimensions added if required. '''
        if self._vector_size > 1:
            return self.proxy_name+"("+str(self._vector_size)+")"
        else:
            return self.proxy_name

    @property
    def declaration_name(self):
        ''' Returns the name for this argument with the array
        dimensions added if required. '''
        if self._vector_size > 1:
            return self._name+"("+str(self._vector_size)+")"
        else:
            return self._name

    @property
    def proxy_name_indexed(self):
        ''' Returns the proxy name for this argument with an
        additional index which accesses the first element for a vector
        argument. '''
        if self._vector_size > 1:
            return self._name+"_proxy(1)"
        else:
            return self._name+"_proxy"

    @property
    def name_indexed(self):
        ''' Returns the name for this argument with an
        additional index which accesses the first element for a vector
        argument. '''
        if self._vector_size > 1:
            return self._name+"(1)"
        else:
            return self._name

    @property
    def function_space(self):
        ''' Returns the expected finite element function space for this
            argument as specified by the kernel argument metadata. '''
        if self._type == "gh_operator":
            return self.function_spaces[1]
        else:
            return self._function_spaces[0]

    @property
    def function_space_to(self):
        ''' Returns the 'to' function space of an operator '''
        return self._function_spaces[0]

    @property
    def function_space_from(self):
        ''' Returns the 'from' function space of an operator '''
        return self._function_spaces[1]

    @property
    def function_spaces(self):
        ''' Returns the expected finite element function spaces for this
        argument as a list as specified by the kernel argument
        metadata. We have more than one function space when dealing
        with operators. '''
        return self._function_spaces

    @property
    def function_space_names(self):
        ''' Returns a list of the names of the function spaces associated
        with this argument. We have more than one function space when
        dealing with operators. '''
        fs_names = []
        for fspace in self._function_spaces:
            if fspace:
                fs_names.append(fspace.orig_name)
        return fs_names

    @property
    def intent(self):
        ''' Returns the fortran intent of this argument. '''
        if self.access == "gh_read":
            return "in"
        elif self.access == "gh_write":
            return "out"
        elif self.access in ["gh_inc"] + VALID_REDUCTION_NAMES:
            return "inout"
        else:
            raise GenerationError(
                "Expecting argument access to be one of 'gh_read, gh_write, "
                "gh_inc' or one of {0}, but found '{1}'".
                format(str(VALID_REDUCTION_NAMES), self.access))

    @property
    def discontinuous(self):
        '''Returns True if this argument is known to be on a discontinuous
        function space, otherwise returns False.'''
        if self.function_space in DISCONTINUOUS_FUNCTION_SPACES:
            return True
        elif self.function_space in VALID_ANY_SPACE_NAMES:
            # we will eventually look this up based on our dependence
            # analysis but for the moment we assume the worst
            return False
        else:  # must be a continuous function space
            return False

    @property
    def stencil(self):
        '''Return stencil information about this kernel argument if it
        exists. The information is returned as a DynStencil object.'''
        return self._stencil

    @stencil.setter
    def stencil(self, value):
        '''Set stencil information for this kernel argument. The information
        should be provided as a DynStencil object. '''
        self._stencil = value


class DynKernCallFactory(object):
    ''' Create the necessary framework for a Dynamo kernel call.
    This consists of a Loop over cells containing a call to the
    user-supplied kernel routine. '''
    @staticmethod
    def create(call, parent=None):
        ''' Create the objects needed for a call to the kernel
        described in the call object '''

        # Loop over cells
        cloop = DynLoop(parent=parent)

        # The kernel itself
        kern = DynKern()
        kern.load(call, cloop)

        # Add the kernel as a child of the loop
        cloop.addchild(kern)

        # Set-up the loop now we have the kernel object
        cloop.load(kern)

        # Return the outermost loop
        return cloop
