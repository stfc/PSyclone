# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017-2018, Science and Technology Facilities Council
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
# Authors R. W. Ford and A. R. Porter, STFC Daresbury Lab
# Modified I. Kavcic, Met Office

''' This module implements the PSyclone Dynamo 0.3 API by 1)
    specialising the required base classes in parser.py (Descriptor,
    KernelType) and adding a new class (DynFuncDescriptor03) to
    capture function descriptor metadata and 2) specialising the
    required base classes in psyGen.py (PSy, Invokes, Invoke, Schedule,
    Loop, Kern, Inf, Arguments and Argument). '''

# Imports
import os
import fparser
from psyclone.parse import Descriptor, KernelType, ParseError
import psyclone.expression as expr
from psyclone import psyGen, config
from psyclone.psyGen import PSy, Invokes, Invoke, Schedule, Loop, Kern, \
    Arguments, KernelArgument, NameSpaceFactory, GenerationError, \
    FieldNotFoundError, HaloExchange, GlobalSum, FORTRAN_INTENT_NAMES

# First section : Parser specialisations and classes

# Function spaces (FS)
# Discontinuous FS
DISCONTINUOUS_FUNCTION_SPACES = ["w3", "wtheta", "w2v"]
# Continuous FS
# Space any_w2 can be w2, w2h or w2v
CONTINUOUS_FUNCTION_SPACES = ["w0", "w1", "w2", "w2h", "any_w2"]
# Valid FS and FS names
VALID_FUNCTION_SPACES = DISCONTINUOUS_FUNCTION_SPACES + \
    CONTINUOUS_FUNCTION_SPACES

VALID_ANY_SPACE_NAMES = ["any_space_1", "any_space_2", "any_space_3",
                         "any_space_4", "any_space_5", "any_space_6",
                         "any_space_7", "any_space_8", "any_space_9"]

VALID_FUNCTION_SPACE_NAMES = VALID_FUNCTION_SPACES + VALID_ANY_SPACE_NAMES

# Evaluators: basis and differential basis
VALID_EVALUATOR_NAMES = ["gh_basis", "gh_diff_basis"]

# Meta functions
VALID_METAFUNC_NAMES = VALID_EVALUATOR_NAMES + ["gh_orientation"]

# Evaluators: quadrature
VALID_QUADRATURE_SHAPES = ["gh_quadrature_xyoz"]
VALID_EVALUATOR_SHAPES = VALID_QUADRATURE_SHAPES + ["gh_evaluator"]
# Dictionary allowing us to look-up the name of the Fortran module, type
# and proxy-type associated with each quadrature shape
QUADRATURE_TYPE_MAP = {
    "gh_quadrature_xyoz": {"module": "quadrature_xyoz_mod",
                           "type": "quadrature_xyoz_type",
                           "proxy_type": "quadrature_xyoz_proxy_type"}}

# Datatypes (scalars, fields, operators)
VALID_SCALAR_NAMES = ["gh_real", "gh_integer"]
VALID_OPERATOR_NAMES = ["gh_operator", "gh_columnwise_operator"]
VALID_ARG_TYPE_NAMES = ["gh_field"] + VALID_OPERATOR_NAMES + \
    VALID_SCALAR_NAMES

# Access types
VALID_REDUCTION_NAMES = ["gh_sum"]
# List of all access types that involve writing to an argument
# in some form
GH_WRITE_ACCESSES = ["gh_write", "gh_readwrite", "gh_inc"] + \
                     VALID_REDUCTION_NAMES
# List of all access types that involve reading an argument in some
# form
GH_READ_ACCESSES = ["gh_read", "gh_readwrite", "gh_inc"]
# Access type that is only a read, as a list for convenience
GH_READ_ONLY_ACCESS = ["gh_read"]

VALID_ACCESS_DESCRIPTOR_NAMES = GH_READ_ONLY_ACCESS + GH_WRITE_ACCESSES

# Stencils
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

# These are the valid mesh types that may be specified for a field
# using the mesh_arg=... meta-data element (for inter-grid kernels that
# perform prolongation/restriction).
VALID_MESH_TYPES = ["gh_coarse", "gh_fine"]

# These are loop bound names which identify positions in a fields
# halo. It is useful to group these together as we often need to
# determine whether an access to a field or other object includes
# access to the halo, or not.
HALO_ACCESS_LOOP_BOUNDS = ["cell_halo", "dof_halo", "colour_halo"]

VALID_LOOP_BOUNDS_NAMES = ["start", "inner", "ncolour", "ncolours", "ncells",
                           "ndofs"] + HALO_ACCESS_LOOP_BOUNDS

# The mapping from meta-data strings to field-access types
# used in this API.
FIELD_ACCESS_MAP = {"write": "gh_write", "read": "gh_read",
                    "inc": "gh_inc", "readwrite": "gh_readwrite"}

# Valid Dynamo0.3 loop types. The default is "" which is over cells (in the
# horizontal plane).
VALID_LOOP_TYPES = ["dofs", "colours", "colour", ""]

# Mappings used by non-API-Specific code in psyGen
psyGen.MAPPING_REDUCTIONS = {"sum": "gh_sum"}
psyGen.MAPPING_SCALARS = {"iscalar": "gh_integer", "rscalar": "gh_real"}
psyGen.MAPPING_ACCESSES = FIELD_ACCESS_MAP
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


def get_fs_basis_name(function_space, qr_var=None, on_space=None):
    '''
    Returns a name for the basis function on this FunctionSpace. If
    the name of an associated quadrature object is supplied then this
    is appended to the returned name. Similarly, if the function space
    at which the basis is to be evaluated is supplied then this is
    also appended to the name.

    :param function_space: the function space for which the basis is required
    :type function_space: :py:class:`psyclone.dynamo0p3.FunctionSpace`
    :param string qr_var: the name of the Quadrature Object for which the
                          basis functions are required
    :param on_space: the function space at which the basis functions
                     will be evaluated
    :type on_space: :py:class:`psyclone.dynamo0p3.FunctionSpace`
    :return: Name for the Fortran array holding the basis function
    :rtype: string
    '''
    name = "_".join(["basis", function_space.mangled_name])
    if qr_var:
        name += "_" + qr_var
    if on_space:
        name += "_on_" + on_space.mangled_name
    return name


def basis_first_dim_name(function_space):
    '''
    Get the name of the variable holding the first dimension of a
    basis function

    :param function_space: the function space the basis function is for
    :type function_space: :py:class:`psyclone.dynamo0p3.FunctionSpace`
    :return: a Fortran variable name
    :rtype: string
    '''
    return "dim_" + function_space.mangled_name


def get_fs_diff_basis_name(function_space, qr_var=None, on_space=None):
    '''
    Returns a name for the differential basis function on the
    supplied FunctionSpace.  If the name of an associated quadrature
    object is supplied then this is appended to the returned name. Similarly,
    if the function space at which the basis is to be evaluated is supplied
    then this is also appended to the name.

    :param function_space: the function space for which the differential basis
                           is required
    :type function_space: :py:class:`psyclone.dynamo0p3.FunctionSpace`
    :param string qr_var: the name of the Quadrature Object for which the
                          differential basis functions are required
    :param on_space: the function space at which the differential basis
                     functions will be evaluated
    :type on_space: :py:class:`psyclone.dynamo0p3.FunctionSpace`
    :return: Name for the Fortran array holding the differential basis function
    :rtype: string
    '''
    name = "_".join(["diff_basis", function_space.mangled_name])
    if qr_var:
        name += "_" + qr_var
    if on_space:
        name += "_on_" + on_space.mangled_name
    return name


def diff_basis_first_dim_name(function_space):
    '''
    Get the name of the variable holding the first dimension of a
    differential basis function

    :param function_space: the function space the diff-basis function is for
    :type function_space: :py:class:`psyclone.dynamo0p3.FunctionSpace`
    :return: a Fortran variable name
    :rtype: string
    '''
    return "diff_dim_" + function_space.mangled_name


def qr_basis_alloc_args(first_dim, basis_fn):
    '''
    Generate the list of dimensions required to allocate the
    supplied basis/diff-basis function

    :param str first_dim: the variable name for the first dimension
    :param basis_fn: dict holding details on the basis function
                     we want to allocate
    :type basis_fn: dict containing 'shape', 'fspace' and and 'qr_var' keys
                    holding the quadrature shape, FunctionSpace and name
                    of the associated quadrature variable (as specified in the
                    Algorithm layer), respectively
    :return: list of dimensions to use to allocate array
    :rtype: list of strings
    '''
    # Dimensionality of the basis arrays depends on the
    # type of quadrature...
    # if basis_fn["shape"] == "gh_quadrature_xyz":
    #     alloc_args = [first_dim, get_fs_ndf_name(basis_fn["fspace"]),
    #          "np_xyz"+"_"+basis_fn["qr_var"]]
    if basis_fn["shape"] == "gh_quadrature_xyoz":
        alloc_args = [first_dim, get_fs_ndf_name(basis_fn["fspace"]),
                      "np_xy"+"_"+basis_fn["qr_var"],
                      "np_z"+"_"+basis_fn["qr_var"]]
    # elif basis_fn["shape"] == "gh_quadrature_xoyoz":
    #     alloc_args = [first_dim, get_fs_ndf_name(basis_fn["fspace"]),
    #                   "np_x"+"_"+basis_fn["qr_var"],
    #                   "np_y"+"_"+basis_fn["qr_var"],
    #                   "np_z"+"_"+basis_fn["qr_var"]]
    else:
        raise GenerationError(
            "Internal error: unrecognised shape ({0}) specified in "
            "dynamo0p3.qr_basis_alloc_args(). Should be one of: "
            "{1}".format(basis_fn["shape"], VALID_QUADRATURE_SHAPES))
    return alloc_args


def get_fs_operator_name(operator_name, function_space, qr_var=None,
                         on_space=None):
    '''
    Returns the name of the specified operator (orientation, basis or
    differential basis) for the supplied FunctionSpace.
    :param string operator_name: Name (type) of the operator
    :param function_space: the function space for which the operator is
                           required
    :type function_space: :py:class:`psyclone.dynamo0p3.FunctionSpace`
    :param string qr_var: the name of the Quadrature Object for which the
                          operator is required.
    :param on_space: the function space at which the operator is required
    :type on_space: :py:class:`psyclone.dynamo0p3.FunctionSpace`
    :return: Name for the Fortran arry holding the named operator
             for the specified function space
    :rtype: string
    '''
    if operator_name == "gh_orientation":
        return get_fs_orientation_name(function_space)
    elif operator_name == "gh_basis":
        return get_fs_basis_name(function_space, qr_var=qr_var,
                                 on_space=on_space)
    elif operator_name == "gh_diff_basis":
        return get_fs_diff_basis_name(function_space, qr_var=qr_var,
                                      on_space=on_space)
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
        '''
        :param arg_type: dynamo0.3 argument type (scalar, field or operator)
        :type arg_type: :py:class:`psyclone.dynamo0p3.DynKernelArgument`
        '''
        self._arg_type = arg_type
        if arg_type.name != 'arg_type':
            raise ParseError(
                "In the dynamo0.3 API each meta_arg entry must be of type "
                "'arg_type', but found '{0}'".format(arg_type.name))

        # We require at least 2 args
        if len(arg_type.args) < 2:
            raise ParseError(
                "In the dynamo0.3 API each meta_arg entry must have at least "
                "2 args, but found '{0}'".format(len(arg_type.args)))

        # The first arg is the type of field, possibly with a *n appended
        self._vector_size = 1
        if isinstance(arg_type.args[0], expr.BinaryOperator):
            # We expect 'field_type * n' to have been specified
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
            if operator != "*":
                raise ParseError(
                    "In the dynamo0.3 API the 1st argument of a meta_arg "
                    "entry may be a vector but if so must use '*' as the "
                    "separator in the format (field*n), but found '{0}' in "
                    "'{1}'".format(operator, arg_type))
            if self._vector_size <= 1:
                raise ParseError(
                    "In the dynamo0.3 API the 1st argument of a meta_arg "
                    "entry may be a vector but if so must contain a valid "
                    "integer vector size in the format (field*n where n>1), "
                    "but found '{0}' in '{1}'".format(self._vector_size,
                                                      arg_type))

        elif isinstance(arg_type.args[0], expr.FunctionVar):
            # We expect 'field_type' to have been specified
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
        # Reduction access descriptors are only valid for real scalar arguments
        if self._type != "gh_real" and \
           self._access_descriptor.name in VALID_REDUCTION_NAMES:
            raise ParseError(
                "In the dynamo0.3 API a reduction access '{0}' is only valid "
                "with a real scalar argument, but '{1}' was found".
                format(self._access_descriptor.name, self._type))

        # FIELD, OPERATOR and SCALAR datatypes descriptors and rules
        stencil = None
        mesh = None
        # Fields
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

            # The optional 4th argument is either a stencil specification
            # or a mesh identifier (for inter-grid kernels)
            if len(arg_type.args) == 4:
                try:
                    from psyclone.parse import get_stencil, get_mesh
                    if "stencil" in str(arg_type.args[3]):
                        stencil = get_stencil(arg_type.args[3],
                                              VALID_STENCIL_TYPES)
                    elif "mesh" in str(arg_type.args[3]):
                        mesh = get_mesh(arg_type.args[3], VALID_MESH_TYPES)
                    else:
                        raise ParseError("Unrecognised meta-data entry")
                except ParseError as err:
                    raise ParseError(
                        "In the dynamo0.3 API the 4th argument of a "
                        "meta_arg entry must be either a valid stencil "
                        "specification  or a mesh identifier (for inter-"
                        "grid kernels). However, "
                        "entry {0} raised the following error: {1}".
                        format(arg_type, str(err)))
            # Test allowed accesses for fields
            if self._function_space1.lower() in DISCONTINUOUS_FUNCTION_SPACES \
               and self._access_descriptor.name.lower() == "gh_inc":
                raise ParseError(
                    "It does not make sense for a field on a discontinuous "
                    "space ({0}) to have a 'gh_inc' access".
                    format(self._function_space1.lower()))
            # TODO: extend for "gh_write"
            if self._function_space1.lower() in CONTINUOUS_FUNCTION_SPACES \
               and self._access_descriptor.name.lower() == "gh_readwrite":
                raise ParseError(
                    "It does not make sense for a field on a continuous "
                    "space ({0}) to have a 'gh_readwrite' access".
                    format(self._function_space1.lower()))
            # TODO: extend for "gh_write"
            if self._function_space1.lower() in VALID_ANY_SPACE_NAMES \
               and self._access_descriptor.name.lower() == "gh_readwrite":
                raise ParseError(
                    "In the dynamo0.3 API a field on any_space cannot "
                    "have 'gh_readwrite' access because it is treated "
                    "as continuous")
            if stencil and self._access_descriptor.name.lower() != "gh_read":
                raise ParseError("a stencil must be read only so its access"
                                 "should be gh_read")

        # Operators
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
            # Test allowed accesses for operators
            if self._access_descriptor.name.lower() == "gh_inc":
                raise ParseError(
                    "In the dynamo0.3 API operators cannot have a 'gh_inc' "
                    "access because they behave as discontinuous quantities")

        # Scalars
        elif self._type in VALID_SCALAR_NAMES:
            if len(arg_type.args) != 2:
                raise ParseError(
                    "In the dynamo0.3 API each meta_arg entry must have 2 "
                    "arguments if its first argument is gh_{{r,i}}scalar, but "
                    "found {0} in '{1}'".format(len(arg_type.args), arg_type))
            # Test allowed accesses for scalars (read_only or reduction)
            if self._access_descriptor.name not in ["gh_read"] + \
               VALID_REDUCTION_NAMES:
                raise ParseError(
                    "In the dynamo0.3 API scalar arguments must be "
                    "read-only (gh_read) or a reduction ({0}) but found "
                    "'{1}' in '{2}'".format(VALID_REDUCTION_NAMES,
                                            self._access_descriptor.name,
                                            arg_type))
            # Scalars don't have a function space
            self._function_space1 = None

        # We should never get to here
        else:
            raise ParseError(
                "Internal error in DynArgDescriptor03.__init__, (2) should "
                "not get to here")
        Descriptor.__init__(self, self._access_descriptor.name,
                            self._function_space1, stencil=stencil,
                            mesh=mesh)

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
        '''
        :param ast: fparser1 AST for the kernel
        :type ast: :py:class:`fparser.block_statements.BeginSource`
        :param str name: The name of this kernel

        :raises ParseError: if the meta-data does not conform to the
                            rules for the Dynamo 0.3 API
        '''
        KernelType.__init__(self, ast, name=name)

        # The type of CMA operation this kernel performs (or None if
        # no CMA operators are involved)
        self._cma_operation = None

        # Query the meta-data for the evaluator shape (only required if
        # kernel uses quadrature or an evaluator). If it is not
        # present then eval_shape will be None.
        self._eval_shape = self.get_integer_variable('gh_shape')

        # Whether or not this is an inter-grid kernel (i.e. has a mesh
        # specified for each [field] argument). This property is
        # set to True if all the checks in _validate_inter_grid() pass.
        self._is_intergrid = False

        # parse the arg_type metadata
        self._arg_descriptors = []
        for arg_type in self._inits:
            self._arg_descriptors.append(DynArgDescriptor03(arg_type))
        # parse the func_type metadata if it exists
        found = False
        for line in self._ktype.content:
            if isinstance(line, fparser.one.typedecl_statements.Type):
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

            # Check that a valid shape has been specified if
            # this function space requires a basis or differential basis
            for name in descriptor.operator_names:
                if name in VALID_EVALUATOR_NAMES:
                    need_evaluator = True
                    if not self._eval_shape:
                        raise ParseError(
                            "In the dynamo0.3 API any kernel requiring "
                            "quadrature or an evaluator ({0}) must also "
                            "supply the shape of that evaluator by setting "
                            "'gh_shape' in the kernel meta-data but "
                            "this is missing for kernel '{1}'".
                            format(VALID_EVALUATOR_NAMES, self.name))
                    if self._eval_shape not in VALID_EVALUATOR_SHAPES:
                        raise ParseError(
                            "In the dynamo0.3 API a kernel requiring either "
                            "quadrature or an evaluator must request a valid "
                            "gh_shape (one of {0}) but got '{1}' for "
                            "kernel '{2}'".
                            format(VALID_EVALUATOR_SHAPES, self._eval_shape,
                                   self.name))
            self._func_descriptors.append(descriptor)
        # Perform further checks that the meta-data we've parsed
        # conforms to the rules for this API
        self._validate(need_evaluator)

    def _validate(self, need_evaluator):
        '''
        Check that the meta-data conforms to Dynamo 0.3 rules for a
        user-provided kernel or a built-in

        :param bool need_evaluator: whether this kernel requires an
                                    evaluator/quadrature
        :raises: ParseError: if meta-data breaks the Dynamo 0.3 rules
        '''
        from psyclone.dynamo0p3_builtins import BUILTIN_MAP
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

        # Check that no shape has been supplied if no basis or
        # differential basis functions are required for the kernel
        if not need_evaluator and self._eval_shape:
            raise ParseError(
                "Kernel '{0}' specifies a gh_shape ({1}) but does not "
                "need an evaluator because no basis or differential basis "
                "functions are required".format(self.name, self._eval_shape))

        # Check that this kernel only updates a single argument if an
        # evaluator is required
        if self._eval_shape == "gh_evaluator" and write_count > 1:
            raise ParseError(
                "A Dynamo 0.3 kernel requiring quadrature/evaluator must "
                "only write to one argument but kernel {0} requires {1} and "
                "updates {2} arguments".format(self.name,
                                               self._eval_shape, write_count))

        # If we have a columnwise operator as argument then we need to
        # identify the operation that this kernel performs (one of
        # assemble, apply/apply-inverse and matrix-matrix)
        cwise_ops = psyGen.args_filter(self._arg_descriptors,
                                       arg_types=["gh_columnwise_operator"])
        if cwise_ops:
            self._cma_operation = self._identify_cma_op(cwise_ops)

        # Perform checks for inter-grid kernels
        self._validate_inter_grid()

    def _validate_inter_grid(self):
        '''
        Checks that the kernel meta-data obeys the rules for Dynamo 0.3
        inter-grid kernels. If none of the kernel arguments has a mesh
        associated with it then it is not an inter-grid kernel and this
        routine silently returns.

        :raises: ParseError: if meta-data breaks inter-grid rules
        '''
        # Dictionary of meshes associated with arguments (for inter-grid
        # kernels). Keys are the meshes, values are lists of function spaces
        # of the corresponding field arguments.
        mesh_dict = {}
        # Whether or not any field args are missing the mesh_arg specifier
        missing_mesh = False
        # If this is an inter-grid kernel then it must only have field
        # arguments. Keep a record of any non-field arguments for the benefit
        # of a verbose error message.
        non_field_arg_types = set()
        for arg in self._arg_descriptors:
            # Collect info so that we can check inter-grid kernels
            if arg.type == "gh_field":
                if arg.mesh:
                    # Argument has a mesh associated with it so this must
                    # be an inter-grid kernel
                    if arg.mesh in mesh_dict:
                        mesh_dict[arg.mesh].append(arg.function_space)
                    else:
                        mesh_dict[arg.mesh] = [arg.function_space]
                else:
                    # Record the fact that we have a field without a
                    # mesh specifier (in case this is an inter-grid kernel)
                    missing_mesh = True
            else:
                # Inter-grid kernels are only permitted to have field args
                # so collect a list of other types
                non_field_arg_types.add(arg.type)

        mesh_list = mesh_dict.keys()
        if not mesh_list:
            # There are no meshes associated with any of the arguments so
            # this is not an inter-grid kernel
            return

        if len(VALID_MESH_TYPES) != 2:
            # Sanity check that nobody has messed with the number of
            # grid types that we recognise. This is here because the
            # implementation assumes that there are just two grids
            # (coarse and fine).
            raise ParseError(
                "The implementation of inter-grid support in the Dynamo "
                "0.3 API assumes there are exactly two mesh types but "
                "dynamo0p3.VALID_MESH_TYPES contains {0}: {1}".
                format(len(VALID_MESH_TYPES), VALID_MESH_TYPES))
        if len(mesh_list) != len(VALID_MESH_TYPES):
            raise ParseError(
                "Inter-grid kernels in the Dynamo 0.3 API must have at least "
                "one field argument on each of the mesh types ({0}). However, "
                "kernel {1} has arguments only on {2}".format(
                    VALID_MESH_TYPES, self.name, mesh_list))
        # Inter-grid kernels must only have field arguments
        if non_field_arg_types:
            raise ParseError(
                "Inter-grid kernels in the Dynamo 0.3 API are only "
                "permitted to have field arguments but kernel {0} also "
                "has arguments of type {1}".format(
                    self.name, list(non_field_arg_types)))
        # Check that all arguments have a mesh specified
        if missing_mesh:
            raise ParseError(
                "Inter-grid kernels in the Dynamo 0.3 API must specify "
                "which mesh each field argument is on but kernel {0} has "
                "at least one field argument for which mesh_arg is "
                "missing.".format(self.name))
        # Check that arguments on different meshes are on different
        # function spaces. We do this by checking that no function space
        # is listed as being associated with (arguments on) both meshes.
        fs_sets = []
        for mesh in mesh_dict:
            fs_sets.append(set(mesh_dict[mesh]))
        # Check that the sets of spaces (one for each mesh type) have
        # no intersection
        fs_common = fs_sets[0] & fs_sets[1]
        if fs_common:
            raise ParseError(
                "In the Dynamo 0.3 API field arguments to inter-grid "
                "kernels must be on different function spaces if they are "
                "on different meshes. However kernel {0} has a field on "
                "function space(s) {1} on each of the mesh types {2}.".
                format(self.name, list(fs_common), mesh_list))
        # Finally, record that this is a valid inter-grid kernel
        self._is_intergrid = True

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
            if cop.access in GH_WRITE_ACCESSES:
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
                                           arg_accesses=GH_READ_ONLY_ACCESS)
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
                    arg_types=["gh_operator"],
                    arg_accesses=GH_READ_ONLY_ACCESS)
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
                    self._arg_descriptors, arg_types=VALID_SCALAR_NAMES)
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
        None if the kernel does not involve CMA operators '''
        return self._cma_operation

    @property
    def eval_shape(self):
        '''
        Returns the shape of evaluator required by this kernel or an
        empty string if none.

        :return: the shape of the evaluator (one of VALID_EVALUATOR_SHAPES)
                 or an empty string if the kernel does not require one.
        :rtype: string
        '''
        if self._eval_shape:
            return self._eval_shape
        else:
            return ""

    @property
    def is_intergrid(self):
        '''
        Returns whether or not this is an inter-grid kernel
        :return: True if kernel is an inter-grid kernel, False otherwise
        :rtype: bool
        '''
        return self._is_intergrid

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

        :return: Root node of generated Fortran AST
        :rtype: :py:class:`psyGen.Node`

        '''
        from psyclone.f2pygen import ModuleGen, UseGen
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
        psy_module.add(UseGen(psy_module, name="constants_mod", only=True,
                              funcnames=["r_def"]))
        # add all invoke specific information
        self.invokes.gen_code(psy_module)
        # inline kernels where requested
        self.inline(psy_module)
        # Return the root node of the generated code
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
        from psyclone.f2pygen import DeclGen
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
        from psyclone.f2pygen import DeclGen
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
        from psyclone.f2pygen import AssignGen, IfThenGen, TypeDeclGen, \
            UseGen, CommentGen, DeclGen
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
        # in this one are themselves dictionaries containing two entries:
        # "argument" - the object holding information on the CMA kernel
        #              argument
        # "direction" - whether the dofmap is required for the "to" for
        #               "from" function space of the operator.
        self._unique_cbanded_maps = {}
        # A dictionary of required CMA indirection dofmaps. As with the
        # column-banded dofmaps, each entry is itself a dictionary with
        # "argument" and "direction" entries.
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

                    # Sanity check - we expect only one CMA argument
                    if len(cma_args) != 1:
                        raise GenerationError(
                            "Internal error: there should only be one CMA "
                            "operator argument for a CMA assembly kernel but "
                            "found {0}".format(len(cma_args)))

                    map_name = get_cbanded_map_name(
                        cma_args[0].function_space_to)
                    if map_name not in self._unique_cbanded_maps:
                        self._unique_cbanded_maps[map_name] = {
                            "argument": cma_args[0],
                            "direction": "to"}
                    map_name = get_cbanded_map_name(
                        cma_args[0].function_space_from)
                    if map_name not in self._unique_cbanded_maps:
                        self._unique_cbanded_maps[map_name] = {
                            "argument": cma_args[0],
                            "direction": "from"}
                elif call.cma_operation == "apply":
                    # A kernel that applies (or applies the inverse of) a
                    # CMA operator requires the indirection dofmaps for the
                    # to- and from-spaces of the operator.
                    cma_args = psyGen.args_filter(
                        call.arguments.args,
                        arg_types=["gh_columnwise_operator"])

                    # Sanity check - we expect only one CMA argument
                    if len(cma_args) != 1:
                        raise GenerationError(
                            "Internal error: there should only be one CMA "
                            "operator argument for a kernel that applies a "
                            "CMA operator but found {0}".format(len(cma_args)))

                    map_name = get_cma_indirection_map_name(
                        cma_args[0].function_space_to)
                    if map_name not in self._unique_indirection_maps:
                        self._unique_indirection_maps[map_name] = {
                            "argument": cma_args[0],
                            "direction": "to"}
                    map_name = get_cma_indirection_map_name(
                        cma_args[0].function_space_from)
                    if map_name not in self._unique_indirection_maps:
                        self._unique_indirection_maps[map_name] = {
                            "argument": cma_args[0],
                            "direction": "from"}

    def initialise_dofmaps(self, parent):
        ''' Generates the calls to the LFRic infrastructure that
        look-up the necessary dofmaps. Adds these calls as children
        of the supplied parent node. This must be an appropriate
        f2pygen object. '''
        from psyclone.f2pygen import CommentGen, AssignGen

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
                                     rhs=cma["argument"].proxy_name_indexed +
                                     "%column_banded_dofmap_" +
                                     cma["direction"]))

        if self._unique_indirection_maps:
            parent.add(CommentGen(parent, ""))
            parent.add(CommentGen(parent,
                                  " Look-up required CMA indirection dofmaps"))
            parent.add(CommentGen(parent, ""))

            for dmap, cma in self._unique_indirection_maps.items():
                parent.add(AssignGen(parent, pointer=True, lhs=dmap,
                                     rhs=cma["argument"].proxy_name_indexed +
                                     "%indirection_dofmap_"+cma["direction"]))

    def declare_dofmaps(self, parent):
        ''' Declare all unique function space dofmaps as pointers to
        integer arrays of rank 2. The declarations are added as
        children of the supplied parent argument. This must be an
        appropriate f2pygen object. '''
        from psyclone.f2pygen import DeclGen

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

    # The scalar parameters that must be passed along with a CMA operator
    # if its 'to' and 'from' spaces are the same
    cma_same_fs_params = ["nrow", "bandwidth", "alpha",
                          "beta", "gamma_m", "gamma_p"]
    # The scalar parameters that must be passed along with a CMA operator
    # if its 'to' and 'from' spaces are different
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
        from psyclone.f2pygen import CommentGen, AssignGen

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
        from psyclone.f2pygen import DeclGen

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


class DynMeshes(object):
    '''
    Holds all mesh-related information. If there are no inter-grid
    kernels then there is only one mesh object required (when doing
    distributed memory). However, kernels performing inter-grid
    operations require multiple mesh objects as well as mesh maps and
    other quantities.

    There are two types of inter-grid operation; the first is "prolongation"
    where a field on a coarse mesh is mapped onto a fine mesh. The second
    is "restriction" where a field on a fine mesh is mapped onto a coarse
    mesh.

    '''

    def __init__(self, schedule, unique_psy_vars):
        '''
        :param schedule: the schedule of the Invoke for which to extract
                         information on all required inter-grid operations
        :type schedule: :py:class:`psyclone.dynamo0p3.DynSchedule`
        :param unique_psy_vars: list of arguments to the PSy-layer routine
        :type unique_psy_vars: list of
                   :py:class:`psyclone.dynamo0p3.DynKernelArgument` objects
        '''
        self._name_space_manager = NameSpaceFactory().create()

        self._kern_calls = []
        self._mesh_names = []
        # Set used to generate a list of the unique mesh objects
        _name_set = set()

        # Find the first non-scalar argument to this PSy layer routine. We
        # will use this to look-up the mesh if there are no inter-grid
        # kernels in this invoke.
        self._first_var = None
        for var in unique_psy_vars:
            if var.type not in VALID_SCALAR_NAMES:
                self._first_var = var
                break

        # Loop over all kernel calls in the schedule. Keep a list of
        # any non-intergrid kernels so that we can generate a verbose error
        # message if necessary.
        non_intergrid_kernels = []
        for call in schedule.kern_calls():

            if not call.is_intergrid:
                non_intergrid_kernels.append(call)
                # Skip over any non-inter-grid kernels
                continue

            fine_args = psyGen.args_filter(call.arguments.args,
                                           arg_meshes=["gh_fine"])
            coarse_args = psyGen.args_filter(call.arguments.args,
                                             arg_meshes=["gh_coarse"])
            fine_arg = fine_args[0]
            coarse_arg = coarse_args[0]

            # Generate name for inter-mesh map
            base_mmap_name = "mmap_{0}_{1}".format(fine_arg.name,
                                                   coarse_arg.name)
            mmap = self._name_space_manager.create_name(
                root_name=base_mmap_name,
                context="PSyVars",
                label=base_mmap_name)

            # Generate name for ncell variables
            ncell_fine = self._name_space_manager.create_name(
                root_name="ncell_{0}".format(fine_arg.name),
                context="PSyVars",
                label="ncell_{0}".format(fine_arg.name))
            ncellpercell = self._name_space_manager.create_name(
                root_name="ncpc_{0}_{1}".format(fine_arg.name,
                                                coarse_arg.name),
                context="PSyVars",
                label="ncpc_{0}_{1}".format(fine_arg.name,
                                            coarse_arg.name))
            # Name for cell map
            base_name = "cell_map_" + coarse_arg.name
            cell_map = self._name_space_manager.create_name(
                root_name=base_name, context="PSyVars", label=base_name)

            # Store this information in our list of dicts
            self._kern_calls.append({"fine": fine_arg,
                                     "ncell_fine": ncell_fine,
                                     "coarse": coarse_arg,
                                     "mmap": mmap,
                                     "ncperc": ncellpercell,
                                     "cellmap": cell_map})

            # Create and store the names of the associated mesh objects
            _name_set.add(
                self._name_space_manager.create_name(
                    root_name="mesh_{0}".format(fine_arg.name),
                    context="PSyVars",
                    label="mesh_{0}".format(fine_arg.name)))
            _name_set.add(
                self._name_space_manager.create_name(
                    root_name="mesh_{0}".format(coarse_arg.name),
                    context="PSyVars",
                    label="mesh_{0}".format(coarse_arg.name)))

        # If we found a mixture of both inter-grid and non-inter-grid kernels
        # then we reject the invoke()
        if non_intergrid_kernels and self._kern_calls:
            raise GenerationError(
                "An invoke containing inter-grid kernels must contain no "
                "other kernel types but kernels '{0}' in invoke '{1}' are "
                "not inter-grid kernels.".format(
                    ", ".join([call.name for call in non_intergrid_kernels]),
                    schedule.invoke.name))

        # If we didn't have any inter-grid kernels but distributed memory
        # is enabled then we will still need a mesh object
        if not _name_set and config.DISTRIBUTED_MEMORY:
            mesh_name = "mesh"
            _name_set.add(
                self._name_space_manager.create_name(
                    root_name=mesh_name, context="PSyVars", label=mesh_name))

        # Convert the set of mesh names to a list and store
        self._mesh_names = list(_name_set)

    def declarations(self, parent):
        '''
        Declare variables specific to inter-grid kernels

        :param parent: the parent node to which to add the declarations
        :type parent: an instance of :py:class:`psyclone.f2pygen.BaseGen`
        '''
        from psyclone.f2pygen import DeclGen, TypeDeclGen, UseGen
        # We'll need various typedefs from the mesh module
        if self._mesh_names:
            parent.add(UseGen(parent, name="mesh_mod", only=True,
                              funcnames=["mesh_type"]))
        if self._kern_calls:
            parent.add(UseGen(parent, name="mesh_map_mod", only=True,
                              funcnames=["mesh_map_type"]))
        # Declare the mesh object(s)
        for name in self._mesh_names:
            parent.add(TypeDeclGen(parent, pointer=True, datatype="mesh_type",
                                   entity_decls=[name + " => null()"]))
        # Declare the inter-mesh map(s) and cell map(s)
        for kern in self._kern_calls:
            parent.add(TypeDeclGen(parent, pointer=True,
                                   datatype="mesh_map_type",
                                   entity_decls=[kern["mmap"] + " => null()"]))
            parent.add(
                DeclGen(parent, pointer=True, datatype="integer",
                        entity_decls=[kern["cellmap"] + "(:,:) => null()"]))

            # Declare the number of cells in the fine mesh and how many fine
            # cells there are per coarse cell
            parent.add(DeclGen(parent, datatype="integer",
                               entity_decls=[kern["ncell_fine"],
                                             kern["ncperc"]]))

    def initialise(self, parent):
        '''
        Initialise parameters specific to inter-grid kernels

        :param parent: the parent node to which to add the initialisations
        :type parent: an instance of :py:class:`psyclone.f2pygen.BaseGen`
        '''
        from psyclone.f2pygen import CommentGen, AssignGen

        # If we haven't got any need for a mesh in this invoke then we
        # don't do anything
        if len(self._mesh_names) == 0:
            return

        parent.add(CommentGen(parent, ""))

        if len(self._mesh_names) == 1:
            # We only require one mesh object which means that this invoke
            # contains no inter-grid kernels
            parent.add(CommentGen(parent, " Create a mesh object"))
            parent.add(CommentGen(parent, ""))
            rhs = self._first_var.name_indexed + "%get_mesh()"
            parent.add(AssignGen(parent, pointer=True,
                                 lhs=self._mesh_names[0], rhs=rhs))
            return

        parent.add(CommentGen(
            parent,
            " Look-up mesh objects and loop limits for inter-grid kernels"))
        parent.add(CommentGen(parent, ""))

        # Keep a list of quantities that we've already initialised so
        # that we don't generate duplicate assignments
        initialised = []

        for kern in self._kern_calls:
            # We need pointers to both the coarse and the fine mesh
            fine_mesh = self._name_space_manager.create_name(
                root_name="mesh_{0}".format(kern["fine"].name),
                context="PSyVars",
                label="mesh_{0}".format(kern["fine"].name))
            coarse_mesh = self._name_space_manager.create_name(
                root_name="mesh_{0}".format(kern["coarse"].name),
                context="PSyVars",
                label="mesh_{0}".format(kern["coarse"].name))
            if fine_mesh not in initialised:
                initialised.append(fine_mesh)
                parent.add(
                    AssignGen(parent, pointer=True,
                              lhs=fine_mesh,
                              rhs=kern["fine"].name_indexed + "%get_mesh()"))
            if coarse_mesh not in initialised:
                initialised.append(coarse_mesh)
                parent.add(
                    AssignGen(parent, pointer=True,
                              lhs=coarse_mesh,
                              rhs=kern["coarse"].name_indexed + "%get_mesh()"))
            # We also need a pointer to the mesh map which we get from
            # the coarse mesh
            if kern["mmap"] not in initialised:
                initialised.append(kern["mmap"])
                parent.add(
                    AssignGen(parent, pointer=True,
                              lhs=kern["mmap"],
                              rhs="{0}%get_mesh_map({1})".format(coarse_mesh,
                                                                 fine_mesh)))

            # Cell map. This is obtained from the mesh map.
            if kern["cellmap"] not in initialised:
                initialised.append(kern["cellmap"])
                parent.add(
                    AssignGen(parent, pointer=True, lhs=kern["cellmap"],
                              rhs=kern["mmap"]+"%get_whole_cell_map()"))

            # Number of cells in the fine mesh
            if kern["ncell_fine"] not in initialised:
                initialised.append(kern["ncell_fine"])
                if config.DISTRIBUTED_MEMORY:
                    # TODO this hardwired depth of 2 will need changing in
                    # order to support redundant computation
                    parent.add(
                        AssignGen(parent, lhs=kern["ncell_fine"],
                                  rhs=(fine_mesh+"%get_last_halo_cell"
                                       "(depth=2)")))
                else:
                    parent.add(
                        AssignGen(parent, lhs=kern["ncell_fine"],
                                  rhs="%".join([kern["fine"].proxy_name,
                                                kern["fine"].ref_name(),
                                                "get_ncell()"])))

            # Number of fine cells per coarse cell.
            if kern["ncperc"] not in initialised:
                initialised.append(kern["ncperc"])
                parent.add(
                    AssignGen(parent, lhs=kern["ncperc"],
                              rhs=kern["mmap"] +
                              "%get_ntarget_cells_per_source_cell()"))


class DynInvokeBasisFns(object):
    ''' Holds all information on the basis and differential basis
    functions required by an invoke. This covers both those required for
    quadrature and for evaluators. '''

    def __init__(self, schedule):
        '''
        :param schedule: the schedule of the Invoke for which to extract
                         information on all required basis/diff-basis
                         functions
        :type schedule: :py:class:`psyclone.dynamo0p3.DynSchedule`
        '''
        self._name_space_manager = NameSpaceFactory().create()
        # Construct a list of all the basis/diff-basis functions required
        # by this invoke. Each entry in the list is a dictionary holding
        # the shape, the function space and the function space
        # of the corresponding kernel argument that is being updated.
        self._basis_fns = []
        self._diff_basis_fns = []
        # The dictionary of quadrature objects passed to this invoke. Keys
        # are the various VALID_QUADRATURE_SHAPES, values are a list of
        # associated quadrature variables. (i.e. we have a list of
        # quadrature arguments for each shape.)
        self._qr_vars = {}
        # The list of kernel args for which we require evaluators
        self._unique_evaluator_args = []
        # Corresponding set of unique function spaces (to ensure we don't
        # duplicate anything)
        _fs_eval_list = set()

        for call in schedule.kern_calls():
            # Does this kernel require basis/diff basis functions?
            if call.eval_shape:
                # Keep a list of the quadrature objects passed to this
                # invoke
                if call.eval_shape in VALID_QUADRATURE_SHAPES:
                    if call.eval_shape not in self._qr_vars:
                        # We haven't seen a quadrature arg with this shape
                        # before so create a dictionary entry with an
                        # empty list
                        self._qr_vars[call.eval_shape] = []
                    if call.qr_name not in self._qr_vars[call.eval_shape]:
                        # Add this qr argument to the list of those that
                        # have this shape
                        self._qr_vars[call.eval_shape].append(call.qr_name)
                elif call.eval_shape == "gh_evaluator":
                    # Keep a list of the unique evaluators we require. We do
                    # this as a list of the kernel arguments to which they
                    # correspond. A kernel requiring an evaluator is only
                    # permitted to update a single argument and the function
                    # space of this argument determines which nodes the basis
                    # functions must be evaluated upon. If this argument is an
                    # operator then the 'to' space is used.
                    arg = call.updated_arg
                    fname = arg.evaluator_function_space.mangled_name
                    if fname not in _fs_eval_list:
                        _fs_eval_list.add(fname)
                        self._unique_evaluator_args.append(arg)

                # For each FS descriptor, we need a full function-space object
                for fsd in call.fs_descriptors.descriptors:

                    # We need the full FS object, not just the name. Therefore
                    # we first have to get a kernel argument that is on this
                    # space...
                    arg = call.arguments.get_arg_on_space_name(fsd.fs_name)
                    # ...and then use that to get the appropriate function
                    # space object. We have to take care that we get the
                    # right object if this argument is an operator
                    if arg.type in VALID_OPERATOR_NAMES:
                        if fsd.fs_name == arg.function_space_to.orig_name:
                            fspace = arg.function_space_to
                        else:
                            fspace = arg.function_space_from
                    else:
                        fspace = arg.function_space

                    # Which FS is this on and is it a basis or diff-basis
                    # function that is required?
                    entry = {"shape": call.eval_shape,
                             "write_arg": call.updated_arg,
                             "fspace": fspace,
                             "arg": arg}
                    if call.eval_shape in VALID_QUADRATURE_SHAPES:
                        entry["qr_var"] = call.qr_name
                        # Quadrature are evaluated at pre-determined
                        # points rather than at the nodes of another FS
                        entry["nodal_fspace"] = None
                    else:
                        entry["qr_var"] = None
                        # Store the function space upon which the basis
                        # functions are to be evaluated
                        entry["nodal_fspace"] = call.updated_arg.\
                            evaluator_function_space
                    if fsd.requires_basis:
                        self._basis_fns.append(entry)
                    if fsd.requires_diff_basis:
                        self._diff_basis_fns.append(entry)

    def declare_qr(self, parent):
        '''
        Create the declarations for any quadrature objects passed
        in to an invoke. These are added as children of the supplied
        parent node.

        :param parent: the node in the f2pygen AST that will be the
                       parent of all of the declarations (i.e. the
                       PSy-layer subroutine)
        :type parent: :py:class:`psyclone.f2pygen.BaseGen`
        '''
        from psyclone.f2pygen import TypeDeclGen
        # Create a single declaration for each quadrature type
        for shape in VALID_QUADRATURE_SHAPES:
            if shape in self._qr_vars and self._qr_vars[shape]:
                # The PSy-layer routine is passed objects of
                # quadrature_* type
                parent.add(
                    TypeDeclGen(parent,
                                datatype=QUADRATURE_TYPE_MAP[shape]["type"],
                                entity_decls=self._qr_vars[shape],
                                intent="in"))
                # For each of these we'll need a corresponding proxy, use
                # our namespace manager to avoid clashes...
                var_names = []
                for var in self._qr_vars[shape]:
                    var_names.append(
                        self._name_space_manager.create_name(
                            root_name=var+"_proxy", context="PSyVars",
                            label=var+"_proxy"))
                parent.add(
                    TypeDeclGen(
                        parent,
                        datatype=QUADRATURE_TYPE_MAP[shape]["proxy_type"],
                        entity_decls=var_names))

    def initialise_basis_fns(self, parent):
        '''
        Create the declarations and assignments required for the
        basis-functions required by an invoke. These are added as children
        of the supplied parent node in the AST.

        :param parent: the node in the f2pygen AST that will be the
                       parent of all of the declarations and assignments
        :type parent: :py:class:`psyclone.f2pygen.SubroutineGen`
        '''
        from psyclone.f2pygen import CommentGen, AssignGen, DeclGen, \
            AllocateGen, UseGen
        var_dim_list = []
        basis_declarations = []
        op_name_list = []

        # We need BASIS and/or DIFF_BASIS if any kernel requires quadrature
        # or an evaluator
        if self._qr_vars or self._unique_evaluator_args:
            parent.add(UseGen(parent, name="function_space_mod",
                              only=True,
                              funcnames=["BASIS", "DIFF_BASIS"]))

        if self._qr_vars:
            parent.add(CommentGen(parent, ""))
            parent.add(CommentGen(parent, " Look-up quadrature variables"))
            parent.add(CommentGen(parent, ""))

            # Look-up the module- and type-names from the QUADRATURE_TYPE_MAP
            for shp in self._qr_vars:
                parent.add(UseGen(parent,
                                  name=QUADRATURE_TYPE_MAP[shp]["module"],
                                  only=True,
                                  funcnames=[
                                      QUADRATURE_TYPE_MAP[shp]["type"],
                                      QUADRATURE_TYPE_MAP[shp]["proxy_type"]]))
            self._initialise_xyz_qr(parent)
            self._initialise_xyoz_qr(parent)
            self._initialise_xoyoz_qr(parent)

        if self._unique_evaluator_args:
            parent.add(CommentGen(parent, ""))
            parent.add(CommentGen(parent,
                                  " Initialise evaluator-related quantities "
                                  "using the field(s) that are written to"))
            parent.add(CommentGen(parent, ""))

        for arg in self._unique_evaluator_args:
            # We need an 'ndf_nodal' for each unique FS for which there
            # is an evaluator

            fspace = arg.evaluator_function_space
            ndf_nodal_name = "ndf_nodal_" + fspace.mangled_name

            rhs = "%".join([arg.proxy_name_indexed, arg.ref_name(fspace),
                            "get_ndf()"])
            parent.add(AssignGen(parent, lhs=ndf_nodal_name, rhs=rhs))
            var_dim_list.append(ndf_nodal_name)
            # ...and the list of nodes for that field
            nodes_name = "nodes_" + fspace.mangled_name
            parent.add(AssignGen(
                parent, lhs=nodes_name,
                rhs="%".join([arg.proxy_name_indexed, arg.ref_name(fspace),
                              "get_nodes()"]),
                pointer=True))
            parent.add(DeclGen(parent, datatype="real", kind="r_def",
                               pointer=True,
                               entity_decls=[nodes_name+"(:,:) => null()"]))

        if self._basis_fns:
            parent.add(CommentGen(parent, ""))
            parent.add(CommentGen(parent, " Allocate basis arrays"))
            parent.add(CommentGen(parent, ""))

        for basis_fn in self._basis_fns:
            # Get the extent of the first dimension of the basis array
            first_dim = basis_first_dim_name(basis_fn["fspace"])
            if first_dim not in var_dim_list:
                var_dim_list.append(first_dim)
                rhs = "%".join([basis_fn["arg"].proxy_name_indexed,
                                basis_fn["arg"].ref_name(basis_fn["fspace"]),
                                "get_dim_space()"])
                parent.add(AssignGen(parent, lhs=first_dim, rhs=rhs))
            op_name = get_fs_operator_name("gh_basis", basis_fn["fspace"],
                                           qr_var=basis_fn["qr_var"],
                                           on_space=basis_fn["nodal_fspace"])
            if op_name not in op_name_list:
                # We haven't seen a basis with this name before so
                # need to declare it and add allocate statement
                op_name_list.append(op_name)
                if basis_fn["shape"] in VALID_QUADRATURE_SHAPES:
                    # Dimensionality of the basis arrays depends on the
                    # type of quadrature...
                    alloc_args = qr_basis_alloc_args(first_dim, basis_fn)
                    parent.add(
                        AllocateGen(parent,
                                    op_name+"("+", ".join(alloc_args)+")"))
                    # Add basis function variable to list to declare later.
                    # We use the length of alloc_args to determine how
                    # many dimensions this array has.
                    basis_declarations.append(
                        op_name+"("+",".join([":"]*len(alloc_args))+")")
                else:
                    # This is an evaluator
                    ndf_nodal_name = "ndf_nodal_" + basis_fn["nodal_fspace"].\
                                     mangled_name
                    alloc_args_str = ", ".join(
                        [first_dim, get_fs_ndf_name(basis_fn["fspace"]),
                         ndf_nodal_name])
                    parent.add(AllocateGen(parent,
                                           op_name+"("+alloc_args_str+")"))
                    # add basis function variable to list to declare later
                    basis_declarations.append(op_name+"(:,:,:)")

        if self._diff_basis_fns:
            parent.add(CommentGen(parent, ""))
            parent.add(CommentGen(parent,
                                  " Allocate differential basis arrays"))
            parent.add(CommentGen(parent, ""))

        for basis_fn in self._diff_basis_fns:
            # initialise 'diff_dim' variable for this function
            # space and add name to list to declare later
            first_dim = diff_basis_first_dim_name(basis_fn["fspace"])
            if first_dim not in var_dim_list:
                var_dim_list.append(first_dim)
                rhs = "%".join([basis_fn["arg"].proxy_name_indexed,
                                basis_fn["arg"].ref_name(basis_fn["fspace"]),
                                "get_dim_space_diff()"])
                parent.add(AssignGen(parent, lhs=first_dim, rhs=rhs))
            op_name = get_fs_operator_name("gh_diff_basis",
                                           basis_fn["fspace"],
                                           qr_var=basis_fn["qr_var"],
                                           on_space=basis_fn["nodal_fspace"])
            if op_name not in op_name_list:
                # We haven't seen a differential basis with this name before
                # so need to declare it and add allocate statement
                op_name_list.append(op_name)
                if basis_fn["shape"] in VALID_QUADRATURE_SHAPES:
                    # Dimensionality of the differential-basis arrays
                    # depends on the type of quadrature...
                    alloc_args = qr_basis_alloc_args(first_dim, basis_fn)
                    parent.add(
                        AllocateGen(parent,
                                    op_name+"("+", ".join(alloc_args)+")"))
                    # Add diff-basis function variable to list to
                    # declare later.  We use the length of alloc_args
                    # to determine how many dimensions this array has.
                    basis_declarations.append(
                        op_name+"("+",".join([":"]*len(alloc_args))+")")
                else:
                    # This is an evaluator.
                    # Need the number of dofs in the field being written by
                    # the kernel that requires this evaluator
                    ndf_nodal_name = "ndf_nodal_" + basis_fn["nodal_fspace"].\
                                     mangled_name
                    alloc_str = ", ".join(
                        [diff_basis_first_dim_name(basis_fn["fspace"]),
                         get_fs_ndf_name(basis_fn["fspace"]),
                         ndf_nodal_name])
                    parent.add(AllocateGen(parent, op_name+"("+alloc_str+")"))
                    # Add diff-basis function variable to list to declare later
                    basis_declarations.append(op_name+"(:,:,:)")

        if var_dim_list:
            # declare dim and diff_dim for all function spaces
            parent.add(DeclGen(parent, datatype="integer",
                               entity_decls=var_dim_list))
        if basis_declarations:
            # declare the basis function arrays
            parent.add(DeclGen(parent, datatype="real",
                               allocatable=True,
                               kind="r_def",
                               entity_decls=basis_declarations))

    def _initialise_xyz_qr(self, parent):
        '''
        Add in the initialisation of variables needed for XYZ
        quadrature

        :param parent: the node in the AST representing the PSy subroutine
                       in which to insert the initialisation
        :type parent: :py:class:``psyclone.f2pygen.SubroutineGen`
        '''
        # This shape is not yet supported so we do nothing
        return

    def _initialise_xyoz_qr(self, parent):
        '''
        Add in the initialisation of variables needed for XYoZ
        quadrature

        :param parent: the node in the AST representing the PSy subroutine
                       in which to insert the initialisation
        :type parent: :py:class:``psyclone.f2pygen.SubroutineGen`
        '''
        from psyclone.f2pygen import AssignGen, DeclGen

        if "gh_quadrature_xyoz" not in self._qr_vars:
            return

        qr_vars = ["np_xy", "np_z"]
        qr_ptr_vars = ["weights_xy", "weights_z"]

        for qr_arg_name in self._qr_vars["gh_quadrature_xyoz"]:

            # We generate unique names for the integers holding the numbers
            # of quadrature points by appending the name of the quadrature
            # argument
            parent.add(
                DeclGen(
                    parent, datatype="integer",
                    entity_decls=[name+"_"+qr_arg_name for name in qr_vars]))
            decl_list = [name+"_"+qr_arg_name+"(:) => null()"
                         for name in qr_ptr_vars]
            parent.add(
                DeclGen(parent, datatype="real", pointer=True,
                        kind="r_def", entity_decls=decl_list))
            # Get the quadrature proxy
            proxy_name = qr_arg_name + "_proxy"
            parent.add(
                AssignGen(parent, lhs=proxy_name,
                          rhs=qr_arg_name+"%"+"get_quadrature_proxy()"))
            # Number of points in each dimension
            for qr_var in qr_vars:
                parent.add(
                    AssignGen(parent, lhs=qr_var+"_"+qr_arg_name,
                              rhs=proxy_name+"%"+qr_var))
            # Pointers to the weights arrays
            for qr_var in qr_ptr_vars:
                parent.add(
                    AssignGen(parent, pointer=True,
                              lhs=qr_var+"_"+qr_arg_name,
                              rhs=proxy_name+"%"+qr_var))

    def _initialise_xoyoz_qr(self, parent):
        '''
        Add in the initialisation of variables needed for XoYoZ
        quadrature

        :param parent: the node in the AST representing the PSy subroutine
                       in which to insert the initialisation
        :type parent: :py:class:``psyclone.f2pygen.SubroutineGen`
        '''
        # This shape is not yet supported so we do nothing
        return

    def compute_basis_fns(self, parent):
        '''
        Generates the necessary Fortran to compute the values of
        any basis/diff-basis arrays required

        :param parent: Node in the f2pygen AST which will be the parent
                       of the assignments created in this routine
        :type parent: :py:class:`psyclone.f2pygen.SubroutineGen`
        '''
        from psyclone.f2pygen import CommentGen, AssignGen, CallGen, DoGen, \
            DeclGen
        loop_var_list = set()
        op_name_list = []
        # add calls to compute the values of any basis arrays
        if self._basis_fns:
            parent.add(CommentGen(parent, ""))
            parent.add(CommentGen(parent, " Compute basis arrays"))
            parent.add(CommentGen(parent, ""))

        for basis_fn in self._basis_fns:
            op_name = get_fs_operator_name("gh_basis",
                                           basis_fn["fspace"],
                                           qr_var=basis_fn["qr_var"],
                                           on_space=basis_fn["nodal_fspace"])
            if op_name in op_name_list:
                # Jump over any basis arrays we've seen before
                continue
            op_name_list.append(op_name)

            if basis_fn["shape"] in VALID_QUADRATURE_SHAPES:
                # Create the argument list
                args = ["BASIS",
                        basis_fn["arg"].proxy_name_indexed + "%" +
                        basis_fn["arg"].ref_name(basis_fn["fspace"]),
                        basis_first_dim_name(basis_fn["fspace"]),
                        get_fs_ndf_name(basis_fn["fspace"]),
                        op_name]
                # insert the basis array call
                parent.add(
                    CallGen(parent,
                            name=basis_fn["qr_var"]+"%compute_function",
                            args=args))
            else:
                # We have an evaluator
                nodal_loop_var = "df_nodal"
                loop_var_list.add(nodal_loop_var)

                nodal_dof_loop = DoGen(
                    parent, nodal_loop_var, "1",
                    "ndf_nodal_"+basis_fn["nodal_fspace"].mangled_name)
                parent.add(nodal_dof_loop)

                dof_loop_var = "df_" + basis_fn["fspace"].mangled_name
                loop_var_list.add(dof_loop_var)

                dof_loop = DoGen(nodal_dof_loop, dof_loop_var,
                                 "1", get_fs_ndf_name(basis_fn["fspace"]))
                nodal_dof_loop.add(dof_loop)
                lhs = op_name + "(:," + "df_" + \
                    basis_fn["fspace"].mangled_name + "," + "df_nodal)"
                rhs = "%".join(
                    [basis_fn["arg"].proxy_name_indexed,
                     basis_fn["arg"].ref_name(basis_fn["fspace"]),
                     "call_function(BASIS," + dof_loop_var +
                     ",nodes_" + basis_fn["nodal_fspace"].mangled_name +
                     "(:," + nodal_loop_var + "))"])
                dof_loop.add(AssignGen(dof_loop, lhs=lhs, rhs=rhs))

        if self._diff_basis_fns:
            parent.add(CommentGen(parent, ""))
            parent.add(CommentGen(parent,
                                  " Compute differential basis arrays"))
            parent.add(CommentGen(parent, ""))

        for dbasis_fn in self._diff_basis_fns:
            op_name = get_fs_operator_name("gh_diff_basis",
                                           dbasis_fn["fspace"],
                                           qr_var=dbasis_fn["qr_var"],
                                           on_space=dbasis_fn["nodal_fspace"])
            if op_name in op_name_list:
                # Jump over any differential basis arrays we've seen before
                continue
            op_name_list.append(op_name)

            if dbasis_fn["shape"] in VALID_QUADRATURE_SHAPES:
                # Create the argument list
                args = ["DIFF_BASIS",
                        dbasis_fn["arg"].proxy_name_indexed + "%" +
                        dbasis_fn["arg"].ref_name(dbasis_fn["fspace"]),
                        diff_basis_first_dim_name(dbasis_fn["fspace"]),
                        get_fs_ndf_name(dbasis_fn["fspace"]),
                        op_name]
                # insert the call to compute the diff basis array
                parent.add(
                    CallGen(parent,
                            name=dbasis_fn["qr_var"]+"%compute_function",
                            args=args))
            else:
                # Have an evaluator
                nodal_loop_var = "df_nodal"
                loop_var_list.add(nodal_loop_var)

                nodal_dof_loop = DoGen(
                    parent, "df_nodal", "1",
                    "ndf_nodal_"+dbasis_fn["nodal_fspace"].mangled_name)
                parent.add(nodal_dof_loop)

                df_loop_var = "df_"+dbasis_fn["fspace"].mangled_name
                loop_var_list.add(df_loop_var)

                dof_loop = DoGen(nodal_dof_loop, df_loop_var,
                                 "1", get_fs_ndf_name(dbasis_fn["fspace"]))
                nodal_dof_loop.add(dof_loop)
                lhs = op_name + "(:," + "df_" + \
                    dbasis_fn["fspace"].mangled_name + "," + "df_nodal)"
                rhs = "%".join(
                    [dbasis_fn["arg"].proxy_name_indexed,
                     dbasis_fn["arg"].ref_name(dbasis_fn["fspace"]),
                     "call_function(DIFF_BASIS," + df_loop_var +
                     ",nodes_" + dbasis_fn["nodal_fspace"].mangled_name +
                     "(:," + nodal_loop_var + "))"])
                dof_loop.add(AssignGen(dof_loop, lhs=lhs, rhs=rhs))

        if loop_var_list:
            # Declare any loop variables
            parent.add(DeclGen(parent, datatype="integer",
                               entity_decls=list(loop_var_list)))

    def deallocate(self, parent):
        '''
        Add code to deallocate all basis/diff-basis function arrays

        :param parent: node in the f2pygen AST to which the deallocate
                       calls will be added
        :type parent: :py:class:`psyclone.f2pygen.SubroutineGen`
        '''
        from psyclone.f2pygen import CommentGen, DeallocateGen

        if self._basis_fns or self._diff_basis_fns:
            # deallocate all allocated basis function arrays
            parent.add(CommentGen(parent, ""))
            parent.add(CommentGen(parent, " Deallocate basis arrays"))
            parent.add(CommentGen(parent, ""))

        func_space_var_names = set()
        for basis_fn in self._basis_fns:
            # add the basis array name to the list to use later
            op_name = get_fs_operator_name("gh_basis",
                                           basis_fn["fspace"],
                                           qr_var=basis_fn["qr_var"],
                                           on_space=basis_fn["nodal_fspace"])
            func_space_var_names.add(op_name)
        for basis_fn in self._diff_basis_fns:
            # add the diff_basis array name to the list to use later
            op_name = get_fs_operator_name("gh_diff_basis",
                                           basis_fn["fspace"],
                                           qr_var=basis_fn["qr_var"],
                                           on_space=basis_fn["nodal_fspace"])
            func_space_var_names.add(op_name)

        if func_space_var_names:
            # add the required deallocate call
            parent.add(DeallocateGen(parent, list(func_space_var_names)))


class DynInvoke(Invoke):
    '''The Dynamo specific invoke class. This passes the Dynamo
    specific schedule class to the base class so it creates the one we
    require.  Also overrides the gen_code method so that we generate
    dynamo specific invocation code.

    '''
    def __init__(self, alg_invocation, idx):
        '''
        :param alg_invocation: node in the AST describing the invoke call
        :type alg_invocation: :py:class:`psyclone.parse.InvokeCall`
        :param int idx: the position of the invoke in the list of invokes
                        contained in the Algorithm
        :raises GenerationError: if integer reductions are required in the
        psy-layer
        '''
        if False:  # pylint: disable=using-constant-test
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

        # Initialise the object holding all information on the quadrature
        # and/or evaluators required by this invoke
        self.evaluators = DynInvokeBasisFns(self.schedule)

        # Initialise the object holding all information related to meshes
        # and inter-grid operations
        self.meshes = DynMeshes(self.schedule, self.psy_unique_vars)

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
            for loop in self.schedule.loops():
                loop.create_halo_exchanges()
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

    def is_coloured(self):
        ''' Returns true if at least one of the loops in the
        schedule of this invoke has been coloured '''
        for loop in self.schedule.loops():
            if loop.loop_type == "colours":
                return True
        return False

    def field_on_space(self, func_space):
        ''' If a field exists on this space for any kernel in this
        invoke then return that field. Otherwise return None. '''
        for kern_call in self.schedule.calls():
            field = field_on_space(func_space, kern_call.arguments)
            if field:
                return field
        return None

    def gen_code(self, parent):
        '''
        Generates Dynamo specific invocation code (the subroutine
        called by the associated invoke call in the algorithm
        layer). This consists of the PSy invocation subroutine and the
        declaration of its arguments.
        :param parent: The parent node in the AST (of the code to be generated)
                       to which the node describing the PSy subroutine will be
                       added
        :type parent: :py:class:`psyclone.f2pygen.ModuleGen`
        '''
        from psyclone.f2pygen import SubroutineGen, TypeDeclGen, AssignGen, \
            DeclGen, CommentGen
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

        # Declare any mesh objects (including those for inter-grid kernels)
        self.meshes.declarations(invoke_sub)

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
        # are read or written (operators are always on discontinuous spaces
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
        self.evaluators.declare_qr(invoke_sub)

        # declare and initialise proxies for each of the (non-scalar)
        # arguments
        invoke_sub.add(CommentGen(invoke_sub, ""))
        invoke_sub.add(CommentGen(invoke_sub,
                                  " Initialise field and/or operator proxies"))
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
        if cma_op_proxy_decs:
            invoke_sub.add(
                TypeDeclGen(invoke_sub,
                            datatype="columnwise_operator_proxy_type",
                            entity_decls=cma_op_proxy_decs))

        # Initialise the number of layers
        invoke_sub.add(CommentGen(invoke_sub, ""))
        invoke_sub.add(CommentGen(invoke_sub, " Initialise number of layers"))
        invoke_sub.add(CommentGen(invoke_sub, ""))

        # Find the first argument that is not a scalar. Also, if there
        # any CMA operators as arguments then keep a reference to one
        # of them so that we can look-up the number of columns in the
        # mesh using its proxy
        first_var = None
        cma_op = None
        for var in self.psy_unique_vars:
            if not first_var and var.type not in VALID_SCALAR_NAMES:
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

        # Initialise mesh object(s) and all related quantities for any
        # inter-grid kernels (number of fine cells per coarse cell etc.)
        self.meshes.initialise(invoke_sub)

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

        if self.schedule.reductions(reprod=True):
            # we have at least one reproducible reduction so we need
            # to know the number of OpenMP threads
            from psyclone.f2pygen import UseGen
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

        var_list = []
        # loop over all unique function spaces used by the kernels in this
        # invoke
        for function_space in self.unique_fss():
            # Initialise information associated with this function space
            invoke_sub.add(CommentGen(invoke_sub, ""))
            invoke_sub.add(
                CommentGen(invoke_sub, " Initialise number of DoFs for " +
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
        if var_list:
            # declare ndf and undf for all function spaces
            invoke_sub.add(DeclGen(invoke_sub, datatype="integer",
                                   entity_decls=var_list))

        # Initialise basis and/or differential-basis functions
        self.evaluators.initialise_basis_fns(invoke_sub)

        if self.is_coloured():
            # Declare the colour map
            declns = ["cmap(:,:)"]
            if not config.DISTRIBUTED_MEMORY:
                # Declare the array holding the no. of cells of each
                # colour. For distributed memory this variable is not
                # used, as a function is called to determine the upper
                # bound in a loop
                declns.append("ncp_colour(:)")
            invoke_sub.add(DeclGen(invoke_sub, datatype="integer",
                                   pointer=True,
                                   entity_decls=declns))
            if not config.DISTRIBUTED_MEMORY:
                # Declaration of variable to hold the number of
                # colours. For distributed memory this variable is not
                # used, as a function is called to determine loop
                # colour information
                invoke_sub.add(DeclGen(invoke_sub, datatype="integer",
                                       entity_decls=["ncolour"]))

        # add calls to compute the values of any basis arrays
        self.evaluators.compute_basis_fns(invoke_sub)

        invoke_sub.add(CommentGen(invoke_sub, ""))
        if config.DISTRIBUTED_MEMORY:
            invoke_sub.add(CommentGen(invoke_sub, " Call kernels and "
                                      "communication routines"))
        else:
            invoke_sub.add(CommentGen(invoke_sub, " Call our kernels"))
        invoke_sub.add(CommentGen(invoke_sub, ""))

        # add content from the schedule
        self.schedule.gen_code(invoke_sub)

        # Deallocate any basis arrays
        self.evaluators.deallocate(invoke_sub)

        invoke_sub.add(CommentGen(invoke_sub, ""))
        # finally, add me to my parent
        parent.add(invoke_sub)


class DynSchedule(Schedule):
    ''' The Dynamo specific schedule class. This passes the Dynamo-
    specific factories for creating kernel and infrastructure calls
    to the base class so it creates the ones we require. '''

    def __init__(self, arg):
        from psyclone.dynamo0p3_builtins import DynBuiltInCallFactory
        Schedule.__init__(self, DynKernCallFactory, DynBuiltInCallFactory, arg)

    def view(self, indent=0):
        '''a method implemented by all classes in a schedule which display the
        tree in a textual form. This method overrides the default view
        method to include distributed memory information '''
        print self.indent(indent) + self.coloured_text + "[invoke='" + \
            self.invoke.name + "' dm="+str(config.DISTRIBUTED_MEMORY)+"]"
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
        from psyclone.f2pygen import AssignGen, TypeDeclGen, UseGen
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


def _create_depth_list(halo_info_list):
    '''Halo's may have more than one dependency. This method simplifies
    multiple dependencies to remove duplicates and any obvious
    redundancy. For example, if one dependency is for depth=1 and
    another for depth=2 then we do not need the former as it is
    covered by the latter. Similarly, if we have a depth=extent+1 and
    another for depth=extent+2 then we do not need the former as
    it is covered by the latter.

    :param: a list containing halo access information derived from
    all read fields dependent on this halo exchange
    :type: :func:`list` of :py:class:`psyclone.dynamo0p3.HaloReadAccess`
    :return: a list containing halo depth information derived from
    the halo access information
    :rtype: :func:`list` of :py:class:`psyclone.dynamo0p3.HaloDepth`

    '''
    depth_info_list = []
    # first look to see if all field dependencies specify
    # annexed_only. If so we only access annexed dofs
    annexed_only = True
    for halo_info in halo_info_list:
        if not halo_info.annexed_only:
            annexed_only = False
            break
    if annexed_only:
        depth_info = HaloDepth()
        depth_info.set_by_value(max_depth=False, var_depth="",
                                literal_depth=1, annexed_only=True)
        return [depth_info]
    # next look to see if one of the field dependencies specifies
    # a max_depth access. If so the whole halo region is accessed
    # so we do not need to be concerned with other accesses.
    for halo_info in halo_info_list:
        if halo_info.max_depth:
            # found a max_depth access so we only need one
            # HaloDepth entry
            depth_info = HaloDepth()
            depth_info.set_by_value(max_depth=True, var_depth="",
                                    literal_depth=0, annexed_only=False)
            return [depth_info]

    for halo_info in halo_info_list:
        # go through the halo information associated with each
        # read dependency
        var_depth = halo_info.var_depth
        literal_depth = halo_info.literal_depth
        match = False
        # check whether we match with existing depth information
        for depth_info in depth_info_list:
            if depth_info.var_depth == var_depth and not match:
                # this dependence uses the same variable to
                # specify its depth as an existing one, or both do
                # not have a variable so we only have a
                # literal. Therefore we only need to update the
                # literal value with the maximum of the two
                # (e.g. var_name,1 and var_name,2 => var_name,2)
                depth_info.literal_depth = max(
                    depth_info.literal_depth, literal_depth)
                match = True
                break
        if not match:
            # no matches were found with existing variables, or no
            # variables so create a new halo depth entry
            depth_info = HaloDepth()
            depth_info.set_by_value(max_depth=False, var_depth=var_depth,
                                    literal_depth=literal_depth,
                                    annexed_only=False)
            depth_info_list.append(depth_info)
    return depth_info_list


class DynHaloExchange(HaloExchange):

    '''Dynamo specific halo exchange class which can be added to and
    manipulated in, a schedule
    '''

    def _compute_stencil_type(self):
        '''Dynamically work out the type of stencil required for this halo
        exchange as it could change as transformations are applied to
        the schedule. If all stencil accesses are of the same type then we
        return that stencil, otherwise we return the "region" stencil
        type (as it is safe for all stencils).

        :return: Return the type of stencil required for this halo exchange
        :rtype: string

        '''
        # get information about stencil accesses from all read fields
        # dependendent on this halo exchange
        halo_info_list = self._compute_halo_read_info()

        trial_stencil = halo_info_list[0].stencil_type
        for halo_info in halo_info_list:
            # assume that if stencil accesses are different that we
            # simply revert to region. We could be more clever in the
            # future e.g. x and y implies cross.
            if halo_info.stencil_type != trial_stencil:
                return "region"
        return trial_stencil

    def _compute_halo_depth(self):
        '''Dynamically determine the depth of the halo for this halo exchange,
        as the depth can change as transformations are applied to the
        schedule

        :return: Return the halo exchange depth as a fortran string
        :rtype: int

        '''
        # get information about reading from the halo from all read fields
        # dependendent on this halo exchange
        depth_info_list = self._compute_halo_read_depth_info()

        # if there is only one entry in the list we can just return
        # the depth
        if len(depth_info_list) == 1:
            depth_info = depth_info_list[0]
            if depth_info.max_depth:
                # return the maximum halo depth (which is returned by
                # calling get_halo_depth with no depth argument)
                # TODO fix hard-wired mesh name here
                return "mesh%get_halo_depth()"
            else:  # return the variable and/or literal depth expression
                return str(depth_info)
        else:
            # the depth information can't be reduced to a single
            # expression, therefore we need to determine the maximum
            # of all expresssions
            depth_str_list = [str(depth_info) for depth_info in
                              depth_info_list]
            return "max("+",".join(depth_str_list)+")"

    def _compute_halo_read_depth_info(self):
        '''Take a list of `psyclone.dynamo0p3.HaloReadAccess` objects and
        create an equivalent list of `psyclone.dynamo0p3.HaloDepth`
        objects. Whilst doing this we simplify the
        `psyclone.dynamo0p3.HaloDepth` list to remove redundant depth
        information e.g. depth=1 is not required if we have a depth=2

        :return: a list containing halo depth information derived from
        all fields dependent on this halo exchange
        :rtype: :func:`list` of :py:class:`psyclone.dynamo0p3.HaloDepth`

        '''
        # get our halo information
        halo_info_list = self._compute_halo_read_info()
        # use the halo information to generate depth information
        depth_info_list = _create_depth_list(halo_info_list)
        return depth_info_list

    def _compute_halo_read_info(self):
        '''Dynamically computes all halo read dependencies and returns the
        required halo information (i.e. halo depth and stencil type) in a
        list of HaloReadAccess objects

        :return: a list containing halo information for each read dependency
        :rtype: :func:`list` of :py:class:`psyclone.dynamo0p3.HaloReadAccess`

        '''
        read_dependencies = self.field.forward_read_dependencies()
        if not read_dependencies:
            raise GenerationError(
                "Internal logic error. There should be at least one read "
                "dependence for a halo exchange")
        return [HaloReadAccess(read_dependency) for read_dependency
                in read_dependencies]

    def _compute_halo_write_info(self):
        '''Determines how much of the halo has been cleaned from any previous
        redundant computation

        :return: a HaloWriteAccess object containing the required
        information, or None if no dependence information is found.
        :rtype::py:class:`psyclone.dynamo0p3.HaloWriteAccess` or None
        :raises GenerationError: if more than one write dependence is
        found for this halo exchange as this should not be possible

        '''
        write_dependencies = self.field.backward_write_dependencies()
        if not write_dependencies:
            # no write dependence information
            return None
        if len(write_dependencies) > 1:
            raise GenerationError(
                "Internal logic error. There should be at most one write "
                "dependence for a halo exchange. Found "
                "'{0}'".format(str(len(write_dependencies))))
        return HaloWriteAccess(write_dependencies[0])

    def required(self):
        '''Determines whether this halo exchange is definitely required (True,
        True), might be required (True, False) or is definitely not required
        (False, *). The first return argument is used to decide whether a halo
        exchange should exist. If it is True then the halo is required or
        might be required. If it is False then the halo is definitely not
        required. The second argument is used to specify whether we definitely
        know that it is required or are not sure.

        Whilst a halo exchange is generally only ever added if it is
        required, or if it may be required, this situation can change
        if redundant computation transformations are applied. The
        first argument can be used to remove such halo exchanges if
        required.

        When the first argument is True, the second argument can be
        used to see if we need to rely on the runtime (set_dirty and
        set_clean calls) and therefore add a check_dirty() call around
        the halo exchange or whether we definitely know that this halo
        exchange is required.

        This routine assumes that a stencil size provided via a
        variable may take the value 0. If a variables value is
        constrained to be 1, or more, then the logic for deciding
        whether a halo exchange is definitely required should be
        updated. Note, the routine would still be correct as is, it
        would just return more unknown results than it should).

        :return: Returns (x, y) where x specifies whether this halo
        exchange is (or might be) required - True, or is not required
        - False. If the first argument is True then the second
        argument specifies whether we definitely know that we need the
        HaloExchange - True, or are not sure - False.
        :rtype: (bool, bool)

        '''
        # get *aggregated* information about halo reads
        required_clean_info = self._compute_halo_read_depth_info()
        # get information about the halo write
        clean_info = self._compute_halo_write_info()

        # no need to test whether we return at least one read
        # dependency as _compute_halo_read_depth_info() raises an
        # exception if none are found

        if not clean_info:
            # this halo exchange has no previous write dependencies so
            # we do not know the initial state of the halo. This means
            # that we do not know if we need a halo exchange or not
            required = True
            known = False
            return required, known

        if clean_info.max_depth:
            if not clean_info.dirty_outer:
                # all of the halo is cleaned by redundant computation
                # so halo exchange is not required
                required = False
                known = True  # redundant information as it is always known
            else:
                # the last level halo is dirty
                if required_clean_info[0].max_depth:
                    # we know that we need to clean the outermost halo level
                    required = True
                    known = True
                else:
                    # we don't know whether the halo exchange is
                    # required or not as the reader reads the halo to
                    # a specified depth but we don't know the depth
                    # of the halo
                    required = True
                    known = False
            return required, known

        # at this point we know that clean_info.max_depth is False

        if not clean_info.literal_depth:
            # if literal_depth is 0 then the writer does not
            # redundantly compute so we definitely need the halo
            # exchange
            required = True
            known = True
            return required, known

        if clean_info.literal_depth == 1 and clean_info.dirty_outer:
            # the writer redundantly computes in the level 1 halo but
            # leaves it dirty (although annexed dofs are now clean).
            if len(required_clean_info) == 1 and \
               required_clean_info[0].annexed_only:
                # we definitely don't need the halo exchange as we
                # only read annexed dofs and these have been made
                # clean by the redundant computation
                required = False
                known = True  # redundant information as it is always known
            else:
                # we definitely need the halo exchange as the reader(s)
                # require the halo to be clean
                required = True
                known = True
            return required, known

        # At this point we know that the writer cleans the halo to a
        # known (literal) depth through redundant computation. We now
        # compute this value for use by the logic in the rest of the
        # routine.
        clean_depth = clean_info.literal_depth
        if clean_info.dirty_outer:
            # outer layer stays dirty
            clean_depth -= 1

        # If a literal value in any of the required clean halo depths
        # is greater than the cleaned depth then we definitely need
        # the halo exchange (as any additional variable depth would
        # increase the required depth value). We only look at the case
        # where we have multiple entries as the single entry case is
        # dealt with separately
        if len(required_clean_info) > 1:
            for required_clean in required_clean_info:
                if required_clean.literal_depth > clean_depth:
                    required = True
                    known = True
                    return required, known

        # The only other case where we know that a halo exchange is
        # required (or not) is where we read the halo to a known
        # literal depth. As the read inforation is aggregated, a known
        # literal depth will mean that there is only one
        # required_clean_info entry
        if len(required_clean_info) == 1:
            # the halo might be read to a fixed literal depth
            if required_clean_info[0].var_depth or \
               required_clean_info[0].max_depth:
                # no it isn't so we might need the halo exchange
                required = True
                known = False
            else:
                # the halo is read to a fixed literal depth.
                required_clean_depth = required_clean_info[0].literal_depth
                if clean_depth < required_clean_depth:
                    # we definitely need this halo exchange
                    required = True
                    known = True
                else:
                    # we definitely don't need this halo exchange
                    required = False
                    known = True  # redundant information as it is always known
            return required, known

        # We now know that at least one required_clean entry has a
        # variable depth and any required_clean fixed depths are less
        # than the cleaned depth so we may need a halo exchange.
        required = True
        known = False
        return required, known

    def view(self, indent=0):
        ''' Class specific view  '''
        _, known = self.required()
        runtime_check = not known
        print self.indent(indent) + (
            "{0}[field='{1}', type='{2}', depth={3}, "
            "check_dirty={4}]".format(self.coloured_text, self._field.name,
                                      self._compute_stencil_type(),
                                      self._compute_halo_depth(),
                                      runtime_check))

    def gen_code(self, parent):
        ''' Dynamo specific code generation for this class '''
        from psyclone.f2pygen import IfThenGen, CallGen, CommentGen
        if self.vector_index:
            ref = "(" + str(self.vector_index) + ")"
        else:
            ref = ""
        _, known = self.required()
        if not known:
            if_then = IfThenGen(parent, self._field.proxy_name + ref +
                                "%is_dirty(depth=" +
                                self._compute_halo_depth() + ")")
            parent.add(if_then)
            halo_parent = if_then
        else:
            halo_parent = parent
        halo_parent.add(
            CallGen(
                halo_parent, name=self._field.proxy_name + ref +
                "%halo_exchange(depth=" + self._compute_halo_depth() + ")"))
        parent.add(CommentGen(parent, ""))


class HaloDepth(object):
    '''Determines how much of the halo a read to a field accesses (the
    halo depth)
    '''
    def __init__(self):
        # literal_depth is used to store any known (literal) component
        # of the depth of halo that is accessed. It may not be the
        # full depth as there may also be an additional var_depth
        # specified.
        self._literal_depth = 0
        # var_depth is used to store any variable component of the
        # depth of halo that is accessed. It may not be the full depth
        # as there may also be an additional literal_depth specified.
        self._var_depth = None
        # max_depth specifies whether the full depth of halo (whatever
        # that might be) is accessed. If this is set then
        # literal_depth and var_depth have no meaning. max_depth being
        # False does not necessarily mean the full halo depth is not
        # accessed, rather it means that we do not know.
        self._max_depth = False
        # annexed only is True if the only access in the halo is for
        # annexed dofs
        self._annexed_only = False

    @property
    def annexed_only(self):
        '''Returns whether the access to the halo is solely to annexed dofs,
        or not

        :return: Return True if only annexed dofs are accessed in the
        halo and False otherwise
        :rtype: bool

        '''
        return self._annexed_only

    @property
    def max_depth(self):
        '''Returns whether the read to the field is known to access all of the
        halo or not

        :return: Return True if the read to the field is known to
        access all of the halo and False otherwise
        :rtype: bool

        '''
        return self._max_depth

    @property
    def var_depth(self):
        '''Returns the name of the variable specifying the depth of halo
        access if one is provided. Note, a variable will only be provided for
        stencil accesses. Also note, this depth should be added to the
        literal_depth to find the total depth.

        :return: Return a variable name specifying the halo
        access depth, if one exists, and None if not
        :rtype: String

        '''
        return self._var_depth

    @property
    def literal_depth(self):
        '''Returns the known fixed (literal) depth of halo access. Note, this
        depth should be added to the var_depth to find the total
        depth.

        :return: Return the known fixed (literal) halo
        access depth
        :rtype: integer

        '''
        return self._literal_depth

    @literal_depth.setter
    def literal_depth(self, value):
        ''' Set the known fixed (literal) depth of halo access.

        :parameter value: Set the known fixed (literal) halo
        access depth
        :type value: integer

        '''
        self._literal_depth = value

    def set_by_value(self, max_depth, var_depth, literal_depth, annexed_only):
        '''Set halo depth information directly

        :param max_depth: True if the field accesses all of the halo
        and False otherwise
        :type max_depth: bool
        :param var_depth: A variable name specifying the halo access
        depth, if one exists, and None if not
        :type var_depth: String
        :param literal_depth: The known fixed (literal) halo access
        depth
        :type literal_depth: integer
        :param annexed_only: True if only the halo's annexed dofs are
        accessed and False otherwise
        :type max_depth: bool

        '''
        self._max_depth = max_depth
        self._var_depth = var_depth
        self._literal_depth = literal_depth
        self._annexed_only = annexed_only

    def __str__(self):
        '''return the depth of a halo dependency
        as a string'''
        depth_str = ""
        if self.var_depth:
            depth_str += self.var_depth
            if self.literal_depth:
                depth_str += "+"
        if self.literal_depth:
            depth_str += str(self.literal_depth)
        return depth_str


def halo_check_arg(field, access_types):
    '''Support function which performs checks to ensure the first argument
    is a field, that the field is contained within Kernel or Builtin
    call and that the field is accessed in one of the ways specified
    by the second argument. If no error is reported it returns the
    call object containing this argument

    :param field: the argument object we are checking
    :type field: :py:class:`psyclone.dynamo0p3.DynArgument`
    :param access_types: list of access types that the field access
    must be one of
    :type access_types: :func:`list` of String
    :return: the call containing the argument object
    :rtype: :py:class:`psyclone.psyGen.Call`
    :raises GenerationError: if the first argument to this function is
    the wrong type
    :raises GenerationError: if the first argument is not accessed in
    one of the ways specified by the second argument to the function
    :raises GenerationError: if the first argument is not contained
    within a call object

    '''
    try:
        # get the kernel/builtin call associated with this field
        call = field.call
    except AttributeError:
        raise GenerationError(
            "HaloInfo class expects an argument of type DynArgument, or "
            "equivalent, on initialisation, but found, "
            "'{0}'".format(type(field)))
    if field.access not in access_types:
        raise GenerationError(
            "In HaloInfo class, field '{0}' should be one of {1}, but found "
            "'{2}'".format(field.name, access_types, field.access))
    from psyclone.dynamo0p3_builtins import DynBuiltIn
    if not (isinstance(call, DynKern) or isinstance(call, DynBuiltIn)):
        raise GenerationError(
            "In HaloInfo class, field '{0}' should be from a call but "
            "found {1}".format(field.name, type(call)))
    return call


class HaloWriteAccess(HaloDepth):
    '''Determines how much of a field's halo is written to (the halo depth)
    when a field is accessed in a particular kernel within a
    particular loop nest

    :param field: the field that we are concerned with
    :type field: :py:class:`psyclone.dynamo0p3.DynArgument`

    '''
    def __init__(self, field):
        HaloDepth.__init__(self)
        self._compute_from_field(field)

    @property
    def dirty_outer(self):
        '''Returns True if the writer is continuous and accesses the halo and
        False otherwise. It indicates that the outer level of halo that has
        been written to is actually dirty (well to be precise it is a partial
        sum).

        :return: Return True if the outer layer of halo
        that is written to remains dirty and False otherwise.
        :rtype: bool

        '''
        return self._dirty_outer

    def _compute_from_field(self, field):
        '''Internal method to compute what parts of a field's halo are written
        to in a certain kernel and loop. The information computed is
        the depth of access and validity of the data after
        writing. The depth of access can be the maximum halo depth or
        a literal depth and the outer halo layer that is written to
        may be dirty or clean.

        :param field: the field that we are concerned with
        :type field: :py:class:`psyclone.dynamo0p3.DynArgument`

        '''
        call = halo_check_arg(field, GH_WRITE_ACCESSES)
        # no test required here as all calls exist within a loop
        loop = call.parent
        # The outermost halo level that is written to is dirty if it
        # is a continuous field which writes into the halo in a loop
        # over cells
        self._dirty_outer = (
            not field.discontinuous and
            loop.iteration_space == "cells" and
            loop.upper_bound_name in HALO_ACCESS_LOOP_BOUNDS)
        depth = 0
        max_depth = False
        if loop.upper_bound_name in HALO_ACCESS_LOOP_BOUNDS:
            # loop does redundant computation
            if loop.upper_bound_halo_depth:
                # loop redundant computation is to a fixed literal depth
                depth = loop.upper_bound_halo_depth
            else:
                # loop redundant computation is to the maximum depth
                max_depth = True
        # If this is an inter-grid kernel and we're writing to the
        # field on the fine mesh then the halo depth is effectively
        # doubled
        if call.is_intergrid and field.mesh == "gh_fine":
            depth *= 2
        # The third argument for set_by_value specifies the name of a
        # variable used to specify the depth. Variables are currently
        # not used when a halo is written to, so we pass None which
        # indicates there is no variable.
        # the fifth argument for set_by_value indicates whether we
        # only access annexed_dofs. At the moment this is not possible
        # when modifying a field so we always return False
        HaloDepth.set_by_value(self, max_depth, None, depth, False)


class HaloReadAccess(HaloDepth):
    '''Determines how much of a field's halo is read (the halo depth) and
    additionally the access pattern (the stencil) when a field is
    accessed in a particular kernel within a particular loop nest

    '''
    def __init__(self, field):
        '''
        :param field: the field that we want to get information on
        :type field: :py:class:`psyclone.dynamo0p3.DynKernelArgument`

        '''
        HaloDepth.__init__(self)
        self._stencil_type = None
        self._compute_from_field(field)

    @property
    def stencil_type(self):
        '''Returns the type of stencil access used by the field(s) in the halo
        if one exists. If redundant computation (accessing the full
        halo) is combined with a stencil access (potentially accessing
        a subset of the halo) then the access is assumed to be full
        access (region) for all depths.

        :return: Return the type of stencil access used
        or None if there is no stencil.
        :rtype: String

        '''
        return self._stencil_type

    def _compute_from_field(self, field):
        '''Internal method to compute which parts of a field's halo are read
        in a certain kernel and loop. The information computed is the
        depth of access and the access pattern. The depth of access
        can be the maximum halo depth, a variable specifying the depth
        and/or a literal depth. The access pattern will only be
        specified if the kernel code performs a stencil access on the
        field.

        :param field: the field that we are concerned with
        :type field: :py:class:`psyclone.dynamo0p3.DynArgument`

        '''
        self._annexed_only = False
        call = halo_check_arg(field, GH_READ_ACCESSES)
        # no test required here as all calls exist within a loop
        loop = call.parent
        # now we have the parent loop we can work out what part of the
        # halo this field accesses
        if loop.upper_bound_name in HALO_ACCESS_LOOP_BOUNDS:
            # this loop performs redundant computation
            if loop.upper_bound_halo_depth:
                # loop redundant computation is to a fixed literal depth
                self._literal_depth = loop.upper_bound_halo_depth
            else:
                # loop redundant computation is to the maximum depth
                self._max_depth = True
        elif loop.upper_bound_name == "ncolour":
            # currenty coloured loops are always transformed from
            # cell_halo depth 1 loops
            self._literal_depth = 1
        elif loop.upper_bound_name == "ncells":
            if field.descriptor.stencil:
                # no need to worry about annexed dofs (if they exist)
                # as the stencil will cover these (this is currently
                # guaranteed as halo exchanges only exchange full
                # halos)
                pass
            else:  # there is no stencil
                if field.discontinuous:
                    # There are only local accesses
                    pass
                else:
                    # This is a continuous field which therefore
                    # accesses annexed dofs. We set access to the
                    # level 1 halo here as there is currently no
                    # mechanism to perform a halo exchange solely on
                    # annexed dofs.
                    self._literal_depth = 1
                    self._annexed_only = True
        elif loop.upper_bound_name == "ndofs":
            # we only access owned dofs so there is no access to the
            # halo
            pass
        else:
            raise GenerationError(
                "Internal error in HaloReadAccess._compute_from_field. Found "
                "unexpected loop upper bound name '{0}'".
                format(loop.upper_bound_name))

        if self._max_depth or self._var_depth or self._literal_depth:
            # Whilst stencil type has no real meaning when there is no
            # stencil it is convenient to set it to "region" when
            # there is redundant computation as the halo exchange
            # logic is interested in the access pattern irrespective
            # of whether there is a stencil access or not. We use
            # "region" as it means access all of the halo data which
            # is what is done when performing redundant computation
            # with no stencil.
            self._stencil_type = "region"
        if field.descriptor.stencil:
            # field has a stencil access
            if self._max_depth:
                raise GenerationError(
                    "redundant computation to max depth with a stencil is "
                    "invalid")
            else:
                self._stencil_type = field.descriptor.stencil['type']
                if self._literal_depth:
                    # halo exchange does not support mixed accesses to the halo
                    self._stencil_type = "region"
                stencil_depth = field.descriptor.stencil['extent']
                if stencil_depth:
                    # stencil_depth is provided in the kernel metadata
                    self._literal_depth += stencil_depth
                else:
                    # stencil_depth is provided by the algorithm layer
                    if field.stencil.extent_arg.is_literal():
                        # a literal is specified
                        value_str = field.stencil.extent_arg.text
                        self._literal_depth += int(value_str)
                    else:
                        # a variable is specified
                        self._var_depth = field.stencil.extent_arg.varName
        # If this is an intergrid kernel and the field in question is on
        # the fine mesh then we must double the halo depth
        if call.is_intergrid and field.mesh == "gh_fine":
            if self._literal_depth:
                self._literal_depth *= 2
            if self._var_depth:
                self._var_depth = "2*" + self._var_depth


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
        self._upper_bound_halo_depth = None

    def view(self, indent=0):
        '''Print out a textual representation of this loop. We override this
        method from the Loop class because, in Dynamo0.3, the function
        space is now an object and we need to call orig_name on it. We
        also output the upper loop bound as this can now be
        modified.

        :param indent: optional argument indicating the level of
        indentation to add before outputting the class information
        :type indent: integer

        '''
        if self._upper_bound_halo_depth:
            upper_bound = "{0}({1})".format(self._upper_bound_name,
                                            self._upper_bound_halo_depth)
        else:
            upper_bound = self._upper_bound_name
        print(self.indent(indent) + self.coloured_text +
              "[type='{0}',field_space='{1}',it_space='{2}', "
              "upper_bound='{3}']".format(self._loop_type,
                                          self._field_space.orig_name,
                                          self.iteration_space, upper_bound))
        for entity in self._children:
            entity.view(indent=indent + 1)

    def load(self, kern):
        '''
        Load the state of this Loop using the supplied Kernel
        object. This method is provided so that we can individually
        construct Loop objects for a given kernel call.

        :param kern: Kernel object to use to populate state of Loop
        :type kern: :py:class:`psyclone.dynamo0p3.DynKern`
        '''
        self._kern = kern

        self._field = kern.arguments.iteration_space_arg()
        self._field_name = self._field.name
        self._field_space = self._field.function_space
        self._iteration_space = kern.iterates_over  # cells etc.

        # Loop bounds
        self.set_lower_bound("start")

        from psyclone.dynamo0p3_builtins import DynBuiltIn
        if isinstance(kern, DynBuiltIn):
            # If the kernel is a built-in/pointwise operation
            # then this loop must be over DoFs
            self.set_upper_bound("ndofs")
        else:
            if config.DISTRIBUTED_MEMORY:
                if self._field.type in VALID_OPERATOR_NAMES:
                    # We always compute operators redundantly out to the L1
                    # halo
                    self.set_upper_bound("cell_halo", index=1)
                elif (self.field_space.orig_name in
                      DISCONTINUOUS_FUNCTION_SPACES):
                    self.set_upper_bound("ncells")
                elif self.field_space.orig_name in CONTINUOUS_FUNCTION_SPACES:
                    # Must iterate out to L1 halo for continuous quantities
                    self.set_upper_bound("cell_halo", index=1)
                elif self.field_space.orig_name in VALID_ANY_SPACE_NAMES:
                    # We don't know whether any-space is continuous or not
                    # so we have to err on the side of caution and assume that
                    # it is.
                    self.set_upper_bound("cell_halo", index=1)
                else:
                    raise GenerationError(
                        "Unexpected function space found. Expecting one of "
                        "{0} but found '{1}'".format(
                            str(VALID_FUNCTION_SPACES),
                            self.field_space.orig_name))
            else:  # sequential
                self.set_upper_bound("ncells")

    def set_lower_bound(self, name, index=None):
        ''' Set the lower bounds of this loop '''
        if name not in VALID_LOOP_BOUNDS_NAMES:
            raise GenerationError(
                "The specified lower bound loop name is invalid")
        if name in ["inner"] + HALO_ACCESS_LOOP_BOUNDS and index < 1:
            raise GenerationError(
                "The specified index '{0}' for this lower loop bound is "
                "invalid".format(str(index)))
        self._lower_bound_name = name
        self._lower_bound_index = index

    def set_upper_bound(self, name, index=None):
        '''Set the upper bound of this loop

        :param name: A loop upper bound name. This should be a supported name.
        :type name: String
        :param index: An optional argument indicating the depth of halo
        :type index: int

        '''
        if name not in VALID_LOOP_BOUNDS_NAMES:
            raise GenerationError(
                "The specified upper loop bound name is invalid. Expected one "
                "of {0} but found '{1}'".format(VALID_LOOP_BOUNDS_NAMES, name))
        if name == "start":
            raise GenerationError("'start' is not a valid upper bound")
        # Only halo bounds and inner may have an index. We could just
        # test for index here and assume that index is None for other
        # types of bounds, but checking the type of bound as well is a
        # safer option.
        if name in (["inner"] + HALO_ACCESS_LOOP_BOUNDS) and \
           index is not None:
            if index < 1:
                raise GenerationError(
                    "The specified index '{0}' for this upper loop bound is "
                    "invalid".format(str(index)))
        self._upper_bound_name = name
        self._upper_bound_halo_depth = index

    @property
    def upper_bound_name(self):
        ''' Returns the name of the upper loop bound '''
        return self._upper_bound_name

    @property
    def upper_bound_halo_depth(self):
        '''Returns the index of the upper loop bound. This is None if the upper
        bound name is not in HALO_ACCESS_LOOP_BOUNDS

        :return: the depth of the halo for a loops upper bound. If it
        is None then a depth has not been provided. The depth value is only
        valid when the upper-bound name is associated with a halo
        e.g. 'cell_halo'
        :rtype: int

        '''
        return self._upper_bound_halo_depth

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
            elif self._lower_bound_name == "ncells":
                prev_space_name = "inner"
                prev_space_index_str = "1"
            elif (self._lower_bound_name == "cell_halo" and
                  self._lower_bound_index == 1):
                prev_space_name = "ncells"
                prev_space_index_str = ""
            elif (self._lower_bound_name == "cell_halo" and
                  self._lower_bound_index > 1):
                prev_space_name = self._lower_bound_name
                prev_space_index_str = str(self._lower_bound_index - 1)
            else:
                raise GenerationError(
                    "Unsupported lower bound name '{0}' "
                    "found".format(self._lower_bound_name))
            mesh_obj_name = self._name_space_manager.create_name(
                root_name="mesh", context="PSyVars", label="mesh")
            return mesh_obj_name + "%get_last_" + prev_space_name + "_cell(" \
                + prev_space_index_str + ")+1"

    def _upper_bound_fortran(self):
        ''' Create the associated fortran code for the type of upper bound

        :return: Fortran code for the upper bound of this loop
        :rtype: String

        '''
        # precompute halo_index as a string as we use it in more than
        # one of the if clauses
        halo_index = ""
        if self._upper_bound_halo_depth:
            halo_index = str(self._upper_bound_halo_depth)

        # We only require a mesh object if distributed memory is enabled
        # and the loop is over cells
        if config.DISTRIBUTED_MEMORY and \
           self._upper_bound_name in ["ncells", "cell_halo"]:
            if self._kern.is_intergrid:
                # We have more than one mesh object to choose from and we
                # want the coarse one because that determines the iteration
                # space. _field_name holds the name of the argument that
                # determines the iteration space of this kernel and that
                # is set-up to be the one on the coarse mesh (in
                # DynKerelArguments.iteration_space_arg()).
                mesh_name = "mesh_" + self._field_name
            else:
                # It's not an inter-grid kernel so there's only one mesh
                mesh_name = "mesh"
            mesh_obj_name = self._name_space_manager.create_name(
                root_name=mesh_name, context="PSyVars", label=mesh_name)

        if self._upper_bound_name == "ncolours":
            if config.DISTRIBUTED_MEMORY:
                # Extract the value in-place rather than extracting to
                # a variable first. This is the way the manual
                # reference examples were implemented so I copied these
                mesh_obj_name = self._name_space_manager.create_name(
                    root_name="mesh", context="PSyVars", label="mesh")
                return "{0}%get_ncolours()".format(mesh_obj_name)
            else:
                return "ncolour"
        elif self._upper_bound_name == "ncolour":
            return "ncp_colour(colour)"
        elif self._upper_bound_name == "colour_halo":
            # the LFRic API used here allows for colouring with
            # redundant computation. This API is now used when
            # ditributed memory is switched on (the default for
            # LFRic). THe original API (see previous elif) is now only
            # used when distributed memory is switched off.
            mesh_obj_name = self._name_space_manager.create_name(
                root_name="mesh", context="PSyVars", label="mesh")
            append = ""
            if halo_index:
                # The colouring API support an additional optional
                # argument which specifies the depth of the halo to
                # which the coloured loop computes. If no argument is
                # supplied it is assumed that the coloured loop
                # computes to the full depth of the halo (whatever that
                # may be).
                append = ","+halo_index
            return ("{0}%get_last_halo_cell_per_colour(colour"
                    "{1})".format(mesh_obj_name, append))
        elif self._upper_bound_name == "ndofs":
            if config.DISTRIBUTED_MEMORY:
                result = self.field.proxy_name_indexed + "%" + \
                    self.field.ref_name() + "%get_last_dof_owned()"
            else:
                result = self._kern.undf_name
            return result
        elif self._upper_bound_name == "ncells":
            if config.DISTRIBUTED_MEMORY:
                result = mesh_obj_name + "%get_last_edge_cell()"
            else:
                result = self.field.proxy_name_indexed + "%" + \
                    self.field.ref_name() + "%get_ncell()"
            return result
        elif self._upper_bound_name == "cell_halo":
            if config.DISTRIBUTED_MEMORY:
                return "{0}%get_last_halo_cell({1})".format(mesh_obj_name,
                                                            halo_index)
            else:
                raise GenerationError(
                    "'cell_halo' is not a valid loop upper bound for "
                    "sequential/shared-memory code")
        elif self._upper_bound_name == "dof_halo":
            if config.DISTRIBUTED_MEMORY:
                return "{0}%{1}%get_last_dof_halo({2})".format(
                    self.field.proxy_name_indexed, self.field.ref_name(),
                    halo_index)
            else:
                raise GenerationError(
                    "'dof_halo' is not a valid loop upper bound for "
                    "sequential/shared-memory code")
        elif self._upper_bound_name == "inner":
            if config.DISTRIBUTED_MEMORY:
                return "{0}%get_last_inner_cell({1})".format(mesh_obj_name,
                                                             halo_index)
            else:
                raise GenerationError(
                    "'inner' is not a valid loop upper bound for "
                    "sequential/shared-memory code")
        else:
            raise GenerationError(
                "Unsupported upper bound name '{0}' found in dynloop.upper_"
                "bound_fortran()".format(self._upper_bound_name))

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
        '''Determines whether the supplied argument has (or might have) its
        halo data read within this loop. Returns True if it does, or if
        it might and False if it definitely does not.

        :param arg: an argument contained within this loop
        :type arg: :py:class:`psyclone.dynamo0p3.DynArgument`
        :return: True if the argument reads, or might read from the
        halo and False otherwise.
        :rtype: bool

        '''
        if arg.descriptor.stencil:
            if self._upper_bound_name not in ["cell_halo", "ncells"]:
                raise GenerationError(
                    "Loop bounds other than cell_halo and ncells are "
                    "currently unsupported for kernels with stencil "
                    "accesses. Found '{0}'.".format(self._upper_bound_name))
            return self._upper_bound_name in ["cell_halo", "ncells"]
        if arg.type in VALID_SCALAR_NAMES:
            # scalars do not have halos
            return False
        elif arg.is_operator:
            # operators do not have halos
            return False
        elif arg.discontinuous and arg.access.lower() in \
                ["gh_read", "gh_readwrite"]:
            # there are no shared dofs so access to inner and ncells are
            # local so we only care about reads in the halo
            return self._upper_bound_name in HALO_ACCESS_LOOP_BOUNDS
        elif arg.access.lower() in ["gh_read", "gh_inc"]:
            # arg is either continuous or we don't know (any_space_x)
            # and we need to assume it may be continuous for
            # correctness
            if self._upper_bound_name in HALO_ACCESS_LOOP_BOUNDS:
                # we read in the halo
                return True
            elif self._upper_bound_name == "ncells":
                # we read annexed dofs
                return True
            elif self._upper_bound_name == "ndofs":
                # argument does not read from the halo
                return False
            else:
                # nothing should get to here so raise an exception
                raise GenerationError(
                    "Internal error in _halo_read_access. It should not be "
                    "possible to get to here. loop upper bound name is '{0}' "
                    "and arg '{1}' access is '{2}'.".format(
                        self._upper_bound_name, arg.name, arg.access))
        else:
            # access is neither a read nor an inc so does not need halo
            return False

    def _add_halo_exchange_code(self, halo_field, idx=None):
        '''An internal helper method to add the halo exchange call immediately
        before this loop using the halo_field argument for the
        associated field information and the optional idx argument if
        the field is a vector field.

        In certain situations the halo exchange will not be
        required. This is dealt with by adding the halo exchange,
        asking it if it is required and then removing it if it is
        not. This may seem strange but the logic for determining
        whether a halo exchange is required is within the halo
        exchange class so it is simplest to do it this way

        :param halo_field: the argument requiring a halo exchange
        :type halo_field: :py:class:`psyclone.dynamo0p3.DynArgument`
        :param index: optional argument providing the vector index if
        there is one and None if not. Defaults to None.
        :type index: int or None

        '''
        exchange = DynHaloExchange(halo_field,
                                   parent=self.parent,
                                   vector_index=idx)
        self.parent.children.insert(self.position,
                                    exchange)
        # check whether this halo exchange has been placed
        # here correctly and if not, remove it.
        required, _ = exchange.required()
        if not required:
            exchange.parent.children.remove(exchange)

    def _add_halo_exchange(self, halo_field):
        '''Internal helper method to add (a) halo exchange call(s) immediately
        before this loop using the halo_field argument for the
        associated field information. If the field is a vector then
        add the appropriate number of halo exchange calls.

        :param halo_field: the argument requiring a halo exchange
        :type halo_field: :py:class:`psyclone.dynamo0p3.DynArgument`

        '''
        if halo_field.vector_size > 1:
            # the range function below returns values from
            # 1 to the vector size which is what we
            # require in our Fortran code
            for idx in range(1, halo_field.vector_size+1):
                self._add_halo_exchange_code(halo_field, idx)
        else:
            self._add_halo_exchange_code(halo_field)

    def update_halo_exchanges(self):
        '''add and/or remove halo exchanges due to changes in the loops
        bounds'''
        # this call adds any new halo exchanges that are
        # required. This is done by adding halo exchanges before this
        # loop for any fields in the loop that require a halo exchange
        # and don't already have one
        self.create_halo_exchanges()
        # Now remove any existing halo exchanges that are no longer
        # required. This is done by removing halo exchanges after this
        # loop where a field in this loop previously had a forward
        # dependence on a halo exchange but no longer does
        for call in self.calls():
            for arg in call.arguments.args:
                if arg.access in GH_WRITE_ACCESSES:
                    dep_arg_list = arg.forward_read_dependencies()
                    for dep_arg in dep_arg_list:
                        if isinstance(dep_arg.call, DynHaloExchange):
                            # found a halo exchange as a forward dependence
                            # ask the halo exchange if it is required
                            halo_exchange = dep_arg.call
                            required, _ = halo_exchange.required()
                            if not required:
                                halo_exchange.parent.children.remove(
                                    halo_exchange)

    def create_halo_exchanges(self):
        '''Add halo exchanges before this loop as required by fields within
        this loop. To keep the logic simple we assume that any field
        that accesses the halo will require a halo exchange and then
        remove the halo exchange if this is not the case (when
        previous writers perform sufficient redundant computation). It
        is implemented this way as the halo exchange class determines
        whether it is required or not so a halo exchange needs to
        exist in order to find out. The appropriate logic is coded in
        the _add_halo_exchange helper method. '''
        for halo_field in self.unique_fields_with_halo_reads():
            # for each unique field in this loop that has its halo
            # read (including annexed dofs), find the previous update
            # of this field
            prev_arg_list = halo_field.backward_write_dependencies()
            if not prev_arg_list:
                # field has no previous dependence so create new halo
                # exchange(s) as we don't know the state of the fields
                # halo on entry to the invoke
                self._add_halo_exchange(halo_field)
            else:
                # field has one or more previous dependencies
                if len(prev_arg_list) > 1:
                    # field has more than one previous dependencies so
                    # should be a vector
                    if halo_field.vector_size <= 1:
                        raise GenerationError(
                            "Error in create_halo_exchanges. Expecting field "
                            "'{0}' to be a vector as it has multiple previous "
                            "dependencies".format(halo_field.name))
                    if len(prev_arg_list) != halo_field.vector_size:
                        raise GenerationError(
                            "Error in create_halo_exchanges. Expecting a "
                            "dependence for each vector index for field '{0}' "
                            "but the number of dependencies is '{1}' and the "
                            "vector size is '{2}'.".format(
                                halo_field.name, halo_field.vector_size,
                                len(prev_arg_list)))
                    for arg in prev_arg_list:
                        if not isinstance(arg.call, DynHaloExchange):
                            raise GenerationError(
                                "Error in create_halo_exchanges. Expecting "
                                "all dependent nodes to be halo exchanges")
                prev_node = prev_arg_list[0].call
                if not isinstance(prev_node, DynHaloExchange):
                    # previous dependence is not a halo exchange so
                    # call the add halo exchange logic which
                    # determines whether a halo exchange is required
                    # or not
                    self._add_halo_exchange(halo_field)

    def gen_code(self, parent):
        '''Work out the appropriate loop bounds and variable name
        depending on the loop type and then call the base class to
        generate the code.

        :param parent: an f2pygen object that will be the parent of
        f2pygen objects created in this method
        :type parent: :py:class:`psyclone.f2pygen.BaseGen`
        :raises GenerationError: if a loop over colours is within an
        OpenMP parallel region (as it must be serial)

        '''
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

            # Set halo clean/dirty for all fields that are modified
            from psyclone.f2pygen import CallGen, CommentGen, DirectiveGen
            fields = self.unique_modified_args(FIELD_ACCESS_MAP, "gh_field")

            if fields:
                parent.add(CommentGen(parent, ""))
                parent.add(CommentGen(parent,
                                      " Set halos dirty/clean for fields "
                                      "modified in the above loop"))
                parent.add(CommentGen(parent, ""))
                from psyclone.psyGen import OMPParallelDoDirective
                use_omp_master = False
                if self.is_openmp_parallel():
                    if not self.ancestor(OMPParallelDoDirective):
                        use_omp_master = True
                        # I am within an OpenMP Do directive so protect
                        # set_dirty() and set_clean() with OpenMP Master
                        parent.add(DirectiveGen(parent, "omp", "begin",
                                                "master", ""))
                # first set all of the halo dirty unless we are
                # subsequently going to set all of the halo clean
                for field in fields:
                    # The HaloWriteAccess class provides information
                    # about how the supplied field is accessed within
                    # its parent loop
                    hwa = HaloWriteAccess(field)
                    if not hwa.max_depth or hwa.dirty_outer:
                        # output set dirty as some of the halo will
                        # not be set to clean
                        if field.vector_size > 1:
                            # the range function below returns values from
                            # 1 to the vector size which is what we
                            # require in our Fortran code
                            for index in range(1, field.vector_size+1):
                                parent.add(CallGen(parent,
                                                   name=field.proxy_name +
                                                   "(" + str(index) +
                                                   ")%set_dirty()"))
                        else:
                            parent.add(CallGen(parent, name=field.proxy_name +
                                               "%set_dirty()"))
                # now set appropriate parts of the halo clean where
                # redundant computation has been performed
                for field in fields:
                    # The HaloWriteAccess class provides information
                    # about how the supplied field is accessed within
                    # its parent loop
                    hwa = HaloWriteAccess(field)
                    if hwa.literal_depth:
                        # halo access(es) is/are to a fixed depth
                        halo_depth = hwa.literal_depth
                        if hwa.dirty_outer:
                            halo_depth -= 1
                        if halo_depth > 0:
                            if field.vector_size > 1:
                                # the range function below returns
                                # values from 1 to the vector size
                                # which is what we require in our
                                # Fortran code
                                for index in range(1, field.vector_size+1):
                                    parent.add(
                                        CallGen(parent,
                                                name="{0}({1})%set_clean"
                                                "({2})".format(
                                                    field.proxy_name,
                                                    str(index),
                                                    halo_depth)))
                            else:
                                parent.add(
                                    CallGen(parent,
                                            name="{0}%set_clean({1})".
                                            format(field.proxy_name,
                                                   halo_depth)))
                    elif hwa.max_depth:
                        # halo accesses(s) is/are to the full halo
                        # depth (-1 if continuous)
                        halo_depth = "mesh%get_halo_depth()"
                        if hwa.dirty_outer:
                            # a continuous field iterating over
                            # cells leaves the outermost halo
                            # dirty
                            halo_depth += "-1"
                        if field.vector_size > 1:
                            # the range function below returns
                            # values from 1 to the vector size
                            # which is what we require in our
                            # Fortran code
                            for index in range(1, field.vector_size+1):
                                call = CallGen(parent,
                                               name="{0}({1})%set_clean("
                                               "{2})".format(
                                                   field.proxy_name,
                                                   str(index),
                                                   halo_depth))
                                parent.add(call)
                        else:
                            call = CallGen(parent, name="{0}%set_clean("
                                           "{1})".format(field.proxy_name,
                                                         halo_depth))
                            parent.add(call)

                if use_omp_master:
                    # I am within an OpenMP Do directive so protect
                    # set_dirty() and set_clean() with OpenMP Master
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
        # Whether this kernel requires quadrature
        self._qr_required = False
        # Whether this kernel requires basis functions
        self._basis_required = False
        self._eval_shape = ""
        self._qr_text = ""
        self._qr_name = None
        self._qr_args = None
        # The function space on which to evaluate basis/diff-basis functions
        # if any are required
        self._nodal_fspace = None
        self._name_space_manager = NameSpaceFactory().create()
        self._cma_operation = None
        self._is_intergrid = False  # Whether this is an inter-grid kernel

    def load(self, call, parent=None):
        '''
        Sets up kernel information with the call object which is
        created by the parser. This object includes information about
        the invoke call and the associated kernel.

        :param call: The KernelCall object from which to extract information
                     about this kernel
        :type call: :py:class:`psyclone.parse.KernelCall`
        :param parent: The parent node of the kernel call in the AST
                       we are constructing. This will be a loop.
        :type parent: :py:class:`psyclone.dynamo0p3.DynLoop`
        '''
        self._setup_basis(call.ktype)
        self._setup(call.ktype, call.module_name, call.args, parent)

    def load_meta(self, ktype):
        '''
        Sets up kernel information with the kernel type object
        which is created by the parser. The object includes the
        metadata describing the kernel code.

        :param ktype: the kernel meta-data object produced by the parser
        :type ktype: :py:class:`psyclone.dynamo0p3.DynKernMetadata`
        '''
        # create a name for each argument
        from psyclone.parse import Arg
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

        # initialise basis/diff basis so we can test whether quadrature
        # or an evaluator is required
        self._setup_basis(ktype)
        if self._basis_required and self._eval_shape in \
           VALID_QUADRATURE_SHAPES:
            # Basis functions on quadrature points are required so add
            # a qr algorithm argument
            args.append(Arg("variable", "qr"))
        self._setup(ktype, "dummy_name", args, None)

    def _setup_basis(self, kmetadata):
        '''
        Initialisation of the basis/diff basis information. This may be
        needed before general setup so is computed in a separate method.

        :param kmetadata: The kernel meta-data object produced by the
                          parser.
        :type kmetadata: :py:class:`psyclone.dynamo0p3.DynKernMetadata`
        '''
        for descriptor in kmetadata.func_descriptors:
            if len(descriptor.operator_names) > 0:
                self._basis_required = True
                self._eval_shape = kmetadata.eval_shape
                break

    def _setup(self, ktype, module_name, args, parent):
        '''
        Internal setup of kernel information.

        :param ktype: Object holding information on the parsed meta-data for
                      this kernel.
        :type ktype: :py:class:`psyclone.dynamo0p3.DynKernMetadata`
        :param str module_name: the name of the Fortran module that contains
                                the source of this Kernel
        :param args: List of Arg objects produced by the parser for the
                     arguments of this kernel call
        :type args: List of :py:class:`psyclone.parse.Arg` objects
        :param parent: the parent of this kernel call in the generated
                       AST (will be a loop object)
        :type parent: :py:class:`psyclone.dynamo0p3.DynLoop`
        '''
        from psyclone.parse import KernelCall
        Kern.__init__(self, DynKernelArguments,
                      KernelCall(module_name, ktype, args),
                      parent, check=False)
        self._func_descriptors = ktype.func_descriptors
        # Keep a record of the type of CMA kernel identified when
        # parsing the kernel meta-data
        self._cma_operation = ktype.cma_operation
        self._fs_descriptors = FSDescriptors(ktype.func_descriptors)

        # Record whether or not the kernel meta-data specifies that this
        # is an inter-grid kernel
        self._is_intergrid = ktype.is_intergrid

        # if there is a quadrature rule, what is the name of the
        # algorithm argument?
        self._qr_text = ""
        self._qr_name = None
        self._qr_args = []

        if self._eval_shape in VALID_QUADRATURE_SHAPES:
            # The quadrature-related arguments always come last
            qr_arg = args[-1]
            self._qr_text = qr_arg.text
            # use our namespace manager to create a unique name unless
            # the context and label match and in this case return the
            # previous name. We use the full text of the original
            # as a label.
            self._qr_name = self._name_space_manager.create_name(
                root_name=qr_arg.varName, context="AlgArgs",
                label=self._qr_text)
            # dynamo 0.3 api kernels require quadrature rule arguments to be
            # passed in if one or more basis functions are used by the kernel
            # and gh_shape == "gh_quadrature_***".
            # Currently only _xyoz is supported...
            # if self._eval_shape == "gh_quadrature_xyz":
            #     self._qr_args = ["np_xyz", "weights_xyz"]
            if self._eval_shape == "gh_quadrature_xyoz":
                self._qr_args = ["np_xy", "np_z", "weights_xy", "weights_z"]
            # elif self._eval_shape == "gh_quadrature_xoyoz":
            #     self._qr_args = ["np_x", "np_y", "np_z",
            #                      "weights_x", "weights_y", "weights_z"]
            else:
                raise GenerationError(
                    "Internal error: unsupported shape ({0}) found in "
                    "DynKern._setup".format(self._eval_shape))

            # If we're not a kernel stub then we will have a name for the qr
            # argument. We append this to the names of the qr-related
            # variables.
            if qr_arg.varName:
                self._qr_args = [
                    arg + "_" + self._qr_name for arg in self._qr_args]

        elif self._eval_shape == "gh_evaluator":
            # Kernel has an evaluator. The FS of the updated argument tells
            # us upon which nodal points the evaluator will be required
            arg = self.updated_arg
            if arg.is_operator:
                self._nodal_fspace = arg.function_space_to
            else:
                self._nodal_fspace = arg.function_space
        elif self._eval_shape:
            # Should never get to here!
            raise GenerationError(
                "Internal error: evaluator shape '{0}' is not recognised. "
                "Must be one of {1}.".format(self._eval_shape,
                                             VALID_EVALUATOR_SHAPES))

    @property
    def cma_operation(self):
        ''' Returns the type of CMA operation performed by this kernel
        (one of 'assembly', 'apply' or 'matrix-matrix') or None if the
        the kernel does not involve CMA operators '''
        return self._cma_operation

    @property
    def is_intergrid(self):
        '''
        Getter for whether or not this is an inter-grid kernel call
        :return: True if it is an inter-grid kernel, False otherwise
        :rtype: bool
        '''
        return self._is_intergrid

    @property
    def fs_descriptors(self):
        ''' Returns a list of function space descriptor objects of
        type FSDescriptor which contain information about the function
        spaces. '''
        return self._fs_descriptors

    @property
    def qr_required(self):
        '''
        :return: True if this kernel requires quadrature, else returns False.
        :rtype: bool
        '''
        if self._basis_required and self._eval_shape in \
           VALID_QUADRATURE_SHAPES:
            return True
        return False

    @property
    def eval_shape(self):
        '''
        :return: the value of GH_SHAPE for this kernel or an empty string
                 if none is specified
        :rtype: str
        '''
        return self._eval_shape

    @property
    def eval_fspace(self):
        '''
        :return: the function space upon which basis/diff-basis functions
                 are to be evaluated.
        :rtype: :py:class:`psyclone.dynamo0p3.FunctionSpace`
        '''
        return self._nodal_fspace

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
        from psyclone.f2pygen import ModuleGen, SubroutineGen

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
        '''Generates dynamo version 0.3 specific psy code for a call to
            the dynamo kernel instance.

        :param parent: an f2pygen object that will be the parent of
        f2pygen objects created in this method
        :type parent: :py:class:`psyclone.f2pygen.BaseGen`
        :raises GenerationError: if the loop goes beyond the level 1
        halo and an operator is accessed
        :raises GenerationError: if a kernel in the loop has an inc
        access and the loop is not coloured but is within an OpenMP
        parallel region.

        '''
        from psyclone.f2pygen import CallGen, DeclGen, AssignGen, UseGen, \
            CommentGen
        parent.add(DeclGen(parent, datatype="integer",
                           entity_decls=["cell"]))

        # Check whether this kernel reads from an operator
        op_args = self.parent.args_filter(arg_types=VALID_OPERATOR_NAMES,
                                          arg_accesses=["gh_read",
                                                        "gh_readwrite"])
        if op_args:
            # It does. We must check that our parent loop does not
            # go beyond the L1 halo.
            if self.parent.upper_bound_name == "cell_halo" and \
               self.parent.upper_bound_halo_depth > 1:
                raise GenerationError(
                    "Kernel '{0}' reads from an operator and therefore "
                    "cannot be used for cells beyond the level 1 halo. "
                    "However the containing loop goes out to level {1}".
                    format(self._name, self.parent.upper_bound_halo_depth))

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
            mesh_obj_name = self._name_space_manager.create_name(
                root_name="mesh", context="PSyVars", label="mesh")
            if config.DISTRIBUTED_MEMORY:
                # the LFRic colouring API for ditributed memory
                # differs from the API without distributed
                # memory. This is to support and control redundant
                # computation with coloured loops.
                new_parent.add(AssignGen(new_parent, pointer=True, lhs="cmap",
                                         rhs=mesh_obj_name +
                                         "%get_colour_map()"),
                               position=["before", position])
            else:
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


class ArgOrdering(object):
    '''Base class capturing the arguments, type and ordering of data in
    a Kernel call.'''
    def __init__(self, kern):
        self._kern = kern

    def generate(self):
        '''
        Specifies which arguments appear in an argument list, their type
        and their ordering. Calls methods for each type of argument
        that can be specialised by a child class for its particular need.

        :raises GenerationError: if the kernel arguments break the
                                 rules for the Dynamo 0.3 API.
        '''
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

        if self._kern.is_intergrid:
            # Inter-grid kernels require special arguments
            # The cell-map for the current column providing the mapping from
            # the coarse to the fine mesh.
            self.cell_map()

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
            # Provide arguments common to LMA operators and fields on
            # a space *unless* this is an inter-grid or CMA
            # matrix-matrix kernel
            if self._kern.cma_operation not in ["matrix-matrix"] and \
               not self._kern.is_intergrid:
                self.fs_common(unique_fs)
            # Provide additional arguments if there is a
            # field on this space
            if field_on_space(unique_fs, self._kern.arguments):
                if self._kern.is_intergrid:
                    self.fs_intergrid(unique_fs)
                else:
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
                self.field_bcs_kernel(unique_fs)

        # Add boundary dofs array to the operator boundary condition
        # kernel (enforce_operator_bc_kernel) arguments
        if self._kern.name.lower() == "enforce_operator_bc_code":
            # Sanity checks - this kernel should only have a single LMA
            # operator as argument
            if len(self._kern.arguments.args) > 1:
                raise GenerationError(
                    "Kernel {0} has {1} arguments when it should only have 1 "
                    "(an LMA operator)".format(self._kern.name,
                                               len(self._kern.arguments.args)))
            op_arg = self._kern.arguments.args[0]
            if op_arg.type != "gh_operator":
                raise GenerationError(
                    "Expected a LMA operator from which to look-up boundary "
                    "dofs but kernel {0} has argument {1}.".
                    format(self._kern.name, op_arg.type))
            if op_arg.access != "gh_readwrite":
                raise GenerationError(
                    "Kernel {0} is recognised as a kernel which applies "
                    "boundary conditions to an operator. However its operator "
                    "argument has access {1} rather than gh_readwrite.".
                    format(self._kern.name, op_arg.access))
            self.operator_bcs_kernel(op_arg.function_space_to)

        # Provide qr arguments if required
        if self._kern.qr_required:
            self.quad_rule()

    def cell_position(self):
        '''
        Add cell position information

        :raises NotImplementedError: because this is an abstract method
        '''
        raise NotImplementedError(
            "Error: ArgOrdering.cell_position() must be implemented by "
            "subclass")

    def cell_map(self):
        '''
        Add cell-map information (for inter-grid kernels)

        :raises NotImplementedError: because this is an abstract method
        '''
        raise NotImplementedError(
            "Error: ArgOrdering.cell_map() must be implemented by subclass")

    def mesh_height(self):
        '''
        Add height information (i.e. no. of layers)

        :raises NotImplementedError: because this is an abstract method
        '''
        raise NotImplementedError(
            "Error: ArgOrdering.mesh_height() must be implemented by subclass")

    def mesh_ncell2d(self):
        '''
        Add the number of columns in the mesh

        :raises NotImplementedError: because this is an abstract method
        '''
        raise NotImplementedError(
            "Error: ArgOrdering.mesh_ncell2d() must be implemented by"
            "subclass")

    def cma_operator(self, arg):
        '''
        Add information on the CMA operator

        :raises NotImplementedError: because this is an abstract method
        '''
        raise NotImplementedError("Error: ArgOrdering.cma_operator() must "
                                  "be implemented by subclass")

    def field_vector(self, arg):
        '''
        Add field-vector information for this field-vector argument

        :raises NotImplementedError: because this is an abstract method
        '''
        raise NotImplementedError(
            "Error: ArgOrdering.field_vector() must be implemented by "
            "subclass")

    def field(self, arg):
        '''
        Add field information for this field argument

        :raises NotImplementedError: because this is an abstract method
        '''
        raise NotImplementedError(
            "Error: ArgOrdering.field() must be implemented by subclass")

    def stencil_unknown_extent(self, arg):
        '''
        Add stencil extent information for this stencil argument

        :raises NotImplementedError: because this is an abstract method
        '''
        raise NotImplementedError(
            "Error: ArgOrdering.stencil_unknown_extent() must be implemented "
            "by subclass")

    def stencil_unknown_direction(self, arg):
        '''
        Add stencil direction information for this stencil argument

        :raises NotImplementedError: because this is an abstract method
        '''
        raise NotImplementedError(
            "Error: ArgOrdering.stencil_unknown_direction() must be "
            "implemented by subclass")

    def stencil(self, arg):
        '''
        Add stencil information for this stencil argument

        :raises NotImplementedError: because this is an abstract method
        '''
        raise NotImplementedError(
            "Error: ArgOrdering.stencil() must be implemented by subclass")

    def operator(self, arg):
        '''
        Add operator information for this operator argument

        :raises NotImplementedError: because this is an abstract method
        '''
        raise NotImplementedError(
            "Error: ArgOrdering.operator() must be implemented by subclass")

    def scalar(self, arg):
        '''
        Add scalar information for this scalar argument

        :raises NotImplementedError: because this is an abstract method
        '''
        raise NotImplementedError(
            "Error: ArgOrdering.scalar() must be implemented by subclass")

    def fs_common(self, function_space):
        '''
        Add information common to LMA operators and fields for this
        function space

        :raises NotImplementedError: because this is an abstract method
        '''
        raise NotImplementedError(
            "Error: ArgOrdering.fs_common() must be implemented by "
            "subclass")

    def fs_compulsory_field(self, function_space):
        '''
        Add compulsory information for this function space

        :raises NotImplementedError: because this is an abstract method
        '''
        raise NotImplementedError(
            "Error: ArgOrdering.fs_compulsory_field() must be implemented "
            "by subclass")

    def basis(self, function_space):
        '''
        Add basis function information for this function space

        :raises NotImplementedError: because this is an abstract method
        '''
        raise NotImplementedError(
            "Error: ArgOrdering.basis() must be implemented by subclass")

    def diff_basis(self, function_space):
        '''
        Add differential basis function information for this function
        space

        :raises NotImplementedError: because this is an abstract method
        '''
        raise NotImplementedError(
            "Error: ArgOrdering.diff_basis() must be implemented by subclass")

    def orientation(self, function_space):
        '''
        Add orientation information for this function space

        :raises NotImplementedError: because this is an abstract method
        '''
        raise NotImplementedError(
            "Error: ArgOrdering.orientation() must be implemented by subclass")

    def field_bcs_kernel(self, function_space):
        '''
        Add boundary condition information for a field on this function
        space

        :raises NotImplementedError: because this is an abstract method
        '''
        raise NotImplementedError(
            "Error: ArgOrdering.field_bcs_kernel() must be implemented by "
            "subclass")

    def operator_bcs_kernel(self, function_space):
        '''
        Add boundary condition information for an operator on this function
        space

        :raises NotImplementedError: because this is an abstract method
        '''
        raise NotImplementedError(
            "Error: ArgOrdering.operator_bcs_kernel() must be implemented by "
            "subclass")

    def quad_rule(self):
        '''
        Add qr information

        :raises NotImplementedError: because this is an abstract method
        '''
        raise NotImplementedError(
            "Error: ArgOrdering.quad_rule() must be implemented by subclass")

    def banded_dofmap(self, function_space):
        '''
        Add banded dofmap (required for CMA operator assembly)

        :raises NotImplementedError: because this is an abstract method
        '''
        raise NotImplementedError("Error: ArgOrdering.banded_dofmap() must"
                                  " be implemented by subclass")

    def indirection_dofmap(self, arg, operator=None):
        '''
        Add indirection dofmap required when applying a CMA operator

        :raises NotImplementedError: because this is an abstract method
        '''
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

    def cell_map(self):
        ''' Add cell-map and related cell counts to the argument list '''
        cargs = psyGen.args_filter(self._kern.args,
                                   arg_meshes=["gh_coarse"])
        carg = cargs[0]
        fargs = psyGen.args_filter(self._kern.args,
                                   arg_meshes=["gh_fine"])
        farg = fargs[0]
        base_name = "cell_map_" + carg.name
        map_name = self._name_space_manager.create_name(
            root_name=base_name, context="PSyVars", label=base_name)
        # Add the cell map to our argument list
        self._arglist.append(map_name+"(:,cell)")
        # No. of fine cells per coarse cell
        base_name = "ncpc_{0}_{1}".format(farg.name, carg.name)
        ncellpercell = self._name_space_manager.create_name(
            root_name=base_name, context="PSyVars", label=base_name)
        self._arglist.append(ncellpercell)
        # No. of columns in the fine mesh
        base_name = "ncell_{0}".format(farg.name)
        ncell_fine = self._name_space_manager.create_name(
            root_name=base_name, context="PSyVars", label=base_name)
        self._arglist.append(ncell_fine)

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

    def fs_common(self, function_space):
        '''add function-space related arguments common to LMA operators and
        fields'''
        # There is currently one argument: "ndf"
        ndf_name = get_fs_ndf_name(function_space)
        self._arglist.append(ndf_name)

    def fs_compulsory_field(self, function_space):
        '''add compulsory arguments to the argument list, when there is a
        field on this function space'''
        undf_name = get_fs_undf_name(function_space)
        self._arglist.append(undf_name)
        map_name = get_fs_map_name(function_space)
        self._arglist.append(map_name+"(:,"+self._cell_ref_name+")")

    def fs_intergrid(self, function_space):
        '''
        Add function-space related arguments for an intergrid kernel

        :param function_space: the function space for which to add arguments
        :type function_space: :py:class:`psyclone.dynamo0p3.FunctionSpace`
        '''
        # Is this FS associated with the coarse or fine mesh? (All fields
        # on a given mesh must be on the same FS.)
        arg = self._kern.arguments.get_arg_on_space(function_space)
        if arg.mesh == "gh_fine":
            # For the fine mesh, we need ndf, undf and the *whole*
            # dofmap
            self.fs_common(function_space)
            undf_name = get_fs_undf_name(function_space)
            self._arglist.append(undf_name)
            map_name = get_fs_map_name(function_space)
            self._arglist.append(map_name)
        else:
            # For the coarse mesh we only need undf and the dofmap for
            # the current column
            self.fs_compulsory_field(function_space)

    def basis(self, function_space):
        '''
        Add basis function information for this function space to the
        argument list.

        :param function_space: the function space for which the basis
                               function is required
        :type function_space: :py:class:`psyclone.dynamo0p3.FunctionSpace`
        '''
        basis_name = get_fs_basis_name(function_space,
                                       qr_var=self._kern.qr_name,
                                       on_space=self._kern.eval_fspace)
        self._arglist.append(basis_name)

    def diff_basis(self, function_space):
        '''
        Add differential basis information for the function space to the
        argument list.

        :param function_space: the function space for which the differential
                               basis functions are required
        :type function_space: :py:class:`psyclone.dynamo0p3.FunctionSpace`
        '''
        diff_basis_name = get_fs_diff_basis_name(
            function_space, qr_var=self._kern.qr_name,
            on_space=self._kern.eval_fspace)
        self._arglist.append(diff_basis_name)

    def orientation(self, function_space):
        '''add orientation information for this function space to the
        argument list'''
        orientation_name = get_fs_orientation_name(function_space)
        self._arglist.append(orientation_name)

    def field_bcs_kernel(self, function_space):
        ''' implement the boundary_dofs array fix for a field '''
        from psyclone.f2pygen import DeclGen, AssignGen
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

    def operator_bcs_kernel(self, function_space):
        ''' Supply necessary additional arguments for the kernel that
        applies boundary conditions to a LMA operator '''
        from psyclone.f2pygen import DeclGen, AssignGen
        # This kernel has only a single LMA operator as argument.
        # Checks for this are performed in ArgOrdering.generate()
        op_arg = self._kern.arguments.args[0]
        self._arglist.append("boundary_dofs")
        parent = self._parent
        parent.add(DeclGen(parent, datatype="integer",
                           pointer=True, entity_decls=[
                               "boundary_dofs(:,:) => null()"]))
        new_parent, position = parent.start_parent_loop()
        new_parent.add(AssignGen(new_parent, pointer=True,
                                 lhs="boundary_dofs",
                                 rhs=op_arg.proxy_name +
                                 "%fs_to%get_boundary_dofs()"),
                       position=["before", position])

    def quad_rule(self):
        ''' add qr information to the argument list'''
        self._arglist.extend(self._kern.qr_args)

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
        '''
        :param kern: Kernel for which to create argument list
        :type kern: :py:class:`psyclone.dynamo0p3.DynKern`
        :param parent: Parent subroutine which calls the kernel
        :type parent: :py:class:`psyclone.f2pygen.SubroutineGen`

        :raises NotImplementedError: if kernel is inter-grid
        '''
        from psyclone.f2pygen import UseGen

        # We don't yet support inter-grid kernels (Issue #162)
        if kern.is_intergrid:
            raise NotImplementedError(
                "Kernel {0} is an inter-grid kernel and stub generation "
                "is not yet supported for inter-grid kernels".
                format(kern.name))

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
        from psyclone.f2pygen import DeclGen
        self._arglist.append("cell")
        self._parent.add(DeclGen(self._parent, datatype="integer", intent="in",
                                 entity_decls=["cell"]))

    def mesh_height(self):
        ''' add mesh height (nlayers) to the argument list if required '''
        from psyclone.f2pygen import DeclGen
        self._arglist.append("nlayers")
        self._parent.add(DeclGen(self._parent, datatype="integer", intent="in",
                                 entity_decls=["nlayers"]))

    def mesh_ncell2d(self):
        ''' Add the number of columns in the mesh to the argument list if
        required '''
        from psyclone.f2pygen import DeclGen
        self._arglist.append("ncell_2d")
        self._parent.add(DeclGen(self._parent, datatype="integer", intent="in",
                                 entity_decls=["ncell_2d"]))

    def field_vector(self, argvect):
        '''add the field vector associated with the argument 'argvect' to the
        argument list '''
        undf_name = get_fs_undf_name(argvect.function_space)
        from psyclone.f2pygen import DeclGen
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
        from psyclone.f2pygen import DeclGen
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
        from psyclone.f2pygen import DeclGen
        name = arg.name + "_stencil_size"
        self._parent.add(DeclGen(self._parent, datatype="integer", intent="in",
                                 entity_decls=[name]))
        self._arglist.append(name)

    def stencil_unknown_direction(self, arg):
        '''add stencil information associated with the argument 'arg' if the
        direction is unknown'''
        from psyclone.f2pygen import DeclGen
        name = arg.name+"_direction"
        self._parent.add(DeclGen(self._parent, datatype="integer", intent="in",
                                 entity_decls=[name]))
        self._arglist.append(name)

    def stencil(self, arg):
        '''add general stencil information associated with the argument
        'arg' '''
        from psyclone.f2pygen import DeclGen
        name = arg.name+"_stencil_map"
        ndf_name = get_fs_ndf_name(arg.function_space)
        self._parent.add(DeclGen(self._parent, datatype="integer", intent="in",
                                 dimension=",".join(
                                     [ndf_name, arg.name + "_stencil_size"]),
                                 entity_decls=[name]))
        self._arglist.append(name)

    def operator(self, arg):
        ''' add the operator arguments to the argument list '''
        from psyclone.f2pygen import DeclGen
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
        from psyclone.f2pygen import DeclGen
        # The CMA operator itself
        self._arglist.append(arg.name)
        # Associated scalar parameters
        nrow = arg.name + "_nrow"
        _local_args = [nrow]
        if arg.function_space_to.orig_name != \
           arg.function_space_from.orig_name:
            # If the to- and from-spaces are different then so are ncol and
            # nrow so we pass both of them. If they are the same then we
            # could pass either but choose to pass nrow and not ncol.
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
        from psyclone.f2pygen import DeclGen
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
        from psyclone.f2pygen import DeclGen
        if not operator:
            raise GenerationError("Internal error: no CMA operator supplied.")
        if operator.type != "gh_columnwise_operator":
            raise GenerationError(
                "Internal error: a CMA operator (gh_columnwise_operator) must "
                "be supplied but got {0}".format(operator.type))
        # The extent of the (1D) dofmap depends on whether it is for the 'to'
        # or 'from' function space of the operator
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
        from psyclone.f2pygen import DeclGen
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

    def fs_common(self, function_space):
        ''' Provide arguments common to LMA operators and
        fields on a space. There is one: "ndf". '''
        from psyclone.f2pygen import DeclGen
        ndf_name = get_fs_ndf_name(function_space)
        self._arglist.append(ndf_name)
        self._parent.add(
            DeclGen(self._parent, datatype="integer", intent="in",
                    entity_decls=[ndf_name]),
            position=["before", self._first_arg_decl.root])

    def fs_compulsory_field(self, function_space):
        ''' Provide compulsory arguments if there is a field on this
        function space'''
        from psyclone.f2pygen import DeclGen
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
        '''
        Add the necessary declarations for a basis function on the supplied
        function space.

        :param function_space: the function space for which to provide
                               the basis functions
        :type function_space: :py:class:`psyclone.dynamo0p3.FunctionSpace`
        '''
        from psyclone.f2pygen import DeclGen
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
              ["w1", "w2", "w2h", "w2v", "any_w2"]):
            first_dim = "3"
        else:
            raise GenerationError(
                "Unsupported space for basis function, "
                "expecting one of {0} but found "
                "'{1}'".format(VALID_FUNCTION_SPACES,
                               function_space.orig_name))
        if self._kern.eval_shape in VALID_QUADRATURE_SHAPES:
            if self._kern.eval_shape == "gh_quadrature_xyoz":
                dim_list = ",".join([first_dim, ndf_name,
                                     "np_xy", "np_z"])
            else:
                raise GenerationError(
                    "Quadrature shapes other than GH_QUADRATURE_XYoZ are not "
                    "yet supported")
        elif self._kern.eval_shape in VALID_EVALUATOR_SHAPES:
            # Need the ndf for the space on which the basis functions
            # have been evaluated
            nodal_ndf_name = get_fs_ndf_name(self._kern.eval_fspace)
            dim_list = ",".join([first_dim, ndf_name, nodal_ndf_name])
        else:
            raise GenerationError(
                "Internal error: unrecognised evaluator shape ({0}). Expected "
                "one of: {1}".format(self._kern.eval_shape,
                                     VALID_EVALUATOR_SHAPES))
        self._parent.add(DeclGen(self._parent, datatype="real",
                                 kind="r_def", intent="in",
                                 dimension=dim_list,
                                 entity_decls=[basis_name]))

    def diff_basis(self, function_space):
        '''
        Provide the necessary declarations for the differential basis function
        on the supplied function space.

        :param function_space: the function space for which to provide the
                               differential basis function
        :type function_space: :py:class:`psyclone.dynamo0p3.FunctionSpace`
        '''
        from psyclone.f2pygen import DeclGen
        ndf_name = get_fs_ndf_name(function_space)
        diff_basis_name = get_fs_diff_basis_name(function_space)
        self._arglist.append(diff_basis_name)
        # the size of the first dimension for a
        # differential basis array depends on the
        # function space. The values are
        # w0=3, w1=3, w2=1, w3=3, wtheta=3, w2h=1, w2v=1
        first_dim = None
        if function_space.orig_name.lower() in \
           ["w2", "w2h", "w2v", "any_w2"]:
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
        if self._kern.eval_shape in VALID_QUADRATURE_SHAPES:
            # We need differential basis functions for quadrature
            if self._kern.eval_shape == "gh_quadrature_xyoz":
                dim_list = ",".join([first_dim, ndf_name,
                                     "np_xy", "np_z"])
            else:
                raise NotImplementedError(
                    "Internal error: diff-basis for quadrature shape '{0}' "
                    "not yet implemented".format(self._kern.eval_shape))
        elif self._kern.eval_shape in VALID_EVALUATOR_SHAPES:
            # We need differential basis functions for an evaluator
            nodal_ndf_name = get_fs_ndf_name(self._kern.eval_fspace)
            dim_list = ",".join([first_dim, ndf_name, nodal_ndf_name])
        else:
            raise GenerationError(
                "Internal error: unrecognised evaluator shape ({0}). Expected "
                "one of: {1}".format(self._kern.eval_shape,
                                     VALID_EVALUATOR_SHAPES))
        self._parent.add(DeclGen(self._parent, datatype="real", kind="r_def",
                                 intent="in", dimension=dim_list,
                                 entity_decls=[diff_basis_name]))

    def orientation(self, function_space):
        ''' provide orientation information for the function space '''
        from psyclone.f2pygen import DeclGen
        ndf_name = get_fs_ndf_name(function_space)
        orientation_name = get_fs_orientation_name(function_space)
        self._arglist.append(orientation_name)
        self._parent.add(DeclGen(self._parent, datatype="integer",
                                 intent="in", dimension=ndf_name,
                                 entity_decls=[orientation_name]))

    def field_bcs_kernel(self, function_space):
        ''' implement the boundary_dofs array fix for fields '''
        from psyclone.f2pygen import DeclGen
        self._arglist.append("boundary_dofs")
        ndf_name = get_fs_ndf_name(function_space)
        self._parent.add(DeclGen(self._parent, datatype="integer", intent="in",
                                 dimension=",".join([ndf_name, "2"]),
                                 entity_decls=["boundary_dofs"]))

    def operator_bcs_kernel(self, function_space):
        ''' Implement the boundary_dofs array fix for operators. This is the
        same as for fields with the function space set to the 'to' space of
        the operator. '''
        self.field_bcs_kernel(function_space)

    def quad_rule(self):
        ''' provide quadrature information for this kernel stub (necessary
        arguments and declarations) '''
        from psyclone.f2pygen import DeclGen
        self._arglist.extend(self._kern.qr_args)
        self._parent.add(DeclGen(self._parent, datatype="integer", intent="in",
                                 entity_decls=["np_xy", "np_z"]))
        self._parent.add(DeclGen(self._parent, datatype="real", kind="r_def",
                                 intent="in",
                                 dimension="np_xy",
                                 entity_decls=["weights_xy"]))
        self._parent.add(DeclGen(self._parent, datatype="real", kind="r_def",
                                 intent="in",
                                 dimension="np_z",
                                 entity_decls=["weights_z"]))

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
#    def fs_common(self, function_space):
#        '''get dino to output any arguments common to LMA operators and
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
#    def field_bcs_kernel(self, function_space):
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
#        from psyclone.f2pygen import CommentGen
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
#        from psyclone.f2pygen import CallGen
#        self._parent.add(CallGen(self._parent, name="dino%output_scalar",
#                                 args=[name]),
#                         position=["after", self._scalar_position])
#
#    def _add_dino_array(self, name):
#        ''' add a dino output call for an array variable '''
#        from psyclone.f2pygen import CallGen
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

    @property
    def descriptors(self):
        '''
        :return: the list of Descriptors, one for each of the meta-funcs
                 entries in the kernel meta-data.
        :rtype: List of :py:class:`psyclone.dynamo0p3.FSDescriptor`
        '''
        return self._descriptors


def check_args(call):
    '''
    Checks that the kernel arguments provided via the invoke call are
    consistent with the information expected, as specified by the
    kernel metadata

    :param call: the object produced by the parser that describes the
                 kernel call to be checked.
    :type call: :py:class:`psyclone.parse.KernelCall`
    :raises: GenerationError if the kernel arguments in the Algorithm layer
             do not match up with the kernel meta-data
    '''
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
    if call.ktype.eval_shape in VALID_QUADRATURE_SHAPES:
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

    def get_arg_on_space_name(self, func_space_name):
        '''
        Returns the first argument (field or operator) found that is on
        the named function space, as specified in the kernel metadata.

        :param str func_space_name: Name of the function space (as specified
                                    in kernel meta-data) for which
                                    to find an argument.
        :return: the first kernel argument that is on the named function
                 space
        :rtype: :py:class:`psyclone.dynamo0p3.DynKernelArgument`
        :raises: FieldNotFoundError if no field or operator argument is found
                 for the named function space.
        '''
        for arg in self._args:
            for function_space in arg.function_spaces:
                if function_space:
                    if func_space_name == function_space.orig_name:
                        return arg
        raise FieldNotFoundError("DynKernelArguments:get_arg_on_space_name: "
                                 "there is no field or operator with function "
                                 "space {0}".format(func_space_name))

    def get_arg_on_space(self, func_space):
        '''
        Returns the first argument (field or operator) found that is on
        the specified function space. The mangled name of the supplied
        function space is used for comparison.

        :param func_space: The function space for which to find an argument.
        :type func_space: :py:class:`psyclone.dynamo0p3.FunctionSpace`
        :return: the first kernel argument that is on the supplied function
                 space
        :rtype: :py:class:`psyclone.dynamo0p3.DynKernelArgument`
        :raises: FieldNotFoundError if no field or operator argument is found
                 for the specified function space.
        '''
        for arg in self._args:
            for function_space in arg.function_spaces:
                if function_space:
                    if func_space.mangled_name == function_space.mangled_name:
                        return arg
        raise FieldNotFoundError("DynKernelArguments:get_arg_on_space: there "
                                 "is no field or operator with function space "
                                 "{0} (mangled name = '{1}')".format(
                                     func_space.orig_name,
                                     func_space.mangled_name))

    def has_operator(self, op_type=None):
        ''' Returns true if at least one of the arguments is an operator
        of type op_type (either gh_operator [LMA] or gh_columnwise_operator
        [CMA]). If op_type is None then searches for *any* valid operator
        type. '''
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
        '''
        Returns an argument we can use to dereference the iteration
        space. This can be a field or operator that is modified or
        alternatively a field that is read if one or more scalars
        are modified. If a kernel writes to more than one argument then
        that requiring the largest iteration space is selected.

        :param dict mapping: un-used argument. Retained for consistency
                             with base class.
        :return: Kernel argument from which to obtain iteration space
        :rtype: :py:class:`psyclone.dynamo0p3.DynKernelArgument`
        '''

        # Since we always compute operators out to the L1 halo we first
        # check whether this kernel writes to an operator
        op_args = psyGen.args_filter(self._args,
                                     arg_types=VALID_OPERATOR_NAMES,
                                     arg_accesses=GH_WRITE_ACCESSES)
        if op_args:
            return op_args[0]

        # Is this an inter-grid kernel? If so, then the iteration space
        # is determined by the coarse mesh, irrespective of whether
        # we are prolonging (and thus writing to a field on the fine mesh)
        # or restricting.
        if self._parent_call.is_intergrid:
            fld_args = psyGen.args_filter(self._args,
                                          arg_types=["gh_field"],
                                          arg_meshes=["gh_coarse"])
            return fld_args[0]

        # This is not an inter-grid kernel and it does not write to an
        # operator. We now check for fields that are written to. We
        # check first for any modified field on a continuous function
        # space, failing that we try any_space function spaces
        # (because we must assume such a space is continuous) and
        # finally we try discontinuous function spaces. We do this
        # because if a quantity on a continuous FS is modified then
        # our iteration space must be larger (include L1 halo cells)
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
        '''
        :param kernel_args: Object encapsulating all arguments to the
                            kernel call
        :type kernel_args: :py:class:`psyclone.dynamo0p3.DynKernelArguments`
        :param arg_meta_data: Information obtained from the meta-data for
                              this kernel argument
        :type arg_meta_data: :py:class:`psyclone.dynamo0p3.DynArgDescriptor03`
        :param arg_info: Information on how this argument is specified in the
                         Algorithm layer
        :type arg_info: :py:class:`psyclone.parse.Arg`
        :param call: The kernel object with which this argument is associated
        :type call: :py:class:`psyclone.dynamo0p3.DynKern`
        '''
        KernelArgument.__init__(self, arg_meta_data, arg_info, call)
        # Keep a reference to DynKernelArguments object that contains
        # this argument. This permits us to manage name-mangling for
        # any-space function spaces.
        self._kernel_args = kernel_args
        self._vector_size = arg_meta_data.vector_size
        self._type = arg_meta_data.type
        self._stencil = None
        if arg_meta_data.mesh:
            self._mesh = arg_meta_data.mesh.lower()
        else:
            self._mesh = None

        # The list of function-space objects for this argument. Each
        # object can be queried for its original name and for the
        # mangled name (used to make any-space arguments distinct
        # within an invoke). The argument will only have more than
        # one function-space associated with it if it is an operator.
        fs1 = None
        fs2 = None

        if self.is_operator:

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
        '''
        Returns the name used to dereference this type of argument (depends
        on whether it is a field or operator and, if the latter, whether it
        is the to- or from-space that is specified).

        :param function_space: the function space of this argument
        :type function_space: :py:class:`psyclone.dynamo0p3.FunctionSpace`
        :return: the name used to dereference this argument
        :rtype: str
        '''
        if not function_space:
            if self.is_operator:
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
        elif self.is_operator:
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
    def mesh(self):
        '''
        Getter for the mesh associated with this argument
        :return: Mesh associated with argument (GH_FINE or GH_COARSE)
        :rtype: str
        '''
        return self._mesh

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
        '''
        :return: the expected finite element function space for this
                 argument as specified by the kernel argument metadata.
        :rtype: :py:class:`psyclone.dynamo0p3.FunctionSpace`
        '''
        if self._type == "gh_operator":
            # We return the 'from' space for an operator argument
            return self.function_space_from
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
    def evaluator_function_space(self):
        '''
        Returns the function space on which any basis/diff-basis functions
        required for an evaluator are to be calculated. (Kernels requiring an
        evaluator are only permitted to write to a single arg and that
        determines the space on which the basis/diff-basis functions are
        required.) For an operator this is the to-space, otherwise it is
        just the function space.
        :return: the Function Space on which basis/diff basis functions must
                 be evaluated
        :rtype: :py:class:`psyclone.dynamo0p3.FunctionSpace`
        '''
        return self._function_spaces[0]

    @property
    def intent(self):
        '''
        Returns the Fortran intent of this argument.

        :return: the expected Fortran intent for this argument as specified
                 by the kernel argument metadata
        :rtype: str
        '''
        if self.access == "gh_read":
            return "in"
        elif self.access == "gh_write":
            return "out"
        elif self.access == "gh_readwrite":
            return "inout"
        elif self.access in ["gh_inc"] + VALID_REDUCTION_NAMES:
            return "inout"
        else:
            raise GenerationError(
                "Expecting argument access to be one of 'gh_read, gh_write, "
                "gh_inc', 'gh_readwrite' or one of {0}, but found '{1}'".
                format(str(VALID_REDUCTION_NAMES), self.access))

    @property
    def discontinuous(self):
        '''Returns True if this argument is known to be on a discontinuous
        function space, otherwise returns False.'''
        if self.function_space.orig_name in DISCONTINUOUS_FUNCTION_SPACES:
            return True
        elif self.function_space.orig_name in VALID_ANY_SPACE_NAMES:
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

    @property
    def is_operator(self):
        '''
        :return: True if this kernel argument represents an operator,
                 False otherwise.
        :rtype: bool
        '''
        return self._type in VALID_OPERATOR_NAMES


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
