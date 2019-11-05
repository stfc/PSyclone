# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017-2019, Science and Technology Facilities Council.
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

''' This module implements the PSyclone Dynamo 0.3 API by 1)
    specialising the required base classes in parser.py (Descriptor,
    KernelType) and adding a new class (DynFuncDescriptor03) to
    capture function descriptor metadata and 2) specialising the
    required base classes in psyGen.py (PSy, Invokes, Invoke, InvokeSchedule,
    Loop, Kern, Inf, Arguments and Argument). '''

# Imports
from __future__ import print_function, absolute_import
import abc
import os
from collections import OrderedDict, namedtuple
import fparser
from psyclone.parse.kernel import Descriptor, KernelType
from psyclone.parse.utils import ParseError
import psyclone.expression as expr
from psyclone import psyGen
from psyclone.configuration import Config
from psyclone.core.access_type import AccessType
from psyclone.psyGen import PSy, Invokes, Invoke, InvokeSchedule, Loop, \
    Arguments, KernelArgument, NameSpaceFactory, GenerationError, \
    InternalError, FieldNotFoundError, HaloExchange, GlobalSum, \
    FORTRAN_INTENT_NAMES, DataAccess, Literal, Schedule, \
    CodedKern, ACCEnterDataDirective

# --------------------------------------------------------------------------- #
# ========== First section : Parser specialisations and classes ============= #
# --------------------------------------------------------------------------- #
#
# ---------- Function spaces (FS) ------------------------------------------- #
# Discontinuous FS
DISCONTINUOUS_FUNCTION_SPACES = ["w3", "wtheta", "w2v", "w2broken"]
# Continuous FS
# Note, any_w2 is not a space on its own. any_w2 is used as a common term for
# any vector "w2*" function space (w2, w2h, w2v, w2broken) but not w2trace
# (a space of scalar functions). As any_w2 stands for all vector "w2*" spaces
# it needs to a) be treated as continuous and b) have vector basis and scalar
# differential basis dimensions.
CONTINUOUS_FUNCTION_SPACES = ["w0", "w1", "w2", "w2h", "w2trace", "any_w2"]

# Valid FS and FS names
VALID_FUNCTION_SPACES = DISCONTINUOUS_FUNCTION_SPACES + \
    CONTINUOUS_FUNCTION_SPACES

# Valid any_space metadata (general FS, could be continuous or discontinuous)
VALID_ANY_SPACE_NAMES = ["any_space_{0}".format(x+1) for x in range(10)]

# Valid any_discontinuous_space metadata (general FS known to be discontinuous)
VALID_ANY_DISCONTINUOUS_SPACE_NAMES = \
    ["any_discontinuous_space_{0}".format(x+1) for x in range(10)]

# Valid discontinuous FS names (for optimisation purposes)
VALID_DISCONTINUOUS_FUNCTION_SPACE_NAMES = DISCONTINUOUS_FUNCTION_SPACES + \
    VALID_ANY_DISCONTINUOUS_SPACE_NAMES

# FS names consist of all valid names
VALID_FUNCTION_SPACE_NAMES = VALID_FUNCTION_SPACES + \
                             VALID_ANY_SPACE_NAMES + \
                             VALID_ANY_DISCONTINUOUS_SPACE_NAMES

# Lists of function spaces that have
# a) scalar basis functions;
SCALAR_BASIS_SPACE_NAMES = ["w0", "w2trace", "w3", "wtheta"]
# b) vector basis functions;
VECTOR_BASIS_SPACE_NAMES = ["w1", "w2", "w2h", "w2v", "w2broken", "any_w2"]
# c) scalar differential basis functions;
SCALAR_DIFF_BASIS_SPACE_NAMES = ["w2", "w2h", "w2v", "w2broken", "any_w2"]
# d) vector differential basis functions.
VECTOR_DIFF_BASIS_SPACE_NAMES = ["w0", "w1", "w2trace", "w3", "wtheta"]

# ---------- Evaluators ---------------------------------------------------- #
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

# ---------- Datatypes (scalars, fields, operators) ------------------------- #
GH_VALID_SCALAR_NAMES = ["gh_real", "gh_integer"]
GH_VALID_OPERATOR_NAMES = ["gh_operator", "gh_columnwise_operator"]
GH_VALID_ARG_TYPE_NAMES = ["gh_field"] + GH_VALID_OPERATOR_NAMES + \
    GH_VALID_SCALAR_NAMES

# ---------- Stencils ------------------------------------------------------- #
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

# ---------- Mesh types ----------------------------------------------------- #
# These are the valid mesh types that may be specified for a field
# using the mesh_arg=... meta-data element (for inter-grid kernels that
# perform prolongation/restriction).
VALID_MESH_TYPES = ["gh_coarse", "gh_fine"]

# ---------- Loops (bounds, types, names) ----------------------------------- #
# These are loop bound names which identify positions in a fields
# halo. It is useful to group these together as we often need to
# determine whether an access to a field or other object includes
# access to the halo, or not.
HALO_ACCESS_LOOP_BOUNDS = ["cell_halo", "dof_halo", "colour_halo"]

VALID_LOOP_BOUNDS_NAMES = (["start",     # the starting
                                         # index. Currently this is
                                         # always 1
                            "inner",     # a placeholder for when we
                                         # support loop splitting into
                                         # work that does not access
                                         # the halo and work that does.
                                         # This will be used to help
                                         # overlap computation and
                                         # communication
                            "ncolour",   # the number of cells with
                                         # the current colour
                            "ncolours",  # the number of colours in a
                                         # coloured loop
                            "ncells",    # the number of owned cells
                            "ndofs",     # the number of owned dofs
                            "nannexed"]  # the number of owned dofs
                                         # plus the number of annexed
                                         # dofs. As the indices of
                                         # dofs are arranged that
                                         # owned dofs have lower
                                         # indices than annexed dofs,
                                         # having this value as an
                                         # upper bound will compute
                                         # both owned and annexed
                                         # dofs.
                           + HALO_ACCESS_LOOP_BOUNDS)


# Valid Dynamo0.3 loop types. The default is "" which is over cells (in the
# horizontal plane).
VALID_LOOP_TYPES = ["dofs", "colours", "colour", ""]

# ---------- psyGen mappings ------------------------------------------------ #
# Mappings used by non-API-Specific code in psyGen
psyGen.MAPPING_SCALARS = {"iscalar": "gh_integer", "rscalar": "gh_real"}
psyGen.VALID_ARG_TYPE_NAMES = GH_VALID_ARG_TYPE_NAMES

# ---------- Functions ------------------------------------------------------ #


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

    :raises InternalError: if an unrecognised quadrature shape is encountered.
    :raises NotImplementedError: if a quadrature shape other than \
                                 "gh_quadrature_xyoz" is supplied.
    '''
    if basis_fn["shape"] not in VALID_QUADRATURE_SHAPES:
        raise InternalError(
            "Unrecognised shape ({0}) specified in "
            "dynamo0p3.qr_basis_alloc_args(). Should be one of: "
            "{1}".format(basis_fn["shape"], VALID_QUADRATURE_SHAPES))

    # Dimensionality of the basis arrays depends on the
    # type of quadrature...
    # if basis_fn["shape"] == "gh_quadrature_xyz":
    #     alloc_args = [first_dim, get_fs_ndf_name(basis_fn["fspace"]),
    #          "np_xyz"+"_"+basis_fn["qr_var"]]
    if basis_fn["shape"] == "gh_quadrature_xyoz":
        alloc_args = [first_dim, get_fs_ndf_name(basis_fn["fspace"])]
        if basis_fn["qr_var"]:
            alloc_args += ["np_xy"+"_"+basis_fn["qr_var"],
                           "np_z"+"_"+basis_fn["qr_var"]]
        else:
            # We have no name for the QR variable (as will be the case when
            # generating a kernel stub).
            alloc_args += ["np_xy", "np_z"]
    # elif basis_fn["shape"] == "gh_quadrature_xoyoz":
    #     alloc_args = [first_dim, get_fs_ndf_name(basis_fn["fspace"]),
    #                   "np_x"+"_"+basis_fn["qr_var"],
    #                   "np_y"+"_"+basis_fn["qr_var"],
    #                   "np_z"+"_"+basis_fn["qr_var"]]
    else:
        raise NotImplementedError(
            "Quadrature shape '{0}' not yet supported in "
            "dynamo0p3.qr_basis_alloc_args().".format(basis_fn["shape"]))
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
    if fs_name not in VALID_ANY_SPACE_NAMES + \
       VALID_ANY_DISCONTINUOUS_SPACE_NAMES:
        # If the supplied function-space name is not any any_space
        # or any_discontinuous_space then we don't need to mangle the name
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

# ---------- Classes -------------------------------------------------------- #


class FunctionSpace(object):
    ''' Manages the name of a function space. If it is an any-space
    then its name is mangled such that it is unique within the scope
    of an Invoke '''

    def __init__(self, name, kernel_args):
        self._orig_name = name
        self._kernel_args = kernel_args
        if self._orig_name not in VALID_ANY_SPACE_NAMES + \
           VALID_ANY_DISCONTINUOUS_SPACE_NAMES:
            # We only need to name-mangle any_space and
            # any_discontinuous_space spaces
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
        :type arg_type: :py:class:`psyclone.expression.FunctionVar`
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
            if self._type not in GH_VALID_ARG_TYPE_NAMES:
                raise ParseError(
                    "In the dynamo0.3 API the 1st argument of a meta_arg "
                    "entry should be a valid argument type (one of {0}), but "
                    "found '{1}' in '{2}'".format(GH_VALID_ARG_TYPE_NAMES,
                                                  self._type, arg_type))
            if self._type in GH_VALID_SCALAR_NAMES and self._vector_size > 1:
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
            if arg_type.args[0].name not in GH_VALID_ARG_TYPE_NAMES:
                raise ParseError(
                    "In the dynamo0.3 API the 1st argument of a "
                    "meta_arg entry should be a valid argument type (one of "
                    "{0}), but found '{1}' in '{2}'".
                    format(GH_VALID_ARG_TYPE_NAMES, arg_type.args[0].name,
                           arg_type))
            self._type = arg_type.args[0].name
        else:
            raise ParseError(
                "Internal error in DynArgDescriptor03.__init__, (1) should "
                "not get to here")

        # The 2nd arg is an access descriptor
        # Convert from GH_* names to the generic access type:
        api_config = Config.get().api_conf("dynamo0.3")
        access_mapping = api_config.get_access_mapping()
        try:
            self._access_type = access_mapping[arg_type.args[1].name]
        except KeyError:
            valid_names = api_config.get_valid_accesses_api()
            raise ParseError(
                "In the dynamo0.3 API the 2nd argument of a meta_arg entry "
                "must be a valid access descriptor (one of {0}), but found "
                "'{1}' in '{2}'".format(valid_names,
                                        arg_type.args[1].name, arg_type))

        # Reduction access descriptors are only valid for real scalar arguments
        if self._type != "gh_real" and \
           self._access_type in \
           AccessType.get_valid_reduction_modes():
            raise ParseError(
                "In the dynamo0.3 API a reduction access '{0}' is only valid "
                "with a real scalar argument, but '{1}' was found".
                format(self._access_type.api_specific_name(),
                       self._type))

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
                    from psyclone.parse.kernel import get_stencil, get_mesh
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
            if self._function_space1.lower() in \
               VALID_DISCONTINUOUS_FUNCTION_SPACE_NAMES \
               and self._access_type == AccessType.INC:
                raise ParseError(
                    "It does not make sense for a field on a discontinuous "
                    "space ({0}) to have a 'gh_inc' access".
                    format(self._function_space1.lower()))
            if self._function_space1.lower() in CONTINUOUS_FUNCTION_SPACES \
               and self._access_type == AccessType.READWRITE:
                raise ParseError(
                    "It does not make sense for a field on a continuous "
                    "space ({0}) to have a 'gh_readwrite' access".
                    format(self._function_space1.lower()))
            # TODO: extend restriction to "gh_write" for kernels that loop
            # over cells (issue #138) and update access rules for kernels
            # (built-ins) that loop over DoFs to accesses for discontinuous
            # quantities (issue #471)
            if self._function_space1.lower() in VALID_ANY_SPACE_NAMES \
               and self._access_type == AccessType.READWRITE:
                raise ParseError(
                    "In the Dynamo0.3 API a field on any_space cannot "
                    "have 'gh_readwrite' access because it is treated "
                    "as continuous")
            if stencil and self._access_type != AccessType.READ:
                raise ParseError("a stencil must be read only so its access "
                                 "should be gh_read")

        # Operators
        elif self._type in GH_VALID_OPERATOR_NAMES:
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
            if self._access_type == AccessType.INC:
                raise ParseError(
                    "In the dynamo0.3 API operators cannot have a 'gh_inc' "
                    "access because they behave as discontinuous quantities")

        # Scalars
        elif self._type in GH_VALID_SCALAR_NAMES:
            if len(arg_type.args) != 2:
                raise ParseError(
                    "In the dynamo0.3 API each meta_arg entry must have 2 "
                    "arguments if its first argument is gh_{{r,i}}scalar, but "
                    "found {0} in '{1}'".format(len(arg_type.args), arg_type))
            # Test allowed accesses for scalars (read_only or reduction)
            if self._access_type not in [AccessType.READ] + \
               AccessType.get_valid_reduction_modes():
                rev_access_mapping = api_config.get_reverse_access_mapping()
                api_specific_name = rev_access_mapping[self._access_type]
                valid_reductions = AccessType.get_valid_reduction_names()
                raise ParseError(
                    "In the dynamo0.3 API scalar arguments must be "
                    "read-only (gh_read) or a reduction ({0}) but found "
                    "'{1}' in '{2}'".format(valid_reductions,
                                            api_specific_name,
                                            arg_type))
            # Scalars don't have a function space
            self._function_space1 = None
            self._vector_size = 0

        # We should never get to here
        else:
            raise ParseError(
                "Internal error in DynArgDescriptor03.__init__, (2) should "
                "not get to here")

        Descriptor.__init__(self, self._access_type,
                            self._function_space1, stencil=stencil,
                            mesh=mesh)

    @property
    def function_space_to(self):
        ''' Return the "to" function space for a gh_operator. This is
        the first function space specified in the metadata. Raise an
        error if this is not an operator. '''
        if self._type in GH_VALID_OPERATOR_NAMES:
            return self._function_space1
        raise RuntimeError(
            "function_space_to only makes sense for one of {0}, but "
            "this is a '{1}'".format(GH_VALID_OPERATOR_NAMES, self._type))

    @property
    def function_space_from(self):
        ''' Return the "from" function space for a gh_operator. This is
        the second function space specified in the metadata. Raise an
        error if this is not an operator. '''
        if self._type in GH_VALID_OPERATOR_NAMES:
            return self._function_space2
        raise RuntimeError(
            "function_space_from only makes sense for one of {0}, but this"
            " is a '{1}'".format(GH_VALID_OPERATOR_NAMES, self._type))

    @property
    def function_space(self):
        ''' Return the function space name that this instance operates
        on. In the case of a gh_operator/gh_columnwise_operator, where there
        are 2 function spaces, return function_space_from. '''
        if self._type == "gh_field":
            return self._function_space1
        if self._type in GH_VALID_OPERATOR_NAMES:
            return self._function_space2
        if self._type in GH_VALID_SCALAR_NAMES:
            return None
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
        if self._type in GH_VALID_OPERATOR_NAMES:
            # return to before from to maintain expected ordering
            return [self.function_space_to, self.function_space_from]
        if self._type in GH_VALID_SCALAR_NAMES:
            return []
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
        res += "  access_descriptor[1]='{0}'"\
               .format(self._access_type.api_specific_name())\
               + os.linesep
        if self._type == "gh_field":
            res += "  function_space[2]='{0}'".format(self._function_space1) \
                   + os.linesep
        elif self._type in GH_VALID_OPERATOR_NAMES:
            res += "  function_space_to[2]='{0}'".\
                   format(self._function_space1) + os.linesep
            res += "  function_space_from[3]='{0}'".\
                   format(self._function_space2) + os.linesep
        elif self._type in GH_VALID_SCALAR_NAMES:
            pass  # we have nothing to add if we're a scalar
        else:  # we should never get to here
            raise ParseError("Internal error in DynArgDescriptor03.__str__")
        return res


class DynKernMetadata(KernelType):
    ''' Captures the Kernel subroutine code and metadata describing
    the subroutine for the Dynamo 0.3 API.

    :param ast: fparser1 AST for the kernel.
    :type ast: :py:class:`fparser.block_statements.BeginSource`
    :param str name: The name of this kernel.

    :raises ParseError: if the meta-data does not conform to the \
                        rules for the Dynamo 0.3 API.
    '''
    def __init__(self, ast, name=None):
        KernelType.__init__(self, ast, name=name)

        # The type of CMA operation this kernel performs (or None if
        # no CMA operators are involved)
        self._cma_operation = None

        # Query the meta-data for the evaluator shape (only required if
        # kernel uses quadrature or an evaluator). If it is not
        # present then eval_shape will be None.
        self._eval_shape = self.get_integer_variable('gh_shape')
        # The list of function space names for which an evaluator is
        # required. We set this up below once we've processed the meta-
        # -data describing the kernel arguments.
        self._eval_targets = []

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

        # Check to see whether the optional 'gh_evaluator_targets'
        # has been supplied. This lists the function spaces for which
        # any evaluators (gh_shape=gh_evaluator) should be provided.
        _targets = self.get_integer_array('gh_evaluator_targets')
        if not _targets and \
           self._eval_shape and self._eval_shape.lower() == "gh_evaluator":
            # Use the FS of the kernel arguments that are updated
            write_accesses = AccessType.all_write_accesses()
            write_args = psyGen.args_filter(self._arg_descriptors,
                                            arg_accesses=write_accesses)
            # We want the 'to' space of any operator arguments so get
            # the first FS associated with the kernel argument.
            _targets = [arg.function_spaces[0] for arg in write_args]
        # Ensure that _eval_targets entries are not duplicated
        for target in _targets:
            if target not in self._eval_targets:
                self._eval_targets.append(target)

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
            if arg.access != AccessType.READ:
                write_count += 1
                # We must not write to scalar arguments if it's not a
                # built-in
                if self.name not in BUILTIN_MAP and \
                   arg.type in GH_VALID_SCALAR_NAMES:
                    raise ParseError(
                        "A user-supplied Dynamo 0.3 kernel must not "
                        "write/update a scalar argument but kernel {0} has "
                        "{1} with {2} access"
                        .format(self.name,
                                arg.type,
                                arg.access.api_specific_name()))
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
        # Check that gh_evaluator_targets is only present if required
        if self._eval_targets:
            if not need_evaluator:
                raise ParseError(
                    "Kernel '{0}' specifies gh_evaluator_targets ({1}) but "
                    "does not need an evaluator because no basis or "
                    "differential basis functions are required".
                    format(self.name, self._eval_targets))
            if self._eval_shape != "gh_evaluator":
                raise ParseError(
                    "Kernel '{0}' specifies gh_evaluator_targets ({1}) but "
                    "does not need an evaluator because gh_shape={2}".
                    format(self.name, self._eval_targets, self._eval_shape))
            # Check that there is a kernel argument on each of the
            # specified spaces...
            # Create a list (set) of the function spaces associated with
            # the kernel arguments
            fs_list = set()
            for arg in self._arg_descriptors:
                fs_list.update(arg.function_spaces)
            # Check each evaluator_target against this list
            for eval_fs in self._eval_targets:
                if eval_fs not in fs_list:
                    raise ParseError(
                        "Kernel '{0}' specifies that an evaluator is required "
                        "on {1} but does not have an argument on this space."
                        .format(self.name, eval_fs))
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
        mesh_dict = OrderedDict()
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
                    VALID_MESH_TYPES, self.name,
                    [str(name) for name in mesh_list]))
        # Inter-grid kernels must only have field arguments
        if non_field_arg_types:
            raise ParseError(
                "Inter-grid kernels in the Dynamo 0.3 API are only "
                "permitted to have field arguments but kernel {0} also "
                "has arguments of type {1}".format(
                    self.name, [str(name) for name in non_field_arg_types]))
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
                format(self.name,
                       [str(name) for name in fs_common],
                       [str(name) for name in mesh_list]))
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
            if cop.access in AccessType.all_write_accesses():
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
                                           arg_accesses=[AccessType.READ])
            write_accesses = AccessType.all_write_accesses()
            farg_write = psyGen.args_filter(self._arg_descriptors,
                                            arg_types=["gh_field"],
                                            arg_accesses=write_accesses)
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
            write_accesses = AccessType.all_write_accesses()
            write_args = psyGen.args_filter(self._arg_descriptors,
                                            arg_accesses=write_accesses)
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
                                      [str(arg.type) for arg in write_args]))
            if len(cwise_ops) == 1:

                # If this is a valid assembly kernel then we need at least one
                # read-only LMA operator
                lma_read_ops = psyGen.args_filter(
                    self._arg_descriptors,
                    arg_types=["gh_operator"],
                    arg_accesses=[AccessType.READ])
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
                    self._arg_descriptors, arg_types=GH_VALID_SCALAR_NAMES)
                if (len(scalar_args) + len(cwise_ops)) != \
                   len(self._arg_descriptors):
                    raise ParseError(
                        "A column-wise matrix-matrix kernel must have only "
                        "column-wise operators and scalars as arguments but "
                        "kernel {0} has: {1}.".
                        format(self.name,
                               [str(arg.type) for arg in
                                self._arg_descriptors]))
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
        return ""

    @property
    def eval_targets(self):
        '''
        Returns the list of function spaces upon which any evaluator must be
        provided. This list is obtained from the GH_EVALUATOR_TARGETS meta-data
        entry (if present). If this is not specified in the meta-data then
        we default to providing evaluators on all of the function spaces
        associated with the arguments which this kernel updates.

        :return: list of the names of the function spaces (as they appear in \
                 kernel metadata) upon which any evaluator must be provided.
        :rtype: list of str
        '''
        return self._eval_targets

    @property
    def is_intergrid(self):
        '''
        Returns whether or not this is an inter-grid kernel.

        :return: True if kernel is an inter-grid kernel, False otherwise
        :rtype: bool
        '''
        return self._is_intergrid

# --------------------------------------------------------------------------- #
# ========== Second section : PSy specialisations =========================== #
# --------------------------------------------------------------------------- #

# ---------- Classes -------------------------------------------------------- #


class DynamoPSy(PSy):
    ''' The Dynamo specific PSy class. This creates a Dynamo specific
    invokes object (which controls all the required invocation calls).
    It also overrides the PSy gen method so that we generate dynamo
    specific PSy module code.

    :param invoke_info: object containing the required invocation information \
                        for code optimisation and generation.
    :type invoke_info: :py:class:`psyclone.parse.algorithm.FileInfo`
    '''
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


class DynCollection(object):
    '''
    Base class for managing the declaration and initialisation of a
    group of related entities within an Invoke or Kernel stub

    :param node: the Kernel or Invoke for which to manage variable \
                 declarations and initialisation.
    :type node: :py:class:`psyclone.dynamo0p3.DynInvoke` or \
                :py:class:`psyclone.dynamo0p3.DynKern`

    :raises InternalError: if the supplied node is not a DynInvoke or a \
                           DynKern.
    '''
    def __init__(self, node):
        if isinstance(node, DynInvoke):
            # We are handling declarations/initialisations for an Invoke
            self._invoke = node
            self._kernel = None
            # The list of kernel calls we are responsible for
            self._calls = node.schedule.kernels()
        elif isinstance(node, DynKern):
            # We are handling declarations for a Kernel stub
            self._invoke = None
            self._kernel = node
            # We only have a single kernel call in this case
            self._calls = [node]
        else:
            raise InternalError("DynCollection takes only a DynInvoke "
                                "or a DynKern but got: {0}".format(
                                    type(node)))

        # Whether or not the associated Invoke contains only kernels that
        # iterate over dofs.
        if self._invoke:
            self._dofs_only = self._invoke.iterate_over_dofs_only
        else:
            self._dofs_only = False

        self._name_space_manager = NameSpaceFactory().create()

    def declarations(self, parent):
        '''
        Insert declarations for all necessary variables into the AST of
        the generated code. Simply calls either _invoke_declarations() or
        _stub_declarations() depending on whether we're handling an Invoke
        or a Kernel stub.

        :param parent: the node in the f2pygen AST representing the routine \
                       in which to insert the declarations.
        :type parent: :py:class:`psyclone.f2pygen.SubroutineGen`

        :raises InternalError: if neither self._invoke or self._kernel \
                               are set.
        '''
        if self._invoke:
            self._invoke_declarations(parent)
        elif self._kernel:
            self._stub_declarations(parent)
        else:
            raise InternalError("DynCollection has neither a Kernel "
                                "or an Invoke - should be impossible.")

    def initialise(self, parent):
        '''
        Add code to initialise the entities being managed by this class.
        We do nothing by default - it is up to the sub-class to override
        this method if initialisation is required.

        :param parent: the node in the f2pygen AST to which to add \
                       initialisation code.
        :type parent: :py:class:`psyclone.f2pygen.SubroutineGen`
        '''

    @abc.abstractmethod
    def _invoke_declarations(self, parent):
        '''
        Add all necessary declarations for an Invoke.

        :param parent: node in the f2pygen AST representing the Invoke to \
                       which to add declarations.
        :type parent: :py:class:`psyclone.f2pygen.SubroutineGen`

        '''

    def _stub_declarations(self, parent):
        '''
        Add all necessary declarations for a Kernel stub. Not abstract because
        not all entities need representing within a Kernel.

        :param parent: node in the f2pygen AST representing the Kernel stub \
                       to which to add declarations.
        :type parent: :py:class:`psyclone.f2pygen.SubroutineGen`

        '''


class DynStencils(DynCollection):
    '''
    Stencil information and code generation associated with a PSy-layer
    routine or Kernel stub.

    :param node: the Invoke or Kernel stub for which to provide stencil info.
    :type node: :py:class:`psyclone.dynamo0p3.DynInvoke` or \
                :py:class:`psyclone.dynamo0p3.DynKern`

    :raises GenerationError: if a literal has been supplied for a stencil \
                             direction.
    '''
    def __init__(self, node):
        super(DynStencils, self).__init__(node)

        # List of arguments which have an extent value passed to this
        # invoke routine from the algorithm layer. Duplicate argument
        # names are removed.
        self._unique_extent_args = []
        extent_names = []
        for call in self._calls:
            for arg in call.arguments.args:
                if arg.stencil:
                    # Check for the existence of arg.extent here as in
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

        # A list of arguments that have a direction variable passed in
        # to this invoke routine from the algorithm layer. Duplicate
        # argument names are removed.
        self._unique_direction_args = []
        direction_names = []
        for call in self._calls:
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
        for call in self._calls:
            for arg in call.arguments.args:
                if arg.stencil:
                    if not arg.stencil.extent:
                        self._kern_args.append(arg)

    @staticmethod
    def extent_value(arg):
        '''
        Returns the content of the stencil extent which may be a literal
        value (a number) or a variable name. This function simplifies this
        problem by returning a string in either case.

        :param arg: the argument with which the stencil is associated.
        :type arg: :py:class:`psyclone.dynamo0p3.DynKernelArgument`

        :returns: the content of the stencil extent.
        :rtype: str

        '''
        if arg.stencil.extent_arg.is_literal():
            return arg.stencil.extent_arg.text
        return arg.stencil.extent_arg.varname

    @staticmethod
    def stencil_unique_str(arg, context):
        '''
        Creates a unique identifier for a stencil. As a stencil
        differs due to the function space it operates on, type of
        stencil and extent of stencil, we concatenate these things together
        to create a unique string.

        :param arg: kernel argument with which stencil is associated.
        :type arg: :py:class:`psyclone.dynamo0p3.DynKernelArgument`
        :param str context: a context for this stencil (e.g. "size" or \
                            "direction").

        :returns: unique string identifying the stencil for this argument.
        :rtype: str

        :raises GenerationError: if an explicit stencil extent is found in \
                                 the meta-data for the kernel argument.
        '''
        unique = context
        unique += arg.function_space.mangled_name
        unique += arg.descriptor.stencil['type']
        if arg.descriptor.stencil['extent']:
            raise GenerationError(
                "Found a stencil with an extent specified in the metadata. "
                "This is not coded for.")
        unique += arg.stencil.extent_arg.text.lower()
        if arg.descriptor.stencil['type'] == 'xory1d':
            unique += arg.stencil.direction_arg.text.lower()
        return unique

    def map_name(self, arg):
        '''
        Creates and registers a name for the stencil map associated with the
        supplied kernel argument.

        :param arg: kernel argument with which the stencil is associated.
        :type arg: :py:class:`psyclone.dynamo0p3.DynKernelArgument`

        :returns: a valid unique map name for a stencil in the PSy layer.
        :rtype: str
        '''
        root_name = arg.name + "_stencil_map"
        unique = DynStencils.stencil_unique_str(arg, "map")
        return self._name_space_manager.create_name(
            root_name=root_name, context="PSyVars", label=unique)

    @staticmethod
    def dofmap_name(arg):
        '''
        Creates and registers a name for the stencil dofmap associated with
        the supplied kernel argument.

        :param arg: kernel argument with which the stencil is associated.
        :type arg: :py:class:`psyclone.dynamo0p3.DynKernelArgument`

        :returns: a valid unique dofmap name for a stencil in the PSy layer.
        :rtype: str
        '''
        name_space_manager = NameSpaceFactory().create()
        root_name = arg.name + "_stencil_dofmap"
        unique = DynStencils.stencil_unique_str(arg, "dofmap")
        return name_space_manager.create_name(
            root_name=root_name, context="PSyVars", label=unique)

    @staticmethod
    def dofmap_size_name(arg):
        '''
        Create a valid unique name for the size (in cells) of a stencil
        dofmap in the PSy layer.

        :param arg: the kernel argument with which the stencil is associated.
        :type arg: :py:class:`psyclone.dynamo0p3.DynKernelArgument`

        :returns: a Fortran variable name for the stencil size.
        :rtype: str
        '''
        root_name = arg.name + "_stencil_size"
        unique = DynStencils.stencil_unique_str(arg, "size")
        name_space_manager = NameSpaceFactory().create()
        return name_space_manager.create_name(
            root_name=root_name, context="PSyVars", label=unique)

    @staticmethod
    def direction_name(arg):
        '''
        Creates a Fortran variable name to hold the direction of the stencil
        associated with the supplied kernel argument.

        :param arg: the kernel argument with which the stencil is associated.
        :type arg: :py:class:`psyclone.dynamo0p3.DynKernelArgument`

        :returns: a Fortran variable name for the stencil direction.
        :rtype: str
        '''
        name_space_manager = NameSpaceFactory().create()
        root_name = arg.name+"_direction"
        unique = DynStencils.stencil_unique_str(arg, "direction")
        return name_space_manager.create_name(
            root_name=root_name, context="PSyVars", label=unique)

    @property
    def _unique_extent_vars(self):
        '''
        :returns: list of all the unique extent argument names in this \
                  invoke or kernel call.
        :rtype: list of str

        :raises InternalError: if neither self._kernel or self._invoke are set.

        '''
        if self._invoke:
            names = [arg.stencil.extent_arg.varname for arg in
                     self._unique_extent_args]
        elif self._kernel:
            # A kernel is passed the size of the stencil map
            names = [self.dofmap_size_name(arg) for arg in
                     self._unique_extent_args]
        else:
            raise InternalError("_unique_extent_vars: have neither Invoke "
                                "or Kernel. Should be impossible.")
        return names

    def _declare_unique_extent_vars(self, parent):
        '''
        Declare all unique extent arguments as integers with intent in and
        add the declaration as a child of the parent argument passed
        in.

        :param parent: the node in the f2pygen AST to which to add the \
                       declarations.
        :type parent: :py:class:`psyclone.f2pygen.SubroutineGen`

        '''
        from psyclone.f2pygen import DeclGen
        if self._unique_extent_vars:
            parent.add(DeclGen(parent, datatype="integer",
                               entity_decls=self._unique_extent_vars,
                               intent="in"))

    @property
    def _unique_direction_vars(self):
        '''
        :returns: a list of all the unique direction argument names in this \
                  invoke call.
        :rtype: list of str
        '''
        names = []
        for arg in self._unique_direction_args:
            if arg.stencil.direction_arg.varname:
                names.append(arg.stencil.direction_arg.varname)
            else:
                names.append(arg.name+"_direction")
        return names

    def _declare_unique_direction_vars(self, parent):
        '''
        Declare all unique direction arguments as integers with intent in
        and add the declaration as a child of the parent argument
        passed in.

        :param parent: the node in the f2pygen AST to which to add the \
                       declarations.
        :type parent: :py:class:`psyclone.f2pygen.SubroutineGen`

        '''
        from psyclone.f2pygen import DeclGen
        if self._unique_direction_vars:
            parent.add(DeclGen(parent, datatype="integer",
                               entity_decls=self._unique_direction_vars,
                               intent="in"))

    @property
    def unique_alg_vars(self):
        '''
        :returns: list of the names of the extent and direction arguments \
                  supplied to the PSy routine from the Algorithm layer.
        :rtype: list of str
        '''
        return self._unique_extent_vars + self._unique_direction_vars

    def _invoke_declarations(self, parent):
        '''
        Declares all stencil maps, extent and direction arguments passed into
        the PSy layer.

        :param parent: node in the f2pygen AST to which to add declarations.
        :type parent: :py:class:`psyclone.f2pygen.SubroutineGen`

        '''
        self._declare_unique_extent_vars(parent)
        self._declare_unique_direction_vars(parent)
        self._declare_maps_invoke(parent)

    def _stub_declarations(self, parent):
        '''
        Declare all stencil-related quanitites for a Kernel stub.

        :param parent: node in the f2pygen AST to which to add declarations.
        :type parent: :py:class:`psyclone.f2pygen.SubroutineGen`

        '''
        self._declare_unique_extent_vars(parent)
        self._declare_unique_direction_vars(parent)
        self._declare_maps_stub(parent)

    def initialise(self, parent):
        '''
        Adds in the code to initialise stencil dofmaps to the PSy layer.

        :param parent: the node in the f2pygen AST to which to add the \
                       initialisations.
        :type parent: :py:class:`psyclone.f2pygen.SubroutineGen`

        :raises GenerationError: if an unsupported stencil type is encountered.
        '''
        from psyclone.f2pygen import AssignGen, IfThenGen, CommentGen
        if not self._kern_args:
            return

        parent.add(CommentGen(parent, ""))
        parent.add(CommentGen(parent, " Initialise stencil dofmaps"))
        parent.add(CommentGen(parent, ""))
        stencil_map_names = []
        for arg in self._kern_args:
            map_name = self.map_name(arg)
            if map_name not in stencil_map_names:
                # Only initialise maps once.
                stencil_map_names.append(map_name)
                stencil_type = arg.descriptor.stencil['type']
                if stencil_type == "xory1d":
                    direction_name = arg.stencil.direction_arg.varname
                    for direction in ["x", "y"]:
                        if_then = IfThenGen(parent, direction_name +
                                            " .eq. " + direction +
                                            "_direction")
                        if_then.add(
                            AssignGen(
                                if_then, pointer=True, lhs=map_name,
                                rhs=arg.proxy_name_indexed +
                                "%vspace%get_stencil_dofmap("
                                "STENCIL_1D" + direction.upper() +
                                ","+self.extent_value(arg)+")"))
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
                    parent.add(
                        AssignGen(parent, pointer=True, lhs=map_name,
                                  rhs=arg.proxy_name_indexed +
                                  "%vspace%get_stencil_dofmap(" +
                                  stencil_name + "," +
                                  self.extent_value(arg) + ")"))

                parent.add(AssignGen(parent, pointer=True,
                                     lhs=self.dofmap_name(arg),
                                     rhs=map_name + "%get_whole_dofmap()"))

                # Add declaration and look-up of stencil size
                parent.add(AssignGen(parent,
                                     lhs=self.dofmap_size_name(arg),
                                     rhs=map_name + "%get_size()"))

    def _declare_maps_invoke(self, parent):
        '''
        Declare all stencil maps in the PSy layer.

        :param parent: the node in the f2pygen AST to which to add \
                       declarations.
        :type parent: :py:class:`psyclone.f2pygen.SubroutineGen`

        :raises GenerationError: if an unsupported stencil type is encountered.
        '''
        from psyclone.f2pygen import TypeDeclGen, DeclGen, UseGen
        if not self._kern_args:
            return

        parent.add(UseGen(parent, name="stencil_dofmap_mod", only=True,
                          funcnames=["stencil_dofmap_type"]))

        stencil_map_names = []
        for arg in self._kern_args:
            map_name = self.map_name(arg)

            if map_name in stencil_map_names:
                continue

            stencil_map_names.append(map_name)

            parent.add(TypeDeclGen(parent, pointer=True,
                                   datatype="stencil_dofmap_type",
                                   entity_decls=[map_name+" => null()"]))
            parent.add(DeclGen(parent, datatype="integer", pointer=True,
                               entity_decls=[self.dofmap_name(arg) +
                                             "(:,:,:) => null()"]))
            parent.add(DeclGen(parent, datatype="integer",
                               entity_decls=[self.dofmap_size_name(arg)]))

            stencil_type = arg.descriptor.stencil['type']
            if stencil_type == "xory1d":
                parent.add(UseGen(parent, name="flux_direction_mod",
                                  only=True, funcnames=["x_direction",
                                                        "y_direction"]))
                parent.add(UseGen(parent, name="stencil_dofmap_mod",
                                  only=True, funcnames=["STENCIL_1DX",
                                                        "STENCIL_1DY"]))
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
                                  only=True, funcnames=[stencil_name]))
                parent.add(DeclGen(parent, datatype="integer", pointer=True,
                                   entity_decls=[self.dofmap_name(arg) +
                                                 "(:,:,:) => null()"]))

    def _declare_maps_stub(self, parent):
        '''
        Add declarations for all stencil maps to a Kernel stub.

        :param parent: the node in the f2pygen AST representing the Kernel \
                       stub routine.
        :type parent: :py:class:`psyclone.f2pygen.SubroutineGen`

        '''
        from psyclone.f2pygen import DeclGen

        for arg in self._kern_args:
            parent.add(DeclGen(
                parent, datatype="integer", intent="in",
                dimension=",".join([get_fs_ndf_name(arg.function_space),
                                    self.dofmap_size_name(arg)]),
                entity_decls=[self.dofmap_name(arg)]))


class DynDofmaps(DynCollection):
    '''
    Holds all information on the dofmaps (including column-banded and
    indirection) required by an invoke.

    :param node: Kernel or Invoke for which to manage dofmaps.
    :type node: :py:class:`psyclone.dynamo0p3.DynKern` or \
                :py:class:`psyclone.dynamo0p3.DynInvoke`

    '''
    def __init__(self, node):
        super(DynDofmaps, self).__init__(node)

        # Look at every kernel call in this invoke and generate a list
        # of the unique function spaces involved.
        # We create a dictionary whose keys are the map names and entries
        # are the corresponding field objects.
        self._unique_fs_maps = OrderedDict()
        # We also create a dictionary of column-banded dofmaps. Entries
        # in this one are themselves dictionaries containing two entries:
        # "argument" - the object holding information on the CMA kernel
        #              argument
        # "direction" - whether the dofmap is required for the "to" or
        #               "from" function space of the operator.
        self._unique_cbanded_maps = OrderedDict()
        # A dictionary of required CMA indirection dofmaps. As with the
        # column-banded dofmaps, each entry is itself a dictionary with
        # "argument" and "direction" entries.
        self._unique_indirection_maps = OrderedDict()

        for call in self._calls:
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

    def initialise(self, parent):
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

    def _invoke_declarations(self, parent):
        '''
        Declare all unique function space dofmaps in the PSy layer as pointers
        to integer arrays of rank 2.

        :param parent: the f2pygen node to which to add the declarations.
        :type parent: :py:class:`psyclone.f2pygen.SubroutineGen`

        '''
        from psyclone.f2pygen import DeclGen

        # Function space dofmaps
        decl_map_names = \
            [dmap+"(:,:) => null()" for dmap in sorted(self._unique_fs_maps)]

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

    def _stub_declarations(self, parent):
        '''
        Add dofmap-related declarations to a Kernel stub.

        :param parent: node in the f2pygen AST representing the Kernel stub.
        :type parent: :py:class:`psyclone.f2pygen.SubroutineGen`

        '''
        from psyclone.f2pygen import DeclGen
        # Function space dofmaps
        for dmap in sorted(self._unique_fs_maps):
            # We declare ndf first as some compilers require this
            ndf_name = get_fs_ndf_name(
                self._unique_fs_maps[dmap].function_space)
            parent.add(DeclGen(parent, datatype="integer", intent="in",
                               entity_decls=[ndf_name]))
            parent.add(DeclGen(parent, datatype="integer", intent="in",
                               dimension=ndf_name, entity_decls=[dmap]))
        # Column-banded dofmaps
        for dmap, cma in self._unique_cbanded_maps.items():
            if cma["direction"] == "to":
                ndf_name = get_fs_ndf_name(cma["argument"].function_space_to)
            elif cma["direction"] == "from":
                ndf_name = get_fs_ndf_name(cma["argument"].function_space_from)
            else:
                raise InternalError(
                    "Invalid direction ('{0}') found for CMA operator when "
                    "collecting column-banded dofmaps. Should "
                    "be either 'to' or 'from'.".format(cma["direction"]))
            parent.add(DeclGen(parent, datatype="integer", intent="in",
                               entity_decls=[ndf_name]))
            parent.add(DeclGen(parent, datatype="integer", intent="in",
                               dimension=",".join([ndf_name, "nlayers"]),
                               entity_decls=[dmap]))
        # CMA operator indirection dofmaps
        for dmap, cma in self._unique_indirection_maps.items():
            if cma["direction"] == "to":
                dim_name = cma["argument"].name + "_nrow"
            elif cma["direction"] == "from":
                dim_name = cma["argument"].name + "_ncol"
            else:
                raise InternalError(
                    "Invalid direction ('{0}') found for CMA operator when "
                    "collecting indirection dofmaps. Should "
                    "be either 'to' or 'from'.".format(cma["direction"]))
            parent.add(DeclGen(parent, datatype="integer", intent="in",
                               entity_decls=[dim_name]))
            parent.add(DeclGen(parent, datatype="integer", intent="in",
                               dimension=dim_name, entity_decls=[dmap]))


class DynOrientations(DynCollection):
    '''
    Handle the declaration of any orientation arrays. Orientation arrays
    are initialised on a per-cell basis (within the loop over cells) and
    this is therefore handled by the kernel-call generation.

    '''
    # We use a named-tuple to manage the storage of the various quantities
    # that we require. This is neater and more robust than a dict.
    Orientation = namedtuple("Orientation", ["name", "field",
                                             "function_space"])

    def __init__(self, node):
        super(DynOrientations, self).__init__(node)

        self._orients = []

        # Loop over each kernel call and check whether orientation is required.
        # If it is then we create an Orientation object for it and store in
        # our internal list.
        for call in self._calls:
            for unique_fs in call.arguments.unique_fss:
                if call.fs_descriptors.exists(unique_fs):
                    fs_descriptor = call.fs_descriptors.get_descriptor(
                        unique_fs)
                    if fs_descriptor.requires_orientation:
                        field = call.arguments.get_arg_on_space(unique_fs)
                        oname = get_fs_orientation_name(unique_fs)
                        self._orients.append(
                            self.Orientation(oname, field, unique_fs))

    def _stub_declarations(self, parent):
        '''
        Insert declarations for any orientation quantities into a Kernel stub.

        :param parent: the f2pygen node representing the Kernel stub.
        :type parent: :py:class:`psyclone.f2pygen.SubroutineGen`

        '''
        from psyclone.f2pygen import DeclGen
        for orient in self._orients:
            ndf_name = get_fs_ndf_name(orient.function_space)
            parent.add(DeclGen(parent, datatype="integer", intent="in",
                               dimension=ndf_name, entity_decls=[orient.name]))

    def _invoke_declarations(self, parent):
        '''
        Insert declarations for any orientation quantities into a PSy-layer
        routine.

        :param parent: the f2pygen node representing the PSy-layer routine.
        :type parent: :py:class:`psyclone.f2pygen.SubroutineGen`

        '''
        from psyclone.f2pygen import DeclGen

        declns = [orient.name+"(:) => null()" for orient in self._orients]
        if declns:
            parent.add(DeclGen(parent, datatype="integer", pointer=True,
                               entity_decls=declns))


class DynFunctionSpaces(DynCollection):
    '''
    Handles the declaration and initialisation of all function-space-related
    quantities required by an Invoke.

    :param invoke: the Invoke or Kernel object.
    '''
    def __init__(self, kern_or_invoke):
        super(DynFunctionSpaces, self).__init__(kern_or_invoke)

        if self._invoke:
            self._function_spaces = self._invoke.unique_fss()[:]
        else:
            self._function_spaces = self._calls[0].arguments.unique_fss

        self._var_list = []

        # Loop over all unique function spaces used by our kernel(s)
        for function_space in self._function_spaces:

            # We need ndf for a space if a kernel iterates over cells,
            # has a field or operator on that space and is not a
            # CMA kernel performing a matrix-matrix operation.
            if self._invoke and not self._dofs_only or \
               self._kernel and self._kernel.cma_operation != "matrix-matrix":
                self._var_list.append(get_fs_ndf_name(function_space))

            # If there is a field on this space then add undf to list
            # to declare later. However, if the invoke contains only
            # kernels that iterate over dofs and distributed memory is
            # enabled then the number of dofs is obtained from the
            # field proxy and undf is not required.
            if self._invoke and self._invoke.field_on_space(function_space):
                if not (self._dofs_only and Config.get().distributed_memory):
                    self._var_list.append(get_fs_undf_name(function_space))
            elif self._kernel and field_on_space(function_space,
                                                 self._kernel.arguments):
                self._var_list.append(get_fs_undf_name(function_space))

    def _stub_declarations(self, parent):
        '''
        Add function-space-related declarations to a Kernel stub.

        :param parent: the node in the f2pygen AST representing the kernel \
                       stub to which to add declarations.
        :type parent: :py:class:`psyclone.f2pygen.SubroutineGen`
        '''
        from psyclone.f2pygen import DeclGen
        if self._var_list:
            # Declare ndf and undf for all function spaces
            parent.add(DeclGen(parent, datatype="integer", intent="in",
                               entity_decls=self._var_list))

    def _invoke_declarations(self, parent):
        '''
        Add function-space-related declarations to a PSy-layer routine.

        :param parent: the node in the f2pygen AST to which to add \
                       declarations.
        :type parent: :py:class:`psyclone.f2pygen.SubroutineGen`

        '''
        from psyclone.f2pygen import DeclGen
        if self._var_list:
            # Declare ndf and undf for all function spaces
            parent.add(DeclGen(parent, datatype="integer",
                               entity_decls=self._var_list))

    def initialise(self, parent):
        '''
        Create the code that initialises function-space quantities.

        :param parent: the node in the f2pygen AST representing the PSy-layer \
                       routine.
        :type parent: :py:class:`psyclone.f2pygen.SubroutineGen`

        '''
        from psyclone.f2pygen import CommentGen, AssignGen
        # Loop over all unique function spaces used by the kernels in
        # the invoke
        for function_space in self._function_spaces:
            # Initialise information associated with this function space.
            # If we have 1+ kernels that iterate over cells then we
            # will need ndf and undf. If we don't then we only need undf
            # (for the upper bound of the loop over dofs) if we're not
            # doing DM.
            if not (self._dofs_only and Config.get().distributed_memory):
                parent.add(CommentGen(parent, ""))
                parent.add(CommentGen(parent,
                                      " Initialise number of DoFs for " +
                                      function_space.mangled_name))
                parent.add(CommentGen(parent, ""))

            # Find an argument on this space to use to dereference
            arg = self._invoke.arg_for_funcspace(function_space)
            name = arg.proxy_name_indexed
            # Initialise ndf for this function space.
            if not self._dofs_only:
                ndf_name = get_fs_ndf_name(function_space)
                parent.add(AssignGen(parent, lhs=ndf_name,
                                     rhs=name +
                                     "%" + arg.ref_name(function_space) +
                                     "%get_ndf()"))
            # If there is a field on this space then initialise undf
            # for this function space. However, if the invoke contains
            # only kernels that iterate over dofs and distributed
            # memory is enabled then the number of dofs is obtained
            # from the field proxy and undf is not required.
            if not (self._dofs_only and Config.get().distributed_memory):
                if self._invoke.field_on_space(function_space):
                    undf_name = get_fs_undf_name(function_space)
                    parent.add(AssignGen(parent, lhs=undf_name,
                                         rhs=name + "%" +
                                         arg.ref_name(function_space) +
                                         "%get_undf()"))


class DynFields(DynCollection):
    '''
    Manages the declarations for all field arguments required by an Invoke
    or Kernel stub.

    '''
    def _invoke_declarations(self, parent):
        '''
        Add field-related declarations to the PSy-layer routine.

        :param parent: the node in the f2pygen AST representing the PSy-layer \
                       routine to which to add declarations.
        :type parent: :py:class:`psyclone.f2pygen.SubroutineGen`
        '''
        from psyclone.f2pygen import TypeDeclGen
        # Add the subroutine argument declarations for fields
        fld_args = self._invoke.unique_declns_by_intent("gh_field")
        for intent in FORTRAN_INTENT_NAMES:
            if fld_args[intent]:
                if intent == "out":
                    # The data part of a field might have intent(out) but
                    # in order to preserve the state of the whole
                    # derived-type object it must be declared as inout.
                    fort_intent = "inout"
                else:
                    fort_intent = intent
                parent.add(TypeDeclGen(parent, datatype="field_type",
                                       entity_decls=fld_args[intent],
                                       intent=fort_intent))

    def _stub_declarations(self, parent):
        '''
        Add field-related declarations to a Kernel stub.

        :param parent: the node in the f2pygen AST representing the Kernel \
                       stub to which to add declarations.
        :type parent: :py:class:`psyclone.f2pygen.SubroutineGen`
        '''
        from psyclone.f2pygen import DeclGen
        fld_args = psyGen.args_filter(self._kernel.args,
                                      arg_types=["gh_field"])
        for fld in fld_args:
            undf_name = get_fs_undf_name(fld.function_space)
            intent = fld.intent

            if fld.vector_size > 1:
                for idx in range(1, fld.vector_size+1):
                    text = (fld.name + "_" +
                            fld.function_space.mangled_name +
                            "_v" + str(idx))
                    parent.add(DeclGen(parent, datatype="real",
                                       kind="r_def", dimension=undf_name,
                                       intent=intent, entity_decls=[text]))
            else:
                parent.add(
                    DeclGen(parent, datatype="real", kind="r_def",
                            intent=fld.intent,
                            dimension=undf_name,
                            entity_decls=[fld.name + "_" +
                                          fld.function_space.mangled_name]))


class DynProxies(DynCollection):
    '''
    Handles all proxy-related declarations and initialisation. Unlike other
    sub-classes of DynCollection, we do not have to handle Kernel-stub
    generation since Kernels know nothing about proxies.

    '''
    def _invoke_declarations(self, parent):
        '''
        Insert declarations of all proxy-related quantities into the PSy layer.

        :param parent: the node in the f2pygen AST representing the PSy- \
                       layer routine.
        :type parent: :py:class:`psyclone.f2pygen.SubroutineGen`

        '''
        from psyclone.f2pygen import TypeDeclGen
        field_proxy_decs = self._invoke.unique_proxy_declarations("gh_field")
        if field_proxy_decs:
            parent.add(TypeDeclGen(parent,
                                   datatype="field_proxy_type",
                                   entity_decls=field_proxy_decs))
        op_proxy_decs = self._invoke.unique_proxy_declarations("gh_operator")
        if op_proxy_decs:
            parent.add(TypeDeclGen(parent,
                                   datatype="operator_proxy_type",
                                   entity_decls=op_proxy_decs))
        cma_op_proxy_decs = self._invoke.unique_proxy_declarations(
            "gh_columnwise_operator")
        if cma_op_proxy_decs:
            parent.add(TypeDeclGen(parent,
                                   datatype="columnwise_operator_proxy_type",
                                   entity_decls=cma_op_proxy_decs))

    def initialise(self, parent):
        '''
        Insert code into the PSy layer to initialise all necessary proxies.

        :param parent: node in the f2pygen AST representing the PSy-layer \
                       routine.
        :type parent: :py:class:`psyclone.f2pygen.SubroutineGen`

        '''
        from psyclone.f2pygen import CommentGen, AssignGen
        parent.add(CommentGen(parent, ""))
        parent.add(CommentGen(parent,
                              " Initialise field and/or operator proxies"))
        parent.add(CommentGen(parent, ""))
        for arg in self._invoke.psy_unique_vars:
            # We don't have proxies for scalars
            if arg.type in GH_VALID_SCALAR_NAMES:
                continue
            if arg.vector_size > 1:
                # the range function below returns values from
                # 1 to the vector size which is what we
                # require in our Fortran code
                for idx in range(1, arg.vector_size+1):
                    parent.add(
                        AssignGen(parent,
                                  lhs=arg.proxy_name+"("+str(idx)+")",
                                  rhs=arg.name+"("+str(idx)+")%get_proxy()"))
            else:
                parent.add(AssignGen(parent, lhs=arg.proxy_name,
                                     rhs=arg.name+"%get_proxy()"))


class DynCellIterators(DynCollection):
    '''
    Handles all entities required by kernels that iterate over cells.

    :param kern_or_invoke: the Kernel or Invoke for which to manage cell \
                           iterators.
    :type kern_or_invoke: :py:class:`psyclone.dynamo0p3.DynKern` or \
                          :py:class:`psyclone.dynamo0p3.DynInvoke`
    '''
    def __init__(self, kern_or_invoke):
        super(DynCellIterators, self).__init__(kern_or_invoke)

        # Use our namespace manager to create a unique name unless
        # the context and label match and in this case return the
        # previous name.
        self._nlayers_name = self._name_space_manager.create_name(
            root_name="nlayers", context="PSyVars", label="nlayers")

        # Store a reference to the first field/operator object that
        # we can use to look-up nlayers in the PSy layer.
        if not self._invoke:
            # We're not generating a PSy layer so we're done here.
            return
        first_var = None
        for var in self._invoke.psy_unique_vars:
            if var.type not in GH_VALID_SCALAR_NAMES:
                first_var = var
                break
        if not first_var:
            raise GenerationError(
                "Cannot create an Invoke with no field/operator arguments")
        self._first_var = first_var

    def _invoke_declarations(self, parent):
        '''
        Declare entities required for iterating over cells in the Invoke.

        :param parent: the f2pygen node representing the PSy-layer routine.
        :type parent: :py:class:`psyclone.f2pygen.SubroutineGen`

        '''
        from psyclone.f2pygen import DeclGen
        # We only need the number of layers in the mesh if we are calling
        # one or more kernels that iterate over cells
        if not self._dofs_only:
            parent.add(DeclGen(parent, datatype="integer",
                               entity_decls=[self._nlayers_name]))

    def _stub_declarations(self, parent):
        '''
        Declare entities required for a kernel stub that iterates over cells.

        :param parent: the f2pygen node representing the Kernel stub.
        :type parent: :py:class:`psyclone.f2pygen.SubroutineGen`
        '''
        from psyclone.f2pygen import DeclGen
        if self._kernel.cma_operation not in ["apply", "matrix-matrix"]:
            parent.add(DeclGen(parent, datatype="integer", intent="in",
                               entity_decls=[self._nlayers_name]))

    def initialise(self, parent):
        '''
        Look-up the number of vertical layers in the mesh in the PSy layer.

        :param parent: the f2pygen node representing the PSy-layer routine.
        :type parent: :py:class:`psyclone.f2pygen.SubroutineGen`

        '''
        from psyclone.f2pygen import CommentGen, AssignGen
        if not self._dofs_only:
            parent.add(CommentGen(parent, ""))
            parent.add(CommentGen(parent, " Initialise number of layers"))
            parent.add(CommentGen(parent, ""))
            parent.add(AssignGen(
                parent, lhs=self._nlayers_name,
                rhs=self._first_var.proxy_name_indexed + "%" +
                self._first_var.ref_name() + "%get_nlayers()"))


class DynScalarArgs(DynCollection):
    '''
    Handles the declaration of scalar kernel arguments appearing in either
    an Invoke or Kernel stub.

    :param node: the Kernel stub or Invoke for which to manage the scalar \
                 arguments.
    :type node: :py:class:`psyclone.dynamo0p3.DynKern` or \
                :py:class:`psyclone.dynamo0p3.DynInvoke`

    :raises InternalError: if an unrecognised type of scalar argument is \
                           encountered.
    '''
    def __init__(self, node):
        super(DynScalarArgs, self).__init__(node)

        if self._invoke:
            self._real_scalars = self._invoke.unique_declns_by_intent(
                "gh_real")
            self._int_scalars = self._invoke.unique_declns_by_intent(
                "gh_integer")
        else:
            # We have a kernel stub.
            self._real_scalars = {}
            self._int_scalars = {}
            for intent in FORTRAN_INTENT_NAMES:
                self._real_scalars[intent] = []
                self._int_scalars[intent] = []
            for arg in self._calls[0].arguments.args:
                if arg.type in GH_VALID_SCALAR_NAMES:
                    if arg.type == "gh_real":
                        self._real_scalars[arg.intent].append(arg.name)
                    elif arg.type == "gh_integer":
                        self._int_scalars[arg.intent].append(arg.name)
                    else:
                        raise InternalError(
                            "Scalar type '{0}' is in GH_VALID_SCALAR_NAMES but"
                            " not handled in DynScalarArgs".format(arg.type))

    def _invoke_declarations(self, parent):
        '''
        Insert declarations for all of the scalar arguments.

        :param parent: the f2pygen node in which to insert declarations.
        :type parent: :py:class:`psyclone.f2pygen.SubroutineGen`
        '''
        from psyclone.f2pygen import DeclGen
        for intent in FORTRAN_INTENT_NAMES:
            if self._real_scalars[intent]:
                parent.add(DeclGen(parent, datatype="real", kind="r_def",
                                   entity_decls=self._real_scalars[intent],
                                   intent=intent))

        for intent in FORTRAN_INTENT_NAMES:
            if self._int_scalars[intent]:
                parent.add(DeclGen(parent, datatype="integer",
                                   entity_decls=self._int_scalars[intent],
                                   intent=intent))

    def _stub_declarations(self, parent):
        '''
        Declarations for scalars in Kernel stubs are the same as for those
        in Invokes.

        :param parent: node in the f2pygen AST representing the Kernel stub \
                       to which to add declarations.
        :type parent: :py:class:`psyclone.f2pygen.SubroutineGen`
        '''
        self._invoke_declarations(parent)


class DynLMAOperators(DynCollection):
    '''
    Handles all entities associated with Local-Matrix-Assembly Operators.
    '''
    def _stub_declarations(self, parent):
        '''
        Declare all LMA-related quantities in a Kernel stub.

        :param parent: the f2pygen node representing the Kernel stub.
        :type parent: :py:class:`psyclone.f2pygen.SubroutineGen`

        '''
        from psyclone.f2pygen import DeclGen
        lma_args = psyGen.args_filter(
            self._kernel.arguments.args, arg_types=["gh_operator"])
        if lma_args:
            parent.add(DeclGen(parent, datatype="integer", intent="in",
                               entity_decls=["cell"]))
        for arg in lma_args:
            size = arg.name+"_ncell_3d"
            parent.add(DeclGen(parent, datatype="integer",
                               intent="in", entity_decls=[size]))
            ndf_name_to = get_fs_ndf_name(arg.function_space_to)
            ndf_name_from = get_fs_ndf_name(arg.function_space_from)
            parent.add(DeclGen(parent, datatype="real", kind="r_def",
                               dimension=",".join([ndf_name_to,
                                                   ndf_name_from, size]),
                               intent=arg.intent,
                               entity_decls=[arg.name]))

    def _invoke_declarations(self, parent):
        '''
        Declare all LMA-related quantities in a PSy-layer routine.

        :param parent: the f2pygen node representing the PSy-layer routine.
        :type parent: :py:class:`psyclone.f2pygen.SubroutineGen`

        '''
        from psyclone.f2pygen import TypeDeclGen

        op_declarations_dict = self._invoke.unique_declns_by_intent(
            "gh_operator")
        for intent in FORTRAN_INTENT_NAMES:
            if op_declarations_dict[intent]:
                if intent == "out":
                    # The data part of an operator might have intent(out) but
                    # in order to preserve the state of the whole derived-type
                    # object it must be declared as inout.
                    fort_intent = "inout"
                else:
                    fort_intent = intent
                parent.add(
                    TypeDeclGen(parent, datatype="operator_type",
                                entity_decls=op_declarations_dict[intent],
                                intent=fort_intent))


class DynCMAOperators(DynCollection):
    '''
    Holds all information on the Column-Matrix-Assembly operators
    required by an Invoke or Kernel stub.

    :param node: either an Invoke schedule or a single Kernel object.
    :type node: :py:class:`psyclone.dynamo0p3.DynSchedule` or \
                :py:class:`psyclone.dynamo0p3.DynKern`

    '''
    # The scalar parameters that must be passed along with a CMA operator
    # if its 'to' and 'from' spaces are the same
    cma_same_fs_params = ["nrow", "bandwidth", "alpha",
                          "beta", "gamma_m", "gamma_p"]
    # The scalar parameters that must be passed along with a CMA operator
    # if its 'to' and 'from' spaces are different
    cma_diff_fs_params = ["nrow", "ncol", "bandwidth", "alpha",
                          "beta", "gamma_m", "gamma_p"]

    def __init__(self, node):
        super(DynCMAOperators, self).__init__(node)

        # Look at every kernel call and generate a set of
        # the unique CMA operators involved. For each one we create a
        # dictionary entry. The key is the name of the CMA argument in the
        # PSy layer and the entry is itself another dictionary containing
        # two entries: the first 'arg' is the CMA argument object and the
        # second 'params' is the list of integer variables associated with
        # that CMA operator. The contents of this list depend on whether
        # or not the to/from function spaces of the CMA operator are the
        # same.
        self._cma_ops = OrderedDict()
        # You can't index into an OrderedDict so we keep a separate ref
        # to the first CMA argument we find.
        self._first_cma_arg = None
        for call in self._calls:
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
                        self._cma_ops[arg.name]["intent"] = arg.intent
                        # Keep a reference to the first CMA argument
                        if not self._first_cma_arg:
                            self._first_cma_arg = arg

    def initialise(self, parent):
        '''
        Generates the calls to the LFRic infrastructure that look-up
        the various components of each CMA operator. Adds these as
        children of the supplied parent node.

        :param parent: f2pygen node representing the PSy-layer routine.
        :type parent: :py:class:`psyclone.f2pygen.SubroutineGen`

        '''
        from psyclone.f2pygen import CommentGen, AssignGen, DeclGen

        # If we have no CMA operators then we do nothing
        if not self._cma_ops:
            return

        # If we have one or more CMA operators then we will need the number
        # of columns in the mesh
        parent.add(CommentGen(parent, ""))
        parent.add(CommentGen(parent, " Initialise number of cols"))
        parent.add(CommentGen(parent, ""))
        ncol_name = self._name_space_manager.create_name(
            root_name="ncell_2d", context="PSyVars", label="ncell_2d")
        parent.add(
            AssignGen(
                parent, lhs=ncol_name,
                rhs=self._first_cma_arg.proxy_name_indexed + "%ncell_2d"))
        parent.add(DeclGen(parent, datatype="integer",
                           entity_decls=[ncol_name]))

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

    def _invoke_declarations(self, parent):
        '''
        Generate the necessary PSy-layer declarations for all column-wise
        operators and their associated parameters.

        :param parent: the f2pygen node representing the PSy-layer routine.
        :type parent: :py:class:`psyclone.f2pygen.SubroutineGen`

        '''
        from psyclone.f2pygen import DeclGen, TypeDeclGen

        # If we have no CMA operators then we do nothing
        if not self._cma_ops:
            return

        # Add subroutine argument declarations for CMA operators that are
        # read or written (as with normal/LMA operators, they are never 'inc'
        # because they are discontinuous)
        cma_op_declarations_dict = self._invoke.unique_declns_by_intent(
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
                parent.add(
                    TypeDeclGen(parent, datatype="columnwise_operator_type",
                                entity_decls=cma_op_declarations_dict[intent],
                                intent=fort_intent))

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

    def _stub_declarations(self, parent):
        '''
        Generate all necessary declarations for CMA operators being passed to
        a Kernel stub.

        :param parent: f2pygen node representing the Kernel stub.
        :type parent: :py:class:`psyclone.f2pygen.SubroutineGen`

        '''
        from psyclone.f2pygen import DeclGen

        # If we have no CMA operators then we do nothing
        if not self._cma_ops:
            return

        # CMA operators always need the current cell index and the number
        # of columns in the mesh
        parent.add(DeclGen(parent, datatype="integer", intent="in",
                           entity_decls=["cell", "ncell_2d"]))

        for op_name in self._cma_ops:
            # Declare the associated scalar arguments before the array because
            # some of them are used to dimension the latter (and some compilers
            # get upset if this ordering is not followed)
            _local_args = []
            for param in self._cma_ops[op_name]["params"]:
                param_name = self._name_space_manager.create_name(
                    root_name=op_name+"_"+param,
                    context="PSyVars",
                    label=op_name+"_"+param)
                _local_args.append(param_name)
            parent.add(DeclGen(parent, datatype="integer",
                               intent="in",
                               entity_decls=_local_args))
            # Declare the array that holds the CMA operator
            bandwidth = op_name + "_bandwidth"
            nrow = op_name + "_nrow"
            intent = self._cma_ops[op_name]["intent"]
            parent.add(DeclGen(parent, datatype="real", kind="r_def",
                               dimension=",".join([bandwidth,
                                                   nrow, "ncell_2d"]),
                               intent=intent, entity_decls=[op_name]))


class DynMeshes(object):
    '''
    Holds all mesh-related information (including colour maps if
    required).  If there are no inter-grid kernels then there is only
    one mesh object required (when colouring or doing distributed memory).
    However, kernels performing inter-grid operations require multiple mesh
    objects as well as mesh maps and other quantities.

    There are two types of inter-grid operation; the first is "prolongation"
    where a field on a coarse mesh is mapped onto a fine mesh. The second
    is "restriction" where a field on a fine mesh is mapped onto a coarse
    mesh.

    :param invoke: the Invoke for which to extract information on all \
                   required inter-grid operations.
    :type invoke: :py:class:`psyclone.dynamo0p3.DynInvoke`
    :param unique_psy_vars: list of arguments to the PSy-layer routine.
    :type unique_psy_vars: list of \
                      :py:class:`psyclone.dynamo0p3.DynKernelArgument` objects.
    '''

    def __init__(self, invoke, unique_psy_vars):
        self._name_space_manager = NameSpaceFactory().create()
        # Dict of DynInterGrid objects holding information on the mesh-related
        # variables required by each inter-grid kernel. Keys are the kernel
        # names.
        self._ig_kernels = OrderedDict()
        # List of names of unique mesh variables referenced in the Invoke
        self._mesh_names = []
        # Whether or not the associated Invoke requires colourmap information
        self._needs_colourmap = False
        # Keep a reference to the InvokeSchedule so we can check for colouring
        # later
        self._schedule = invoke.schedule

        # Set used to generate a list of the unique mesh objects
        _name_set = set()

        # Find the first non-scalar argument to this PSy layer routine. We
        # will use this to look-up the mesh if there are no inter-grid
        # kernels in this invoke.
        self._first_var = None
        for var in unique_psy_vars:
            if var.type not in GH_VALID_SCALAR_NAMES:
                self._first_var = var
                break

        # Loop over all kernel calls in the schedule. Keep a list of
        # any non-intergrid kernels so that we can generate a verbose error
        # message if necessary.
        non_intergrid_kernels = []
        for call in self._schedule.coded_kernels():

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

            # Create an object to capture info. on this inter-grid kernel
            # and store in our dictionary
            self._ig_kernels[call.name] = DynInterGrid(fine_arg, coarse_arg)

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
        if non_intergrid_kernels and self._ig_kernels:
            raise GenerationError(
                "An invoke containing inter-grid kernels must contain no "
                "other kernel types but kernels '{0}' in invoke '{1}' are "
                "not inter-grid kernels.".format(
                    ", ".join([call.name for call in non_intergrid_kernels]),
                    invoke.name))

        # If we didn't have any inter-grid kernels but distributed memory
        # is enabled then we will still need a mesh object if we have one or
        # more kernels that iterate over cells. (Colourmaps also
        # require a mesh object but that is handled in _colourmap_init().)
        if not _name_set and (Config.get().distributed_memory and
                              not invoke.iterate_over_dofs_only):
            _name_set.add(self._name_space_manager.create_name(
                root_name="mesh", context="PSyVars", label="mesh"))

        # Convert the set of mesh names to a list and store
        self._mesh_names = sorted(_name_set)

    def _colourmap_init(self):
        '''
        Sets-up information on any required colourmaps. This cannot be done
        in the constructor since colouring is applied by Transformations
        and happens after the Schedule has already been constructed.
        '''
        for call in [call for call in self._schedule.coded_kernels() if
                     call.is_coloured()]:
            # Keep a record of whether or not any kernels (loops) in this
            # invoke have been coloured
            self._needs_colourmap = True

            if call.is_intergrid:
                # This is an inter-grid kernel so look-up the names of
                # the colourmap variables associated with the coarse
                # mesh (since that determines the iteration space).
                carg_name = self._ig_kernels[call.name].coarse.name
                # Colour map
                base_name = "cmap_" + carg_name
                colour_map = self._name_space_manager.create_name(
                    root_name=base_name, context="PSyVars", label=base_name)
                # No. of colours
                base_name = "ncolour_" + carg_name
                ncolours = self._name_space_manager.create_name(
                    root_name=base_name, context="PSyVars", label=base_name)
                # Add these names into the dictionary entry for this
                # inter-grid kernel
                self._ig_kernels[call.name].colourmap = colour_map
                self._ig_kernels[call.name].ncolours_var = ncolours

        if not self._mesh_names and self._needs_colourmap:
            # There aren't any inter-grid kernels but we do need colourmap
            # information and that means we'll need a mesh object
            mesh_name = self._name_space_manager.create_name(
                root_name="mesh", context="PSyVars", label="mesh")
            self._mesh_names.append(mesh_name)

    def declarations(self, parent):
        '''
        Declare variables specific to mesh objects.

        :param parent: the parent node to which to add the declarations
        :type parent: an instance of :py:class:`psyclone.f2pygen.BaseGen`
        '''
        from psyclone.f2pygen import DeclGen, TypeDeclGen, UseGen

        # Since we're now generating code, any transformations must
        # have been applied so we can set-up colourmap information
        self._colourmap_init()

        # We'll need various typedefs from the mesh module
        if self._mesh_names:
            parent.add(UseGen(parent, name="mesh_mod", only=True,
                              funcnames=["mesh_type"]))
        if self._ig_kernels:
            parent.add(UseGen(parent, name="mesh_map_mod", only=True,
                              funcnames=["mesh_map_type"]))
        # Declare the mesh object(s)
        for name in self._mesh_names:
            parent.add(TypeDeclGen(parent, pointer=True, datatype="mesh_type",
                                   entity_decls=[name + " => null()"]))
        # Declare the inter-mesh map(s) and cell map(s)
        for kern in self._ig_kernels.values():
            parent.add(TypeDeclGen(parent, pointer=True,
                                   datatype="mesh_map_type",
                                   entity_decls=[kern.mmap + " => null()"]))
            parent.add(
                DeclGen(parent, pointer=True, datatype="integer",
                        entity_decls=[kern.cell_map + "(:,:) => null()"]))

            # Declare the number of cells in the fine mesh and how many fine
            # cells there are per coarse cell
            parent.add(DeclGen(parent, datatype="integer",
                               entity_decls=[kern.ncell_fine,
                                             kern.ncellpercell]))
            # Declare variables to hold the colourmap information if required
            if kern.colourmap:
                parent.add(DeclGen(parent, datatype="integer",
                                   pointer=True,
                                   entity_decls=[kern.colourmap+"(:,:)"]))
                parent.add(DeclGen(parent, datatype="integer",
                                   entity_decls=[kern.ncolours_var]))

        if not self._ig_kernels and self._needs_colourmap:
            # There aren't any inter-grid kernels but we do need
            # colourmap information
            base_name = "cmap"
            colour_map = self._name_space_manager.create_name(
                root_name=base_name, context="PSyVars", label=base_name)
            # No. of colours
            base_name = "ncolour"
            ncolours = self._name_space_manager.create_name(
                root_name=base_name, context="PSyVars", label=base_name)
            # Add declarations for these variables
            parent.add(DeclGen(parent, datatype="integer",
                               pointer=True,
                               entity_decls=[colour_map+"(:,:)"]))
            parent.add(DeclGen(parent, datatype="integer",
                               entity_decls=[ncolours]))

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
            # contains no inter-grid kernels (which would require at least 2)
            parent.add(CommentGen(parent, " Create a mesh object"))
            parent.add(CommentGen(parent, ""))
            rhs = "%".join([self._first_var.proxy_name_indexed,
                            self._first_var.ref_name(), "get_mesh()"])
            parent.add(AssignGen(parent, pointer=True,
                                 lhs=self._mesh_names[0], rhs=rhs))
            if self._needs_colourmap:
                parent.add(CommentGen(parent, ""))
                parent.add(CommentGen(parent, " Get the colourmap"))
                parent.add(CommentGen(parent, ""))
                # Look-up variable names for colourmap and number of colours
                colour_map = self._name_space_manager.create_name(
                    root_name="cmap", context="PSyVars", label="cmap")
                ncolour = self._name_space_manager.create_name(
                    root_name="ncolour", context="PSyVars",
                    label="ncolour")
                # Get the number of colours
                parent.add(AssignGen(
                    parent, lhs=ncolour,
                    rhs="{0}%get_ncolours()".format(self._mesh_names[0])))
                # Get the colour map
                parent.add(AssignGen(parent, pointer=True, lhs=colour_map,
                                     rhs=self._mesh_names[0] +
                                     "%get_colour_map()"))
            return

        parent.add(CommentGen(
            parent,
            " Look-up mesh objects and loop limits for inter-grid kernels"))
        parent.add(CommentGen(parent, ""))

        # Keep a list of quantities that we've already initialised so
        # that we don't generate duplicate assignments
        initialised = []

        # Loop over the DynInterGrid objects in our dictionary
        for dig in self._ig_kernels.values():
            # We need pointers to both the coarse and the fine mesh
            fine_mesh = self._name_space_manager.create_name(
                root_name="mesh_{0}".format(dig.fine.name),
                context="PSyVars",
                label="mesh_{0}".format(dig.fine.name))
            coarse_mesh = self._name_space_manager.create_name(
                root_name="mesh_{0}".format(dig.coarse.name),
                context="PSyVars",
                label="mesh_{0}".format(dig.coarse.name))
            if fine_mesh not in initialised:
                initialised.append(fine_mesh)
                parent.add(
                    AssignGen(parent, pointer=True,
                              lhs=fine_mesh,
                              rhs="%".join([dig.fine.proxy_name_indexed,
                                            dig.fine.ref_name(),
                                            "get_mesh()"])))

            if coarse_mesh not in initialised:
                initialised.append(coarse_mesh)
                parent.add(
                    AssignGen(parent, pointer=True,
                              lhs=coarse_mesh,
                              rhs="%".join([dig.coarse.proxy_name_indexed,
                                            dig.coarse.ref_name(),
                                            "get_mesh()"])))
            # We also need a pointer to the mesh map which we get from
            # the coarse mesh
            if dig.mmap not in initialised:
                initialised.append(dig.mmap)
                parent.add(
                    AssignGen(parent, pointer=True,
                              lhs=dig.mmap,
                              rhs="{0}%get_mesh_map({1})".format(coarse_mesh,
                                                                 fine_mesh)))

            # Cell map. This is obtained from the mesh map.
            if dig.cell_map not in initialised:
                initialised.append(dig.cell_map)
                parent.add(
                    AssignGen(parent, pointer=True, lhs=dig.cell_map,
                              rhs=dig.mmap+"%get_whole_cell_map()"))

            # Number of cells in the fine mesh
            if dig.ncell_fine not in initialised:
                initialised.append(dig.ncell_fine)
                if Config.get().distributed_memory:
                    # TODO this hardwired depth of 2 will need changing in
                    # order to support redundant computation
                    parent.add(
                        AssignGen(parent, lhs=dig.ncell_fine,
                                  rhs=(fine_mesh+"%get_last_halo_cell"
                                       "(depth=2)")))
                else:
                    parent.add(
                        AssignGen(parent, lhs=dig.ncell_fine,
                                  rhs="%".join([dig.fine.proxy_name,
                                                dig.fine.ref_name(),
                                                "get_ncell()"])))

            # Number of fine cells per coarse cell.
            if dig.ncellpercell not in initialised:
                initialised.append(dig.ncellpercell)
                parent.add(
                    AssignGen(parent, lhs=dig.ncellpercell,
                              rhs=dig.mmap +
                              "%get_ntarget_cells_per_source_cell()"))

            # Colour map for the coarse mesh (if required)
            if dig.colourmap:
                # Number of colours
                parent.add(AssignGen(parent, lhs=dig.ncolours_var,
                                     rhs=coarse_mesh + "%get_ncolours()"))
                # Colour map itself
                parent.add(AssignGen(parent, lhs=dig.colourmap,
                                     pointer=True,
                                     rhs=coarse_mesh + "%get_colour_map()"))

    @property
    def intergrid_kernels(self):
        ''' Getter for the dictionary of intergrid kernels.

        :returns: Dictionary of intergrid kernels, indexed by name.
        :rtype: :py:class:`collections.OrderedDict`
        '''
        return self._ig_kernels


class DynInterGrid(object):
    '''
    Holds information on quantities required by an inter-grid kernel.

    :param fine_arg: Kernel argument on the fine mesh.
    :type fine_arg: :py:class:`psyclone.dynamo0p3.DynKernelArgument`
    :param coarse_arg: Kernel argument on the coarse mesh.
    :type coarse_arg: :py:class:`psyclone.dynamo0p3.DynKernelArgument`
    '''
    def __init__(self, fine_arg, coarse_arg):
        self._name_space_manager = NameSpaceFactory().create()

        # Arguments on the coarse and fine grids
        self.coarse = coarse_arg
        self.fine = fine_arg

        # Generate name for inter-mesh map
        base_mmap_name = "mmap_{0}_{1}".format(fine_arg.name,
                                               coarse_arg.name)
        self.mmap = self._name_space_manager.create_name(
            root_name=base_mmap_name,
            context="PSyVars",
            label=base_mmap_name)

        # Generate name for ncell variables
        self.ncell_fine = self._name_space_manager.create_name(
            root_name="ncell_{0}".format(fine_arg.name),
            context="PSyVars",
            label="ncell_{0}".format(fine_arg.name))
        # No. of fine cells per coarse cell
        self.ncellpercell = self._name_space_manager.create_name(
            root_name="ncpc_{0}_{1}".format(fine_arg.name,
                                            coarse_arg.name),
            context="PSyVars",
            label="ncpc_{0}_{1}".format(fine_arg.name,
                                        coarse_arg.name))
        # Name for cell map
        base_name = "cell_map_" + coarse_arg.name
        self.cell_map = self._name_space_manager.create_name(
            root_name=base_name, context="PSyVars", label=base_name)
        # We have no colourmap information when first created
        self.colourmap = ""
        # Name of the variable holding the number of colours
        self.ncolours_var = ""


class DynBasisFunctions(DynCollection):
    ''' Holds all information on the basis and differential basis
    functions required by an invoke or kernel call. This covers both those
    required for quadrature and for evaluators.

    :param node: either the schedule of an Invoke or a single Kernel object \
                 for which to extract information on all required \
                 basis/diff-basis functions.
    :type node: :py:class:`psyclone.dynamo0p3.DynInvokeSchedule` or \
                :py:class:`psyclone.dynamo0p3.DynKern`

    :raises InternalError: if a call has an unrecognised evaluator shape.
    '''
    qr_dim_vars = {"xyoz": ["np_xy", "np_z"]}
    qr_weight_vars = {"xyoz": ["weights_xy", "weights_z"]}

    def __init__(self, node):
        from psyclone.dynamo0p3_builtins import DynBuiltIn

        super(DynBasisFunctions, self).__init__(node)

        # Construct a list of all the basis/diff-basis functions required
        # by this invoke. Each entry in the list is a dictionary holding
        # the shape, the function space and the 'target' function spaces
        # (upon which the basis functions are evaluated).
        self._basis_fns = []
        # The dictionary of quadrature objects passed to this invoke. Keys
        # are the various VALID_QUADRATURE_SHAPES, values are a list of
        # associated quadrature variables. (i.e. we have a list of
        # quadrature arguments for each shape.)
        self._qr_vars = {}
        # The dict of target function spaces upon which we must provide
        # evaluators. Keys are the FS names, values are (FunctionSpace,
        # DynKernelArgument) tuples.
        self._eval_targets = OrderedDict()

        for call in self._calls:

            if isinstance(call, DynBuiltIn) or not call.eval_shape:
                # Skip this kernel if it doesn't require basis/diff basis fns
                continue

            if call.eval_shape in VALID_QUADRATURE_SHAPES:

                # This kernel requires quadrature
                if call.eval_shape not in self._qr_vars:
                    # We haven't seen a quadrature arg with this shape
                    # before so create a dictionary entry with an
                    # empty list
                    self._qr_vars[call.eval_shape] = []
                if call.qr_name not in self._qr_vars[call.eval_shape]:
                    # Add this qr argument to the list of those that
                    # have this shape
                    self._qr_vars[call.eval_shape].append(call.qr_name)

            elif call.eval_shape.lower() == "gh_evaluator":
                # An evaluator consists of basis or diff basis functions
                # for one FS evaluated on the nodes of another 'target' FS.
                # Make a dict of 2-tuples, each containing the FunctionSpace
                # and associated kernel argument for the target FSs.

                # Loop over the target FS for evaluators required by this
                # kernel
                for fs_name in call.eval_targets:
                    if fs_name not in self._eval_targets:
                        # We don't already have this space in our list so
                        # add it to the list of target spaces
                        self._eval_targets[fs_name] = \
                            call.eval_targets[fs_name]
            else:
                raise InternalError("Unrecognised evaluator shape: '{0}'. "
                                    "Should be one of {1}".format(
                                        call.eval_shape,
                                        VALID_EVALUATOR_SHAPES))

            # Both quadrature and evaluators require basis and/or differential
            # basis functions. This helper routine populates self._basis_fns
            # with entries describing the basis functions required by
            # this call.
            self._setup_basis_fns_for_call(call)

    @staticmethod
    def basis_first_dim_name(function_space):
        '''
        Get the name of the variable holding the first dimension of a
        basis function

        :param function_space: the function space the basis function is for
        :type function_space: :py:class:`psyclone.dynamo0p3.FunctionSpace`
        :return: a Fortran variable name
        :rtype: str

        '''
        return "dim_" + function_space.mangled_name

    @staticmethod
    def basis_first_dim_value(function_space):
        '''
        Get the size of the first dimension of a basis function.

        :param function_space: the function space the basis function is for
        :type function_space: :py:class:`psyclone.dynamo0p3.FunctionSpace`
        :return: an integer length.
        :rtype: string

        :raises GenerationError: if an unsupported function space is supplied \
                                 (e.g. ANY_SPACE_*, ANY_DISCONTINUOUS_SPACE_*)
        '''
        if function_space.orig_name.lower() in SCALAR_BASIS_SPACE_NAMES:
            first_dim = "1"
        elif function_space.orig_name.lower() in VECTOR_BASIS_SPACE_NAMES:
            first_dim = "3"
        else:
            # It is not possible to determine explicitly the first basis
            # function array dimension from the metadata for any_space or
            # any_discontinuous_space. This information needs to be passed
            # from the PSy layer to the kernels (see issue #461).
            raise GenerationError(
                "Unsupported space for basis function, "
                "expecting one of {0} but found "
                "'{1}'".format(VALID_FUNCTION_SPACES,
                               function_space.orig_name))
        return first_dim

    @staticmethod
    def diff_basis_first_dim_name(function_space):
        '''
        Get the name of the variable holding the first dimension of a
        differential basis function.

        :param function_space: the function space the diff-basis function \
                               is for.
        :type function_space: :py:class:`psyclone.dynamo0p3.FunctionSpace`
        :return: a Fortran variable name.
        :rtype: str

        '''
        return "diff_dim_" + function_space.mangled_name

    @staticmethod
    def diff_basis_first_dim_value(function_space):
        '''
        Get the size of the first dimension of an array for a
        differential basis function.

        :param function_space: the function space the diff-basis function \
                               is for.
        :type function_space: :py:class:`psyclone.dynamo0p3.FunctionSpace`
        :return: an integer length.
        :rtype: str

        :raises GenerationError: if an unsupported function space is \
                                 supplied (e.g. ANY_SPACE_*, \
                                 ANY_DISCONTINUOUS_SPACE_*)

        '''
        if function_space.orig_name.lower() in SCALAR_DIFF_BASIS_SPACE_NAMES:
            first_dim = "1"
        elif function_space.orig_name.lower() in VECTOR_DIFF_BASIS_SPACE_NAMES:
            first_dim = "3"
        else:
            # It is not possible to determine explicitly the first
            # differential basis function array dimension from the metadata
            # for any_space or any_discontinuous_space. This information
            # needs to be passed from the PSy layer to the kernels
            # (see issue #461).
            raise GenerationError(
                "Unsupported space for differential basis function, expecting "
                "one of {0} but found '{1}'".format(VALID_FUNCTION_SPACES,
                                                    function_space.orig_name))
        return first_dim

    def _setup_basis_fns_for_call(self, call):
        '''
        Populates self._basis_fns with entries describing the basis
        functions required by the supplied Call.

        :param call: the kernel call for which basis functions are required.
        :type call: :py:class:`psyclone.dynamo0p3.DynKern`

        :raises InternalError: if the supplied call is of incorrect type.
        :raises InternalError: if the supplied call has an unrecognised \
                               evaluator shape.
        '''
        if not isinstance(call, DynKern):
            raise InternalError("Expected a DynKern object but got: '{0}'".
                                format(type(call)))
        # We need a full FunctionSpace object for each function space
        # that has basis functions associated with it.
        for fsd in call.fs_descriptors.descriptors:

            # We need the full FS object, not just the name. Therefore
            # we first have to get a kernel argument that is on this
            # space...
            arg, fspace = call.arguments.get_arg_on_space_name(fsd.fs_name)

            # Populate a dict with the shape, function space and
            # associated kernel argument for this basis/diff-basis f'n.
            entry = {"shape": call.eval_shape,
                     "fspace": fspace,
                     "arg": arg}
            if call.eval_shape in VALID_QUADRATURE_SHAPES:
                # This is for quadrature - store the name of the
                # qr variable
                entry["qr_var"] = call.qr_name
                # Quadrature are evaluated at pre-determined
                # points rather than at the nodes of another FS.
                # We put one entry of None in the list of target
                # spaces to facilitate cases where we loop over
                # this list.
                entry["nodal_fspaces"] = [None]
            elif call.eval_shape.lower() == "gh_evaluator":
                # This is an evaluator
                entry["qr_var"] = None
                # Store a list of the FunctionSpace objects for which
                # these basis functions are to be evaluated
                entry["nodal_fspaces"] = [items[0] for items in
                                          call.eval_targets.values()]
            else:
                raise InternalError("Unrecognised evaluator shape: '{0}'. "
                                    "Should be one of {1}".format(
                                        call.eval_shape,
                                        VALID_EVALUATOR_SHAPES))

            # Add our newly-constructed dict object to the list describing
            # the required basis and/or differential basis functions for
            # this Invoke.
            if fsd.requires_basis:
                entry["type"] = "basis"
                self._basis_fns.append(entry)
            if fsd.requires_diff_basis:
                # Take a shallow copy of the dict and just modify the
                # 'type' of the basis function it describes (this works
                # because the 'type' entry is a primitive type [str]).
                diff_entry = entry.copy()
                diff_entry["type"] = "diff-basis"
                self._basis_fns.append(diff_entry)

    def _stub_declarations(self, parent):
        '''
        Insert the variable declarations required by the basis functions into
        the Kernel stub.

        :param parent: the f2pygen node representing the Kernel stub.
        :type parent: :py:class:`psyclone.f2pygen.SubroutineGen`

        '''
        from psyclone.f2pygen import DeclGen

        if not self._qr_vars and not self._eval_targets:
            return

        # Get the lists of dimensioning variables and basis arrays
        var_dims, basis_arrays = self._basis_fn_declns()

        if var_dims:
            parent.add(DeclGen(parent, datatype="integer", intent="in",
                               entity_decls=var_dims))
        for basis in basis_arrays:
            parent.add(DeclGen(parent, datatype="real",
                               kind="r_def", intent="in",
                               dimension=",".join(basis_arrays[basis]),
                               entity_decls=[basis]))
        for qr_shape in VALID_QUADRATURE_SHAPES:
            if qr_shape.lower() == "gh_quadrature_xyoz":
                if qr_shape not in self._qr_vars:
                    continue
                parent.add(DeclGen(parent, datatype="real", kind="r_def",
                                   intent="in", dimension="np_xy",
                                   entity_decls=["weights_xy"]))
                parent.add(DeclGen(parent, datatype="real", kind="r_def",
                                   intent="in", dimension="np_z",
                                   entity_decls=["weights_z"]))
            else:
                raise GenerationError(
                    "Quadrature shapes other than GH_QUADRATURE_XYoZ are not "
                    "yet supported - got '{0}'".format(qr_shape))

    def _invoke_declarations(self, parent):
        '''
        Add basis-function declarations to the PSy layer.

        :param parent: f2pygen node represening the PSy-layer routine.
        :type parent: :py:class:`psyclone.f2pygen.SubroutineGen`

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

    def initialise(self, parent):
        '''
        Create the declarations and assignments required for the
        basis-functions required by an invoke. These are added as children
        of the supplied parent node in the AST.

        :param parent: the node in the f2pygen AST that will be the
                       parent of all of the declarations and assignments.
        :type parent: :py:class:`psyclone.f2pygen.SubroutineGen`

        :raises InternalError: if an invalid entry is encountered in the \
                               self._basis_fns list.
        '''
        from psyclone.f2pygen import CommentGen, AssignGen, DeclGen, \
            AllocateGen, UseGen
        basis_declarations = []

        # We need BASIS and/or DIFF_BASIS if any kernel requires quadrature
        # or an evaluator
        if self._qr_vars or self._eval_targets:
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

        if self._eval_targets:
            parent.add(CommentGen(parent, ""))
            parent.add(CommentGen(parent,
                                  " Initialise evaluator-related quantities "
                                  "for the target function spaces"))
            parent.add(CommentGen(parent, ""))

        for (fspace, arg) in self._eval_targets.values():
            # We need the list of nodes for each unique FS upon which we need
            # to evaluate basis/diff-basis functions
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
            parent.add(CommentGen(parent, " Allocate basis/diff-basis arrays"))
            parent.add(CommentGen(parent, ""))

        var_dim_list = []
        for basis_fn in self._basis_fns:
            # Get the extent of the first dimension of the basis array. and
            if basis_fn['type'] == "basis":
                first_dim = self.basis_first_dim_name(basis_fn["fspace"])
                dim_space = "get_dim_space()"
            elif basis_fn['type'] == "diff-basis":
                first_dim = self.diff_basis_first_dim_name(
                    basis_fn["fspace"])
                dim_space = "get_dim_space_diff()"
            else:
                raise InternalError(
                    "Unrecognised type of basis function: '{0}'. Should "
                    "be either 'basis' or 'diff-basis'.".format(
                        basis_fn['type']))

            if first_dim not in var_dim_list:
                var_dim_list.append(first_dim)
                rhs = "%".join(
                    [basis_fn["arg"].proxy_name_indexed,
                     basis_fn["arg"].ref_name(basis_fn["fspace"]),
                     dim_space])
                parent.add(AssignGen(parent, lhs=first_dim, rhs=rhs))

        var_dims, basis_arrays = self._basis_fn_declns()

        if var_dims:
            # declare dim and diff_dim for all function spaces
            parent.add(DeclGen(parent, datatype="integer",
                               entity_decls=var_dims))

        basis_declarations = []
        for basis in basis_arrays:
            parent.add(
                AllocateGen(parent,
                            basis+"("+", ".join(basis_arrays[basis])+")"))
            basis_declarations.append(
                basis+"("+",".join([":"]*len(basis_arrays[basis]))+")")

        # declare the basis function arrays
        if basis_declarations:
            parent.add(DeclGen(parent, datatype="real",
                               allocatable=True, kind="r_def",
                               entity_decls=basis_declarations))

        # Compute the values for any basis arrays
        self._compute_basis_fns(parent)

    def _basis_fn_declns(self):
        '''
        Extracts all information relating to the necessary declarations
        for basis-function arrays.

        :returns: a 2-tuple containing a list of dimensioning variables & a \
                  dict of basis arrays.
        :rtype: (list of str, dict)

        :raises InternalError: if neither self._invoke or self._kernel are set.
        :raises InternalError: if an unrecognised type of basis function is \
                               encountered.
        :raises InternalError: if an unrecognised evaluator shape is \
                               encountered.
        '''
        # Dictionary of basis arrays where key values are the array names and
        # entries are a list of dimensions.
        basis_arrays = OrderedDict()
        # List of names of dimensioning (scalar) variables
        var_dim_list = []

        # Loop over the list of dicts describing each basis function
        # required by this Invoke.
        for basis_fn in self._basis_fns:
            # Get the extent of the first dimension of the basis array and
            # store whether we have a basis or a differential basis function.
            # Currently there are only those two possible types of basis
            # function and we store which we have in is_diff_basis. Should
            # further basis-function types be added in the future then the if
            # blocks that use if_diff_basis further down must be updated.
            if basis_fn['type'] == "basis":
                if self._invoke:
                    first_dim = self.basis_first_dim_name(basis_fn["fspace"])
                elif self._kernel:
                    first_dim = self.basis_first_dim_value(basis_fn["fspace"])
                else:
                    raise InternalError("Require basis functions but do not "
                                        "have either a Kernel or an "
                                        "Invoke. Should be impossible.")
                dim_space = "get_dim_space()"
                is_diff_basis = False
            elif basis_fn['type'] == "diff-basis":
                if self._invoke:
                    first_dim = self.diff_basis_first_dim_name(
                        basis_fn["fspace"])
                elif self._kernel:
                    first_dim = self.diff_basis_first_dim_value(
                        basis_fn["fspace"])
                else:
                    raise InternalError("Require differential basis functions "
                                        "but do not have either a Kernel or "
                                        "an Invoke. Should be impossible.")
                dim_space = "get_dim_space_diff()"
                is_diff_basis = True
            else:
                raise InternalError(
                    "Unrecognised type of basis function: '{0}'. Should "
                    "be either 'basis' or 'diff-basis'.".format(
                        basis_fn['type']))

            if self._invoke and first_dim not in var_dim_list:
                var_dim_list.append(first_dim)

            if basis_fn["shape"] in VALID_QUADRATURE_SHAPES:

                if is_diff_basis:
                    op_name = get_fs_operator_name("gh_diff_basis",
                                                   basis_fn["fspace"],
                                                   qr_var=basis_fn["qr_var"])
                else:
                    op_name = get_fs_operator_name("gh_basis",
                                                   basis_fn["fspace"],
                                                   qr_var=basis_fn["qr_var"])
                if op_name in basis_arrays:
                    # We've already seen a basis with this name so skip
                    continue

                # Dimensionality of the basis arrays depends on the
                # type of quadrature...
                alloc_args = qr_basis_alloc_args(first_dim, basis_fn)
                for arg in alloc_args:
                    # In a kernel stub the first dimension of the array is
                    # a numerical value so make sure we don't try and declare
                    # it as a variable.
                    if not arg[0].isdigit() and arg not in var_dim_list:
                        var_dim_list.append(arg)
                basis_arrays[op_name] = alloc_args

            elif basis_fn["shape"].lower() == "gh_evaluator":
                # This is an evaluator and thus may be required on more than
                # one function space
                for target_space in basis_fn["nodal_fspaces"]:
                    if is_diff_basis:
                        op_name = get_fs_operator_name(
                            "gh_diff_basis", basis_fn["fspace"],
                            qr_var=basis_fn["qr_var"],
                            on_space=target_space)
                    else:
                        op_name = get_fs_operator_name(
                            "gh_basis", basis_fn["fspace"],
                            qr_var=basis_fn["qr_var"],
                            on_space=target_space)
                    if op_name in basis_arrays:
                        continue
                    # We haven't seen a basis with this name before so
                    # need to store its dimensions
                    basis_arrays[op_name] = [
                        first_dim,
                        get_fs_ndf_name(basis_fn["fspace"]),
                        get_fs_ndf_name(target_space)]
            else:
                raise InternalError(
                    "Unrecognised evaluator shape: '{0}'. Should be one of "
                    "{1}".format(basis_fn["shape"], VALID_EVALUATOR_SHAPES))

        return (var_dim_list, basis_arrays)

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

        for qr_arg_name in self._qr_vars["gh_quadrature_xyoz"]:

            # We generate unique names for the integers holding the numbers
            # of quadrature points by appending the name of the quadrature
            # argument
            parent.add(
                DeclGen(
                    parent, datatype="integer",
                    entity_decls=[name+"_"+qr_arg_name
                                  for name in self.qr_dim_vars["xyoz"]]))
            decl_list = [name+"_"+qr_arg_name+"(:) => null()"
                         for name in self.qr_weight_vars["xyoz"]]
            parent.add(
                DeclGen(parent, datatype="real", pointer=True,
                        kind="r_def", entity_decls=decl_list))
            # Get the quadrature proxy
            proxy_name = qr_arg_name + "_proxy"
            parent.add(
                AssignGen(parent, lhs=proxy_name,
                          rhs=qr_arg_name+"%"+"get_quadrature_proxy()"))
            # Number of points in each dimension
            for qr_var in self.qr_dim_vars["xyoz"]:
                parent.add(
                    AssignGen(parent, lhs=qr_var+"_"+qr_arg_name,
                              rhs=proxy_name+"%"+qr_var))
            # Pointers to the weights arrays
            for qr_var in self.qr_weight_vars["xyoz"]:
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

    def _compute_basis_fns(self, parent):
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
            parent.add(CommentGen(parent, " Compute basis/diff-basis arrays"))
            parent.add(CommentGen(parent, ""))

        for basis_fn in self._basis_fns:

            # Currently there are only two possible types of basis function
            # and we store which we have in is_diff_basis. If support for
            # other basis function types is added in future then the if-blocks
            # (further down) that use is_diff_basis will have to be changed.
            if basis_fn["type"] == "diff-basis":
                is_diff_basis = True
            elif basis_fn["type"] == "basis":
                is_diff_basis = False
            else:
                raise InternalError(
                    "Unrecognised type of basis function: '{0}'. Expected one "
                    "of 'basis' or 'diff-basis'.". format(basis_fn["type"]))
            if basis_fn["shape"] in VALID_QUADRATURE_SHAPES:
                if is_diff_basis:
                    op_name = get_fs_operator_name("gh_diff_basis",
                                                   basis_fn["fspace"],
                                                   qr_var=basis_fn["qr_var"])
                else:
                    op_name = get_fs_operator_name("gh_basis",
                                                   basis_fn["fspace"],
                                                   qr_var=basis_fn["qr_var"])
                if op_name in op_name_list:
                    # Jump over any basis arrays we've seen before
                    continue
                op_name_list.append(op_name)

                # Create the argument list
                if is_diff_basis:
                    args = ["DIFF_BASIS",
                            basis_fn["arg"].proxy_name_indexed + "%" +
                            basis_fn["arg"].ref_name(basis_fn["fspace"]),
                            self.diff_basis_first_dim_name(basis_fn["fspace"]),
                            get_fs_ndf_name(basis_fn["fspace"]), op_name]
                else:
                    args = ["BASIS",
                            basis_fn["arg"].proxy_name_indexed + "%" +
                            basis_fn["arg"].ref_name(basis_fn["fspace"]),
                            self.basis_first_dim_name(basis_fn["fspace"]),
                            get_fs_ndf_name(basis_fn["fspace"]), op_name]
                # insert the basis array call
                parent.add(
                    CallGen(parent,
                            name=basis_fn["qr_var"]+"%compute_function",
                            args=args))
            elif basis_fn["shape"].lower() == "gh_evaluator":
                # We have an evaluator. We may need this on more than one
                # function space.
                for space in basis_fn["nodal_fspaces"]:
                    if is_diff_basis:
                        op_name = get_fs_operator_name("gh_diff_basis",
                                                       basis_fn["fspace"],
                                                       on_space=space)
                    else:
                        op_name = get_fs_operator_name("gh_basis",
                                                       basis_fn["fspace"],
                                                       on_space=space)
                    if op_name in op_name_list:
                        # Jump over any basis arrays we've seen before
                        continue
                    op_name_list.append(op_name)

                    nodal_loop_var = "df_nodal"
                    loop_var_list.add(nodal_loop_var)

                    # Loop over dofs of target function space
                    nodal_dof_loop = DoGen(
                        parent, nodal_loop_var, "1", get_fs_ndf_name(space))
                    parent.add(nodal_dof_loop)

                    dof_loop_var = "df_" + basis_fn["fspace"].mangled_name
                    loop_var_list.add(dof_loop_var)

                    dof_loop = DoGen(nodal_dof_loop, dof_loop_var,
                                     "1", get_fs_ndf_name(basis_fn["fspace"]))
                    nodal_dof_loop.add(dof_loop)
                    lhs = op_name + "(:," + "df_" + \
                        basis_fn["fspace"].mangled_name + "," + "df_nodal)"
                    if is_diff_basis:
                        rhs = "%".join(
                            [basis_fn["arg"].proxy_name_indexed,
                             basis_fn["arg"].ref_name(basis_fn["fspace"]),
                             "call_function(DIFF_BASIS," + dof_loop_var +
                             ",nodes_" + space.mangled_name +
                             "(:," + nodal_loop_var + "))"])
                    else:
                        rhs = "%".join(
                            [basis_fn["arg"].proxy_name_indexed,
                             basis_fn["arg"].ref_name(basis_fn["fspace"]),
                             "call_function(BASIS," + dof_loop_var +
                             ",nodes_" + space.mangled_name +
                             "(:," + nodal_loop_var + "))"])
                    dof_loop.add(AssignGen(dof_loop, lhs=lhs, rhs=rhs))
            else:
                raise InternalError(
                    "Unrecognised shape '{0}' specified for basis function. "
                    "Should be one of: {1}".format(basis_fn['shape'],
                                                   VALID_EVALUATOR_SHAPES))
        if loop_var_list:
            # Declare any loop variables
            parent.add(DeclGen(parent, datatype="integer",
                               entity_decls=sorted(loop_var_list)))

    def deallocate(self, parent):
        '''
        Add code to deallocate all basis/diff-basis function arrays

        :param parent: node in the f2pygen AST to which the deallocate
                       calls will be added
        :type parent: :py:class:`psyclone.f2pygen.SubroutineGen`

        :raises InternalError: if an unrecognised type of basis function \
                               is encountered.
        '''
        from psyclone.f2pygen import CommentGen, DeallocateGen

        if self._basis_fns:
            # deallocate all allocated basis function arrays
            parent.add(CommentGen(parent, ""))
            parent.add(CommentGen(parent, " Deallocate basis arrays"))
            parent.add(CommentGen(parent, ""))

        func_space_var_names = set()
        for basis_fn in self._basis_fns:
            # add the basis array name to the list to use later
            if basis_fn["type"] == "basis":
                for fspace in basis_fn["nodal_fspaces"]:
                    op_name = get_fs_operator_name("gh_basis",
                                                   basis_fn["fspace"],
                                                   qr_var=basis_fn["qr_var"],
                                                   on_space=fspace)
                    func_space_var_names.add(op_name)
            elif basis_fn["type"] == "diff-basis":
                for fspace in basis_fn["nodal_fspaces"]:
                    op_name = get_fs_operator_name("gh_diff_basis",
                                                   basis_fn["fspace"],
                                                   qr_var=basis_fn["qr_var"],
                                                   on_space=fspace)
                    func_space_var_names.add(op_name)
            else:
                raise InternalError(
                    "Unrecognised type of basis function: '{0}'. Should be "
                    "one of 'basis' or 'diff-basis'.".format(basis_fn["type"]))
        if func_space_var_names:
            # add the required deallocate call
            parent.add(DeallocateGen(parent, sorted(func_space_var_names)))


class DynBoundaryConditions(DynCollection):
    '''
    Manages declarations and initialisation of quantities required by
    kernels that need boundary condition information.

    :param node: the Invoke or Kernel stub for which we are to handle \
                 any boundary conditions.
    :type node: :py:class:`psyclone.dynamo0p3.DynInvoke` or \
                :py:class:`psyclone.dynamo0p3.DynKern`

    :raises GenerationError: if a kernel named "enforce_bc_code" is found \
                             but does not have an argument on ANY_SPACE_1.
    :raises GenerationError: if a kernel named "enforce_operator_bc_code" is \
                             found but does not have exactly one argument.
    '''
    # Define a BoundaryDofs namedtuple to help us manage the arrays that
    # are required.
    BoundaryDofs = namedtuple("BoundaryDofs", ["argument", "function_space"])

    def __init__(self, node):
        super(DynBoundaryConditions, self).__init__(node)

        self._boundary_dofs = []
        # Check through all the kernel calls to see whether any of them
        # require boundary conditions. Currently this is done by recognising
        # the kernel name.
        for call in self._calls:
            if call.name.lower() == "enforce_bc_code":
                bc_fs = None
                for fspace in call.arguments.unique_fss:
                    if fspace.orig_name == "any_space_1":
                        bc_fs = fspace
                        break
                if not bc_fs:
                    raise GenerationError(
                        "The enforce_bc_code kernel must have an argument on "
                        "ANY_SPACE_1 but failed to find such an argument.")
                farg = call.arguments.get_arg_on_space(bc_fs)
                self._boundary_dofs.append(self.BoundaryDofs(farg, bc_fs))
            elif call.name.lower() == "enforce_operator_bc_code":
                # Check that the kernel only has one argument
                if len(call.arguments.args) != 1:
                    raise GenerationError(
                        "The enforce_operator_bc_code kernel must have exactly"
                        " one argument but found {0}".format(
                            len(call.arguments.args)))
                op_arg = call.arguments.args[0]
                bc_fs = op_arg.function_space_to
                self._boundary_dofs.append(self.BoundaryDofs(op_arg, bc_fs))

    def _invoke_declarations(self, parent):
        '''
        Add declarations for any boundary-dofs arrays required by an Invoke.

        :param parent: node in the PSyIR to which to add declarations.
        :type parent: :py:class:`psyclone.psyGen.Node`
        '''
        from psyclone.f2pygen import DeclGen
        for dofs in self._boundary_dofs:
            name = "boundary_dofs_" + dofs.argument.name
            parent.add(DeclGen(parent, datatype="integer", pointer="True",
                               entity_decls=[name+"(:,:) => null()"]))

    def _stub_declarations(self, parent):
        '''
        Add declarations for any boundary-dofs arrays required by a kernel.

        :param parent: node in the PSyIR to which to add declarations.
        :type parent: :py:class:`psyclone.psyGen.Node`
        '''
        from psyclone.f2pygen import DeclGen
        for dofs in self._boundary_dofs:
            name = "boundary_dofs_" + dofs.argument.name
            ndf_name = get_fs_ndf_name(dofs.function_space)
            parent.add(DeclGen(parent, datatype="integer", intent="in",
                               dimension=",".join([ndf_name, "2"]),
                               entity_decls=[name]))

    def initialise(self, parent):
        '''
        Initialise any boundary-dofs arrays required by an Invoke.

        :param parent: node in PSyIR to which to add declarations.
        :type parent: :py:class:`psyclone.psyGen.Node`
        '''
        from psyclone.f2pygen import AssignGen
        for dofs in self._boundary_dofs:
            name = "boundary_dofs_" + dofs.argument.name
            parent.add(AssignGen(
                parent, pointer=True, lhs=name,
                rhs="%".join([dofs.argument.proxy_name,
                              dofs.argument.ref_name(dofs.function_space),
                              "get_boundary_dofs()"])))


class DynInvoke(Invoke):
    '''The Dynamo specific invoke class. This passes the Dynamo
    specific InvokeSchedule class to the base class so it creates the one we
    require.  Also overrides the gen_code method so that we generate
    dynamo specific invocation code.

    '''
    def __init__(self, alg_invocation, idx):
        '''
        :param alg_invocation: node in the AST describing the invoke call
        :type alg_invocation: :py:class:`psyclone.parse.algorithm.InvokeCall`
        :param int idx: the position of the invoke in the list of invokes \
                        contained in the Algorithm
        :raises GenerationError: if integer reductions are required in the \
                                 psy-layer
        '''
        if False:  # pylint: disable=using-constant-test
            self._schedule = DynInvokeSchedule(None)  # for pyreverse
        reserved_names_list = []
        reserved_names_list.extend(STENCIL_MAPPING.values())
        reserved_names_list.extend(VALID_STENCIL_DIRECTIONS)
        reserved_names_list.extend(["omp_get_thread_num",
                                    "omp_get_max_threads"])
        Invoke.__init__(self, alg_invocation, idx, DynInvokeSchedule,
                        reserved_names=reserved_names_list)

        # The baseclass works out the algorithm code's unique argument
        # list and stores it in the self._alg_unique_args
        # list. However, the base class currently ignores any stencil and qr
        # arguments so we need to add them in.

        self.scalar_args = DynScalarArgs(self)

        # initialise our invoke stencil information
        self.stencil = DynStencils(self)

        # Initialise our information on the function spaces used by this Invoke
        self.function_spaces = DynFunctionSpaces(self)

        # Initialise the object holding all information on the dofmaps
        # required by this invoke.
        self.dofmaps = DynDofmaps(self)

        # Initialise information on all of the fields accessed in this Invoke.
        self.fields = DynFields(self)

        # Initialise info. on all of the LMA operators used in this Invoke.
        self.lma_ops = DynLMAOperators(self)

        # Initialise the object holding all information on the column-
        # -matrix assembly operators required by this invoke.
        self.cma_ops = DynCMAOperators(self)

        # Initialise the object holding all information on the quadrature
        # and/or evaluators required by this invoke
        self.evaluators = DynBasisFunctions(self)

        # Initialise the object holding all information related to meshes
        # and inter-grid operations
        self.meshes = DynMeshes(self, self.psy_unique_vars)

        # Initialise the object holding information on any boundary-condition
        # kernel calls
        self.boundary_conditions = DynBoundaryConditions(self)

        # Information on all proxies required by this Invoke
        self.proxies = DynProxies(self)

        # Information required by kernels that iterate over cells
        self.cell_iterators = DynCellIterators(self)

        # Information on any orientation arrays required by this invoke
        self.orientation = DynOrientations(self)

        # Extend arg list with stencil information
        self._alg_unique_args.extend(self.stencil.unique_alg_vars)

        # adding in qr arguments
        self._alg_unique_qr_args = []
        for call in self.schedule.kernels():
            if call.qr_required:
                if call.qr_text not in self._alg_unique_qr_args:
                    self._alg_unique_qr_args.append(call.qr_text)
        self._alg_unique_args.extend(self._alg_unique_qr_args)
        # we also need to work out the names to use for the qr
        # arguments within the psy layer. These are stored in the
        # _psy_unique_qr_vars list
        self._psy_unique_qr_vars = []
        for call in self.schedule.kernels():
            if call.qr_required:
                if call.qr_name not in self._psy_unique_qr_vars:
                    self._psy_unique_qr_vars.append(call.qr_name)

        # lastly, add in halo exchange calls and global sums if
        # required. We only need to add halo exchange calls for fields
        # since operators are assembled in place and scalars don't
        # have halos. We only need to add global sum calls for scalars
        # which have a gh_sum access.
        if Config.get().distributed_memory:
            # halo exchange calls
            for loop in self.schedule.loops():
                loop.create_halo_exchanges()
            # global sum calls
            for loop in self.schedule.loops():
                for scalar in loop.args_filter(
                        arg_types=GH_VALID_SCALAR_NAMES,
                        arg_accesses=AccessType.get_valid_reduction_modes(),
                        unique=True):
                    global_sum = DynGlobalSum(scalar, parent=loop.parent)
                    loop.parent.children.insert(loop.position+1, global_sum)

    def unique_proxy_declarations(self, datatype, access=None):
        ''' Returns a list of all required proxy declarations for the
        specified datatype.  If access is supplied (e.g. "AccessType.WRITE")
        then only declarations with that access are returned.
        :param str datatype: Datatype that proxy declarations are \
                             searched for.
        :param access: optional AccessType for the specified data type.
        :type access: :py:class:`psyclone.core.access_type.AccessType`.
        :return: a list of all required proxy declarations for the \
                 specified datatype.
        :raises GenerationError: if datatype is invalid.
        :raises InternalError: if an invalid access is specified, i.e. \
                not of type AccessType.
        '''
        if datatype not in GH_VALID_ARG_TYPE_NAMES:
            raise GenerationError(
                "unique_proxy_declarations called with an invalid datatype. "
                "Expected one of '{0}' but found '{1}'".
                format(str(GH_VALID_ARG_TYPE_NAMES), datatype))
        if access and not isinstance(access, AccessType):
            api_config = Config.get().api_conf("dynamo0.3")
            valid_names = api_config.get_valid_accesses_api()
            raise InternalError(
                "unique_proxy_declarations called with an invalid access "
                "type. Expected one of '{0}' but got '{1}'".
                format(valid_names, access))
        declarations = []
        for call in self.schedule.kernels():
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
        for kern_call in self.schedule.kernels():
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
        for kern_call in self.schedule.kernels():
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

    @property
    def iterate_over_dofs_only(self):
        '''
        :returns: whether or not this Invoke consists only of kernels that \
                  iterate over DoFs.
        :rtype: bool
        '''
        for kern_call in self.schedule.kernels():
            if kern_call.iterates_over.lower() != "dofs":
                return False
        return True

    def field_on_space(self, func_space):
        ''' If a field exists on this space for any kernel in this
        invoke then return that field. Otherwise return None. '''
        for kern_call in self.schedule.kernels():
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
        :param parent: The parent node in the AST (of the code to be \
                       generated) to which the node describing the PSy \
                       subroutine will be added
        :type parent: :py:class:`psyclone.f2pygen.ModuleGen`
        '''
        from psyclone.f2pygen import SubroutineGen, AssignGen, \
            DeclGen, CommentGen

        # Create a namespace manager so we can avoid name clashes
        self._name_space_manager = NameSpaceFactory().create()
        # Create the subroutine
        invoke_sub = SubroutineGen(parent, name=self.name,
                                   args=self.psy_unique_var_names +
                                   self.stencil.unique_alg_vars +
                                   self._psy_unique_qr_vars)

        # Declare all quantities required by this PSy routine (invoke)
        for entities in [self.scalar_args, self.fields, self.lma_ops,
                         self.stencil, self.orientation, self.meshes,
                         self.function_spaces, self.dofmaps, self.cma_ops,
                         self.boundary_conditions, self.evaluators,
                         self.proxies, self.cell_iterators]:
            entities.declarations(invoke_sub)

        # Initialise all quantities required by this PSy routine (invoke)

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

        for entities in [self.proxies, self.cell_iterators, self.meshes,
                         self.stencil, self.orientation, self.dofmaps,
                         self.cma_ops, self.boundary_conditions,
                         self.function_spaces, self.evaluators]:
            entities.initialise(invoke_sub)

        # Now that everything is initialised, we can call our kernels

        invoke_sub.add(CommentGen(invoke_sub, ""))
        if Config.get().distributed_memory:
            invoke_sub.add(CommentGen(invoke_sub, " Call kernels and "
                                      "communication routines"))
        else:
            invoke_sub.add(CommentGen(invoke_sub, " Call our kernels"))
        invoke_sub.add(CommentGen(invoke_sub, ""))

        # Add content from the schedule
        self.schedule.gen_code(invoke_sub)

        # Deallocate any basis arrays
        self.evaluators.deallocate(invoke_sub)

        invoke_sub.add(CommentGen(invoke_sub, ""))

        # finally, add me to my parent
        parent.add(invoke_sub)


class DynInvokeSchedule(InvokeSchedule):
    ''' The Dynamo specific InvokeSchedule sub-class. This passes the Dynamo-
    specific factories for creating kernel and infrastructure calls
    to the base class so it creates the ones we require. '''

    def __init__(self, arg):
        from psyclone.dynamo0p3_builtins import DynBuiltInCallFactory
        InvokeSchedule.__init__(self, DynKernCallFactory,
                                DynBuiltInCallFactory, arg)

    def node_str(self, colour=True):
        ''' Creates a text summary of this node.

        :param bool colour: whether or not to include control codes for colour.

        :returns: text summary of this node, optionally with control codes \
                  for colour highlighting.
        :rtype: str

        '''
        return (self.coloured_name(colour) + "[invoke='" + self.invoke.name +
                "', dm=" + str(Config.get().distributed_memory)+"]")


class DynGlobalSum(GlobalSum):
    '''
    Dynamo specific global sum class which can be added to and
    manipulated in, a schedule.

    :param scalar: the kernel argument for which to perform a global sum.
    :type scalar: :py:class:`psyclone.dynamo0p3.DynKernelArgument`
    :param parent: the parent node of this node in the PSyIR
    :type parent: :py:class:`psyclone.psyGen.Node`

    :raises GenerationError: if distributed memory is not enabled.
    :raises GenerationError: if the scalar is not of type gh_real.
    '''
    def __init__(self, scalar, parent=None):
        if not Config.get().distributed_memory:
            raise GenerationError("It makes no sense to create a DynGlobalSum "
                                  "object when dm=False")
        # a list of scalar types that this class supports
        self._supported_scalars = ["gh_real"]
        if scalar.type not in self._supported_scalars:
            raise GenerationError(
                "DynGlobalSum currently only supports '{0}', but found '{1}'. "
                "Error found in Kernel '{2}', argument '{3}'".
                format(self._supported_scalars, scalar.type,
                       scalar.call.name, scalar.name))
        super(DynGlobalSum, self).__init__(scalar, parent=parent)

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
    '''Halo exchanges may have more than one dependency. This method
    simplifies multiple dependencies to remove duplicates and any
    obvious redundancy. For example, if one dependency is for depth=1
    and another for depth=2 then we do not need the former as it is
    covered by the latter. Similarly, if we have a depth=extent+1 and
    another for depth=extent+2 then we do not need the former as it is
    covered by the latter. It also takes into account
    needs_clean_outer, which indicates whether the outermost halo
    needs to be clean (and therefore whether there is a dependence).

    :param: a list containing halo access information derived from
    all read fields dependent on this halo exchange
    :type: :func:`list` of :py:class:`psyclone.dynamo0p3.HaloReadAccess`
    :return: a list containing halo depth information derived from
    the halo access information
    :rtype: :func:`list` of :py:class:`psyclone.dynamo0p3.HaloDepth`

    '''
    depth_info_list = []
    # first look to see if all field dependencies are
    # annexed_only. If so we only care about annexed dofs
    annexed_only = True
    for halo_info in halo_info_list:
        if not (halo_info.annexed_only or
                (halo_info.literal_depth == 1
                 and not halo_info.needs_clean_outer)):
            # There are two cases when we only care about accesses to
            # annexed dofs. 1) when annexed_only is set and 2) when
            # the halo depth is 1 but we only depend on annexed dofs
            # being up-to-date (needs_clean_outer is False)
            annexed_only = False
            break
    if annexed_only:
        depth_info = HaloDepth()
        depth_info.set_by_value(max_depth=False, var_depth="",
                                literal_depth=1, annexed_only=True,
                                max_depth_m1=False)
        return [depth_info]
    # next look to see if one of the field dependencies specifies
    # a max_depth access. If so the whole halo region is accessed
    # so we do not need to be concerned with other accesses.
    max_depth_m1 = False
    for halo_info in halo_info_list:
        if halo_info.max_depth:
            if halo_info.needs_clean_outer:
                # found a max_depth access so we only need one
                # HaloDepth entry
                depth_info = HaloDepth()
                depth_info.set_by_value(max_depth=True, var_depth="",
                                        literal_depth=0, annexed_only=False,
                                        max_depth_m1=False)
                return [depth_info]
            # remember that we found a max_depth-1 access
            max_depth_m1 = True

    if max_depth_m1:
        # we have at least one max_depth-1 access.
        depth_info = HaloDepth()
        depth_info.set_by_value(max_depth=False, var_depth="",
                                literal_depth=0, annexed_only=False,
                                max_depth_m1=True)
        depth_info_list.append(depth_info)

    for halo_info in halo_info_list:
        # go through the halo information associated with each
        # read dependency, skipping any max_depth-1 accesses
        if halo_info.max_depth and not halo_info.needs_clean_outer:
            continue
        var_depth = halo_info.var_depth
        literal_depth = halo_info.literal_depth
        if literal_depth and not halo_info.needs_clean_outer:
            # decrease depth by 1 if we don't care about the outermost
            # access
            literal_depth -= 1
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
            # no matches were found with existing entries so
            # create a new one
            depth_info = HaloDepth()
            depth_info.set_by_value(max_depth=False, var_depth=var_depth,
                                    literal_depth=literal_depth,
                                    annexed_only=False, max_depth_m1=False)
            depth_info_list.append(depth_info)
    return depth_info_list


class DynHaloExchange(HaloExchange):

    '''Dynamo specific halo exchange class which can be added to and
    manipulated in a schedule.

    :param field: the field that this halo exchange will act on
    :type field: :py:class:`psyclone.dynamo0p3.DynKernelArgument`
    :param check_dirty: optional argument default True indicating \
    whether this halo exchange should be subject to a run-time check \
    for clean/dirty halos.
    :type check_dirty: bool
    :param vector_index: optional vector index (default None) to \
    identify which index of a vector field this halo exchange is \
    responsible for
    :type vector_index: int
    :param parent: optional PSyIRe parent node (default None) of this \
    object
    :type parent: :py:class:`psyclone.psyGen.node`

    '''
    def __init__(self, field, check_dirty=True,
                 vector_index=None, parent=None):
        HaloExchange.__init__(self, field, check_dirty=check_dirty,
                              vector_index=vector_index, parent=parent)
        # set up some defaults for this class
        self._halo_exchange_name = "halo_exchange"

    def _compute_stencil_type(self):
        '''Dynamically work out the type of stencil required for this halo
        exchange as it could change as transformations are applied to
        the schedule. If all stencil accesses are of the same type then we
        return that stencil, otherwise we return the "region" stencil
        type (as it is safe for all stencils).

        :return: the type of stencil required for this halo exchange
        :rtype: str

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
        schedule.

        :return: the halo exchange depth as a Fortran string
        :rtype: str

        '''
        # get information about reading from the halo from all read fields
        # dependendent on this halo exchange
        depth_info_list = self._compute_halo_read_depth_info()

        # if there is only one entry in the list we can just return
        # the depth
        if len(depth_info_list) == 1:
            return str(depth_info_list[0])
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

        :return: a list containing halo depth information derived from \
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

        :return: a HaloWriteAccess object containing the required \
        information, or None if no dependence information is found.
        :rtype: :py:class:`psyclone.dynamo0p3.HaloWriteAccess` or None
        :raises GenerationError: if more than one write dependence is \
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
        True), might be required (True, False) or is definitely not
        required (False, *). The first return argument is used to
        decide whether a halo exchange should exist. If it is True
        then the halo is required or might be required. If it is False
        then the halo exchange is definitely not required. The second
        argument is used to specify whether we definitely know that it
        is required or are not sure.

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

        :return: Returns (x, y) where x specifies whether this halo \
        exchange is (or might be) required - True, or is not required \
        - False. If the first tuple item is True then the second \
        argument specifies whether we definitely know that we need the \
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

        if Config.get().api_conf("dynamo0.3").compute_annexed_dofs and \
           len(required_clean_info) == 1 and \
           required_clean_info[0].annexed_only:
            # We definitely don't need the halo exchange as we
            # only read annexed dofs and these are always clean as
            # they are computed by default when iterating over
            # dofs and kept up-to-date by redundant computation
            # when iterating over cells.
            required = False
            known = True  # redundant information as it is always known
            return required, known

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

    def node_str(self, colour=True):
        ''' Creates a text summary of this HaloExchange node.

        :param bool colour: whether or not to include control codes for colour.

        :returns: text summary of this node, optionally with control codes \
                  for colour highlighting.
        :rtype: str

        '''
        _, known = self.required()
        runtime_check = not known
        field_id = self._field.name
        if self.vector_index:
            field_id += "({0})".format(self.vector_index)
        return ("{0}[field='{1}', type='{2}', depth={3}, "
                "check_dirty={4}]".format(self.coloured_name(colour), field_id,
                                          self._compute_stencil_type(),
                                          self._compute_halo_depth(),
                                          runtime_check))

    def gen_code(self, parent):
        '''Dynamo specific code generation for this class.

        :param parent: an f2pygen object that will be the parent of \
        f2pygen objects created in this method
        :type parent: :py:class:`psyclone.f2pygen.BaseGen`

        '''
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
                "%" + self._halo_exchange_name +
                "(depth=" + self._compute_halo_depth() + ")"))
        parent.add(CommentGen(parent, ""))


class DynHaloExchangeStart(DynHaloExchange):
    '''The start of an asynchronous halo exchange. This is similar to a
    regular halo exchange except that the Fortran name of the call is
    different and the routine only reads the data being transferred
    (the associated field is specified as having a read access). As a
    result this class is not able to determine some important
    properties (such as whether the halo exchange is known to be
    required or not). This is solved by finding the corresponding
    asynchronous halo exchange end (a halo exchange start always has a
    corresponding halo exchange end and vice versa) and calling its
    methods (a halo exchange end is specified as having readwrite
    access to its associated field and therefore is able to determine
    the required properties).

    :param field: the field that this halo exchange will act on
    :type field: :py:class:`psyclone.dynamo0p3.DynKernelArgument`
    :param check_dirty: optional argument (default True) indicating \
    whether this halo exchange should be subject to a run-time check \
    for clean/dirty halos.
    :type check_dirty: bool
    :param vector_index: optional vector index (default None) to \
    identify which component of a vector field this halo exchange is \
    responsible for
    :type vector_index: int
    :param parent: optional PSyIRe parent node (default None) of this \
    object
    :type parent: :py:class:`psyclone.psyGen.node`

    '''
    def __init__(self, field, check_dirty=True,
                 vector_index=None, parent=None):
        DynHaloExchange.__init__(self, field, check_dirty=check_dirty,
                                 vector_index=vector_index, parent=parent)
        # Update the field's access appropriately. Here "gh_read"
        # specifies that the start of a halo exchange only reads
        # the field's data.
        self._field.access = AccessType.READ
        # override appropriate parent class names
        self._halo_exchange_name = "halo_exchange_start"
        self._text_name = "HaloExchangeStart"
        self._colour_map_name = "HaloExchangeStart"

    def _compute_stencil_type(self):
        '''Call the required method in the corresponding halo exchange end
        object. This is done as the field in halo exchange start is
        only read and the dependence analysis beneath this call
        requires the field to be modified.

        :return: Return the type of stencil required for this pair of \
        halo exchanges
        :rtype: str

        '''
        return self._get_hex_end()._compute_stencil_type()

    def _compute_halo_depth(self):
        '''Call the required method in the corresponding halo exchange end
        object. This is done as the field in halo exchange start is
        only read and the dependence analysis beneath this call
        requires the field to be modified.

        :return: Return the halo exchange depth as a Fortran string
        :rtype: str

        '''
        return self._get_hex_end()._compute_halo_depth()

    def required(self):
        '''Call the required method in the corresponding halo exchange end
        object. This is done as the field in halo exchange start is
        only read and the dependence analysis beneath this call
        requires the field to be modified.

        :return: Returns (x, y) where x specifies whether this halo \
        exchange is (or might be) required - True, or is not required \
        - False. If the first tuple item is True then the second \
        argument specifies whether we definitely know that we need the \
        HaloExchange - True, or are not sure - False.
        :rtype: (bool, bool)

        '''
        return self._get_hex_end().required()

    def _get_hex_end(self):
        '''An internal helper routine for this class which finds the halo
        exchange end object corresponding to this halo exchange start
        object or raises an exception if one is not found.

        :return: The corresponding halo exchange end object
        :rtype: :py:class:`psyclone.dynamo0p3.DynHaloExchangeEnd`
        :raises GenerationError: If no matching HaloExchangeEnd is \
        found, or if the first matching haloexchange that is found is \
        not a HaloExchangeEnd

        '''
        # Look at all nodes following this one in schedule order
        # (which is PSyIRe node order)
        for node in self.following():
            if self.sameParent(node) and isinstance(node, DynHaloExchange):
                # Found a following `haloexchange`,
                # `haloexchangestart` or `haloexchangeend` PSyIRe node
                # that is at the same calling hierarchy level as this
                # haloexchangestart
                access = DataAccess(self.field)
                if access.overlaps(node.field):
                    if isinstance(node, DynHaloExchangeEnd):
                        return node
                    raise GenerationError(
                        "Halo exchange start for field '{0}' should match "
                        "with a halo exchange end, but found {1}".format(
                            self.field.name, type(node)))
        # no match has been found which is an error as a halo exchange
        # start should always have a matching halo exchange end that
        # follows it in schedule (PSyIRe sibling) order
        raise GenerationError(
            "Halo exchange start for field '{0}' has no matching halo "
            "exchange end".format(self.field.name))


class DynHaloExchangeEnd(DynHaloExchange):
    '''The end of an asynchronous halo exchange. This is similar to a
    regular halo exchange except that the Fortran name of the call is
    different and the routine only writes to the data being
    transferred.

    :param field: the field that this halo exchange will act on
    :type field: :py:class:`psyclone.dynamo0p3.DynKernelArgument`
    :param check_dirty: optional argument (default True) indicating \
    whether this halo exchange should be subject to a run-time check \
    for clean/dirty halos.
    :type check_dirty: bool
    :param vector_index: optional vector index (default None) to \
    identify which index of a vector field this halo exchange is \
    responsible for
    :type vector_index: int
    :param parent: optional PSyIRe parent node (default None) of this \
    object
    :type parent: :py:class:`psyclone.psyGen.node`

    '''
    def __init__(self, field, check_dirty=True,
                 vector_index=None, parent=None):
        DynHaloExchange.__init__(self, field, check_dirty=check_dirty,
                                 vector_index=vector_index, parent=parent)
        # Update field properties appropriately. The associated field is
        # written to. However, a readwrite field access needs to be
        # specified as this is required for the halo exchange logic to
        # work correctly.
        self._field.access = AccessType.READWRITE
        # override appropriate parent class names
        self._halo_exchange_name = "halo_exchange_finish"
        self._text_name = "HaloExchangeEnd"
        self._colour_map_name = "HaloExchangeEnd"


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
        # literal_depth, var_depth and max_depth_m1 have no
        # meaning. max_depth being False does not necessarily mean the
        # full halo depth is not accessed, rather it means that we do
        # not know.
        self._max_depth = False
        # max_depth_m1 specifies whether the full depth of halo
        # (whatever that might be) apart from the outermost level is
        # accessed. If this is set then literal_depth, var_depth and
        # max_depth have no meaning.
        self._max_depth_m1 = False
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
    def max_depth_m1(self):
        '''Returns whether the read to the field is known to access all of the
        halo except the outermost level or not.

        :return: Return True if the read to the field is known to
        access all of the halo except the outermost and False otherwise
        :rtype: bool

        '''
        return self._max_depth_m1

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

    def set_by_value(self, max_depth, var_depth, literal_depth, annexed_only,
                     max_depth_m1):
        '''Set halo depth information directly

        :param bool max_depth: True if the field accesses all of the \
        halo and False otherwise
        :param str var_depth: A variable name specifying the halo \
        access depth, if one exists, and None if not
        :param int literal_depth: The known fixed (literal) halo \
        access depth
        :param bool annexed_only: True if only the halo's annexed dofs \
        are accessed and False otherwise
        :param bool max_depth_m1: True if the field accesses all of \
        the halo but does not require the outermost halo to be correct \
        and False otherwise

        '''
        self._max_depth = max_depth
        self._var_depth = var_depth
        self._literal_depth = literal_depth
        self._annexed_only = annexed_only
        self._max_depth_m1 = max_depth_m1

    def __str__(self):
        '''return the depth of a halo dependency
        as a string'''
        depth_str = ""
        if self.max_depth:
            depth_str += "mesh%get_halo_depth()"
        elif self.max_depth_m1:
            depth_str += "mesh%get_halo_depth()-1"
        else:
            if self.var_depth:
                depth_str += self.var_depth
                if self.literal_depth:
                    depth_str += "+"
            if self.literal_depth:
                depth_str += str(self.literal_depth)
        return depth_str


def halo_check_arg(field, access_types):
    '''
    Support function which performs checks to ensure the first argument
    is a field, that the field is contained within Kernel or Builtin
    call and that the field is accessed in one of the ways specified
    by the second argument. If no error is reported it returns the
    call object containing this argument.

    :param field: the argument object we are checking
    :type field: :py:class:`psyclone.dynamo0p3.DynArgument`
    :param access_types: List of allowed access types.
    :type access_types: List of :py:class:`psyclone.psyGen.AccessType`.
    :return: the call containing the argument object
    :rtype: sub-class of :py:class:`psyclone.psyGen.Kern`

    :raises GenerationError: if the first argument to this function is \
                             the wrong type.
    :raises GenerationError: if the first argument is not accessed in one of \
                    the ways specified by the second argument to the function.
    :raises GenerationError: if the first argument is not contained \
                             within a call object.

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
        api_strings = [access.api_specific_name() for access in access_types]
        raise GenerationError(
            "In HaloInfo class, field '{0}' should be one of {1}, but found "
            "'{2}'".format(field.name, api_strings,
                           field.access.api_specific_name()))
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

    '''
    def __init__(self, field):
        '''
        :param field: the field that we are concerned with
        :type field: :py:class:`psyclone.dynamo0p3.DynArgument`

        '''

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
        call = halo_check_arg(field, AccessType.all_write_accesses())
        # no test required here as all calls exist within a loop
        loop = call.parent.parent
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
        # indicates there is no variable.  the fifth argument for
        # set_by_value indicates whether we only access
        # annexed_dofs. At the moment this is not possible when
        # modifying a field so we always return False. The sixth
        # argument indicates if the depth of access is the
        # maximum-1. This is not possible here so we return False.
        HaloDepth.set_by_value(self, max_depth, None, depth, False, False)


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
        self._needs_clean_outer = None
        self._compute_from_field(field)

    @property
    def needs_clean_outer(self):
        '''Returns False if the reader has a gh_inc access and accesses the
        halo. Otherwise returns True.  Indicates that the outer level
        of halo that has been read does not need to be clean (although
        any annexed dofs do).

        :return: Returns False if the outer layer of halo that is read \
        does not need to be clean and True otherwise.
        :rtype: bool

        '''
        return self._needs_clean_outer

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
        call = halo_check_arg(field, AccessType.all_read_accesses())
        # no test required here as all calls exist within a loop
        loop = call.parent.parent

        # For GH_INC we accumulate contributions into the field being
        # modified. In order to get correct results for owned and
        # annexed dofs, this requires that the fields we are
        # accumulating contributions from have up-to-date values in
        # the halo cell(s). However, we do not need to be concerned
        # with the values of the modified field in the last-level of
        # the halo. This is because we only have enough information to
        # partially compute the contributions in those cells
        # anyway. (If the values of the field being modified are
        # required, at some later point, in that level of the halo
        # then we do a halo swap.)
        self._needs_clean_outer = (
            not (field.access == AccessType.INC
                 and loop.upper_bound_name in ["cell_halo",
                                               "colour_halo"]))
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
        elif loop.upper_bound_name in ["ncells", "nannexed"]:
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
                        self._var_depth = field.stencil.extent_arg.varname
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

        # Pre-initialise the Loop children  # TODO: See issue #440
        self.addchild(Literal("NOT_INITIALISED", parent=self))  # start
        self.addchild(Literal("NOT_INITIALISED", parent=self))  # stop
        self.addchild(Literal("1", parent=self))  # step
        self.addchild(Schedule(parent=self))  # loop body

        # At this stage we don't know what our loop bounds are
        self._lower_bound_name = None
        self._lower_bound_index = None
        self._upper_bound_name = None
        self._upper_bound_halo_depth = None

    def node_str(self, colour=True):
        ''' Creates a text summary of this loop node. We override this
        method from the Loop class because, in Dynamo0.3, the function
        space is now an object and we need to call orig_name on it. We
        also include the upper loop bound as this can now be modified.

        :param bool colour: whether or not to include control codes for colour.

        :returns: text summary of this node, optionally with control codes \
                  for colour highlighting.
        :rtype: str

        '''
        if self._upper_bound_halo_depth:
            upper_bound = "{0}({1})".format(self._upper_bound_name,
                                            self._upper_bound_halo_depth)
        else:
            upper_bound = self._upper_bound_name
        return ("{0}[type='{1}', field_space='{2}', it_space='{3}', "
                "upper_bound='{4}']".format(
                    self.coloured_name(colour),
                    self._loop_type,
                    self._field_space.orig_name,
                    self.iteration_space, upper_bound))

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
            if Config.get().api_conf("dynamo0.3").compute_annexed_dofs \
               and Config.get().distributed_memory \
               and not kern.is_reduction:
                self.set_upper_bound("nannexed")
            else:
                self.set_upper_bound("ndofs")
        else:
            if Config.get().distributed_memory:
                if self._field.type in GH_VALID_OPERATOR_NAMES:
                    # We always compute operators redundantly out to the L1
                    # halo
                    self.set_upper_bound("cell_halo", index=1)
                elif (self.field_space.orig_name in
                      VALID_DISCONTINUOUS_FUNCTION_SPACE_NAMES):
                    # Iterate to ncells for all discontinuous quantities,
                    # including any_discontinuous_space
                    self.set_upper_bound("ncells")
                elif self.field_space.orig_name in CONTINUOUS_FUNCTION_SPACES:
                    # Must iterate out to L1 halo for continuous quantities
                    self.set_upper_bound("cell_halo", index=1)
                elif self.field_space.orig_name in VALID_ANY_SPACE_NAMES:
                    # We don't know whether any_space is continuous or not
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
        '''
        Create the associated Fortran code for the type of lower bound.
        :returns: the Fortran code for the lower bound
        :rtype: str
        :raises GenerationError: if self._lower_bound_name is not "start"
                                 for sequential code.
        :raises GenerationError: if self._lower_bound_name is unrecognised
        '''
        if not Config.get().distributed_memory and \
           self._lower_bound_name != "start":
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

        # We must allow for self._kern being None (as it will be for
        # a built-in).
        if self._kern and self._kern.is_intergrid:
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
        mesh = self._name_space_manager.create_name(
            root_name=mesh_name, context="PSyVars", label=mesh_name)

        if self._upper_bound_name == "ncolours":
            # Loop over colours
            kernels = self.walk(DynKern)
            if not kernels:
                raise InternalError(
                    "Failed to find a kernel within a loop over colours.")
            # Check that all kernels have been coloured. We can't check the
            # number of colours since that is only known at runtime.
            ncolours = kernels[0].ncolours_var
            for kern in kernels:
                if not kern.ncolours_var:
                    raise InternalError(
                        "All kernels within a loop over colours must have been"
                        " coloured but kernel '{0}' has not".format(kern.name))
            return ncolours
        elif self._upper_bound_name == "ncolour":
            # Loop over cells of a particular colour when DM is disabled.
            # We use the same, DM API as that returns sensible values even
            # when running without MPI.
            return "{0}%get_last_edge_cell_per_colour(colour)".format(mesh)
        elif self._upper_bound_name == "colour_halo":
            # Loop over cells of a particular colour when DM is enabled. The
            # LFRic API used here allows for colouring with redundant
            # computation.
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
                    "{1})".format(mesh, append))
        elif self._upper_bound_name in ["ndofs", "nannexed"]:
            if Config.get().distributed_memory:
                if self._upper_bound_name == "ndofs":
                    result = self.field.proxy_name_indexed + "%" + \
                             self.field.ref_name() + "%get_last_dof_owned()"
                else:  # nannexed
                    result = self.field.proxy_name_indexed + "%" + \
                             self.field.ref_name() + "%get_last_dof_annexed()"
            else:
                result = self._kern.undf_name
            return result
        elif self._upper_bound_name == "ncells":
            if Config.get().distributed_memory:
                result = mesh + "%get_last_edge_cell()"
            else:
                result = self.field.proxy_name_indexed + "%" + \
                    self.field.ref_name() + "%get_ncell()"
            return result
        elif self._upper_bound_name == "cell_halo":
            if Config.get().distributed_memory:
                return "{0}%get_last_halo_cell({1})".format(mesh,
                                                            halo_index)
            else:
                raise GenerationError(
                    "'cell_halo' is not a valid loop upper bound for "
                    "sequential/shared-memory code")
        elif self._upper_bound_name == "dof_halo":
            if Config.get().distributed_memory:
                return "{0}%{1}%get_last_dof_halo({2})".format(
                    self.field.proxy_name_indexed, self.field.ref_name(),
                    halo_index)
            else:
                raise GenerationError(
                    "'dof_halo' is not a valid loop upper bound for "
                    "sequential/shared-memory code")
        elif self._upper_bound_name == "inner":
            if Config.get().distributed_memory:
                return "{0}%get_last_inner_cell({1})".format(mesh,
                                                             halo_index)
            else:
                raise GenerationError(
                    "'inner' is not a valid loop upper bound for "
                    "sequential/shared-memory code")
        else:
            raise GenerationError(
                "Unsupported upper bound name '{0}' found in dynloop.upper_"
                "bound_fortran()".format(self._upper_bound_name))

    def unique_fields_with_halo_reads(self):
        ''' Returns all fields in this loop that require at least some
        of their halo to be clean to work correctly. '''

        unique_fields = []
        unique_field_names = []

        for call in self.kernels():
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
        :return: True if the argument reads, or might read from the \
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
        if arg.type in GH_VALID_SCALAR_NAMES:
            # scalars do not have halos
            return False
        if arg.is_operator:
            # operators do not have halos
            return False
        if arg.discontinuous and arg.access in \
                [AccessType.READ, AccessType.READWRITE]:
            # there are no shared dofs so access to inner and ncells are
            # local so we only care about reads in the halo
            return self._upper_bound_name in HALO_ACCESS_LOOP_BOUNDS
        if arg.access in [AccessType.READ, AccessType.INC]:
            # arg is either continuous or we don't know (any_space_x)
            # and we need to assume it may be continuous for
            # correctness
            if self._upper_bound_name in HALO_ACCESS_LOOP_BOUNDS:
                # we read in the halo
                return True
            if self._upper_bound_name in ["ncells", "nannexed"]:
                # we read annexed dofs. Return False if we always
                # compute annexed dofs and True if we don't (as
                # annexed dofs are part of the level 1 halo).
                return not Config.get()\
                                 .api_conf("dynamo0.3").compute_annexed_dofs
            if self._upper_bound_name in ["ndofs"]:
                # argument does not read from the halo
                return False
            # nothing should get to here so raise an exception
            raise GenerationError(
                "Internal error in _halo_read_access. It should not be "
                "possible to get to here. loop upper bound name is '{0}' "
                "and arg '{1}' access is '{2}'.".format(
                    self._upper_bound_name, arg.name,
                    arg.access.api_specific_name()))

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
        for call in self.kernels():
            for arg in call.arguments.args:
                if arg.access in AccessType.all_write_accesses():
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

        :param parent: an f2pygen object that will be the parent of \
        f2pygen objects created in this method
        :type parent: :py:class:`psyclone.f2pygen.BaseGen`
        :raises GenerationError: if a loop over colours is within an \
        OpenMP parallel region (as it must be serial)

        '''
        # Check that we're not within an OpenMP parallel region if
        # we are a loop over colours.
        if self._loop_type == "colours" and self.is_openmp_parallel():
            raise GenerationError("Cannot have a loop over "
                                  "colours within an OpenMP "
                                  "parallel region.")

        # Generate the upper and lower loop bounds
        # TODO: Issue 440. upper/lower_bound_fortran should generate PSyIR
        self.start_expr = Literal(self._lower_bound_fortran(), parent=self)
        self.stop_expr = Literal(self._upper_bound_fortran(), parent=self)

        Loop.gen_code(self, parent)

        if Config.get().distributed_memory and self._loop_type != "colour":

            # Set halo clean/dirty for all fields that are modified
            from psyclone.f2pygen import CallGen, CommentGen, DirectiveGen
            fields = self.unique_modified_args("gh_field")

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


class DynKern(CodedKern):
    ''' Stores information about Dynamo Kernels as specified by the
    Kernel metadata and associated algorithm call. Uses this
    information to generate appropriate PSy layer code for the Kernel
    instance or to generate a Kernel stub'''

    def __init__(self):
        if False:  # pylint: disable=using-constant-test
            self._arguments = DynKernelArguments(None, None)  # for pyreverse
        self._base_name = ""
        self._func_descriptors = None
        self._fs_descriptors = None
        # Whether this kernel requires quadrature
        self._qr_required = False
        # Whether this kernel requires basis functions
        self._basis_required = False
        # What shape of evaluator this kernel requires (if any)
        self._eval_shape = ""
        # The function spaces on which to *evaluate* basis/diff-basis
        # functions if any are required for this kernel. Is a dict with
        # (mangled) FS names as keys and associated kernel argument as value.
        self._eval_targets = OrderedDict()
        self._qr_text = ""
        self._qr_name = None
        self._qr_args = None
        self._name_space_manager = NameSpaceFactory().create()
        self._cma_operation = None
        self._is_intergrid = False  # Whether this is an inter-grid kernel

    def reference_accesses(self, var_accesses):
        '''Get all variable access information. All accesses are marked
        according to the kernel metadata

        :param var_accesses: VariablesAccessInfo instance that stores the\
            information about variable accesses.
        :type var_accesses: \
            :py:class:`psyclone.core.access_info.VariablesAccessInfo`
        '''
        for arg in self.arguments.args:
            if arg.is_scalar():
                var_accesses.add_access(arg.name, arg.access, self)
            else:
                # It's an array, so add an arbitrary index value for the
                # stored indices (which is at this stage the only way to
                # indicate an array access).
                var_accesses.add_access(arg.name, arg.access, self, [1])
        super(DynKern, self).reference_accesses(var_accesses)
        # Set the current location index to the next location, since after
        # this kernel a new statement starts.
        var_accesses.next_location()

    def load(self, call, parent=None):
        '''
        Sets up kernel information with the call object which is
        created by the parser. This object includes information about
        the invoke call and the associated kernel.

        :param call: The KernelCall object from which to extract information
                     about this kernel
        :type call: :py:class:`psyclone.parse.algorithm.KernelCall`
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
        from psyclone.parse.algorithm import Arg
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
                    "found '{1}'".format(GH_VALID_ARG_TYPE_NAMES,
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
        :type args: List of :py:class:`psyclone.parse.algorithm.Arg` objects
        :param parent: the parent of this kernel call in the generated
                       AST (will be a loop object)
        :type parent: :py:class:`psyclone.dynamo0p3.DynLoop`
        '''
        from psyclone.parse.algorithm import KernelCall
        CodedKern.__init__(self, DynKernelArguments,
                           KernelCall(module_name, ktype, args),
                           parent, check=False)
        # Remove "_code" from the name if it exists to determine the
        # base name which (if dynamo0.3 naming conventions are
        # followed) is used as the root for the module and subroutine
        # names.
        if self.name.lower().endswith("_code"):
            self._base_name = self.name[:-5]
        else:
            # TODO: #11 add a warning here when logging is added
            self._base_name = self.name
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
            if qr_arg.varname:
                self._qr_name = self._name_space_manager.create_name(
                    root_name=qr_arg.varname, context="AlgArgs",
                    label=self._qr_text)
            else:
                self._qr_name = ""
            # Dynamo 0.3 api kernels require quadrature rule arguments to be
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
            if qr_arg.varname:
                self._qr_args = [
                    arg + "_" + self._qr_name for arg in self._qr_args]

        elif self._eval_shape == "gh_evaluator":
            # Kernel has an evaluator. If gh_evaluator_targets is present
            # then that specifies the function spaces for which the evaluator
            # is required. Otherwise, the FS of the updated argument(s) tells
            # us upon which nodal points the evaluator will be required
            for fs_name in ktype.eval_targets:
                arg, fspace = self.arguments.get_arg_on_space_name(fs_name)
                # Set up our dict of evaluator targets, one entry per
                # target FS.
                if fspace.mangled_name not in self._eval_targets:
                    self._eval_targets[fspace.mangled_name] = (fspace, arg)

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
    def colourmap(self):
        '''
        Getter for the name of the colourmap associated with this kernel call.

        :return: name of the colourmap (Fortran array)
        :rtype: str
        :raises InternalError: if this kernel is not coloured or the \
                               dictionary of inter-grid kernels and \
                               colourmaps has not been constructed.
        '''
        if not self.is_coloured():
            raise InternalError("Kernel '{0}' is not inside a coloured "
                                "loop.".format(self.name))
        if self._is_intergrid:
            invoke = self.root.invoke
            if self.name not in invoke.meshes.intergrid_kernels:
                raise InternalError(
                    "Colourmap information for kernel '{0}' has not yet "
                    "been initialised".format(self.name))
            cmap = invoke.meshes.intergrid_kernels[self.name].colourmap
        else:
            cmap = self._name_space_manager.create_name(
                root_name="cmap", context="PSyVars", label="cmap")
        return cmap

    @property
    def ncolours_var(self):
        '''
        Getter for the name of the variable holding the number of colours
        associated with this kernel call.

        :return: name of the variable holding the number of colours
        :rtype: str
        :raises InternalError: if this kernel is not coloured or the \
                               colour-map information has not been initialised.
        '''
        if not self.is_coloured():
            raise InternalError("Kernel '{0}' is not inside a coloured "
                                "loop.".format(self.name))
        if self._is_intergrid:
            invoke = self.root.invoke
            if self.name not in invoke.meshes.intergrid_kernels:
                raise InternalError(
                    "Colourmap information for kernel '{0}' has not yet "
                    "been initialised".format(self.name))
            ncols = invoke.meshes.intergrid_kernels[self.name].ncolours_var
        else:
            ncols = self._name_space_manager.create_name(
                root_name="ncolour", context="PSyVars", label="ncolour")
        return ncols

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
        :return: the value of GH_SHAPE for this kernel or an empty string \
                 if none is specified.
        :rtype: str
        '''
        return self._eval_shape

    @property
    def eval_targets(self):
        '''
        :return: the function spaces upon which basis/diff-basis functions \
                 are to be evaluated for this kernel.
        :rtype: dict of (:py:class:`psyclone.dynamo0p3.FunctionSpace`, \
                :py:class`psyclone.dynamo0p3.DynKernelArgument`), indexed by \
                the names of the target function spaces.
        '''
        return self._eval_targets

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
    def base_name(self):
        '''
        :returns: a base name for this kernel.
        :rtype: str
        '''
        return self._base_name

    @property
    def gen_stub(self):
        '''
        Create the fparser1 AST for a kernel stub.

        :returns: root of fparser1 AST for the stub routine.
        :rtype: :py:class:`fparser.one.XXXX`

        '''
        from psyclone.f2pygen import ModuleGen, SubroutineGen, UseGen

        # create an empty PSy layer module
        base_name = self._base_name
        psy_module = ModuleGen(self._base_name+"_mod")

        # Create the subroutine
        sub_stub = SubroutineGen(psy_module, name=self._base_name+"_code",
                                 implicitnone=True)
        sub_stub.add(UseGen(sub_stub, name="constants_mod", only=True,
                            funcnames=["r_def"]))

        # Add all the declarations
        for entities in [DynCellIterators, DynDofmaps, DynFunctionSpaces,
                         DynCMAOperators, DynScalarArgs, DynFields,
                         DynLMAOperators, DynStencils, DynBasisFunctions,
                         DynOrientations, DynBoundaryConditions]:
            entities(self).declarations(sub_stub)

        # Create the arglist
        create_arg_list = KernStubArgList(self)
        create_arg_list.generate()

        # Add the arglist
        sub_stub.args = create_arg_list.arglist

        # Add the subroutine to the parent module
        psy_module.add(sub_stub)
        return psy_module.root

    def gen_code(self, parent):
        '''Generates dynamo version 0.3 specific psy code for a call to
           the dynamo kernel instance.

        :param parent: an f2pygen object that will be the parent of \
                       f2pygen objects created in this method.
        :type parent: :py:class:`psyclone.f2pygen.BaseGen`
        :raises GenerationError: if the loop goes beyond the level 1 \
                                 halo and an operator is accessed.
        :raises GenerationError: if a kernel in the loop has an inc access \
                                 and the loop is not coloured but is within \
                                 an OpenMP parallel region.

        '''
        from psyclone.f2pygen import DeclGen, AssignGen, CommentGen
        parent.add(DeclGen(parent, datatype="integer",
                           entity_decls=["cell"]))

        parent_loop = self.parent.parent

        # Check whether this kernel reads from an operator
        op_args = parent_loop.args_filter(arg_types=GH_VALID_OPERATOR_NAMES,
                                          arg_accesses=[AccessType.READ,
                                                        AccessType.READWRITE])
        if op_args:
            # It does. We must check that our parent loop does not
            # go beyond the L1 halo.
            if parent_loop.upper_bound_name == "cell_halo" and \
               parent_loop.upper_bound_halo_depth > 1:
                raise GenerationError(
                    "Kernel '{0}' reads from an operator and therefore "
                    "cannot be used for cells beyond the level 1 halo. "
                    "However the containing loop goes out to level {1}".
                    format(self._name, parent_loop.upper_bound_halo_depth))

        # If this kernel is being called from within a coloured
        # loop then we have to look-up the name of the colour map
        if self.is_coloured():
            # TODO Check whether this arg is gh_inc and if not, Warn that
            # we're colouring a kernel that has no field object with INC access

            # We must look-up the cell index using the colour map rather than
            # use the current cell index directly. We need to know the name
            # of the variable holding the colour map for this kernel.
            cell_index = self.colourmap + "(colour, cell)"
        else:
            # This kernel call has not been coloured
            #  - is it OpenMP parallel, i.e. are we a child of
            # an OpenMP directive?
            if self.is_openmp_parallel():
                try:
                    # It is OpenMP parallel - does it have an argument
                    # with INC access?
                    arg = self.incremented_arg()
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

        # Orientation array lookup is done for each cell
        oname = ""
        for unique_fs in self.arguments.unique_fss:
            if self._fs_descriptors.exists(unique_fs):
                fs_descriptor = self._fs_descriptors.get_descriptor(unique_fs)
                if fs_descriptor.requires_orientation:
                    field = self.arguments.get_arg_on_space(unique_fs)
                    oname = get_fs_orientation_name(unique_fs)
                    parent.add(
                        AssignGen(parent, pointer=True,
                                  lhs=oname,
                                  rhs=field.proxy_name_indexed + "%" +
                                  field.ref_name(unique_fs) +
                                  "%get_cell_orientation(" +
                                  cell_index + ")"))
        if oname:
            parent.add(CommentGen(parent, ""))

        super(DynKern, self).gen_code(parent)


class ArgOrdering(object):
    '''Base class capturing the arguments, type and ordering of data in
    a Kernel call.'''
    def __init__(self, kern):
        self._kern = kern
        self._generate_called = False

    def generate(self):
        '''
        Specifies which arguments appear in an argument list, their type
        and their ordering. Calls methods for each type of argument
        that can be specialised by a child class for its particular need.

        :raises GenerationError: if the kernel arguments break the
                                 rules for the Dynamo 0.3 API.
        '''
        self._generate_called = True
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
            elif arg.type in GH_VALID_SCALAR_NAMES:
                self.scalar(arg)
            else:
                raise GenerationError(
                    "Unexpected arg type found in dynamo0p3.py:"
                    "ArgOrdering:generate(). Expected one of '{0}' "
                    "but found '{1}'".format(GH_VALID_ARG_TYPE_NAMES,
                                             arg.type))
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
            if op_arg.access != AccessType.READWRITE:
                raise GenerationError(
                    "Kernel {0} is recognised as a kernel which applies "
                    "boundary conditions to an operator. However its operator "
                    "argument has access {1} rather than gh_readwrite.".
                    format(self._kern.name, op_arg.access.api_specific_name()))
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

    def indirection_dofmap(self, function_space, operator=None):
        '''
        Add indirection dofmap required when applying a CMA operator

        :raises NotImplementedError: because this is an abstract method
        '''
        raise NotImplementedError("Error: ArgOrdering.indirection_dofmap() "
                                  "must be implemented by subclass")


class KernCallArgList(ArgOrdering):
    '''Creates the argument list required to call kernel "kern" from the
    PSy-layer and captures the positions of the following arguments in
    the argument list: nlayers, number of quadrature points and number
    of degrees of freedom. The ordering and type of arguments is
    captured by the base class.

    :param kern: The kernel that is being called.
    :type kern: :py:class:`psyclone.dynamo0p3.DynKern`

    '''
    NdfInfo = namedtuple("NdfInfo", ["position", "function_space"])

    def __init__(self, kern):
        ArgOrdering.__init__(self, kern)
        self._arglist = []
        self._name_space_manager = NameSpaceFactory().create()
        self._nlayers_positions = []
        self._nqp_positions = []
        self._ndf_positions = []

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
        self._arglist.append("{0}(:,{1})".format(map_name,
                                                 self._cell_ref_name))
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
        self._nlayers_positions.append(len(self._arglist))

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
        '''
        Add stencil information to the argument list associated with the
        argument 'arg' if the extent is unknown.

        :param arg: the kernel argument with which the stencil is associated.
        :type arg: :py:class:`psyclone.dynamo0p3.DynKernelArgument`

        '''
        # The extent is not specified in the metadata so pass the value in
        name = DynStencils.dofmap_size_name(arg)
        self._arglist.append(name)

    def stencil_unknown_direction(self, arg):
        '''
        Add stencil information to the argument list associated with the
        argument 'arg' if the direction is unknown (i.e. it's being supplied
        in a variable).

        :param arg: the kernel argument with which the stencil is associated.
        :type arg: :py:class:`psyclone.dynamo0p3.DynKernelArgument`

        '''
        # the direction of the stencil is not known so pass the value in
        name = arg.stencil.direction_arg.varname
        self._arglist.append(name)

    def stencil(self, arg):
        '''
        Add general stencil information associated with the argument 'arg'
        to the argument list.

        :param arg: the kernel argument with which the stencil is associated.
        :type arg: :py:class:`psyclone.dynamo0p3.DynKernelArgument`

        '''
        # add in stencil dofmap
        var_name = DynStencils.dofmap_name(arg)
        name = var_name + "(:,:," + self._cell_ref_name + ")"
        self._arglist.append(name)

    def operator(self, arg):
        ''' add the operator arguments to the argument list '''
        # TODO we should only be including ncell_3d once in the argument
        # list but this adds it for every operator
        self._arglist.append(arg.proxy_name_indexed+"%ncell_3d")
        self._arglist.append(arg.proxy_name_indexed+"%local_stencil")

    def cma_operator(self, arg):
        '''
        Add the CMA operator and associated scalars to the argument
        list.

        :param arg: the CMA operator argument.
        :type arg: :py:class:`psyclone.dynamo0p3.DynKernelArgument`

        '''
        components = ["matrix"]
        if arg.function_space_to.orig_name != \
           arg.function_space_from.orig_name:
            components += DynCMAOperators.cma_diff_fs_params
        else:
            components += DynCMAOperators.cma_same_fs_params
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
        self._ndf_positions.append(
            KernCallArgList.NdfInfo(position=len(self._arglist),
                                    function_space=function_space.orig_name))

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

        :param function_space: the function space for which the basis \
                               function is required.
        :type function_space: :py:class:`psyclone.dynamo0p3.FunctionSpace`
        '''
        if self._kern.qr_required:
            basis_name = get_fs_basis_name(function_space,
                                           qr_var=self._kern.qr_name)
            self._arglist.append(basis_name)
        else:
            # We are dealing with an evaluator and therefore need as many
            # basis functions as there are target function spaces.
            for fs_name in self._kern.eval_targets:
                # The associated FunctionSpace object is the first item in
                # the tuple dict entry associated with the name of the target
                # function space
                fspace = self._kern.eval_targets[fs_name][0]
                basis_name = get_fs_basis_name(function_space,
                                               qr_var=self._kern.qr_name,
                                               on_space=fspace)
                self._arglist.append(basis_name)

    def diff_basis(self, function_space):
        '''
        Add differential basis information for the function space to the
        argument list.

        :param function_space: the function space for which the differential
                               basis functions are required
        :type function_space: :py:class:`psyclone.dynamo0p3.FunctionSpace`
        '''
        if self._kern.qr_required:
            diff_basis_name = get_fs_diff_basis_name(
                function_space, qr_var=self._kern.qr_name)
            self._arglist.append(diff_basis_name)
        else:
            # We are dealing with an evaluator and therefore need as many
            # basis functions as there are target function spaces.
            for fs_name in self._kern.eval_targets:
                # The associated FunctionSpace object is the first item in
                # the tuple dict entry associated with the name of the target
                # function space
                fspace = self._kern.eval_targets[fs_name][0]
                diff_basis_name = get_fs_diff_basis_name(
                    function_space, qr_var=self._kern.qr_name, on_space=fspace)
                self._arglist.append(diff_basis_name)

    def orientation(self, function_space):
        '''add orientation information for this function space to the
        argument list'''
        orientation_name = get_fs_orientation_name(function_space)
        self._arglist.append(orientation_name)

    def field_bcs_kernel(self, function_space):
        '''
        Implement the boundary_dofs array fix for a field.

        :param function_space: unused argument.
        '''
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
        base_name = "boundary_dofs_" + farg.name
        name = self._name_space_manager.create_name(root_name=base_name,
                                                    context="PSyVars",
                                                    label=base_name)
        self._arglist.append(name)

    def operator_bcs_kernel(self, _):
        '''
        Supply necessary additional arguments for the kernel that
        applies boundary conditions to a LMA operator. 2nd (unused)
        argument is for consistency with base class.

        '''
        # This kernel has only a single LMA operator as argument.
        # Checks for this are performed in ArgOrdering.generate()
        op_arg = self._kern.arguments.args[0]
        base_name = "boundary_dofs_"+op_arg.name
        name = self._name_space_manager.create_name(root_name=base_name,
                                                    context="PSyVars",
                                                    label=base_name)
        self._arglist.append(name)

    def quad_rule(self):
        ''' add qr information to the argument list'''
        # At the moment we only support XYoZ quadrature which requires
        # a number of quadrature points in the horizontal and
        # vertical.

        self._nqp_positions.append({"horizontal": len(self._arglist) + 1,
                                    "vertical": len(self._arglist) + 2})
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
    def nlayers_positions(self):
        '''
        :return: the position(s) in the argument list of the \
        variable(s) that passes the number of layers. The generate \
        method must be called first.
        :rtype: list of int.

        :raises InternalError: if the generate() method has not been
        called.

        '''
        if not self._generate_called:
            raise InternalError(
                "KernCallArgList: the generate() method should be called "
                "before the nlayers_positions() method")
        return self._nlayers_positions

    @property
    def nqp_positions(self):
        '''
        :return: the positions in the argument list of the variables that \
        pass the number of quadrature points. The number and type of \
        these will change depending on the type of quadrature. A list \
        of dictionaries is returned with the quadrature directions \
        being the keys to the dictionaries and their position in the \
        argument list being the values. At the moment only XYoZ is \
        supported (which has horizontal and vertical quadrature \
        points). The generate method must be called first.
        :rtype: [{str: int, ...}]

        :raises InternalError: if the generate() method has not been \
        called.

        '''
        if not self._generate_called:
            raise InternalError(
                "KernCallArgList: the generate() method should be called "
                "before the nqp_positions() method")
        return self._nqp_positions

    @property
    def ndf_positions(self):
        '''
        :return: the position(s) in the argument list and the function \
        space(s) associated with the variable(s) that pass(es) the \
        number of degrees of freedom for the function space. The \
        generate method must be called first.
        :rtype: list of namedtuple (position=int, function_space=str).

        :raises InternalError: if the generate() method has not been \
        called.

        '''
        if not self._generate_called:
            raise InternalError(
                "KernCallArgList: the generate() method should be called "
                "before the ndf_positions() method")
        return self._ndf_positions

    @property
    def arglist(self):
        '''
        :return: the kernel argument list. The generate method must be \
        called first.
        :rtype: list of str.

        :raises InternalError: if the generate() method has not been \
        called.

        '''
        if not self._generate_called:
            raise InternalError(
                "KernCallArgList: the generate() method should be called "
                "before the arglist() method")
        return self._arglist

    @property
    def _cell_ref_name(self):
        '''
        Utility routine which determines whether to return the cell value
        or the colourmap lookup value.

        :returns: the Fortran code needed to access the current cell index.
        :rtype: str
        '''
        if self._kern.is_coloured():
            return self._kern.colourmap + "(colour, cell)"
        return "cell"


class KernStubArgList(ArgOrdering):
    '''Creates the argument list required to create and declare the
    required arguments for a kernel subroutine.  The ordering and type
    of the arguments is captured by the base class '''
    def __init__(self, kern):
        '''
        :param kern: Kernel for which to create argument list
        :type kern: :py:class:`psyclone.dynamo0p3.DynKern`

        :raises NotImplementedError: if kernel is inter-grid
        '''

        # We don't yet support inter-grid kernels (Issue #162)
        if kern.is_intergrid:
            raise NotImplementedError(
                "Kernel {0} is an inter-grid kernel and stub generation "
                "is not yet supported for inter-grid kernels".
                format(kern.name))

        self._first_arg = True
        self._first_arg_decl = None
        ArgOrdering.__init__(self, kern)
        self._arglist = []
        self._name_space_manager = NameSpaceFactory().create()

    def cell_position(self):
        ''' Add cell position to the argument list. '''
        self._arglist.append("cell")

    def mesh_height(self):
        ''' Add mesh height (nlayers) to the argument list. '''
        self._arglist.append("nlayers")

    def mesh_ncell2d(self):
        ''' Add the number of columns in the mesh to the argument list. '''
        self._arglist.append("ncell_2d")

    def field_vector(self, argvect):
        '''Add the field vector associated with the argument 'argvect' to the
        argument list.

        :param argvect: the corresponding kernel argument.
        :type argvect:  :py:class:`psyclone.dynamo0p3.DynKernelArgument`
        '''
        # the range function below returns values from
        # 1 to the vector size which is what we
        # require in our Fortran code
        for idx in range(1, argvect.vector_size+1):
            text = (argvect.name + "_" +
                    argvect.function_space.mangled_name +
                    "_v" + str(idx))
            if self._first_arg:
                self._first_arg = False
            self._arglist.append(text)

    def field(self, arg):
        '''
        Add the field associated with the argument 'arg' to the argument list.

        :param arg: the kernel argument (field).
        :type arg: :py:class:`psyclone.dynamo0p3.DynKernelArgument`
        '''
        text = arg.name + "_" + arg.function_space.mangled_name
        self._arglist.append(text)

    def stencil_unknown_extent(self, arg):
        '''
        Add stencil information associated with a kernel argument if the
        extent is unknown.
        :param arg: the meta-data description of the kernel argument with \
                    which the stencil is associated.
        :type arg: :py:class:`psyclone.dynamo0p3.DynKernelArgument`
        '''
        name = DynStencils.dofmap_size_name(arg)
        self._arglist.append(name)

    def stencil_unknown_direction(self, arg):
        '''
        Add stencil information associated with the argument 'arg' if the
        direction is unknown.

        :param arg: the kernel argument with which the stencil is associated.
        :type arg: :py:class:`psyclone.dynamo0p3.DynKernelArgument`

        '''
        self._arglist.append(DynStencils.direction_name(arg))

    def stencil(self, arg):
        '''
        Add general stencil information associated with a kernel argument.

        :param arg: the meta-data description of the kernel argument with \
                    which the stencil is associated.
        :type arg: :py:class:`psyclone.dynamo0p3.DynKernelArgument`
        '''
        self._arglist.append(DynStencils.dofmap_name(arg))

    def operator(self, arg):
        ''' add the operator arguments to the argument list '''
        size = arg.name + "_ncell_3d"
        self._arglist.append(size)
        # If this is the first argument in the kernel then keep a
        # note so that we can put subsequent declarations in the
        # correct location
        if self._first_arg:
            self._first_arg = False
        text = arg.name
        self._arglist.append(text)

    def cma_operator(self, arg):
        ''' add the CMA operator arguments to the argument list '''
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

        if self._first_arg:
            self._first_arg = False

    def banded_dofmap(self, function_space):
        ''' Declare the banded dofmap required for a CMA operator
        that maps to/from the specified function space '''
        dofmap = get_cbanded_map_name(function_space)
        self._arglist.append(dofmap)

    def indirection_dofmap(self, function_space, operator=None):
        '''
        Declare the indirection dofmaps required when applying a
        CMA operator.

        :param function_space: the function space for which the dofmap \
                               is required.
        :type function_space: :py:class:`psyclone.dynamo0p3.FunctionSpace`
        :param operator: the CMA operator for which the dofmap is required.
        :type operator: :py:class:`psyclone.dynamo0p3.DynKernelArgument`

        :raises GenerationError: if no kernel argument is supplied.
        :raises GenerationError: if the supplied kernel argument is not a \
                                 CMA operator.

        '''
        if not operator:
            raise GenerationError("Internal error: no CMA operator supplied.")
        if operator.type != "gh_columnwise_operator":
            raise GenerationError(
                "Internal error: a CMA operator (gh_columnwise_operator) must "
                "be supplied but got {0}".format(operator.type))
        map_name = get_cma_indirection_map_name(function_space)
        self._arglist.append(map_name)

    def scalar(self, arg):
        '''
        Add the name associated with the scalar argument to the argument list.

        :param arg: the kernel argument.
        :type arg: :py:class:`psyclone.dynamo0p3.DynKernelArgument`

        :raises InternalError: if the argument is not a recognised scalar type.
        '''
        if arg.type not in GH_VALID_SCALAR_NAMES:
            raise InternalError(
                "Expected argument type to be one of '{0}' but got '{1}'".
                format(GH_VALID_SCALAR_NAMES, arg.type))
        self._arglist.append(arg.name)

    def fs_common(self, function_space):
        ''' Provide arguments common to LMA operators and
        fields on a space. There is one: "ndf". '''
        ndf_name = get_fs_ndf_name(function_space)
        self._arglist.append(ndf_name)

    def fs_compulsory_field(self, function_space):
        ''' Provide compulsory arguments if there is a field on this
        function space'''
        undf_name = get_fs_undf_name(function_space)
        self._arglist.append(undf_name)
        map_name = get_fs_map_name(function_space)
        self._arglist.append(map_name)

    def basis(self, function_space):
        '''
        Add the necessary declarations for basis function(s) on the supplied
        function space. There can be more than one if this is an evaluator.

        :param function_space: the function space for which to provide \
                               the basis functions
        :type function_space: :py:class:`psyclone.dynamo0p3.FunctionSpace`

        :raises InternalError: if the evaluator shape is not recognised.

        '''
        if self._kern.eval_shape in VALID_QUADRATURE_SHAPES:
            basis_name = get_fs_basis_name(function_space)
            self._arglist.append(basis_name)
        elif self._kern.eval_shape in VALID_EVALUATOR_SHAPES:
            # Need a basis array for each target space upon which the basis
            # functions have been evaluated. _kern.eval_targets is a dict
            # where the values are 2-tuples of (FunctionSpace, argument).
            for _, target in self._kern.eval_targets.items():
                basis_name = get_fs_basis_name(function_space,
                                               on_space=target[0])
                self._arglist.append(basis_name)
        else:
            raise InternalError(
                "Unrecognised evaluator shape ({0}). Expected one of: {1}".
                format(self._kern.eval_shape, VALID_EVALUATOR_SHAPES))

    def diff_basis(self, function_space):
        '''
        Provide the necessary declarations for the differential basis function
        on the supplied function space.

        :param function_space: the function space for which to provide the \
                               differential basis function
        :type function_space: :py:class:`psyclone.dynamo0p3.FunctionSpace`

        :raises GenerationError: if the evaluator shape is not recognised.
        '''
        if self._kern.eval_shape in VALID_QUADRATURE_SHAPES:
            # We need differential basis functions for quadrature
            diff_basis_name = get_fs_diff_basis_name(function_space)
            self._arglist.append(diff_basis_name)

        elif self._kern.eval_shape in VALID_EVALUATOR_SHAPES:
            # We need differential basis functions for an evaluator,
            # potentially for multiple target spaces. _kern.eval_targets is
            # a dict where the values are 2-tuples of
            # (FunctionSpace, argument).
            for _, target in self._kern.eval_targets.items():
                diff_basis_name = get_fs_diff_basis_name(function_space,
                                                         on_space=target[0])
                self._arglist.append(diff_basis_name)
        else:
            raise GenerationError(
                "Internal error: unrecognised evaluator shape ({0}). Expected "
                "one of: {1}".format(self._kern.eval_shape,
                                     VALID_EVALUATOR_SHAPES))

    def orientation(self, function_space):
        '''
        Provide orientation information for the function space.

        :param function_space: the function space for which orientation \
                               is required.
        :type function_space: :py:class:`psyclone.dynamo0p3.FunctionSpace`

        '''
        orientation_name = get_fs_orientation_name(function_space)
        self._arglist.append(orientation_name)

    def field_bcs_kernel(self, function_space):
        ''' implement the boundary_dofs array fix for fields '''
        arg = self._kern.arguments.get_arg_on_space(function_space)
        self._arglist.append("boundary_dofs_"+arg.name)

    def operator_bcs_kernel(self, function_space):
        ''' Implement the boundary_dofs array fix for operators. This is the
        same as for fields with the function space set to the 'to' space of
        the operator. '''
        self.field_bcs_kernel(function_space)

    def quad_rule(self):
        ''' provide quadrature information for this kernel stub (necessary
        arguments and declarations) '''
        self._arglist.extend(self._kern.qr_args)

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
    meta-data.
    #TODO #274 this should actually be named something like
    BasisFuncDescriptors as it holds information describing the
    basis/diff-basis functions required by a kernel.

    :param descriptors: list of objects describing the basis/diff-basis \
                        functions required by a kernel, as obtained from \
                        meta-data.
    :type descriptors: list of :py:class:`psyclone.DynFuncDescriptor03`.

    '''
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
    :type call: :py:class:`psyclone.parse.algorithm.KernelCall`
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
    '''
    Provides information about Dynamo kernel call arguments
    collectively, as specified by the kernel argument metadata.

    :param call: the kernel meta-data for which to extract argument info.
    :type call: :py:class:`psyclone.parse.KernelCall`
    :param parent_call: the kernel-call object.
    :type parent_call: :py:class:`psyclone.dynamo0p3.DynKern`

    :raises GenerationError: if the kernel meta-data specifies stencil extent.
    '''
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
                # Create a stencil object and store a reference to it in our
                # new DynKernelArgument object.
                stencil = DynStencil(dyn_argument.descriptor.stencil['type'])
                dyn_argument.stencil = stencil

                if dyn_argument.descriptor.stencil['extent']:
                    raise GenerationError("extent metadata not yet supported")
                    # if supported we would add the following
                    # line. However, note there is currently no setter
                    # for extent in DynStencil so this would need to
                    # be added.  stencil.extent =
                    # dyn_argument.descriptor.stencil['extent']
                else:
                    # An extent argument has been added.
                    stencil.extent_arg = call.args[idx]
                    idx += 1
                if dyn_argument.descriptor.stencil['type'] == 'xory1d':
                    # a direction argument has been added
                    stencil.direction_arg = call.args[idx]
                    idx += 1
            self._args.append(dyn_argument)

        # We have now completed the construction of the kernel arguments so
        # we can go back and update the names of any stencil size and/or
        # direction variable names to ensure there are no clashes.
        for arg in self._args:
            if not arg.descriptor.stencil:
                continue
            if not arg.stencil.extent_arg.is_literal():
                if arg.stencil.extent_arg.varname:
                    # Ensure extent argument name is registered with
                    # namespace manager
                    root = arg.stencil.extent_arg.varname
                    new_name = self._name_space_manager.create_name(
                        root_name=root, context="AlgArgs",
                        label=arg.stencil.extent_arg.text)
                    arg.stencil.extent_arg.varname = new_name
            if arg.descriptor.stencil['type'] == 'xory1d':
                # a direction argument has been added
                if arg.stencil.direction_arg.varname and \
                   arg.stencil.direction_arg.varname not in \
                   VALID_STENCIL_DIRECTIONS:
                    # Register the name of the direction argument to ensure
                    # it is unique in the PSy layer
                    root = arg.stencil.direction_arg.varname
                    unique_name = self._name_space_manager.create_name(
                        root_name=root, context="AlgArgs",
                        label=arg.stencil.direction_arg.text)
                    arg.stencil.direction_arg.varname = unique_name

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
        the named function space, as specified in the kernel metadata. Also
        returns the associated FunctionSpace object.

        :param str func_space_name: Name of the function space (as specified \
                                    in kernel meta-data) for which to \
                                    find an argument.
        :return: the first kernel argument that is on the named function \
                 space and the associated FunctionSpace object.
        :rtype: (:py:class:`psyclone.dynamo0p3.DynKernelArgument`,
                 :py:class:`psyclone.dynamo0p3.FunctionSpace`)
        :raises: FieldNotFoundError if no field or operator argument is found \
                 for the named function space.
        '''
        for arg in self._args:
            for function_space in arg.function_spaces:
                if function_space:
                    if func_space_name == function_space.orig_name:
                        return arg, function_space
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
        if op_type and op_type not in GH_VALID_OPERATOR_NAMES:
            raise GenerationError(
                "If supplied, op_type must be a valid operator type (one "
                "of {0}) but got {1}".format(GH_VALID_OPERATOR_NAMES, op_type))
        if not op_type:
            # If no operator type is specified then we match any type
            op_list = GH_VALID_OPERATOR_NAMES
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

    def iteration_space_arg(self):
        '''
        Returns an argument we can use to dereference the iteration
        space. This can be a field or operator that is modified or
        alternatively a field that is read if one or more scalars
        are modified. If a kernel writes to more than one argument then
        that requiring the largest iteration space is selected.

        :return: Kernel argument from which to obtain iteration space
        :rtype: :py:class:`psyclone.dynamo0p3.DynKernelArgument`
        '''

        # Since we always compute operators out to the L1 halo we first
        # check whether this kernel writes to an operator
        write_accesses = AccessType.all_write_accesses()
        op_args = psyGen.args_filter(self._args,
                                     arg_types=GH_VALID_OPERATOR_NAMES,
                                     arg_accesses=write_accesses)
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
        # finally we try all discontinuous function spaces including
        # any_discontinuous_space. We do this because if a quantity on
        # a continuous FS is modified then our iteration space must be
        # larger (include L1-halo cells)
        write_accesses = AccessType.all_write_accesses()
        fld_args = psyGen.args_filter(self._args,
                                      arg_types=["gh_field"],
                                      arg_accesses=write_accesses)
        if fld_args:
            for spaces in [CONTINUOUS_FUNCTION_SPACES,
                           VALID_ANY_SPACE_NAMES,
                           VALID_DISCONTINUOUS_FUNCTION_SPACE_NAMES]:
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
        ''' Currently required for Invoke base class although this
        makes no sense for Dynamo. Need to refactor the Invoke base class
        and remove the need for this property (#279). '''
        return self._dofs

    def raw_arg_list(self):
        '''
        Constructs the class-specific argument list for a kernel.

        :returns: a list of all of the actual arguments to the \
                  kernel call.
        :rtype: list of str.
        '''
        create_arg_list = KernCallArgList(self._parent_call)
        create_arg_list.generate()
        self._raw_arg_list = create_arg_list.arglist

        return self._raw_arg_list

    @property
    def acc_args(self):
        '''
        :returns: the list of quantities that must be available on an \
                  OpenACC device before the associated kernel can be launched.
        :rtype: list of str

        '''
        class KernCallAccArgList(KernCallArgList):
            '''
            Kernel call arguments that need to be declared by OpenACC
            directives. KernCallArgList only needs to be specialised
            where modified, or additional, arguments are required.
            Scalars are apparently not required but it is valid in
            OpenACC to include them and requires less specialisation
            to keep them in.

            '''
            def field_vector(self, argvect):
                '''
                Add the field vector associated with the argument 'argvect' to
                the argument list. OpenACC requires the field and the
                dereferenced data to be specified.

                :param argvect: the kernel argument (vector field).
                :type argvect:  :py:class:`psyclone.dynamo0p3.\
                                DynKernelArgument`

                '''
                for idx in range(1, argvect.vector_size+1):
                    text1 = argvect.proxy_name + "(" + str(idx) + ")"
                    self._arglist.append(text1)
                    text2 = text1 + "%data"
                    self._arglist.append(text2)

            def field(self, arg):
                '''
                Add the field associated with the argument 'arg' to
                the argument list. OpenACC requires the field and the
                dereferenced data to be specified.

                :param arg: the kernel argument (field).
                :type arg: :py:class:`psyclone.dynamo0p3.DynKernelArgument`

                '''
                text1 = arg.proxy_name
                self._arglist.append(text1)
                text2 = text1 + "%data"
                self._arglist.append(text2)

            def stencil(self, arg):
                '''
                Add the stencil dofmap associated with this kernel
                argument. OpenACC requires the full dofmap to be
                specified.

                :param arg: the meta-data description of the kernel \
                argument with which the stencil is associated.
                :type arg: :py:class:`psyclone.dynamo0p3.DynKernelArgument`

                '''
                var_name = DynStencils.dofmap_name(arg)
                self._arglist.append(var_name)

            def operator(self, arg):
                '''
                Add the operator arguments to the argument list if
                they have not already been added. OpenACC requires the
                derived type and the dereferenced data to be
                specified.

                :param arg: the meta-data description of the operator.
                :type arg: :py:class:`psyclone.dynamo0p3.DynKernelArgument`

                '''
                if arg.proxy_name_indexed not in self._arglist:
                    self._arglist.append(arg.proxy_name_indexed)
                    self._arglist.append(arg.proxy_name_indexed + "%ncell_3d")
                    self._arglist.append(arg.proxy_name_indexed +
                                         "%local_stencil")

            def fs_compulsory_field(self, function_space):
                '''
                Add compulsory arguments associated with this function space to
                the list. OpenACC requires the full function-space map
                to be specified.

                :param arg: the current functionspace.
                :type arg: :py:class:`psyclone.dynamo0p3.FunctionSpace`

                '''
                undf_name = get_fs_undf_name(function_space)
                self._arglist.append(undf_name)
                map_name = get_fs_map_name(function_space)
                self._arglist.append(map_name)

        create_acc_arg_list = KernCallAccArgList(self._parent_call)
        create_acc_arg_list.generate()
        return create_acc_arg_list.arglist

    @property
    def scalars(self):
        '''
        Provides the list of names of scalar arguments required by the
        kernel associated with this Arguments object. If there are none
        then the returned list is empty.

        :returns: A list of the names of scalar arguments in this object.
        :rtype: list of str
        '''
        # Return nothing for the moment as it is unclear whether
        # scalars need to be explicitly dealt with (for OpenACC) in
        # the dynamo api.
        return []


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
        :type arg_info: :py:class:`psyclone.parse.algorithm.Arg`
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

    def is_scalar(self):
        ''':return: whether this variable is a scalar variable or not.
        :rtype: bool'''
        return self.type in GH_VALID_SCALAR_NAMES

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
        '''
        :returns: the proxy name for this argument.
        :rtype: str
        '''
        return self._name+"_proxy"

    @property
    def proxy_declaration_name(self):
        '''
        :returns: the proxy name for this argument with the array \
                  dimensions added if required.
        :rtype: str
        '''
        if self._vector_size > 1:
            return self.proxy_name+"("+str(self._vector_size)+")"
        return self.proxy_name

    @property
    def declaration_name(self):
        ''' Returns the name for this argument with the array
        dimensions added if required. '''
        if self._vector_size > 1:
            return self._name+"("+str(self._vector_size)+")"
        return self._name

    @property
    def proxy_name_indexed(self):
        '''
        :returns: the proxy name for this argument with an additional \
                  index which accesses the first element for a vector \
                  argument.
        :rtype: str
        '''
        if self._vector_size > 1:
            return self._name+"_proxy(1)"
        return self._name+"_proxy"

    @property
    def name_indexed(self):
        '''
        :returns: the name for this argument with an additional index \
                  which accesses the first element for a vector argument.
        :rtype: str
        '''
        if self._vector_size > 1:
            return self._name+"(1)"
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
        '''
        Returns the Fortran intent of this argument.

        :return: the expected Fortran intent for this argument as specified
                 by the kernel argument metadata
        :rtype: str
        '''
        if self.access == AccessType.READ:
            return "in"
        elif self.access == AccessType.WRITE:
            return "out"
        elif self.access == AccessType.READWRITE:
            return "inout"
        elif self.access in [AccessType.INC] + \
                AccessType.get_valid_reduction_modes():
            return "inout"
        else:
            valid_reductions = AccessType.get_valid_reduction_names()
            raise GenerationError(
                "Expecting argument access to be one of 'gh_read, gh_write, "
                "gh_inc', 'gh_readwrite' or one of {0}, but found '{1}'".
                format(valid_reductions, self.access))

    @property
    def discontinuous(self):
        '''Returns True if this argument is known to be on a discontinuous
        function space including any_discontinuous_space, otherwise
        returns False.'''
        if self.function_space.orig_name in \
           VALID_DISCONTINUOUS_FUNCTION_SPACE_NAMES:
            return True
        if self.function_space.orig_name in VALID_ANY_SPACE_NAMES:
            # we will eventually look this up based on our dependence
            # analysis but for the moment we assume the worst
            return False
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
        return self._type in GH_VALID_OPERATOR_NAMES


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
        kern.load(call)

        # Add the kernel as a child of the loop
        cloop.loop_body.addchild(kern)
        kern.parent = cloop.children[3]

        # Set-up the loop now we have the kernel object
        cloop.load(kern)

        # Return the outermost loop
        return cloop


class DynACCEnterDataDirective(ACCEnterDataDirective):
    '''
    Sub-classes ACCEnterDataDirective to provide an API-specific implementation
    of data_on_device().

    '''
    def data_on_device(self, _):
        '''
        Provide a hook to be able to add information about data being on a
        device (or not). This is currently not used in dynamo0p3.

        '''
        return None


# ---------- Documentation utils -------------------------------------------- #
# The list of module members that we wish AutoAPI to generate
# documentation for. (See https://psyclone-ref.readthedocs.io)
__all__ = [
    'FunctionSpace',
    'DynFuncDescriptor03',
    'DynArgDescriptor03',
    'DynKernMetadata',
    'DynamoPSy',
    'DynamoInvokes',
    'DynCollection',
    'DynStencils',
    'DynDofmaps',
    'DynOrientations',
    'DynFunctionSpaces',
    'DynFields',
    'DynProxies',
    'DynCellIterators',
    'DynScalarArgs',
    'DynLMAOperators',
    'DynCMAOperators',
    'DynMeshes',
    'DynInterGrid',
    'DynBasisFunctions',
    'DynBoundaryConditions',
    'DynInvoke',
    'DynInvokeSchedule',
    'DynGlobalSum',
    'DynHaloExchange',
    'DynHaloExchangeStart',
    'DynHaloExchangeEnd',
    'HaloDepth',
    'HaloWriteAccess',
    'HaloReadAccess',
    'DynLoop',
    'DynKern',
    'ArgOrdering',
    'KernCallArgList',
    'KernStubArgList',
    'FSDescriptor',
    'FSDescriptors',
    'DynStencil',
    'DynKernelArguments',
    'DynKernelArgument',
    'DynKernCallFactory',
    'DynACCEnterDataDirective']
