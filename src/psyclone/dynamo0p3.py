# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017-2022, Science and Technology Facilities Council.
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
# Modified J. Henrichs, Bureau of Meteorology
# Modified A. B. G. Chalk, STFC Daresbury Lab

''' This module implements the PSyclone Dynamo 0.3 API by 1)
    specialising the required base classes in parser.py (KernelType) and
    adding a new class (DynFuncDescriptor03) to capture function descriptor
    metadata and 2) specialising the required base classes in psyGen.py
    (PSy, Invokes, Invoke, InvokeSchedule, Loop, Kern, Inf, Arguments and
    Argument). '''

# Imports
from __future__ import print_function, absolute_import
import abc
import os
from enum import Enum
from collections import OrderedDict, namedtuple, Counter
import six
import fparser

from psyclone import psyGen
from psyclone.configuration import Config
from psyclone.core import AccessType, Signature
from psyclone.domain.lfric.lfric_builtins import (
    LFRicBuiltInCallFactory, LFRicBuiltIn, BUILTIN_MAP)
from psyclone.domain.lfric import (FunctionSpace, KernCallAccArgList,
                                   KernCallArgList, KernStubArgList,
                                   LFRicArgDescriptor, KernelInterface,
                                   LFRicConstants)
from psyclone.errors import GenerationError, InternalError, FieldNotFoundError
from psyclone.f2pygen import (AllocateGen, AssignGen, CallGen, CommentGen,
                              DeallocateGen, DeclGen, DirectiveGen, DoGen,
                              IfThenGen, ModuleGen, SubroutineGen, TypeDeclGen,
                              UseGen)
from psyclone.parse.algorithm import Arg, KernelCall
from psyclone.parse.kernel import KernelType, getkerneldescriptors
from psyclone.parse.utils import ParseError
from psyclone.psyGen import (PSy, Invokes, Invoke, InvokeSchedule,
                             Arguments, KernelArgument, HaloExchange,
                             GlobalSum, FORTRAN_INTENT_NAMES, DataAccess,
                             CodedKern)
from psyclone.psyir.frontend.fortran import FortranReader
from psyclone.psyir.nodes import (Loop, Literal, Schedule, Reference,
                                  ArrayReference, ACCEnterDataDirective,
                                  OMPParallelDoDirective)
from psyclone.psyir.symbols import (
    INTEGER_TYPE, INTEGER_SINGLE_TYPE, DataSymbol, SymbolTable, ScalarType,
    DeferredType, DataTypeSymbol, ContainerSymbol, ImportInterface, ArrayType)

# pylint: disable=too-many-lines
# --------------------------------------------------------------------------- #
# ========== First section : Parser specialisations and classes ============= #
# --------------------------------------------------------------------------- #
#

# ---------- Functions ------------------------------------------------------ #


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
    const = LFRicConstants()
    if basis_fn["shape"] not in const.VALID_QUADRATURE_SHAPES:
        raise InternalError(
            "Unrecognised shape ('{0}') specified in "
            "dynamo0p3.qr_basis_alloc_args(). Should be one of: "
            "{1}".format(basis_fn["shape"], const.VALID_QUADRATURE_SHAPES))

    qr_var = "_" + basis_fn["qr_var"]

    # Dimensionality of the basis arrays depends on the
    # type of quadrature...
    # if basis_fn["shape"] == "gh_quadrature_xyz":
    #     alloc_args = [first_dim, basis_fn["fspace"].ndf_name,
    #          "np_xyz"+"_"+basis_fn["qr_var"]]
    if basis_fn["shape"] == "gh_quadrature_xyoz":
        alloc_args = [first_dim, basis_fn["fspace"].ndf_name,
                      "np_xy"+qr_var, "np_z"+qr_var]
    # elif basis_fn["shape"] == "gh_quadrature_xoyoz":
    #     alloc_args = [first_dim, basis_fn["fspace"].ndf_name,
    #                   "np_x"+"_"+basis_fn["qr_var"],
    #                   "np_y"+"_"+basis_fn["qr_var"],
    #                   "np_z"+"_"+basis_fn["qr_var"]]
    elif basis_fn["shape"] == "gh_quadrature_face":
        alloc_args = [first_dim, basis_fn["fspace"].ndf_name,
                      "np_xyz"+qr_var, "nfaces"+qr_var]
    elif basis_fn["shape"] == "gh_quadrature_edge":
        alloc_args = [first_dim, basis_fn["fspace"].ndf_name,
                      "np_xyz"+qr_var, "nedges"+qr_var]
    else:
        raise NotImplementedError(
            "Unrecognised shape '{0}' specified in "
            "dynamo0p3.qr_basis_alloc_args(). Should be one of: "
            "{1}".format(basis_fn["shape"], const.VALID_QUADRATURE_SHAPES))
    return alloc_args

# ---------- Classes -------------------------------------------------------- #


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
        const = LFRicConstants()
        for idx, arg in enumerate(func_type.args):
            if idx == 0:  # first func_type arg
                if arg.name not in const.VALID_FUNCTION_SPACE_NAMES:
                    raise ParseError(
                        "In the dynamo0p3 API the 1st argument of a "
                        "meta_func entry should be a valid function space "
                        "name (one of {0}), but found '{1}' in '{2}'".format(
                            const.VALID_FUNCTION_SPACE_NAMES,
                            arg.name, func_type))
                self._function_space_name = arg.name
            else:  # subsequent func_type args
                if arg.name not in const.VALID_METAFUNC_NAMES:
                    raise ParseError(
                        "In the dynamo0.3 API, the 2nd argument and all "
                        "subsequent arguments of a meta_func entry should "
                        "be one of {0}, but found "
                        "'{1}' in '{2}".
                        format(const.VALID_METAFUNC_NAMES,
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


class RefElementMetaData(object):
    '''
    Class responsible for parsing reference-element meta-data and storing
    the properties that a kernel requires.

    :param str kernel_name: name of the Kernel that the meta-data is for.
    :param type_declns: list of fparser1 parse tree nodes representing type \
                        declaration statements
    :type type_declns: list of :py:class:`fparser.one.typedecl_statements.Type`

    :raises ParseError: if an unrecognised reference-element property is found.
    :raises ParseError: if a duplicate reference-element property is found.

    '''
    # pylint: disable=too-few-public-methods
    class Property(Enum):
        '''
        Enumeration of the various properties of the Reference Element
        (that a kernel can request). The names of each of these corresponds to
        the names that must be used in kernel meta-data.

        '''
        NORMALS_TO_HORIZONTAL_FACES = 1
        NORMALS_TO_VERTICAL_FACES = 2
        NORMALS_TO_FACES = 3
        OUTWARD_NORMALS_TO_HORIZONTAL_FACES = 4
        OUTWARD_NORMALS_TO_VERTICAL_FACES = 5
        OUTWARD_NORMALS_TO_FACES = 6

    def __init__(self, kernel_name, type_declns):
        # The list of properties requested in the meta-data (if any)
        self.properties = []

        re_properties = []
        # Search the supplied list of type declarations for the one
        # describing the reference-element properties required by the kernel.
        for line in type_declns:
            for entry in line.selector:
                if entry == "reference_element_data_type":
                    # getkerneldescriptors raises a ParseError if the named
                    # element cannot be found.
                    re_properties = getkerneldescriptors(
                        kernel_name, line, var_name="meta_reference_element",
                        var_type="reference_element_data_type")
                    break
            if re_properties:
                # Optimisation - stop searching if we've found a type
                # declaration for the reference-element data
                break
        try:
            # The meta-data entry is a declaration of a Fortran array of type
            # reference_element_data_type. The initialisation of each member
            # of this array is done as a Fortran structure constructor, the
            # argument to which gives a property of the reference element.
            for re_prop in re_properties:
                for arg in re_prop.args:
                    self.properties.append(
                        self.Property[str(arg).upper()])
        except KeyError as err:
            # We found a reference-element property that we don't recognise.
            # Sort for consistency when testing.
            sorted_names = sorted([prop.name for prop in self.Property])
            six.raise_from(ParseError(
                "Unsupported reference-element property: '{0}'. Supported "
                "values are: {1}".format(arg, sorted_names)), err)

        # Check for duplicate properties
        for prop in self.properties:
            if self.properties.count(prop) > 1:
                raise ParseError("Duplicate reference-element property "
                                 "found: '{0}'.".format(prop))


class MeshProperty(Enum):
    '''
    Enumeration of the various properties of the mesh that a kernel may
    require (either named in metadata or implicitly, depending on the type
    of kernel).

    '''
    # pylint: disable=too-few-public-methods
    ADJACENT_FACE = 1
    NCELL_2D = 2
    NCELL_2D_NO_HALOS = 3


class MeshPropertiesMetaData(object):
    '''
    Parses any mesh-property kernel metadata and stores the properties that
    a kernel requires.

    :param str kernel_name: name of the kernel that the meta-data is for.
    :param type_declns: list of fparser1 parse tree nodes representing type \
                        declaration statements.
    :type type_declns: list of :py:class:`fparser.one.typedecl_statements.Type`

    :raises ParseError: if an unrecognised mesh property is found.
    :raises ParseError: if a duplicate mesh property is found.

    '''
    # pylint: disable=too-few-public-methods
    # The properties that may be specified in kernel meta-data are a subset
    # of the MeshProperty enumeration values.
    supported_properties = [MeshProperty.ADJACENT_FACE]

    def __init__(self, kernel_name, type_declns):
        # The list of mesh properties requested in the meta-data.
        self.properties = []

        mesh_props = []
        # Search the supplied list of type declarations for the one
        # describing the reference-element properties required by the kernel.
        for line in type_declns:
            for entry in line.selector:
                if entry == "mesh_data_type":
                    # getkerneldescriptors raises a ParseError if the named
                    # element cannot be found.
                    mesh_props = getkerneldescriptors(
                        kernel_name, line, var_name="meta_mesh",
                        var_type="mesh_data_type")
                    break
            if mesh_props:
                # Optimisation - stop searching if we've found a type
                # declaration for the mesh data
                break
        try:
            # The meta-data entry is a declaration of a Fortran array of type
            # mesh_data_type. The initialisation of each member
            # of this array is done as a Fortran structure constructor, the
            # argument to which gives a mesh property.
            for prop in mesh_props:
                for arg in prop.args:
                    mesh_prop = MeshProperty[str(arg).upper()]
                    if mesh_prop not in self.supported_properties:
                        raise KeyError()
                    self.properties.append(mesh_prop)
        except KeyError as err:
            # We found a mesh property that we don't recognise or that
            # is not supported.
            six.raise_from(
                ParseError("Unsupported mesh property in metadata: '{0}'. "
                           "Supported values are: {1}".format(
                               arg, [prop.name for prop in
                                     self.supported_properties])), err)

        # Check for duplicate properties
        for prop in self.properties:
            if self.properties.count(prop) > 1:
                raise ParseError("Duplicate mesh property "
                                 "found: '{0}'.".format(prop))


class DynKernMetadata(KernelType):
    ''' Captures the Kernel subroutine code and metadata describing
    the subroutine for the Dynamo 0.3 API.

    :param ast: fparser1 AST for the kernel.
    :type ast: :py:class:`fparser.block_statements.BeginSource`
    :param str name: The name of this kernel.

    :raises ParseError: if the meta-data does not conform to the \
                        rules for the Dynamo 0.3 API.
    '''
    # pylint: disable=too-many-instance-attributes
    def __init__(self, ast, name=None):
        # pylint: disable=too-many-branches, too-many-locals

        KernelType.__init__(self, ast, name=name)

        # The type of CMA operation this kernel performs (or None if
        # no CMA operators are involved)
        self._cma_operation = None

        # Query the meta-data for the evaluator shape(s) (only required if
        # kernel uses quadrature or an evaluator). If it is not
        # present then eval_shapes will be an empty list.
        shape = self.get_integer_variable('gh_shape')
        if not shape:
            # There's no scalar gh_shape - is it present as an array?
            self._eval_shapes = self.get_integer_array('gh_shape')
        else:
            self._eval_shapes = [shape]

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
            self._arg_descriptors.append(
                LFRicArgDescriptor(arg_type, self.iterates_over))

        # Get a list of the Type declarations in the metadata
        type_declns = [cline for cline in self._ktype.content if
                       isinstance(cline, fparser.one.typedecl_statements.Type)]

        # Parse the func_type metadata if it exists
        func_types = []
        for line in type_declns:
            for entry in line.selector:
                if entry == "func_type":
                    func_types = getkerneldescriptors(
                        name, line, var_name="meta_funcs",
                        var_type="func_type")
                    break

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

            const = LFRicConstants()
            # Check that a valid shape has been specified if
            # this function space requires a basis or differential basis
            for op_name in descriptor.operator_names:
                if op_name in const.VALID_EVALUATOR_NAMES:
                    need_evaluator = True
                    if not self._eval_shapes:
                        raise ParseError(
                            "In the Dynamo0.3 API any kernel requiring "
                            "quadrature or an evaluator ({0}) must also "
                            "supply the shape of that evaluator by setting "
                            "'gh_shape' in the kernel meta-data but "
                            "this is missing for kernel '{1}'".
                            format(const.VALID_EVALUATOR_NAMES,
                                   self.name))
                    shape_set = set(self._eval_shapes)
                    if not shape_set.issubset(
                            set(const.VALID_EVALUATOR_SHAPES)):
                        raise ParseError(
                            "In the Dynamo0.3 API a kernel requiring either "
                            "quadrature or an evaluator must request one or "
                            "more valid gh_shapes (one of {0}) but got '{1}' "
                            "for kernel '{2}'".
                            format(const.VALID_EVALUATOR_SHAPES,
                                   self._eval_shapes, self.name))

            self._func_descriptors.append(descriptor)

        # Check to see whether the optional 'gh_evaluator_targets'
        # has been supplied. This lists the function spaces for which
        # any evaluators (gh_shape=gh_evaluator) should be provided.
        _targets = self.get_integer_array('gh_evaluator_targets')
        if not _targets and \
           self._eval_shapes and "gh_evaluator" in self._eval_shapes:
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

        # Does this kernel require any properties of the reference element?
        self.reference_element = RefElementMetaData(self.name, type_declns)

        # Does this kernel require any properties of the mesh?
        self.mesh = MeshPropertiesMetaData(self.name, type_declns)

        # Perform further checks that the meta-data we've parsed
        # conforms to the rules for this API
        self._validate(need_evaluator)

    def _validate(self, need_evaluator):
        '''
        Check that the meta-data conforms to Dynamo 0.3 rules for a
        user-provided kernel or a built-in

        :param bool need_evaluator: whether this kernel requires an \
                                    evaluator/quadrature.
        :raises ParseError: if the kernel metadata specifies writing to the \
                            read-only function space.
        :raises ParseError: if a user-supplied LFRic kernel updates/writes \
                            to a scalar argument.
        :raises ParseError: if a kernel does not have at least one argument \
                            that is updated/written to.
        :raises ParseError: if a kernel does not require basis or \
                            differential basis functions but specifies one \
                            or more gh_shapes.
        :raises ParseError: if a kernel does not require basis or \
                            differential basis functions but specifies \
                            gh_evaluator_targets.
        :raises ParseError: if a kernel specifies gh_evaluator_targets \
                            but does not need an evaluator.
        :raises ParseError: if a kernel requires an evaluator on a \
                            specific function space but does not have an \
                            argument on that space.
        :raises ParseError: if a kernel that has LMA operator arguments \
                            also has a field argument with an invalid \
                            data type (other than 'gh_real').

        '''
        # pylint: disable=too-many-branches
        # We must have at least one argument that is written to
        const = LFRicConstants()
        write_count = 0
        for arg in self._arg_descriptors:
            if arg.access != AccessType.READ:
                write_count += 1
                # We must not write to a field on a read-only function space
                if arg.argument_type in const.VALID_FIELD_NAMES \
                   and arg.function_spaces[0] in \
                   const.READ_ONLY_FUNCTION_SPACES:
                    raise ParseError(
                        "Found kernel metadata in '{0}' that specifies "
                        "writing to the read-only function space '{1}'."
                        "".format(self.name, arg.function_spaces[0]))

                # We must not write to scalar arguments if it's not a
                # built-in
                if self.name not in BUILTIN_MAP and \
                   arg.argument_type in const.VALID_SCALAR_NAMES:
                    raise ParseError(
                        "A user-supplied LFRic kernel must not write/update "
                        "a scalar argument but kernel '{0}' has a scalar "
                        "argument with '{1}' access."
                        .format(self.name, arg.access.api_specific_name()))
        if write_count == 0:
            raise ParseError("An LFRic kernel must have at least one "
                             "argument that is updated (written to) but "
                             "found none for kernel '{0}'.".format(self.name))

        # Check that no shape has been supplied if no basis or
        # differential basis functions are required for the kernel
        if not need_evaluator and self._eval_shapes:
            raise ParseError(
                "Kernel '{0}' specifies one or more gh_shapes ({1}) but does "
                "not need an evaluator because no basis or differential basis "
                "functions are required".format(self.name, self._eval_shapes))
        # Check that gh_evaluator_targets is only present if required
        if self._eval_targets:
            if not need_evaluator:
                raise ParseError(
                    "Kernel '{0}' specifies gh_evaluator_targets ({1}) but "
                    "does not need an evaluator because no basis or "
                    "differential basis functions are required".
                    format(self.name, self._eval_targets))
            if "gh_evaluator" not in self._eval_shapes:
                raise ParseError(
                    "Kernel '{0}' specifies gh_evaluator_targets ({1}) but "
                    "does not need an evaluator because gh_shape={2}".
                    format(self.name, self._eval_targets, self._eval_shapes))
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
                        "on '{1}' but does not have an argument on this space."
                        .format(self.name, eval_fs))

        # If we have an LMA operator as argument then only field arguments
        # with 'gh_real' data type are permitted
        lma_ops = psyGen.args_filter(self._arg_descriptors,
                                     arg_types=["gh_operator"])
        if lma_ops:
            for arg in self._arg_descriptors:
                if (arg.argument_type in const.VALID_FIELD_NAMES
                        and arg.data_type != "gh_real"):
                    raise ParseError(
                        "In the LFRic API a kernel that has an LMA "
                        "operator argument must only have field arguments "
                        "with 'gh_real' data type but kernel '{0}' has a "
                        "field argument with '{1}' data type.".
                        format(self.name, arg.data_type))

        # If we have a columnwise operator as argument then we need to
        # identify the operation that this kernel performs (one of
        # assemble, apply/apply-inverse and matrix-matrix)
        cwise_ops = psyGen.args_filter(self._arg_descriptors,
                                       arg_types=["gh_columnwise_operator"])
        if cwise_ops:
            self._cma_operation = self._identify_cma_op(cwise_ops)

        # Perform checks for inter-grid kernels
        self._validate_inter_grid()

        # Perform checks for a kernel with operates_on == domain
        self._validate_operates_on_domain(need_evaluator)

    def _validate_inter_grid(self):
        '''
        Checks that the kernel meta-data obeys the rules for Dynamo 0.3
        inter-grid kernels. If none of the kernel arguments has a mesh
        associated with it then it is not an inter-grid kernel and this
        routine silently returns.

        :raises: ParseError: if meta-data breaks inter-grid rules
        '''
        # pylint: disable=too-many-branches
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
        const = LFRicConstants()
        for arg in self._arg_descriptors:
            # Collect info so that we can check inter-grid kernels
            if arg.argument_type in const.VALID_FIELD_NAMES:
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
                non_field_arg_types.add(arg.argument_type)

        mesh_list = mesh_dict.keys()
        if not mesh_list:
            # There are no meshes associated with any of the arguments so
            # this is not an inter-grid kernel
            return

        if len(const.VALID_MESH_TYPES) != 2:
            # Sanity check that nobody has messed with the number of
            # grid types that we recognise. This is here because the
            # implementation assumes that there are just two grids
            # (coarse and fine).
            raise InternalError(
                "The implementation of inter-grid support in the LFRic "
                "API assumes there are exactly two mesh types but "
                "LFRicConstants.VALID_MESH_TYPES contains {0}: {1}".
                format(len(const.VALID_MESH_TYPES),
                       const.VALID_MESH_TYPES))
        if len(mesh_list) != len(const.VALID_MESH_TYPES):
            raise ParseError(
                "Inter-grid kernels in the Dynamo 0.3 API must have at least "
                "one field argument on each of the mesh types ({0}). However, "
                "kernel {1} has arguments only on {2}".format(
                    const.VALID_MESH_TYPES, self.name,
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
        '''
        Identify and return the type of CMA-operator-related operation
        this kernel performs (one of "assemble", "apply" or "matrix-matrix")

        :param cwise_ops: all column-wise operator arguments in a kernel.
        :type cwise_ops: list of str

        :returns: the type of CMA-operator-related operation that this \
                  kernel performs.
        :rtype: str

        :raises ParseError: if the kernel metadata does not conform to the \
                            LFRic rules for a kernel with a CMA operator.

        '''
        # pylint: disable=too-many-branches
        const = LFRicConstants()
        for arg in self._arg_descriptors:
            # No vector arguments are permitted
            if arg.vector_size > 1:
                raise ParseError(
                    "Kernel '{0}' takes a CMA operator but has a "
                    "vector argument '{1}'. This is forbidden.".
                    format(self.name,
                           arg.argument_type+"*"+str(arg.vector_size)))
            # No stencil accesses are permitted
            if arg.stencil:
                raise ParseError(
                    "Kernel '{0}' takes a CMA operator but has an argument "
                    "with a stencil access ('{1}'). This is forbidden.".
                    format(self.name, arg.stencil['type']))
            # Only field arguments with 'gh_real' data type are permitted
            if (arg.argument_type in const.VALID_FIELD_NAMES and
                    arg.data_type != "gh_real"):
                raise ParseError(
                    "In the LFRic API a kernel that takes a CMA operator "
                    "argument must only have field arguments with "
                    "'gh_real' data type but kernel '{0}' has a field "
                    "argument with '{1}' data type.".
                    format(self.name, arg.data_type))

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
                    "In the LFRic API a kernel that applies a CMA "
                    "operator must only have one such operator in its "
                    "list of arguments but found {0} for kernel '{1}'.".
                    format(len(cwise_ops), self.name))
            cma_op = cwise_ops[0]
            if len(self._arg_descriptors) != 3:
                raise ParseError(
                    "In the LFRic API a kernel that applies a CMA "
                    "operator must have 3 arguments (the operator and "
                    "two fields) but kernel '{0}' has {1} arguments.".
                    format(self.name, len(self._arg_descriptors)))
            # Check that the other two arguments are fields
            farg_read = psyGen.args_filter(
                self._arg_descriptors,
                arg_types=const.VALID_FIELD_NAMES,
                arg_accesses=[AccessType.READ])
            write_accesses = AccessType.all_write_accesses()
            farg_write = psyGen.args_filter(
                self._arg_descriptors,
                arg_types=const.VALID_FIELD_NAMES,
                arg_accesses=write_accesses)
            if len(farg_read) != 1:
                raise ParseError(
                    "Kernel '{0}' has a read-only CMA operator. In order "
                    "to apply it the kernel must have one read-only field "
                    "argument.".format(self.name))
            if len(farg_write) != 1:
                raise ParseError(
                    "Kernel '{0}' has a read-only CMA operator. In order "
                    "to apply it the kernel must write to one field "
                    "argument.".format(self.name))
            # Check that the function spaces match up
            if farg_read[0].function_space != cma_op.function_space_from:
                raise ParseError(
                    "Kernel '{0}' applies a CMA operator but the function "
                    "space of the field argument it reads from ('{1}') "
                    "does not match the 'from' space of the operator "
                    "('{2}').".format(self.name, farg_read[0].function_space,
                                      cma_op.function_space_from))
            if farg_write[0].function_space != cma_op.function_space_to:
                raise ParseError(
                    "Kernel '{0}' applies a CMA operator but the function "
                    "space of the field argument it writes to ('{1}') "
                    "does not match the 'to' space of the operator "
                    "('{2}').".format(self.name, farg_write[0].function_space,
                                      cma_op.function_space_to))
            # This is a valid CMA-apply or CMA-apply-inverse kernel
            return "apply"

        if write_count == 1:
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
                    if arg.argument_type == 'gh_columnwise_operator':
                        write_args.remove(arg)
                        break
                raise ParseError(
                    "Kernel '{0}' writes to a column-wise operator but "
                    "also writes to {1} argument(s). This is not "
                    "allowed.".format(self.name,
                                      [str(arg.argument_type) for arg
                                       in write_args]))
            if len(cwise_ops) == 1:

                # If this is a valid assembly kernel then we need at least one
                # read-only LMA operator
                lma_read_ops = psyGen.args_filter(
                    self._arg_descriptors,
                    arg_types=["gh_operator"],
                    arg_accesses=[AccessType.READ])
                if lma_read_ops:
                    return "assembly"
                raise ParseError(
                    "Kernel '{0}' has a single column-wise operator "
                    "argument but does not conform to the rules for an "
                    "Assembly kernel because it does not have any read-"
                    "only LMA operator arguments.".format(self.name))
            # A valid matrix-matrix kernel must only have CMA operators
            # and scalars as arguments.
            scalar_args = psyGen.args_filter(
                self._arg_descriptors,
                arg_types=const.VALID_SCALAR_NAMES)
            if (len(scalar_args) + len(cwise_ops)) != \
               len(self._arg_descriptors):
                raise ParseError(
                    "A column-wise matrix-matrix kernel must have only "
                    "column-wise operators and scalars as arguments but "
                    "kernel '{0}' has: {1}.".
                    format(self.name,
                           [str(arg.argument_type) for arg in
                            self._arg_descriptors]))
            return "matrix-matrix"
        raise ParseError(
            "An LFRic kernel cannot update more than one CMA "
            "(column-wise) operator but kernel '{0}' updates {1}.".
            format(self.name, write_count))

    def _validate_operates_on_domain(self, need_evaluator):
        '''
        Check whether a kernel that has operates_on == domain obeys
        the rules for the LFRic API.

        :raises ParseError: if the kernel metadata does not obey the rules \
                            for an LFRic kernel with operates_on = domain.
        '''
        if self.iterates_over != "domain":
            return

        const = LFRicConstants()
        # A kernel which operates on the 'domain' is currently restricted
        # to only accepting scalar and field arguments.
        valid_arg_types = (const.VALID_SCALAR_NAMES + const.VALID_FIELD_NAMES)
        for arg in self._arg_descriptors:
            if arg.argument_type not in valid_arg_types:
                raise ParseError(
                    "In the LFRic API a kernel which operates on the 'domain' "
                    "is only permitted to accept scalar and field arguments "
                    "but the metadata for kernel '{0}' includes an argument "
                    "of type '{1}'".format(self.name, arg.argument_type))

        if need_evaluator:
            raise ParseError(
                "In the LFRic API a kernel that operates on the 'domain' "
                "cannot be passed basis/differential basis functions but the "
                "metadata for kernel '{0}' contains an entry for 'meta_funcs'".
                format(self.name))

        if self.reference_element.properties:
            raise ParseError(
                "Kernel '{0}' operates on the domain but requests properties "
                "of the reference element ({1}). This is not permitted in the "
                "LFRic API.".format(self.name,
                                    self.reference_element.properties))

        if self.mesh.properties:
            raise ParseError(
                "Kernel '{0}' operates on the domain but requests properties "
                "of the mesh ({1}). This is not permitted in the "
                "LFRic API.".format(self.name, self.mesh.properties))

        if self._is_intergrid:
            raise ParseError(
                "Kernel '{0}' operates on the domain but has fields on "
                "different mesh resolutions (inter-grid). This is not "
                "permitted in the LFRic API.".format(self.name))

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
    def eval_shapes(self):
        '''
        Returns the shape(s) of evaluator required by this kernel or an
        empty string if none.

        :return: the shape(s) of the evaluator (one of VALID_EVALUATOR_SHAPES)
                 or an empty list if the kernel does not require one.
        :rtype: list

        '''
        return self._eval_shapes

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
    '''
    The LFRic-specific PSy class. This creates an LFRic-specific
    Invokes object (which controls all the required invocation calls).
    It also overrides the PSy gen method so that we generate
    LFRic-specific PSy module code.

    :param invoke_info: object containing the required invocation information \
                        for code optimisation and generation.
    :type invoke_info: :py:class:`psyclone.parse.algorithm.FileInfo`

    '''
    def __init__(self, invoke_info):
        PSy.__init__(self, invoke_info)
        self._invokes = DynamoInvokes(invoke_info.calls, self)
        # Initialise the dictionary that holds the names of the required
        # LFRic constants, data structures and data structure proxies for
        # the "use" statements in modules that contain PSy-layer routines.
        const = LFRicConstants()
        const_mod = const.UTILITIES_MOD_MAP["constants"]["module"]
        infmod_list = [const_mod]
        # Add all field and operator modules that might be used in the
        # algorithm layer. These do not appear in the code unless a
        # variable is added to the "only" part of the
        # '_infrastructure_modules' map.
        for data_type_info in const.DATA_TYPE_MAP.values():
            infmod_list.append(data_type_info["module"])

        # This also removes any duplicates from infmod_list
        self._infrastructure_modules = OrderedDict(
            (k, set()) for k in infmod_list)

        kind_names = set()

        # The infrastructure declares integer types with default
        # precision so always add this.
        api_config = Config.get().api_conf("dynamo0.3")
        kind_names.add(api_config.default_kind["integer"])

        # Datatypes declare precision information themselves. However,
        # that is not the case for literals. Therefore deal
        # with these separately here.
        for invoke in self.invokes.invoke_list:
            schedule = invoke.schedule
            for kernel in schedule.kernels():
                for arg in kernel.args:
                    if arg.is_literal:
                        kind_names.add(arg.precision)
        # Add precision names to the dictionary storing the required
        # LFRic constants. Sort the names so that all versions of
        # Python return the same order (for testing purposes).
        self._infrastructure_modules[const_mod] = list(sorted(kind_names))

    @property
    def name(self):
        '''
        :returns: a name for the PSy layer. This is used as the PSy module \
                  name. We override the default value as the Met Office \
                  prefer "_psy" to be appended, rather than prepended.
        :rtype: str

        '''
        return self._name + "_psy"

    @property
    def orig_name(self):
        '''
        :returns: the unmodified PSy-layer name.
        :rtype: str

        '''
        return self._name

    @property
    def infrastructure_modules(self):
        '''
        :returns: the dictionary that holds the names of the required \
                  LFRic infrastructure modules to create "use" \
                  statements in the PSy-layer modules.
        :rtype: dict of set

        '''
        return self._infrastructure_modules

    @property
    def gen(self):
        '''
        Generate PSy code for the LFRic (Dynamo0.3) API.

        :returns: root node of generated Fortran AST.
        :rtype: :py:class:`psyir.nodes.Node`

        '''
        # Create an empty PSy layer module
        psy_module = ModuleGen(self.name)

        # Add all invoke-specific information
        self.invokes.gen_code(psy_module)

        # Include required constants and infrastructure modules. The sets of
        # required LFRic data structures and their proxies are updated in
        # the relevant field and operator subclasses of DynCollection.
        # Here we sort the inputs in reverse order to have "_type" before
        # "_proxy_type" and "operator_" before "columnwise_operator_".
        # We also iterate through the dictionary in reverse order so the
        # "use" statements for field types are before the "use" statements
        # for operator types.
        for infmod in reversed(self._infrastructure_modules):
            if self._infrastructure_modules[infmod]:
                infmod_types = sorted(
                    list(self._infrastructure_modules[infmod]), reverse=True)
                psy_module.add(UseGen(psy_module, name=infmod,
                                      only=True, funcnames=infmod_types))

        # Return the root node of the generated code
        return psy_module.root


class DynamoInvokes(Invokes):
    '''The Dynamo specific invokes class. This passes the Dynamo
    specific invoke class to the base class so it creates the one we
    require.

    :param alg_calls: list of objects containing the parsed invoke \
        information.
    :type alg_calls: list of \
        :py:class:`psyclone.parse.algorithm.InvokeCall`
    :param psy: the PSy object containing this DynamoInvokes object.
    :type psy: :py:class`psyclone.dynamo0p3.DynamoPSy`

    '''
    def __init__(self, alg_calls, psy):
        self._0_to_n = DynInvoke(None, None, None)  # for pyreverse
        Invokes.__init__(self, alg_calls, DynInvoke, psy)


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
            self._symbol_table = self._invoke.schedule.symbol_table
            # The list of kernel calls we are responsible for
            self._calls = node.schedule.kernels()
        elif isinstance(node, DynKern):
            # We are handling declarations for a Kernel stub
            self._invoke = None
            self._kernel = node
            # TODO 719 The symbol table is not connected to other parts of
            # the Stub generation.
            self._symbol_table = SymbolTable()
            # We only have a single kernel call in this case
            self._calls = [node]
        else:
            raise InternalError("DynCollection takes only a DynInvoke "
                                "or a DynKern but got: {0}".format(
                                    type(node)))

        # Whether or not the associated Invoke contains only kernels that
        # operate on dofs.
        if self._invoke:
            self._dofs_only = self._invoke.operates_on_dofs_only
        else:
            self._dofs_only = False

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
        # pylint: disable=too-many-branches
        super(DynStencils, self).__init__(node)

        # List of arguments which have an extent value passed to this
        # invoke routine from the algorithm layer. Duplicate argument
        # names are removed.
        self._unique_extent_args = []
        extent_names = []
        # pylint: disable=too-many-nested-blocks
        for call in self._calls:
            for arg in call.arguments.args:
                if arg.stencil:
                    # Check for the existence of arg.extent here as in
                    # the future we plan to support kernels which
                    # specify the value of extent in metadata. If this
                    # is the case then an extent argument is not
                    # required.
                    # TODO #963
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
        return self._symbol_table.find_or_create_tag(unique, root_name).name

    @staticmethod
    def dofmap_name(symtab, arg):
        '''
        Creates and registers a name for the stencil dofmap associated with
        the supplied kernel argument.

        :param symtab: symbol table that will contain (or already contains) \
            the symbol with this name.
        :type symtab: :py:class:`psyclone.psyir.symbols.SymbolTable`
        :param arg: kernel argument with which the stencil is associated.
        :type arg: :py:class:`psyclone.dynamo0p3.DynKernelArgument`

        :returns: a valid unique dofmap name for a stencil in the PSy layer.
        :rtype: str
        '''
        root_name = arg.name + "_stencil_dofmap"
        unique = DynStencils.stencil_unique_str(arg, "dofmap")
        return symtab.find_or_create_tag(unique, root_name).name

    @staticmethod
    def dofmap_size_name(symtab, arg):
        '''
        Create a valid unique name for the size (in cells) of a stencil
        dofmap in the PSy layer.

        :param symtab: symbol table that will contain (or already contains) \
            the symbol with this name.
        :type symtab: :py:class:`psyclone.psyir.symbols.SymbolTable`
        :param arg: the kernel argument with which the stencil is associated.
        :type arg: :py:class:`psyclone.dynamo0p3.DynKernelArgument`

        :returns: a Fortran variable name for the stencil size.
        :rtype: str
        '''
        root_name = arg.name + "_stencil_size"
        unique = DynStencils.stencil_unique_str(arg, "size")
        return symtab.find_or_create_tag(unique, root_name).name

    @staticmethod
    def max_branch_length_name(symtab, arg):
        '''
        Create a valid unique name for the maximum length of a stencil branch
        (in cells) of a 2D stencil dofmap in the PSy layer. This is required
        in the kernels for defining the maximum possible length of one of the
        dofmap array dimensions.

        :param symtab: symbol table that will contain (or already contains) \
            the symbol with this name.
        :type symtab: :py:class:`psyclone.psyir.symbols.SymbolTable`
        :param arg: the kernel argument with which the stencil is associated.
        :type arg: :py:class:`psyclone.dynamo0p3.DynKernelArgument`

        :returns: a Fortran variable name for the max stencil branch length.
        :rtype: str
        '''
        root_name = arg.name + "_max_branch_length"
        unique = DynStencils.stencil_unique_str(arg, "length")
        return symtab.find_or_create_tag(unique, root_name).name

    def _unique_max_branch_length_vars(self):
        '''
        :returns: list of all the unique max stencil extent argument names in
                  this kernel call for cross2d stencils.
        :rtype: list of str
        '''
        names = []
        for arg in self._kern_args:
            if arg.descriptor.stencil['type'] == "cross2d":
                names.append(arg.name + "_max_branch_length")

        return names

    def _declare_unique_max_branch_length_vars(self, parent):
        '''
        Declare all unique max branch length arguments as integers with intent
        in and add the declaration as a child of the parent argument passed
        in.

        :param parent: the node in the f2pygen AST to which to add the \
                       declarations.
        :type parent: :py:class:`psyclone.f2pygen.SubroutineGen`

        '''
        api_config = Config.get().api_conf("dynamo0.3")

        if self._unique_max_branch_length_vars():
            parent.add(DeclGen(
                parent, datatype="integer",
                kind=api_config.default_kind["integer"],
                entity_decls=self._unique_max_branch_length_vars(), intent="in"
            ))

    @staticmethod
    def direction_name(symtab, arg):
        '''
        Creates a Fortran variable name to hold the direction of the stencil
        associated with the supplied kernel argument.

        :param symtab: symbol table that will contain (or already contains) \
            the symbol with this name.
        :type symtab: :py:class:`psyclone.psyir.symbols.SymbolTable`
        :param arg: the kernel argument with which the stencil is associated.
        :type arg: :py:class:`psyclone.dynamo0p3.DynKernelArgument`

        :returns: a Fortran variable name for the stencil direction.
        :rtype: str
        '''
        root_name = arg.name+"_direction"
        unique = DynStencils.stencil_unique_str(arg, "direction")
        return symtab.find_or_create_tag(unique, root_name).name

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
            names = [self.dofmap_size_name(self._symbol_table, arg)
                     for arg in self._unique_extent_args]
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
        api_config = Config.get().api_conf("dynamo0.3")

        if self._unique_extent_vars:
            if self._kernel:
                for arg in self._kern_args:
                    if arg.descriptor.stencil['type'] == "cross2d":
                        parent.add(DeclGen(
                            parent, datatype="integer",
                            kind=api_config.default_kind["integer"],
                            dimension="4",
                            entity_decls=self._unique_extent_vars, intent="in"
                        ))
                    else:
                        parent.add(DeclGen(
                            parent, datatype="integer",
                            kind=api_config.default_kind["integer"],
                            entity_decls=self._unique_extent_vars,
                            intent="in"))
            elif self._invoke:
                parent.add(DeclGen(
                    parent, datatype="integer",
                    kind=api_config.default_kind["integer"],
                    entity_decls=self._unique_extent_vars, intent="in"
                ))

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
        api_config = Config.get().api_conf("dynamo0.3")

        if self._unique_direction_vars:
            parent.add(DeclGen(parent, datatype="integer",
                               kind=api_config.default_kind["integer"],
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
        self._declare_unique_max_branch_length_vars(parent)
        self._declare_maps_stub(parent)

    def initialise(self, parent):
        '''
        Adds in the code to initialise stencil dofmaps to the PSy layer.

        :param parent: the node in the f2pygen AST to which to add the \
                       initialisations.
        :type parent: :py:class:`psyclone.f2pygen.SubroutineGen`

        :raises GenerationError: if an unsupported stencil type is encountered.
        '''
        if not self._kern_args:
            return

        parent.add(CommentGen(parent, ""))
        parent.add(CommentGen(parent, " Initialise stencil dofmaps"))
        parent.add(CommentGen(parent, ""))
        api_config = Config.get().api_conf("dynamo0.3")
        stencil_map_names = []
        const = LFRicConstants()
        for arg in self._kern_args:
            map_name = self.map_name(arg)
            if map_name not in stencil_map_names:
                # Only initialise maps once.
                stencil_map_names.append(map_name)
                stencil_type = arg.descriptor.stencil['type']
                symtab = self._symbol_table
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
                elif stencil_type == "cross2d":
                    parent.add(
                        AssignGen(parent, pointer=True, lhs=map_name,
                                  rhs=arg.proxy_name_indexed +
                                  "%vspace%get_stencil_2D_dofmap(" +
                                  "STENCIL_2D_CROSS" + "," +
                                  self.extent_value(arg) + ")"))
                    # Max branch length in the CROSS2D stencil is used when
                    # defining the stencil_dofmap dimensions at declaration of
                    # the dummy argument in the kernel. This value is 1
                    # greater than the stencil extent as the central cell
                    # is included as part of the stencil_dofmap.
                    parent.add(
                        AssignGen(parent,
                                  lhs=self.max_branch_length_name(symtab,
                                                                  arg),
                                  rhs=self.extent_value(arg) + " + 1_" +
                                  api_config.default_kind["integer"]))
                else:
                    try:
                        stencil_name = const.STENCIL_MAPPING[stencil_type]
                    except KeyError as err:
                        six.raise_from(GenerationError(
                            "Unsupported stencil type '{0}' supplied. "
                            "Supported mappings are {1}".
                            format(arg.descriptor.stencil['type'],
                                   str(const.STENCIL_MAPPING))), err)
                    parent.add(
                        AssignGen(parent, pointer=True, lhs=map_name,
                                  rhs=arg.proxy_name_indexed +
                                  "%vspace%get_stencil_dofmap(" +
                                  stencil_name + "," +
                                  self.extent_value(arg) + ")"))

                parent.add(AssignGen(parent, pointer=True,
                                     lhs=self.dofmap_name(symtab, arg),
                                     rhs=map_name + "%get_whole_dofmap()"))

                # Add declaration and look-up of stencil size
                parent.add(AssignGen(parent, pointer=True,
                                     lhs=self.dofmap_size_name(symtab, arg),
                                     rhs=map_name + "%get_stencil_sizes()"))

    def _declare_maps_invoke(self, parent):
        '''
        Declare all stencil maps in the PSy layer.

        :param parent: the node in the f2pygen AST to which to add \
                       declarations.
        :type parent: :py:class:`psyclone.f2pygen.SubroutineGen`

        :raises GenerationError: if an unsupported stencil type is encountered.
        '''
        api_config = Config.get().api_conf("dynamo0.3")

        if not self._kern_args:
            return

        symtab = self._symbol_table
        stencil_map_names = []
        const = LFRicConstants()

        for arg in self._kern_args:
            map_name = self.map_name(arg)

            if map_name in stencil_map_names:
                continue

            stencil_map_names.append(map_name)
            stencil_type = arg.descriptor.stencil['type']
            if stencil_type == "cross2d":
                smap_type = const.STENCIL_TYPE_MAP["stencil_2D_dofmap"]["type"]
                smap_mod = const.STENCIL_TYPE_MAP[
                    "stencil_2D_dofmap"]["module"]
                parent.add(UseGen(parent, name=smap_mod, only=True,
                                  funcnames=[smap_type, "STENCIL_2D_CROSS"]))
                parent.add(TypeDeclGen(parent, pointer=True,
                                       datatype=smap_type,
                                       entity_decls=[map_name +
                                                     " => null()"]))
                parent.add(DeclGen(parent, datatype="integer",
                                   kind=api_config.default_kind["integer"],
                                   pointer=True,
                                   entity_decls=[self.dofmap_name(symtab,
                                                                  arg) +
                                                 "(:,:,:,:) => null()"]))
                parent.add(DeclGen(parent, datatype="integer",
                                   kind=api_config.default_kind["integer"],
                                   pointer=True,
                                   entity_decls=[self.dofmap_size_name(symtab,
                                                                       arg) +
                                                 "(:,:) => null()"]))
                parent.add(DeclGen(parent, datatype="integer",
                                   kind=api_config.default_kind["integer"],
                                   entity_decls=[self.max_branch_length_name(
                                       symtab, arg)]))
            else:
                smap_type = const.STENCIL_TYPE_MAP["stencil_dofmap"]["type"]
                smap_mod = const.STENCIL_TYPE_MAP["stencil_dofmap"]["module"]
                parent.add(UseGen(parent, name=smap_mod,
                                  only=True, funcnames=[smap_type]))
                if stencil_type == 'xory1d':
                    drct_mod = const.STENCIL_TYPE_MAP["direction"]["module"]
                    parent.add(UseGen(parent, name=drct_mod,
                                      only=True, funcnames=["x_direction",
                                                            "y_direction"]))
                    parent.add(UseGen(parent, name=smap_mod,
                                      only=True, funcnames=["STENCIL_1DX",
                                                            "STENCIL_1DY"]))
                else:
                    try:
                        stencil_name = const.STENCIL_MAPPING[stencil_type]
                    except KeyError as err:
                        six.raise_from(GenerationError(
                            "Unsupported stencil type '{0}' supplied. "
                            "Supported mappings are {1}".
                            format(arg.descriptor.stencil['type'],
                                   str(const.STENCIL_MAPPING))), err)
                    parent.add(UseGen(parent, name=smap_mod,
                                      only=True, funcnames=[stencil_name]))

                parent.add(TypeDeclGen(parent, pointer=True,
                                       datatype=smap_type,
                                       entity_decls=[map_name+" => null()"]))
                parent.add(DeclGen(parent, datatype="integer",
                                   kind=api_config.default_kind["integer"],
                                   pointer=True,
                                   entity_decls=[self.dofmap_name(symtab,
                                                                  arg) +
                                                 "(:,:,:) => null()"]))
                parent.add(DeclGen(parent, datatype="integer",
                                   kind=api_config.default_kind["integer"],
                                   pointer=True,
                                   entity_decls=[self.dofmap_size_name(symtab,
                                                                       arg) +
                                                 "(:) => null()"]))

    def _declare_maps_stub(self, parent):
        '''
        Add declarations for all stencil maps to a kernel stub.

        :param parent: the node in the f2pygen AST representing the kernel \
                       stub routine.
        :type parent: :py:class:`psyclone.f2pygen.SubroutineGen`

        '''
        api_config = Config.get().api_conf("dynamo0.3")

        symtab = self._symbol_table
        for arg in self._kern_args:
            if arg.descriptor.stencil['type'] == "cross2d":
                parent.add(DeclGen(
                    parent, datatype="integer",
                    kind=api_config.default_kind["integer"], intent="in",
                    dimension=",".join([arg.function_space.ndf_name,
                                        self.max_branch_length_name(
                                            symtab, arg), "4"]),
                    entity_decls=[self.dofmap_name(symtab, arg)]))
            else:
                parent.add(DeclGen(
                    parent, datatype="integer",
                    kind=api_config.default_kind["integer"], intent="in",
                    dimension=",".join([arg.function_space.ndf_name,
                                        self.dofmap_size_name(symtab, arg)]),
                    entity_decls=[self.dofmap_name(symtab, arg)]))


class LFRicMeshProperties(DynCollection):
    '''
    Holds all information on the the mesh properties required by either an
    invoke or a kernel stub. Note that the creation of a suitable mesh
    object is handled in the `DynMeshes` class. This class merely deals with
    extracting the necessary properties from that object and providing them to
    kernels.

    :param node: kernel or invoke for which to manage mesh properties.
    :type node: :py:class:`psyclone.dynamo0p3.DynKern` or \
                :py:class:`psyclone.dynamo0p3.DynInvoke`

    '''
    def __init__(self, node):
        super(LFRicMeshProperties, self).__init__(node)

        # The (ordered) list of mesh properties required by this invoke or
        # kernel stub.
        self._properties = []

        for call in self._calls:
            if call.mesh:
                self._properties += [prop for prop in call.mesh.properties
                                     if prop not in self._properties]
            # Kernels that operate on the 'domain' need the number of columns,
            # excluding those in the halo.
            if call.iterates_over == "domain":
                if MeshProperty.NCELL_2D_NO_HALOS not in self._properties:
                    self._properties.append(MeshProperty.NCELL_2D_NO_HALOS)
            # Kernels performing CMA operations need the number of columns,
            # including those in the halo.
            if call.cma_operation:
                if MeshProperty.NCELL_2D not in self._properties:
                    self._properties.append(MeshProperty.NCELL_2D)

        # Store properties in symbol table
        for prop in self._properties:
            self._symbol_table.find_or_create_tag(prop.name.lower())

    def kern_args(self, stub=False, var_accesses=None):
        '''
        Provides the list of kernel arguments associated with the mesh
        properties that the kernel requires. Optionally adds variable
        access information if var_accesses is given.

        :param bool stub: whether or not we are generating code for a \
                          kernel stub.
        :param var_accesses: optional VariablesAccessInfo instance to store \
            the information about variable accesses.
        :type var_accesses: \
            :py:class:`psyclone.core.access_info.VariablesAccessInfo`

        :returns: the kernel arguments associated with the mesh properties.
        :rtype: list of str

        :raises InternalError: if the class has been constructed for an \
                               invoke rather than a single kernel call.
        :raises InternalError: if an unsupported mesh property is encountered.

        '''
        if not self._kernel:
            raise InternalError(
                "LFRicMeshProperties.kern_args() can only be called when "
                "LFRicMeshProperties has been instantiated for a kernel "
                "rather than an invoke.")

        arg_list = []

        for prop in self._properties:
            if prop == MeshProperty.ADJACENT_FACE:
                # Is this kernel already being passed the number of horizontal
                # faces of the reference element?
                has_nfaces = (
                    RefElementMetaData.Property.NORMALS_TO_HORIZONTAL_FACES
                    in self._kernel.reference_element.properties or
                    RefElementMetaData.Property.
                    OUTWARD_NORMALS_TO_HORIZONTAL_FACES
                    in self._kernel.reference_element.properties)
                if not has_nfaces:
                    name = self._symbol_table.find_or_create_tag(
                        "nfaces_re_h").name
                    arg_list.append(name)
                    if var_accesses is not None:
                        var_accesses.add_access(Signature(name),
                                                AccessType.READ, self._kernel)

                adj_face = self._symbol_table.find_or_create_tag(
                    "adjacent_face").name
                if var_accesses is not None:
                    # TODO #1320 Replace [1]
                    # The [1] just indicates that this variable is accessed
                    # as a rank 1 array. #1320 will improve this.
                    var_accesses.add_access(Signature(adj_face),
                                            AccessType.READ, self._kernel,
                                            [1])
                if not stub:
                    # This is a kernel call from within an invoke
                    cell_name = "cell"
                    if self._kernel.is_coloured():
                        colour_name = "colour"
                        cmap_name = "cmap"
                        adj_face += "(:,{0}({1}, {2}))".format(
                            cmap_name, colour_name, cell_name)
                    else:
                        adj_face += "(:,{0})".format(cell_name)
                arg_list.append(adj_face)

            else:
                raise InternalError(
                    "kern_args: found unsupported mesh property '{0}' when "
                    "generating arguments for kernel '{1}'. Only members of "
                    "the MeshProperty Enum are permitted "
                    "({2}).".format(
                        str(prop), self._kernel.name, list(MeshProperty)))

        return arg_list

    def _invoke_declarations(self, parent):
        '''
        Creates the necessary declarations for variables needed in order to
        provide mesh properties to a kernel call.

        :param parent: node in the f2pygen AST to which to add declarations.
        :type parent: :py:class:`psyclone.f2pygen.SubroutineGen`

        :raises InternalError: if this class has been instantiated for a \
                               kernel instead of an invoke.
        :raises InternalError: if an unsupported mesh property is found.

        '''
        api_config = Config.get().api_conf("dynamo0.3")

        if not self._invoke:
            raise InternalError(
                "_invoke_declarations() cannot be called because "
                "LFRicMeshProperties has been instantiated for a kernel and "
                "not an invoke.")

        for prop in self._properties:
            # The DynMeshes class will have created a mesh object so we
            # don't need to do that here.
            if prop == MeshProperty.ADJACENT_FACE:
                adj_face = self._symbol_table.find_or_create_tag(
                    "adjacent_face").name + "(:,:) => null()"
                parent.add(DeclGen(parent, datatype="integer",
                                   kind=api_config.default_kind["integer"],
                                   pointer=True, entity_decls=[adj_face]))
            elif prop == MeshProperty.NCELL_2D_NO_HALOS:
                name = self._symbol_table.find_or_create_tag(
                    "ncell_2d_no_halos").name
                parent.add(DeclGen(parent, datatype="integer",
                                   kind=api_config.default_kind["integer"],
                                   entity_decls=[name]))
            elif prop == MeshProperty.NCELL_2D:
                name = self._symbol_table.find_or_create_tag("ncell_2d").name
                parent.add(DeclGen(parent, datatype="integer",
                                   kind=api_config.default_kind["integer"],
                                   entity_decls=[name]))
            else:
                raise InternalError(
                    "Found unsupported mesh property '{0}' when "
                    "generating invoke declarations. Only members of "
                    "the MeshProperty Enum are permitted "
                    "({1}).".format(str(prop), list(MeshProperty)))

    def _stub_declarations(self, parent):
        '''
        Creates the necessary declarations for the variables needed in order
        to provide properties of the mesh in a kernel stub.

        :param parent: node in the f2pygen AST to which to add declarations.
        :type parent: :py:class:`psyclone.f2pygen.SubroutineGen`

        :raises InternalError: if the class has been instantiated for an \
                               invoke and not a kernel.
        :raises InternalError: if an unsupported mesh property is encountered.

        '''
        api_config = Config.get().api_conf("dynamo0.3")

        if not self._kernel:
            raise InternalError(
                "_stub_declarations() cannot be called because "
                "LFRicMeshProperties has been instantiated for an invoke and "
                "not a kernel.")

        for prop in self._properties:
            if prop == MeshProperty.ADJACENT_FACE:
                adj_face = self._symbol_table.find_or_create_tag(
                    "adjacent_face").name
                # 'nfaces_re_h' will have been declared by the
                # DynReferenceElement class.
                parent.add(
                    DeclGen(
                        parent, datatype="integer",
                        kind=api_config.default_kind["integer"],
                        dimension=self._symbol_table.find_or_create_tag(
                            "nfaces_re_h").name,
                        intent="in", entity_decls=[adj_face]))
            elif prop == MeshProperty.NCELL_2D:
                ncell_2d = self._symbol_table.find_or_create_tag("ncell_2d")
                parent.add(
                    DeclGen(parent, datatype="integer",
                            kind=api_config.default_kind["integer"],
                            intent="in", entity_decls=[ncell_2d.name]))
            else:
                raise InternalError(
                    "Found unsupported mesh property '{0}' when generating "
                    "declarations for kernel stub. Only members of the "
                    "MeshProperty Enum are permitted "
                    "({1})".format(str(prop), list(MeshProperty)))

    def initialise(self, parent):
        '''
        Creates the f2pygen nodes for the initialisation of properties of
        the mesh.

        :param parent: node in the f2pygen tree to which to add statements.
        :type parent: :py:class:`psyclone.f2pygen.SubroutineGen`

        :raises InternalError: if an unsupported mesh property is encountered.

        '''
        const = LFRicConstants()
        # Since colouring is applied via transformations, we have to check for
        # it now, rather than when this class was first constructed.
        need_colour_limits = False
        need_colour_halo_limits = False
        for call in self._calls:
            if call.is_coloured() and not call.is_intergrid:
                loop = call.parent.parent
                # Record whether or not this coloured loop accesses the halo.
                if loop.upper_bound_name in const.HALO_ACCESS_LOOP_BOUNDS:
                    need_colour_halo_limits = True
                else:
                    need_colour_limits = True

        if not self._properties and not (need_colour_limits or
                                         need_colour_halo_limits):
            # If no mesh properties are required and there's no colouring
            # (which requires a mesh object to lookup loop bounds) then we
            # need do nothing.
            return

        parent.add(CommentGen(parent, ""))
        parent.add(CommentGen(parent, " Initialise mesh properties"))
        parent.add(CommentGen(parent, ""))

        mesh = self._symbol_table.find_or_create_tag("mesh").name

        for prop in self._properties:
            if prop == MeshProperty.ADJACENT_FACE:
                adj_face = self._symbol_table.find_or_create_tag(
                    "adjacent_face").name
                parent.add(AssignGen(parent, pointer=True, lhs=adj_face,
                                     rhs=mesh+"%get_adjacent_face()"))

            elif prop == MeshProperty.NCELL_2D_NO_HALOS:
                name = self._symbol_table.find_or_create_tag(
                    "ncell_2d_no_halos").name
                parent.add(AssignGen(parent, lhs=name,
                                     rhs=mesh+"%get_last_edge_cell()"))

            elif prop == MeshProperty.NCELL_2D:
                name = self._symbol_table.find_or_create_tag("ncell_2d").name
                parent.add(AssignGen(parent, lhs=name,
                                     rhs=mesh+"%get_ncells_2d()"))
            else:
                raise InternalError(
                    f"Found unsupported mesh property '{str(prop)}' when "
                    f"generating initialisation code. Only members of the "
                    f"MeshProperty Enum are permitted ({list(MeshProperty)})")

        if need_colour_halo_limits:
            lhs = self._symbol_table.find_or_create_tag(
                "last_halo_cell_all_colours").name
            rhs = f"{mesh}%get_last_halo_cell_all_colours()"
            parent.add(AssignGen(parent, lhs=lhs, rhs=rhs))
        if need_colour_limits:
            lhs = self._symbol_table.find_or_create_tag(
                "last_edge_cell_all_colours").name
            rhs = f"{mesh}%get_last_edge_cell_all_colours()"
            parent.add(AssignGen(parent, lhs=lhs, rhs=rhs))


class DynReferenceElement(DynCollection):
    '''
    Holds all information on the properties of the Reference Element
    required by an Invoke or a Kernel stub.

    :param node: Kernel or Invoke for which to manage Reference-Element \
                 properties.
    :type node: :py:class:`psyclone.dynamo0p3.DynKern` or \
                :py:class:`psyclone.dynamo0p3.DynInvoke`

    :raises InternalError: if an unsupported reference-element property \
                           is encountered.

    '''
    # pylint: disable=too-many-instance-attributes
    def __init__(self, node):
        # pylint: disable=too-many-branches, too-many-statements
        super(DynReferenceElement, self).__init__(node)

        # Create a union of the reference-element properties required by all
        # kernels in this invoke. Use a list to preserve the order in the
        # kernel metadata (in the case of a kernel stub) and remove duplicate
        # entries by using OrderedDict.
        self._properties = []
        self._nfaces_h_required = False

        for call in self._calls:
            if call.reference_element:
                self._properties.extend(call.reference_element.properties)
            if call.mesh and call.mesh.properties:
                # If a kernel requires a property of the mesh then it will
                # also require the number of horizontal faces of the
                # reference element.
                self._nfaces_h_required = True

        if not (self._properties or self._nfaces_h_required):
            return

        if self._properties:
            self._properties = list(OrderedDict.fromkeys(self._properties))

        symtab = self._symbol_table

        # Create and store a name for the reference element object
        self._ref_elem_name = \
            symtab.find_or_create_tag("reference_element").name

        # Initialise names for the properties of the reference element object:
        # Number of horizontal/vertical/all faces,
        self._nfaces_h_name = ""
        self._nfaces_v_name = ""
        self._nfaces_name = ""
        # Horizontal normals to faces,
        self._horiz_face_normals_name = ""
        self._horiz_face_out_normals_name = ""
        # Vertical normals to faces,
        self._vert_face_normals_name = ""
        self._vert_face_out_normals_name = ""
        # All normals to faces.
        self._face_normals_name = ""
        self._face_out_normals_name = ""

        # Store argument properties for kernel calls and stub declarations
        # and argument list
        self._arg_properties = OrderedDict()

        # Populate and check reference element properties
        # Provide no. of horizontal faces if required
        if (RefElementMetaData.Property.NORMALS_TO_HORIZONTAL_FACES
                in self._properties or
                RefElementMetaData.Property.OUTWARD_NORMALS_TO_HORIZONTAL_FACES
                in self._properties or
                self._nfaces_h_required):
            self._nfaces_h_name = symtab.find_or_create_tag("nfaces_re_h").name
        # Provide no. of vertical faces if required
        if (RefElementMetaData.Property.NORMALS_TO_VERTICAL_FACES
                in self._properties or
                RefElementMetaData.Property.OUTWARD_NORMALS_TO_VERTICAL_FACES
                in self._properties):
            self._nfaces_v_name = symtab.find_or_create_tag("nfaces_re_v").name
        # Provide no. of all faces if required
        if (RefElementMetaData.Property.NORMALS_TO_FACES
                in self._properties or
                RefElementMetaData.Property.OUTWARD_NORMALS_TO_FACES
                in self._properties):
            self._nfaces_name = symtab.find_or_create_tag("nfaces_re").name

        # Now the arrays themselves, in the order specified in the
        # kernel metadata (in the case of a kernel stub)
        for prop in self._properties:
            # Provide horizontal normals to faces
            if prop == \
               RefElementMetaData.Property.NORMALS_TO_HORIZONTAL_FACES:
                self._horiz_face_normals_name = \
                    symtab.find_or_create_tag("normals_to_horiz_faces").name
                if self._horiz_face_normals_name not in self._arg_properties:
                    self._arg_properties[self._horiz_face_normals_name] = \
                         self._nfaces_h_name
            # Provide horizontal normals to "outward" faces
            elif prop == (RefElementMetaData.Property.
                          OUTWARD_NORMALS_TO_HORIZONTAL_FACES):
                self._horiz_face_out_normals_name = \
                    symtab.find_or_create_tag("out_normals_to_horiz_faces") \
                          .name
                if self._horiz_face_out_normals_name not in \
                   self._arg_properties:
                    self._arg_properties[self._horiz_face_out_normals_name] = \
                         self._nfaces_h_name
            elif prop == (RefElementMetaData.Property.
                          NORMALS_TO_VERTICAL_FACES):
                self._vert_face_normals_name = \
                    symtab.find_or_create_tag("normals_to_vert_faces").name
                if self._vert_face_normals_name not in self._arg_properties:
                    self._arg_properties[self._vert_face_normals_name] = \
                         self._nfaces_v_name
            # Provide vertical normals to "outward" faces
            elif prop == (RefElementMetaData.Property.
                          OUTWARD_NORMALS_TO_VERTICAL_FACES):
                self._vert_face_out_normals_name = \
                    symtab.find_or_create_tag("out_normals_to_vert_faces").name
                if self._vert_face_out_normals_name not in \
                   self._arg_properties:
                    self._arg_properties[self._vert_face_out_normals_name] = \
                        self._nfaces_v_name
            # Provide normals to all faces
            elif prop == RefElementMetaData.Property.NORMALS_TO_FACES:
                self._face_normals_name = \
                    symtab.find_or_create_tag("normals_to_faces").name
                if self._face_normals_name not in self._arg_properties:
                    self._arg_properties[self._face_normals_name] = \
                        self._nfaces_name
            # Provide vertical normals to all "outward" faces
            elif prop == RefElementMetaData.Property.OUTWARD_NORMALS_TO_FACES:
                self._face_out_normals_name = \
                    symtab.find_or_create_tag("out_normals_to_faces").name
                if self._face_out_normals_name not in \
                   self._arg_properties:
                    self._arg_properties[self._face_out_normals_name] = \
                        self._nfaces_name
            else:
                raise InternalError(
                    "Unsupported reference-element property ('{0}') found "
                    "when generating arguments for kernel '{1}'. Supported "
                    "properties are: {2}".format(
                        str(prop), self._kernel.name,
                        [str(sprop) for sprop in RefElementMetaData.Property]))

    def kern_args(self):
        '''
        Create argument list for kernel call and stub.

        :return: kernel call/stub arguments.
        :rtype: list

        '''
        argdict = self._arg_properties
        # Remove duplicate "nfaces" by using OrderedDict
        nfaces = list(OrderedDict.fromkeys(argdict.values()))
        kern_args = nfaces + list(argdict.keys())
        return kern_args

    def _invoke_declarations(self, parent):
        '''
        Create the necessary declarations for the variables needed in order
        to provide properties of the reference element in a Kernel call.

        :param parent: node in the f2pygen AST to which to add declarations.
        :type parent: :py:class:`psyclone.f2pygen.SubroutineGen`

        '''
        # Get the list of the required scalars
        if self._properties:
            # remove duplicates with an OrderedDict
            nface_vars = list(OrderedDict.fromkeys(
                self._arg_properties.values()))
        elif self._nfaces_h_required:
            # We only need the number of 'horizontal' faces
            nface_vars = [self._nfaces_h_name]
        else:
            # No reference-element properties required
            return

        api_config = Config.get().api_conf("dynamo0.3")
        const = LFRicConstants()

        refelem_type = const.REFELEMENT_TYPE_MAP["refelement"]["type"]
        refelem_mod = const.REFELEMENT_TYPE_MAP["refelement"]["module"]
        parent.add(UseGen(parent, name=refelem_mod, only=True,
                          funcnames=[refelem_type]))
        parent.add(
            TypeDeclGen(parent, pointer=True, is_class=True,
                        datatype=refelem_type,
                        entity_decls=[self._ref_elem_name + " => null()"]))

        parent.add(DeclGen(parent, datatype="integer",
                           kind=api_config.default_kind["integer"],
                           entity_decls=nface_vars))

        if not self._properties:
            # We only need the number of horizontal faces so we're done
            return

        # Declare the necessary arrays
        array_decls = [arr + "(:,:)" for arr in self._arg_properties.keys()]
        my_kind = api_config.default_kind["real"]
        parent.add(DeclGen(parent, datatype="real", kind=my_kind,
                           allocatable=True, entity_decls=array_decls))
        const_mod = const.UTILITIES_MOD_MAP["constants"]["module"]
        const_mod_list = self._invoke.invokes.psy. \
            infrastructure_modules[const_mod]
        if my_kind not in const_mod_list:
            const_mod_list.append(my_kind)

    def _stub_declarations(self, parent):
        '''
        Create the necessary declarations for the variables needed in order
        to provide properties of the reference element in a Kernel stub.

        :param parent: node in the f2pygen AST to which to add declarations.
        :type parent: :py:class:`psyclone.f2pygen.SubroutineGen`

        '''
        api_config = Config.get().api_conf("dynamo0.3")

        if not (self._properties or self._nfaces_h_required):
            return

        # Declare the necessary scalars (duplicates are ignored by parent.add)
        scalars = list(self._arg_properties.values())
        nfaces_h = self._symbol_table.find_or_create_tag("nfaces_re_h").name
        if self._nfaces_h_required and nfaces_h not in scalars:
            scalars.append(nfaces_h)

        for nface in scalars:
            parent.add(DeclGen(parent, datatype="integer",
                               kind=api_config.default_kind["integer"],
                               intent="in", entity_decls=[nface]))

        # Declare the necessary arrays
        for arr in self._arg_properties.keys():
            dimension = ",".join(["3", self._arg_properties[arr]])
            parent.add(DeclGen(parent, datatype="real",
                               kind=api_config.default_kind["real"],
                               intent="in", dimension=dimension,
                               entity_decls=[arr]))

    def initialise(self, parent):
        '''
        Creates the f2pygen nodes representing the necessary initialisation
        code for properties of the reference element.

        :param parent: node in the f2pygen tree to which to add statements.
        :type parent: :py:class:`psyclone.f2pygen.SubroutineGen`

        '''
        if not (self._properties or self._nfaces_h_required):
            return

        parent.add(CommentGen(parent, ""))
        parent.add(
            CommentGen(parent,
                       " Get the reference element and query its properties"))
        parent.add(CommentGen(parent, ""))

        mesh_obj_name = self._symbol_table.find_or_create_tag("mesh").name
        parent.add(AssignGen(parent, pointer=True, lhs=self._ref_elem_name,
                             rhs=mesh_obj_name+"%get_reference_element()"))

        if self._nfaces_h_name:
            parent.add(
                AssignGen(parent, lhs=self._nfaces_h_name,
                          rhs=self._ref_elem_name +
                          "%get_number_horizontal_faces()"))
        if self._nfaces_v_name:
            parent.add(
                AssignGen(
                    parent, lhs=self._nfaces_v_name,
                    rhs=self._ref_elem_name + "%get_number_vertical_faces()"))

        if self._nfaces_name:
            parent.add(
                AssignGen(
                    parent, lhs=self._nfaces_name,
                    rhs=self._ref_elem_name + "%get_number_faces()"))

        if self._horiz_face_normals_name:
            parent.add(
                CallGen(parent,
                        name="{0}%get_normals_to_horizontal_faces({1})".format(
                            self._ref_elem_name,
                            self._horiz_face_normals_name)))

        if self._horiz_face_out_normals_name:
            parent.add(
                CallGen(
                    parent,
                    name="{0}%get_outward_normals_to_horizontal_faces({1})".
                    format(self._ref_elem_name,
                           self._horiz_face_out_normals_name)))

        if self._vert_face_normals_name:
            parent.add(
                CallGen(parent,
                        name="{0}%get_normals_to_vertical_faces({1})".format(
                            self._ref_elem_name,
                            self._vert_face_normals_name)))

        if self._vert_face_out_normals_name:
            parent.add(
                CallGen(
                    parent,
                    name="{0}%get_outward_normals_to_vertical_faces({1})".
                    format(self._ref_elem_name,
                           self._vert_face_out_normals_name)))

        if self._face_normals_name:
            parent.add(
                CallGen(parent,
                        name="{0}%get_normals_to_faces({1})".format(
                            self._ref_elem_name,
                            self._face_normals_name)))

        if self._face_out_normals_name:
            parent.add(
                CallGen(
                    parent,
                    name="{0}%get_outward_normals_to_faces({1})".
                    format(self._ref_elem_name,
                           self._face_out_normals_name)))


class DynDofmaps(DynCollection):
    '''
    Holds all information on the dofmaps (including column-banded and
    indirection) required by an invoke.

    :param node: Kernel or Invoke for which to manage dofmaps.
    :type node: :py:class:`psyclone.dynamo0p3.DynKern` or \
                :py:class:`psyclone.dynamo0p3.DynInvoke`

    '''
    def __init__(self, node):
        # pylint: disable=too-many-branches
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
            # We only need a dofmap if the kernel operates on a cell_column
            # or the domain.
            if call.iterates_over in ["cell_column", "domain"]:
                for unique_fs in call.arguments.unique_fss:
                    # We only need a dofmap if there is a *field* on this
                    # function space. If there is then we use it to look
                    # up the dofmap.
                    fld_arg = unique_fs.field_on_space(call.arguments)
                    if fld_arg:
                        map_name = unique_fs.map_name
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

                    map_name = \
                        cma_args[0].function_space_to.cbanded_map_name
                    if map_name not in self._unique_cbanded_maps:
                        self._unique_cbanded_maps[map_name] = {
                            "argument": cma_args[0],
                            "direction": "to"}
                    map_name = \
                        cma_args[0].function_space_from.cbanded_map_name
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

                    map_name = cma_args[0].function_space_to\
                        .cma_indirection_map_name
                    if map_name not in self._unique_indirection_maps:
                        self._unique_indirection_maps[map_name] = {
                            "argument": cma_args[0],
                            "direction": "to"}
                    map_name = cma_args[0].function_space_from\
                        .cma_indirection_map_name
                    if map_name not in self._unique_indirection_maps:
                        self._unique_indirection_maps[map_name] = {
                            "argument": cma_args[0],
                            "direction": "from"}

    def initialise(self, parent):
        ''' Generates the calls to the LFRic infrastructure that
        look-up the necessary dofmaps. Adds these calls as children
        of the supplied parent node. This must be an appropriate
        f2pygen object. '''

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
        api_config = Config.get().api_conf("dynamo0.3")

        # Function space dofmaps
        decl_map_names = \
            [dmap+"(:,:) => null()" for dmap in sorted(self._unique_fs_maps)]

        if decl_map_names:
            parent.add(DeclGen(parent, datatype="integer",
                               kind=api_config.default_kind["integer"],
                               pointer=True, entity_decls=decl_map_names))

        # Column-banded dofmaps
        decl_bmap_names = \
            [dmap+"(:,:) => null()" for dmap in self._unique_cbanded_maps]
        if decl_bmap_names:
            parent.add(DeclGen(parent, datatype="integer",
                               kind=api_config.default_kind["integer"],
                               pointer=True, entity_decls=decl_bmap_names))

        # CMA operator indirection dofmaps
        decl_ind_map_names = \
            [dmap+"(:) => null()" for dmap in self._unique_indirection_maps]
        if decl_ind_map_names:
            parent.add(DeclGen(parent, datatype="integer",
                               kind=api_config.default_kind["integer"],
                               pointer=True, entity_decls=decl_ind_map_names))

    def _stub_declarations(self, parent):
        '''
        Add dofmap-related declarations to a Kernel stub.

        :param parent: node in the f2pygen AST representing the Kernel stub.
        :type parent: :py:class:`psyclone.f2pygen.SubroutineGen`

        '''
        api_config = Config.get().api_conf("dynamo0.3")

        # Function space dofmaps
        for dmap in sorted(self._unique_fs_maps):
            # We declare ndf first as some compilers require this
            ndf_name = \
                self._unique_fs_maps[dmap].function_space.ndf_name
            parent.add(DeclGen(parent, datatype="integer",
                               kind=api_config.default_kind["integer"],
                               intent="in", entity_decls=[ndf_name]))
            parent.add(DeclGen(parent, datatype="integer",
                               kind=api_config.default_kind["integer"],
                               intent="in", dimension=ndf_name,
                               entity_decls=[dmap]))
        # Column-banded dofmaps
        for dmap, cma in self._unique_cbanded_maps.items():
            if cma["direction"] == "to":
                ndf_name = cma["argument"].function_space_to.ndf_name
            elif cma["direction"] == "from":
                ndf_name = cma["argument"].function_space_from.ndf_name
            else:
                raise InternalError(
                    "Invalid direction ('{0}') found for CMA operator when "
                    "collecting column-banded dofmaps. Should "
                    "be either 'to' or 'from'.".format(cma["direction"]))
            parent.add(DeclGen(parent, datatype="integer",
                               kind=api_config.default_kind["integer"],
                               intent="in", entity_decls=[ndf_name]))
            parent.add(DeclGen(parent, datatype="integer",
                               kind=api_config.default_kind["integer"],
                               intent="in",
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
            parent.add(DeclGen(parent, datatype="integer",
                               kind=api_config.default_kind["integer"],
                               intent="in", entity_decls=[dim_name]))
            parent.add(DeclGen(parent, datatype="integer",
                               kind=api_config.default_kind["integer"],
                               intent="in", dimension=dim_name,
                               entity_decls=[dmap]))


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

            # We need ndf for a space if a kernel operates on cell-columns,
            # has a field or operator on that space and is not a
            # CMA kernel performing a matrix-matrix operation.
            if self._invoke and not self._dofs_only or \
               self._kernel and self._kernel.cma_operation != "matrix-matrix":
                self._var_list.append(function_space.ndf_name)

            # If there is a field on this space then add undf to list
            # to declare later. However, if the invoke contains only
            # kernels that operate on dofs and distributed memory is
            # enabled then the number of dofs is obtained from the
            # field proxy and undf is not required.
            if self._invoke and self._invoke.field_on_space(function_space):
                if not (self._dofs_only and Config.get().distributed_memory):
                    self._var_list.append(function_space.undf_name)
            elif self._kernel and \
                    function_space.field_on_space(self._kernel.arguments):
                self._var_list.append(function_space.undf_name)

    def _stub_declarations(self, parent):
        '''
        Add function-space-related declarations to a Kernel stub.

        :param parent: the node in the f2pygen AST representing the kernel \
                       stub to which to add declarations.
        :type parent: :py:class:`psyclone.f2pygen.SubroutineGen`

        '''
        api_config = Config.get().api_conf("dynamo0.3")

        if self._var_list:
            # Declare ndf and undf for all function spaces
            parent.add(DeclGen(parent, datatype="integer",
                               kind=api_config.default_kind["integer"],
                               intent="in", entity_decls=self._var_list))

    def _invoke_declarations(self, parent):
        '''
        Add function-space-related declarations to a PSy-layer routine.

        :param parent: the node in the f2pygen AST to which to add \
                       declarations.
        :type parent: :py:class:`psyclone.f2pygen.SubroutineGen`

        '''
        api_config = Config.get().api_conf("dynamo0.3")

        if self._var_list:
            # Declare ndf and undf for all function spaces
            parent.add(DeclGen(parent, datatype="integer",
                               kind=api_config.default_kind["integer"],
                               entity_decls=self._var_list))

    def initialise(self, parent):
        '''
        Create the code that initialises function-space quantities.

        :param parent: the node in the f2pygen AST representing the PSy-layer \
                       routine.
        :type parent: :py:class:`psyclone.f2pygen.SubroutineGen`

        '''
        # Loop over all unique function spaces used by the kernels in
        # the invoke
        for function_space in self._function_spaces:
            # Initialise information associated with this function space.
            # If we have 1+ kernels that operate on cell-columns then we
            # will need ndf and undf. If we don't then we only need undf
            # (for the upper bound of the loop over dofs) if we're not
            # doing DM.
            if not (self._dofs_only and Config.get().distributed_memory):
                parent.add(CommentGen(parent, ""))
                parent.add(CommentGen(parent,
                                      " Initialise number of DoFs for " +
                                      function_space.mangled_name))
                parent.add(CommentGen(parent, ""))

            # Find argument proxy name used to dereference the argument
            arg = self._invoke.arg_for_funcspace(function_space)
            name = arg.proxy_name_indexed
            # Initialise ndf for this function space.
            if not self._dofs_only:
                ndf_name = function_space.ndf_name
                parent.add(AssignGen(parent, lhs=ndf_name,
                                     rhs=name +
                                     "%" + arg.ref_name(function_space) +
                                     "%get_ndf()"))
            # If there is a field on this space then initialise undf
            # for this function space. However, if the invoke contains
            # only kernels that operate on dofs and distributed
            # memory is enabled then the number of dofs is obtained
            # from the field proxy and undf is not required.
            if not (self._dofs_only and Config.get().distributed_memory):
                if self._invoke.field_on_space(function_space):
                    undf_name = function_space.undf_name
                    parent.add(AssignGen(parent, lhs=undf_name,
                                         rhs=name + "%" +
                                         arg.ref_name(function_space) +
                                         "%get_undf()"))


class LFRicFields(DynCollection):
    '''
    Manages the declarations for all field arguments required by an Invoke
    or Kernel stub.

    '''
    def _invoke_declarations(self, parent):
        '''
        Add field-related declarations to the PSy-layer routine.
        Note: PSy layer in LFRic does not modify the field objects. Hence,
        their Fortran intents are always in (the data updated in the kernels
        is only pointed to from the field object and is thus not a part of
        the object).

        :param parent: the node in the f2pygen AST representing the PSy-layer \
                       routine to which to add declarations.
        :type parent: :py:class:`psyclone.f2pygen.SubroutineGen`

        :raises InternalError: for unsupported intrinsic types of field \
                               argument data.
        :raises GenerationError: if the same field has different data \
                                 types in different kernel calls within \
                                 the same Invoke.

        '''
        # Create dict of all field arguments for checks
        const = LFRicConstants()
        fld_args = self._invoke.unique_declarations(
            argument_types=const.VALID_FIELD_NAMES)
        # Filter field arguments by intent and intrinsic type
        real_field_args = self._invoke.unique_declarations(
            argument_types=const.VALID_FIELD_NAMES,
            intrinsic_type=const.MAPPING_DATA_TYPES["gh_real"])
        int_field_args = self._invoke.unique_declarations(
            argument_types=const.VALID_FIELD_NAMES,
            intrinsic_type=const.MAPPING_DATA_TYPES["gh_integer"])

        # Create lists of field names for real- and integer-valued fields
        fld_arg_list = [arg.declaration_name for arg in fld_args]
        real_field_arg_list = [arg.declaration_name for arg in real_field_args]
        int_field_arg_list = [arg.declaration_name for arg in int_field_args]
        # Check for unsupported intrinsic types
        fld_inv = (set(fld_arg_list) -
                   set(real_field_arg_list).union(set(int_field_arg_list)))
        if fld_inv:
            raise InternalError(
                "Found unsupported intrinsic types for the field "
                "arguments {0} to Invoke '{1}'. Supported types are {2}.".
                format(list(fld_inv), self._invoke.name,
                       const.VALID_FIELD_INTRINSIC_TYPES))
        # Check that the same field name is not found in both real and
        # integer field lists (for instance if passed to one kernel as a
        # real-valued and to another kernel as an integer-valued field)
        fld_multi_type = \
            set(real_field_arg_list).intersection(set(int_field_arg_list))
        if fld_multi_type:
            raise GenerationError(
                "Field argument(s) {0} in Invoke '{1}' have different "
                "metadata for data type ({2}) in different kernels. "
                "This is invalid.".
                format(list(fld_multi_type), self._invoke.name,
                       const.VALID_FIELD_DATA_TYPES))

        # Create a field argument map that splits the (real and
        # integer) fields into their different datatypes.
        field_datatype_map = OrderedDict()
        for arg in real_field_args + int_field_args:
            try:
                field_datatype_map[
                    (arg.data_type, arg.module_name)].append(arg)
            except KeyError:
                # This datatype has not been seen before so create a
                # new entry
                field_datatype_map[(arg.data_type, arg.module_name)] = [arg]

        # Add the Invoke subroutine argument declarations for the
        # different fields types. They are declared as intent "in" as
        # they contain a pointer to the data that is modified.
        for fld_type, fld_mod in field_datatype_map:
            args = field_datatype_map[(fld_type, fld_mod)]
            arg_list = [arg.declaration_name for arg in args]
            parent.add(TypeDeclGen(parent, datatype=fld_type,
                                   entity_decls=arg_list,
                                   intent="in"))
            (self._invoke.invokes.psy.
             infrastructure_modules[fld_mod].add(fld_type))

    def _stub_declarations(self, parent):
        '''
        Add field-related declarations to a Kernel stub.

        :param parent: the node in the f2pygen AST representing the Kernel \
                       stub to which to add declarations.
        :type parent: :py:class:`psyclone.f2pygen.SubroutineGen`

        :raises InternalError: for an unsupported data type of field \
                               argument data.

        '''
        const = LFRicConstants()

        fld_args = psyGen.args_filter(
            self._kernel.args, arg_types=const.VALID_FIELD_NAMES)
        for fld in fld_args:
            undf_name = fld.function_space.undf_name
            fld_dtype = fld.intrinsic_type
            fld_kind = fld.precision

            # Check for invalid descriptor data type
            fld_ad_dtype = fld.descriptor.data_type
            if fld_ad_dtype not in const.VALID_FIELD_DATA_TYPES:
                raise InternalError(
                    "Found an unsupported data type '{0}' in kernel "
                    "stub declarations for the field argument '{1}'. "
                    "Supported types are {2}.".
                    format(fld_ad_dtype, fld.declaration_name,
                           const.VALID_FIELD_DATA_TYPES))

            if fld.vector_size > 1:
                for idx in range(1, fld.vector_size+1):
                    text = (fld.name + "_" +
                            fld.function_space.mangled_name +
                            "_v" + str(idx))
                    parent.add(
                        DeclGen(parent, datatype=fld_dtype, kind=fld_kind,
                                dimension=undf_name,
                                intent=fld.intent, entity_decls=[text]))
            else:
                parent.add(
                    DeclGen(parent, datatype=fld_dtype, kind=fld_kind,
                            intent=fld.intent,
                            dimension=undf_name,
                            entity_decls=[fld.name + "_" +
                                          fld.function_space.mangled_name]))


class LFRicRunTimeChecks(DynCollection):
    '''Handle declarations and code generation for run-time checks. This
    is not used in the stub generator.

    '''

    def _invoke_declarations(self, parent):
        '''Insert declarations of all data and functions required by the
        run-time checks code into the PSy layer.

        :param parent: the node in the f2pygen AST representing the PSy- \
                       layer routine.
        :type parent: :py:class:`psyclone.f2pygen.SubroutineGen`

        '''
        if Config.get().api_conf("dynamo0.3").run_time_checks:
            # Only add if run-time checks are requested
            const = LFRicConstants()
            parent.add(
                UseGen(parent, name=const.
                       FUNCTION_SPACE_TYPE_MAP["fs_continuity"]["module"]))
            parent.add(UseGen(parent, name=const.
                              UTILITIES_MOD_MAP["logging"]["module"],
                              only=True,
                              funcnames=["log_event", "LOG_LEVEL_ERROR"]))

    def _check_field_fs(self, parent):
        '''
        Internal method that adds run-time checks to make sure that the
        field's function space is consistent with the appropriate
        kernel metadata function spaces.

        :param parent: the node in the f2pygen AST representing the PSy- \
                       layer routine.
        :type parent: :py:class:`psyclone.f2pygen.SubroutineGen`

        '''
        parent.add(CommentGen(
            parent, " Check field function space and kernel metadata "
            "function spaces are compatible"))

        # When issue #753 is addressed (with isue #79 helping further)
        # we may know some or all field function spaces statically. If
        # so, we should remove these from the fields to check at run
        # time (as they will have already been checked at code
        # generation time).

        const = LFRicConstants()
        existing_checks = []
        for kern_call in self._invoke.schedule.kernels():
            for arg in kern_call.arguments.args:
                if not arg.is_field:
                    # This check is limited to fields
                    continue
                fs_name = arg.function_space.orig_name
                field_name = arg.name_indexed
                if fs_name in const.VALID_ANY_SPACE_NAMES:
                    # We don't need to check validity of a field's
                    # function space if the metadata specifies
                    # any_space as this means that all spaces are
                    # valid.
                    continue
                if (fs_name, field_name) in existing_checks:
                    # This particular combination has already been
                    # checked.
                    continue
                existing_checks.append((fs_name, field_name))

                if fs_name in const.VALID_ANY_DISCONTINUOUS_SPACE_NAMES:
                    # We need to check against all discontinuous
                    # function spaces
                    function_space_names = const.DISCONTINUOUS_FUNCTION_SPACES
                elif fs_name == "any_w2":
                    # We need to check against all any_w2 function
                    # spaces
                    function_space_names = const.ANY_W2_FUNCTION_SPACES
                else:
                    # We need to check against a specific function space
                    function_space_names = [fs_name]

                if_condition = " .and. ".join(
                    ["{0}%which_function_space() /= {1}".format(
                        field_name, name.upper())
                     for name in function_space_names])
                if_then = IfThenGen(parent, if_condition)
                call_abort = CallGen(
                    if_then, "log_event(\"In alg '{0}' invoke '{1}', the "
                    "field '{2}' is passed to kernel '{3}' but its function "
                    "space is not compatible with the function space "
                    "specified in the kernel metadata '{4}'.\", "
                    "LOG_LEVEL_ERROR)"
                    "".format(self._invoke.invokes.psy.orig_name,
                              self._invoke.name, arg.name,
                              kern_call.name, fs_name))
                if_then.add(call_abort)
                parent.add(if_then)

    def _check_field_ro(self, parent):
        '''
        Internal method that adds runtime checks to make sure that if the
        field is on a read-only function space then the associated
        kernel metadata does not specify that the field is modified.

        As we make use of the LFRic infrastructure halo exchange
        function, there is no need to check whether the halo of a
        read-only field is clean (which it should always be) as the
        LFric halo-exchange will raise an exception if it is called
        with a read-only field.

        Whilst the LFRic infrastructure halo exchange would also
        indirectly pick up a readonly field being modified, it would
        not be picked up where the error occured. Therefore adding
        checks here is still useful.

        :param parent: the node in the f2pygen AST representing the PSy- \
                       layer routine.
        :type parent: :py:class:`psyclone.f2pygen.SubroutineGen`

        '''
        # When issue #753 is addressed (with isue #79 helping further)
        # we may know some or all field function spaces statically. If
        # so, we should remove these from the fields to check at run
        # time (as they will have already been checked at code
        # generation time).

        # Create a list of modified fields
        modified_fields = []
        for call in self._invoke.schedule.kernels():
            for arg in call.arguments.args:
                if (arg.text and arg.is_field and
                        arg.access != AccessType.READ and
                        not [entry for entry in modified_fields if
                             entry[0].name == arg.name]):
                    modified_fields.append((arg, call))
        if modified_fields:
            parent.add(CommentGen(
                parent, " Check that read-only fields are not modified"))
        for field, call in modified_fields:
            if_then = IfThenGen(
                parent, "{0}%vspace%is_readonly()".format(
                    field.proxy_name_indexed))
            call_abort = CallGen(
                if_then, "log_event(\"In alg '{0}' invoke '{1}', field "
                "'{2}' is on a read-only function space but is modified "
                "by kernel '{3}'.\", LOG_LEVEL_ERROR)"
                "".format(self._invoke.invokes.psy.orig_name,
                          self._invoke.name, field.name, call.name))
            if_then.add(call_abort)
            parent.add(if_then)

    def initialise(self, parent):
        '''Add runtime checks to make sure that the arguments being passed
        from the algorithm layer are consistent with the metadata
        specified in the associated kernels. Currently checks are
        limited to ensuring that field function spaces are consistent
        with the associated kernel function-space metadata.

        :param parent: the node in the f2pygen AST representing the PSy- \
                       layer routine.
        :type parent: :py:class:`psyclone.f2pygen.SubroutineGen`

        '''
        if not Config.get().api_conf("dynamo0.3").run_time_checks:
            # Run-time checks are not requested.
            return

        parent.add(CommentGen(parent, ""))
        parent.add(CommentGen(parent, " Perform run-time checks"))
        parent.add(CommentGen(parent, ""))

        # Check that field function spaces are compatible with the
        # function spaces specified in the kernel metadata.
        self._check_field_fs(parent)

        # Check that fields on read-only function spaces are not
        # passed into a kernel where the kernel metadata specifies
        # that the field will be modified.
        self._check_field_ro(parent)

        # These checks should be expanded. Issue #768 suggests
        # extending function space checks to operators.


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
        # Declarations of real and integer field proxies
        const = LFRicConstants()
        # Filter field arguments by intrinsic type
        real_field_args = self._invoke.unique_declarations(
            argument_types=const.VALID_FIELD_NAMES,
            intrinsic_type=const.MAPPING_DATA_TYPES["gh_real"])
        int_field_args = self._invoke.unique_declarations(
            argument_types=const.VALID_FIELD_NAMES,
            intrinsic_type=const.MAPPING_DATA_TYPES["gh_integer"])

        # Create a field argument map that splits the (real and
        # integer) fields into their different datatypes for their
        # proxy's
        field_datatype_map = OrderedDict()
        for arg in real_field_args + int_field_args:
            try:
                field_datatype_map[
                    (arg.proxy_data_type, arg.module_name)].append(arg)
            except KeyError:
                # This datatype has not been seen before so create a
                # new entry
                field_datatype_map[
                    (arg.proxy_data_type, arg.module_name)] = [arg]

        # Add the Invoke subroutine declarations for the different
        # field-type proxies
        for (fld_type, fld_mod), args in field_datatype_map.items():
            arg_list = [arg.proxy_declaration_name for arg in args]
            parent.add(TypeDeclGen(parent, datatype=fld_type,
                                   entity_decls=arg_list))
            (self._invoke.invokes.psy.
             infrastructure_modules[fld_mod].add(fld_type))

        # Declarations of LMA operator proxies
        op_args = self._invoke.unique_declarations(
            argument_types=["gh_operator"])
        # Filter operators by their proxy datatype
        operators_datatype_map = OrderedDict()
        for op_arg in op_args:
            try:
                operators_datatype_map[op_arg.proxy_data_type].append(op_arg)
            except KeyError:
                # This proxy datatype has not been seen before so
                # create new entry
                operators_datatype_map[op_arg.proxy_data_type] = [op_arg]
        # Declare the operator proxies
        for operator_datatype, operators_list in \
                operators_datatype_map.items():
            operators_names = [arg.proxy_declaration_name for
                               arg in operators_list]
            parent.add(TypeDeclGen(parent, datatype=operator_datatype,
                                   entity_decls=operators_names))
            op_mod = operators_list[0].module_name
            (self._invoke.invokes.psy.infrastructure_modules[op_mod].
             add(operator_datatype))

        # Declarations of CMA operator proxies
        cma_op_args = self._invoke.unique_declarations(
            argument_types=["gh_columnwise_operator"])
        cma_op_proxy_decs = [arg.proxy_declaration_name for
                             arg in cma_op_args]
        if cma_op_proxy_decs:
            op_type = cma_op_args[0].proxy_data_type
            op_mod = cma_op_args[0].module_name
            parent.add(TypeDeclGen(parent,
                                   datatype=op_type,
                                   entity_decls=cma_op_proxy_decs))
            (self._invoke.invokes.psy.infrastructure_modules[op_mod].
             add(op_type))

    def initialise(self, parent):
        '''
        Insert code into the PSy layer to initialise all necessary proxies.

        :param parent: node in the f2pygen AST representing the PSy-layer \
                       routine.
        :type parent: :py:class:`psyclone.f2pygen.SubroutineGen`

        '''
        parent.add(CommentGen(parent, ""))
        parent.add(CommentGen(parent,
                              " Initialise field and/or operator proxies"))
        parent.add(CommentGen(parent, ""))
        for arg in self._invoke.psy_unique_vars:
            # We don't have proxies for scalars
            if arg.is_scalar:
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
    Handles all entities required by kernels that operate on cell-columns.

    :param kern_or_invoke: the Kernel or Invoke for which to manage cell \
                           iterators.
    :type kern_or_invoke: :py:class:`psyclone.dynamo0p3.DynKern` or \
                          :py:class:`psyclone.dynamo0p3.DynInvoke`

    : raises GenerationError: if an Invoke has no field or operator arguments.

    '''
    def __init__(self, kern_or_invoke):
        super(DynCellIterators, self).__init__(kern_or_invoke)

        self._nlayers_name = \
            self._symbol_table.find_or_create_tag("nlayers").name

        # Store a reference to the first field/operator object that
        # we can use to look-up nlayers in the PSy layer.
        if not self._invoke:
            # We're not generating a PSy layer so we're done here.
            return
        first_var = None
        for var in self._invoke.psy_unique_vars:
            if not var.is_scalar:
                first_var = var
                break
        if not first_var:
            raise GenerationError(
                "Cannot create an Invoke with no field/operator arguments.")
        self._first_var = first_var

    def _invoke_declarations(self, parent):
        '''
        Declare entities required for iterating over cells in the Invoke.

        :param parent: the f2pygen node representing the PSy-layer routine.
        :type parent: :py:class:`psyclone.f2pygen.SubroutineGen`

        '''
        api_config = Config.get().api_conf("dynamo0.3")

        # We only need the number of layers in the mesh if we are calling
        # one or more kernels that operate on cell-columns.
        if not self._dofs_only:
            parent.add(DeclGen(parent, datatype="integer",
                               kind=api_config.default_kind["integer"],
                               entity_decls=[self._nlayers_name]))

    def _stub_declarations(self, parent):
        '''
        Declare entities required for a kernel stub that operates on
        cell-columns.

        :param parent: the f2pygen node representing the Kernel stub.
        :type parent: :py:class:`psyclone.f2pygen.SubroutineGen`

        '''
        api_config = Config.get().api_conf("dynamo0.3")

        if self._kernel.cma_operation not in ["apply", "matrix-matrix"]:
            parent.add(DeclGen(parent, datatype="integer",
                               kind=api_config.default_kind["integer"],
                               intent="in", entity_decls=[self._nlayers_name]))

    def initialise(self, parent):
        '''
        Look-up the number of vertical layers in the mesh in the PSy layer.

        :param parent: the f2pygen node representing the PSy-layer routine.
        :type parent: :py:class:`psyclone.f2pygen.SubroutineGen`

        '''
        if not self._dofs_only:
            parent.add(CommentGen(parent, ""))
            parent.add(CommentGen(parent, " Initialise number of layers"))
            parent.add(CommentGen(parent, ""))
            parent.add(AssignGen(
                parent, lhs=self._nlayers_name,
                rhs=self._first_var.proxy_name_indexed + "%" +
                self._first_var.ref_name() + "%get_nlayers()"))


class LFRicLoopBounds(DynCollection):
    '''
    Handles all variables required for specifying loop limits within a
    PSy-layer routine.

    '''
    def _invoke_declarations(self, parent):
        '''
        Only needed because method is virtual in parent class.

        :param parent: the f2pygen node representing the PSy-layer routine.
        :type parent: :py:class:`psyclone.f2pygen.SubroutineGen`

        '''

    def initialise(self, parent):
        '''
        Updates the f2pygen AST so that all of the variables holding the lower
        and upper bounds of all loops in an Invoke are initialised.

        :param parent: the f2pygen node representing the PSy-layer routine.
        :type parent: :py:class:`psyclone.f2pygen.SubroutineGen`

        '''
        loops = self._invoke.schedule.loops()

        if not loops:
            return

        parent.add(CommentGen(parent, ""))
        parent.add(CommentGen(parent, " Set-up all of the loop bounds"))
        parent.add(CommentGen(parent, ""))

        sym_table = self._invoke.schedule.symbol_table
        config = Config.get()
        api_config = config.api_conf("dynamo0.3")

        for idx, loop in enumerate(loops):
            root_name = f"loop{idx}_start"
            try:
                lbound = sym_table.lookup_with_tag(root_name)
            except KeyError:
                # TODO #1258 the loop bound symbol should be of
                # precision 'i_def'.
                lbound = sym_table.new_symbol(root_name=root_name,
                                              tag=root_name,
                                              symbol_type=DataSymbol,
                                              datatype=INTEGER_TYPE)
            parent.add(AssignGen(parent, lhs=lbound.name,
                                 rhs=loop._lower_bound_fortran()))
            entities = [lbound.name]

            if loop.loop_type != "colour":
                root_name = f"loop{idx}_stop"
                try:
                    ubound = sym_table.lookup_with_tag(root_name)
                except KeyError:
                    # TODO #1258 the loop bound symbol should be of
                    # precision 'i_def'.
                    ubound = sym_table.new_symbol(root_name=root_name,
                                                  tag=root_name,
                                                  symbol_type=DataSymbol,
                                                  datatype=INTEGER_TYPE)
                entities.append(ubound.name)
                parent.add(AssignGen(parent, lhs=ubound.name,
                                     rhs=loop._upper_bound_fortran()))

            parent.add(DeclGen(parent, datatype="integer",
                               kind=api_config.default_kind["integer"],
                               entity_decls=entities))


class LFRicScalarArgs(DynCollection):
    '''
    Handles the declarations of scalar kernel arguments appearing in either
    an Invoke or a Kernel stub.

    :param node: the Invoke or Kernel stub for which to manage the scalar \
                 arguments.
    :type node: :py:class:`psyclone.dynamo0p3.DynKern` or \
                :py:class:`psyclone.dynamo0p3.DynInvoke`

    '''
    def __init__(self, node):
        super(LFRicScalarArgs, self).__init__(node)

        # Initialise dictionaries of 'real', 'integer' and 'logical'
        # scalar arguments by data type and intent
        self._scalar_args = {}
        self._real_scalars = {}
        self._integer_scalars = {}
        self._logical_scalars = {}
        for intent in FORTRAN_INTENT_NAMES:
            self._scalar_args[intent] = []
            self._real_scalars[intent] = []
            self._integer_scalars[intent] = []
            self._logical_scalars[intent] = []

    def _invoke_declarations(self, parent):
        '''
        Create argument lists and declarations for all scalar arguments
        in an Invoke.

        :param parent: the f2pygen node representing the PSy-layer routine \
                       to which to add declarations.
        :type parent: :py:class:`psyclone.f2pygen.SubroutineGen`

        :raises InternalError: for unsupported argument intrinsic types.
        :raises GenerationError: if the same scalar argument has different \
                                 data types in different Kernel calls \
                                 within the same Invoke.

        '''
        # Create dictionary of all scalar arguments for checks
        const = LFRicConstants()
        self._scalar_args = self._invoke.unique_declns_by_intent(
            const.VALID_SCALAR_NAMES)
        # Filter scalar arguments by intent and intrinsic type
        self._real_scalars = self._invoke.unique_declns_by_intent(
            const.VALID_SCALAR_NAMES,
            intrinsic_type=const.MAPPING_DATA_TYPES["gh_real"])
        self._integer_scalars = self._invoke.unique_declns_by_intent(
            const.VALID_SCALAR_NAMES,
            intrinsic_type=const.MAPPING_DATA_TYPES["gh_integer"])
        self._logical_scalars = self._invoke.unique_declns_by_intent(
            const.VALID_SCALAR_NAMES,
            intrinsic_type=const.MAPPING_DATA_TYPES["gh_logical"])

        for intent in FORTRAN_INTENT_NAMES:
            scal = [arg.declaration_name for arg in self._scalar_args[intent]]
            rscal = [arg.declaration_name for
                     arg in self._real_scalars[intent]]
            iscal = [arg.declaration_name for
                     arg in self._integer_scalars[intent]]
            lscal = [arg.declaration_name for
                     arg in self._logical_scalars[intent]]
            # Add "real", "integer" and "logical" scalar lists for checks
            decl_scal = rscal + iscal + lscal
            # Check for unsupported intrinsic types
            scal_inv = sorted(set(scal) - set(decl_scal))
            if scal_inv:
                raise InternalError(
                    "Found unsupported intrinsic types for the scalar "
                    "arguments {0} to Invoke '{1}'. Supported types are {2}.".
                    format(scal_inv, self._invoke.name,
                           const.VALID_INTRINSIC_TYPES))
            # Check that the same scalar name is not found in either of
            # 'real', 'integer' or 'logical' scalar lists (for instance if
            # passed to one kernel as a 'real' and to another kernel as an
            # 'integer' scalar)
            scal_multi_type = [item for item, count in
                               Counter(decl_scal).items() if count > 1]
            if scal_multi_type:
                raise GenerationError(
                    "Scalar argument(s) {0} in Invoke '{1}' have different "
                    "metadata for data type ({2}) in different kernels. "
                    "This is invalid.".
                    format(scal_multi_type, self._invoke.name,
                           list(const.MAPPING_DATA_TYPES.keys())))

        # Create declarations
        self._create_declarations(parent)

    def _stub_declarations(self, parent):
        '''
        Create and add declarations for all scalar arguments in
        a Kernel stub.

        :param parent: node in the f2pygen AST representing the Kernel stub \
                       to which to add declarations.
        :type parent: :py:class:`psyclone.f2pygen.SubroutineGen`

        :raises InternalError: for an unsupported argument data type.

        '''
        # Extract all scalar arguments
        for arg in self._calls[0].arguments.args:
            if arg.is_scalar:
                self._scalar_args[arg.intent].append(arg)

        const = LFRicConstants()
        # Filter scalar arguments by intent and data type
        for intent in FORTRAN_INTENT_NAMES:
            for arg in self._scalar_args[intent]:
                if arg.descriptor.data_type == "gh_real":
                    self._real_scalars[intent].append(arg)
                elif arg.descriptor.data_type == "gh_integer":
                    self._integer_scalars[intent].append(arg)
                elif arg.descriptor.data_type == "gh_logical":
                    self._logical_scalars[intent].append(arg)
                else:
                    raise InternalError(
                        "Found an unsupported data type '{0}' for the "
                        "scalar argument '{1}'. Supported types are {2}.".
                        format(arg.descriptor.data_type, arg.declaration_name,
                               const.VALID_SCALAR_DATA_TYPES))

        # Create declarations
        self._create_declarations(parent)

    def _create_declarations(self, parent):
        '''Add declarations for the scalar arguments.

        :param parent: the f2pygen node in which to insert declarations \
                       (Invoke or Kernel).
        :type parent: :py:class:`psyclone.f2pygen.SubroutineGen`

        :raises InternalError: if neither self._invoke nor \
            self._kernel are set.

        '''
        const = LFRicConstants()
        const_mod = const.UTILITIES_MOD_MAP["constants"]["module"]
        const_mod_list = None
        if self._invoke:
            const_mod_list = self._invoke.invokes.psy. \
                infrastructure_modules[const_mod]
        # Real scalar arguments
        for intent in FORTRAN_INTENT_NAMES:
            if self._real_scalars[intent]:
                # Filter scalars based on precision
                real_scalars_precision_map = OrderedDict()
                for real_scalar in self._real_scalars[intent]:
                    try:
                        real_scalars_precision_map[
                            real_scalar.precision].append(real_scalar)
                    except KeyError:
                        # This precision has not been seen before so
                        # create a new entry
                        real_scalars_precision_map[
                            real_scalar.precision] = [real_scalar]
                # Declare scalars
                for real_scalar_kind, real_scalars_list in \
                        real_scalars_precision_map.items():
                    real_scalar_type = real_scalars_list[0].intrinsic_type
                    real_scalar_names = [arg.declaration_name for arg
                                         in real_scalars_list]
                    parent.add(
                        DeclGen(parent, datatype=real_scalar_type,
                                kind=real_scalar_kind,
                                entity_decls=real_scalar_names,
                                intent=intent))
                    if self._invoke:
                        if real_scalar_kind not in const_mod_list:
                            const_mod_list.append(real_scalar_kind)
                    elif self._kernel:
                        self._kernel.argument_kinds.add(real_scalar_kind)
                    else:
                        raise InternalError(
                            "Expected the declaration of real scalar kernel "
                            "arguments to be for either an invoke or a "
                            "kernel stub, but it is neither.")

        # Integer scalar arguments
        for intent in FORTRAN_INTENT_NAMES:
            if self._integer_scalars[intent]:
                dtype = self._integer_scalars[intent][0].intrinsic_type
                dkind = self._integer_scalars[intent][0].precision
                integer_scalar_names = [arg.declaration_name for arg
                                        in self._integer_scalars[intent]]
                parent.add(
                    DeclGen(parent, datatype=dtype, kind=dkind,
                            entity_decls=integer_scalar_names,
                            intent=intent))
                if self._invoke:
                    if dkind not in const_mod_list:
                        const_mod_list.append(dkind)
                elif self._kernel:
                    self._kernel.argument_kinds.add(dkind)
                else:
                    raise InternalError(
                        "Expected the declaration of integer scalar kernel "
                        "arguments to be for either an invoke or a "
                        "kernel stub, but it is neither.")

        # Logical scalar arguments
        for intent in FORTRAN_INTENT_NAMES:
            if self._logical_scalars[intent]:
                dtype = self._logical_scalars[intent][0].intrinsic_type
                dkind = self._logical_scalars[intent][0].precision
                logical_scalar_names = [arg.declaration_name for arg
                                        in self._logical_scalars[intent]]
                parent.add(
                    DeclGen(parent, datatype=dtype, kind=dkind,
                            entity_decls=logical_scalar_names,
                            intent=intent))
                if self._invoke:
                    if dkind not in const_mod_list:
                        const_mod_list.append(dkind)
                elif self._kernel:
                    self._kernel.argument_kinds.add(dkind)
                else:
                    raise InternalError(
                        "Expected the declaration of logical scalar kernel "
                        "arguments to be for either an invoke or a "
                        "kernel stub, but it is neither.")


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
        api_config = Config.get().api_conf("dynamo0.3")

        lma_args = psyGen.args_filter(
            self._kernel.arguments.args, arg_types=["gh_operator"])
        if lma_args:
            parent.add(DeclGen(parent, datatype="integer",
                               kind=api_config.default_kind["integer"],
                               intent="in", entity_decls=["cell"]))
        for arg in lma_args:
            size = arg.name+"_ncell_3d"
            op_dtype = arg.intrinsic_type
            op_kind = arg.precision
            parent.add(DeclGen(parent, datatype="integer",
                               kind=api_config.default_kind["integer"],
                               intent="in", entity_decls=[size]))
            ndf_name_to = arg.function_space_to.ndf_name
            ndf_name_from = arg.function_space_from.ndf_name
            parent.add(DeclGen(parent, datatype=op_dtype, kind=op_kind,
                               dimension=",".join([ndf_name_to,
                                                   ndf_name_from, size]),
                               intent=arg.intent,
                               entity_decls=[arg.name]))

    def _invoke_declarations(self, parent):
        '''
        Declare all LMA-related quantities in a PSy-layer routine.
        Note: PSy layer in LFRic does not modify the LMA operator objects.
        Hence, their Fortran intents are always "in" (the data updated in the
        kernels is only pointed to from the LMA operator object and is thus
        not a part of the object).

        :param parent: the f2pygen node representing the PSy-layer routine.
        :type parent: :py:class:`psyclone.f2pygen.SubroutineGen`

        '''
        # Add the Invoke subroutine argument declarations for operators
        op_args = self._invoke.unique_declarations(
            argument_types=["gh_operator"])
        # Filter operators by their datatype
        operators_datatype_map = OrderedDict()
        for op_arg in op_args:
            try:
                operators_datatype_map[op_arg.data_type].append(op_arg)
            except KeyError:
                # This datatype has not been seen before so create new entry
                operators_datatype_map[op_arg.data_type] = [op_arg]
        # Declare the operators
        for op_datatype, op_list in operators_datatype_map.items():
            operators_names = [arg.declaration_name for arg in op_list]
            parent.add(TypeDeclGen(
                parent, datatype=op_datatype,
                entity_decls=operators_names, intent="in"))
            op_mod = op_list[0].module_name
            # Record that we will need to import this operator
            # datatype from the appropriate infrastructure module
            (self._invoke.invokes.psy.infrastructure_modules[op_mod].
             add(op_datatype))


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
                        self._cma_ops[arg.name]["datatype"] = \
                            arg.intrinsic_type
                        self._cma_ops[arg.name]["kind"] = arg.precision
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
            cma_name = self._symbol_table.find_or_create_tag(
                op_name+"_matrix").name
            parent.add(AssignGen(parent, lhs=cma_name, pointer=True,
                                 rhs=self._cma_ops[op_name]["arg"].
                                 proxy_name_indexed+"%columnwise_matrix"))
            # Then make copies of the related integer parameters
            for param in self._cma_ops[op_name]["params"]:
                param_name = self._symbol_table.find_or_create_tag(
                    op_name+"_"+param).name
                parent.add(AssignGen(parent, lhs=param_name,
                                     rhs=self._cma_ops[op_name]["arg"].
                                     proxy_name_indexed+"%"+param))

    def _invoke_declarations(self, parent):
        '''
        Generate the necessary PSy-layer declarations for all column-wise
        operators and their associated parameters.
        Note: PSy layer in LFRic does not modify the CMA operator objects.
        Hence, their Fortran intents are always "in" (the data updated in the
        kernels is only pointed to from the column-wise operator object and is
        thus not a part of the object).

        :param parent: the f2pygen node representing the PSy-layer routine.
        :type parent: :py:class:`psyclone.f2pygen.SubroutineGen`

        '''
        api_config = Config.get().api_conf("dynamo0.3")

        # If we have no CMA operators then we do nothing
        if not self._cma_ops:
            return

        # Add the Invoke subroutine argument declarations for column-wise
        # operators
        cma_op_args = self._invoke.unique_declarations(
            argument_types=["gh_columnwise_operator"])
        # Create a list of column-wise operator names
        cma_op_arg_list = [arg.declaration_name for arg in cma_op_args]
        if cma_op_arg_list:
            op_type = cma_op_args[0].data_type
            op_mod = cma_op_args[0].module_name
            parent.add(TypeDeclGen(parent,
                                   datatype=op_type,
                                   entity_decls=cma_op_arg_list,
                                   intent="in"))
            (self._invoke.invokes.psy.infrastructure_modules[op_mod].
             add(op_type))

        for op_name in self._cma_ops:
            # Declare the operator matrix itself
            cma_name = self._symbol_table.find_or_create_tag(
                op_name+"_matrix").name
            cma_dtype = self._cma_ops[op_name]["datatype"]
            cma_kind = self._cma_ops[op_name]["kind"]
            parent.add(DeclGen(parent, datatype=cma_dtype,
                               kind=cma_kind, pointer=True,
                               entity_decls=[cma_name+"(:,:,:) => null()"]))
            const = LFRicConstants()
            const_mod = const.UTILITIES_MOD_MAP["constants"]["module"]
            const_mod_list = self._invoke.invokes.psy. \
                infrastructure_modules[const_mod]
            if cma_kind not in const_mod_list:
                # Record that we will need to import the kind of this
                # cma operator from the appropriate infrastructure
                # module
                const_mod_list.append(cma_kind)

            # Declare the associated integer parameters
            param_names = []
            for param in self._cma_ops[op_name]["params"]:
                param_names.append(self._symbol_table.find_or_create_tag(
                    op_name+"_"+param).name)
            parent.add(DeclGen(parent, datatype="integer",
                               kind=api_config.default_kind["integer"],
                               entity_decls=param_names))

    def _stub_declarations(self, parent):
        '''
        Generate all necessary declarations for CMA operators being passed to
        a Kernel stub.

        :param parent: f2pygen node representing the Kernel stub.
        :type parent: :py:class:`psyclone.f2pygen.SubroutineGen`

        '''
        api_config = Config.get().api_conf("dynamo0.3")

        # If we have no CMA operators then we do nothing
        if not self._cma_ops:
            return

        symtab = self._symbol_table

        # CMA operators always need the current cell index and the number
        # of columns in the mesh
        parent.add(DeclGen(parent, datatype="integer",
                           kind=api_config.default_kind["integer"],
                           intent="in", entity_decls=["cell", "ncell_2d"]))

        for op_name in self._cma_ops:
            # Declare the associated scalar arguments before the array because
            # some of them are used to dimension the latter (and some compilers
            # get upset if this ordering is not followed)
            _local_args = []
            for param in self._cma_ops[op_name]["params"]:
                param_name = symtab.find_or_create_tag(op_name+"_"+param).name
                _local_args.append(param_name)
            parent.add(DeclGen(parent, datatype="integer",
                               kind=api_config.default_kind["integer"],
                               intent="in", entity_decls=_local_args))
            # Declare the array that holds the CMA operator
            bandwidth = op_name + "_bandwidth"
            nrow = op_name + "_nrow"
            intent = self._cma_ops[op_name]["intent"]
            op_dtype = self._cma_ops[op_name]["datatype"]
            op_kind = self._cma_ops[op_name]["kind"]
            parent.add(DeclGen(parent, datatype=op_dtype, kind=op_kind,
                               dimension=",".join([bandwidth,
                                                   nrow, "ncell_2d"]),
                               intent=intent, entity_decls=[op_name]))


class DynMeshes(object):
    '''
    Holds all mesh-related information (including colour maps if
    required).  If there are no inter-grid kernels then there is only
    one mesh object required (when calling kernels with operates_on==domain,
    colouring, doing distributed memory or querying the reference element).
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
        # Dict of DynInterGrid objects holding information on the mesh-related
        # variables required by each inter-grid kernel. Keys are the kernel
        # names.
        self._ig_kernels = OrderedDict()
        # List of names of unique mesh variables referenced in the Invoke
        self._mesh_tag_names = []
        # Whether or not the associated Invoke requires colourmap information
        self._needs_colourmap = False
        self._needs_colourmap_halo = False
        # Keep a reference to the InvokeSchedule so we can check for colouring
        # later
        self._schedule = invoke.schedule
        self._symbol_table = self._schedule.symbol_table
        # Set used to generate a list of the unique mesh objects
        _name_set = set()

        # Find the first non-scalar argument to this PSy layer routine. We
        # will use this to look-up the mesh if there are no inter-grid
        # kernels in this invoke.
        self._first_var = None
        for var in unique_psy_vars:
            if not var.is_scalar:
                self._first_var = var
                break

        # Loop over all kernel calls in the schedule. Keep a list of
        # any non-intergrid kernels so that we can generate a verbose error
        # message if necessary.
        non_intergrid_kernels = []
        for call in self._schedule.coded_kernels():

            if (call.reference_element.properties or call.mesh.properties or
                    call.iterates_over == "domain" or call.cma_operation):
                _name_set.add("mesh")

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
            self._ig_kernels[id(call)] = DynInterGrid(fine_arg, coarse_arg)

            # Create and store the names of the associated mesh objects
            _name_set.add("mesh_{0}".format(fine_arg.name))
            _name_set.add("mesh_{0}".format(coarse_arg.name))

        # If we found a mixture of both inter-grid and non-inter-grid kernels
        # then we reject the invoke()
        if non_intergrid_kernels and self._ig_kernels:
            raise GenerationError(
                "An invoke containing inter-grid kernels must contain no "
                "other kernel types but kernels '{0}' in invoke '{1}' are "
                "not inter-grid kernels.".format(
                    ", ".join([call.name for call in non_intergrid_kernels]),
                    invoke.name))

        # If distributed memory is enabled then we will need at least
        # one mesh object if we have one or more kernels that operate
        # on cell-columns or are doing redundant computation for a
        # kernel that operates on dofs. Since the latter condition
        # comes about through the application of a transformation, we
        # don't yet know whether or not a mesh is required. Therefore,
        # the only solution is to assume that a mesh object is
        # required if distributed memory is enabled. We also require a
        # mesh object if any of the kernels require properties of
        # either the reference element or the mesh. (Colourmaps also
        # require a mesh object but that is handled in _colourmap_init().)
        if not _name_set and Config.get().distributed_memory:
            # We didn't already have a requirement for a mesh so add one now.
            _name_set.add("mesh")

        self._add_mesh_symbols(list(_name_set))

    def _add_mesh_symbols(self, mesh_tags):
        '''
        Add DataSymbols for the supplied list of mesh names and store the
        corresponding list of tags.

        A ContainerSymbol is created for the LFRic mesh module and a TypeSymbol
        for the mesh type. If distributed memory is enabled then a DataSymbol
        to hold the maximum halo depth is created for each mesh.

        :param mesh_tags: tag names for every mesh object required.
        :type mesh_tags: list of str

        '''
        if not mesh_tags:
            return

        self._mesh_tag_names = sorted(mesh_tags)

        # Look up the names of the module and type for the mesh object
        # from the LFRic constants class.
        const = LFRicConstants()
        mmod = const.MESH_TYPE_MAP["mesh"]["module"]
        mtype = const.MESH_TYPE_MAP["mesh"]["type"]
        # Create a Container symbol for the module
        csym = self._symbol_table.find_or_create_tag(
            mmod, symbol_type=ContainerSymbol)
        # Create a TypeSymbol for the mesh type
        mtype_sym = self._symbol_table.find_or_create_tag(
            mtype, symbol_type=DataTypeSymbol,
            datatype=DeferredType(),
            interface=ImportInterface(csym))

        name_list = []
        for name in mesh_tags:
            name_list.append(self._symbol_table.find_or_create_tag(
                name, symbol_type=DataSymbol, datatype=mtype_sym).name)

        if Config.get().distributed_memory:
            # If distributed memory is enabled then we require a variable
            # holding the maximum halo depth for each mesh.
            for name in mesh_tags:
                # TODO #1258 this variable should have precision 'i_def'.
                self._symbol_table.find_or_create_tag(f"max_halo_depth_{name}",
                                                      symbol_type=DataSymbol,
                                                      datatype=INTEGER_TYPE)

    def _colourmap_init(self):
        '''
        Sets-up information on any required colourmaps. This cannot be done
        in the constructor since colouring is applied by Transformations
        and happens after the Schedule has already been constructed.

        '''
        const = LFRicConstants()
        have_non_intergrid = False

        array_type_1d = ArrayType(INTEGER_TYPE, [ArrayType.Extent.DEFERRED])

        array_type_2d = ArrayType(INTEGER_TYPE, [ArrayType.Extent.DEFERRED,
                                                 ArrayType.Extent.DEFERRED])
        for call in [call for call in self._schedule.coded_kernels() if
                     call.is_coloured()]:
            # Keep a record of whether or not any kernels (loops) in this
            # invoke have been coloured and, if so, whether the associated loop
            # goes into the halo.
            if (call.parent.parent.upper_bound_name in
                    const.HALO_ACCESS_LOOP_BOUNDS):
                self._needs_colourmap_halo = True
            else:
                self._needs_colourmap = True

            if not call.is_intergrid:
                have_non_intergrid = True
                continue

            # This is an inter-grid kernel so look-up the names of
            # the colourmap variables associated with the coarse
            # mesh (since that determines the iteration space).
            carg_name = self._ig_kernels[id(call)].coarse.name
            # Colour map
            base_name = "cmap_" + carg_name
            colour_map = self._schedule.symbol_table.find_or_create_tag(
                base_name, symbol_type=DataSymbol,
                datatype=array_type_2d).name
            # No. of colours
            # TODO #1258 this variable should have precision 'i_def'.
            base_name = "ncolour_" + carg_name
            ncolours = self._schedule.symbol_table.find_or_create_tag(
                base_name, symbol_type=DataSymbol,
                datatype=INTEGER_TYPE).name
            # Array holding the last halo or edge cell of a given colour
            # and halo depth.
            if Config.get().distributed_memory:
                base_name = "last_halo_cell_all_colours_" + carg_name
                last_cell = self._schedule.symbol_table.find_or_create_tag(
                    base_name, symbol_type=DataSymbol,
                    datatype=array_type_2d).name
            else:
                base_name = "last_edge_cell_all_colours_" + carg_name
                last_cell = self._schedule.symbol_table.find_or_create_tag(
                    base_name, symbol_type=DataSymbol,
                    datatype=array_type_1d).name
            # Add these names into the dictionary entry for this
            # inter-grid kernel
            self._ig_kernels[id(call)].colourmap = colour_map
            self._ig_kernels[id(call)].ncolours_var = ncolours
            self._ig_kernels[id(call)].last_cell_var = last_cell

        if have_non_intergrid and (self._needs_colourmap or
                                   self._needs_colourmap_halo):
            # There aren't any inter-grid kernels but we do need colourmap
            # information and that means we'll need a mesh object
            self._add_mesh_symbols(["mesh"])
            colour_map = self._schedule.symbol_table.find_or_create_tag(
                "cmap", symbol_type=DataSymbol, datatype=array_type_2d).name
            # No. of colours
            # TODO #1258 this variable should have precision 'i_def'.
            ncolours = self._schedule.symbol_table.find_or_create_tag(
                "ncolour", symbol_type=DataSymbol, datatype=INTEGER_TYPE).name
            if self._needs_colourmap_halo:
                dtype = array_type_2d
                self._symbol_table.find_or_create_tag(
                    "last_halo_cell_all_colours", symbol_type=DataSymbol,
                    datatype=dtype)
            if self._needs_colourmap:
                dtype = array_type_1d
                self._symbol_table.find_or_create_tag(
                    "last_edge_cell_all_colours", symbol_type=DataSymbol,
                    datatype=dtype)

    def declarations(self, parent):
        '''
        Declare variables specific to mesh objects.

        :param parent: the parent node to which to add the declarations
        :type parent: an instance of :py:class:`psyclone.f2pygen.BaseGen`

        '''
        api_config = Config.get().api_conf("dynamo0.3")
        const = LFRicConstants()

        # Since we're now generating code, any transformations must
        # have been applied so we can set-up colourmap information
        self._colourmap_init()

        # We'll need various typedefs from the mesh module
        mtype = const.MESH_TYPE_MAP["mesh"]["type"]
        mmod = const.MESH_TYPE_MAP["mesh"]["module"]
        mmap_type = const.MESH_TYPE_MAP["mesh_map"]["type"]
        mmap_mod = const.MESH_TYPE_MAP["mesh_map"]["module"]
        if self._mesh_tag_names:
            name = self._symbol_table.lookup_with_tag(mtype).name
            parent.add(UseGen(parent, name=mmod, only=True,
                              funcnames=[name]))
        if self._ig_kernels:
            parent.add(UseGen(parent, name=mmap_mod, only=True,
                              funcnames=[mmap_type]))
        # Declare the mesh object(s) and associated halo depths
        for tag_name in self._mesh_tag_names:
            name = self._symbol_table.lookup_with_tag(tag_name).name
            parent.add(TypeDeclGen(parent, pointer=True, datatype=mtype,
                                   entity_decls=[name + " => null()"]))
            # For each mesh we also need the maximum halo depth.
            if Config.get().distributed_memory:
                name = self._symbol_table.lookup_with_tag(
                    f"max_halo_depth_{tag_name}").name
                parent.add(DeclGen(parent, datatype="integer",
                                   kind=api_config.default_kind["integer"],
                                   entity_decls=[name]))

        # Declare the inter-mesh map(s) and cell map(s)
        for kern in self._ig_kernels.values():
            parent.add(TypeDeclGen(parent, pointer=True,
                                   datatype=mmap_type,
                                   entity_decls=[kern.mmap + " => null()"]))
            parent.add(
                DeclGen(parent, pointer=True, datatype="integer",
                        kind=api_config.default_kind["integer"],
                        entity_decls=[kern.cell_map + "(:,:,:) => null()"]))

            # Declare the number of cells in the fine mesh and how many fine
            # cells there are per coarse cell
            parent.add(DeclGen(parent, datatype="integer",
                               kind=api_config.default_kind["integer"],
                               entity_decls=[kern.ncell_fine,
                                             kern.ncellpercellx,
                                             kern.ncellpercelly]))
            # Declare variables to hold the colourmap information if required
            if kern.colourmap:
                parent.add(
                    DeclGen(parent, datatype="integer",
                            kind=api_config.default_kind["integer"],
                            pointer=True,
                            entity_decls=[kern.colourmap+"(:,:)"]))
                parent.add(
                    DeclGen(parent, datatype="integer",
                            kind=api_config.default_kind["integer"],
                            entity_decls=[kern.ncolours_var]))
                decln = kern.last_cell_var
                if Config.get().distributed_memory:
                    # If DM is enabled then the cell-count array is 2D because
                    # it has a halo-depth dimension.
                    decln += "(:,:)"
                else:
                    decln += "(:)"
                parent.add(
                    DeclGen(parent, datatype="integer", allocatable=True,
                            kind=api_config.default_kind["integer"],
                            entity_decls=[decln]))

        if not self._ig_kernels and (self._needs_colourmap or
                                     self._needs_colourmap_halo):
            # There aren't any inter-grid kernels but we do need
            # colourmap information
            base_name = "cmap"
            colour_map = \
                self._schedule.symbol_table.find_or_create_tag(base_name).name
            # No. of colours
            base_name = "ncolour"
            ncolours = \
                self._schedule.symbol_table.find_or_create_tag(base_name).name
            # Add declarations for these variables
            parent.add(DeclGen(parent, datatype="integer",
                               kind=api_config.default_kind["integer"],
                               pointer=True,
                               entity_decls=[colour_map+"(:,:)"]))
            parent.add(DeclGen(parent, datatype="integer",
                               kind=api_config.default_kind["integer"],
                               entity_decls=[ncolours]))
            if self._needs_colourmap_halo:
                last_cell = self._symbol_table.find_or_create_tag(
                    "last_halo_cell_all_colours")
                parent.add(DeclGen(parent, datatype="integer",
                                   kind=api_config.default_kind["integer"],
                                   allocatable=True,
                                   entity_decls=[last_cell.name+"(:,:)"]))
            if self._needs_colourmap:
                last_cell = self._symbol_table.find_or_create_tag(
                    "last_edge_cell_all_colours")
                parent.add(DeclGen(parent, datatype="integer",
                                   kind=api_config.default_kind["integer"],
                                   allocatable=True,
                                   entity_decls=[last_cell.name+"(:)"]))

    def initialise(self, parent):
        '''
        Initialise parameters specific to inter-grid kernels.

        :param parent: the parent node to which to add the initialisations.
        :type parent: an instance of :py:class:`psyclone.f2pygen.BaseGen`

        '''
        # pylint: disable=too-many-branches
        # If we haven't got any need for a mesh in this invoke then we
        # don't do anything
        if not self._mesh_tag_names:
            return

        parent.add(CommentGen(parent, ""))

        if len(self._mesh_tag_names) == 1:
            # We only require one mesh object which means that this invoke
            # contains no inter-grid kernels (which would require at least 2)
            parent.add(CommentGen(parent, " Create a mesh object"))
            parent.add(CommentGen(parent, ""))
            rhs = "%".join([self._first_var.proxy_name_indexed,
                            self._first_var.ref_name(), "get_mesh()"])
            mesh_name = self._symbol_table.lookup_with_tag(
                self._mesh_tag_names[0]).name
            parent.add(AssignGen(parent, pointer=True, lhs=mesh_name, rhs=rhs))
            if Config.get().distributed_memory:
                # If distributed memory is enabled then we need the maximum
                # halo depth.
                depth_name = self._symbol_table.lookup_with_tag(
                    f"max_halo_depth_{self._mesh_tag_names[0]}").name
                parent.add(AssignGen(parent, lhs=depth_name,
                                     rhs=f"{mesh_name}%get_halo_depth()"))
            if self._needs_colourmap or self._needs_colourmap_halo:
                parent.add(CommentGen(parent, ""))
                parent.add(CommentGen(parent, " Get the colourmap"))
                parent.add(CommentGen(parent, ""))
                # Look-up variable names for colourmap and number of colours
                colour_map = self._schedule.symbol_table.find_or_create_tag(
                    "cmap").name
                ncolour = \
                    self._schedule.symbol_table.find_or_create_tag("ncolour")\
                                               .name
                # Get the number of colours
                parent.add(AssignGen(
                    parent, lhs=ncolour, rhs=f"{mesh_name}%get_ncolours()"))
                # Get the colour map
                parent.add(AssignGen(parent, pointer=True, lhs=colour_map,
                                     rhs=f"{mesh_name}%get_colour_map()"))
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
            # We need pointers to both the coarse and the fine mesh as well
            # as the maximum halo depth for each.
            fine_mesh = self._schedule.symbol_table.find_or_create_tag(
                f"mesh_{dig.fine.name}").name
            coarse_mesh = self._schedule.symbol_table.find_or_create_tag(
                f"mesh_{dig.coarse.name}").name
            if fine_mesh not in initialised:
                initialised.append(fine_mesh)
                parent.add(
                    AssignGen(parent, pointer=True,
                              lhs=fine_mesh,
                              rhs="%".join([dig.fine.proxy_name_indexed,
                                            dig.fine.ref_name(),
                                            "get_mesh()"])))
                if Config.get().distributed_memory:
                    max_halo_f_mesh = (
                        self._schedule.symbol_table.find_or_create_tag(
                            f"max_halo_depth_mesh_{dig.fine.name}").name)

                    parent.add(AssignGen(parent, lhs=max_halo_f_mesh,
                                         rhs=f"{fine_mesh}%get_halo_depth()"))
            if coarse_mesh not in initialised:
                initialised.append(coarse_mesh)
                parent.add(
                    AssignGen(parent, pointer=True,
                              lhs=coarse_mesh,
                              rhs="%".join([dig.coarse.proxy_name_indexed,
                                            dig.coarse.ref_name(),
                                            "get_mesh()"])))
                if Config.get().distributed_memory:
                    max_halo_c_mesh = (
                        self._schedule.symbol_table.find_or_create_tag(
                            f"max_halo_depth_mesh_{dig.coarse.name}").name)
                    parent.add(AssignGen(
                        parent, lhs=max_halo_c_mesh,
                        rhs=f"{coarse_mesh}%get_halo_depth()"))
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

            # Number of fine cells per coarse cell in x.
            if dig.ncellpercellx not in initialised:
                initialised.append(dig.ncellpercellx)
                parent.add(
                    AssignGen(parent, lhs=dig.ncellpercellx,
                              rhs=dig.mmap +
                              "%get_ntarget_cells_per_source_x()"))

            # Number of fine cells per coarse cell in y.
            if dig.ncellpercelly not in initialised:
                initialised.append(dig.ncellpercelly)
                parent.add(
                    AssignGen(parent, lhs=dig.ncellpercelly,
                              rhs=dig.mmap +
                              "%get_ntarget_cells_per_source_y()"))

            # Colour map for the coarse mesh (if required)
            if dig.colourmap:
                # Number of colours
                parent.add(AssignGen(parent, lhs=dig.ncolours_var,
                                     rhs=coarse_mesh + "%get_ncolours()"))
                # Colour map itself
                parent.add(AssignGen(parent, lhs=dig.colourmap,
                                     pointer=True,
                                     rhs=coarse_mesh + "%get_colour_map()"))
                if Config.get().distributed_memory:
                    name = "%get_last_halo_cell_all_colours()"
                else:
                    name = "%get_last_edge_cell_all_colours()"
                parent.add(AssignGen(parent, lhs=dig.last_cell_var,
                                     rhs=coarse_mesh + name))

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
    # pylint: disable=too-few-public-methods, too-many-instance-attributes
    def __init__(self, fine_arg, coarse_arg):

        # Arguments on the coarse and fine grids
        self.coarse = coarse_arg
        self.fine = fine_arg

        # Get a reference to the InvokeSchedule SymbolTable
        symtab = self.coarse.call.root.symbol_table

        # Generate name for inter-mesh map
        base_mmap_name = "mmap_{0}_{1}".format(fine_arg.name,
                                               coarse_arg.name)
        self.mmap = symtab.find_or_create_tag(base_mmap_name).name

        # Generate name for ncell variables
        self.ncell_fine = symtab.find_or_create_tag(
            "ncell_{0}".format(fine_arg.name)).name
        # No. of fine cells per coarse cell in x
        self.ncellpercellx = symtab.find_or_create_tag(
            "ncpc_{0}_{1}_x".format(fine_arg.name, coarse_arg.name)).name
        # No. of fine cells per coarse cell in y
        self.ncellpercelly = symtab.find_or_create_tag(
            "ncpc_{0}_{1}_y".format(fine_arg.name, coarse_arg.name)).name
        # Name for cell map
        base_name = "cell_map_" + coarse_arg.name
        self.cell_map = symtab.find_or_create_tag(base_name).name

        # We have no colourmap information when first created
        self.colourmap = ""
        # Name of the variable holding the number of colours
        self.ncolours_var = ""
        # Name of the variable holding the last cell of a particular colour
        self.last_cell_var = ""


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
    # Dimensioning vars for the basis function arrays required by each
    # type of quadrature
    qr_dim_vars = {"xyoz": ["np_xy", "np_z"],
                   "edge": ["np_xyz", "nedges"],
                   "face": ["np_xyz", "nfaces"]}
    # The different weights arrays required by each type of quadrature
    qr_weight_vars = {"xyoz": ["weights_xy", "weights_z"],
                      "edge": ["weights_xyz"],
                      "face": ["weights_xyz"]}

    def __init__(self, node):

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
        self._qr_vars = OrderedDict()
        # The dict of target function spaces upon which we must provide
        # evaluators. Keys are the FS names, values are (FunctionSpace,
        # DynKernelArgument) tuples.
        self._eval_targets = OrderedDict()

        for call in self._calls:

            if isinstance(call, LFRicBuiltIn) or not call.eval_shapes:
                # Skip this kernel if it doesn't require basis/diff basis fns
                continue

            for shape, rule in call.qr_rules.items():

                # This kernel requires quadrature
                if shape not in self._qr_vars:
                    # We haven't seen a quadrature arg with this shape
                    # before so create a dictionary entry with an
                    # empty list
                    self._qr_vars[shape] = []
                if rule.psy_name not in self._qr_vars[shape]:
                    # Add this qr argument to the list of those that
                    # have this shape
                    self._qr_vars[shape].append(rule.psy_name)

            if "gh_evaluator" in call.eval_shapes:
                # An evaluator consists of basis or diff basis functions
                # for one FS evaluated on the nodes of another 'target' FS.
                # Make a dict of 2-tuples, each containing the
                # FunctionSpace and associated kernel argument for the
                # target FSs.

                # Loop over the target FS for evaluators required by this
                # kernel
                for fs_name in call.eval_targets:
                    if fs_name not in self._eval_targets:
                        # We don't already have this space in our list so
                        # add it to the list of target spaces
                        self._eval_targets[fs_name] = \
                            call.eval_targets[fs_name]

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
        :type function_space: :py:class:`psyclone.domain.lfric.FunctionSpace`
        :return: a Fortran variable name
        :rtype: str

        '''
        return "dim_" + function_space.mangled_name

    @staticmethod
    def basis_first_dim_value(function_space):
        '''
        Get the size of the first dimension of a basis function.

        :param function_space: the function space the basis function is for
        :type function_space: :py:class:`psyclone.domain.lfric.FunctionSpace`
        :return: an integer length.
        :rtype: string

        :raises GenerationError: if an unsupported function space is supplied \
                                 (e.g. ANY_SPACE_*, ANY_DISCONTINUOUS_SPACE_*)
        '''
        if function_space.has_scalar_basis:
            first_dim = "1"
        elif function_space.has_vector_basis:
            first_dim = "3"
        else:
            # It is not possible to determine explicitly the first basis
            # function array dimension from the metadata for any_space or
            # any_discontinuous_space. This information needs to be passed
            # from the PSy layer to the kernels (see issue #461).
            const = LFRicConstants()
            raise GenerationError(
                "Unsupported space for basis function, "
                "expecting one of {0} but found "
                "'{1}'".format(const.VALID_FUNCTION_SPACES,
                               function_space.orig_name))
        return first_dim

    @staticmethod
    def diff_basis_first_dim_name(function_space):
        '''
        Get the name of the variable holding the first dimension of a
        differential basis function.

        :param function_space: the function space the diff-basis function \
                               is for.
        :type function_space: :py:class:`psyclone.domain.lfric.FunctionSpace`
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
        :type function_space: :py:class:`psyclone.domain.lfric.FunctionSpace`
        :return: an integer length.
        :rtype: str

        :raises GenerationError: if an unsupported function space is \
                                 supplied (e.g. ANY_SPACE_*, \
                                 ANY_DISCONTINUOUS_SPACE_*)

        '''
        if function_space.has_scalar_diff_basis:
            first_dim = "1"
        elif function_space.has_vector_diff_basis:
            first_dim = "3"
        else:
            # It is not possible to determine explicitly the first
            # differential basis function array dimension from the metadata
            # for any_space or any_discontinuous_space. This information
            # needs to be passed from the PSy layer to the kernels
            # (see issue #461).
            const = LFRicConstants()
            raise GenerationError(
                "Unsupported space for differential basis function, expecting "
                "one of {0} but found '{1}'"
                .format(const.VALID_FUNCTION_SPACES,
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
        const = LFRicConstants()
        # We need a full FunctionSpace object for each function space
        # that has basis functions associated with it.
        for fsd in call.fs_descriptors.descriptors:

            # We need the full FS object, not just the name. Therefore
            # we first have to get a kernel argument that is on this
            # space...
            arg, fspace = call.arguments.get_arg_on_space_name(fsd.fs_name)

            for shape in call.eval_shapes:

                # Populate a dict with the shape, function space and
                # associated kernel argument for this basis/diff-basis f'n.
                entry = {"shape": shape,
                         "fspace": fspace,
                         "arg": arg}
                if shape in const.VALID_QUADRATURE_SHAPES:
                    # This is for quadrature - store the name of the
                    # qr variable
                    entry["qr_var"] = call.qr_rules[shape].psy_name
                    # Quadrature weights are evaluated at pre-determined
                    # points rather than at the nodes of another FS.
                    # We put one entry of None in the list of target
                    # spaces to facilitate cases where we loop over
                    # this list.
                    entry["nodal_fspaces"] = [None]
                elif shape == "gh_evaluator":
                    # This is an evaluator
                    entry["qr_var"] = None
                    # Store a list of the FunctionSpace objects for which
                    # these basis functions are to be evaluated
                    entry["nodal_fspaces"] = [items[0] for items in
                                              call.eval_targets.values()]
                else:
                    raise InternalError("Unrecognised evaluator shape: '{0}'. "
                                        "Should be one of {1}".format(
                                            shape,
                                            const.VALID_EVALUATOR_SHAPES))

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

        :raises InternalError: if an unsupported quadrature shape is found.

        '''
        api_config = Config.get().api_conf("dynamo0.3")

        if not self._qr_vars and not self._eval_targets:
            return

        # The quadrature shapes that this method supports
        supported_shapes = ["gh_quadrature_xyoz", "gh_quadrature_face",
                            "gh_quadrature_edge"]

        # Get the lists of dimensioning variables and basis arrays
        var_dims, basis_arrays = self._basis_fn_declns()

        if var_dims:
            parent.add(DeclGen(parent, datatype="integer",
                               kind=api_config.default_kind["integer"],
                               intent="in", entity_decls=var_dims))
        for basis in basis_arrays:
            parent.add(DeclGen(parent, datatype="real",
                               kind=api_config.default_kind["real"],
                               intent="in",
                               dimension=",".join(basis_arrays[basis]),
                               entity_decls=[basis]))

        const = LFRicConstants()

        for shape in self._qr_vars:
            qr_name = "_qr_" + shape.split("_")[-1]
            if shape == "gh_quadrature_xyoz":
                datatype = const.QUADRATURE_TYPE_MAP[shape]["intrinsic"]
                kind = const.QUADRATURE_TYPE_MAP[shape]["kind"]
                parent.add(DeclGen(
                    parent, datatype=datatype, kind=kind,
                    intent="in", dimension="np_xy"+qr_name,
                    entity_decls=["weights_xy"+qr_name]))
                parent.add(DeclGen(
                    parent, datatype=datatype, kind=kind,
                    intent="in", dimension="np_z"+qr_name,
                    entity_decls=["weights_z"+qr_name]))
            elif shape == "gh_quadrature_face":
                parent.add(DeclGen(
                    parent,
                    datatype=const.QUADRATURE_TYPE_MAP[shape]["intrinsic"],
                    kind=const.QUADRATURE_TYPE_MAP[shape]["kind"], intent="in",
                    dimension=",".join(["np_xyz"+qr_name, "nfaces"+qr_name]),
                    entity_decls=["weights_xyz"+qr_name]))
            elif shape == "gh_quadrature_edge":
                parent.add(DeclGen(
                    parent,
                    datatype=const.QUADRATURE_TYPE_MAP[shape]["intrinsic"],
                    kind=const.QUADRATURE_TYPE_MAP[shape]["kind"], intent="in",
                    dimension=",".join(["np_xyz"+qr_name, "nedges"+qr_name]),
                    entity_decls=["weights_xyz"+qr_name]))
            else:
                raise InternalError(
                    "Quadrature shapes other than {0} are not yet "
                    "supported - got: '{1}'".format(supported_shapes, shape))

    def _invoke_declarations(self, parent):
        '''
        Add basis-function declarations to the PSy layer.

        :param parent: f2pygen node represening the PSy-layer routine.
        :type parent: :py:class:`psyclone.f2pygen.SubroutineGen`

        '''
        # Create a single declaration for each quadrature type
        const = LFRicConstants()
        for shape in const.VALID_QUADRATURE_SHAPES:
            if shape in self._qr_vars and self._qr_vars[shape]:
                # The PSy-layer routine is passed objects of
                # quadrature_* type
                parent.add(
                    TypeDeclGen(parent,
                                datatype=const.
                                QUADRATURE_TYPE_MAP[shape]["type"],
                                entity_decls=self._qr_vars[shape],
                                intent="in"))
                # For each of these we'll need a corresponding proxy, use
                # the symbol_table to avoid clashes...
                var_names = []
                for var in self._qr_vars[shape]:
                    var_names.append(
                        self._symbol_table.find_or_create_tag(var+"_proxy")
                                          .name)
                parent.add(
                    TypeDeclGen(
                        parent,
                        datatype=const.
                        QUADRATURE_TYPE_MAP[shape]["proxy_type"],
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
        # pylint: disable=too-many-branches, too-many-locals
        api_config = Config.get().api_conf("dynamo0.3")
        const = LFRicConstants()
        basis_declarations = []

        # We need BASIS and/or DIFF_BASIS if any kernel requires quadrature
        # or an evaluator
        if self._qr_vars or self._eval_targets:
            parent.add(
                UseGen(parent, name=const.
                       FUNCTION_SPACE_TYPE_MAP["function_space"]["module"],
                       only=True, funcnames=["BASIS", "DIFF_BASIS"]))

        if self._qr_vars:
            parent.add(CommentGen(parent, ""))
            parent.add(CommentGen(parent, " Look-up quadrature variables"))
            parent.add(CommentGen(parent, ""))

            # Look-up the module- and type-names from the QUADRATURE_TYPE_MAP
            for shp in self._qr_vars:
                quad_map = const.QUADRATURE_TYPE_MAP[shp]
                parent.add(UseGen(parent,
                                  name=quad_map["module"],
                                  only=True,
                                  funcnames=[quad_map["type"],
                                             quad_map["proxy_type"]]))
            self._initialise_xyz_qr(parent)
            self._initialise_xyoz_qr(parent)
            self._initialise_xoyoz_qr(parent)
            self._initialise_face_or_edge_qr(parent, "face")
            self._initialise_face_or_edge_qr(parent, "edge")

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
            my_kind = api_config.default_kind["real"]
            parent.add(DeclGen(parent, datatype="real",
                               kind=my_kind,
                               pointer=True,
                               entity_decls=[nodes_name+"(:,:) => null()"]))
            const_mod = const.UTILITIES_MOD_MAP["constants"]["module"]
            const_mod_list = self._invoke.invokes.psy. \
                infrastructure_modules[const_mod]
            if my_kind not in const_mod_list:
                # Record that we will need to import the kind for a
                # pointer declaration (associated with a function
                # space) from the appropriate infrastructure module
                const_mod_list.append(my_kind)

        if self._basis_fns:
            parent.add(CommentGen(parent, ""))
            parent.add(CommentGen(parent, " Allocate basis/diff-basis arrays"))
            parent.add(CommentGen(parent, ""))

        var_dim_list = []
        for basis_fn in self._basis_fns:
            # Get the extent of the first dimension of the basis array.
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
                               kind=api_config.default_kind["integer"],
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
            my_kind = api_config.default_kind["real"]
            parent.add(DeclGen(parent, datatype="real", kind=my_kind,
                               allocatable=True,
                               entity_decls=basis_declarations))
            const_mod = const.UTILITIES_MOD_MAP["constants"]["module"]
            const_mod_list = self._invoke.invokes.psy. \
                infrastructure_modules[const_mod]
            # Default kind (r_def) will always already exist due to
            # arrays associated with gh_shape, so there is no need to
            # declare it here.

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
        :raises InternalError: if there is no name for the quadrature object \
                               when generating PSy-layer code.

        '''
        # pylint: disable=too-many-branches
        # Dictionary of basis arrays where key values are the array names and
        # entries are a list of dimensions.
        basis_arrays = OrderedDict()
        # List of names of dimensioning (scalar) variables
        var_dim_list = []

        const = LFRicConstants()
        # Loop over the list of dicts describing each basis function
        # required by this Invoke.
        for basis_fn in self._basis_fns:
            # Get the extent of the first dimension of the basis array and
            # store whether we have a basis or a differential basis function.
            # Currently there are only those two possible types of basis
            # function and we store the required diff basis name in basis_name.
            if basis_fn['type'] == "basis":
                if self._invoke:
                    first_dim = self.basis_first_dim_name(basis_fn["fspace"])
                elif self._kernel:
                    first_dim = self.basis_first_dim_value(basis_fn["fspace"])
                else:
                    raise InternalError("Require basis functions but do not "
                                        "have either a Kernel or an "
                                        "Invoke. Should be impossible.")
                basis_name = "gh_basis"
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
                basis_name = "gh_diff_basis"
            else:
                raise InternalError(
                    "Unrecognised type of basis function: '{0}'. Should "
                    "be either 'basis' or 'diff-basis'.".format(
                        basis_fn['type']))

            if self._invoke and first_dim not in var_dim_list:
                var_dim_list.append(first_dim)

            if basis_fn["shape"] in const.VALID_QUADRATURE_SHAPES:

                qr_var = basis_fn["qr_var"]
                if not qr_var:
                    raise InternalError(
                        "Quadrature '{0}' is required but have no name for the"
                        " associated Quadrature object.".format(
                            basis_fn["shape"]))

                op_name = basis_fn["fspace"].get_operator_name(basis_name,
                                                               qr_var=qr_var)
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
                    op_name = basis_fn["fspace"].\
                        get_operator_name(basis_name,
                                          qr_var=basis_fn["qr_var"],
                                          on_space=target_space)
                    if op_name in basis_arrays:
                        continue
                    # We haven't seen a basis with this name before so
                    # need to store its dimensions
                    basis_arrays[op_name] = [
                        first_dim,
                        basis_fn["fspace"].ndf_name,
                        target_space.ndf_name]
            else:
                raise InternalError(
                    "Unrecognised evaluator shape: '{0}'. Should be one of "
                    "{1}".format(basis_fn["shape"],
                                 const.VALID_EVALUATOR_SHAPES))

        return (var_dim_list, basis_arrays)

    def _initialise_xyz_qr(self, parent):
        '''
        Add in the initialisation of variables needed for XYZ
        quadrature

        :param parent: the node in the AST representing the PSy subroutine
                       in which to insert the initialisation
        :type parent: :py:class:`psyclone.f2pygen.SubroutineGen`

        '''
        # pylint: disable=no-self-use,unused-argument
        # This shape is not yet supported so we do nothing
        return

    def _initialise_xyoz_qr(self, parent):
        '''
        Add in the initialisation of variables needed for XYoZ
        quadrature

        :param parent: the node in the AST representing the PSy subroutine
                       in which to insert the initialisation
        :type parent: :py:class:`psyclone.f2pygen.SubroutineGen`

        '''
        api_config = Config.get().api_conf("dynamo0.3")

        if "gh_quadrature_xyoz" not in self._qr_vars:
            return

        for qr_arg_name in self._qr_vars["gh_quadrature_xyoz"]:

            # We generate unique names for the integers holding the numbers
            # of quadrature points by appending the name of the quadrature
            # argument
            parent.add(
                DeclGen(
                    parent, datatype="integer",
                    kind=api_config.default_kind["integer"],
                    entity_decls=[name+"_"+qr_arg_name
                                  for name in self.qr_dim_vars["xyoz"]]))
            decl_list = [name+"_"+qr_arg_name+"(:) => null()"
                         for name in self.qr_weight_vars["xyoz"]]
            const = LFRicConstants()
            datatype = \
                const.QUADRATURE_TYPE_MAP["gh_quadrature_xyoz"]["intrinsic"]
            kind = const.QUADRATURE_TYPE_MAP["gh_quadrature_xyoz"]["kind"]
            parent.add(
                DeclGen(parent, datatype=datatype, kind=kind,
                        pointer=True, entity_decls=decl_list))
            const_mod = const.UTILITIES_MOD_MAP["constants"]["module"]
            const_mod_list = self._invoke.invokes.psy. \
                infrastructure_modules[const_mod]
            if kind not in const_mod_list:
                # Record that we will need to import the kind for a
                # declaration (associated with quadrature) from
                # the appropriate infrastructure module
                const_mod_list.append(kind)

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
        quadrature.

        :param parent: the node in the AST representing the PSy subroutine \
                       in which to insert the initialisation.
        :type parent: :py:class:`psyclone.f2pygen.SubroutineGen`

        '''
        # pylint: disable=no-self-use,unused-argument
        # This shape is not yet supported so we do nothing
        return

    def _initialise_face_or_edge_qr(self, parent, qr_type):
        '''
        Add in the initialisation of variables needed for face or edge
        quadrature.

        :param parent: the node in the AST representing the PSy subroutine \
                       in which to insert the initialisation.
        :type parent: :py:class:`psyclone.f2pygen.SubroutineGen`
        :param str qr_type: whether to generate initialisation code for \
                            "face" or "edge" quadrature.

        :raises InternalError: if `qr_type` is not "face" or "edge".

        '''
        if qr_type not in ["face", "edge"]:
            raise InternalError(
                "_initialise_face_or_edge_qr: qr_type argument must be either "
                "'face' or 'edge' but got: '{0}'".format(qr_type))

        quadrature_name = "gh_quadrature_" + qr_type

        if quadrature_name not in self._qr_vars:
            return

        api_config = Config.get().api_conf("dynamo0.3")
        symbol_table = self._symbol_table

        for qr_arg_name in self._qr_vars[quadrature_name]:
            # We generate unique names for the integers holding the numbers
            # of quadrature points by appending the name of the quadrature
            # argument
            decl_list = [
                symbol_table.find_or_create_tag(name+"_"+qr_arg_name).name
                for name in self.qr_dim_vars[qr_type]]
            parent.add(DeclGen(parent, datatype="integer",
                               kind=api_config.default_kind["integer"],
                               entity_decls=decl_list))

            decl_list = [
                symbol_table.find_or_create_tag(name+"_"+qr_arg_name).name
                + "(:,:) => null()" for name in self.qr_weight_vars[qr_type]]
            const = LFRicConstants()
            datatype = const.QUADRATURE_TYPE_MAP[quadrature_name]["intrinsic"]
            kind = const.QUADRATURE_TYPE_MAP[quadrature_name]["kind"]
            parent.add(
                DeclGen(parent, datatype=datatype, pointer=True, kind=kind,
                        entity_decls=decl_list))
            const_mod = const.UTILITIES_MOD_MAP["constants"]["module"]
            const_mod_list = self._invoke.invokes.psy. \
                infrastructure_modules[const_mod]
            if kind not in const_mod_list:
                # Record that we will need to import the kind for a
                # declaration (associated with quadrature) from the
                # appropriate infrastructure module
                const_mod_list.append(kind)
            # Get the quadrature proxy
            proxy_name = symbol_table.find_or_create_tag(
                qr_arg_name+"_proxy").name
            parent.add(
                AssignGen(parent, lhs=proxy_name,
                          rhs=qr_arg_name+"%"+"get_quadrature_proxy()"))
            # The dimensioning variables required for this quadrature
            # (e.g. nedges/nfaces, np_xyz)
            for qr_var in self.qr_dim_vars[qr_type]:
                parent.add(
                    AssignGen(parent, lhs=qr_var+"_"+qr_arg_name,
                              rhs=proxy_name+"%"+qr_var))
            # Pointers to the weights arrays
            for qr_var in self.qr_weight_vars[qr_type]:
                parent.add(
                    AssignGen(parent, pointer=True,
                              lhs=qr_var+"_"+qr_arg_name,
                              rhs=proxy_name+"%"+qr_var))

    def _compute_basis_fns(self, parent):
        '''
        Generates the necessary Fortran to compute the values of
        any basis/diff-basis arrays required

        :param parent: Node in the f2pygen AST which will be the parent
                       of the assignments created in this routine
        :type parent: :py:class:`psyclone.f2pygen.SubroutineGen`

        '''
        # pylint: disable=too-many-locals
        const = LFRicConstants()
        api_config = Config.get().api_conf("dynamo0.3")

        loop_var_list = set()
        op_name_list = []
        # add calls to compute the values of any basis arrays
        if self._basis_fns:
            parent.add(CommentGen(parent, ""))
            parent.add(CommentGen(parent, " Compute basis/diff-basis arrays"))
            parent.add(CommentGen(parent, ""))

        for basis_fn in self._basis_fns:

            # Currently there are only two possible types of basis function
            # and we store the corresponding strings to use in basis_name,
            # basis_type, and first_dim. If support for other basis function
            # types is added in future then more tests need to be added here.
            if basis_fn["type"] == "diff-basis":
                basis_name = "gh_diff_basis"
                basis_type = "DIFF_BASIS"
                first_dim = self.diff_basis_first_dim_name(basis_fn["fspace"])
            elif basis_fn["type"] == "basis":
                basis_name = "gh_basis"
                basis_type = "BASIS"
                first_dim = self.basis_first_dim_name(basis_fn["fspace"])
            else:
                raise InternalError(
                    "Unrecognised type of basis function: '{0}'. Expected one "
                    "of 'basis' or 'diff-basis'.". format(basis_fn["type"]))
            if basis_fn["shape"] in const.VALID_QUADRATURE_SHAPES:
                op_name = basis_fn["fspace"].\
                    get_operator_name(basis_name, qr_var=basis_fn["qr_var"])
                if op_name in op_name_list:
                    # Jump over any basis arrays we've seen before
                    continue
                op_name_list.append(op_name)

                # Create the argument list
                args = [basis_type, basis_fn["arg"].proxy_name_indexed + "%" +
                        basis_fn["arg"].ref_name(basis_fn["fspace"]),
                        first_dim, basis_fn["fspace"].ndf_name, op_name]

                # insert the basis array call
                parent.add(
                    CallGen(parent,
                            name=basis_fn["qr_var"]+"%compute_function",
                            args=args))
            elif basis_fn["shape"].lower() == "gh_evaluator":
                # We have an evaluator. We may need this on more than one
                # function space.
                for space in basis_fn["nodal_fspaces"]:
                    op_name = basis_fn["fspace"].\
                        get_operator_name(basis_name, on_space=space)
                    if op_name in op_name_list:
                        # Jump over any basis arrays we've seen before
                        continue
                    op_name_list.append(op_name)

                    nodal_loop_var = "df_nodal"
                    loop_var_list.add(nodal_loop_var)

                    # Loop over dofs of target function space
                    nodal_dof_loop = DoGen(
                        parent, nodal_loop_var, "1", space.ndf_name)
                    parent.add(nodal_dof_loop)

                    dof_loop_var = "df_" + basis_fn["fspace"].mangled_name
                    loop_var_list.add(dof_loop_var)

                    dof_loop = DoGen(nodal_dof_loop, dof_loop_var,
                                     "1", basis_fn["fspace"].ndf_name)
                    nodal_dof_loop.add(dof_loop)
                    lhs = op_name + "(:," + "df_" + \
                        basis_fn["fspace"].mangled_name + "," + "df_nodal)"
                    rhs = "{0}%{1}%call_function({2},{3},nodes_{4}(:,{5}))"\
                        .format(basis_fn["arg"].proxy_name_indexed,
                                basis_fn["arg"].ref_name(basis_fn["fspace"]),
                                basis_type, dof_loop_var, space.mangled_name,
                                nodal_loop_var)
                    dof_loop.add(AssignGen(dof_loop, lhs=lhs, rhs=rhs))
            else:
                raise InternalError(
                    "Unrecognised shape '{0}' specified for basis function. "
                    "Should be one of: {1}"
                    .format(basis_fn['shape'], const.VALID_EVALUATOR_SHAPES))
        if loop_var_list:
            # Declare any loop variables
            parent.add(DeclGen(parent, datatype="integer",
                               kind=api_config.default_kind["integer"],
                               entity_decls=sorted(loop_var_list)))

    def deallocate(self, parent):
        '''
        Add code to deallocate all basis/diff-basis function arrays

        :param parent: node in the f2pygen AST to which the deallocate \
                       calls will be added.
        :type parent: :py:class:`psyclone.f2pygen.SubroutineGen`

        :raises InternalError: if an unrecognised type of basis function \
                               is encountered.
        '''
        if self._basis_fns:
            # deallocate all allocated basis function arrays
            parent.add(CommentGen(parent, ""))
            parent.add(CommentGen(parent, " Deallocate basis arrays"))
            parent.add(CommentGen(parent, ""))

        func_space_var_names = set()
        for basis_fn in self._basis_fns:
            # add the basis array name to the list to use later
            if basis_fn["type"] == "basis":
                basis_name = "gh_basis"
            elif basis_fn["type"] == "diff-basis":
                basis_name = "gh_diff_basis"
            else:
                raise InternalError(
                    "Unrecognised type of basis function: '{0}'. Should be "
                    "one of 'basis' or 'diff-basis'.".format(basis_fn["type"]))
            for fspace in basis_fn["nodal_fspaces"]:
                op_name = basis_fn["fspace"].\
                    get_operator_name(basis_name,
                                      qr_var=basis_fn["qr_var"],
                                      on_space=fspace)
                func_space_var_names.add(op_name)

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
        :type parent: :py:class:`psyclone.psyir.nodes.Node`

        '''
        api_config = Config.get().api_conf("dynamo0.3")

        for dofs in self._boundary_dofs:
            name = "boundary_dofs_" + dofs.argument.name
            parent.add(DeclGen(parent, datatype="integer",
                               kind=api_config.default_kind["integer"],
                               pointer=True,
                               entity_decls=[name+"(:,:) => null()"]))

    def _stub_declarations(self, parent):
        '''
        Add declarations for any boundary-dofs arrays required by a kernel.

        :param parent: node in the PSyIR to which to add declarations.
        :type parent: :py:class:`psyclone.psyir.nodes.Node`

        '''
        api_config = Config.get().api_conf("dynamo0.3")

        for dofs in self._boundary_dofs:
            name = "boundary_dofs_" + dofs.argument.name
            ndf_name = dofs.function_space.ndf_name
            parent.add(DeclGen(parent, datatype="integer",
                               kind=api_config.default_kind["integer"],
                               intent="in",
                               dimension=",".join([ndf_name, "2"]),
                               entity_decls=[name]))

    def initialise(self, parent):
        '''
        Initialise any boundary-dofs arrays required by an Invoke.

        :param parent: node in PSyIR to which to add declarations.
        :type parent: :py:class:`psyclone.psyir.nodes.Node`

        '''
        for dofs in self._boundary_dofs:
            name = "boundary_dofs_" + dofs.argument.name
            parent.add(AssignGen(
                parent, pointer=True, lhs=name,
                rhs="%".join([dofs.argument.proxy_name,
                              dofs.argument.ref_name(dofs.function_space),
                              "get_boundary_dofs()"])))


class DynInvoke(Invoke):
    '''The Dynamo specific invoke class. This passes the Dynamo specific
    InvokeSchedule class to the base class so it creates the one we
    require.  Also overrides the gen_code method so that we generate
    dynamo specific invocation code.

    :param alg_invocation: object containing the invoke call information.
    :type alg_invocation: :py:class:`psyclone.parse.algorithm.InvokeCall`
    :param int idx: the position of the invoke in the list of invokes \
        contained in the Algorithm.
    :param invokes: the Invokes object containing this DynInvoke \
        object.
    :type invokes: :py:class:`psyclone.dynamo0p3.DynamoInvokes`

    :raises GenerationError: if integer reductions are required in the \
        psy-layer.

    '''
    # pylint: disable=too-many-instance-attributes
    def __init__(self, alg_invocation, idx, invokes):
        if not alg_invocation and not idx:
            # This if test is added to support pyreverse.
            return
        self._schedule = DynInvokeSchedule('name', None)  # for pyreverse
        reserved_names_list = []
        const = LFRicConstants()
        reserved_names_list.extend(const.STENCIL_MAPPING.values())
        reserved_names_list.extend(const.VALID_STENCIL_DIRECTIONS)
        reserved_names_list.extend(["omp_get_thread_num",
                                    "omp_get_max_threads"])
        Invoke.__init__(self, alg_invocation, idx, DynInvokeSchedule,
                        invokes, reserved_names=reserved_names_list)

        # The baseclass works out the algorithm code's unique argument
        # list and stores it in the self._alg_unique_args
        # list. However, the base class currently ignores any stencil and qr
        # arguments so we need to add them in.

        self.scalar_args = LFRicScalarArgs(self)

        # initialise our invoke stencil information
        self.stencil = DynStencils(self)

        # Initialise our information on the function spaces used by this Invoke
        self.function_spaces = DynFunctionSpaces(self)

        # Initialise the object holding all information on the dofmaps
        # required by this invoke.
        self.dofmaps = DynDofmaps(self)

        # Initialise information on all of the fields accessed in this Invoke.
        self.fields = LFRicFields(self)

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

        # Run-time checks for this invoke
        self.run_time_checks = LFRicRunTimeChecks(self)

        # Information required by kernels that operate on cell-columns
        self.cell_iterators = DynCellIterators(self)

        # Information on the required properties of the reference element
        self.reference_element_properties = DynReferenceElement(self)

        # Properties of the mesh
        self.mesh_properties = LFRicMeshProperties(self)

        # Manage the variables used to store the upper and lower limits of
        # all loops in this Invoke.
        self.loop_bounds = LFRicLoopBounds(self)

        # Extend arg list with stencil information
        self._alg_unique_args.extend(self.stencil.unique_alg_vars)

        # adding in qr arguments
        self._alg_unique_qr_args = []
        for call in self.schedule.kernels():
            for rule in call.qr_rules.values():
                if rule.alg_name not in self._alg_unique_qr_args:
                    self._alg_unique_qr_args.append(rule.alg_name)
        self._alg_unique_args.extend(self._alg_unique_qr_args)
        # we also need to work out the names to use for the qr
        # arguments within the psy layer. These are stored in the
        # _psy_unique_qr_vars list
        self._psy_unique_qr_vars = []
        for call in self.schedule.kernels():
            for rule in call.qr_rules.values():
                if rule.psy_name not in self._psy_unique_qr_vars:
                    self._psy_unique_qr_vars.append(rule.psy_name)

        # lastly, add in halo exchange calls and global sums if
        # required. We only need to add halo exchange calls for fields
        # since operators are assembled in place and scalars don't
        # have halos. We only need to add global sum calls for scalars
        # which have a gh_sum access.
        if Config.get().distributed_memory:
            # halo exchange calls
            const = LFRicConstants()
            for loop in self.schedule.loops():
                loop.create_halo_exchanges()
            # global sum calls
            for loop in self.schedule.loops():
                for scalar in loop.args_filter(
                        arg_types=const.VALID_SCALAR_NAMES,
                        arg_accesses=AccessType.get_valid_reduction_modes(),
                        unique=True):
                    global_sum = DynGlobalSum(scalar, parent=loop.parent)
                    loop.parent.children.insert(loop.position+1, global_sum)

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

    @property
    def operates_on_dofs_only(self):
        '''
        :returns: whether or not this Invoke consists only of kernels that \
                  operate on DoFs.
        :rtype: bool
        '''
        return all(call.iterates_over.lower() == "dof" for call in
                   self.schedule.kernels())

    def field_on_space(self, func_space):
        ''' If a field exists on this space for any kernel in this
        invoke then return that field. Otherwise return None. '''
        for kern_call in self.schedule.kernels():
            field = func_space.field_on_space(kern_call.arguments)
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
        # Create the subroutine
        invoke_sub = SubroutineGen(parent, name=self.name,
                                   args=self.psy_unique_var_names +
                                   self.stencil.unique_alg_vars +
                                   self._psy_unique_qr_vars)

        # Declare all quantities required by this PSy routine (invoke)
        for entities in [self.scalar_args, self.fields, self.lma_ops,
                         self.stencil, self.meshes,
                         self.function_spaces, self.dofmaps, self.cma_ops,
                         self.boundary_conditions, self.evaluators,
                         self.proxies, self.cell_iterators,
                         self.reference_element_properties,
                         self.mesh_properties, self.loop_bounds,
                         self.run_time_checks]:
            entities.declarations(invoke_sub)

        # Initialise all quantities required by this PSy routine (invoke)

        if self.schedule.reductions(reprod=True):
            # We have at least one reproducible reduction so we need
            # to know the number of OpenMP threads
            omp_function_name = "omp_get_max_threads"
            tag = "omp_num_threads"
            nthreads_name = \
                self.schedule.symbol_table.lookup_with_tag(tag).name
            invoke_sub.add(UseGen(invoke_sub, name="omp_lib", only=True,
                                  funcnames=[omp_function_name]))
            # Note: There is no assigned kind for integer nthreads as this
            # would imply assigning kind to th_idx and other elements of
            # the OMPParallelDirective
            invoke_sub.add(DeclGen(invoke_sub, datatype="integer",
                                   entity_decls=[nthreads_name]))
            invoke_sub.add(CommentGen(invoke_sub, ""))
            invoke_sub.add(CommentGen(
                invoke_sub, " Determine the number of OpenMP threads"))
            invoke_sub.add(CommentGen(invoke_sub, ""))
            invoke_sub.add(AssignGen(invoke_sub, lhs=nthreads_name,
                                     rhs=omp_function_name+"()"))

        for entities in [self.proxies, self.run_time_checks,
                         self.cell_iterators, self.meshes,
                         self.stencil, self.dofmaps,
                         self.cma_ops, self.boundary_conditions,
                         self.function_spaces, self.evaluators,
                         self.reference_element_properties,
                         self.mesh_properties, self.loop_bounds]:
            entities.initialise(invoke_sub)

        # Now that everything is initialised and checked, we can call
        # our kernels

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
    to the base class so it creates the ones we require.

    :param str name: name of the Invoke.
    :param arg: list of KernelCalls parsed from the algorithm layer.
    :type arg: list of :py:class:`psyclone.parse.algorithm.KernelCall`
    :param reserved_names: optional list of names that are not allowed in the \
                           new InvokeSchedule SymbolTable.
    :type reserved_names: list of str
    :param parent: the parent of this node in the PSyIR.
    :type parent: :py:class:`psyclone.psyir.nodes.Node`

    '''

    def __init__(self, name, arg, reserved_names=None, parent=None):
        InvokeSchedule.__init__(self, name, DynKernCallFactory,
                                LFRicBuiltInCallFactory, arg, reserved_names,
                                parent=parent)

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
    manipulated in a schedule.

    :param scalar: the kernel argument for which to perform a global sum.
    :type scalar: :py:class:`psyclone.dynamo0p3.DynKernelArgument`
    :param parent: the parent node of this node in the PSyIR.
    :type parent: :py:class:`psyclone.psyir.nodes.Node`

    :raises GenerationError: if distributed memory is not enabled.
    :raises InternalError: if the supplied argument is not a scalar.
    :raises GenerationError: if the scalar is not of "real" intrinsic type.

    '''
    def __init__(self, scalar, parent=None):
        # Check that distributed memory is enabled
        if not Config.get().distributed_memory:
            raise GenerationError(
                "It makes no sense to create a DynGlobalSum object when "
                "distributed memory is not enabled (dm=False).")
        # Check that the global sum argument is indeed a scalar
        if not scalar.is_scalar:
            raise InternalError(
                "DynGlobalSum.init(): A global sum argument should be "
                "a scalar but found argument of type '{0}'.".
                format(scalar.argument_type))
        # Check scalar intrinsic types that this class supports (only
        # "real" for now)
        if scalar.intrinsic_type != "real":
            raise GenerationError(
                "DynGlobalSum currently only supports real scalars, but "
                "argument '{0}' in Kernel '{1}' has '{2}' intrinsic type.".
                format(scalar.name, scalar.call.name, scalar.intrinsic_type))
        # Initialise the parent class
        super(DynGlobalSum, self).__init__(scalar, parent=parent)

    def gen_code(self, parent):
        '''
        Dynamo-specific code generation for this class.

        :param parent: f2pygen node to which to add AST nodes.
        :type parent: :py:class:`psyclone.f2pygen.SubroutineGen`

        '''
        name = self._scalar.name
        # Use InvokeSchedule SymbolTable to share the same symbol for all
        # GlobalSums in the Invoke.
        sum_name = self.ancestor(InvokeSchedule).symbol_table.\
            find_or_create_tag("global_sum").name
        sum_type = self._scalar.data_type
        sum_mod = self._scalar.module_name
        parent.add(UseGen(parent, name=sum_mod, only=True,
                          funcnames=[sum_type]))
        parent.add(TypeDeclGen(parent, datatype=sum_type,
                               entity_decls=[sum_name]))
        parent.add(AssignGen(parent, lhs=sum_name+"%value", rhs=name))
        parent.add(AssignGen(parent, lhs=name, rhs=sum_name+"%get_sum()"))


def _create_depth_list(halo_info_list, sym_table):
    '''Halo exchanges may have more than one dependency. This method
    simplifies multiple dependencies to remove duplicates and any
    obvious redundancy. For example, if one dependency is for depth=1
    and another for depth=2 then we do not need the former as it is
    covered by the latter. Similarly, if we have a depth=extent+1 and
    another for depth=extent+2 then we do not need the former as it is
    covered by the latter. It also takes into account
    needs_clean_outer, which indicates whether the outermost halo
    needs to be clean (and therefore whether there is a dependence).

    :param halo_info_list: a list containing halo access information \
        derived from all read fields dependent on this halo exchange.
    :type: :func:`list` of :py:class:`psyclone.dynamo0p3.HaloReadAccess`
    :param sym_table: the symbol table of the enclosing InvokeSchedule.
    :type sym_table: :py:class:`psyclone.psyir.symbols.SymbolTable`

    :returns: a list containing halo depth information derived from \
        the halo access information.
    :rtype: :func:`list` of :py:class:`psyclone.dynamo0p3.HaloDepth`

    '''
    # pylint: disable=too-many-branches
    depth_info_list = []
    # First look to see if all field dependencies are
    # annexed_only. If so we only care about annexed dofs.
    annexed_only = True
    for halo_info in halo_info_list:
        if not (halo_info.annexed_only or
                (halo_info.literal_depth == 1
                 and not halo_info.needs_clean_outer)):
            # There are two cases when we only care about accesses to
            # annexed dofs. 1) when annexed_only is set and 2) when
            # the halo depth is 1 but we only depend on annexed dofs
            # being up-to-date (needs_clean_outer is False).
            annexed_only = False
            break
    if annexed_only:
        depth_info = HaloDepth(sym_table)
        depth_info.set_by_value(max_depth=False, var_depth="",
                                literal_depth=1, annexed_only=True,
                                max_depth_m1=False)
        return [depth_info]
    # Next look to see if one of the field dependencies specifies
    # a max_depth access. If so the whole halo region is accessed
    # so we do not need to be concerned with other accesses.
    max_depth_m1 = False
    for halo_info in halo_info_list:
        if halo_info.max_depth:
            if halo_info.needs_clean_outer:
                # Found a max_depth access so we only need one
                # HaloDepth entry.
                depth_info = HaloDepth(sym_table)
                depth_info.set_by_value(max_depth=True, var_depth="",
                                        literal_depth=0, annexed_only=False,
                                        max_depth_m1=False)
                return [depth_info]
            # Remember that we found a max_depth-1 access.
            max_depth_m1 = True

    if max_depth_m1:
        # We have at least one max_depth-1 access.
        depth_info = HaloDepth(sym_table)
        depth_info.set_by_value(max_depth=False, var_depth="",
                                literal_depth=0, annexed_only=False,
                                max_depth_m1=True)
        depth_info_list.append(depth_info)

    for halo_info in halo_info_list:
        # Go through the halo information associated with each
        # read dependency, skipping any max_depth-1 accesses.
        if halo_info.max_depth and not halo_info.needs_clean_outer:
            continue
        var_depth = halo_info.var_depth
        literal_depth = halo_info.literal_depth
        if literal_depth and not halo_info.needs_clean_outer:
            # Decrease depth by 1 if we don't care about the outermost
            # access.
            literal_depth -= 1
        match = False
        # check whether we match with existing depth information
        for depth_info in depth_info_list:
            if depth_info.var_depth == var_depth and not match:
                # This dependence uses the same variable to
                # specify its depth as an existing one, or both do
                # not have a variable so we only have a
                # literal. Therefore we only need to update the
                # literal value with the maximum of the two
                # (e.g. var_name,1 and var_name,2 => var_name,2).
                depth_info.literal_depth = max(
                    depth_info.literal_depth, literal_depth)
                match = True
                break
        if not match:
            # No matches were found with existing entries so create a
            # new one (unless no 'var_depth' and 'literal_depth' is 0).
            if var_depth or literal_depth > 0:
                depth_info = HaloDepth(sym_table)
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
    :type parent: :py:class:`psyclone.psyir.nodes.Node`

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

    def _compute_halo_read_depth_info(self, ignore_hex_dep=False):
        '''Take a list of `psyclone.dynamo0p3.HaloReadAccess` objects and
        create an equivalent list of `psyclone.dynamo0p3.HaloDepth`
        objects. Whilst doing this we simplify the
        `psyclone.dynamo0p3.HaloDepth` list to remove redundant depth
        information e.g. depth=1 is not required if we have a depth=2.
        If the optional ignore_hex_dep argument is set to True then
        any read accesses contained in halo exchange nodes are
        ignored. This option can therefore be used to filter out any
        halo exchange dependencies and only return non-halo exchange
        dependencies if and when required.

        :param bool ignore_hex_dep: if True then ignore any read \
            accesses contained in halo exchanges. This is an optional \
            argument that defaults to False.

        :return: a list containing halo depth information derived from \
            all fields dependent on this halo exchange.
        :rtype: :func:`list` of :py:class:`psyclone.dynamo0p3.HaloDepth`

        '''
        # get our halo information
        halo_info_list = self._compute_halo_read_info(ignore_hex_dep)
        # use the halo information to generate depth information
        depth_info_list = _create_depth_list(halo_info_list,
                                             self._symbol_table)
        return depth_info_list

    def _compute_halo_read_info(self, ignore_hex_dep=False):
        '''Dynamically computes all halo read dependencies and returns the
        required halo information (i.e. halo depth and stencil type)
        in a list of HaloReadAccess objects. If the optional
        ignore_hex_dep argument is set to True then any read accesses
        contained in halo exchange nodes are ignored. This option can
        therefore be used to filter out any halo exchange dependencies
        and only return non-halo exchange dependencies if and when
        required.

        :param bool ignore_hex_dep: if True then ignore any read \
            accesses contained in halo exchanges. This is an optional \
            argument that defaults to False.

        :return: a list containing halo information for each read dependency.
        :rtype: :func:`list` of :py:class:`psyclone.dynamo0p3.HaloReadAccess`

        :raises InternalError: if there is more than one read \
            dependency associated with a halo exchange.
        :raises InternalError: if there is a read dependency \
            associated with a halo exchange and it is not the last \
            entry in the read dependency list.
        :raises GenerationError: if there is a read dependency \
            associated with an asynchronous halo exchange.
        :raises InternalError: if no read dependencies are found.

        '''
        read_dependencies = self.field.forward_read_dependencies()
        hex_deps = [dep for dep in read_dependencies
                    if isinstance(dep.call, DynHaloExchange)]
        if hex_deps:
            # There is a field accessed by a halo exchange that is
            # a read dependence. As ignore_hex_dep is True this should
            # be ignored so this is removed from the list.
            if any(dep for dep in hex_deps
                   if isinstance(dep.call, (DynHaloExchangeStart,
                                            DynHaloExchangeEnd))):
                raise GenerationError(
                    "Please perform redundant computation transformations "
                    "before asynchronous halo exchange transformations.")

            # There can only be one field accessed by a
            # halo exchange that is a read dependence.
            if len(hex_deps) != 1:
                raise InternalError(
                    "There should only ever be at most one read dependency "
                    "associated with a halo exchange in the read dependency "
                    "list, but found {0} for field {1}."
                    "".format(len(hex_deps), self.field.name))
            # For sanity, check that the field accessed by the halo
            # exchange is the last dependence in the list.
            if not isinstance(read_dependencies[-1].call, DynHaloExchange):
                raise InternalError(
                    "If there is a read dependency associated with a halo "
                    "exchange in the list of read dependencies then it should "
                    "be the last one in the list.")
            if ignore_hex_dep:
                # Remove the last entry in the list (the field accessed by
                # the halo exchange).
                del read_dependencies[-1]

        if not read_dependencies:
            raise InternalError(
                "Internal logic error. There should be at least one read "
                "dependence for a halo exchange.")
        return [HaloReadAccess(read_dependency, self._symbol_table) for
                read_dependency in read_dependencies]

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
        return HaloWriteAccess(write_dependencies[0], self._symbol_table)

    def required(self, ignore_hex_dep=False):
        '''Determines whether this halo exchange is definitely required
        ``(True, True)``, might be required ``(True, False)`` or is definitely
        not required ``(False, *)``.

        If the optional ignore_hex_dep argument is set to True then
        any read accesses contained in halo exchange nodes are
        ignored. This option can therefore be used to filter out any
        halo exchange dependencies and only consider non-halo exchange
        dependencies if and when required.

        Whilst a halo exchange is generally only ever added if it is
        required, or if it may be required, this situation can change
        if redundant computation transformations are applied. The
        first argument can be used to remove such halo exchanges if
        required.

        When the first return value is True, the second return value
        can be used to see if we need to rely on the runtime
        (set_dirty and set_clean calls) and therefore add a
        check_dirty() call around the halo exchange or whether we
        definitely know that this halo exchange is required.

        This routine assumes that a stencil size provided via a
        variable may take the value 0. If a variables value is
        constrained to be 1, or more, then the logic for deciding
        whether a halo exchange is definitely required should be
        updated. Note, the routine would still be correct as is, it
        would just return more unknown results than it should).

        :param bool ignore_hex_dep: if True then ignore any read \
            accesses contained in halo exchanges. This is an optional \
            argument that defaults to False.

        :returns: (x, y) where x specifies whether this halo \
            exchange is (or might be) required - True, or is not \
            required - False. If the first tuple item is True then the \
            second argument specifies whether we definitely know that \
            we need the HaloExchange - True, or are not sure - False.
        :rtype: (bool, bool)

        '''
        # pylint: disable=too-many-branches, too-many-return-statements
        # get *aggregated* information about halo reads
        required_clean_info = self._compute_halo_read_depth_info(
            ignore_hex_dep)
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
        if self.vector_index:
            ref = f"({self.vector_index})"
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
    :type parent: :py:class:`psyclone.psyir.nodes.Node`

    '''
    # Textual description of the node.
    _text_name = "HaloExchangeStart"
    _colour = "yellow"

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

    def _compute_stencil_type(self):
        '''Call the required method in the corresponding halo exchange end
        object. This is done as the field in halo exchange start is
        only read and the dependence analysis beneath this call
        requires the field to be modified.

        :return: Return the type of stencil required for this pair of \
        halo exchanges
        :rtype: str

        '''
        # pylint: disable=protected-access
        return self._get_hex_end()._compute_stencil_type()

    def _compute_halo_depth(self):
        '''Call the required method in the corresponding halo exchange end
        object. This is done as the field in halo exchange start is
        only read and the dependence analysis beneath this call
        requires the field to be modified.

        :return: Return the halo exchange depth as a Fortran string
        :rtype: str

        '''
        # pylint: disable=protected-access
        return self._get_hex_end()._compute_halo_depth()

    def required(self):
        '''Call the required method in the corresponding halo exchange end
        object. This is done as the field in halo exchange start is
        only read and the dependence analysis beneath this call
        requires the field to be modified.

        :returns: (x, y) where x specifies whether this halo exchange \
                  is (or might be) required - True, or is not required \
                  - False. If the first tuple item is True then the second \
                  argument specifies whether we definitely know that we need \
                  the HaloExchange - True, or are not sure - False.
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
    :type parent: :py:class:`psyclone.psyir.nodes.Node`

    '''
    # Textual description of the node.
    _text_name = "HaloExchangeEnd"
    _colour = "yellow"

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


class HaloDepth(object):
    '''Determines how much of the halo a read to a field accesses (the
    halo depth).

    :param sym_table: the symbol table of the enclosing InvokeSchedule.
    :type sym_table: :py:class:`psyclone.psyir.symbols.SymbolTable`

    '''
    def __init__(self, sym_table):
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
        # Keep a reference to the symbol table so that we can look-up
        # variables holding the maximum halo depth.
        self._symbol_table = sym_table

    @property
    def annexed_only(self):
        '''
        :returns: True if only annexed dofs are accessed in the halo and \
                  False otherwise.
        :rtype: bool
        '''
        return self._annexed_only

    @property
    def max_depth(self):
        '''
        :returns: True if the read to the field is known to access all \
                  of the halo and False otherwise.
        :rtype: bool
        '''
        return self._max_depth

    @property
    def max_depth_m1(self):
        '''Returns whether the read to the field is known to access all of the
        halo except the outermost level or not.

        :returns: True if the read to the field is known to access all \
                  of the halo except the outermost and False otherwise.
        :rtype: bool

        '''
        return self._max_depth_m1

    @property
    def var_depth(self):
        '''Returns the name of the variable specifying the depth of halo
        access if one is provided. Note, a variable will only be provided for
        stencil accesses. Also note, this depth should be added to the
        literal_depth to find the total depth.

        :returns: a variable name specifying the halo access depth \
                  if one exists, and None if not.
        :rtype: str

        '''
        return self._var_depth

    @property
    def literal_depth(self):
        '''Returns the known fixed (literal) depth of halo access. Note, this
        depth should be added to the var_depth to find the total depth.

        :returns: the known fixed (literal) halo access depth.
        :rtype: int

        '''
        return self._literal_depth

    @literal_depth.setter
    def literal_depth(self, value):
        ''' Set the known fixed (literal) depth of halo access.

        :param int value: Set the known fixed (literal) halo access depth.

        '''
        self._literal_depth = value

    def set_by_value(self, max_depth, var_depth, literal_depth, annexed_only,
                     max_depth_m1):
        # pylint: disable=too-many-arguments
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
            max_depth = self._symbol_table.lookup_with_tag(
                "max_halo_depth_mesh")
            depth_str += max_depth.name
        elif self.max_depth_m1:
            max_depth = self._symbol_table.lookup_with_tag(
                "max_halo_depth_mesh")
            depth_str += f"{max_depth.name}-1"
        else:
            if self.var_depth:
                depth_str += self.var_depth
                if self.literal_depth:
                    # Ignores depth == 0
                    depth_str += f"+{self.literal_depth}"
            elif self.literal_depth is not None:
                # Returns depth if depth has any value, including 0
                depth_str = str(self.literal_depth)
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
        # Get the kernel/built-in call associated with this field
        call = field.call
    except AttributeError as err:
        six.raise_from(GenerationError(
            "HaloInfo class expects an argument of type DynArgument, or "
            "equivalent, on initialisation, but found, "
            "'{0}'".format(type(field))), err)

    if field.access not in access_types:
        api_strings = [access.api_specific_name() for access in access_types]
        raise GenerationError(
            "In HaloInfo class, field '{0}' should be one of {1}, but found "
            "'{2}'".format(field.name, api_strings,
                           field.access.api_specific_name()))
    if not isinstance(call, (LFRicBuiltIn, DynKern)):
        raise GenerationError(
            "In HaloInfo class, field '{0}' should be from a call but "
            "found {1}".format(field.name, type(call)))
    return call


class HaloWriteAccess(HaloDepth):
    '''Determines how much of a field's halo is written to (the halo depth)
    when a field is accessed in a particular kernel within a
    particular loop nest.

    :param field: the field that we are concerned with.
    :type field: :py:class:`psyclone.dynamo0p3.DynArgument`
    :param sym_table: the symbol table associated with the scoping region \
                      that contains this halo access.
    :type sym_table: :py:class:`psyclone.psyir.symbols.SymbolTable`

    '''
    def __init__(self, field, sym_table):
        HaloDepth.__init__(self, sym_table)
        self._compute_from_field(field)

    @property
    def dirty_outer(self):
        '''Returns True if the writer is continuous and accesses the halo and
        False otherwise. It indicates that the outer level of halo that has
        been written to is actually dirty (well to be precise it is a partial
        sum).

        :returns: True if the outer layer of halo that is written \
                  to remains dirty and False otherwise.
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

        :param field: the field that we are concerned with.
        :type field: :py:class:`psyclone.dynamo0p3.DynArgument`

        '''
        const = LFRicConstants()

        call = halo_check_arg(field, AccessType.all_write_accesses())
        # no test required here as all calls exist within a loop

        loop = call.parent.parent
        # The outermost halo level that is written to is dirty if it
        # is a continuous field which writes into the halo in a loop
        # over cells
        self._dirty_outer = (
            not field.discontinuous and
            loop.iteration_space == "cell_column" and
            loop.upper_bound_name in const.HALO_ACCESS_LOOP_BOUNDS)
        depth = 0
        max_depth = False
        if loop.upper_bound_name in const.HALO_ACCESS_LOOP_BOUNDS:
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
    accessed in a particular kernel within a particular loop nest.

    :param field: the field for which we want information.
    :type field: :py:class:`psyclone.dynamo0p3.DynKernelArgument`
    :param sym_table: the symbol table associated with the scoping region \
                      that contains this halo access.
    :type sym_table: :py:class:`psyclone.psyir.symbols.SymbolTable`

    '''
    def __init__(self, field, sym_table):
        HaloDepth.__init__(self, sym_table)
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

        :returns: the type of stencil access used or None if there is no \
                  stencil.
        :rtype: str

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
        # pylint: disable=too-many-branches
        const = LFRicConstants()

        self._annexed_only = False
        call = halo_check_arg(field, AccessType.all_read_accesses())

        loop = call.ancestor(DynLoop)

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
        if loop.upper_bound_name in const.HALO_ACCESS_LOOP_BOUNDS:
            # this loop performs redundant computation
            if loop.upper_bound_halo_depth:
                # loop redundant computation is to a fixed literal depth
                self._literal_depth = loop.upper_bound_halo_depth
            else:
                # loop redundant computation is to the maximum depth
                self._max_depth = True
        elif loop.upper_bound_name == "ncolour":
            # Loop is coloured but does not access the halo.
            pass
        elif loop.upper_bound_name in ["ncells", "nannexed"]:
            if field.descriptor.stencil:
                # no need to worry about annexed dofs (if they exist)
                # as the stencil will cover these (this is currently
                # guaranteed as halo exchanges only exchange full
                # halos)
                pass
            else:  # there is no stencil
                if (field.discontinuous or call.iterates_over == "dof" or
                        call.all_updates_are_writes):
                    # There are only local accesses or the kernel is of the
                    # special form where any iteration is guaranteed to write
                    # the same value to a given shared entity.
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
            self._stencil_type = field.descriptor.stencil['type']
            if self._literal_depth:
                # halo exchange does not support mixed accesses to the halo
                self._stencil_type = "region"
            stencil_depth = field.descriptor.stencil['extent']
            if stencil_depth:
                # stencil_depth is provided in the kernel metadata
                self._literal_depth += stencil_depth
            else:
                # Stencil_depth is provided by the algorithm layer.
                # It is currently not possible to specify kind for an
                # integer literal stencil depth in a kernel call. This
                # will be enabled when addressing issue #753.
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
    '''
    The LFRic-specific Loop class. This passes the LFRic-specific
    loop information to the base class so it creates the one
    we require.  Creates LFRic-specific loop bounds when the code is
    being generated.

    :param parent: the parent of this Node in the PSyIR.
    :type parent: :py:class:`psyclone.psyir.nodes.Node`
    :param str loop_type: the type (iteration space) of this loop.

    :raises InternalError: if an unrecognised loop_type is specified.

    '''
    # pylint: disable=too-many-instance-attributes
    def __init__(self, parent=None, loop_type=""):
        const = LFRicConstants()
        super(DynLoop, self).__init__(parent=parent,
                                      valid_loop_types=const.VALID_LOOP_TYPES)
        self.loop_type = loop_type

        # Set our variable at initialisation as it might be required
        # by other classes before code generation. A 'null' loop does not
        # have an associated variable.
        if self.loop_type != "null":

            if self.loop_type == "colours":
                tag = "colours_loop_idx"
                suggested_name = "colour"
            elif self.loop_type == "colour":
                tag = "cell_loop_idx"
                suggested_name = "cell"
            elif self.loop_type == "dof":
                tag = "dof_loop_idx"
                suggested_name = "df"
            elif self.loop_type == "":
                tag = "cell_loop_idx"
                suggested_name = "cell"
            else:
                raise InternalError(
                    "Unsupported loop type '{0}' found when creating loop "
                    "variable. Supported values are 'colours', 'colour', "
                    "'dof' or '' (for cell-columns).".format(self.loop_type))

            symtab = self.scope.symbol_table
            try:
                self.variable = symtab.lookup_with_tag(tag)
            except KeyError:
                # TODO #696 - each loop variable should have KIND i_def.
                self.variable = symtab.new_symbol(
                    suggested_name, tag, symbol_type=DataSymbol,
                    datatype=INTEGER_TYPE)

        # Pre-initialise the Loop children  # TODO: See issue #440
        self.addchild(Literal("NOT_INITIALISED", INTEGER_TYPE,
                              parent=self))  # start
        self.addchild(Literal("NOT_INITIALISED", INTEGER_TYPE,
                              parent=self))  # stop
        self.addchild(Literal("1", INTEGER_TYPE, parent=self))  # step
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
        if self._loop_type == "null":
            return "{0}[type='null']".format(self.coloured_name(colour))

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

        :raises GenerationError: if the field updated by the kernel has an \
            unexpected function space or if the kernel's 'operates-on' is \
            not consistent with the loop type.

        '''
        self._kern = kern

        self._field = kern.arguments.iteration_space_arg()
        self._field_name = self._field.name
        self._field_space = self._field.function_space

        if self.loop_type == "null" and kern.iterates_over != "domain":
            raise GenerationError(
                "A DynLoop of type 'null' can only contain a kernel that "
                "operates on the 'domain' but kernel '{0}' operates on "
                "'{1}'.".format(kern.name, kern.iterates_over))
        self._iteration_space = kern.iterates_over  # cell_columns etc.

        # Loop bounds
        self.set_lower_bound("start")
        const = LFRicConstants()
        if isinstance(kern, LFRicBuiltIn):
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
                if self._field.is_operator:
                    # We always compute operators redundantly out to the L1
                    # halo
                    self.set_upper_bound("cell_halo", index=1)
                elif (self.field_space.orig_name in
                      const.VALID_DISCONTINUOUS_NAMES):
                    # Iterate to ncells for all discontinuous quantities,
                    # including any_discontinuous_space
                    self.set_upper_bound("ncells")
                elif (self.field_space.orig_name in
                      const.CONTINUOUS_FUNCTION_SPACES):
                    # Must iterate out to L1 halo for continuous quantities
                    # unless the only arguments that are updated all have
                    # 'GH_WRITE' access. The only time such an access is
                    # permitted for a field on a continuous space is when the
                    # kernel is implemented such that any writes to a given
                    # shared dof are guaranteed to write the same value. There
                    # is therefore no need to iterate into the L1 halo in order
                    # to get correct values for annexed dofs.
                    if not kern.all_updates_are_writes:
                        self.set_upper_bound("cell_halo", index=1)
                    else:
                        self.set_upper_bound("ncells")
                elif (self.field_space.orig_name in
                      const.VALID_ANY_SPACE_NAMES):
                    # We don't know whether any_space is continuous or not
                    # so we have to err on the side of caution and assume that
                    # it is. Again, if the only arguments that are updated have
                    # 'GH_WRITE' access then we can relax this condition.
                    if not kern.all_updates_are_writes:
                        self.set_upper_bound("cell_halo", index=1)
                    else:
                        self.set_upper_bound("ncells")
                else:
                    raise GenerationError(
                        "Unexpected function space found. Expecting one of "
                        "{0} but found '{1}'".format(
                            str(const.VALID_FUNCTION_SPACES),
                            self.field_space.orig_name))
            else:  # sequential
                self.set_upper_bound("ncells")

    def set_lower_bound(self, name, index=None):
        ''' Set the lower bounds of this loop '''
        const = LFRicConstants()
        if name not in const.VALID_LOOP_BOUNDS_NAMES:
            raise GenerationError(
                "The specified lower bound loop name is invalid")
        if name in ["inner"] + const.HALO_ACCESS_LOOP_BOUNDS and index < 1:
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
        const = LFRicConstants()
        if name not in const.VALID_LOOP_BOUNDS_NAMES:
            raise GenerationError(
                "The specified upper loop bound name is invalid. Expected one "
                "of {0} but found '{1}'".format(const.VALID_LOOP_BOUNDS_NAMES,
                                                name))
        if name == "start":
            raise GenerationError("'start' is not a valid upper bound")
        # Only halo bounds and inner may have an index. We could just
        # test for index here and assume that index is None for other
        # types of bounds, but checking the type of bound as well is a
        # safer option.
        if name in (["inner"] + const.HALO_ACCESS_LOOP_BOUNDS) and \
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
        bound name is not in HALO_ACCESS_LOOP_BOUNDS.

        :returns: the depth of the halo for a loops upper bound. If it \
            is None then a depth has not been provided. The depth value is \
            only valid when the upper-bound name is associated with a halo \
            e.g. 'cell_halo'.
        :rtype: int

        '''
        return self._upper_bound_halo_depth

    def _lower_bound_fortran(self):
        '''
        Create the associated Fortran code for the type of lower bound.

        TODO: Issue #440. lower_bound_fortran should generate PSyIR.

        :returns: the Fortran code for the lower bound.
        :rtype: str

        :raises GenerationError: if self._lower_bound_name is not "start" \
                                 for sequential code.
        :raises GenerationError: if self._lower_bound_name is unrecognised.

        '''
        if not Config.get().distributed_memory and \
           self._lower_bound_name != "start":
            raise GenerationError(
                "The lower bound must be 'start' if we are sequential but "
                "found '{0}'".format(self._upper_bound_name))
        if self._lower_bound_name == "start":
            return "1"

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
        # Use InvokeSchedule SymbolTable to share the same symbol for all
        # Loops in the Invoke.
        mesh_obj_name = self.ancestor(InvokeSchedule).symbol_table.\
            find_or_create_tag("mesh").name
        return mesh_obj_name + "%get_last_" + prev_space_name + "_cell(" \
            + prev_space_index_str + ")+1"

    @property
    def _mesh_name(self):
        '''
        :returns: the name of the mesh variable from which to get the bounds \
                  for this loop.
        :rtype: str
        '''
        # We must allow for self._kern being None (as it will be for
        # a built-in).
        if self._kern and self._kern.is_intergrid:
            # We have more than one mesh object to choose from and we
            # want the coarse one because that determines the iteration
            # space. _field_name holds the name of the argument that
            # determines the iteration space of this kernel and that
            # is set-up to be the one on the coarse mesh (in
            # DynKernelArguments.iteration_space_arg()).
            tag_name = "mesh_" + self._field_name
        else:
            # It's not an inter-grid kernel so there's only one mesh
            tag_name = "mesh"

        # The symbol for the mesh will already have been added to the
        # symbol table associated with the InvokeSchedule.
        return self.ancestor(InvokeSchedule).symbol_table.\
            lookup_with_tag(tag_name).name

    def _upper_bound_fortran(self):
        ''' Create the Fortran code that gives the appropriate upper bound
        value for this type of loop.

        TODO: Issue #440. upper_bound_fortran should generate PSyIR.

        :returns: Fortran code for the upper bound of this loop.
        :rtype: str

        '''
        # pylint: disable=too-many-branches, too-many-return-statements
        # precompute halo_index as a string as we use it in more than
        # one of the if clauses
        halo_index = ""
        if self._upper_bound_halo_depth:
            halo_index = str(self._upper_bound_halo_depth)

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
        if self._upper_bound_name == "ncolour":
            # Loop over cells of a particular colour when DM is disabled.
            # We use the same, DM API as that returns sensible values even
            # when running without MPI.
            root_name = "last_edge_cell_all_colours"
            if self._kern.is_intergrid:
                root_name += "_" + self._field_name
            sym = self.ancestor(
                InvokeSchedule).symbol_table.find_or_create_tag(root_name)
            return f"{sym.name}(colour)"
        if self._upper_bound_name == "colour_halo":
            # Loop over cells of a particular colour when DM is enabled. The
            # LFRic API used here allows for colouring with redundant
            # computation.
            if halo_index:
                # The colouring API provides a 2D array that holds the last
                # halo cell for a given colour and halo depth.
                depth = halo_index
            else:
                # If no depth is specified then we go to the full halo depth
                depth = self.ancestor(InvokeSchedule).symbol_table.\
                    find_or_create_tag(
                        f"max_halo_depth_{self._mesh_name}").name
            root_name = "last_halo_cell_all_colours"
            if self._kern.is_intergrid:
                root_name += "_" + self._field_name
            sym = self.ancestor(
                InvokeSchedule).symbol_table.find_or_create_tag(root_name)
            return f"{sym.name}(colour, {depth})"
        if self._upper_bound_name in ["ndofs", "nannexed"]:
            if Config.get().distributed_memory:
                if self._upper_bound_name == "ndofs":
                    result = (f"{self.field.proxy_name_indexed}%"
                              f"{self.field.ref_name()}%get_last_dof_owned()")
                else:  # nannexed
                    result = (
                        f"{self.field.proxy_name_indexed}%"
                        f"{self.field.ref_name()}%get_last_dof_annexed()")
            else:
                result = self._kern.undf_name
            return result
        if self._upper_bound_name == "ncells":
            if Config.get().distributed_memory:
                result = f"{self._mesh_name}%get_last_edge_cell()"
            else:
                result = (f"{self.field.proxy_name_indexed}%"
                          f"{self.field.ref_name()}%get_ncell()")
            return result
        if self._upper_bound_name == "cell_halo":
            if Config.get().distributed_memory:
                return f"{self._mesh_name}%get_last_halo_cell({halo_index})"
            raise GenerationError(
                "'cell_halo' is not a valid loop upper bound for "
                "sequential/shared-memory code")
        if self._upper_bound_name == "dof_halo":
            if Config.get().distributed_memory:
                return (f"{self.field.proxy_name_indexed}%"
                        f"{self.field.ref_name()}%get_last_dof_halo("
                        f"{halo_index})")
            raise GenerationError(
                "'dof_halo' is not a valid loop upper bound for "
                "sequential/shared-memory code")
        if self._upper_bound_name == "inner":
            if Config.get().distributed_memory:
                return f"{self._mesh_name}%get_last_inner_cell({halo_index})"
            raise GenerationError(
                "'inner' is not a valid loop upper bound for "
                "sequential/shared-memory code")
        raise GenerationError(
            "Unsupported upper bound name '{0}' found in dynloop.upper_"
            "bound_fortran()".format(self._upper_bound_name))

    def _halo_read_access(self, arg):
        '''
        Determines whether the supplied argument has (or might have) its
        halo data read within this loop. Returns True if it does, or if
        it might and False if it definitely does not.

        :param arg: an argument contained within this loop.
        :type arg: :py:class:`psyclone.dynamo0p3.DynArgument`

        :returns: True if the argument reads, or might read from the \
            halo and False otherwise.
        :rtype: bool

        :raises GenerationError: if an unsupported upper loop bound name is \
            provided for kernels with stencil access.
        :raises InternalError: if an unsupported field access is found.
        :raises InternalError: if an unsupported argument type is found.

        '''
        const = LFRicConstants()
        if arg.is_scalar or arg.is_operator:
            # Scalars and operators do not have halos
            return False
        if arg.is_field:
            # This is a field so might read from a halo
            if arg.access in [AccessType.WRITE]:
                # This is not a read access
                return False
            if arg.access in AccessType.all_read_accesses():
                # This is a read access
                if arg.descriptor.stencil:
                    if self._upper_bound_name not in ["cell_halo", "ncells"]:
                        raise GenerationError(
                            "Loop bounds other than 'cell_halo' and 'ncells' "
                            "are currently unsupported for kernels with "
                            "stencil accesses. Found '{0}'."
                            "".format(self._upper_bound_name))
                    # An upper bound of 'cell_halo' means that the
                    # halo might be accessed irrespective of the
                    # stencil and a stencil read access with upper
                    # bound 'ncells' might read from the
                    # halo due to the stencil.
                    return True
                # This is a non-stencil read access
                if self._upper_bound_name in const.HALO_ACCESS_LOOP_BOUNDS:
                    # An upper bound that is part of the halo means
                    # that the halo might be accessed.
                    return True
                if (not arg.discontinuous and
                        self.kernel.iterates_over == "cell_column" and
                        self.kernel.all_updates_are_writes and
                        self._upper_bound_name == "ncells"):
                    # This is the special case of a kernel that guarantees to
                    # write the same value to any given dof, irrespective of
                    # cell column.
                    return False
                if not arg.discontinuous and \
                   self._upper_bound_name in ["ncells", "nannexed"]:
                    # Annexed dofs may be accessed. Return False if we
                    # always compute annexed dofs and True if we don't
                    # (as annexed dofs are part of the level 1 halo).
                    return not Config.get().api_conf("dynamo0.3").\
                        compute_annexed_dofs
                # The halo is not accessed.
                return False
            raise InternalError(
                "Unexpected field access type '{0}' found for arg '{1}'."
                "".format(arg.access, arg.name))
        raise InternalError(
            "Expecting arg '{0}' to be an operator, scalar or field, "
            "but found '{1}'.".format(arg.name, arg.argument_type))

    def _add_field_component_halo_exchange(self, halo_field, idx=None):
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

        :raises InternalError: if there are two forward write \
            dependencies and they are both associated with halo \
            exchanges.

        '''
        exchange = DynHaloExchange(halo_field,
                                   parent=self.parent,
                                   vector_index=idx)
        self.parent.children.insert(self.position,
                                    exchange)

        # Is this halo exchange required? The halo exchange being
        # added may replace an existing halo exchange, which would
        # then be returned as a halo exchange dependence and an
        # exception raised (as a halo exchange should not have another
        # halo exchange as a dependence). Therefore, halo exchange
        # dependencies are ignored here by setting the ignore_hex_dep
        # optional argument.
        required, _ = exchange.required(ignore_hex_dep=True)
        if not required:
            exchange.detach()
        else:
            # The halo exchange we have added may be replacing an
            # existing one. If so, the one being replaced will be the
            # first and only write dependence encountered and must be
            # removed.
            results = exchange.field.forward_write_dependencies()
            if results:
                first_dep_call = results[0].call
                if isinstance(first_dep_call, HaloExchange):
                    # Sanity check. If the first dependence is a field
                    # accessed within a halo exchange then the
                    # subsequent one must not be.
                    next_results = results[0].forward_write_dependencies()
                    if next_results and any(tmp for tmp in next_results
                                            if isinstance(tmp.call,
                                                          HaloExchange)):
                        raise InternalError(
                            "When replacing a halo exchange with another one "
                            "for field {0}, a subsequent dependent halo "
                            "exchange was found. This should never happen."
                            "".format(exchange.field.name))
                    first_dep_call.detach()

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
                self._add_field_component_halo_exchange(halo_field, idx)
        else:
            self._add_field_component_halo_exchange(halo_field)

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
        # pylint: disable=too-many-nested-blocks
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
                                halo_exchange.detach()

    def create_halo_exchanges(self):
        '''Add halo exchanges before this loop as required by fields within
        this loop. To keep the logic simple we assume that any field
        that accesses the halo will require a halo exchange and then
        remove the halo exchange if this is not the case (when
        previous writers perform sufficient redundant computation). It
        is implemented this way as the halo exchange class determines
        whether it is required or not so a halo exchange needs to
        exist in order to find out. The appropriate logic is coded in
        the _add_halo_exchange helper method. In some cases a new halo
        exchange will replace an existing one. In this situation that
        routine also removes the old one.

        '''
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

    @property
    def start_expr(self):
        '''
        :returns: the PSyIR for the lower bound of this loop.
        :rtype: :py:class:`psyclone.psyir.Node`

        '''
        inv_sched = self.ancestor(InvokeSchedule)
        sym_table = inv_sched.symbol_table
        loops = inv_sched.loops()
        posn = None
        for index, loop in enumerate(loops):
            if loop is self:
                posn = index
                break
        lbound = sym_table.lookup_with_tag(f"loop{posn}_start")
        return Reference(lbound)

    @property
    def stop_expr(self):
        '''
        :returns: the PSyIR for the upper bound of this loop.
        :rtype: :py:class:`psyclone.psyir.Node`

        '''
        inv_sched = self.ancestor(InvokeSchedule)
        sym_table = inv_sched.symbol_table

        if self._loop_type == "colour":
            # If this loop is over all cells of a given colour then we must
            # lookup the loop bound as it depends on the current colour.
            parent_loop = self.ancestor(Loop)
            colour_var = parent_loop.variable

            asym = sym_table.lookup(self.kernel.last_cell_all_colours)
            const = LFRicConstants()

            if self.upper_bound_name in const.HALO_ACCESS_LOOP_BOUNDS:
                if self._upper_bound_halo_depth:
                    # TODO: #696 Add kind (precision) once the
                    # DynInvokeSchedule constructor has been extended to
                    # create the necessary symbols.
                    halo_depth = Literal(str(self._upper_bound_halo_depth),
                                         INTEGER_TYPE)
                else:
                    # We need to go to the full depth of the halo.
                    root_name = "mesh"
                    if self.kernels()[0].is_intergrid:
                        root_name += f"_{self._field_name}"
                    depth_sym = sym_table.lookup_with_tag(
                        f"max_halo_depth_{root_name}")
                    halo_depth = Reference(depth_sym)

                return ArrayReference.create(asym, [Reference(colour_var),
                                                    halo_depth])
            return ArrayReference.create(asym, [Reference(colour_var)])

        # This isn't a 'colour' loop so we have already set-up a
        # variable that holds the upper bound.
        loops = inv_sched.loops()
        posn = None
        for index, loop in enumerate(loops):
            if loop is self:
                posn = index
                break
        ubound = sym_table.lookup_with_tag(f"loop{posn}_stop")
        return Reference(ubound)

    def gen_code(self, parent):
        ''' Call the base class to generate the code and then add any
        required halo exchanges.

        :param parent: an f2pygen object that will be the parent of \
            f2pygen objects created in this method.
        :type parent: :py:class:`psyclone.f2pygen.BaseGen`

        :raises GenerationError: if a loop over colours is within an \
            OpenMP parallel region (as it must be serial).

        '''
        # pylint: disable=too-many-statements, too-many-branches
        # Check that we're not within an OpenMP parallel region if
        # we are a loop over colours.
        if self._loop_type == "colours" and self.is_openmp_parallel():
            raise GenerationError("Cannot have a loop over colours within an "
                                  "OpenMP parallel region.")

        if self._loop_type != "null":
            super(DynLoop, self).gen_code(parent)
        else:
            # This is a 'null' loop and therefore we do not actually generate
            # a loop - we go on down to the children instead.
            for child in self.loop_body.children:
                child.gen_code(parent)

        if not (Config.get().distributed_memory and
                self._loop_type != "colour"):
            # No need to add halo exchanges so we are done.
            return

        # Set halo clean/dirty for all fields that are modified
        fields = self.unique_modified_args("gh_field")

        if not fields:
            return

        parent.add(CommentGen(parent, ""))
        if self._loop_type != "null":
            prev_node_name = "loop"
        else:
            prev_node_name = "kernel"
        parent.add(CommentGen(parent, f" Set halos dirty/clean for fields "
                              f"modified in the above {prev_node_name}"))
        parent.add(CommentGen(parent, ""))
        use_omp_master = False
        if self.is_openmp_parallel():
            if not self.ancestor(OMPParallelDoDirective):
                use_omp_master = True
                # I am within an OpenMP Do directive so protect
                # set_dirty() and set_clean() with OpenMP Master
                parent.add(DirectiveGen(parent, "omp", "begin", "master", ""))

        sym_table = self.ancestor(InvokeSchedule).symbol_table

        # first set all of the halo dirty unless we are
        # subsequently going to set all of the halo clean
        for field in fields:
            # The HaloWriteAccess class provides information about how the
            # supplied field is accessed within its parent loop
            hwa = HaloWriteAccess(field, sym_table)
            if not hwa.max_depth or hwa.dirty_outer:
                # output set dirty as some of the halo will not be set to clean
                if field.vector_size > 1:
                    # the range function below returns values from 1 to the
                    # vector size which is what we require in our Fortran code
                    for index in range(1, field.vector_size+1):
                        parent.add(CallGen(parent, name=field.proxy_name +
                                           f"({index})%set_dirty()"))
                else:
                    parent.add(CallGen(parent, name=field.proxy_name +
                                       "%set_dirty()"))
        # now set appropriate parts of the halo clean where
        # redundant computation has been performed
        for field in fields:
            # The HaloWriteAccess class provides information about how the
            # supplied field is accessed within its parent loop
            hwa = HaloWriteAccess(field, sym_table)
            if hwa.literal_depth:
                # halo access(es) is/are to a fixed depth
                halo_depth = hwa.literal_depth
                if hwa.dirty_outer:
                    halo_depth -= 1
                if halo_depth > 0:
                    if field.vector_size > 1:
                        # The range function below returns values from 1 to the
                        # vector size, as required in our Fortran code.
                        for index in range(1, field.vector_size+1):
                            parent.add(CallGen(
                                parent, name=f"{field.proxy_name}({index})%"
                                f"set_clean({halo_depth})"))
                    else:
                        parent.add(CallGen(
                            parent, name=f"{field.proxy_name}%set_clean("
                            f"{halo_depth})"))
            elif hwa.max_depth:
                # halo accesses(s) is/are to the full halo
                # depth (-1 if continuous)
                halo_depth = sym_table.lookup_with_tag(
                    "max_halo_depth_mesh").name

                if hwa.dirty_outer:
                    # a continuous field iterating over cells leaves the
                    # outermost halo dirty
                    halo_depth += "-1"
                if field.vector_size > 1:
                    # the range function below returns values from 1 to the
                    # vector size which is what we require in our Fortran code
                    for index in range(1, field.vector_size+1):
                        call = CallGen(parent,
                                       name=f"{field.proxy_name}({index})%"
                                       f"set_clean({halo_depth})")
                        parent.add(call)
                else:
                    call = CallGen(parent, name=f"{field.proxy_name}%"
                                   f"set_clean({halo_depth})")
                    parent.add(call)

        if use_omp_master:
            # I am within an OpenMP Do directive so protect
            # set_dirty() and set_clean() with OpenMP Master
            parent.add(DirectiveGen(parent, "omp", "end", "master", ""))
        parent.add(CommentGen(parent, ""))


class DynKern(CodedKern):
    ''' Stores information about Dynamo Kernels as specified by the
    Kernel metadata and associated algorithm call. Uses this
    information to generate appropriate PSy layer code for the Kernel
    instance or to generate a Kernel stub.

    '''
    # pylint: disable=too-many-instance-attributes
    # An instance of this `namedtuple` is used to store information on each of
    # the quadrature rules required by a kernel.
    #
    # alg_name: The actual argument text specifying the QR object in the
    #           Alg. layer.
    # psy_name: The PSy-layer variable name for the QR object.
    # kernel_args: List of kernel arguments associated with this QR rule.
    QRRule = namedtuple("QRRule",
                        ["alg_name", "psy_name", "kernel_args"])

    def __init__(self):
        # The super-init is called from the _setup() method which in turn
        # is called from load().
        # pylint: disable=super-init-not-called
        if False:  # pylint: disable=using-constant-test
            self._arguments = DynKernelArguments(None, None)  # for pyreverse
        self._parent = None
        self._base_name = ""
        self._func_descriptors = None
        self._fs_descriptors = None
        # Whether this kernel requires quadrature
        self._qr_required = False
        # Whether this kernel requires basis functions
        self._basis_required = False
        # What shapes of evaluator/quadrature this kernel requires (if any)
        self._eval_shapes = []
        # The function spaces on which to *evaluate* basis/diff-basis
        # functions if an evaluator is required for this kernel. Is a dict with
        # (mangled) FS names as keys and associated kernel argument as value.
        self._eval_targets = OrderedDict()
        # Will hold a dict of QRRule namedtuple objects, one for each QR
        # rule required by a kernel, indexed by shape. Needs to be ordered
        # because we must preserve the ordering specified in the metadata.
        self._qr_rules = OrderedDict()
        self._cma_operation = None
        self._is_intergrid = False  # Whether this is an inter-grid kernel
        # The reference-element properties required by this kernel
        self._reference_element = None
        # The mesh properties required by this kernel
        self._mesh_properties = None
        # Initialise kinds (precisions) of all kernel arguments (start
        # with 'real' and 'integer' kinds)
        api_config = Config.get().api_conf("dynamo0.3")
        self._argument_kinds = {api_config.default_kind["real"],
                                api_config.default_kind["integer"]}

    def reference_accesses(self, var_accesses):
        '''Get all variable access information. All accesses are marked
        according to the kernel metadata

        :param var_accesses: VariablesAccessInfo instance that stores the \
            information about variable accesses.
        :type var_accesses: \
            :py:class:`psyclone.core.access_info.VariablesAccessInfo`
        '''

        # Use the KernelCallArgList class, which can also provide variable
        # access information:
        create_arg_list = KernCallArgList(self)
        create_arg_list.generate(var_accesses)

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

        :raises InternalError: for an invalid data type of a scalar argument.
        :raises GenerationError: if an invalid argument type is found \
                                 in the kernel.

        '''
        # pylint: disable=too-many-branches
        # Create a name for each argument
        args = []
        const = LFRicConstants()
        for idx, descriptor in enumerate(ktype.arg_descriptors):
            pre = None
            if descriptor.argument_type.lower() == "gh_operator":
                pre = "op_"
            elif descriptor.argument_type.lower() == "gh_columnwise_operator":
                pre = "cma_op_"
            elif descriptor.argument_type.lower() == "gh_field":
                pre = "field_"
            elif (descriptor.argument_type.lower() in
                  const.VALID_SCALAR_NAMES):
                if descriptor.data_type.lower() == "gh_real":
                    pre = "rscalar_"
                elif descriptor.data_type.lower() == "gh_integer":
                    pre = "iscalar_"
                elif descriptor.data_type.lower() == "gh_logical":
                    pre = "lscalar_"
                else:
                    raise InternalError(
                        "Expected one of {0} data types for a scalar "
                        "argument but found '{1}'.".
                        format(const.VALID_SCALAR_DATA_TYPES,
                               descriptor.data_type))
            else:
                raise GenerationError(
                    "DynKern.load_meta() expected one of {0} but found '{1}'".
                    format(const.VALID_ARG_TYPE_NAMES,
                           descriptor.argument_type))
            args.append(Arg("variable", pre+str(idx+1)))

            if descriptor.stencil:
                if not descriptor.stencil["extent"]:
                    # Stencil size (in cells) is passed in
                    args.append(Arg("variable",
                                    pre+str(idx+1)+"_stencil_size"))
                if descriptor.stencil["type"] == "xory1d":
                    # Direction is passed in
                    args.append(Arg("variable", pre+str(idx+1)+"_direction"))

        # Initialise basis/diff basis so we can test whether quadrature
        # or an evaluator is required
        self._setup_basis(ktype)
        if self._basis_required:
            for shape in self._eval_shapes:
                if shape in const.VALID_QUADRATURE_SHAPES:
                    # Add a quadrature argument for each required quadrature
                    # rule.
                    args.append(Arg("variable", "qr_"+shape))
        self._setup(ktype, "dummy_name", args, None, check=False)

    def _setup_basis(self, kmetadata):
        '''
        Initialisation of the basis/diff basis information. This may be
        needed before general setup so is computed in a separate method.

        :param kmetadata: The kernel meta-data object produced by the parser.
        :type kmetadata: :py:class:`psyclone.dynamo0p3.DynKernMetadata`
        '''
        for descriptor in kmetadata.func_descriptors:
            if len(descriptor.operator_names) > 0:
                self._basis_required = True
                self._eval_shapes = kmetadata.eval_shapes[:]
                break

    def _setup(self, ktype, module_name, args, parent, check=True):
        '''Internal setup of kernel information.

        :param ktype: object holding information on the parsed metadata for \
                      this kernel.
        :type ktype: :py:class:`psyclone.dynamo0p3.DynKernMetadata`
        :param str module_name: the name of the Fortran module that contains \
                                the source of this Kernel.
        :param args: list of Arg objects produced by the parser for the \
                     arguments of this kernel call.
        :type args: list of :py:class:`psyclone.parse.algorithm.Arg` objects
        :param parent: the parent of this kernel call in the generated \
                       AST (will be a loop object).
        :type parent: :py:class:`psyclone.dynamo0p3.DynLoop`
        :param bool check: whether to check for consistency between the \
            kernel metadata and the algorithm layer. Defaults to True.

        '''
        # pylint: disable=too-many-branches, too-many-locals
        CodedKern.__init__(self, DynKernelArguments,
                           KernelCall(module_name, ktype, args),
                           parent, check)
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

        const = LFRicConstants()
        # Check that all specified evaluator shapes are recognised
        invalid_shapes = set(self._eval_shapes) \
            - set(const.VALID_EVALUATOR_SHAPES)
        if invalid_shapes:
            raise InternalError(
                "Evaluator shape(s) {0} is/are not recognised. "
                "Must be one of {1}.".format(list(invalid_shapes),
                                             const.VALID_EVALUATOR_SHAPES))

        # If there are any quadrature rule(s), what are the names of the
        # corresponding algorithm arguments? Can't use set() here because
        # we need to preserve the ordering specified in the metadata.
        qr_shapes = [shape for shape in self._eval_shapes if
                     shape in const.VALID_QUADRATURE_SHAPES]

        # The quadrature-related arguments to a kernel always come last so
        # construct an enumerator with start value -<no. of qr rules>
        for idx, shape in enumerate(qr_shapes, -len(qr_shapes)):

            qr_arg = args[idx]

            # Use the InvokeSchedule symbol_table to create a unique symbol
            # name for the whole Invoke.
            if qr_arg.varname:
                tag = "AlgArgs_" + qr_arg.text
                qr_name = self.ancestor(InvokeSchedule).symbol_table.\
                    find_or_create_tag(tag, qr_arg.varname).name
            else:
                # If we don't have a name then we must be doing kernel-stub
                # generation so create a suitable name.
                # TODO #719 we don't yet have a symbol table to prevent
                # clashes.
                qr_name = "qr_"+shape.split("_")[-1]

            # Dynamo 0.3 api kernels require quadrature rule arguments to be
            # passed in if one or more basis functions are used by the kernel
            # and gh_shape == "gh_quadrature_***".
            # if self._eval_shape == "gh_quadrature_xyz":
            #     self._qr_args = ["np_xyz", "weights_xyz"]
            if shape == "gh_quadrature_xyoz":
                qr_args = ["np_xy", "np_z", "weights_xy", "weights_z"]
            # elif self._eval_shape == "gh_quadrature_xoyoz":
            #     qr_args = ["np_x", "np_y", "np_z",
            #                "weights_x", "weights_y", "weights_z"]
            elif shape == "gh_quadrature_face":
                qr_args = ["nfaces", "np_xyz", "weights_xyz"]
            elif shape == "gh_quadrature_edge":
                qr_args = ["nedges", "np_xyz", "weights_xyz"]
            else:
                raise InternalError("Unsupported quadrature shape ('{0}') "
                                    "found in DynKern._setup".format(shape))

            # Append the name of the qr argument to the names of the qr-related
            # variables.
            qr_args = [arg + "_" + qr_name for arg in qr_args]

            self._qr_rules[shape] = self.QRRule(qr_arg.text, qr_name, qr_args)

        if "gh_evaluator" in self._eval_shapes:
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

        # Properties of the reference element required by this kernel
        self._reference_element = ktype.reference_element

        # Properties of the mesh required by this kernel
        self._mesh_properties = ktype.mesh

    @property
    def qr_rules(self):
        '''
        :return: details of each of the quadrature rules required by this \
                 kernel.
        :rtype: OrderedDict containing \
                :py:class:`psyclone.dynamo0p3.DynKern.QRRule` indexed by \
                quadrature shape.
        '''
        return self._qr_rules

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

        :returns: name of the colourmap (Fortran array).
        :rtype: str

        :raises InternalError: if this kernel is not coloured or the \
                               dictionary of inter-grid kernels and \
                               colourmaps has not been constructed.

        '''
        if not self.is_coloured():
            raise InternalError(f"Kernel '{self.name}' is not inside a "
                                f"coloured loop.")
        if self._is_intergrid:
            invoke = self.ancestor(InvokeSchedule).invoke
            if id(self) not in invoke.meshes.intergrid_kernels:
                raise InternalError(
                    f"Colourmap information for kernel '{self.name}' has "
                    f"not yet been initialised")
            cmap = invoke.meshes.intergrid_kernels[id(self)].colourmap
        else:
            cmap = self.scope.symbol_table.lookup_with_tag("cmap").name
        return cmap

    @property
    def last_cell_all_colours(self):
        '''
        Getter for the name of the array holding the index of the last cell of
        each colour.

        :returns: name of the array.
        :rtype: str

        :raises InternalError: if this kernel is not coloured or the \
                               dictionary of inter-grid kernels and \
                               colourmaps has not been constructed.
        '''
        if not self.is_coloured():
            raise InternalError("Kernel '{0}' is not inside a coloured "
                                "loop.".format(self.name))
        if self._is_intergrid:
            invoke = self.ancestor(InvokeSchedule).invoke
            if id(self) not in invoke.meshes.intergrid_kernels:
                raise InternalError(
                    "Colourmap information for kernel '{0}' has not yet "
                    "been initialised".format(self.name))
            return invoke.meshes.intergrid_kernels[id(self)].last_cell_var

        const = LFRicConstants()

        if (self.ancestor(Loop).upper_bound_name in
                const.HALO_ACCESS_LOOP_BOUNDS):
            return self.scope.symbol_table.lookup_with_tag(
                "last_halo_cell_all_colours").name
        else:
            return self.scope.symbol_table.lookup_with_tag(
                "last_edge_cell_all_colours").name

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
            invoke = self.ancestor(InvokeSchedule).invoke
            if id(self) not in invoke.meshes.intergrid_kernels:
                raise InternalError(
                    "Colourmap information for kernel '{0}' has not yet "
                    "been initialised".format(self.name))
            ncols = invoke.meshes.intergrid_kernels[id(self)].ncolours_var
        else:
            ncols = self.scope.symbol_table.lookup_with_tag("ncolour").name
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
        return self._basis_required and self.qr_rules

    @property
    def eval_shapes(self):
        '''
        :return: the value(s) of GH_SHAPE for this kernel or an empty list \
                 if none are specified.
        :rtype: list

        '''
        return self._eval_shapes

    @property
    def eval_targets(self):
        '''
        :return: the function spaces upon which basis/diff-basis functions \
                 are to be evaluated for this kernel.
        :rtype: dict of (:py:class:`psyclone.domain.lfric.FunctionSpace`, \
                :py:class`psyclone.dynamo0p3.DynKernelArgument`), indexed by \
                the names of the target function spaces.
        '''
        return self._eval_targets

    @property
    def reference_element(self):
        '''
        :returns: the reference-element properties required by this kernel.
        :rtype: :py:class:`psyclone.dynamo0p3.RefElementMetaData`
        '''
        return self._reference_element

    @property
    def mesh(self):
        '''
        :returns: the mesh properties required by this kernel.
        :rtype: :py:class`psyclone.dynamo0p3.MeshPropertiesMetaData`
        '''
        return self._mesh_properties

    @property
    def all_updates_are_writes(self):
        '''
        :returns: True if all of the arguments updated by this kernel have \
                  'GH_WRITE' access, False otherwise.
        :rtype: bool

        '''
        if self.is_intergrid:
            # This is not a special kernel
            return False
        accesses = set(arg.access for arg in self.args)
        all_writes = AccessType.all_write_accesses()
        all_writes.remove(AccessType.WRITE)
        return (not accesses.intersection(set(all_writes)))

    def local_vars(self):
        ''' Returns the names used by the Kernel that vary from one
        invocation to the next and therefore require privatisation
        when parallelised. '''
        return []

    @property
    def base_name(self):
        '''
        :returns: a base name for this kernel.
        :rtype: str
        '''
        return self._base_name

    @property
    def argument_kinds(self):
        '''
        :returns: kinds (precisions) for all arguments in a kernel.
        :rtype: set of str

        '''
        return self._argument_kinds

    @property
    def gen_stub(self):
        '''
        Create the fparser1 AST for a kernel stub.

        :returns: root of fparser1 AST for the stub routine.
        :rtype: :py:class:`fparser.one.block_statements.Module`

        :raises GenerationError: if the supplied kernel stub does not operate \
            on a supported subset of the domain (currently only "cell_column").

        '''
        # The operates-on/iterates-over values supported by the stub generator.
        const = LFRicConstants()
        supported_operates_on = const.USER_KERNEL_ITERATION_SPACES[:]
        # TODO #925 Add support for 'domain' kernels
        supported_operates_on.remove("domain")

        # Check operates-on (iteration space) before generating code
        if self.iterates_over not in supported_operates_on:
            raise GenerationError(
                "The LFRic API kernel-stub generator supports kernels that "
                "operate on one of {0} but found '{1}' "
                "in kernel '{2}'.".format(supported_operates_on,
                                          self.iterates_over, self.name))

        # Create an empty PSy layer module
        psy_module = ModuleGen(self._base_name+"_mod")

        # Create the subroutine
        sub_stub = SubroutineGen(psy_module, name=self._base_name+"_code",
                                 implicitnone=True)

        # Add all the declarations
        for entities in [DynCellIterators, DynDofmaps, DynFunctionSpaces,
                         DynCMAOperators, LFRicScalarArgs, LFRicFields,
                         DynLMAOperators, DynStencils, DynBasisFunctions,
                         DynBoundaryConditions, DynReferenceElement,
                         LFRicMeshProperties]:
            entities(self).declarations(sub_stub)

        # Add wildcard "use" statement for all supported argument
        # kinds (precisions)
        sub_stub.add(
            UseGen(sub_stub,
                   name=const.UTILITIES_MOD_MAP["constants"]["module"]))

        # Create the arglist
        create_arg_list = KernStubArgList(self)
        create_arg_list.generate()

        # Add the arglist
        sub_stub.args = create_arg_list.arglist

        # Add the subroutine to the parent module
        psy_module.add(sub_stub)
        return psy_module.root

    def gen_code(self, parent):
        '''
        Generates LFRic (Dynamo 0.3) specific PSy layer code for a call
        to this user-supplied LFRic kernel.

        :param parent: an f2pygen object that will be the parent of \
                       f2pygen objects created in this method.
        :type parent: :py:class:`psyclone.f2pygen.BaseGen`

        :raises GenerationError: if this kernel does not have a supported \
                        operates-on (currently only "cell_column").
        :raises GenerationError: if the loop goes beyond the level 1 \
                        halo and an operator is accessed.
        :raises GenerationError: if a kernel in the loop has an inc access \
                        and the loop is not coloured but is within an OpenMP \
                        parallel region.

        '''
        # Check operates-on (iteration space) before generating code
        const = LFRicConstants()
        if self.iterates_over not in const.USER_KERNEL_ITERATION_SPACES:
            raise GenerationError(
                "The LFRic API supports calls to user-supplied kernels "
                "that operate on one of {0}, but kernel '{1}' "
                "operates on '{2}'.".
                format(const.USER_KERNEL_ITERATION_SPACES, self.name,
                       self.iterates_over))

        # Get configuration for valid argument kinds
        api_config = Config.get().api_conf("dynamo0.3")

        parent.add(DeclGen(parent, datatype="integer",
                           kind=api_config.default_kind["integer"],
                           entity_decls=["cell"]))

        parent_loop = self.ancestor(DynLoop)

        # Check whether this kernel reads from an operator
        op_args = parent_loop.args_filter(
            arg_types=const.VALID_OPERATOR_NAMES,
            arg_accesses=[AccessType.READ, AccessType.READWRITE])
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

        if not self.is_coloured():
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

        parent.add(CommentGen(parent, ""))

        super(DynKern, self).gen_code(parent)

    def get_kernel_schedule(self):
        '''Returns a PSyIR Schedule representing the kernel code. The base
        class creates the PSyIR schedule on first invocation which is
        then checked for consistency with the kernel metadata
        here. The Schedule is just generated on first invocation, this
        allows us to retain transformations that may subsequently be
        applied to the Schedule.

        Once issue #935 is implemented, this routine will return the
        PSyIR Schedule using LFRic-specific PSyIR where possible.

        :returns: Schedule representing the kernel code.
        :rtype: :py:class:`psyclone.psyGen.KernelSchedule`

        '''
        if self._kern_schedule is None:
            # Get the PSyIR Kernel Schedule
            psyir_schedule = super(DynKern, self).get_kernel_schedule()

            # Before transforming, check the validity of the kernel
            # arguments
            self.validate_kernel_code_args()

            # TODO replace the PSyIR argument data symbols with LFRic
            # data symbols, see issue #935. For the moment we simply
            # return the unmodified PSyIR schedule
            self._kern_schedule = psyir_schedule

        return self._kern_schedule

    def validate_kernel_code_args(self):
        '''Check that the arguments in the kernel code match the expected
        arguments as defined by the kernel metadata and the LFRic
        API.

        '''
        # Get the kernel code arguments
        psyir_schedule = super(DynKern, self).get_kernel_schedule()
        symbol_table = psyir_schedule.symbol_table
        kern_code_args = symbol_table.argument_list

        # Get the kernel code interface according to the kernel
        # metadata and LFRic API
        interface_info = KernelInterface(self)
        interface_info.generate()
        interface_args = interface_info.arglist

        # 1: Check the the number of arguments match
        actual_n_args = len(kern_code_args)
        expected_n_args = len(interface_args)
        if actual_n_args != expected_n_args:
            raise GenerationError(
                "In kernel '{0}' the number of arguments indicated by the "
                "kernel metadata is {1} but the actual number of kernel "
                "arguments found is {2}.".format(
                    self.name, expected_n_args, actual_n_args))

        # 2: Check that the properties of each argument matches
        for idx, kern_code_arg in enumerate(kern_code_args):
            interface_arg = interface_args[idx]
            self._validate_kernel_code_arg(kern_code_arg, interface_arg)

    def _validate_kernel_code_arg(self, kern_code_arg, interface_arg):
        '''Internal method to check that the supplied argument descriptions
        match and raise appropriate exceptions if not.

        :param kern_code_arg: kernel code argument.
        :type kern_code_arg: :py:class:`psyclone.psyir.symbols.DataSymbol`
        :param interface_arg: expected argument.
        :type interface_arg: :py:class:`psyclone.psyir.symbols.DataSymbol`

        :raises GenerationError: if the contents of the arguments do \
            not match.
        :raises InternalError: if an unexpected datatype is found.

        '''
        # 1: intrinsic datatype
        actual_datatype = kern_code_arg.datatype.intrinsic
        expected_datatype = interface_arg.datatype.intrinsic
        if actual_datatype != expected_datatype:
            raise GenerationError(
                "Kernel argument '{0}' has datatype '{1}' "
                "in kernel '{2}' but the LFRic API expects '{3}'."
                "".format(kern_code_arg.name, actual_datatype,
                          self.name, expected_datatype))
        # 2: precision
        actual_precision = kern_code_arg.datatype.precision
        expected_precision = interface_arg.datatype.precision
        if actual_precision.name != expected_precision.name:
            raise GenerationError(
                "Kernel argument '{0}' has precision '{1}' "
                "in kernel '{2}' but the LFRic API expects '{3}'."
                "".format(kern_code_arg.name, actual_precision.name,
                          self.name, expected_precision.name))
        # 3: intent
        actual_intent = kern_code_arg.interface.access
        expected_intent = interface_arg.interface.access
        if actual_intent.name != expected_intent.name:
            raise GenerationError(
                "Kernel argument '{0}' has intent '{1}' "
                "in kernel '{2}' but the LFRic API expects intent '{3}'."
                "".format(kern_code_arg.name, actual_intent.name,
                          self.name, expected_intent.name))
        # 4: scalar or array
        if interface_arg.is_scalar:
            if not kern_code_arg.is_scalar:
                raise GenerationError(
                    "Argument '{0}' to kernel '{1}' should be a scalar "
                    "according to the LFRic API, but it is not."
                    "".format(kern_code_arg.name, self.name))
        elif interface_arg.is_array:
            if not kern_code_arg.is_array:
                raise GenerationError(
                    "Argument '{0}' to kernel '{1}' should be an array "
                    "according to the LFRic API, but it is not."
                    "".format(kern_code_arg.name, self.name))
            # 4.1: array dimensions
            if len(interface_arg.shape) != len(kern_code_arg.shape):
                raise GenerationError(
                    "Argument '{0}' to kernel '{1}' should be an array "
                    "with {2} dimension(s) according to the LFRic API, but "
                    "found {3}.".format(kern_code_arg.name, self.name,
                                        len(interface_arg.shape),
                                        len(kern_code_arg.shape)))
            for dim_idx, kern_code_arg_dim in enumerate(kern_code_arg.shape):
                if not isinstance(kern_code_arg_dim, ArrayType.ArrayBounds):
                    continue
                if (not isinstance(kern_code_arg_dim.lower, Literal) or
                        kern_code_arg_dim.lower.value != "1"):
                    raise GenerationError(
                        "All array arguments to LFRic kernels must have lower "
                        "bounds of 1 for all dimensions. However, array '{0}' "
                        "has a lower bound of '{1}' for dimension {2}".format(
                            kern_code_arg.name, str(kern_code_arg_dim.lower),
                            dim_idx))
                kern_code_arg_upper_dim = kern_code_arg_dim.upper
                interface_arg_upper_dim = interface_arg.shape[dim_idx].upper
                if (isinstance(kern_code_arg_upper_dim, Reference) and
                        isinstance(interface_arg_upper_dim, Reference) and
                        isinstance(kern_code_arg_upper_dim.symbol,
                                   DataSymbol) and
                        isinstance(interface_arg_upper_dim.symbol,
                                   DataSymbol)):
                    # Only check when there is a symbol. Unspecified
                    # dimensions, dimensions with scalar values,
                    # offsets, or dimensions that include arithmetic
                    # are skipped.
                    try:
                        self._validate_kernel_code_arg(
                            kern_code_arg_upper_dim.symbol,
                            interface_arg_upper_dim.symbol)
                    except GenerationError as info:
                        six.raise_from(GenerationError(
                            "For dimension {0} in array argument '{1}' to "
                            "kernel '{2}' the following error was found: "
                            "{3}".format(dim_idx+1, kern_code_arg.name,
                                         self.name, str(info.args[0]))), info)
        else:
            raise InternalError(
                "unexpected argument type found for '{0}' in kernel '{1}'. "
                "Expecting a scalar or an array.".format(
                    kern_code_arg.name, self.name))


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
    def fs_name(self):
        ''' Returns the raw metadata value of this function space. '''
        return self._descriptor.function_space_name


class FSDescriptors(object):
    ''' Contains a collection of FSDescriptor objects and methods
    that provide information across these objects. We have one
    FSDescriptor for each meta-funcs entry in the kernel
    meta-data.
    # TODO #274 this should actually be named something like
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

    const = LFRicConstants()
    # Quadrature arguments - will have as many as there are distinct
    # quadrature shapes specified in the metadata.
    qr_arg_count = len(set(call.ktype.eval_shapes).intersection(
        set(const.VALID_QUADRATURE_SHAPES)))

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
    :param bool check: whether to check for consistency between the \
        kernel metadata and the algorithm layer. Defaults to True.

    :raises GenerationError: if the kernel meta-data specifies stencil extent.
    '''
    def __init__(self, call, parent_call, check=True):
        # pylint: disable=too-many-branches
        if False:  # pylint: disable=using-constant-test
            # For pyreverse
            self._0_to_n = DynKernelArgument(None, None, None, None)

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
                                             parent_call, check)
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
        if self._parent_call and hasattr(self._parent_call.root,
                                         'symbol_table'):
            symtab = self._parent_call.root.symbol_table
        else:
            # TODO 719 The symtab is not connected to other parts of the
            # Stub generation.
            symtab = SymbolTable()
        const = LFRicConstants()
        for arg in self._args:
            if not arg.descriptor.stencil:
                continue
            if not arg.stencil.extent_arg.is_literal():
                if arg.stencil.extent_arg.varname:
                    # Ensure extent argument name is registered in the
                    # symbol_table.
                    tag = "AlgArgs_" + arg.stencil.extent_arg.text
                    root = arg.stencil.extent_arg.varname
                    new_name = symtab.find_or_create_tag(tag, root).name
                    arg.stencil.extent_arg.varname = new_name
            if arg.descriptor.stencil['type'] == 'xory1d':
                # a direction argument has been added
                if arg.stencil.direction_arg.varname and \
                   arg.stencil.direction_arg.varname not in \
                   const.VALID_STENCIL_DIRECTIONS:
                    # Register the name of the direction argument to ensure
                    # it is unique in the PSy layer
                    tag = "AlgArgs_" + arg.stencil.direction_arg.text
                    root = arg.stencil.direction_arg.varname
                    new_name = symtab.find_or_create_tag(tag, root).name
                    arg.stencil.direction_arg.varname = new_name

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
                 :py:class:`psyclone.domain.lfric.FunctionSpace`)
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
        :type func_space: :py:class:`psyclone.domain.lfric.FunctionSpace`
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
        const = LFRicConstants()
        if op_type and op_type not in const.VALID_OPERATOR_NAMES:
            raise GenerationError(
                "If supplied, 'op_type' must be a valid operator type (one "
                "of {0}) but got '{1}'".
                format(const.VALID_OPERATOR_NAMES, op_type))
        if not op_type:
            # If no operator type is specified then we match any type
            op_list = const.VALID_OPERATOR_NAMES
        else:
            op_list = [op_type]
        for arg in self._args:
            if arg.argument_type in op_list:
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
        const = LFRicConstants()
        op_args = psyGen.args_filter(
            self._args,
            arg_types=const.VALID_OPERATOR_NAMES,
            arg_accesses=write_accesses)
        if op_args:
            return op_args[0]

        # Is this an inter-grid kernel? If so, then the iteration space
        # is determined by the coarse mesh, irrespective of whether
        # we are prolonging (and thus writing to a field on the fine mesh)
        # or restricting.
        if self._parent_call.is_intergrid:
            fld_args = psyGen.args_filter(
                self._args,
                arg_types=const.VALID_FIELD_NAMES,
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
        const = LFRicConstants()
        write_accesses = AccessType.all_write_accesses()
        fld_args = psyGen.args_filter(
            self._args,
            arg_types=const.VALID_FIELD_NAMES,
            arg_accesses=write_accesses)
        if fld_args:
            for spaces in [const.CONTINUOUS_FUNCTION_SPACES,
                           const.VALID_ANY_SPACE_NAMES,
                           const.VALID_DISCONTINUOUS_NAMES]:
                for arg in fld_args:
                    if arg.function_space.orig_name in spaces:
                        return arg

        # No modified fields or operators. Check for unmodified fields...
        fld_args = psyGen.args_filter(
            self._args,
            arg_types=const.VALID_FIELD_NAMES)
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
    '''
    This class provides information about individual LFRic kernel call
    arguments as specified by the kernel argument metadata and the
    kernel invocation in the Algorithm layer.

    :param kernel_args: object encapsulating all arguments to the \
                        kernel call.
    :type kernel_args: :py:class:`psyclone.dynamo0p3.DynKernelArguments`
    :param arg_meta_data: information obtained from the meta-data for \
                          this kernel argument.
    :type arg_meta_data: :py:class:`psyclone.domain.lfric.LFRicArgDescriptor`
    :param arg_info: information on how this argument is specified in \
                     the Algorithm layer.
    :type arg_info: :py:class:`psyclone.parse.algorithm.Arg`
    :param call: the kernel object with which this argument is associated.
    :type call: :py:class:`psyclone.dynamo0p3.DynKern`
    :param bool check: whether to check for consistency between the \
        kernel metadata and the algorithm layer. Defaults to True.

    :raises InternalError: for an unsupported metadata in the argument \
                           descriptor data type.

    '''
    # pylint: disable=too-many-public-methods, too-many-instance-attributes
    def __init__(self, kernel_args, arg_meta_data, arg_info, call, check=True):
        # Keep a reference to DynKernelArguments object that contains
        # this argument. This permits us to manage name-mangling for
        # any-space function spaces.
        self._kernel_args = kernel_args
        self._vector_size = arg_meta_data.vector_size
        self._argument_type = arg_meta_data.argument_type
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

        # Set the argument's intrinsic type from its descriptor's
        # data type and check if an invalid data type is passed from
        # the argument descriptor.
        try:
            const = LFRicConstants()
            self._intrinsic_type = const.MAPPING_DATA_TYPES[
                arg_meta_data.data_type]
        except KeyError as err:
            six.raise_from(InternalError(
                "DynKernelArgument.__init__(): Found unsupported data "
                "type '{0}' in the kernel argument descriptor '{1}'.".
                format(arg_meta_data.data_type, arg_meta_data)), err)

        # Addressing issue #753 will allow us to perform static checks
        # for consistency between the algorithm and the kernel
        # metadata. This will include checking that a field on a read
        # only function space is not passed to a kernel that modifies
        # it. Note, issue #79 is also related to this.
        KernelArgument.__init__(self, arg_meta_data, arg_info, call)
        # Argument proxy data type (if/as defined in LFRic infrastructure)
        self._proxy_data_type = None
        # Set up kernel argument information for scalar, field and operator
        # arguments: precision, module name, data type and proxy data type
        self._init_data_type_properties(arg_info, check)
        # Complete the initialisation of the argument (after
        # _init_data_type_properties() so the precision info etc is
        # already set up)
        self._complete_init(arg_info)

    def ref_name(self, function_space=None):
        '''
        Returns the name used to dereference this type of argument (depends
        on whether it is a field or operator and, if the latter, whether it
        is the to- or from-space that is specified).

        :param function_space: the function space of this argument
        :type function_space: :py:class:`psyclone.domain.lfric.FunctionSpace`

        :returns: the name used to dereference this argument.
        :rtype: str

        :raises GenerationError: if the supplied function space is not one \
                                 of the function spaces associated with \
                                 this argument.
        :raises GenerationError: if the supplied function space is not being \
                                 returned by either 'function_space_from' or \
                                 'function_space_to'.
        :raises GenerationError: if the argument type is not supported.

        '''
        # pylint: disable=too-many-branches
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
                    "DynKernelArgument.ref_name(fs): The supplied function "
                    "space (fs='{0}') is not one of the function spaces "
                    "associated with this argument (fss={1}).".format(
                        function_space.orig_name,
                        self.function_space_names))
        if self.is_field:
            return "vspace"
        if self.is_operator:
            if function_space.orig_name == self.descriptor.function_space_from:
                return "fs_from"
            if function_space.orig_name == self.descriptor.function_space_to:
                return "fs_to"
            raise GenerationError(
                "DynKernelArgument.ref_name(fs): Function space '{0}' "
                "is one of the 'gh_operator' function spaces '{1}' but "
                "is not being returned by either function_space_from "
                "'{2}' or function_space_to '{3}'.".format(
                    function_space.orig_name, self.function_spaces,
                    self.descriptor.function_space_from,
                    self.descriptor.function_space_to))
        raise GenerationError(
            "DynKernelArgument.ref_name(fs): Found unsupported argument "
            "type '{0}'.".format(self._argument_type))

    def _init_data_type_properties(self, arg_info, check=True):
        '''Set up kernel argument information from LFRicConstants: precision,
        data type, proxy data type and module name. This is currently
        supported for scalar, field and operator arguments.

        :param arg_info: information on how this argument is specified \
            in the Algorithm layer.
        :type arg_info: :py:class:`psyclone.parse.algorithm.Arg`
        :param bool check: whether to use the algorithm \
            information. Optional argument that defaults to True.

        '''
        alg_datatype_info = None
        if arg_info:
            alg_datatype_info = arg_info._datatype
        alg_datatype = None
        alg_precision = None
        if alg_datatype_info:
            alg_datatype, alg_precision = alg_datatype_info

        const = LFRicConstants()
        if arg_info and arg_info.form == "collection":
            try:
                alg_datatype = const.FIELD_VECTOR_TO_FIELD_MAP[alg_datatype]
            except KeyError:
                # The collection datatype is not recognised or supported.
                alg_datatype = None

        if self.is_scalar:
            self._init_scalar_properties(alg_datatype, alg_precision,
                                         check)
        elif self.is_field:
            self._init_field_properties(alg_datatype, check)
        elif self.is_operator:
            self._init_operator_properties(alg_datatype, check)
        else:
            raise InternalError(
                f"Supported argument types are scalar, field and operator, "
                f"but the argument '{self.name}' in kernel "
                f"'{self._call.name}' is none of these.")

    def _init_scalar_properties(
            self, alg_datatype, alg_precision, check=True):
        '''Set up the properties of this scalar using algorithm datatype
        information if it is available.

        :param alg_datatype: the datatype of this argument as \
            specified in the algorithm layer or None if it is not \
            known.
        :type alg_datatype: str or NoneType
        :param alg_precision: the precision of this argument as \
            specified in the algorithm layer or None if it is not \
            known.
        :type alg_precision: str or NoneType
        :param bool check: whether to use the algorithm \
            information. Optional argument that defaults to True.

        :raises InternalError: if the intrinsic type of the scalar is \
            not supported.
        :raises GenerationError: if the datatype specified in the \
            algorithm layer is inconsistent with the kernel metadata.
        :raises GenerationError: if the datatype for a gh_scalar \
            could not be found in the algorithm layer.
        :raises NotImplementedError: if the scalar is a reduction and \
            its intrinsic type is not real.
        :raises GenerationError: if the scalar is a reduction and is \
            not declared with default precision.

        '''
        const = LFRicConstants()
        # Check the type of scalar defined in the metadata is supported.
        if self.intrinsic_type not in const.VALID_INTRINSIC_TYPES:
            raise InternalError(
                f"Expected one of {const.VALID_INTRINSIC_TYPES} intrinsic "
                f"types for a scalar argument but found "
                f"'{self.intrinsic_type}' in the metadata of kernel "
                f"{self._call.name} for argument {self.name}.")

        # Check the metadata and algorithm types are consistent if
        # the algorithm information is available and is not being ignored.
        if check and alg_datatype and \
           alg_datatype != self.intrinsic_type:
            raise GenerationError(
                f"The kernel metadata for argument '{self.name}' in "
                f"kernel '{self._call.name}' specifies this argument "
                f"should be a scalar of type '{self.intrinsic_type}' but "
                f"in the algorithm layer it is defined as a "
                f"'{alg_datatype}'.")

        # If the algorithm information is not being ignored and
        # the datatype is known in the algorithm layer and it is
        # not a literal then its precision should also be defined.
        if check and alg_datatype and not alg_precision and \
           not self.is_literal:
            raise GenerationError(
                f"LFRic coding standards require scalars to have "
                f"their precision defined in the algorithm layer but "
                f"'{self.name}' in '{self._call.name}' does not.")

        if self.access in AccessType.get_valid_reduction_modes():
            # Treat reductions separately to other scalars as it
            # is expected that they should match the precision of
            # the field they are reducing. At the moment there is
            # an assumption that the precision will always be a
            # particular value (the default), see issue #1570.

            # Only real reductions are supported.
            if not self.intrinsic_type == "real":
                raise NotImplementedError(
                    "Reductions for datatypes other than real are not yet "
                    "supported in PSyclone.")

            expected_precision = const.DATA_TYPE_MAP["reduction"]["kind"]
            # If the algorithm information is not being ignored
            # then check that the expected precision and the
            # precision defined in the algorithn layer are
            # the same.
            if check and alg_precision and \
               alg_precision != expected_precision:
                raise GenerationError(
                    f"This scalar is a reduction which assumes precision "
                    f"of type '{expected_precision}' but the algorithm "
                    f"declares this scalar with precision "
                    f"'{alg_precision}'.")

            # Use the default 'real' scalar reduction properties.
            self._precision = expected_precision
            self._data_type = const.DATA_TYPE_MAP["reduction"]["type"]
            self._proxy_data_type = const.DATA_TYPE_MAP[
                "reduction"]["proxy_type"]
            self._module_name = const.DATA_TYPE_MAP["reduction"]["module"]
        else:
            # This is a scalar that is not part of a reduction.

            if check and alg_precision:
                # Use the algorithm precision if it is available
                # and not being ignored.
                self._precision = alg_precision
            else:
                # Use default precision for this datatype if the
                # algorithm precision is either not avaiable or is
                # being ignored.
                self._precision = const.SCALAR_PRECISION_MAP[
                    self.intrinsic_type]

    def _init_field_properties(self, alg_datatype, check=True):
        '''Set up the properties of this field using algorithm datatype
        information if it is available.

        :param alg_datatype: the datatype of this argument as \
            specified in the algorithm layer or None if it is not \
            known.
        :type alg_datatype: str or NoneType
        :param bool check: whether to use the algorithm \
            information. Optional argument that defaults to True.

        :raises GenerationError: if the datatype for a gh_field \
            could not be found in the algorithm layer.
        :raises GenerationError: if the datatype specified in the \
            algorithm layer is inconsistent with the kernel metadata.
        :raises InternalError: if the intrinsic type of the field is \
            not supported (i.e. is not real or integer).

        '''
        const = LFRicConstants()
        argtype = None
        # If the algorithm information is not being ignored then
        # it must be available.
        if check and not alg_datatype:
            raise GenerationError(
                f"It was not possible to determine the field type from "
                f"the algorithm layer for argument '{self.name}' in "
                f"kernel '{self._call.name}'.")

        # If the algorithm information is not being ignored then
        # check the metadata and algorithm type are consistent and
        # that the metadata specifies a supported intrinsic type.
        if self.intrinsic_type == "real":
            if not check:
                # Use the default as we are ignoring any algorithm info
                argtype = "field"
            elif alg_datatype == "field_type":
                argtype = "field"
            elif alg_datatype == "r_solver_field_type":
                argtype = "r_solver_field"
            elif alg_datatype == "r_tran_field_type":
                argtype = "r_tran_field"
            else:
                raise GenerationError(
                    f"The metadata for argument '{self.name}' in kernel "
                    f"'{self._call.name}' specifies that this is a real "
                    f"field, however it is declared as a "
                    f"'{alg_datatype}' in the algorithm code.")

        elif self.intrinsic_type == "integer":
            if check and alg_datatype != "integer_field_type":
                raise GenerationError(
                    f"The metadata for argument '{self.name}' in kernel "
                    f"'{self._call.name}' specifies that this is an "
                    f"integer field, however it is declared as a "
                    f"'{alg_datatype}' in the algorithm code.")
            argtype = "integer_field"
        else:
            raise InternalError(
                f"Expected one of {const.VALID_FIELD_INTRINSIC_TYPES} "
                f"intrinsic types for a field argument but found "
                f"'{self.intrinsic_type}'.")
        self._data_type = const.DATA_TYPE_MAP[argtype]["type"]
        self._precision = const.DATA_TYPE_MAP[argtype]["kind"]
        self._proxy_data_type = const.DATA_TYPE_MAP[argtype]["proxy_type"]
        self._module_name = const.DATA_TYPE_MAP[argtype]["module"]

    def _init_operator_properties(self, alg_datatype, check=True):
        '''Set up the properties of this operator using algorithm datatype
        information if it is available.

        :param alg_datatype: the datatype of this argument as \
            specified in the algorithm layer or None if it is not \
            known.
        :type alg_datatype: str or NoneType
        :param bool check: whether to use the algorithm \
            information. Optional argument that defaults to True.
        :raises GenerationError: if the datatype for a gh_operator \
            could not be found in the algorithm layer (and check is \
            True).
        :raises GenerationError: if the datatype specified in the \
            algorithm layer is inconsistent with the kernel metadata.
        :raises InternalError: if this argument is not an operator.

        '''
        const = LFRicConstants()
        argtype = None
        if self.argument_type == "gh_operator":
            if not check:
                # Use the default as we are ignoring any algorithm info
                argtype = "operator"
            elif not alg_datatype:
                # Raise an exception as we require algorithm
                # information to determine the precision of the
                # operator
                raise GenerationError(
                    f"It was not possible to determine the operator type "
                    f"from the algorithm layer for argument '{self.name}' "
                    f"in kernel '{self._call.name}'.")
            elif alg_datatype == "operator_type":
                argtype = "operator"
            elif alg_datatype == "r_solver_operator_type":
                argtype = "r_solver_operator"
            else:
                raise GenerationError(
                    f"The metadata for argument '{self.name}' in kernel "
                    f"'{self._call.name}' specifies that this is an "
                    f"operator, however it is declared as a "
                    f"'{alg_datatype}' in the algorithm code.")
        elif self.argument_type == "gh_columnwise_operator":
            if check and alg_datatype and \
               alg_datatype != "columnwise_operator_type":
                raise GenerationError(
                    f"The metadata for argument '{self.name}' in kernel "
                    f"'{self._call.name}' specifies that this is a "
                    f"columnwise operator, however it is declared as a "
                    f"'{alg_datatype}' in the algorithm code.")
            argtype = "columnwise_operator"
        else:
            raise InternalError(
                f"Expected 'gh_operator' or 'gh_columnwise_operator' "
                f"argument type but found '{self.argument_type}'.")
        self._data_type = const.DATA_TYPE_MAP[argtype]["type"]
        self._precision = const.DATA_TYPE_MAP[argtype]["kind"]
        self._proxy_data_type = const.DATA_TYPE_MAP[argtype]["proxy_type"]
        self._module_name = const.DATA_TYPE_MAP[argtype]["module"]

    @property
    def is_scalar(self):
        '''
        :returns: True if this kernel argument represents a scalar, \
                  False otherwise.
        :rtype: bool
        '''
        const = LFRicConstants()
        return self._argument_type in const.VALID_SCALAR_NAMES

    @property
    def is_field(self):
        '''
        :returns: True if this kernel argument represents a field, \
                  False otherwise.
        :rtype: bool
        '''
        const = LFRicConstants()
        return self._argument_type in const.VALID_FIELD_NAMES

    @property
    def is_operator(self):
        '''
        :returns: True if this kernel argument represents an operator, \
                  False otherwise.
        :rtype: bool
        '''
        const = LFRicConstants()
        return self._argument_type in const.VALID_OPERATOR_NAMES

    @property
    def descriptor(self):
        '''
        :returns: a descriptor object which contains Kernel metadata \
                  about this argument.
        :rtype: :py:class:`psyclone.domain.lfric.LFRicArgDescriptor`
        '''
        return self._arg

    @property
    def argument_type(self):
        '''
        :returns: the API type of this argument, as specified in \
                  the metadata.
        :rtype: str
        '''
        return self._argument_type

    @property
    def intrinsic_type(self):
        '''
        :returns: the intrinsic Fortran type of this argument for scalars \
                  or of the argument's data for fields and operators.
        :rtype: str
        '''
        return self._intrinsic_type

    @property
    def mesh(self):
        '''
        :returns: mesh associated with argument ('GH_FINE' or 'GH_COARSE').
        :rtype: str
        '''
        return self._mesh

    @property
    def vector_size(self):
        '''
        :returns: the vector size of this argument as specified in \
                  the Kernel metadata.
        :rtype: str
        '''
        return self._vector_size

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

    def psyir_expression(self):
        '''
        Looks up or creates a reference to a suitable Symbol for this kernel
        argument. If the argument is a scalar that has been provided as a
        literal (in the Algorithm layer) then the PSyIR of the expression
        is returned.

        :returns: the PSyIR for this kernel argument.
        :rtype: :py:class:`psyclone.psyir.nodes.Node`

        :raises NotImplementedError: if this argument is not a literal, scalar
                                     or field.

        '''
        symbol_table = self._call.scope.symbol_table

        if self.is_literal:
            reader = FortranReader()
            return reader.psyir_from_expression(self.name, symbol_table)

        if self.is_scalar:
            try:
                scalar_sym = symbol_table.lookup(self.name)
            except KeyError:
                # TODO once #1258 is done the symbols should already exist
                # and therefore we should raise an exception if not.
                scalar_sym = symbol_table.new_symbol(
                    self.name, symbol_type=DataSymbol,
                    datatype=self.infer_datatype())
            return Reference(scalar_sym)

        if self.is_field:
            # Although the argument to a Kernel is a field, the data itself
            # is accessed through a field_proxy.
            try:
                sym = symbol_table.lookup(self.proxy_name)
            except KeyError:
                # TODO once #1258 is done the symbols should already exist
                # and therefore we should raise an exception if not.
                sym = symbol_table.new_symbol(
                    self.proxy_name, symbol_type=DataSymbol,
                    datatype=self.infer_datatype(proxy=True))
            return Reference(sym)

        raise NotImplementedError(
            "Unsupported kernel argument type: '{0}' is of type '{1}' "
            "which is not recognised as being a literal, scalar or "
            "field.".format(self.name, self.argument_type))

    @property
    def declaration_name(self):
        '''
        :returns: the name for this argument with the array dimensions \
                  added if required.
        :rtype: str
        '''
        if self._vector_size > 1:
            return self._name+"("+str(self._vector_size)+")"
        return self._name

    @property
    def proxy_name(self):
        '''
        :returns: the proxy name for this argument.
        :rtype: str
        '''
        return self._name+"_proxy"

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
    def proxy_data_type(self):
        '''
        :returns: the type of this argument's proxy (if it exists) as \
                  defined in LFRic infrastructure.
        :rtype: str or NoneType

        '''
        return self._proxy_data_type

    @property
    def function_space(self):
        '''
        Returns the expected finite element function space for a kernel
        argument as specified by the kernel argument metadata: a single
        function space for a field and function_space_from for an operator.

        :returns: function space for this argument.
        :rtype: :py:class:`psyclone.domain.lfric.FunctionSpace`
        '''
        if self._argument_type == "gh_operator":
            # We return the 'from' space for an operator argument
            return self.function_space_from
        return self._function_spaces[0]

    @property
    def function_space_to(self):
        '''
        :returns: the 'to' function space of an operator.
        :rtype: str
        '''
        return self._function_spaces[0]

    @property
    def function_space_from(self):
        '''
        :returns:  the 'from' function space of an operator.
        :rtype: str
        '''
        return self._function_spaces[1]

    @property
    def function_spaces(self):
        '''
        Returns the expected finite element function space for a kernel
        argument as specified by the kernel argument metadata: a single
        function space for a field and a list containing
        function_space_to and function_space_from for an operator.

        :returns: function space(s) for this argument.
        :rtype: list of :py:class:`psyclone.domain.lfric.FunctionSpace`

        '''
        return self._function_spaces

    @property
    def function_space_names(self):
        '''
        Returns a list of the names of the function spaces associated
        with this argument. We have more than one function space when
        dealing with operators.

        :returns: list of function space names for this argument.
        :rtype: list of str

        '''
        fs_names = []
        for fspace in self._function_spaces:
            if fspace:
                fs_names.append(fspace.orig_name)
        return fs_names

    @property
    def intent(self):
        '''
        Returns the Fortran intent of this argument as defined by the
        valid access types for this API

        :returns: the expected Fortran intent for this argument as \
                  specified by the kernel argument metadata
        :rtype: str

        '''
        write_accesses = AccessType.all_write_accesses()
        if self.access == AccessType.READ:
            return "in"
        if self.access in write_accesses:
            return "inout"
        # An argument access other than the pure "read" or one of
        # the "write" accesses is invalid
        valid_accesses = [AccessType.READ.api_specific_name()] + \
            [access.api_specific_name() for access in write_accesses]
        raise GenerationError(
            "In the LFRic API the argument access must be one of {0}, "
            "but found '{1}'.".format(valid_accesses, self.access))

    @property
    def discontinuous(self):
        '''
        Returns True if this argument is known to be on a discontinuous
        function space including any_discontinuous_space, otherwise
        returns False.

        :returns: whether the argument is discontinuous.
        :rtype: bool

        '''
        const = LFRicConstants()
        if self.function_space.orig_name in \
           const.VALID_DISCONTINUOUS_NAMES:
            return True
        if self.function_space.orig_name in \
           const.VALID_ANY_SPACE_NAMES:
            # We will eventually look this up based on our dependence
            # analysis but for the moment we assume the worst
            return False
        return False

    @property
    def stencil(self):
        '''
        :returns: stencil information for this argument if it exists.
        :rtype: :py:class:`psyclone.dynamo0p3.DynStencil`
        '''
        return self._stencil

    @stencil.setter
    def stencil(self, value):
        '''
        Sets stencil information for this kernel argument.

        :param value: stencil information for this argument.
        :type value: :py:class:`psyclone.dynamo0p3.DynStencil`

        '''
        self._stencil = value

    def infer_datatype(self, proxy=False):
        '''
        Infer the datatype of this kernel argument in the PSy layer using
        the LFRic API rules. If any LFRic infrastructure modules are required
        but are not already present then suitable ContainerSymbols are added
        to the outermost symbol table. Similarly, DataTypeSymbols are added for
        any required LFRic derived types that are not already in the symbol
        table.

        TODO #1258 - ultimately this routine should not have to create any
        DataTypeSymbols as that should already have been done.

        :param bool proxy: whether or not we want the type of the proxy \
            object for this kernel argument. Defaults to False (i.e.
            return the type rather than the proxy type).

        :returns: the datatype of this argument.
        :rtype: :py:class:`psyclone.psyir.symbols.DataType`

        :raises NotImplementedError: if an unsupported argument type is found.

        '''
        # We want to put any Container symbols in the outermost scope so find
        # the corresponding symbol table.
        symbol_table = self._call.scope.symbol_table
        root_table = symbol_table
        while root_table.parent_symbol_table():
            root_table = root_table.parent_symbol_table()

        def _find_or_create_type(mod_name, type_name):
            '''
            Utility to find or create a DataTypeSymbol with the supplied name,
            imported from the named module.

            :param str mod_name: the name of the module from which the \
                                 DataTypeSymbol should be imported.
            :param str type_name: the name of the derived type for which to \
                                  create a DataTypeSymbol.

            :returns: the symbol for the requested type.
            :rtype: :py:class:`psyclone.psyir.symbols.DataTypeSymbol`

            '''
            return root_table.find_or_create(
                    type_name,
                    symbol_type=DataTypeSymbol,
                    datatype=DeferredType(),
                    interface=ImportInterface(root_table.find_or_create(
                        mod_name,
                        symbol_type=ContainerSymbol)
                        ))

        if self.is_scalar:
            # Find or create the DataType for the appropriate scalar type.
            if self.intrinsic_type == "real":
                prim_type = ScalarType.Intrinsic.REAL
            elif self.intrinsic_type == "integer":
                prim_type = ScalarType.Intrinsic.INTEGER
            elif self.intrinsic_type == "logical":
                prim_type = ScalarType.Intrinsic.BOOLEAN
            else:
                raise NotImplementedError(
                    f"Unsupported scalar type '{self.intrinsic_type}'")

            kind_name = self.precision
            try:
                kind_symbol = symbol_table.lookup(kind_name)
            except KeyError:
                try:
                    constants_container = symbol_table.lookup(
                        "constants_mod")
                except KeyError:
                    # TODO Once #696 is done, we should *always* have a
                    # symbol for this container at this point so should
                    # raise an exception if we haven't. Also, the name
                    # of the Fortran module should be read from the config
                    # file.
                    constants_container = ContainerSymbol("constants_mod")
                    root_table.add(constants_container)
                kind_symbol = DataSymbol(
                    kind_name, INTEGER_SINGLE_TYPE,
                    interface=ImportInterface(constants_container))
                root_table.add(kind_symbol)
            return ScalarType(prim_type, kind_symbol)

        if self.is_field or self.is_operator:
            # Find or create the DataTypeSymbol for the appropriate
            # field or operator type.
            mod_name = self._module_name
            if proxy:
                type_name = self._proxy_data_type
            else:
                type_name = self._data_type
            return _find_or_create_type(mod_name, type_name)

        raise NotImplementedError(
            f"'{str(self)}' is not a scalar, field or operator argument")


class DynKernCallFactory(object):
    ''' Create the necessary framework for a Dynamo kernel call.
    This consists of a Loop over cells containing a call to the
    user-supplied kernel routine.

    '''
    # pylint: disable=too-few-public-methods
    @staticmethod
    def create(call, parent=None):
        '''
        Create the objects needed for a call to the kernel
        described in the call object.

        :param call: information on the kernel call as obtained from the \
                     Algorithm layer.
        :type call: :py:class:`psyclone.parse.algorithm.KernelCall`
        :param parent: the parent of this kernel call in the PSyIR.
        :type parent: :py:class:`psyclone.psyir.nodes.Schedule`

        '''
        if call.ktype.iterates_over == "domain":
            # Kernel operates on whole domain so there is no loop.
            # We still need a loop object though as that is where the logic
            # for handling halo exchanges is currently implemented.
            loop_type = "null"
        else:
            # Loop over cells, indicated by an empty string.
            loop_type = ""
        cloop = DynLoop(parent=parent, loop_type=loop_type)

        # The kernel itself
        kern = DynKern()
        kern.load(call, cloop.loop_body)

        # Add the kernel as a child of the loop
        cloop.loop_body.addchild(kern)

        # Set-up the loop now we have the kernel object
        cloop.load(kern)
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
    'DynFuncDescriptor03',
    'DynKernMetadata',
    'DynamoPSy',
    'DynamoInvokes',
    'DynCollection',
    'DynStencils',
    'DynDofmaps',
    'DynFunctionSpaces',
    'LFRicFields',
    'DynProxies',
    'DynCellIterators',
    'LFRicScalarArgs',
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
    'FSDescriptor',
    'FSDescriptors',
    'DynStencil',
    'DynKernelArguments',
    'DynKernelArgument',
    'DynKernCallFactory',
    'DynACCEnterDataDirective']
