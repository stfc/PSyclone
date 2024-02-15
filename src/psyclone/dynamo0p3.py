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
# Authors R. W. Ford, A. R. Porter and S. Siso, STFC Daresbury Lab
# Modified I. Kavcic, A. Coughtrie, L. Turner and O. Brunt, Met Office
# Modified J. Henrichs, Bureau of Meteorology
# Modified A. B. G. Chalk and N. Nobre, STFC Daresbury Lab

''' This module implements the PSyclone Dynamo 0.3 API by 1)
    specialising the required base classes in parser.py (KernelType) and
    adding a new class (DynFuncDescriptor03) to capture function descriptor
    metadata and 2) specialising the required base classes in psyGen.py
    (PSy, Invokes, Invoke, InvokeSchedule, Loop, Kern, Inf, Arguments and
    Argument). '''

import os
from enum import Enum
from collections import OrderedDict, namedtuple
from dataclasses import dataclass
from typing import Any

from psyclone import psyGen
from psyclone.configuration import Config
from psyclone.core import AccessType, Signature
from psyclone.domain.lfric.lfric_builtins import (LFRicBuiltInCallFactory,
                                                  LFRicBuiltIn)
from psyclone.domain.lfric import (FunctionSpace, KernCallAccArgList,
                                   KernCallArgList,
                                   LFRicCollection, LFRicConstants,
                                   LFRicSymbolTable, LFRicKernCallFactory,
                                   LFRicKern, LFRicInvokes, LFRicTypes,
                                   LFRicLoop)
from psyclone.errors import GenerationError, InternalError, FieldNotFoundError
from psyclone.f2pygen import (AllocateGen, AssignGen, CallGen, CommentGen,
                              DeallocateGen, DeclGen, DoGen, IfThenGen,
                              ModuleGen, TypeDeclGen, UseGen, PSyIRGen)
from psyclone.parse.kernel import getkerneldescriptors
from psyclone.parse.utils import ParseError
from psyclone.psyGen import (PSy, InvokeSchedule, Arguments,
                             KernelArgument, HaloExchange, GlobalSum,
                             DataAccess)
from psyclone.psyir.frontend.fortran import FortranReader
from psyclone.psyir.nodes import Reference, ACCEnterDataDirective, ScopingNode
from psyclone.psyir.symbols import (INTEGER_TYPE, DataSymbol, ScalarType,
                                    UnresolvedType, DataTypeSymbol,
                                    ContainerSymbol, ImportInterface,
                                    ArrayType, UnsupportedFortranType)


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
            f"Unrecognised shape ('{basis_fn['''shape''']}') specified in "
            f"dynamo0p3.qr_basis_alloc_args(). Should be one of: "
            f"{const.VALID_QUADRATURE_SHAPES}")

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
            f"Unrecognised shape '{basis_fn['''shape''']}' specified in "
            f"dynamo0p3.qr_basis_alloc_args(). Should be one of: "
            f"{const.VALID_QUADRATURE_SHAPES}")
    return alloc_args

# ---------- Classes -------------------------------------------------------- #


class DynFuncDescriptor03():
    ''' The Dynamo 0.3 API includes a function-space descriptor as
    well as an argument descriptor which is not supported by the base
    classes. This class captures the information specified in a
    function-space descriptor. '''

    def __init__(self, func_type):
        self._func_type = func_type
        if func_type.name != 'func_type':
            raise ParseError(
                f"In the dynamo0.3 API each meta_func entry must be of type "
                f"'func_type' but found '{func_type.name}'")
        if len(func_type.args) < 2:
            raise ParseError(
                f"In the dynamo0.3 API each meta_func entry must have at "
                f"least 2 args, but found {len(func_type.args)}")
        self._operator_names = []
        const = LFRicConstants()
        for idx, arg in enumerate(func_type.args):
            if idx == 0:  # first func_type arg
                if arg.name not in const.VALID_FUNCTION_SPACE_NAMES:
                    raise ParseError(
                        f"In the dynamo0p3 API the 1st argument of a "
                        f"meta_func entry should be a valid function space "
                        f"name (one of {const.VALID_FUNCTION_SPACE_NAMES}), "
                        f"but found '{arg.name}' in '{func_type}'")
                self._function_space_name = arg.name
            else:  # subsequent func_type args
                if arg.name not in const.VALID_METAFUNC_NAMES:
                    raise ParseError(
                        f"In the dynamo0.3 API, the 2nd argument and all "
                        f"subsequent arguments of a meta_func entry should "
                        f"be one of {const.VALID_METAFUNC_NAMES}, but found "
                        f"'{arg.name}' in '{func_type}'")
                if arg.name in self._operator_names:
                    raise ParseError(
                        f"In the dynamo0.3 API, it is an error to specify an "
                        f"operator name more than once in a meta_func entry, "
                        f"but '{arg.name}' is replicated in '{func_type}'")
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
        return f"DynFuncDescriptor03({self._func_type})"

    def __str__(self):
        res = "DynFuncDescriptor03 object" + os.linesep
        res += f"  name='{self._name}'" + os.linesep
        res += f"  nargs={len(self._operator_names)+1}" + os.linesep
        res += f"  function_space_name[{0}] = '{self._function_space_name}'" \
               + os.linesep
        for idx, arg in enumerate(self._operator_names):
            res += f"  operator_name[{idx+1}] = '{arg}'" + \
                   os.linesep
        return res


class RefElementMetaData():
    '''
    Class responsible for parsing reference-element metadata and storing
    the properties that a kernel requires.

    :param str kernel_name: name of the Kernel that the metadata is for.
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
        the names that must be used in kernel metadata.

        '''
        NORMALS_TO_HORIZONTAL_FACES = 1
        NORMALS_TO_VERTICAL_FACES = 2
        NORMALS_TO_FACES = 3
        OUTWARD_NORMALS_TO_HORIZONTAL_FACES = 4
        OUTWARD_NORMALS_TO_VERTICAL_FACES = 5
        OUTWARD_NORMALS_TO_FACES = 6

    def __init__(self, kernel_name, type_declns):
        # The list of properties requested in the metadata (if any)
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
            # The metadata entry is a declaration of a Fortran array of type
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
            raise ParseError(
                f"Unsupported reference-element property: '{arg}'. Supported "
                f"values are: {sorted_names}") from err

        # Check for duplicate properties
        for prop in self.properties:
            if self.properties.count(prop) > 1:
                raise ParseError(f"Duplicate reference-element property "
                                 f"found: '{prop}'.")


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


class MeshPropertiesMetaData():
    '''
    Parses any mesh-property kernel metadata and stores the properties that
    a kernel requires.

    :param str kernel_name: name of the kernel that the metadata is for.
    :param type_declns: list of fparser1 parse tree nodes representing type \
                        declaration statements.
    :type type_declns: list of :py:class:`fparser.one.typedecl_statements.Type`

    :raises ParseError: if an unrecognised mesh property is found.
    :raises ParseError: if a duplicate mesh property is found.

    '''
    # pylint: disable=too-few-public-methods
    # The properties that may be specified in kernel metadata are a subset
    # of the MeshProperty enumeration values.
    supported_properties = [MeshProperty.ADJACENT_FACE]

    def __init__(self, kernel_name, type_declns):
        # The list of mesh properties requested in the metadata.
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
            # The metadata entry is a declaration of a Fortran array of type
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
            supported_mesh_prop = [pr.name for pr in self.supported_properties]
            raise ParseError(f"Unsupported mesh property in metadata: "
                             f"'{arg}'. Supported values are: "
                             f"{supported_mesh_prop}") from err

        # Check for duplicate properties
        for prop in self.properties:
            if self.properties.count(prop) > 1:
                raise ParseError(f"Duplicate mesh property "
                                 f"found: '{prop}'.")

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
        # Make sure the scoping node creates LFRicSymbolTables
        # TODO #1954: Remove the protected access using a factory
        ScopingNode._symbol_table_class = LFRicSymbolTable
        PSy.__init__(self, invoke_info)
        self._invokes = LFRicInvokes(invoke_info.calls, self)
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
        # LFRic constants.
        self._infrastructure_modules[const_mod] = kind_names

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

        # If the container has a Routine that is not an InvokeSchedule
        # it should also be added to the generated module.
        for routine in self.container.children:
            if not isinstance(routine, InvokeSchedule):
                psy_module.add(PSyIRGen(psy_module, routine))

        # Add all invoke-specific information
        self.invokes.gen_code(psy_module)

        # Include required constants and infrastructure modules. The sets of
        # required LFRic data structures and their proxies are updated in
        # the relevant field and operator subclasses of LFRicCollection.
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


class LFRicMeshProperties(LFRicCollection):
    '''
    Holds all information on the the mesh properties required by either an
    invoke or a kernel stub. Note that the creation of a suitable mesh
    object is handled in the `DynMeshes` class. This class merely deals with
    extracting the necessary properties from that object and providing them to
    kernels.

    :param node: kernel or invoke for which to manage mesh properties.
    :type node: :py:class:`psyclone.domain.lfric.LFRicKern` or \
                :py:class:`psyclone.dynamo0p3.LFRicInvoke`

    '''
    def __init__(self, node):
        super().__init__(node)

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
            name_lower = prop.name.lower()
            if prop.name in ["NCELL_2D", "NCELL_2D_NO_HALOS"]:
                # This is an integer:
                self._symbol_table.find_or_create_integer_symbol(
                    name_lower, tag=name_lower)
            else:
                # E.g.: adjacent_face
                self._symbol_table.find_or_create_array(
                    name_lower, 2, ScalarType.Intrinsic.INTEGER,
                    tag=name_lower)

    def kern_args(self, stub=False, var_accesses=None,
                  kern_call_arg_list=None):
        # pylint: disable=too-many-locals, too-many-branches
        '''
        Provides the list of kernel arguments associated with the mesh
        properties that the kernel requires. Optionally adds variable
        access information if var_accesses is given.

        :param bool stub: whether or not we are generating code for a \
                          kernel stub.
        :param var_accesses: optional VariablesAccessInfo instance to store \
            the information about variable accesses.
        :type var_accesses: \
            :py:class:`psyclone.core.VariablesAccessInfo`
        :param kern_call_arg_list: an optional KernCallArgList instance \
            used to store PSyIR representation of the arguments.
        :type kern_call_arg_list: \
            Optional[:py:class:`psyclone.domain.lfric.KernCallArgList`]

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
                    if kern_call_arg_list:
                        sym = kern_call_arg_list.\
                            append_integer_reference("nfaces_re_h")
                        name = sym.name
                    else:
                        name = self._symbol_table.\
                            find_or_create_integer_symbol(
                                "nfaces_re_h", tag="nfaces_re_h").name
                    arg_list.append(name)
                    if var_accesses is not None:
                        var_accesses.add_access(Signature(name),
                                                AccessType.READ, self._kernel)

                adj_face = "adjacent_face"
                if not stub and kern_call_arg_list:
                    # Use the functionality in kern_call_arg_list to properly
                    # declare the symbol and to create a PSyIR reference for it
                    _, cell_ref = \
                        kern_call_arg_list.cell_ref_name(var_accesses)
                    adj_face_sym = kern_call_arg_list. \
                        append_array_reference(adj_face,
                                               [":", cell_ref],
                                               ScalarType.Intrinsic.INTEGER)
                    # Update the name in case there was a clash
                    adj_face = adj_face_sym.name
                    if var_accesses:
                        var_accesses.add_access(Signature(adj_face),
                                                AccessType.READ, self._kernel,
                                                [":", cell_ref])

                if not stub:
                    adj_face = self._symbol_table.find_or_create_tag(
                        "adjacent_face").name
                    cell_name = "cell"
                    if self._kernel.is_coloured():
                        colour_name = "colour"
                        cmap_name = self._symbol_table.find_or_create_tag(
                            "cmap", root_name="cmap").name
                        adj_face += (f"(:,{cmap_name}({colour_name},"
                                     f"{cell_name}))")
                    else:
                        adj_face += f"(:,{cell_name})"
                arg_list.append(adj_face)

                if var_accesses and not kern_call_arg_list:
                    # TODO #1320 Replace [1]
                    # The [1] just indicates that this variable is accessed
                    # as a rank 1 array. #1320 will improve this.
                    var_accesses.add_access(Signature(adj_face),
                                            AccessType.READ, self._kernel,
                                            [1])
            else:
                raise InternalError(
                    f"kern_args: found unsupported mesh property '{prop}' "
                    f"when generating arguments for kernel "
                    f"'{self._kernel.name}'. Only members of the MeshProperty "
                    f"Enum are permitted ({list(MeshProperty)}).")

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
                name = self._symbol_table.find_or_create_integer_symbol(
                    "ncell_2d_no_halos",
                    tag="ncell_2d_no_halos").name
                parent.add(DeclGen(parent, datatype="integer",
                                   kind=api_config.default_kind["integer"],
                                   entity_decls=[name]))
            elif prop == MeshProperty.NCELL_2D:
                name = self._symbol_table.find_or_create_integer_symbol(
                    "ncell_2d", tag="ncell_2d").name
                parent.add(DeclGen(parent, datatype="integer",
                                   kind=api_config.default_kind["integer"],
                                   entity_decls=[name]))
            else:
                raise InternalError(
                    f"Found unsupported mesh property '{prop}' when generating"
                    f" invoke declarations. Only members of the MeshProperty "
                    f"Enum are permitted ({list(MeshProperty)}).")

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
                adj_face = self._symbol_table.find_or_create_array(
                    "adjacent_face", 2, ScalarType.Intrinsic.INTEGER,
                    tag="adjacent_face").name
                # 'nfaces_re_h' will have been declared by the
                # DynReferenceElement class.
                dimension = self._symbol_table.\
                    find_or_create_integer_symbol("nfaces_re_h",
                                                  tag="nfaces_re_h").name
                parent.add(
                    DeclGen(
                        parent, datatype="integer",
                        kind=api_config.default_kind["integer"],
                        dimension=dimension,
                        intent="in", entity_decls=[adj_face]))
            elif prop == MeshProperty.NCELL_2D:
                ncell_2d = self._symbol_table.find_or_create_integer_symbol(
                    "ncell_2d", tag="ncell_2d")
                parent.add(
                    DeclGen(parent, datatype="integer",
                            kind=api_config.default_kind["integer"],
                            intent="in", entity_decls=[ncell_2d.name]))
            else:
                raise InternalError(
                    f"Found unsupported mesh property '{prop}' when generating"
                    f" declarations for kernel stub. Only members of the "
                    f"MeshProperty Enum are permitted ({list(MeshProperty)})")

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
                name = self._symbol_table.find_or_create_integer_symbol(
                    "ncell_2d_no_halos", tag="ncell_2d_no_halos").name
                parent.add(AssignGen(parent, lhs=name,
                                     rhs=mesh+"%get_last_edge_cell()"))

            elif prop == MeshProperty.NCELL_2D:
                name = self._symbol_table.find_or_create_integer_symbol(
                    "ncell_2d", tag="ncell_2d").name
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


class DynReferenceElement(LFRicCollection):
    '''
    Holds all information on the properties of the Reference Element
    required by an Invoke or a Kernel stub.

    :param node: Kernel or Invoke for which to manage Reference-Element \
                 properties.
    :type node: :py:class:`psyclone.domain.lfric.LFRicKern` or \
                :py:class:`psyclone.dynamo0p3.LFRicInvoke`

    :raises InternalError: if an unsupported reference-element property \
                           is encountered.

    '''
    # pylint: disable=too-many-instance-attributes
    def __init__(self, node):
        # pylint: disable=too-many-branches, too-many-statements
        super().__init__(node)

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
        self._nfaces_h_symbol = None
        self._nfaces_v_symbol = None
        self._nfaces_symbol = None
        # Horizontal normals to faces,
        self._horiz_face_normals_symbol = None
        self._horiz_face_out_normals_symbol = None
        # Vertical normals to faces,
        self._vert_face_normals_symbol = None
        self._vert_face_out_normals_symbol = None
        # All normals to faces.
        self._face_normals_symbol = None
        self._face_out_normals_symbol = None

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
            self._nfaces_h_symbol = symtab.find_or_create_integer_symbol(
                "nfaces_re_h", tag="nfaces_re_h")
        # Provide no. of vertical faces if required
        if (RefElementMetaData.Property.NORMALS_TO_VERTICAL_FACES
                in self._properties or
                RefElementMetaData.Property.OUTWARD_NORMALS_TO_VERTICAL_FACES
                in self._properties):
            self._nfaces_v_symbol = symtab.find_or_create_integer_symbol(
                "nfaces_re_v", tag="nfaces_re_v")
        # Provide no. of all faces if required
        if (RefElementMetaData.Property.NORMALS_TO_FACES
                in self._properties or
                RefElementMetaData.Property.OUTWARD_NORMALS_TO_FACES
                in self._properties):
            self._nfaces_symbol = symtab.find_or_create_integer_symbol(
                "nfaces_re", tag="nfaces_re")

        # Now the arrays themselves, in the order specified in the
        # kernel metadata (in the case of a kernel stub)
        for prop in self._properties:
            # Provide horizontal normals to faces
            if prop == RefElementMetaData.Property.NORMALS_TO_HORIZONTAL_FACES:
                name = "normals_to_horiz_faces"
                self._horiz_face_normals_symbol = \
                    symtab.find_or_create_array(name, 2,
                                                ScalarType.Intrinsic.REAL,
                                                tag=name)
                if self._horiz_face_normals_symbol not in self._arg_properties:
                    self._arg_properties[self._horiz_face_normals_symbol] = \
                         self._nfaces_h_symbol
            # Provide horizontal normals to "outward" faces
            elif prop == (RefElementMetaData.Property.
                          OUTWARD_NORMALS_TO_HORIZONTAL_FACES):
                name = "out_normals_to_horiz_faces"
                self._horiz_face_out_normals_symbol = \
                    symtab.find_or_create_array(name, 2,
                                                ScalarType.Intrinsic.REAL,
                                                tag=name)
                if self._horiz_face_out_normals_symbol not in \
                        self._arg_properties:
                    self._arg_properties[self._horiz_face_out_normals_symbol] \
                        = self._nfaces_h_symbol
            elif prop == (RefElementMetaData.Property.
                          NORMALS_TO_VERTICAL_FACES):
                name = "normals_to_vert_faces"
                self._vert_face_normals_symbol = \
                    symtab.find_or_create_array(name, 2,
                                                ScalarType.Intrinsic.REAL,
                                                tag=name)
                if self._vert_face_normals_symbol not in self._arg_properties:
                    self._arg_properties[self._vert_face_normals_symbol] = \
                         self._nfaces_v_symbol
            # Provide vertical normals to "outward" faces
            elif prop == (RefElementMetaData.Property.
                          OUTWARD_NORMALS_TO_VERTICAL_FACES):
                name = "out_normals_to_vert_faces"
                self._vert_face_out_normals_symbol = \
                    symtab.find_or_create_array(name, 2,
                                                ScalarType.Intrinsic.REAL,
                                                tag=name)
                if self._vert_face_out_normals_symbol not in \
                        self._arg_properties:
                    self._arg_properties[self._vert_face_out_normals_symbol] \
                        = self._nfaces_v_symbol
            # Provide normals to all faces
            elif prop == RefElementMetaData.Property.NORMALS_TO_FACES:
                name = "normals_to_faces"
                self._face_normals_symbol = \
                    symtab.find_or_create_array(name, 2,
                                                ScalarType.Intrinsic.REAL,
                                                tag=name)
                if self._face_normals_symbol not in self._arg_properties:
                    self._arg_properties[self._face_normals_symbol] = \
                        self._nfaces_symbol
            # Provide vertical normals to all "outward" faces
            elif prop == RefElementMetaData.Property.OUTWARD_NORMALS_TO_FACES:
                name = "out_normals_to_faces"
                self._face_out_normals_symbol = \
                    symtab.find_or_create_array(name, 2,
                                                ScalarType.Intrinsic.REAL,
                                                tag=name)
                if self._face_out_normals_symbol not in \
                   self._arg_properties:
                    self._arg_properties[self._face_out_normals_symbol] = \
                        self._nfaces_symbol
            else:
                all_props = [str(sprop)
                             for sprop in RefElementMetaData.Property]
                raise InternalError(
                    f"Unsupported reference-element property ('{prop}') "
                    f"found when generating arguments for kernel "
                    f"'{self._kernel.name}'. Supported properties are: "
                    f"{all_props}")

    def kern_args(self):
        '''
        :returns: the argument list for kernel call/stub arguments.
        :rtype: List[str]

        '''
        argdict = self._arg_properties
        # Remove duplicate "nfaces" by using OrderedDict
        nfaces = list(OrderedDict.fromkeys(argdict.values()))
        kern_args = nfaces + list(argdict.keys())
        return [sym.name for sym in kern_args]

    def kern_args_symbols(self):
        '''
        :returns: the argument symbol list for kernel call/stub arguments.
        :rtype: List[:py:class:`psyclone.psyir.symbols.Symbol`]

        '''
        argdict = self._arg_properties
        # Remove duplicate "nfaces" by using OrderedDict
        nfaces = list(OrderedDict.fromkeys(argdict.values()))
        return nfaces + list(argdict.keys())

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
            nface_vars = [self._nfaces_h_symbol]
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
                           entity_decls=[var.name for var in nface_vars]))

        if not self._properties:
            # We only need the number of horizontal faces so we're done
            return

        # Declare the necessary arrays
        array_decls = [f"{sym.name}(:,:)"
                       for sym in self._arg_properties.keys()]
        my_kind = api_config.default_kind["real"]
        parent.add(DeclGen(parent, datatype="real", kind=my_kind,
                           allocatable=True, entity_decls=array_decls))
        # Ensure the necessary kind parameter is imported.
        const_mod = const.UTILITIES_MOD_MAP["constants"]["module"]
        const_mod_uses = self._invoke.invokes.psy.infrastructure_modules[
            const_mod]
        const_mod_uses.add(my_kind)

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
        nfaces_h = self._symbol_table.find_or_create_integer_symbol(
            "nfaces_re_h", tag="nfaces_re_h")
        if self._nfaces_h_required and nfaces_h not in scalars:
            scalars.append(nfaces_h)

        for nface in scalars:
            parent.add(DeclGen(parent, datatype="integer",
                               kind=api_config.default_kind["integer"],
                               intent="in", entity_decls=[nface.name]))

        # Declare the necessary arrays
        for arr, sym in self._arg_properties.items():
            dimension = f"3,{sym.name}"
            parent.add(DeclGen(parent, datatype="real",
                               kind=api_config.default_kind["real"],
                               intent="in", dimension=dimension,
                               entity_decls=[arr.name]))

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

        if self._nfaces_h_symbol:
            parent.add(
                AssignGen(parent, lhs=self._nfaces_h_symbol.name,
                          rhs=self._ref_elem_name +
                          "%get_number_horizontal_faces()"))
        if self._nfaces_v_symbol:
            parent.add(
                AssignGen(
                    parent, lhs=self._nfaces_v_symbol.name,
                    rhs=self._ref_elem_name + "%get_number_vertical_faces()"))

        if self._nfaces_symbol:
            parent.add(
                AssignGen(
                    parent, lhs=self._nfaces_symbol.name,
                    rhs=self._ref_elem_name + "%get_number_faces()"))

        if self._horiz_face_normals_symbol:
            parent.add(
                CallGen(parent,
                        name=f"{self._ref_elem_name}%get_normals_to_"
                             f"horizontal_faces("
                             f"{self._horiz_face_normals_symbol.name})"))

        if self._horiz_face_out_normals_symbol:
            parent.add(
                CallGen(
                    parent,
                    name=f"{self._ref_elem_name}%get_outward_normals_to_"
                         f"horizontal_faces("
                         f"{self._horiz_face_out_normals_symbol.name})"))

        if self._vert_face_normals_symbol:
            parent.add(
                CallGen(parent,
                        name=f"{self._ref_elem_name}%get_normals_to_vertical_"
                             f"faces({self._vert_face_normals_symbol.name})"))

        if self._vert_face_out_normals_symbol:
            parent.add(
                CallGen(
                    parent,
                    name=f"{self._ref_elem_name}%get_outward_normals_to_"
                         f"vertical_faces"
                         f"({self._vert_face_out_normals_symbol.name})"))

        if self._face_normals_symbol:
            parent.add(
                CallGen(parent,
                        name=f"{self._ref_elem_name}%get_normals_to_faces"
                             f"({self._face_normals_symbol.name})"))

        if self._face_out_normals_symbol:
            parent.add(
                CallGen(
                    parent,
                    name=f"{self._ref_elem_name}%get_outward_normals_to_"
                    f"faces({self._face_out_normals_symbol.name})"))


class DynDofmaps(LFRicCollection):
    '''
    Holds all information on the dofmaps (including column-banded and
    indirection) required by an invoke.

    :param node: Kernel or Invoke for which to manage dofmaps.
    :type node: :py:class:`psyclone.domain.lfric.LFRicKern` or \
                :py:class:`psyclone.dynamo0p3.LFRicInvoke`

    '''
    def __init__(self, node):
        # pylint: disable=too-many-branches
        super().__init__(node)

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
                            f"Internal error: there should only be one CMA "
                            f"operator argument for a CMA assembly kernel but "
                            f"found {len(cma_args)}")

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
                            f"Internal error: there should only be one CMA "
                            f"operator argument for a kernel that applies a "
                            f"CMA operator but found {len(cma_args)}")

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
                    f"Invalid direction ('{cma['''direction''']}') found for "
                    f"CMA operator when collecting column-banded dofmaps. "
                    f"Should be either 'to' or 'from'.")
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
                    f"Invalid direction ('{cma['''direction''']}') found for "
                    f"CMA operator when collecting indirection dofmaps. "
                    f"Should be either 'to' or 'from'.")
            parent.add(DeclGen(parent, datatype="integer",
                               kind=api_config.default_kind["integer"],
                               intent="in", entity_decls=[dim_name]))
            parent.add(DeclGen(parent, datatype="integer",
                               kind=api_config.default_kind["integer"],
                               intent="in", dimension=dim_name,
                               entity_decls=[dmap]))


class DynFunctionSpaces(LFRicCollection):
    '''
    Handles the declaration and initialisation of all function-space-related
    quantities required by an Invoke.

    :param invoke: the Invoke or Kernel object.
    '''
    def __init__(self, kern_or_invoke):
        super().__init__(kern_or_invoke)

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


class DynProxies(LFRicCollection):
    '''
    Handles all proxy-related declarations and initialisation. Unlike other
    sub-classes of LFRicCollection, we do not have to handle Kernel-stub
    generation since Kernels know nothing about proxies.

    An instance of this class is instantiated for each Invoke before the
    PSy Layer is constructed. For each unique field or operator argument to
    a kernel in the Invoke it:

      * Creates a DataSymbol for the corresponding proxy;
      * Creates a DataSymbol for the pointer to the data array accessed via
        the proxy. If the argument is a field vector then a DataSymbol is
        created for each component of the vector;
      * Tags that DataSymbol so that the correct symbol can always be looked
        up, irrespective of any name clashes;

    Note that since the Fortran standard forbids (Note 12.34 in the
    Fortran2008 standard) aliasing of effective arguments that are written to,
    the set of unique kernel arguments must refer to unique memory locations
    or to those that are read only.

    '''
    def __init__(self, node):
        super().__init__(node)
        const = LFRicConstants()
        real_field_args = self._invoke.unique_declarations(
            argument_types=const.VALID_FIELD_NAMES,
            intrinsic_type=const.MAPPING_DATA_TYPES["gh_real"])
        int_field_args = self._invoke.unique_declarations(
            argument_types=const.VALID_FIELD_NAMES,
            intrinsic_type=const.MAPPING_DATA_TYPES["gh_integer"])
        op_args = self._invoke.unique_declarations(
            argument_types=const.VALID_OPERATOR_NAMES)

        # We put precision Symbols in the Container symbol table.
        ctable = self._invoke.schedule.parent.symbol_table

        for arg in real_field_args + int_field_args + op_args:
            # Create symbols that we will associate with the internal
            # data arrays of fields, field vectors and LMA operators.
            if arg.argument_type == "gh_columnwise_operator":
                # CMA operators are handled by the DynCMAOperators class.
                continue
            ctable.add_lfric_precision_symbol(arg.precision)
            intrinsic_type = "integer" if arg in int_field_args else "real"
            suffix = const.ARG_TYPE_SUFFIX_MAPPING[arg.argument_type]
            if arg.vector_size > 1:
                for idx in range(1, arg.vector_size+1):
                    # Make sure we're going to create a Symbol with a unique
                    # name.
                    new_name = self._symbol_table.next_available_name(
                        f"{arg.name}_{idx}_{suffix}")
                    tag = f"{arg.name}_{idx}:{suffix}"
                    # The data for a field lives in a rank-1 array.
                    self._add_symbol(new_name, tag, intrinsic_type, arg, 1)
            else:
                # Make sure we're going to create a Symbol with a unique
                # name (since this is hardwired into the
                # UnsupportedFortranType).
                new_name = self._symbol_table.next_available_name(
                    f"{arg.name}_{suffix}")
                tag = f"{arg.name}:{suffix}"
                # The data for an operator lives in a rank-3 array.
                rank = 1 if arg not in op_args else 3
                self._add_symbol(new_name, tag, intrinsic_type, arg, rank)

    def _add_symbol(self, name, tag, intrinsic_type, arg, rank):
        '''
        Creates a new DataSymbol representing either an LFRic field or
        operator and adds it to the SymbolTable associated with this class.
        The Symbol is of UnsupportedFortranType because it is a pointer
        to the internal data array and the PSyIR does not support pointers. The
        remainder of the type information is fully supplied in the
        `partial_datatype` property of the UnsupportedFortranType.
        The supplied Symbol name is assumed not to already exist in the
        SymbolTable (e.g. it is obtained with the `next_available_name` method
        of SymbolTable) because it is used in constructing the
        UnsupportedFortranType which must be done before the Symbol is created.

        :param str name: the name of the new Symbol.
        :param str tag: the tag to associate with the new Symbol.
        :param str intrinsic_type: whether the Symbol represents "real" or
                                   "integer" data.
        :param arg: the metadata description of the associated kernel argument.
        :type arg: :py:class:`psyclone.dynamo0p3.DynKernelArgument`
        :param int rank: the rank of the array represented by the Symbol.

        '''
        if intrinsic_type == "real":
            lfric_type = "LFRicRealScalarDataType"
        else:
            lfric_type = "LFRicIntegerScalarDataType"
        precision = LFRicConstants().precision_for_type(arg.data_type)
        array_type = ArrayType(
                LFRicTypes(lfric_type)(precision),
                [ArrayType.Extent.DEFERRED]*rank)

        # Since the PSyIR doesn't have the pointer concept, we have
        # to have an UnsupportedFortranType.
        index_str = ",".join(rank*[":"])
        dtype = UnsupportedFortranType(
            f"{intrinsic_type}(kind={arg.precision}), pointer, "
            f"dimension({index_str}) :: {name} => null()",
            partial_datatype=array_type)
        try:
            self._symbol_table.new_symbol(name,
                                          symbol_type=DataSymbol,
                                          datatype=dtype,
                                          tag=tag)
        except KeyError:
            # The tag already exists and therefore we don't need to do
            # anything. This can happen if the Symbol Table has already
            # been populated by a previous call to this constructor. Even if
            # this is not the case, within a single Invoke we can have user-
            # supplied kernels that accept a full field-vector as argument
            # but also individual components of that vector might
            # be passed to Builtins. Therefore a clash with an
            # existing tag may occur which we can safely ignore.
            pass

    def _invoke_declarations(self, parent):
        '''
        Insert declarations of all proxy-related quantities into the PSy layer.

        :param parent: the node in the f2pygen AST representing the PSy- \
                       layer routine.
        :type parent: :py:class:`psyclone.f2pygen.SubroutineGen`

        '''
        const = LFRicConstants()
        const_mod = const.UTILITIES_MOD_MAP["constants"]["module"]
        table = self._symbol_table

        # Declarations of real and integer field proxies

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

            # Create declarations for the pointers to the internal
            # data arrays.
            for arg in args:
                (self._invoke.invokes.psy.infrastructure_modules[const_mod].
                 add(arg.precision))
                suffix = const.ARG_TYPE_SUFFIX_MAPPING[arg.argument_type]
                if arg.vector_size > 1:
                    entity_names = []
                    for idx in range(1, arg.vector_size+1):
                        ttext = f"{arg.name}_{idx}:{suffix}"
                        vsym = table.lookup_with_tag(ttext)
                        entity_names.append(vsym.name)
                else:
                    ttext = f"{arg.name}:{suffix}"
                    sym = table.lookup_with_tag(ttext)
                    entity_names = [sym.name]
                if entity_names:
                    parent.add(
                        DeclGen(
                            parent, datatype=arg.intrinsic_type,
                            kind=arg.precision, dimension=":",
                            entity_decls=[f"{name} => null()" for
                                          name in entity_names],
                            pointer=True))

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
            for arg in operators_list:
                name = arg.name
                suffix = const.ARG_TYPE_SUFFIX_MAPPING[arg.argument_type]
                ttext = f"{name}:{suffix}"
                sym = table.lookup_with_tag(ttext)
                # Declare the pointer to the stencil array.
                parent.add(DeclGen(parent, datatype="real",
                                   kind=arg.precision,
                                   dimension=":,:,:",
                                   entity_decls=[f"{sym.name} => null()"],
                                   pointer=True))
            op_mod = operators_list[0].module_name
            # Ensure the appropriate derived datatype will be imported.
            (self._invoke.invokes.psy.infrastructure_modules[op_mod].
             add(operator_datatype))
            # Ensure the appropriate kind parameter will be imported.
            (self._invoke.invokes.psy.infrastructure_modules[const_mod].
             add(arg.precision))

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

        :param parent: node in the f2pygen AST representing the PSy-layer
                       routine.
        :type parent: :py:class:`psyclone.f2pygen.SubroutineGen`

        :raises InternalError: if a kernel argument of an unrecognised type
            is encountered.

        '''
        parent.add(CommentGen(parent, ""))
        parent.add(CommentGen(parent,
                              " Initialise field and/or operator proxies"))
        parent.add(CommentGen(parent, ""))
        for arg in self._invoke.psy_unique_vars:
            # We don't have proxies for scalars
            if arg.is_scalar:
                continue

            const = LFRicConstants()
            suffix = const.ARG_TYPE_SUFFIX_MAPPING[arg.argument_type]

            if arg.vector_size > 1:
                # the range function below returns values from
                # 1 to the vector size which is what we
                # require in our Fortran code
                for idx in range(1, arg.vector_size+1):
                    parent.add(
                        AssignGen(parent,
                                  lhs=arg.proxy_name+"("+str(idx)+")",
                                  rhs=arg.name+"("+str(idx)+")%get_proxy()"))
                    name = self._symbol_table.lookup_with_tag(
                        f"{arg.name}_{idx}:{suffix}").name
                    parent.add(
                        AssignGen(parent,
                                  lhs=name,
                                  rhs=f"{arg.proxy_name}({idx})%data",
                                  pointer=True))
            else:
                parent.add(AssignGen(parent, lhs=arg.proxy_name,
                                     rhs=arg.name+"%get_proxy()"))
                if arg.is_field:
                    name = self._symbol_table.lookup_with_tag(
                        f"{arg.name}:{suffix}").name
                    parent.add(
                        AssignGen(parent,
                                  lhs=name,
                                  rhs=f"{arg.proxy_name}%data",
                                  pointer=True))
                elif arg.is_operator:
                    if arg.argument_type == "gh_columnwise_operator":
                        # CMA operator arguments are handled in DynCMAOperators
                        pass
                    elif arg.argument_type == "gh_operator":
                        name = self._symbol_table.lookup_with_tag(
                            f"{arg.name}:{suffix}").name
                        parent.add(
                            AssignGen(parent,
                                      lhs=name,
                                      rhs=f"{arg.proxy_name}%local_stencil",
                                      pointer=True))
                    else:
                        raise InternalError(
                            f"Kernel argument '{arg.name}' is a recognised "
                            f"operator but its type ('{arg.argument_type}') is"
                            f" not supported by DynProxies.initialise()")
                else:
                    raise InternalError(
                        f"Kernel argument '{arg.name}' of type "
                        f"'{arg.argument_type}' not "
                        f"handled in DynProxies.initialise()")


class DynCellIterators(LFRicCollection):
    '''
    Handles all entities required by kernels that operate on cell-columns.

    :param kern_or_invoke: the Kernel or Invoke for which to manage cell \
                           iterators.
    :type kern_or_invoke: :py:class:`psyclone.domain.lfric.LFRicKern` or \
                          :py:class:`psyclone.dynamo0p3.LFRicInvoke`

    : raises GenerationError: if an Invoke has no field or operator arguments.

    '''
    def __init__(self, kern_or_invoke):
        super().__init__(kern_or_invoke)

        self._nlayers_name = self._symbol_table.find_or_create_tag(
            "nlayers", symbol_type=LFRicTypes("MeshHeightDataSymbol")).name

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


class DynLMAOperators(LFRicCollection):
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


class DynCMAOperators(LFRicCollection):
    '''
    Holds all information on the Column-Matrix-Assembly operators
    required by an Invoke or Kernel stub.

    :param node: either an Invoke schedule or a single Kernel object.
    :type node: :py:class:`psyclone.dynamo0p3.DynSchedule` or \
                :py:class:`psyclone.domain.lfric.LFRicKern`

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
        super().__init__(node)

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

        # Create all the necessary Symbols here so that they are available
        # without the need to do a 'gen'.
        symtab = self._symbol_table
        const = LFRicConstants()
        suffix = const.ARG_TYPE_SUFFIX_MAPPING["gh_columnwise_operator"]
        for op_name in self._cma_ops:
            new_name = self._symbol_table.next_available_name(
                f"{op_name}_{suffix}")
            tag = f"{op_name}:{suffix}"
            arg = self._cma_ops[op_name]["arg"]
            precision = LFRicConstants().precision_for_type(arg.data_type)
            array_type = ArrayType(
                LFRicTypes("LFRicRealScalarDataType")(precision),
                [ArrayType.Extent.DEFERRED]*3)
            index_str = ",".join(3*[":"])
            dtype = UnsupportedFortranType(
                f"real(kind={arg.precision}), pointer, "
                f"dimension({index_str}) :: {new_name} => null()",
                partial_datatype=array_type)
            symtab.new_symbol(new_name,
                              symbol_type=DataSymbol,
                              datatype=dtype,
                              tag=tag)
            # Now the various integer parameters of the operator.
            for param in self._cma_ops[op_name]["params"]:
                symtab.find_or_create_integer_symbol(
                    f"{op_name}_{param}", tag=f"{op_name}:{param}:{suffix}")

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

        const = LFRicConstants()
        suffix = const.ARG_TYPE_SUFFIX_MAPPING["gh_columnwise_operator"]

        for op_name in self._cma_ops:
            # First, assign a pointer to the array containing the actual
            # matrix.
            cma_name = self._symbol_table.lookup_with_tag(
                f"{op_name}:{suffix}").name
            parent.add(AssignGen(parent, lhs=cma_name, pointer=True,
                                 rhs=self._cma_ops[op_name]["arg"].
                                 proxy_name_indexed+"%columnwise_matrix"))
            # Then make copies of the related integer parameters
            for param in self._cma_ops[op_name]["params"]:
                param_name = self._symbol_table.find_or_create_tag(
                    f"{op_name}:{param}:{suffix}").name
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

        const = LFRicConstants()
        suffix = const.ARG_TYPE_SUFFIX_MAPPING["gh_columnwise_operator"]
        for op_name in self._cma_ops:
            # Declare the operator matrix itself.
            tag_name = f"{op_name}:{suffix}"
            cma_name = self._symbol_table.lookup_with_tag(tag_name).name
            cma_dtype = self._cma_ops[op_name]["datatype"]
            cma_kind = self._cma_ops[op_name]["kind"]
            parent.add(DeclGen(parent, datatype=cma_dtype,
                               kind=cma_kind, pointer=True,
                               dimension=":,:,:",
                               entity_decls=[f"{cma_name} => null()"]))
            const = LFRicConstants()
            const_mod = const.UTILITIES_MOD_MAP["constants"]["module"]
            const_mod_uses = self._invoke.invokes.psy. \
                infrastructure_modules[const_mod]
            # Record that we will need to import the kind of this
            # cma operator from the appropriate infrastructure
            # module
            const_mod_uses.add(cma_kind)

            # Declare the associated integer parameters
            param_names = []
            for param in self._cma_ops[op_name]["params"]:
                name = f"{op_name}_{param}"
                tag = f"{op_name}:{param}:{suffix}"
                sym = self._symbol_table.find_or_create_integer_symbol(
                    name, tag=tag)
                param_names.append(sym.name)
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

        const = LFRicConstants()
        suffix = const.ARG_TYPE_SUFFIX_MAPPING["gh_columnwise_operator"]

        for op_name in self._cma_ops:
            # Declare the associated scalar arguments before the array because
            # some of them are used to dimension the latter (and some compilers
            # get upset if this ordering is not followed)
            _local_args = []
            for param in self._cma_ops[op_name]["params"]:
                param_name = symtab.find_or_create_tag(
                    f"{op_name}:{param}:{suffix}",
                    root_name=f"{op_name}_{param}").name
                _local_args.append(param_name)
            parent.add(DeclGen(parent, datatype="integer",
                               kind=api_config.default_kind["integer"],
                               intent="in", entity_decls=_local_args))
            # Declare the array that holds the CMA operator
            bandwidth = symtab.find_or_create_tag(
                f"{op_name}:bandwidth:{suffix}",
                root_name=f"{op_name}_bandwidth").name
            nrow = symtab.find_or_create_tag(
                f"{op_name}:nrow:{suffix}",
                root_name=f"{op_name}_nrow").name
            intent = self._cma_ops[op_name]["intent"]
            op_dtype = self._cma_ops[op_name]["datatype"]
            op_kind = self._cma_ops[op_name]["kind"]
            parent.add(DeclGen(parent, datatype=op_dtype, kind=op_kind,
                               dimension=",".join([bandwidth,
                                                   nrow, "ncell_2d"]),
                               intent=intent, entity_decls=[op_name]))


class DynMeshes():
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
    :type invoke: :py:class:`psyclone.dynamo0p3.LFRicInvoke`
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
            _name_set.add(f"mesh_{fine_arg.name}")
            _name_set.add(f"mesh_{coarse_arg.name}")

        # If we found a mixture of both inter-grid and non-inter-grid kernels
        # then we reject the invoke()
        if non_intergrid_kernels and self._ig_kernels:
            raise GenerationError(
                f"An invoke containing inter-grid kernels must contain no "
                f"other kernel types but kernels "
                f"'{''', '''.join([c.name for c in non_intergrid_kernels])}' "
                f"in invoke '{invoke.name}' are not inter-grid kernels.")

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
            datatype=UnresolvedType(),
            interface=ImportInterface(csym))

        name_list = []
        for name in mesh_tags:
            name_list.append(self._symbol_table.find_or_create_tag(
                name, symbol_type=DataSymbol, datatype=mtype_sym).name)

        if Config.get().distributed_memory:
            # If distributed memory is enabled then we require a variable
            # holding the maximum halo depth for each mesh.
            for name in mesh_tags:
                var_name = f"max_halo_depth_{name}"
                self._symbol_table.find_or_create_integer_symbol(
                    var_name, tag=var_name)

    def _colourmap_init(self):
        '''
        Sets-up information on any required colourmaps. This cannot be done
        in the constructor since colouring is applied by Transformations
        and happens after the Schedule has already been constructed. Therefore,
        this method is called at code-generation time.

        '''
        # pylint: disable=too-many-locals
        const = LFRicConstants()
        non_intergrid_kern = None
        sym_tab = self._schedule.symbol_table

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
                non_intergrid_kern = call
                continue

            # This is an inter-grid kernel so look-up the names of
            # the colourmap variables associated with the coarse
            # mesh (since that determines the iteration space).
            carg_name = self._ig_kernels[id(call)].coarse.name
            # Colour map
            base_name = "cmap_" + carg_name
            colour_map = sym_tab.find_or_create_array(
                base_name, 2, ScalarType.Intrinsic.INTEGER,
                tag=base_name)
            # No. of colours
            base_name = "ncolour_" + carg_name
            ncolours = sym_tab.find_or_create_integer_symbol(
                base_name, tag=base_name)
            # Array holding the last cell of a given colour.
            if (Config.get().distributed_memory and
                    not call.all_updates_are_writes):
                # This will require a loop into the halo and so the array is
                # 2D (indexed by colour *and* halo depth).
                base_name = "last_halo_cell_all_colours_" + carg_name
                last_cell = self._schedule.symbol_table.find_or_create_array(
                    base_name, 2, ScalarType.Intrinsic.INTEGER, tag=base_name)
            else:
                # Array holding the last edge cell of a given colour. Just 1D
                # as indexed by colour only.
                base_name = "last_edge_cell_all_colours_" + carg_name
                last_cell = self._schedule.symbol_table.find_or_create_array(
                    base_name, 1, ScalarType.Intrinsic.INTEGER, tag=base_name)
            # Add these symbols into the dictionary entry for this
            # inter-grid kernel
            self._ig_kernels[id(call)].set_colour_info(
                colour_map, ncolours, last_cell)

        if non_intergrid_kern and (self._needs_colourmap or
                                   self._needs_colourmap_halo):
            # There aren't any inter-grid kernels but we do need colourmap
            # information and that means we'll need a mesh object
            self._add_mesh_symbols(["mesh"])
            # This creates the colourmap information for this invoke if we
            # don't already have one.
            colour_map = non_intergrid_kern.colourmap
            # No. of colours
            ncolours = sym_tab.find_or_create_integer_symbol(
                "ncolour", tag="ncolour").name
            if self._needs_colourmap_halo:
                sym_tab.find_or_create_array(
                    "last_halo_cell_all_colours", 2,
                    ScalarType.Intrinsic.INTEGER,
                    tag="last_halo_cell_all_colours")
            if self._needs_colourmap:
                sym_tab.find_or_create_array(
                    "last_edge_cell_all_colours", 1,
                    ScalarType.Intrinsic.INTEGER,
                    tag="last_edge_cell_all_colours")

    def declarations(self, parent):
        '''
        Declare variables specific to mesh objects.

        :param parent: the parent node to which to add the declarations
        :type parent: :py:class:`psyclone.f2pygen.BaseGen`

        '''
        # pylint: disable=too-many-locals, too-many-statements
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
            if kern.colourmap_symbol:
                parent.add(
                    DeclGen(parent, datatype="integer",
                            kind=api_config.default_kind["integer"],
                            pointer=True,
                            entity_decls=[kern.colourmap_symbol.name+"(:,:)"]))
                parent.add(
                    DeclGen(parent, datatype="integer",
                            kind=api_config.default_kind["integer"],
                            entity_decls=[kern.ncolours_var_symbol.name]))
                # The cell-count array is 2D if we go into the halo and 1D
                # otherwise (i.e. no DM or this kernel is GH_WRITE only and
                # does not access the halo).
                dim_list = len(kern.last_cell_var_symbol.datatype.shape)*":"
                decln = (f"{kern.last_cell_var_symbol.name}("
                         f"{','.join(dim_list)})")
                parent.add(
                    DeclGen(parent, datatype="integer", allocatable=True,
                            kind=api_config.default_kind["integer"],
                            entity_decls=[decln]))

        if not self._ig_kernels and (self._needs_colourmap or
                                     self._needs_colourmap_halo):
            # There aren't any inter-grid kernels but we do need
            # colourmap information
            base_name = "cmap"
            csym = self._schedule.symbol_table.lookup_with_tag("cmap")
            colour_map = csym.name
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
        :type parent: :py:class:`psyclone.f2pygen.BaseGen`

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
                              rhs=f"{coarse_mesh}%get_mesh_map({fine_mesh})"))

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
            if dig.colourmap_symbol:
                # Number of colours
                parent.add(AssignGen(parent, lhs=dig.ncolours_var_symbol.name,
                                     rhs=coarse_mesh + "%get_ncolours()"))
                # Colour map itself
                parent.add(AssignGen(parent, lhs=dig.colourmap_symbol.name,
                                     pointer=True,
                                     rhs=coarse_mesh + "%get_colour_map()"))
                # Last halo/edge cell per colour.
                sym = dig.last_cell_var_symbol
                if len(sym.datatype.shape) == 2:
                    # Array is 2D so is a halo access.
                    name = "%get_last_halo_cell_all_colours()"
                else:
                    # Array is just 1D so go to the last edge cell.
                    name = "%get_last_edge_cell_all_colours()"
                parent.add(AssignGen(parent, lhs=sym.name,
                                     rhs=coarse_mesh + name))

    @property
    def intergrid_kernels(self):
        ''' Getter for the dictionary of intergrid kernels.

        :returns: Dictionary of intergrid kernels, indexed by name.
        :rtype: :py:class:`collections.OrderedDict`
        '''
        return self._ig_kernels


class DynInterGrid():
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
        symtab = self.coarse.call.ancestor(InvokeSchedule).symbol_table

        # Generate name for inter-mesh map
        base_mmap_name = f"mmap_{fine_arg.name}_{coarse_arg.name}"
        self.mmap = symtab.find_or_create_tag(base_mmap_name).name

        # Generate name for ncell variables
        name = f"ncell_{fine_arg.name}"
        self.ncell_fine = symtab.find_or_create_integer_symbol(
            name, tag=name).name
        # No. of fine cells per coarse cell in x
        name = f"ncpc_{fine_arg.name}_{coarse_arg.name}_x"
        self.ncellpercellx = symtab.find_or_create_integer_symbol(
            name, tag=name).name
        # No. of fine cells per coarse cell in y
        name = f"ncpc_{fine_arg.name}_{coarse_arg.name}_y"
        self.ncellpercelly = symtab.find_or_create_integer_symbol(
            name, tag=name).name
        # Name for cell map
        base_name = "cell_map_" + coarse_arg.name
        sym = symtab.find_or_create_array(base_name, 3,
                                          ScalarType.Intrinsic.INTEGER,
                                          tag=base_name)
        self.cell_map = sym.name

        # We have no colourmap information when first created
        self._colourmap_symbol = None
        # Symbol for the variable holding the number of colours
        self._ncolours_var_symbol = None
        # Symbol of the variable holding the last cell of a particular colour.
        # Will be a 2D array if the kernel iteration space includes the halo
        # and 1D otherwise.
        self._last_cell_var_symbol = None

    def set_colour_info(self, colour_map, ncolours, last_cell):
        '''Sets the colour_map, number of colours, and
        last cell of a particular colour.

        :param colour_map: the colour map symbol.
        :type: colour_map:py:class:`psyclone.psyir.symbols.Symbol`
        :param ncolours: the number of colours.
        :type: ncolours: :py:class:`psyclone.psyir.symbols.Symbol`
        :param last_cell: the last halo cell of a particular colour.
        :type last_cell: :py:class:`psyclone.psyir.symbols.Symbol`

        '''
        self._colourmap_symbol = colour_map
        self._ncolours_var_symbol = ncolours
        self._last_cell_var_symbol = last_cell

    @property
    def colourmap_symbol(self):
        ''':returns: the colour map symbol.
        :rtype: :py:class:`psyclone.psyir.symbols.Symbol`
        '''
        return self._colourmap_symbol

    @property
    def ncolours_var_symbol(self):
        ''':returns: the symbol for storing the number of colours.
        :rtype: :py:class:`psyclone.psyir.symbols.Symbol`
        '''
        return self._ncolours_var_symbol

    @property
    def last_cell_var_symbol(self):
        ''':returns: the last halo/edge cell variable.
        :rtype: :py:class:`psyclone.psyir.symbols.Symbol`
        '''
        return self._last_cell_var_symbol


class DynBasisFunctions(LFRicCollection):
    ''' Holds all information on the basis and differential basis
    functions required by an invoke or kernel call. This covers both those
    required for quadrature and for evaluators.

    :param node: either the schedule of an Invoke or a single Kernel object \
                 for which to extract information on all required \
                 basis/diff-basis functions.
    :type node: :py:class:`psyclone.dynamo0p3.DynInvokeSchedule` or \
                :py:class:`psyclone.domain.lfric.LFRicKern`

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

        super().__init__(node)

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
                f"Unsupported space for basis function, "
                f"expecting one of {const.VALID_FUNCTION_SPACES} but found "
                f"'{function_space.orig_name}'")
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
                f"Unsupported space for differential basis function, "
                f"expecting one of {const.VALID_FUNCTION_SPACES} but found "
                f"'{function_space.orig_name}'")
        return first_dim

    def _setup_basis_fns_for_call(self, call):
        '''
        Populates self._basis_fns with entries describing the basis
        functions required by the supplied Call.

        :param call: the kernel call for which basis functions are required.
        :type call: :py:class:`psyclone.domain.lfric.LFRicKern`

        :raises InternalError: if the supplied call is of incorrect type.
        :raises InternalError: if the supplied call has an unrecognised \
                               evaluator shape.
        '''
        if not isinstance(call, LFRicKern):
            raise InternalError(f"Expected a LFRicKern object but got: "
                                f"'{type(call)}'")
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
                    raise InternalError(f"Unrecognised evaluator shape: "
                                        f"'{shape}'. Should be one of "
                                        f"{const.VALID_EVALUATOR_SHAPES}")

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
                    f"Quadrature shapes other than {supported_shapes} are not "
                    f"yet supported - got: '{shape}'")

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
            const_mod_uses = self._invoke.invokes.psy. \
                infrastructure_modules[const_mod]
            # Record that we will need to import the kind for a
            # pointer declaration (associated with a function
            # space) from the appropriate infrastructure module
            const_mod_uses.add(my_kind)

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
                    f"Unrecognised type of basis function: "
                    f"'{basis_fn['''type''']}'. Should be either 'basis' or "
                    f"'diff-basis'.")

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
                    f"Unrecognised type of basis function: "
                    f"'{basis_fn['''type''']}'. Should be either 'basis' or "
                    f"'diff-basis'.")

            if self._invoke and first_dim not in var_dim_list:
                var_dim_list.append(first_dim)

            if basis_fn["shape"] in const.VALID_QUADRATURE_SHAPES:

                qr_var = basis_fn["qr_var"]
                if not qr_var:
                    raise InternalError(
                        f"Quadrature '{basis_fn['''shape''']}' is required but"
                        f" have no name for the associated Quadrature object.")

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
                    f"Unrecognised evaluator shape: '{basis_fn['''shape''']}'."
                    f" Should be one of {const.VALID_EVALUATOR_SHAPES}")

        return (var_dim_list, basis_arrays)

    def _initialise_xyz_qr(self, parent):
        '''
        Add in the initialisation of variables needed for XYZ
        quadrature

        :param parent: the node in the AST representing the PSy subroutine
                       in which to insert the initialisation
        :type parent: :py:class:`psyclone.f2pygen.SubroutineGen`

        '''
        # pylint: disable=unused-argument
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
            const_mod_uses = self._invoke.invokes.psy. \
                infrastructure_modules[const_mod]
            # Record that we will need to import the kind for a
            # declaration (associated with quadrature) from
            # the appropriate infrastructure module
            const_mod_uses.add(kind)

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
        # pylint: disable=unused-argument
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
                f"_initialise_face_or_edge_qr: qr_type argument must be "
                f"either 'face' or 'edge' but got: '{qr_type}'")

        quadrature_name = f"gh_quadrature_{qr_type}"

        if quadrature_name not in self._qr_vars:
            return

        api_config = Config.get().api_conf("dynamo0.3")
        symbol_table = self._symbol_table

        for qr_arg_name in self._qr_vars[quadrature_name]:
            # We generate unique names for the integers holding the numbers
            # of quadrature points by appending the name of the quadrature
            # argument
            decl_list = [
                symbol_table.find_or_create_integer_symbol(
                    name+"_"+qr_arg_name, tag=name+"_"+qr_arg_name).name
                for name in self.qr_dim_vars[qr_type]]
            parent.add(DeclGen(parent, datatype="integer",
                               kind=api_config.default_kind["integer"],
                               entity_decls=decl_list))

            names = [f"{name}_{qr_arg_name}"
                     for name in self.qr_weight_vars[qr_type]]
            decl_list = [
                symbol_table.find_or_create_array(name, 2,
                                                  ScalarType.Intrinsic.REAL,
                                                  tag=name).name
                + "(:,:) => null()" for name in names]
            const = LFRicConstants()
            datatype = const.QUADRATURE_TYPE_MAP[quadrature_name]["intrinsic"]
            kind = const.QUADRATURE_TYPE_MAP[quadrature_name]["kind"]
            parent.add(
                DeclGen(parent, datatype=datatype, pointer=True, kind=kind,
                        entity_decls=decl_list))
            const_mod = const.UTILITIES_MOD_MAP["constants"]["module"]
            const_mod_uses = self._invoke.invokes.psy. \
                infrastructure_modules[const_mod]
            # Record that we will need to import the kind for a
            # declaration (associated with quadrature) from the
            # appropriate infrastructure module
            const_mod_uses.add(kind)
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
                    f"Unrecognised type of basis function: "
                    f"'{basis_fn['''type''']}'. Expected one of 'basis' or "
                    f"'diff-basis'.")
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
                    rhs = (f"{basis_fn['arg'].proxy_name_indexed}%"
                           f"{basis_fn['arg'].ref_name(basis_fn['fspace'])}%"
                           f"call_function({basis_type},{dof_loop_var},nodes_"
                           f"{space.mangled_name}(:,{nodal_loop_var}))")
                    dof_loop.add(AssignGen(dof_loop, lhs=lhs, rhs=rhs))
            else:
                raise InternalError(
                    f"Unrecognised shape '{basis_fn['''shape''']}' specified "
                    f"for basis function. Should be one of: "
                    f"{const.VALID_EVALUATOR_SHAPES}")
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
                    f"Unrecognised type of basis function: "
                    f"'{basis_fn['''type''']}'. Should be one of 'basis' or "
                    f"'diff-basis'.")
            for fspace in basis_fn["nodal_fspaces"]:
                op_name = basis_fn["fspace"].\
                    get_operator_name(basis_name,
                                      qr_var=basis_fn["qr_var"],
                                      on_space=fspace)
                func_space_var_names.add(op_name)

        if func_space_var_names:
            # add the required deallocate call
            parent.add(DeallocateGen(parent, sorted(func_space_var_names)))


class DynBoundaryConditions(LFRicCollection):
    '''
    Manages declarations and initialisation of quantities required by
    kernels that need boundary condition information.

    :param node: the Invoke or Kernel stub for which we are to handle \
                 any boundary conditions.
    :type node: :py:class:`psyclone.dynamo0p3.LFRicInvoke` or \
                :py:class:`psyclone.domain.lfric.LFRicKern`

    :raises GenerationError: if a kernel named "enforce_bc_code" is found \
                             but does not have an argument on ANY_SPACE_1.
    :raises GenerationError: if a kernel named "enforce_operator_bc_code" is \
                             found but does not have exactly one argument.
    '''
    # Define a BoundaryDofs namedtuple to help us manage the arrays that
    # are required.
    BoundaryDofs = namedtuple("BoundaryDofs", ["argument", "function_space"])

    def __init__(self, node):
        super().__init__(node)

        self._boundary_dofs = []
        # Check through all the kernel calls to see whether any of them
        # require boundary conditions. Currently this is done by recognising
        # the kernel name.
        # pylint: disable=import-outside-toplevel
        from psyclone.domain.lfric.metadata_to_arguments_rules import (
            MetadataToArgumentsRules)
        for call in self._calls:
            if MetadataToArgumentsRules.bc_kern_regex.match(call.name):
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
                        f"The enforce_operator_bc_code kernel must have "
                        f"exactly one argument but found "
                        f"{len(call.arguments.args)}")
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
        super().__init__(name, LFRicKernCallFactory,
                         LFRicBuiltInCallFactory, arg, reserved_names,
                         parent=parent, symbol_table=LFRicSymbolTable())

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
                f"DynGlobalSum.init(): A global sum argument should be a "
                f"scalar but found argument of type '{scalar.argument_type}'.")
        # Check scalar intrinsic types that this class supports (only
        # "real" for now)
        if scalar.intrinsic_type != "real":
            raise GenerationError(
                f"DynGlobalSum currently only supports real scalars, but "
                f"argument '{scalar.name}' in Kernel '{scalar.call.name}' has "
                f"'{scalar.intrinsic_type}' intrinsic type.")
        # Initialise the parent class
        super().__init__(scalar, parent=parent)

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


class LFRicHaloExchange(HaloExchange):

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
        # dependent on this halo exchange
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
        # dependent on this halo exchange
        depth_info_list = self._compute_halo_read_depth_info()

        # if there is only one entry in the list we can just return
        # the depth
        if len(depth_info_list) == 1:
            return str(depth_info_list[0])
        # the depth information can't be reduced to a single
        # expression, therefore we need to determine the maximum
        # of all expressions
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
                    if isinstance(dep.call, LFRicHaloExchange)]
        if hex_deps:
            # There is a field accessed by a halo exchange that is
            # a read dependence. As ignore_hex_dep is True this should
            # be ignored so this is removed from the list.
            if any(dep for dep in hex_deps
                   if isinstance(dep.call, (LFRicHaloExchangeStart,
                                            LFRicHaloExchangeEnd))):
                raise GenerationError(
                    "Please perform redundant computation transformations "
                    "before asynchronous halo exchange transformations.")

            # There can only be one field accessed by a
            # halo exchange that is a read dependence.
            if len(hex_deps) != 1:
                raise InternalError(
                    f"There should only ever be at most one read dependency "
                    f"associated with a halo exchange in the read dependency "
                    f"list, but found {len(hex_deps)} for field "
                    f"{self.field.name}.")
            # For sanity, check that the field accessed by the halo
            # exchange is the last dependence in the list.
            if not isinstance(read_dependencies[-1].call, LFRicHaloExchange):
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
                f"Internal logic error. There should be at most one write "
                f"dependence for a halo exchange. Found "
                f"'{len(write_dependencies)}'")
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
        # literal depth. As the read information is aggregated, a known
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
            field_id += f"({self.vector_index})"
        return (f"{self.coloured_name(colour)}[field='{field_id}', "
                f"type='{self._compute_stencil_type()}', "
                f"depth={self._compute_halo_depth()}, "
                f"check_dirty={runtime_check}]")

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


class LFRicHaloExchangeStart(LFRicHaloExchange):
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
        LFRicHaloExchange.__init__(self, field, check_dirty=check_dirty,
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
        :rtype: :py:class:`psyclone.dynamo0p3.LFRicHaloExchangeEnd`
        :raises GenerationError: If no matching HaloExchangeEnd is \
        found, or if the first matching haloexchange that is found is \
        not a HaloExchangeEnd

        '''
        # Look at all nodes following this one in schedule order
        # (which is PSyIRe node order)
        for node in self.following():
            if self.sameParent(node) and isinstance(node, LFRicHaloExchange):
                # Found a following `haloexchange`,
                # `haloexchangestart` or `haloexchangeend` PSyIRe node
                # that is at the same calling hierarchy level as this
                # haloexchangestart
                access = DataAccess(self.field)
                if access.overlaps(node.field):
                    if isinstance(node, LFRicHaloExchangeEnd):
                        return node
                    raise GenerationError(
                        f"Halo exchange start for field '{self.field.name}' "
                        f"should match with a halo exchange end, but found "
                        f"{type(node)}")
        # no match has been found which is an error as a halo exchange
        # start should always have a matching halo exchange end that
        # follows it in schedule (PSyIRe sibling) order
        raise GenerationError(
            f"Halo exchange start for field '{self.field.name}' has no "
            f"matching halo exchange end")


class LFRicHaloExchangeEnd(LFRicHaloExchange):
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
        LFRicHaloExchange.__init__(self, field, check_dirty=check_dirty,
                                   vector_index=vector_index, parent=parent)
        # Update field properties appropriately. The associated field is
        # written to. However, a readwrite field access needs to be
        # specified as this is required for the halo exchange logic to
        # work correctly.
        self._field.access = AccessType.READWRITE
        # override appropriate parent class names
        self._halo_exchange_name = "halo_exchange_finish"


class HaloDepth():
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
        raise GenerationError(
            f"HaloInfo class expects an argument of type DynArgument, or "
            f"equivalent, on initialisation, but found, "
            f"'{type(field)}'") from err

    if field.access not in access_types:
        api_strings = [access.api_specific_name() for access in access_types]
        raise GenerationError(
            f"In HaloInfo class, field '{field.name}' should be one of "
            f"{api_strings}, but found '{field.access.api_specific_name()}'")
    if not isinstance(call, (LFRicBuiltIn, LFRicKern)):
        raise GenerationError(
            f"In HaloInfo class, field '{field.name}' should be from a call "
            f"but found {type(call)}")
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

        loop = call.ancestor(LFRicLoop)

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
                f"Internal error in HaloReadAccess._compute_from_field. Found "
                f"unexpected loop upper bound name '{loop.upper_bound_name}'")

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


class FSDescriptor():
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


class FSDescriptors():
    ''' Contains a collection of FSDescriptor objects and methods
    that provide information across these objects. We have one
    FSDescriptor for each meta-funcs entry in the kernel
    metadata.
    # TODO #274 this should actually be named something like
    BasisFuncDescriptors as it holds information describing the
    basis/diff-basis functions required by a kernel.

    :param descriptors: list of objects describing the basis/diff-basis \
                        functions required by a kernel, as obtained from \
                        metadata.
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
            f"FSDescriptors:get_descriptor: there is no descriptor for "
            f"function space {fspace.orig_name}")

    @property
    def descriptors(self):
        '''
        :return: the list of Descriptors, one for each of the meta-funcs
                 entries in the kernel metadata.
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
             do not match up with the kernel metadata
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
            f"error: expected '{expected_arg_count}' arguments in the "
            f"algorithm layer but found '{len(call.args)}'. Expected "
            f"'{len(call.ktype.arg_descriptors)}' standard arguments, "
            f"'{stencil_arg_count}' tencil arguments and '{qr_arg_count}' "
            f"qr_arguments'")


@dataclass(frozen=True)
class LFRicArgStencil:
    '''
    Provides stencil information about an LFRic kernel argument.
    LFRicArgStencil can provide the extent, algorithm argument for the extent,
    and the direction argument of a stencil or set any of these properties.

    :param name:            the name of the stencil.
    :param extent:          the extent of the stencil if it is known. It will
                            be known if it is specified in the metadata.
    :param extent_arg:      the algorithm argument associated with the extent
                            value if extent was not found in the metadata.
    :param direction_arg:   the direction argument associated with the
                            direction of the stencil if the direction of the
                            stencil is not known.
    '''
    name: str
    extent: str = None
    extent_arg: Any = None
    direction_arg: Any = None


class DynKernelArguments(Arguments):
    '''
    Provides information about Dynamo kernel call arguments
    collectively, as specified by the kernel argument metadata.

    :param call: the kernel metadata for which to extract argument info.
    :type call: :py:class:`psyclone.parse.KernelCall`
    :param parent_call: the kernel-call object.
    :type parent_call: :py:class:`psyclone.domain.lfric.LFRicKern`
    :param bool check: whether to check for consistency between the \
        kernel metadata and the algorithm layer. Defaults to True.

    :raises GenerationError: if the kernel metadata specifies stencil extent.
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
                if dyn_argument.descriptor.stencil['extent']:
                    raise GenerationError("extent metadata not yet supported")
                    # if supported we would add the following
                    # line: stencil.extent =
                    # dyn_argument.descriptor.stencil['extent']
                # An extent argument has been added.
                stencil_extent_arg = call.args[idx]
                idx += 1
                if dyn_argument.descriptor.stencil['type'] == 'xory1d':
                    # a direction argument has been added
                    stencil = LFRicArgStencil(
                        name=dyn_argument.descriptor.stencil['type'],
                        extent_arg=stencil_extent_arg,
                        direction_arg=call.args[idx]
                        )
                    idx += 1
                else:
                    # Create a stencil object and store a reference to it in
                    # our new DynKernelArgument object.
                    stencil = LFRicArgStencil(
                        name=dyn_argument.descriptor.stencil['type'],
                        extent_arg=stencil_extent_arg
                        )
                dyn_argument.stencil = stencil
            self._args.append(dyn_argument)

        # We have now completed the construction of the kernel arguments so
        # we can go back and update the names of any stencil size and/or
        # direction variable names to ensure there are no clashes.
        if self._parent_call:
            inv_sched = self._parent_call.ancestor(InvokeSchedule)
            if hasattr(inv_sched, "symbol_table"):
                symtab = inv_sched.symbol_table
            else:
                # This can happen in stub generation.
                symtab = LFRicSymbolTable()
        else:
            # TODO 719 The symtab is not connected to other parts of the
            # Stub generation.
            symtab = LFRicSymbolTable()
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
                    new_name = symtab.find_or_create_integer_symbol(
                        root, tag=tag).name
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
                                    in kernel metadata) for which to \
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
        raise FieldNotFoundError(f"DynKernelArguments:get_arg_on_space_name: "
                                 f"there is no field or operator with "
                                 f"function space {func_space_name}")

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

        raise FieldNotFoundError(f"DynKernelArguments:get_arg_on_space: there "
                                 f"is no field or operator with function space"
                                 f" {func_space.orig_name} (mangled name = "
                                 f"'{func_space.mangled_name}')")

    def has_operator(self, op_type=None):
        ''' Returns true if at least one of the arguments is an operator
        of type op_type (either gh_operator [LMA] or gh_columnwise_operator
        [CMA]). If op_type is None then searches for *any* valid operator
        type. '''
        const = LFRicConstants()
        if op_type and op_type not in const.VALID_OPERATOR_NAMES:
            raise GenerationError(
                f"If supplied, 'op_type' must be a valid operator type (one "
                f"of {const.VALID_OPERATOR_NAMES}) but got '{op_type}'")
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

    def psyir_expressions(self):
        '''
        :returns: the PSyIR expressions representing this Argument list.
        :rtype: list of :py:class:`psyclone.psyir.nodes.Node`

        '''
        create_arg_list = KernCallArgList(self._parent_call)
        create_arg_list.generate()
        return create_arg_list.psyir_arglist

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
    :param arg_meta_data: information obtained from the metadata for \
                          this kernel argument.
    :type arg_meta_data: :py:class:`psyclone.domain.lfric.LFRicArgDescriptor`
    :param arg_info: information on how this argument is specified in \
                     the Algorithm layer.
    :type arg_info: :py:class:`psyclone.parse.algorithm.Arg`
    :param call: the kernel object with which this argument is associated.
    :type call: :py:class:`psyclone.domain.lfric.LFRicKern`
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
            raise InternalError(
                f"DynKernelArgument.__init__(): Found unsupported data "
                f"type '{arg_meta_data.data_type}' in the kernel argument "
                f"descriptor '{arg_meta_data}'.") from err

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
                    f"DynKernelArgument.ref_name(fs): The supplied function "
                    f"space (fs='{function_space.orig_name}') is not one of "
                    f"the function spaces associated with this argument "
                    f"(fss={self.function_space_names}).")
        if self.is_field:
            return "vspace"
        if self.is_operator:
            if function_space.orig_name == self.descriptor.function_space_from:
                return "fs_from"
            if function_space.orig_name == self.descriptor.function_space_to:
                return "fs_to"
            raise GenerationError(
                f"DynKernelArgument.ref_name(fs): Function space "
                f"'{function_space.orig_name}' is one of the 'gh_operator' "
                f"function spaces '{self.function_spaces}' but is not being "
                f"returned by either function_space_from "
                f"'{self.descriptor.function_space_from}' or "
                f"function_space_to '{self.descriptor.function_space_to}'.")
        raise GenerationError(
            f"DynKernelArgument.ref_name(fs): Found unsupported argument "
            f"type '{self._argument_type}'.")

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
            # precision defined in the algorithm layer are
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
                # algorithm precision is either not available or is
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
            elif alg_datatype == "r_bl_field_type":
                argtype = "r_bl_field"
            elif alg_datatype == "r_phys_field_type":
                argtype = "r_phys_field"
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
            elif alg_datatype == "r_tran_operator_type":
                argtype = "r_tran_operator"
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

        :raises InternalError: if this argument is a literal but we fail to \
                               construct PSyIR that is consistent with this.
        :raises NotImplementedError: if this argument is not a literal, scalar
                                     or field.

        '''
        symbol_table = self._call.scope.symbol_table

        if self.is_literal:
            reader = FortranReader()
            if self.precision:
                # Ensure any associated precision symbol is in the table.
                symbol_table.add_lfric_precision_symbol(self.precision)
            lit = reader.psyir_from_expression(self.name, symbol_table)

            # Sanity check that the resulting expression is a literal.
            if lit.walk(Reference):
                raise InternalError(
                    f"Expected argument '{self.name}' to kernel "
                    f"'{self.call.name}' to be a literal but the created "
                    f"PSyIR contains one or more References.")
            return lit

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

        const = LFRicConstants()
        try:
            suffix = const.ARG_TYPE_SUFFIX_MAPPING[self.argument_type]
            tag_name = f"{self.name}:{suffix}"
            sym = symbol_table.lookup_with_tag(tag_name)
            return Reference(sym)

        except KeyError as err:
            raise NotImplementedError(
                f"Unsupported kernel argument type: '{self.name}' is of type "
                f"'{self.argument_type}' which is not recognised as being a "
                f"literal, scalar or field.") from err

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
            f"In the LFRic API the argument access must be one of "
            f"{valid_accesses}, but found '{self.access}'.")

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
        :rtype: :py:class:`psyclone.dynamo0p3.LFRicArgStencil`
        '''
        return self._stencil

    @stencil.setter
    def stencil(self, value):
        '''
        Sets stencil information for this kernel argument.

        :param value: stencil information for this argument.
        :type value: :py:class:`psyclone.dynamo0p3.LFRicArgStencil`

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
                    datatype=UnresolvedType(),
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
                mod_map = LFRicConstants().UTILITIES_MOD_MAP
                const_mod = mod_map["constants"]["module"]
                try:
                    constants_container = symbol_table.lookup(const_mod)
                except KeyError:
                    # TODO Once #696 is done, we should *always* have a
                    # symbol for this container at this point so should
                    # raise an exception if we haven't.
                    constants_container = LFRicTypes(const_mod)
                    root_table.add(constants_container)
                kind_symbol = DataSymbol(
                    kind_name, INTEGER_TYPE,
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
    'DynamoPSy',
    'DynDofmaps',
    'DynFunctionSpaces',
    'DynProxies',
    'DynCellIterators',
    'DynLMAOperators',
    'DynCMAOperators',
    'DynMeshes',
    'DynInterGrid',
    'DynBasisFunctions',
    'DynBoundaryConditions',
    'DynInvokeSchedule',
    'DynGlobalSum',
    'LFRicHaloExchange',
    'LFRicHaloExchangeStart',
    'LFRicHaloExchangeEnd',
    'HaloDepth',
    'HaloWriteAccess',
    'HaloReadAccess',
    'FSDescriptor',
    'FSDescriptors',
    'LFRicArgStencil',
    'DynKernelArguments',
    'DynKernelArgument',
    'DynACCEnterDataDirective']
