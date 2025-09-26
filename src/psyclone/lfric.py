# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017-2025, Science and Technology Facilities Council.
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

''' This module implements the PSyclone LFRic API by 1) specialising the
    required base classes in parser.py (KernelType) and
    adding a new class (LFRicFuncDescriptor) to capture function descriptor
    metadata and 2) specialising the required base classes in psyGen.py
    (PSy, Invokes, Invoke, InvokeSchedule, Loop, Kern, Inf, Arguments and
    Argument). '''

from __future__ import annotations
import os
from enum import Enum
from collections import OrderedDict, namedtuple
from dataclasses import dataclass
from typing import Any, List, Optional

from psyclone import psyGen
from psyclone.configuration import Config
from psyclone.core import AccessType, Signature, SymbolicMaths
from psyclone.domain.lfric.lfric_builtins import LFRicBuiltIn
from psyclone.domain.lfric import (
    FunctionSpace, KernCallAccArgList, KernCallArgList, LFRicCollection,
    LFRicConstants, LFRicSymbolTable, LFRicKern,
    LFRicTypes, LFRicLoop)
from psyclone.domain.lfric.lfric_invoke_schedule import LFRicInvokeSchedule
from psyclone.errors import GenerationError, InternalError, FieldNotFoundError
from psyclone.parse.kernel import getkerneldescriptors
from psyclone.parse.utils import ParseError
from psyclone.psyGen import (Arguments, DataAccess, InvokeSchedule, Kern,
                             KernelArgument, HaloExchange, GlobalSum)
from psyclone.psyir.frontend.fortran import FortranReader
from psyclone.psyir.nodes import (
    Reference, ACCEnterDataDirective, ArrayOfStructuresReference,
    StructureReference, Literal, IfBlock, Call, BinaryOperation, IntrinsicCall,
    Assignment, ArrayReference, Loop, Container, DataNode, Schedule, Node)
from psyclone.psyir.symbols import (
    INTEGER_TYPE, DataSymbol, DataType, DataTypeSymbol, ScalarType,
    UnresolvedType, ContainerSymbol, ImportInterface, StructureType,
    ArrayType, UnsupportedFortranType, ArgumentInterface)


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
            f"lfric.qr_basis_alloc_args(). Should be one of: "
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
            f"lfric.qr_basis_alloc_args(). Should be one of: "
            f"{const.VALID_QUADRATURE_SHAPES}")
    return alloc_args

# ---------- Classes -------------------------------------------------------- #


class LFRicFuncDescriptor():
    ''' The LFRic API includes a function-space descriptor as
    well as an argument descriptor which is not supported by the base
    classes. This class captures the information specified in a
    function-space descriptor. '''

    def __init__(self, func_type):
        self._func_type = func_type
        if func_type.name != 'func_type':
            raise ParseError(
                f"In the lfric API each meta_func entry must be of type "
                f"'func_type' but found '{func_type.name}'")
        if len(func_type.args) < 2:
            raise ParseError(
                f"In the lfric API each meta_func entry must have at "
                f"least 2 args, but found {len(func_type.args)}")
        self._operator_names = []
        const = LFRicConstants()
        for idx, arg in enumerate(func_type.args):
            if idx == 0:  # first func_type arg
                if arg.name not in const.VALID_FUNCTION_SPACE_NAMES:
                    raise ParseError(
                        f"In the LFRic API the 1st argument of a "
                        f"meta_func entry should be a valid function space "
                        f"name (one of {const.VALID_FUNCTION_SPACE_NAMES}), "
                        f"but found '{arg.name}' in '{func_type}'")
                self._function_space_name = arg.name
            else:  # subsequent func_type args
                if arg.name not in const.VALID_METAFUNC_NAMES:
                    raise ParseError(
                        f"In the lfric API, the 2nd argument and all "
                        f"subsequent arguments of a meta_func entry should "
                        f"be one of {const.VALID_METAFUNC_NAMES}, but found "
                        f"'{arg.name}' in '{func_type}'")
                if arg.name in self._operator_names:
                    raise ParseError(
                        f"In the lfric API, it is an error to specify an "
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
        return f"LFRicFuncDescriptor({self._func_type})"

    def __str__(self):
        res = "LFRicFuncDescriptor object" + os.linesep
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


class LFRicMeshProperties(LFRicCollection):
    '''
    Holds all information on the the mesh properties required by either an
    invoke or a kernel stub. Note that the creation of a suitable mesh
    object is handled in the `LFRicMeshes` class. This class merely deals with
    extracting the necessary properties from that object and providing them to
    kernels.

    :param node: kernel or invoke for which to manage mesh properties.
    :type node: :py:class:`psyclone.domain.lfric.LFRicKern` or \
                :py:class:`psyclone.domain.lfric.LFRicInvoke`

    '''
    def __init__(self, node):
        super().__init__(node)

        # The (ordered) list of mesh properties required by this invoke or
        # kernel stub.
        self._properties = []

        for call in self.kernel_calls:
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
            if prop == MeshProperty.ADJACENT_FACE:
                # If it's adjacent face, make it a pointer array
                self.symtab.find_or_create(
                    name_lower, symbol_type=DataSymbol,
                    datatype=UnsupportedFortranType(
                        "integer(kind=i_def), pointer :: adjacent_face(:,:) "
                        "=> null()",
                        partial_datatype=ArrayType(
                            LFRicTypes("LFRicIntegerScalarDataType")(),
                            [ArrayType.Extent.DEFERRED]*2)
                    ),
                    tag=name_lower)
            else:
                # Everything else is an integer
                self.symtab.find_or_create(
                    name_lower, tag=name_lower,
                    symbol_type=DataSymbol,
                    datatype=LFRicTypes("LFRicIntegerScalarDataType")())

    def kern_args(self, stub=False, var_accesses=None,
                  kern_call_arg_list=None):
        # pylint: disable=too-many-locals, too-many-branches
        '''
        Provides the list of kernel arguments associated with the mesh
        properties that the kernel requires. Optionally adds variable
        access information if var_accesses is given.

        :param bool stub: whether or not we are generating code for a \
                          kernel stub.
        :param var_accesses: optional VariablesAccessMap instance to store \
            the information about variable accesses.
        :type var_accesses: \
            :py:class:`psyclone.core.VariablesAccessMap`
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
                        lisdt = LFRicTypes("LFRicIntegerScalarDataType")()
                        name = self.symtab.\
                            find_or_create(
                                "nfaces_re_h", tag="nfaces_re_h",
                                symbol_type=DataSymbol,
                                datatype=lisdt
                            ).name
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
                                               [":", cell_ref])
                    # Update the name in case there was a clash
                    adj_face = adj_face_sym.name
                    if var_accesses:
                        var_accesses.add_access(Signature(adj_face),
                                                AccessType.READ, self._kernel,
                                                [":", cell_ref])

                if not stub:
                    adj_face = self.symtab.find_or_create_tag(
                        "adjacent_face").name
                    cell_name = "cell"
                    if self._kernel.is_coloured():
                        colour_name = "colour"
                        cmap_name = self.symtab.find_or_create_tag(
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

    def invoke_declarations(self):
        '''
        Creates the necessary declarations for variables needed in order to
        provide mesh properties to a kernel call.

        :raises InternalError: if an unsupported mesh property is found.

        '''
        super().invoke_declarations()
        for prop in self._properties:
            # The LFRicMeshes class will have created a mesh object so we
            # don't need to do that here.
            if prop == MeshProperty.ADJACENT_FACE:
                self.symtab.lookup_with_tag("adjacent_face")
            elif prop == MeshProperty.NCELL_2D_NO_HALOS:
                self.symtab.find_or_create(
                    "ncell_2d_no_halos",
                    symbol_type=DataSymbol,
                    datatype=LFRicTypes("LFRicIntegerScalarDataType")(),
                    tag="ncell_2d_no_halos")
            elif prop == MeshProperty.NCELL_2D:
                self.symtab.find_or_create(
                    "ncell_2d", tag="ncell_2d",
                    symbol_type=DataSymbol,
                    datatype=LFRicTypes("LFRicIntegerScalarDataType")()
                )
            else:
                raise InternalError(
                    f"Found unsupported mesh property '{prop}' when generating"
                    f" invoke declarations. Only members of the MeshProperty "
                    f"Enum are permitted ({list(MeshProperty)}).")

    def stub_declarations(self):
        '''
        Creates the necessary declarations for the variables needed in order
        to provide properties of the mesh in a kernel stub.
        Note that argument order is redefined later by ArgOrdering.

        :raises InternalError: if an unsupported mesh property is encountered.

        '''
        super().stub_declarations()
        for prop in self._properties:
            if prop == MeshProperty.ADJACENT_FACE:
                adj_face = self.symtab.lookup("adjacent_face")
                dimension = self.symtab.lookup("nfaces_re_h")
                adj_face.datatype = ArrayType(
                            LFRicTypes("LFRicIntegerScalarDataType")(),
                            [Reference(dimension)])
                adj_face.interface = ArgumentInterface(
                                            ArgumentInterface.Access.READ)
                self.symtab.append_argument(adj_face)
            elif prop == MeshProperty.NCELL_2D:
                ncell_2d = self.symtab.lookup("ncell_2d")
                ncell_2d.interface = ArgumentInterface(
                                            ArgumentInterface.Access.READ)
                self.symtab.append_argument(ncell_2d)
            else:
                raise InternalError(
                    f"Found unsupported mesh property '{prop}' when generating"
                    f" declarations for kernel stub. Only members of the "
                    f"MeshProperty Enum are permitted ({list(MeshProperty)})")

    def initialise(self, cursor: int) -> int:
        '''
        Creates the PSyIR nodes for the initialisation of properties of
        the mesh.

        :param cursor: position where to add the next initialisation
            statements.
        :returns: Updated cursor value.

        :raises InternalError: if an unsupported mesh property is encountered.

        '''
        const = LFRicConstants()
        # Since colouring is applied via transformations, we have to check for
        # it now, rather than when this class was first constructed.
        need_colour_limits = False
        need_colour_halo_limits = False
        need_tilecolour_limits = False
        need_tilecolour_halo_limits = False
        for call in self.kernel_calls:
            if call.is_coloured() and not call.is_intergrid:
                loop = call.parent.parent
                is_tiled = loop.loop_type == "cells_in_tile"
                # Record which colour maps will be needed
                if is_tiled:
                    has_halo = (loop.parent.parent.upper_bound_name in
                                const.HALO_ACCESS_LOOP_BOUNDS)
                    if has_halo:
                        need_tilecolour_halo_limits = True
                    else:
                        need_tilecolour_limits = True
                else:
                    has_halo = (loop.upper_bound_name in
                                const.HALO_ACCESS_LOOP_BOUNDS)
                    if has_halo:
                        need_colour_halo_limits = True
                    else:
                        need_colour_limits = True

        needs_colour_maps = (need_colour_limits or
                             need_colour_halo_limits or
                             need_tilecolour_limits or
                             need_tilecolour_halo_limits)

        if not self._properties and not needs_colour_maps:
            # If no mesh properties are required and there's no colouring
            # (which requires a mesh object to lookup loop bounds) then we
            # need do nothing.
            return cursor

        mesh = self.symtab.lookup_with_tag("mesh")

        init_cursor = cursor
        for prop in self._properties:
            if prop == MeshProperty.ADJACENT_FACE:
                adj_face = self.symtab.find_or_create_tag(
                    "adjacent_face")
                assignment = Assignment.create(
                        lhs=Reference(adj_face),
                        rhs=Call.create(StructureReference.create(
                            mesh, ["get_adjacent_face"])),
                        is_pointer=True)
                self._invoke.schedule.addchild(assignment, cursor)
                cursor += 1

            elif prop == MeshProperty.NCELL_2D_NO_HALOS:
                symbol = self.symtab.find_or_create(
                    "ncell_2d_no_halos", tag="ncell_2d_no_halos",
                    symbol_type=DataSymbol,
                    datatype=LFRicTypes("LFRicIntegerScalarDataType")()
                )
                assignment = Assignment.create(
                        lhs=Reference(symbol),
                        rhs=Call.create(StructureReference.create(
                            mesh, ["get_last_edge_cell"])),)
                self._invoke.schedule.addchild(assignment, cursor)
                cursor += 1

            elif prop == MeshProperty.NCELL_2D:
                symbol = self.symtab.find_or_create(
                    "ncell_2d", tag="ncell_2d",
                    symbol_type=DataSymbol,
                    datatype=LFRicTypes("LFRicIntegerScalarDataType")()
                )
                assignment = Assignment.create(
                        lhs=Reference(symbol),
                        rhs=Call.create(StructureReference.create(
                            mesh, ["get_ncells_2d"])),)
                self._invoke.schedule.addchild(assignment, cursor)
                cursor += 1
            else:
                raise InternalError(
                    f"Found unsupported mesh property '{str(prop)}' when "
                    f"generating initialisation code. Only members of the "
                    f"MeshProperty Enum are permitted ({list(MeshProperty)})")
        self._invoke.schedule[init_cursor].append_preceding_comment(
            "Initialise mesh properties")

        if need_colour_halo_limits:
            lhs = self.symtab.find_or_create_tag(
                "last_halo_cell_all_colours")
            assignment = Assignment.create(
                    lhs=Reference(lhs),
                    rhs=Call.create(StructureReference.create(
                        mesh, ["get_last_halo_cell_all_colours"])))
            self._invoke.schedule.addchild(assignment, cursor)
            cursor += 1
        if need_colour_limits:
            lhs = self.symtab.find_or_create_tag(
                "last_edge_cell_all_colours")
            assignment = Assignment.create(
                    lhs=Reference(lhs),
                    rhs=Call.create(StructureReference.create(
                        mesh, ["get_last_edge_cell_all_colours"])))
            self._invoke.schedule.addchild(assignment, cursor)
            cursor += 1
        if need_tilecolour_halo_limits:
            lhs = self.symtab.find_or_create_tag(
                "last_halo_tile_per_colour")
            assignment = Assignment.create(
                    lhs=Reference(lhs),
                    rhs=Call.create(StructureReference.create(
                        mesh, ["get_last_halo_tile_all_colours"])))
            self._invoke.schedule.addchild(assignment, cursor)
            cursor += 1
            lhs = self.symtab.find_or_create_tag(
                "last_halo_cell_per_colour_and_tile")
            assignment = Assignment.create(
                    lhs=Reference(lhs),
                    rhs=Call.create(StructureReference.create(
                        mesh, ["get_last_halo_cell_all_colours_all_tiles"])))
            self._invoke.schedule.addchild(assignment, cursor)
            cursor += 1
        if need_tilecolour_limits:
            lhs = self.symtab.find_or_create_tag(
                "last_edge_tile_per_colour")
            assignment = Assignment.create(
                    lhs=Reference(lhs),
                    rhs=Call.create(StructureReference.create(
                        mesh, ["get_last_edge_tile_all_colours"])))
            self._invoke.schedule.addchild(assignment, cursor)
            cursor += 1
            lhs = self.symtab.find_or_create_tag(
                "last_edge_cell_per_colour_and_tile")
            assignment = Assignment.create(
                    lhs=Reference(lhs),
                    rhs=Call.create(StructureReference.create(
                        mesh, ["get_last_edge_cell_all_colours_all_tiles"])))
            self._invoke.schedule.addchild(assignment, cursor)
            cursor += 1
        return cursor


class LFRicReferenceElement(LFRicCollection):
    '''
    Holds all information on the properties of the Reference Element
    required by an Invoke or a Kernel stub.

    :param node: Kernel or Invoke for which to manage Reference-Element \
                 properties.
    :type node: :py:class:`psyclone.domain.lfric.LFRicKern` or \
                :py:class:`psyclone.lfric.LFRicInvoke`

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

        for call in self.kernel_calls:
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

        symtab = self.symtab

        # Create and store symbol for the reference element object
        self._ref_elem_symbol = None

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
            self._nfaces_h_symbol = symtab.find_or_create(
                "nfaces_re_h", tag="nfaces_re_h", symbol_type=DataSymbol,
                datatype=LFRicTypes("LFRicIntegerScalarDataType")())
        # Provide no. of vertical faces if required
        if (RefElementMetaData.Property.NORMALS_TO_VERTICAL_FACES
                in self._properties or
                RefElementMetaData.Property.OUTWARD_NORMALS_TO_VERTICAL_FACES
                in self._properties):
            self._nfaces_v_symbol = symtab.find_or_create(
                "nfaces_re_v", tag="nfaces_re_v", symbol_type=DataSymbol,
                datatype=LFRicTypes("LFRicIntegerScalarDataType")())
        # Provide no. of all faces if required
        if (RefElementMetaData.Property.NORMALS_TO_FACES
                in self._properties or
                RefElementMetaData.Property.OUTWARD_NORMALS_TO_FACES
                in self._properties):
            self._nfaces_symbol = symtab.find_or_create(
                "nfaces_re", tag="nfaces_re", symbol_type=DataSymbol,
                datatype=LFRicTypes("LFRicIntegerScalarDataType")())

        # Now the arrays themselves, in the order specified in the
        # kernel metadata (in the case of a kernel stub)
        for prop in self._properties:
            # Provide horizontal normals to faces
            if prop == RefElementMetaData.Property.NORMALS_TO_HORIZONTAL_FACES:
                name = "normals_to_horiz_faces"
                self._horiz_face_normals_symbol = \
                    symtab.find_or_create(
                        name, symbol_type=DataSymbol,
                        datatype=ArrayType(
                                LFRicTypes("LFRicRealScalarDataType")(),
                                [ArrayType.Extent.DEFERRED]*2),
                        tag=name)
                if self._horiz_face_normals_symbol not in self._arg_properties:
                    self._arg_properties[self._horiz_face_normals_symbol] = \
                         self._nfaces_h_symbol
            # Provide horizontal normals to "outward" faces
            elif prop == (RefElementMetaData.Property.
                          OUTWARD_NORMALS_TO_HORIZONTAL_FACES):
                name = "out_normals_to_horiz_faces"
                self._horiz_face_out_normals_symbol = \
                    symtab.find_or_create(
                        name, symbol_type=DataSymbol,
                        datatype=ArrayType(
                                LFRicTypes("LFRicRealScalarDataType")(),
                                [ArrayType.Extent.DEFERRED]*2),
                        tag=name)
                if self._horiz_face_out_normals_symbol not in \
                        self._arg_properties:
                    self._arg_properties[self._horiz_face_out_normals_symbol] \
                        = self._nfaces_h_symbol
            elif prop == (RefElementMetaData.Property.
                          NORMALS_TO_VERTICAL_FACES):
                name = "normals_to_vert_faces"
                self._vert_face_normals_symbol = \
                    symtab.find_or_create(
                        name, symbol_type=DataSymbol,
                        datatype=ArrayType(
                                LFRicTypes("LFRicRealScalarDataType")(),
                                [ArrayType.Extent.DEFERRED]*2),
                        tag=name)
                if self._vert_face_normals_symbol not in self._arg_properties:
                    self._arg_properties[self._vert_face_normals_symbol] = \
                         self._nfaces_v_symbol
            # Provide vertical normals to "outward" faces
            elif prop == (RefElementMetaData.Property.
                          OUTWARD_NORMALS_TO_VERTICAL_FACES):
                name = "out_normals_to_vert_faces"
                self._vert_face_out_normals_symbol = \
                    symtab.find_or_create(
                        name, symbol_type=DataSymbol,
                        datatype=ArrayType(
                                LFRicTypes("LFRicRealScalarDataType")(),
                                [ArrayType.Extent.DEFERRED]*2),
                        tag=name)
                if self._vert_face_out_normals_symbol not in \
                        self._arg_properties:
                    self._arg_properties[self._vert_face_out_normals_symbol] \
                        = self._nfaces_v_symbol
            # Provide normals to all faces
            elif prop == RefElementMetaData.Property.NORMALS_TO_FACES:
                name = "normals_to_faces"
                self._face_normals_symbol = \
                    symtab.find_or_create(
                        name, symbol_type=DataSymbol,
                        datatype=ArrayType(
                                LFRicTypes("LFRicRealScalarDataType")(),
                                [ArrayType.Extent.DEFERRED]*2),
                        tag=name)
                if self._face_normals_symbol not in self._arg_properties:
                    self._arg_properties[self._face_normals_symbol] = \
                        self._nfaces_symbol
            # Provide vertical normals to all "outward" faces
            elif prop == RefElementMetaData.Property.OUTWARD_NORMALS_TO_FACES:
                name = "out_normals_to_faces"
                self._face_out_normals_symbol = \
                    symtab.find_or_create(
                        name, symbol_type=DataSymbol,
                        datatype=ArrayType(
                                LFRicTypes("LFRicRealScalarDataType")(),
                                [ArrayType.Extent.DEFERRED]*2),
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

    def invoke_declarations(self):
        '''
        Create the necessary declarations for the variables needed in order
        to provide properties of the reference element in a Kernel call.

        '''
        super().invoke_declarations()
        if not self._properties and not self._nfaces_h_required:
            # No reference-element properties required
            return

        const = LFRicConstants()

        refelem_type = const.REFELEMENT_TYPE_MAP["refelement"]["type"]
        refelem_mod = const.REFELEMENT_TYPE_MAP["refelement"]["module"]
        mod = ContainerSymbol(refelem_mod)
        self.symtab.add(mod)
        self.symtab.add(
            DataTypeSymbol(refelem_type, datatype=StructureType(),
                           interface=ImportInterface(mod)))
        self._ref_elem_symbol = self.symtab.find_or_create_tag(
                                                    "reference_element")
        self._ref_elem_symbol.specialise(
               DataSymbol,
               datatype=UnsupportedFortranType(
                   f"class({refelem_type}), pointer :: "
                   f"{self._ref_elem_symbol.name} => null()"))

    def stub_declarations(self):
        '''
        Create the necessary declarations for the variables needed in order
        to provide properties of the reference element in a Kernel stub.
        Note that argument order is redefined later by ArgOrdering.

        '''
        super().stub_declarations()
        if not (self._properties or self._nfaces_h_required):
            return

        # Declare the necessary scalars (duplicates are ignored)
        scalars = list(self._arg_properties.values())
        nfaces_h = self.symtab.find_or_create(
            "nfaces_re_h", tag="nfaces_re_h",
            symbol_type=DataSymbol,
            datatype=LFRicTypes("LFRicIntegerScalarDataType")()
        )
        if self._nfaces_h_required and nfaces_h not in scalars:
            scalars.append(nfaces_h)

        for nface in scalars:
            sym = self.symtab.find_or_create(
                nface.name,
                symbol_type=DataSymbol,
                datatype=LFRicTypes("LFRicIntegerScalarDataType")()
            )
            sym.interface = ArgumentInterface(ArgumentInterface.Access.READ)
            self.symtab.append_argument(sym)

        # Declare the necessary arrays
        for arr, sym in self._arg_properties.items():
            arrsym = self.symtab.lookup(arr.name)
            arrsym.datatype = ArrayType(
                    LFRicTypes("LFRicRealScalarDataType")(),
                    [Literal("3", INTEGER_TYPE), Reference(sym)])
            arrsym.interface = ArgumentInterface(
                                    ArgumentInterface.Access.READ)
            self.symtab.append_argument(arrsym)

    def initialise(self, cursor: int) -> int:
        '''
        Creates the PSyIR nodes representing the necessary initialisation
        code for properties of the reference element.

        :param cursor: position where to add the next initialisation
            statements.

        :returns: Updated cursor value.

        '''
        if not (self._properties or self._nfaces_h_required):
            return cursor

        mesh_obj = self.symtab.find_or_create_tag("mesh")
        ref_element = self._ref_elem_symbol
        stmt = Assignment.create(
                lhs=Reference(ref_element),
                rhs=Call.create(
                    StructureReference.create(
                        mesh_obj, ["get_reference_element"])),
                is_pointer=True)
        stmt.preceding_comment = (
            "Get the reference element and query its properties"
        )
        self._invoke.schedule.addchild(stmt, cursor)
        cursor += 1

        if self._nfaces_h_symbol:
            stmt = Assignment.create(
                    lhs=Reference(self._nfaces_h_symbol),
                    rhs=Call.create(
                        StructureReference.create(
                            ref_element, ["get_number_horizontal_faces"])))
            self._invoke.schedule.addchild(stmt, cursor)
            cursor += 1
        if self._nfaces_v_symbol:
            stmt = Assignment.create(
                    lhs=Reference(self._nfaces_v_symbol),
                    rhs=Call.create(
                        StructureReference.create(
                            ref_element, ["get_number_vertical_faces"])))
            self._invoke.schedule.addchild(stmt, cursor)
            cursor += 1

        if self._nfaces_symbol:
            stmt = Assignment.create(
                    lhs=Reference(self._nfaces_symbol),
                    rhs=Call.create(
                        StructureReference.create(
                            ref_element, ["get_number_faces"])))
            self._invoke.schedule.addchild(stmt, cursor)
            cursor += 1

        if self._horiz_face_normals_symbol:
            stmt = Call.create(
                StructureReference.create(
                    ref_element, ["get_normals_to_horizontal_faces"]))
            stmt.addchild(Reference(self._horiz_face_normals_symbol))
            self._invoke.schedule.addchild(stmt, cursor)
            cursor += 1

        if self._horiz_face_out_normals_symbol:
            stmt = Call.create(
                StructureReference.create(
                    ref_element,
                    ["get_outward_normals_to_horizontal_faces"]))
            stmt.addchild(Reference(self._horiz_face_out_normals_symbol))
            self._invoke.schedule.addchild(stmt, cursor)
            cursor += 1

        if self._vert_face_normals_symbol:
            stmt = Call.create(
                StructureReference.create(
                    ref_element,
                    ["get_normals_to_vertical_faces"]))
            stmt.addchild(Reference(self._vert_face_normals_symbol))
            self._invoke.schedule.addchild(stmt, cursor)
            cursor += 1

        if self._vert_face_out_normals_symbol:
            stmt = Call.create(
                StructureReference.create(
                    ref_element,
                    ["get_outward_normals_to_vertical_faces"]))
            stmt.addchild(Reference(self._vert_face_out_normals_symbol))
            self._invoke.schedule.addchild(stmt, cursor)
            cursor += 1

        if self._face_normals_symbol:
            stmt = Call.create(
                StructureReference.create(
                    ref_element,
                    ["get_normals_to_faces"]))
            stmt.addchild(Reference(self._face_normals_symbol))
            self._invoke.schedule.addchild(stmt, cursor)
            cursor += 1

        if self._face_out_normals_symbol:
            stmt = Call.create(
                StructureReference.create(
                    ref_element,
                    ["get_outward_normals_to_faces"]))
            stmt.addchild(Reference(self._face_out_normals_symbol))
            self._invoke.schedule.addchild(stmt, cursor)
            cursor += 1
        return cursor


class LFRicFunctionSpaces(LFRicCollection):
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
            self._function_spaces = self.kernel_calls[0].arguments.unique_fss

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

    def stub_declarations(self):
        '''
        Add function-space-related declarations to a Kernel stub.
        Note that argument order is redefined later by ArgOrdering.

        '''
        super().stub_declarations()
        for var in self._var_list:
            arg = self.symtab.find_or_create(
                var, symbol_type=DataSymbol,
                datatype=LFRicTypes("LFRicIntegerScalarDataType")())
            arg.interface = ArgumentInterface(ArgumentInterface.Access.READ)
            self.symtab.append_argument(arg)

    def invoke_declarations(self):
        '''
        Add function-space-related declarations to a PSy-layer routine.

        '''
        super().invoke_declarations()
        for var in self._var_list:
            self.symtab.new_symbol(
                var,
                symbol_type=DataSymbol,
                datatype=LFRicTypes("LFRicIntegerScalarDataType")())

    def initialise(self, cursor: int) -> int:
        '''
        Create the code that initialises function-space quantities.

        :param cursor: position where to add the next initialisation
            statements.

        :returns: Updated cursor value.

        '''
        # Loop over all unique function spaces used by the kernels in
        # the invoke
        for function_space in self._function_spaces:
            # Initialise information associated with this function space.
            # If we have 1+ kernels that operate on cell-columns then we
            # will need ndf and undf. If we don't then we only need undf
            # (for the upper bound of the loop over dofs) if we're not
            # doing DM.

            # Find argument proxy name used to dereference the argument
            arg = self._invoke.arg_for_funcspace(function_space)
            # Initialise ndf for this function space.
            if not self._dofs_only:
                ndf_name = function_space.ndf_name
                assignment = Assignment.create(
                        lhs=Reference(self.symtab.lookup(ndf_name)),
                        rhs=arg.generate_method_call(
                              "get_ndf", function_space=function_space))
                assignment.preceding_comment = (
                    f"Initialise number of DoFs for "
                    f"{function_space.mangled_name}")
                self._invoke.schedule.addchild(assignment, cursor)
                cursor += 1
            # If there is a field on this space then initialise undf
            # for this function space. However, if the invoke contains
            # only kernels that operate on dofs and distributed
            # memory is enabled then the number of dofs is obtained
            # from the field proxy and undf is not required.
            if not (self._dofs_only and Config.get().distributed_memory):
                if self._invoke.field_on_space(function_space):
                    undf_name = function_space.undf_name
                    self._invoke.schedule.addchild(
                        Assignment.create(
                            lhs=Reference(self.symtab.lookup(undf_name)),
                            rhs=arg.generate_method_call("get_undf")),
                        cursor)
                    cursor += 1
        return cursor


class LFRicProxies(LFRicCollection):
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
                # CMA operators are handled by the LFRicCMAOperators class.
                continue
            ctable.add_lfric_precision_symbol(arg.precision)
            intrinsic_type = "integer" if arg in int_field_args else "real"
            suffix = const.ARG_TYPE_SUFFIX_MAPPING[arg.argument_type]
            if arg.vector_size > 1:
                for idx in range(1, arg.vector_size+1):
                    # Make sure we're going to create a Symbol with a unique
                    # name.
                    new_name = self.symtab.next_available_name(
                        f"{arg.name}_{idx}_{suffix}")
                    tag = f"{arg.name}_{idx}:{suffix}"
                    # The data for a field lives in a rank-1 array.
                    self._add_symbol(new_name, tag, intrinsic_type, arg, 1)
            else:
                # Make sure we're going to create a Symbol with a unique
                # name (since this is hardwired into the
                # UnsupportedFortranType).
                tag = f"{arg.name}:{suffix}"
                new_name = self.symtab.next_available_name(
                    f"{arg.name}_{suffix}")
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
        :type arg: :py:class:`psyclone.lfric.LFRicKernelArgument`
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
            self.symtab.new_symbol(name,
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

    def invoke_declarations(self):
        '''
        Insert declarations of all proxy-related quantities into the PSy layer.

        '''
        super().invoke_declarations()
        const = LFRicConstants()
        table = self.symtab

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
            fld_mod_symbol = table.node.parent.symbol_table.lookup(fld_mod)
            fld_type_sym = table.node.parent.symbol_table.new_symbol(
                    fld_type,
                    symbol_type=DataTypeSymbol,
                    datatype=UnresolvedType(),
                    interface=ImportInterface(fld_mod_symbol))
            for arg in args:
                if arg._vector_size > 1:
                    decl_type = ArrayType(fld_type_sym, [arg._vector_size])
                else:
                    decl_type = fld_type_sym
                table.new_symbol(arg.proxy_name,
                                 symbol_type=DataSymbol,
                                 datatype=decl_type)

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
            mod_name = operators_list[0].module_name
            mod_st = table.node.parent.symbol_table
            fld_mod_symbol = mod_st.lookup(mod_name)
            op_datatype_symbol = mod_st.find_or_create(
                    operator_datatype,
                    symbol_type=DataTypeSymbol,
                    datatype=UnresolvedType(),
                    interface=ImportInterface(fld_mod_symbol))
            for op in operators_list:
                table.new_symbol(op.proxy_declaration_name,
                                 symbol_type=DataSymbol,
                                 datatype=op_datatype_symbol)

        # Declarations of CMA operator proxies
        cma_op_args = self._invoke.unique_declarations(
            argument_types=["gh_columnwise_operator"])
        if cma_op_args:
            op_type = cma_op_args[0].proxy_data_type
            mod_name = cma_op_args[0].module_name
            mod_st = table.node.parent.symbol_table
            fld_mod_symbol = mod_st.lookup(mod_name)
            op_datatype_symbol = mod_st.find_or_create(
                    op_type,
                    symbol_type=DataTypeSymbol,
                    datatype=UnresolvedType(),
                    interface=ImportInterface(fld_mod_symbol))
            for arg in cma_op_args:
                table.new_symbol(arg.proxy_declaration_name,
                                 symbol_type=DataSymbol,
                                 datatype=op_datatype_symbol)

    def initialise(self, cursor: int) -> int:
        '''
        Insert code into the PSy layer to initialise all necessary proxies.

        :param cursor: position where to add the next initialisation
            statements.

        :returns: Updated cursor value.

        :raises InternalError: if a kernel argument of an unrecognised type
            is encountered.

        '''
        init_cursor = cursor
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
                    self._invoke.schedule.addchild(
                        Assignment.create(
                            lhs=ArrayReference.create(
                                self.symtab.lookup(arg.proxy_name),
                                [Literal(str(idx), INTEGER_TYPE)]),
                            rhs=Call.create(ArrayOfStructuresReference.create(
                                self.symtab.lookup(arg.name),
                                [Literal(str(idx), INTEGER_TYPE)],
                                ["get_proxy"]))),
                        cursor)
                    cursor += 1
                    symbol = self.symtab.lookup_with_tag(
                        f"{arg.name}_{idx}:{suffix}")
                    self._invoke.schedule.addchild(
                        Assignment.create(
                            lhs=Reference(symbol),
                            rhs=ArrayOfStructuresReference.create(
                                    self.symtab.lookup(arg.proxy_name),
                                    [Literal(str(idx), INTEGER_TYPE)],
                                    ["data"]),
                            is_pointer=True),
                        cursor)
                    cursor += 1
            else:
                self._invoke.schedule.addchild(
                    Assignment.create(
                        lhs=Reference(
                            self.symtab.find_or_create(arg.proxy_name)),
                        rhs=Call.create(StructureReference.create(
                            self.symtab.lookup(arg.name), ["get_proxy"]))),
                    cursor)
                cursor += 1
                if arg.is_field:
                    symbol = self.symtab.lookup_with_tag(
                        f"{arg.name}:{suffix}")
                    self._invoke.schedule.addchild(
                        Assignment.create(
                            lhs=Reference(symbol),
                            rhs=StructureReference.create(
                                self.symtab.lookup(arg.proxy_name), ["data"]),
                            is_pointer=True),
                        cursor)
                    cursor += 1
                elif arg.is_operator:
                    if arg.argument_type == "gh_columnwise_operator":
                        # CMA operator arguments are handled in
                        # LFRicCMAOperators
                        pass
                    elif arg.argument_type == "gh_operator":
                        symbol = self.symtab.lookup_with_tag(
                            f"{arg.name}:{suffix}")
                        self._invoke.schedule.addchild(
                            Assignment.create(
                                lhs=Reference(symbol),
                                rhs=StructureReference.create(
                                    self.symtab.lookup(arg.proxy_name),
                                    ["local_stencil"]),
                                is_pointer=True),
                            cursor)
                        cursor += 1
                    else:
                        raise InternalError(
                            f"Kernel argument '{arg.name}' is a recognised "
                            f"operator but its type ('{arg.argument_type}') is"
                            f" not supported by LFRicProxies.initialise()")
                else:
                    raise InternalError(
                        f"Kernel argument '{arg.name}' of type "
                        f"'{arg.argument_type}' not "
                        f"handled in LFRicProxies.initialise()")
            if cursor > init_cursor:
                self._invoke.schedule[init_cursor].preceding_comment = (
                    "Initialise field and/or operator proxies")

        return cursor


class LFRicLMAOperators(LFRicCollection):
    '''
    Handles all entities associated with Local-Matrix-Assembly Operators.
    '''
    def stub_declarations(self):
        '''
        Declare all LMA-related quantities in a Kernel stub. Note that argument
        order will be defined later by ArgOrdering.

        '''
        super().stub_declarations()
        lma_args = psyGen.args_filter(
            self._kernel.arguments.args, arg_types=["gh_operator"])
        if lma_args:
            arg = self.symtab.find_or_create(
                "cell", symbol_type=DataSymbol,
                datatype=LFRicTypes("LFRicIntegerScalarDataType")())
            arg.interface = ArgumentInterface(ArgumentInterface.Access.READ)
            self.symtab.append_argument(arg)
        for arg in lma_args:
            size = arg.name+"_ncell_3d"
            op_dtype = arg.intrinsic_type
            op_kind = arg.precision
            size_sym = self.symtab.find_or_create(
                size, symbol_type=DataSymbol,
                datatype=LFRicTypes("LFRicIntegerScalarDataType")())
            size_sym.interface = ArgumentInterface(
                                        ArgumentInterface.Access.READ)
            self.symtab.append_argument(size_sym)
            ndf_name_to = self.symtab.lookup(
                                    arg.function_space_to.ndf_name)
            ndf_name_from = self.symtab.lookup(
                                    arg.function_space_from.ndf_name)

            # Create the PSyIR intrinsic DataType
            kind_sym = self.symtab.find_or_create(
                op_kind, symbol_type=DataSymbol, datatype=UnresolvedType(),
                interface=ImportInterface(
                    self.symtab.lookup("constants_mod")))
            if op_dtype == "real":
                intr_type = ScalarType(ScalarType.Intrinsic.REAL,
                                       Reference(kind_sym))
            elif op_dtype == "integer":
                intr_type = ScalarType(ScalarType.Intrinsic.INTEGER,
                                       Reference(kind_sym))
            else:
                raise NotImplementedError(
                    f"Only REAL and INTEGER LMA Operator types are supported, "
                    f"but found '{op_dtype}'")
            if arg.intent == "in":
                intent = ArgumentInterface.Access.READ
            elif arg.intent == "inout":
                intent = ArgumentInterface.Access.READWRITE
            # No need for else as arg.intent only returns in/inout or errors

            arg_sym = self.symtab.find_or_create(
                arg.name, symbol_type=DataSymbol,
                datatype=ArrayType(intr_type, [
                    Reference(size_sym),
                    Reference(ndf_name_to),
                    Reference(ndf_name_from),
                ]))
            arg_sym.interface = ArgumentInterface(intent)
            self.symtab.append_argument(arg_sym)

    def invoke_declarations(self):
        '''
        Declare all LMA-related quantities in a PSy-layer routine.
        Note: PSy layer in LFRic does not modify the LMA operator objects.
        Hence, their Fortran intents are always "in" (the data updated in the
        kernels is only pointed to from the LMA operator object and is thus
        not a part of the object).

        '''
        super().invoke_declarations()
        # Add the Invoke subroutine argument declarations for operators
        op_args = self._invoke.unique_declarations(
                                        argument_types=["gh_operator"])
        # Update the operator intents
        for arg in op_args:
            symbol = self.symtab.lookup(arg.declaration_name)
            symbol.interface = ArgumentInterface(ArgumentInterface.Access.READ)


class LFRicCMAOperators(LFRicCollection):
    '''
    Holds all information on the Column-Matrix-Assembly operators
    required by an Invoke or Kernel stub.

    :param node: either an Invoke schedule or a single Kernel object.
    :type node: :py:class:`psyclone.lfric.LFRicSchedule` or \
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
        for call in self.kernel_calls:
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

    def initialise(self, cursor: int) -> int:
        '''
        Generates the calls to the LFRic infrastructure that look-up
        the various components of each CMA operator. Adds these as
        children of the supplied parent node.

        :param cursor: position where to add the next initialisation
            statements.

        :returns: Updated cursor value.

        '''
        # If we have no CMA operators then we do nothing
        if not self._cma_ops:
            return cursor

        const = LFRicConstants()
        suffix = const.ARG_TYPE_SUFFIX_MAPPING["gh_columnwise_operator"]

        first = True
        for op_name in self._cma_ops:
            # First, assign a pointer to the array containing the actual
            # matrix.
            cma_sym = self.symtab.find_or_create_tag(
                f"{op_name}:{suffix}", op_name,
                symbol_type=DataSymbol, datatype=UnresolvedType())
            stmt = Assignment.create(
                    lhs=Reference(cma_sym),
                    rhs=StructureReference.create(
                             self.symtab.lookup(
                                self._cma_ops[op_name]["arg"].proxy_name),
                             ["columnwise_matrix"]),
                    is_pointer=True)
            if first:
                stmt.preceding_comment = (
                    "Look-up information for each CMA operator"
                )
                first = False
            self._invoke.schedule.addchild(stmt, cursor)
            cursor += 1
            # Then make copies of the related integer parameters
            for param in self._cma_ops[op_name]["params"]:
                param_name = self.symtab.find_or_create_tag(
                    f"{op_name}:{param}:{suffix}",
                    root_name=f"{op_name}_{param}",
                    symbol_type=DataSymbol,
                    datatype=LFRicTypes("LFRicIntegerScalarDataType")()
                )
                stmt = Assignment.create(
                        lhs=Reference(param_name),
                        rhs=StructureReference.create(
                             self.symtab.lookup(
                                self._cma_ops[op_name]["arg"].proxy_name),
                             [param]),
                    )
                self._invoke.schedule.addchild(stmt, cursor)
                cursor += 1
        return cursor

    def invoke_declarations(self):
        '''
        Generate the necessary PSy-layer declarations for all column-wise
        operators and their associated parameters.
        Note: PSy layer in LFRic does not modify the CMA operator objects.
        Hence, their Fortran intents are always "in" (the data updated in the
        kernels is only pointed to from the column-wise operator object and is
        thus not a part of the object).

        '''
        super().invoke_declarations()
        # If we have no CMA operators then we do nothing
        if not self._cma_ops:
            return

        const = LFRicConstants()
        suffix = const.ARG_TYPE_SUFFIX_MAPPING["gh_columnwise_operator"]
        for op_name in self._cma_ops:
            new_name = self.symtab.next_available_name(
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
            self.symtab.new_symbol(new_name,
                                   symbol_type=DataSymbol,
                                   datatype=dtype,
                                   tag=tag)

            # Declare the associated integer parameters
            for param in self._cma_ops[op_name]["params"]:
                name = f"{op_name}_{param}"
                tag = f"{op_name}:{param}:{suffix}"
                self.symtab.find_or_create(
                    name, tag=tag,
                    symbol_type=DataSymbol,
                    datatype=LFRicTypes("LFRicIntegerScalarDataType")()
                )

    def stub_declarations(self):
        '''
        Generate all necessary declarations for CMA operators being passed to
        a Kernel stub.
        Note that argument order is redefined later by ArgOrdering.

        '''
        super().stub_declarations()
        # If we have no CMA operators then we do nothing
        if not self._cma_ops:
            return

        symtab = self.symtab

        # CMA operators always need the current cell index and the number
        # of columns in the mesh
        symbol = symtab.find_or_create(
            "cell", symbol_type=DataSymbol,
            datatype=LFRicTypes("LFRicIntegerScalarDataType")())
        symbol.interface = ArgumentInterface(ArgumentInterface.Access.READ)
        symtab.append_argument(symbol)
        symbol = symtab.find_or_create(
            "ncell_2d", symbol_type=DataSymbol,
            datatype=LFRicTypes("LFRicIntegerScalarDataType")())
        symbol.interface = ArgumentInterface(ArgumentInterface.Access.READ)
        symtab.append_argument(symbol)

        const = LFRicConstants()
        suffix = const.ARG_TYPE_SUFFIX_MAPPING["gh_columnwise_operator"]

        for op_name in self._cma_ops:
            # Declare the associated scalar arguments before the array because
            # some of them are used to dimension the latter (and some compilers
            # get upset if this ordering is not followed)
            for param in self._cma_ops[op_name]["params"]:
                symbol = symtab.find_or_create_tag(
                    f"{op_name}:{param}:{suffix}",
                    root_name=f"{op_name}_{param}",
                    symbol_type=DataSymbol,
                    datatype=LFRicTypes("LFRicIntegerScalarDataType")())
                symbol.interface = ArgumentInterface(
                        ArgumentInterface.Access.READ)
                symtab.append_argument(symbol)
            # Declare the array that holds the CMA operator
            bandwidth = symtab.find_or_create_tag(
                f"{op_name}:bandwidth:{suffix}",
                root_name=f"{op_name}_bandwidth",
                symbol_type=DataSymbol,
                datatype=LFRicTypes("LFRicIntegerScalarDataType")())
            bandwidth.interface = ArgumentInterface(
                    ArgumentInterface.Access.READ)

            nrow = symtab.find_or_create_tag(
                f"{op_name}:nrow:{suffix}",
                root_name=f"{op_name}_nrow",
                symbol_type=DataSymbol,
                datatype=LFRicTypes("LFRicIntegerScalarDataType")())
            nrow.interface = ArgumentInterface(
                    ArgumentInterface.Access.READ)

            op = symtab.find_or_create(
                op_name, symbol_type=DataSymbol,
                datatype=ArrayType(
                    LFRicTypes("LFRicRealScalarDataType")(),
                    [Reference(bandwidth), Reference(nrow),
                     Reference(symtab.lookup("ncell_2d"))]))
            if self._kernel.cma_operation == 'assembly':
                op.interface = ArgumentInterface(
                        ArgumentInterface.Access.READWRITE)
            else:
                op.interface = ArgumentInterface(
                        ArgumentInterface.Access.READ)
            symtab.append_argument(op)


class LFRicMeshes():
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
    :type invoke: :py:class:`psyclone.domain.lfric.LFRicInvoke`
    :param unique_psy_vars: list of arguments to the PSy-layer routine.
    :type unique_psy_vars: list of \
                      :py:class:`psyclone.lfric.LFRicKernelArgument` objects.
    '''

    def __init__(self, invoke, unique_psy_vars):
        # List of names of unique mesh variables referenced in the Invoke
        self._mesh_tag_names = []
        # Whether or not the associated Invoke requires colourmap information
        self._needs_colourmap = False
        self._needs_colourmap_halo = False
        self._needs_colourtilemap = False
        self._needs_colourtilemap_halo = False
        # Keep a reference to the Invoke so we can check its properties later
        self._invoke = invoke
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
        has_intergrid = False
        for call in self._invoke.schedule.coded_kernels():

            if (call.reference_element.properties or call.mesh.properties or
                    call.iterates_over == "domain" or call.cma_operation):
                _name_set.add("mesh")

            if not call.is_intergrid:
                non_intergrid_kernels.append(call)
            else:
                has_intergrid = True
                # Create and store the names of the associated mesh objects
                _name_set.add(f"mesh_{call._intergrid_ref.fine.name}")
                _name_set.add(f"mesh_{call._intergrid_ref.coarse.name}")

        # If we found a mixture of both inter-grid and non-inter-grid kernels
        # then we reject the invoke()
        if non_intergrid_kernels and has_intergrid:
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
        # require a mesh object but that is handled in colourmap_init().)
        if not _name_set and Config.get().distributed_memory:
            # We didn't already have a requirement for a mesh so add one now.
            _name_set.add("mesh")

        self._add_mesh_symbols(list(_name_set))

    @property
    def symtab(self):
        '''
        :returns: associated symbol table.
        :rtype: :py:class:`psyclone.psyir.symbols.SymbolTable`
        '''
        return self._invoke.schedule.symbol_table

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
        csym = self.symtab.find_or_create_tag(
            mmod, symbol_type=ContainerSymbol)
        # Create a TypeSymbol for the mesh type
        mtype_sym = self.symtab.find_or_create_tag(
            mtype, symbol_type=DataTypeSymbol,
            datatype=UnresolvedType(),
            interface=ImportInterface(csym))

        name_list = []
        for name in mesh_tags:
            dt = UnsupportedFortranType(
                f"type({mtype_sym.name}), pointer :: {name} => null()")
            name_list.append(self.symtab.find_or_create_tag(
                name, symbol_type=DataSymbol, datatype=dt).name)

        if Config.get().distributed_memory:
            # If distributed memory is enabled then we require a variable
            # holding the maximum halo depth for each mesh.
            for name in mesh_tags:
                var_name = f"max_halo_depth_{name}"
                self.symtab.find_or_create(
                    var_name, tag=var_name,
                    symbol_type=DataSymbol,
                    datatype=LFRicTypes("LFRicIntegerScalarDataType")()
                )

    def colourmap_init(self):
        '''
        Sets-up information on any required colourmaps. Since colouring is
        applied by Transformations, this method is called as the final step
        of LFRicColourTrans.apply().

        '''
        # pylint: disable=too-many-locals
        const = LFRicConstants()
        non_intergrid_kern = None

        for call in [call for call in self._invoke.schedule.coded_kernels() if
                     call.is_coloured()]:
            # Keep a record of whether or not any kernels (loops) in this
            # invoke have been coloured and, if so, whether the associated loop
            # is tiled or it goes into the halo.
            loop = call.parent.parent
            is_tiled = loop.loop_type == "cells_in_tile"
            if is_tiled:
                has_halo = (loop.parent.parent.upper_bound_name in
                            const.HALO_ACCESS_LOOP_BOUNDS)
                if has_halo:
                    self._needs_colourtilemap_halo = True
                else:
                    self._needs_colourtilemap = True
            else:
                has_halo = (loop.upper_bound_name in
                            const.HALO_ACCESS_LOOP_BOUNDS)
                if has_halo:
                    self._needs_colourmap_halo = True
                else:
                    self._needs_colourmap = True

            if not call.is_intergrid:
                non_intergrid_kern = call
                continue

            # This is an inter-grid kernel so look-up the names of
            # the colourmap variables associated with the coarse
            # mesh (since that determines the iteration space).
            carg_name = call._intergrid_ref.coarse.name

            # We use different variables if it is a tiled or a regular
            # colour map
            if not is_tiled:
                base_name = "cmap_" + carg_name
                array_type = ArrayType(
                    LFRicTypes("LFRicRealScalarDataType")(),
                    [ArrayType.Extent.DEFERRED]*2)
                colour_map = self.symtab.find_or_create_tag(
                    base_name,
                    symbol_type=DataSymbol,
                    datatype=UnsupportedFortranType(
                        f"integer(kind=i_def), pointer, dimension(:,:) :: "
                        f"{base_name} => null()",
                        partial_datatype=array_type))
                # No. of colours
                base_name = "ncolour_" + carg_name
                ncolours = self.symtab.find_or_create_tag(
                    base_name,
                    symbol_type=DataSymbol,
                    datatype=LFRicTypes("LFRicIntegerScalarDataType")()
                )
                # Array holding the last cell of a given colour.
                if (Config.get().distributed_memory and
                        not call.all_updates_are_writes):
                    # This will require a loop into the halo and so the array
                    # is 2D (indexed by colour *and* halo depth).
                    base_name = "last_halo_cell_all_colours_" + carg_name
                    last_cell = self.symtab.find_or_create_tag(
                        base_name,
                        symbol_type=DataSymbol,
                        datatype=ArrayType(
                                LFRicTypes("LFRicIntegerScalarDataType")(),
                                [ArrayType.Extent.DEFERRED]*2))
                else:
                    # Array holding the last edge cell of a given colour. Just
                    # 1D as indexed by colour only.
                    base_name = "last_edge_cell_all_colours_" + carg_name
                    last_cell = self.symtab.find_or_create_tag(
                        base_name,
                        symbol_type=DataSymbol,
                        datatype=ArrayType(
                                LFRicTypes("LFRicIntegerScalarDataType")(),
                                [ArrayType.Extent.DEFERRED]*1))
                # Add these symbols into the LFRicInterGrid entry for this
                # kernel
                call._intergrid_ref.set_colour_info(colour_map, ncolours,
                                                    last_cell)
            else:
                # Tiled colour map
                base_name = "tmap_" + carg_name
                tilecolour_map = self.symtab.find_or_create_tag(
                    base_name, symbol_type=DataSymbol,
                    datatype=UnsupportedFortranType(
                        f"integer(kind=i_def), pointer :: {base_name}(:,:,:)"))
                base_name = "ntilecolour_" + carg_name
                ntilecolours = self.symtab.find_or_create_integer_symbol(
                                    base_name, tag=base_name)
                # Array holding the last cell of a given colour.
                if (Config.get().distributed_memory and
                        not call.all_updates_are_writes):
                    # This will require a loop into the halo and so the array
                    # is 2D (indexed by colour *and* halo depth).
                    base_name = "last_halo_tile_per_colour_" + carg_name
                    last_tile = self.symtab.find_or_create_array(
                        base_name, 2, ScalarType.Intrinsic.INTEGER,
                        tag=base_name)
                    base_name = ("last_halo_cell_per_colour_and_tile_" +
                                 carg_name)
                    last_cell_tile = self.symtab.find_or_create_array(
                        base_name, 3, ScalarType.Intrinsic.INTEGER,
                        tag=base_name)
                else:
                    # Array holding the last edge cell of a given colour. Just
                    # 1D as indexed by colour only.
                    base_name = "last_edge_tile_per_colour_" + carg_name
                    last_tile = self.symtab.find_or_create_array(
                        base_name, 1, ScalarType.Intrinsic.INTEGER,
                        tag=base_name)
                    base_name = ("last_edge_cell_per_colour_and_tile_"
                                 + carg_name)
                    last_cell_tile = self.symtab.find_or_create_array(
                        base_name, 2, ScalarType.Intrinsic.INTEGER,
                        tag=base_name)
                # Add these symbols into the dictionary entry for this
                # inter-grid kernel
                call._intergrid_ref.set_tilecolour_info(
                    tilecolour_map, ntilecolours, last_tile, last_cell_tile)

        if non_intergrid_kern and (self._needs_colourmap or
                                   self._needs_colourmap_halo):
            # There aren't any inter-grid kernels but we do need colourmap
            # information and that means we'll need a mesh object
            self._add_mesh_symbols(["mesh"])
            # This creates the colourmap information for this invoke if we
            # don't already have one.
            colour_map = non_intergrid_kern.colourmap
            # No. of colours
            ncolours = self.symtab.find_or_create(
                "ncolour", tag="ncolour",
                symbol_type=DataSymbol,
                datatype=LFRicTypes("LFRicIntegerScalarDataType")()
            ).name
            if self._needs_colourmap_halo:
                self.symtab.find_or_create(
                    "last_halo_cell_all_colours",
                    symbol_type=DataSymbol,
                    datatype=ArrayType(
                            LFRicTypes("LFRicIntegerScalarDataType")(),
                            [ArrayType.Extent.DEFERRED]*2),
                    tag="last_halo_cell_all_colours")
            if self._needs_colourmap:
                self.symtab.find_or_create(
                    "last_edge_cell_all_colours",
                    symbol_type=DataSymbol,
                    datatype=ArrayType(
                            LFRicTypes("LFRicIntegerScalarDataType")(),
                            [ArrayType.Extent.DEFERRED]*1),
                    tag="last_edge_cell_all_colours")
        if non_intergrid_kern and (self._needs_colourtilemap or
                                   self._needs_colourtilemap_halo):
            # There aren't any inter-grid kernels but we do need colourtilemap
            # information and that means we'll need a mesh object
            self._add_mesh_symbols(["mesh"])
            # Creates the colourtilemap information for this invoke if we
            # don't already have one.
            colour_map = non_intergrid_kern.tilecolourmap
            # Create the No. of colours over tiles
            _ = self.symtab.find_or_create_tag(
                "ntilecolours",
                symbol_type=DataSymbol,
                datatype=LFRicTypes("LFRicIntegerScalarDataType")()
            )
            if self._needs_colourtilemap_halo:
                self.symtab.find_or_create(
                    "last_halo_tile_per_colour",
                    symbol_type=DataSymbol,
                    datatype=ArrayType(
                            LFRicTypes("LFRicIntegerScalarDataType")(),
                            [ArrayType.Extent.DEFERRED]*2),
                    tag="last_halo_tile_per_colour")
                self.symtab.find_or_create(
                    "last_halo_cell_per_colour_and_tile",
                    symbol_type=DataSymbol,
                    datatype=ArrayType(
                            LFRicTypes("LFRicIntegerScalarDataType")(),
                            [ArrayType.Extent.DEFERRED]*3),
                    tag="last_halo_cell_per_colour_and_tile")
            if self._needs_colourtilemap:
                self.symtab.find_or_create_tag(
                    "last_edge_tile_per_colour",
                    symbol_type=DataSymbol,
                    datatype=ArrayType(
                            LFRicTypes("LFRicIntegerScalarDataType")(),
                            [ArrayType.Extent.DEFERRED]*1))
                self.symtab.find_or_create_tag(
                    "last_edge_cell_per_colour_and_tile",
                    symbol_type=DataSymbol,
                    datatype=ArrayType(
                            LFRicTypes("LFRicIntegerScalarDataType")(),
                            [ArrayType.Extent.DEFERRED]*2))

    def invoke_declarations(self):
        '''
        Declare variables specific to mesh objects.

        '''
        # pylint: disable=too-many-locals, too-many-statements
        const = LFRicConstants()

        if self.intergrid_kernels:
            mmap_type = const.MESH_TYPE_MAP["mesh_map"]["type"]
            mmap_mod = const.MESH_TYPE_MAP["mesh_map"]["module"]
            # Create a Container symbol for the module
            csym = self.symtab.find_or_create_tag(
                mmap_mod, symbol_type=ContainerSymbol)
            # Create a TypeSymbol for the mesh type
            self.symtab.find_or_create_tag(
                mmap_type, symbol_type=DataTypeSymbol,
                datatype=UnresolvedType(),
                interface=ImportInterface(csym))

        if not self.intergrid_kernels:
            if self._needs_colourmap or self._needs_colourmap_halo:
                # There aren't any inter-grid kernels but we do need
                # colourmap information
                csym = self.symtab.lookup_with_tag("cmap")
            if self._needs_colourtilemap or self._needs_colourtilemap_halo:
                # There aren't any inter-grid kernels but we do need
                # colourmap information
                csym = self.symtab.lookup_with_tag("tilecolourmap")

    def initialise(self, cursor: int) -> int:
        '''
        Initialise parameters specific to inter-grid kernels.

        :param cursor: position where to add the next initialisation
            statements.

        :returns: Updated cursor value.

        '''
        # pylint: disable=too-many-branches
        # If we haven't got any need for a mesh in this invoke then we
        # don't do anything
        if not self._mesh_tag_names:
            return cursor

        symtab = self._invoke.schedule.symbol_table

        if len(self._mesh_tag_names) == 1:
            # We only require one mesh object which means that this invoke
            # contains no inter-grid kernels (which would require at least 2)
            mesh_sym = symtab.lookup_with_tag(self._mesh_tag_names[0])
            assignment = Assignment.create(
                lhs=Reference(mesh_sym),
                rhs=self._first_var.generate_method_call("get_mesh"),
                is_pointer=True)
            assignment.preceding_comment = "Create a mesh object"
            self._invoke.schedule.addchild(assignment, cursor)
            cursor += 1
            if Config.get().distributed_memory:
                # If distributed memory is enabled then we need the maximum
                # halo depth.
                depth_sym = self.symtab.lookup_with_tag(
                    f"max_halo_depth_{self._mesh_tag_names[0]}")
                self._invoke.schedule.addchild(Assignment.create(
                    lhs=Reference(depth_sym),
                    rhs=Call.create(StructureReference.create(
                        mesh_sym, ["get_halo_depth"]))),
                    cursor)
                cursor += 1
            if self._needs_colourmap or self._needs_colourmap_halo:
                # Look-up variable names for colourmap and number of colours
                cmap = self.symtab.find_or_create_tag("cmap")
                ncolour = self.symtab.find_or_create_tag("ncolour")
                # Get the number of colours
                assignment = Assignment.create(
                        lhs=Reference(ncolour),
                        rhs=Call.create(StructureReference.create(
                            mesh_sym, ["get_ncolours"])))
                assignment.preceding_comment = "Get the colourmap"
                self._invoke.schedule.addchild(assignment, cursor)
                cursor += 1
                # Get the colour map
                assignment = Assignment.create(
                        lhs=Reference(cmap),
                        rhs=Call.create(StructureReference.create(
                            mesh_sym, ["get_colour_map"])),
                        is_pointer=True)
                self._invoke.schedule.addchild(assignment, cursor)
                cursor += 1
            if self._needs_colourtilemap or self._needs_colourtilemap_halo:
                # Look-up variable names for colourmap and number of colours
                tmap = self.symtab.lookup_with_tag("tilecolourmap")
                ntc = self.symtab.lookup_with_tag("ntilecolours")
                # Get the number of colours
                assignment = Assignment.create(
                        lhs=Reference(ntc),
                        rhs=Call.create(StructureReference.create(
                            mesh_sym, ["get_ntilecolours"])))
                assignment.preceding_comment = "Get the tiled colourmap"
                self._invoke.schedule.addchild(assignment, cursor)
                cursor += 1
                # Get the colour map
                assignment = Assignment.create(
                        lhs=Reference(tmap),
                        rhs=Call.create(StructureReference.create(
                            mesh_sym, ["get_coloured_tiling_map"])),
                        is_pointer=True)
                self._invoke.schedule.addchild(assignment, cursor)
                cursor += 1

        # Keep a list of quantities that we've already initialised so
        # that we don't generate duplicate assignments
        initialised = []

        comment_cursor = cursor
        # Loop over the LFRicInterGrid objects
        for dig in self.intergrid_kernels:
            # We need pointers to both the coarse and the fine mesh as well
            # as the maximum halo depth for each.
            fine_mesh = self.symtab.find_or_create_tag(f"mesh_{dig.fine.name}")
            coarse_mesh = self.symtab.find_or_create_tag(
                                            f"mesh_{dig.coarse.name}")
            if fine_mesh not in initialised:
                initialised.append(fine_mesh)
                assignment = Assignment.create(
                        lhs=Reference(fine_mesh),
                        rhs=dig.fine.generate_method_call("get_mesh"),
                        is_pointer=True)
                self._invoke.schedule.addchild(assignment, cursor)
                cursor += 1
                if Config.get().distributed_memory:
                    max_halo_f_mesh = (
                        self.symtab.find_or_create_tag(
                            f"max_halo_depth_mesh_{dig.fine.name}"))
                    assignment = Assignment.create(
                            lhs=Reference(max_halo_f_mesh),
                            rhs=Call.create(StructureReference.create(
                                fine_mesh, ["get_halo_depth"])))
                    self._invoke.schedule.addchild(assignment, cursor)
                    cursor += 1
            if coarse_mesh not in initialised:
                initialised.append(coarse_mesh)
                assignment = Assignment.create(
                        lhs=Reference(coarse_mesh),
                        rhs=dig.coarse.generate_method_call("get_mesh"),
                        is_pointer=True)
                self._invoke.schedule.addchild(assignment, cursor)
                cursor += 1
                if Config.get().distributed_memory:
                    max_halo_c_mesh = (
                        self.symtab.find_or_create_tag(
                            f"max_halo_depth_mesh_{dig.coarse.name}"))
                    assignment = Assignment.create(
                            lhs=Reference(max_halo_c_mesh),
                            rhs=Call.create(StructureReference.create(
                                coarse_mesh, ["get_halo_depth"])))
                    self._invoke.schedule.addchild(assignment, cursor)
                    cursor += 1
            # We also need a pointer to the mesh map which we get from
            # the coarse mesh
            if dig.mmap not in initialised:
                initialised.append(dig.mmap)
                digmmap = self.symtab.lookup(dig.mmap)
                assignment = Assignment.create(
                        lhs=Reference(digmmap),
                        rhs=Call.create(StructureReference.create(
                                coarse_mesh, ["get_mesh_map"]),
                            arguments=[Reference(fine_mesh)]),
                        is_pointer=True)
                self._invoke.schedule.addchild(assignment, cursor)
                cursor += 1

            # Cell map. This is obtained from the mesh map.
            if dig.cell_map not in initialised:
                initialised.append(dig.cell_map)
                digcellmap = self.symtab.lookup(dig.cell_map)
                assignment = Assignment.create(
                        lhs=Reference(digcellmap),
                        rhs=Call.create(StructureReference.create(
                            digmmap, ["get_whole_cell_map"])),
                        is_pointer=True)
                self._invoke.schedule.addchild(assignment, cursor)
                cursor += 1

            # Number of cells in the fine mesh
            if dig.ncell_fine not in initialised:
                initialised.append(dig.ncell_fine)
                digncellfine = self.symtab.lookup(dig.ncell_fine)
                if Config.get().distributed_memory:
                    # TODO this hardwired depth of 2 will need changing in
                    # order to support redundant computation
                    assignment = Assignment.create(
                            lhs=Reference(digncellfine),
                            rhs=Call.create(StructureReference.create(
                                fine_mesh, ["get_last_halo_cell"])))
                    assignment.rhs.append_named_arg("depth",
                                                    Literal("2", INTEGER_TYPE))
                    self._invoke.schedule.addchild(assignment, cursor)
                    cursor += 1
                else:
                    assignment = Assignment.create(
                            lhs=Reference(digncellfine),
                            rhs=dig.fine.generate_method_call("get_ncell"))
                    self._invoke.schedule.addchild(assignment, cursor)
                    cursor += 1

            # Number of fine cells per coarse cell in x.
            if dig.ncellpercellx not in initialised:
                initialised.append(dig.ncellpercellx)
                digncellpercellx = self.symtab.lookup(dig.ncellpercellx)
                assignment = Assignment.create(
                        lhs=Reference(digncellpercellx),
                        rhs=Call.create(StructureReference.create(
                            digmmap, ["get_ntarget_cells_per_source_x"])))
                self._invoke.schedule.addchild(assignment, cursor)
                cursor += 1

            # Number of fine cells per coarse cell in y.
            if dig.ncellpercelly not in initialised:
                initialised.append(dig.ncellpercelly)
                digncellpercelly = self.symtab.lookup(dig.ncellpercelly)
                assignment = Assignment.create(
                        lhs=Reference(digncellpercelly),
                        rhs=Call.create(StructureReference.create(
                            digmmap, ["get_ntarget_cells_per_source_y"])))
                self._invoke.schedule.addchild(assignment, cursor)
                cursor += 1

            # Colour map for the coarse mesh (if required)
            if dig.colourmap_symbol:
                # Number of colours
                assignment = Assignment.create(
                        lhs=Reference(dig.ncolours_var_symbol),
                        rhs=Call.create(StructureReference.create(
                            coarse_mesh, ["get_ncolours"])))
                self._invoke.schedule.addchild(assignment, cursor)
                cursor += 1
                # Colour map itself
                assignment = Assignment.create(
                        lhs=Reference(dig.colourmap_symbol),
                        rhs=Call.create(StructureReference.create(
                            coarse_mesh, ["get_colour_map"])),
                        is_pointer=True)
                self._invoke.schedule.addchild(assignment, cursor)
                cursor += 1
                # Last halo/edge cell per colour.
                sym = dig.last_cell_var_symbol
                if len(sym.datatype.shape) == 2:
                    # Array is 2D so is a halo access.
                    name = "get_last_halo_cell_all_colours"
                else:
                    # Array is just 1D so go to the last edge cell.
                    name = "get_last_edge_cell_all_colours"
                assignment = Assignment.create(
                        lhs=Reference(sym),
                        rhs=Call.create(StructureReference.create(
                            coarse_mesh, [name])))
                self._invoke.schedule.addchild(assignment, cursor)
                cursor += 1
            # Colour map for the coarse mesh (if required)
            if dig.tilecolourmap_symbol:
                # Number of colours
                assignment = Assignment.create(
                        lhs=Reference(dig.ntilecolours_var_symbol),
                        rhs=Call.create(StructureReference.create(
                            coarse_mesh, ["get_ntilecolours"])))
                self._invoke.schedule.addchild(assignment, cursor)
                cursor += 1
                # Colour map itself
                assignment = Assignment.create(
                        lhs=Reference(dig.tilecolourmap_symbol),
                        rhs=Call.create(StructureReference.create(
                            coarse_mesh, ["get_coloured_tiling_map"])),
                        is_pointer=True)
                self._invoke.schedule.addchild(assignment, cursor)
                cursor += 1
                # Last halo/edge tile per colour.
                sym = dig.last_tile_var_symbol
                if len(sym.datatype.shape) == 2:
                    # Array is 2D so is a halo access.
                    name = "get_last_halo_tile_all_colours"
                else:
                    # Array is just 1D so go to the last edge cell.
                    name = "get_last_edge_tile_all_colours"
                assignment = Assignment.create(
                        lhs=Reference(sym),
                        rhs=Call.create(StructureReference.create(
                            coarse_mesh, [name])))
                self._invoke.schedule.addchild(assignment, cursor)
                cursor += 1
                # Last halo/edge cell per colour and tile.
                sym = dig.last_cell_tile_var_symbol
                if len(sym.datatype.shape) == 3:
                    # Array is 3D so is a halo access.
                    name = "get_last_halo_cell_all_colours_all_tiles"
                else:
                    # Array is just 2D so go to the last edge cell.
                    name = "get_last_edge_cell_all_colours_all_tiles"
                assignment = Assignment.create(
                        lhs=Reference(sym),
                        rhs=Call.create(StructureReference.create(
                            coarse_mesh, [name])))
                self._invoke.schedule.addchild(assignment, cursor)
                cursor += 1
        if cursor != comment_cursor:
            self._invoke.schedule[comment_cursor].preceding_comment = (
                "Look-up mesh objects and loop limits for inter-grid kernels")

        return cursor

    @property
    def intergrid_kernels(self):
        '''
        :returns: A list of objects describing the intergrid kernels used in
            this invoke.
        :rtype: list[:py:class:`psyclone.lfric.LFRicInterGrid`]
        '''
        intergrids = []
        for call in self._invoke.schedule.coded_kernels():
            if call.is_intergrid:
                intergrids.append(call._intergrid_ref)
        return intergrids


class LFRicInterGrid():
    '''
    Holds information on quantities required by an inter-grid kernel.

    :param fine_arg: Kernel argument on the fine mesh.
    :type fine_arg: :py:class:`psyclone.lfric.LFRicKernelArgument`
    :param coarse_arg: Kernel argument on the coarse mesh.
    :type coarse_arg: :py:class:`psyclone.lfric.LFRicKernelArgument`
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
        self.mmap = symtab.find_or_create(
                base_mmap_name, tag=base_mmap_name,
                symbol_type=DataSymbol,
                datatype=UnsupportedFortranType(
                    f"type(mesh_map_type), pointer :: {base_mmap_name}"
                    f" => null()")
            ).name

        # Generate name for ncell variables
        name = f"ncell_{fine_arg.name}"
        self.ncell_fine = symtab.find_or_create(
            name, tag=name,
            symbol_type=DataSymbol,
            datatype=LFRicTypes("LFRicIntegerScalarDataType")()
        ).name
        # No. of fine cells per coarse cell in x
        name = f"ncpc_{fine_arg.name}_{coarse_arg.name}_x"
        self.ncellpercellx = symtab.find_or_create(
            name, tag=name,
            symbol_type=DataSymbol,
            datatype=LFRicTypes("LFRicIntegerScalarDataType")()
        ).name
        # No. of fine cells per coarse cell in y
        name = f"ncpc_{fine_arg.name}_{coarse_arg.name}_y"
        self.ncellpercelly = symtab.find_or_create(
            name, tag=name,
            symbol_type=DataSymbol,
            datatype=LFRicTypes("LFRicIntegerScalarDataType")()
        ).name
        # Name for cell map
        base_name = "cell_map_" + coarse_arg.name
        ArrayType(
            LFRicTypes("LFRicRealScalarDataType")(),
            [ArrayType.Extent.DEFERRED]*2)
        sym = symtab.find_or_create(
                base_name,
                symbol_type=DataSymbol,
                datatype=UnsupportedFortranType(
                    f"integer(kind=i_def), pointer :: {base_name}"
                    f"(:,:,:) => null()",
                    partial_datatype=ArrayType(
                        LFRicTypes("LFRicRealScalarDataType")(),
                        [ArrayType.Extent.DEFERRED]*3)
                ))

        self.cell_map = sym.name

        # We have no colourmap information when first created
        self._colourmap_symbol = None
        # Symbol for the variable holding the number of colours
        self._ncolours_var_symbol = None
        # Symbol of the variable holding the last cell of a particular colour.
        # Will be a 2D array if the kernel iteration space includes the halo
        # and 1D otherwise.
        self._last_cell_var_symbol = None

        # We have no colourmap information when first created
        self._tilecolourmap_symbol = None
        # Symbol for the variable holding the number of colours
        self._ntilecolours_var_symbol = None
        # Symbol of the variable holding the last tile of a particular colour
        self._last_tile_var_symbol = None
        # Symbol of the variable holding the last cell of a particular tile
        self._last_cell_tile_var_symbol = None

    def set_colour_info(self, colour_map: DataSymbol,
                        ncolours: DataSymbol, last_cell: DataSymbol):
        '''Sets the colour_map, number of colours, and last cell of a
        particular colour.

        :param colour_map: the colour map symbol.
        :param ncolours: the number of colours.
        :param last_cell: the last halo cell of a particular colour.

        '''
        self._colourmap_symbol = colour_map
        self._ncolours_var_symbol = ncolours
        self._last_cell_var_symbol = last_cell

    def set_tilecolour_info(self, tilecolour_map: DataSymbol,
                            ntilecolours: DataSymbol,
                            last_tile: DataSymbol,
                            last_cell_tile: DataSymbol):
        '''Sets the tilecolour_map, number of colours of tiles, last tile
        of a particular colour and last cell of a particular tile and colour.

        :param tilecolour_map: the tilecolourmap symbol.
        :param ntilecolours: the number of tilecolours.
        :param last_tile: the last tile of a particular colour.
        :param last_cell_tile: the last cell of a particular tilecolour.
        '''
        self._tilecolourmap_symbol = tilecolour_map
        self._ntilecolours_var_symbol = ntilecolours
        self._last_tile_var_symbol = last_tile
        self._last_cell_tile_var_symbol = last_cell_tile

    @property
    def colourmap_symbol(self) -> DataSymbol:
        '''
        :returns: the colour map symbol.
        '''
        return self._colourmap_symbol

    @property
    def ncolours_var_symbol(self) -> DataSymbol:
        '''
        :returns: the symbol storing the number of colours.
        '''
        return self._ncolours_var_symbol

    @property
    def last_cell_var_symbol(self) -> DataSymbol:
        '''
        :returns: the last halo/edge cell variable.
        '''
        return self._last_cell_var_symbol

    @property
    def tilecolourmap_symbol(self) -> DataSymbol:
        '''
        :returns: the tilecolour map symbol.
        '''
        return self._tilecolourmap_symbol

    @property
    def ntilecolours_var_symbol(self) -> DataSymbol:
        '''
        :returns: the symbol storing the number of tilecolours.
        '''
        return self._ntilecolours_var_symbol

    @property
    def last_tile_var_symbol(self) -> DataSymbol:
        '''
        :returns: the symbol with the last tile of a given colour.
        '''
        return self._last_tile_var_symbol

    @property
    def last_cell_tile_var_symbol(self) -> DataSymbol:
        '''
        :returns: the last cell in the tilecolour.
        '''
        return self._last_cell_tile_var_symbol


class LFRicBasisFunctions(LFRicCollection):
    ''' Holds all information on the basis and differential basis
    functions required by an invoke or kernel call. This covers both those
    required for quadrature and for evaluators.

    :param node: either the schedule of an Invoke or a single Kernel object
                 for which to extract information on all required
                 basis/diff-basis functions.
    :type node: :py:class:`psyclone.domain.lfric.LFRicInvokeSchedule` or
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
        # LFRicKernelArgument) tuples.
        self._eval_targets = OrderedDict()

        for call in self.kernel_calls:

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
            raise InternalError(f"Expected an LFRicKern object but got: "
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

    def stub_declarations(self):
        '''
        Insert the variable declarations required by the basis functions into
        the Kernel stub.
        Note that argument order is redefined later by ArgOrdering.

        :raises InternalError: if an unsupported quadrature shape is found.

        '''
        super().stub_declarations()
        if not self._qr_vars and not self._eval_targets:
            return

        # The quadrature shapes that this method supports
        supported_shapes = ["gh_quadrature_xyoz", "gh_quadrature_face",
                            "gh_quadrature_edge"]

        # Get the lists of dimensioning variables and basis arrays
        var_dims, basis_arrays = self._basis_fn_declns()

        for var in var_dims:
            arg = self.symtab.find_or_create(
                var, symbol_type=DataSymbol,
                datatype=LFRicTypes("LFRicIntegerScalarDataType")())
            if arg not in self.symtab.argument_list:
                arg.interface = ArgumentInterface(
                                        ArgumentInterface.Access.READ)
                self.symtab.append_argument(arg)

        for basis in basis_arrays:
            dims = []
            for value in basis_arrays[basis]:
                try:
                    dims.append(Literal(value, INTEGER_TYPE))
                except ValueError:
                    dims.append(Reference(self.symtab.find_or_create(value)))
            arg = self.symtab.find_or_create(
                basis, symbol_type=DataSymbol,
                datatype=ArrayType(LFRicTypes("LFRicRealScalarDataType")(),
                                   dims))
            arg.interface = ArgumentInterface(ArgumentInterface.Access.READ)
            self.symtab.append_argument(arg)

        const = LFRicConstants()

        for shape in self._qr_vars:
            qr_name = "_qr_" + shape.split("_")[-1]
            # Create the PSyIR intrinsic DataType
            if shape not in const.QUADRATURE_TYPE_MAP:
                raise InternalError(
                    f"Quadrature shapes other than {supported_shapes} are not "
                    f"yet supported - got: '{shape}'")
            kind_sym = self.symtab.find_or_create(
                const.QUADRATURE_TYPE_MAP[shape]["kind"],
                symbol_type=DataSymbol, datatype=UnresolvedType(),
                interface=ImportInterface(
                    self.symtab.lookup("constants_mod")))

            # All quatratures are REAL
            intr_type = ScalarType(ScalarType.Intrinsic.REAL,
                                   Reference(kind_sym))

            if shape == "gh_quadrature_xyoz":
                dim = self.symtab.find_or_create(
                    "np_xy"+qr_name, symbol_type=DataSymbol,
                    datatype=LFRicTypes("LFRicIntegerScalarDataType")())
                sym = self.symtab.find_or_create(
                    "weights_xy"+qr_name, symbol_type=DataSymbol,
                    datatype=ArrayType(intr_type, [Reference(dim)]))
                sym.interface = ArgumentInterface(
                                        ArgumentInterface.Access.READ)
                self.symtab.append_argument(sym)
                dim = self.symtab.find_or_create(
                    "np_z"+qr_name, symbol_type=DataSymbol,
                    datatype=LFRicTypes("LFRicIntegerScalarDataType")())
                sym = self.symtab.find_or_create(
                    "weights_z"+qr_name, symbol_type=DataSymbol,
                    datatype=ArrayType(intr_type, [Reference(dim)]))
                sym.interface = ArgumentInterface(
                                        ArgumentInterface.Access.READ)
                self.symtab.append_argument(sym)
            elif shape == "gh_quadrature_face":
                dim1 = self.symtab.find_or_create(
                    "np_xyz"+qr_name, symbol_type=DataSymbol,
                    datatype=LFRicTypes("LFRicIntegerScalarDataType")())
                dim2 = self.symtab.find_or_create(
                    "nfaces"+qr_name, symbol_type=DataSymbol,
                    datatype=LFRicTypes("LFRicIntegerScalarDataType")())
                sym = self.symtab.find_or_create(
                    "weights_xyz"+qr_name, symbol_type=DataSymbol,
                    datatype=ArrayType(intr_type, [Reference(dim1),
                                                   Reference(dim2)]))
                sym.interface = ArgumentInterface(
                                        ArgumentInterface.Access.READ)
                self.symtab.append_argument(sym)
            elif shape == "gh_quadrature_edge":
                dim1 = self.symtab.find_or_create(
                    "np_xyz"+qr_name, symbol_type=DataSymbol,
                    datatype=LFRicTypes("LFRicIntegerScalarDataType")())
                dim2 = self.symtab.find_or_create(
                    "nedges"+qr_name, symbol_type=DataSymbol,
                    datatype=LFRicTypes("LFRicIntegerScalarDataType")())
                sym = self.symtab.find_or_create(
                    "weights_xyz"+qr_name, symbol_type=DataSymbol,
                    datatype=ArrayType(intr_type, [Reference(dim1),
                                                   Reference(dim2)]))
                sym.interface = ArgumentInterface(
                                        ArgumentInterface.Access.READ)
                self.symtab.append_argument(sym)

    def invoke_declarations(self):
        '''
        Add basis-function declarations to the PSy layer.

        '''
        super().invoke_declarations()
        const = LFRicConstants()

        # We need BASIS and/or DIFF_BASIS if any kernel requires quadrature
        # or an evaluator
        if self._qr_vars or self._eval_targets:
            module = self.symtab.find_or_create(
                const.FUNCTION_SPACE_TYPE_MAP["function_space"]["module"],
                symbol_type=ContainerSymbol)
            self.symtab.find_or_create(
                "BASIS", symbol_type=DataSymbol, datatype=UnresolvedType(),
                interface=ImportInterface(module))
            self.symtab.find_or_create(
                "DIFF_BASIS", symbol_type=DataSymbol,
                datatype=UnresolvedType(), interface=ImportInterface(module))

        if self._qr_vars:
            # Look-up the module- and type-names from the QUADRATURE_TYPE_MAP
            for shp in self._qr_vars:
                quad_map = const.QUADRATURE_TYPE_MAP[shp]
                module = self.symtab.find_or_create(
                    quad_map["module"],
                    symbol_type=ContainerSymbol)
                self.symtab.find_or_create(
                    quad_map["type"], symbol_type=DataTypeSymbol,
                    datatype=UnresolvedType(),
                    interface=ImportInterface(module))
                self.symtab.find_or_create(
                    quad_map["proxy_type"], symbol_type=DataTypeSymbol,
                    datatype=UnresolvedType(),
                    interface=ImportInterface(module))

        for shape in self._qr_vars.keys():
            # The PSy-layer routine is passed objects of
            # quadrature_* type
            dt_symbol = self.symtab.lookup(
                const.QUADRATURE_TYPE_MAP[shape]["type"])
            for name in self._qr_vars[shape]:
                new_arg = self.symtab.find_or_create(
                    name, symbol_type=DataSymbol, datatype=dt_symbol,
                )
                new_arg.interface = ArgumentInterface(
                    ArgumentInterface.Access.READ)
                self.symtab.append_argument(new_arg)

    def initialise(self, cursor: int) -> int:
        '''
        Create the declarations and assignments required for the
        basis-functions required by an invoke. These are added as children
        of the supplied parent node in the AST.

        :param cursor: position where to add the next initialisation
            statements.

        :returns: Updated cursor value.

        :raises InternalError: if an invalid entry is encountered in the \
                               self._basis_fns list.
        '''
        # pylint: disable=too-many-branches, too-many-locals
        api_config = Config.get().api_conf("lfric")
        const = LFRicConstants()

        # We need BASIS and/or DIFF_BASIS if any kernel requires quadrature
        # or an evaluator
        if self._qr_vars or self._eval_targets:
            module = self.symtab.find_or_create(
                const.FUNCTION_SPACE_TYPE_MAP["function_space"]["module"],
                symbol_type=ContainerSymbol)
            self.symtab.find_or_create(
                "BASIS", symbol_type=DataSymbol, datatype=UnresolvedType(),
                interface=ImportInterface(module))
            self.symtab.find_or_create(
                "DIFF_BASIS", symbol_type=DataSymbol,
                datatype=UnresolvedType(), interface=ImportInterface(module))

        if self._qr_vars:
            init_cursor = cursor
            # Look-up the module- and type-names from the QUADRATURE_TYPE_MAP
            for shp in self._qr_vars:
                quad_map = const.QUADRATURE_TYPE_MAP[shp]
                module = self.symtab.find_or_create(
                    quad_map["module"],
                    symbol_type=ContainerSymbol)
                symbol = self.symtab.lookup(quad_map["type"])
                symbol.interface = ImportInterface(module)
                symbol = self.symtab.lookup(quad_map["proxy_type"])
                symbol.interface = ImportInterface(module)

            cursor = self._initialise_xyz_qr(cursor)
            cursor = self._initialise_xyoz_qr(cursor)
            cursor = self._initialise_xoyoz_qr(cursor)
            cursor = self._initialise_face_or_edge_qr(cursor, "face")
            cursor = self._initialise_face_or_edge_qr(cursor, "edge")

            if init_cursor < cursor:
                self._invoke.schedule[init_cursor].preceding_comment = (
                    "Look-up quadrature variables")

        first = True
        for (fspace, arg) in self._eval_targets.values():
            # We need the list of nodes for each unique FS upon which we need
            # to evaluate basis/diff-basis functions
            nodes_name = "nodes_" + fspace.mangled_name
            kind = api_config.default_kind["real"]
            symbol = self.symtab.new_symbol(
                nodes_name, symbol_type=DataSymbol,
                datatype=UnsupportedFortranType(
                    f"real(kind={kind}), pointer :: {nodes_name}"
                    f"(:,:) => null()",
                    partial_datatype=ArrayType(
                        LFRicTypes("LFRicRealScalarDataType")(),
                        [ArrayType.Extent.DEFERRED]*2)
                    ))
            assignment = Assignment.create(
                    lhs=Reference(symbol),
                    rhs=arg.generate_method_call(
                        "get_nodes", function_space=fspace),
                    is_pointer=True)
            if first:
                assignment.preceding_comment = (
                    "Initialise evaluator-related quantities for the target "
                    "function spaces")
                first = False
            self._invoke.schedule.addchild(assignment, cursor)
            cursor += 1

        init_cursor = cursor
        var_dim_list = []
        for basis_fn in self._basis_fns:
            # Get the extent of the first dimension of the basis array.
            if basis_fn['type'] == "basis":
                first_dim = self.basis_first_dim_name(basis_fn["fspace"])
                dim_space = "get_dim_space"
            elif basis_fn['type'] == "diff-basis":
                first_dim = self.diff_basis_first_dim_name(
                    basis_fn["fspace"])
                dim_space = "get_dim_space_diff"
            else:
                raise InternalError(
                    f"Unrecognised type of basis function: "
                    f"'{basis_fn['''type''']}'. Should be either 'basis' or "
                    f"'diff-basis'.")

            if first_dim not in var_dim_list:
                var_dim_list.append(first_dim)
                symbol = self.symtab.find_or_create(
                    first_dim, symbol_type=DataSymbol,
                    datatype=LFRicTypes("LFRicIntegerScalarDataType")())

                assignment = Assignment.create(
                        lhs=Reference(symbol),
                        rhs=basis_fn["arg"].generate_method_call(
                                dim_space, function_space=basis_fn["fspace"]))
                self._invoke.schedule.addchild(assignment, cursor)
                cursor += 1

        _, basis_arrays = self._basis_fn_declns()

        # Allocate basis arrays
        for basis in basis_arrays:
            dims = "("+",".join([":"]*len(basis_arrays[basis]))+")"
            symbol = self.symtab.find_or_create(
                basis, symbol_type=DataSymbol, datatype=UnsupportedFortranType(
                    f"real(kind=r_def), allocatable :: {basis}{dims}"
                ))
            alloc = IntrinsicCall.create(
                IntrinsicCall.Intrinsic.ALLOCATE,
                [ArrayReference.create(
                    symbol,
                    [Reference(self.symtab.find_or_create(
                                bn, symbol_type=DataSymbol,
                                datatype=UnresolvedType()))
                     for bn in basis_arrays[basis]])]
            )
            self._invoke.schedule.addchild(alloc, cursor)
            cursor += 1

        # Compute the values for any basis arrays
        cursor = self._compute_basis_fns(cursor)
        if init_cursor < cursor:
            self._invoke.schedule[init_cursor].preceding_comment = (
                "Allocate basis/diff-basis arrays")
        return cursor

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

    def _initialise_xyz_qr(self, cursor: int) -> int:
        '''
        Add in the initialisation of variables needed for XYZ
        quadrature

        :param cursor: position where to add the next initialisation
            statements.

        :returns: Updated cursor value.

        '''
        # pylint: disable=unused-argument
        # This shape is not yet supported so we do nothing
        return cursor

    def _initialise_xyoz_qr(self, cursor):
        '''
        Add in the initialisation of variables needed for XYoZ
        quadrature

        :param int cursor: position where to add the next initialisation
            statements.
        :returns: Updated cursor value.
        :rtype: int

        '''
        const = LFRicConstants()

        if "gh_quadrature_xyoz" not in self._qr_vars:
            return cursor

        for qr_arg_name in self._qr_vars["gh_quadrature_xyoz"]:

            # We generate unique names for the integers holding the numbers
            # of quadrature points by appending the name of the quadrature
            # argument
            for name in self.qr_dim_vars["xyoz"]:
                self.symtab.new_symbol(
                    name+"_"+qr_arg_name, symbol_type=DataSymbol,
                    datatype=LFRicTypes("LFRicIntegerScalarDataType")())
            dtype = \
                const.QUADRATURE_TYPE_MAP["gh_quadrature_xyoz"]["intrinsic"]
            kind = const.QUADRATURE_TYPE_MAP["gh_quadrature_xyoz"]["kind"]
            for name in self.qr_weight_vars["xyoz"]:
                self.symtab.find_or_create(
                    name+"_"+qr_arg_name, symbol_type=DataSymbol,
                    datatype=UnsupportedFortranType(
                        f"{dtype}(kind={kind}), pointer :: "
                        f"{name}_{qr_arg_name}(:) => null()"))

            # Get the quadrature proxy
            dtp_symbol = self.symtab.lookup(
                const.QUADRATURE_TYPE_MAP["gh_quadrature_xyoz"]["proxy_type"])
            proxy_symbol = self.symtab.find_or_create(
                    qr_arg_name+"_proxy", symbol_type=DataSymbol,
                    datatype=dtp_symbol)
            symbol = self.symtab.lookup(qr_arg_name)

            assignment = Assignment.create(
                    lhs=Reference(proxy_symbol),
                    rhs=Call.create(
                        StructureReference.create(
                            symbol, ['get_quadrature_proxy'])))
            self._invoke.schedule.addchild(assignment, cursor)
            cursor += 1

            # Number of points in each dimension
            for qr_var in self.qr_dim_vars["xyoz"]:
                self._invoke.schedule.addchild(
                    Assignment.create(
                        lhs=Reference(self.symtab.lookup(
                                qr_var+"_"+qr_arg_name)),
                        rhs=StructureReference.create(
                                proxy_symbol, [qr_var])),
                    cursor)
                cursor += 1

            # Pointers to the weights arrays
            for qr_var in self.qr_weight_vars["xyoz"]:
                self._invoke.schedule.addchild(
                    Assignment.create(
                        lhs=Reference(self.symtab.lookup(
                                qr_var+"_"+qr_arg_name)),
                        rhs=StructureReference.create(
                                proxy_symbol, [qr_var]),
                        is_pointer=True),
                    cursor)
                cursor += 1

        return cursor

    def _initialise_xoyoz_qr(self, cursor: int) -> int:
        '''
        Add in the initialisation of variables needed for XoYoZ
        quadrature.

        :param cursor: position where to add the next initialisation
            statements.

        :returns: Updated cursor value.

        '''
        # pylint: disable=unused-argument
        # This shape is not yet supported so we do nothing
        return cursor

    def _initialise_face_or_edge_qr(self, cursor, qr_type):
        '''
        Add in the initialisation of variables needed for face or edge
        quadrature.

        :param int cursor: position where to add the next initialisation
            statements.
        :param str qr_type: whether to generate initialisation code for \
                            "face" or "edge" quadrature.
        :returns: Updated cursor value.
        :rtype: int

        :raises InternalError: if `qr_type` is not "face" or "edge".

        '''
        if qr_type not in ["face", "edge"]:
            raise InternalError(
                f"_initialise_face_or_edge_qr: qr_type argument must be "
                f"either 'face' or 'edge' but got: '{qr_type}'")

        quadrature_name = f"gh_quadrature_{qr_type}"

        if quadrature_name not in self._qr_vars:
            return cursor

        for qr_arg_name in self._qr_vars[quadrature_name]:

            arg_symbol = self.symtab.lookup(qr_arg_name)
            arg_symbol.interface = ArgumentInterface(
                                    ArgumentInterface.Access.READ)
            self.symtab.append_argument(arg_symbol)

            # We generate unique names for the integers holding the numbers
            # of quadrature points by appending the name of the quadrature
            # argument
            for name in self.qr_dim_vars[qr_type]:
                self.symtab.find_or_create(
                    name+"_"+qr_arg_name, tag=name+"_"+qr_arg_name,
                    symbol_type=DataSymbol,
                    datatype=LFRicTypes("LFRicIntegerScalarDataType")())

            array_type = ArrayType(
                LFRicTypes("LFRicRealScalarDataType")(),
                [ArrayType.Extent.DEFERRED]*2)
            for name in self.qr_weight_vars[qr_type]:
                self.symtab.find_or_create(
                    f"{name}_{qr_arg_name}", symbol_type=DataSymbol,
                    datatype=UnsupportedFortranType(
                        f"real(kind=r_def), pointer, dimension(:,:) :: "
                        f"{name}_{qr_arg_name} => null()\n",
                        partial_datatype=array_type
                    ),
                    tag=f"{name}_{qr_arg_name}")
            const = LFRicConstants()

            # Get the quadrature proxy
            ptype = self.symtab.lookup(
                const.QUADRATURE_TYPE_MAP[quadrature_name]["proxy_type"])

            proxy_sym = self.symtab.find_or_create_tag(
                qr_arg_name+"_proxy", symbol_type=DataSymbol, datatype=ptype)
            call = Call.create(
                StructureReference.create(
                    self.symtab.lookup(qr_arg_name),
                    ["get_quadrature_proxy"]))
            assignment = Assignment.create(
                    lhs=Reference(proxy_sym),
                    rhs=call)
            self._invoke.schedule.addchild(assignment, cursor)
            cursor += 1

            # The dimensioning variables required for this quadrature
            # (e.g. nedges/nfaces, np_xyz)
            for qr_var in self.qr_dim_vars[qr_type]:
                qr_sym = self.symtab.lookup(qr_var+'_'+qr_arg_name)
                assignment = Assignment.create(
                        lhs=Reference(qr_sym),
                        rhs=StructureReference.create(proxy_sym, [qr_var]))
                self._invoke.schedule.addchild(assignment, cursor)
                cursor += 1

            # Pointers to the weights arrays
            for qr_var in self.qr_weight_vars[qr_type]:
                qr_sym = self.symtab.lookup(qr_var+'_'+qr_arg_name)
                assignment = Assignment.create(
                        lhs=Reference(qr_sym),
                        rhs=StructureReference.create(
                            proxy_sym, [qr_var]),
                        is_pointer=True)
                self._invoke.schedule.addchild(assignment, cursor)
                cursor += 1

        return cursor

    def _compute_basis_fns(self, cursor):
        '''
        Generates the necessary Fortran to compute the values of
        any basis/diff-basis arrays required

        :param int cursor: position where to add the next initialisation
            statements.
        :returns: Updated cursor value.
        :rtype: int

        '''
        # pylint: disable=too-many-locals
        const = LFRicConstants()

        loop_var_list = set()
        op_name_list = []

        # add calls to compute the values of any basis arrays
        first = True
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
                args = [Reference(self.symtab.lookup(basis_type)),
                        basis_fn["arg"].generate_accessor(basis_fn["fspace"]),
                        Reference(self.symtab.lookup(first_dim)),
                        Reference(self.symtab.lookup(
                            basis_fn["fspace"].ndf_name)),
                        Reference(self.symtab.lookup(op_name))]

                # insert the basis array call
                call = Call.create(
                    StructureReference.create(
                        self.symtab.lookup(basis_fn["qr_var"]),
                        ["compute_function"]),
                    args)
                if first:
                    call.preceding_comment = "Compute basis/diff-basis arrays"
                    first = False
                self._invoke.schedule.addchild(call, cursor)
                cursor += 1
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
                    symbol = self.symtab.find_or_create_tag(
                        nodal_loop_var,
                        symbol_type=DataSymbol,
                        datatype=LFRicTypes("LFRicIntegerScalarDataType")())
                    loop = Loop.create(
                            symbol, Literal('1', INTEGER_TYPE),
                            Reference(self.symtab.lookup(space.ndf_name)),
                            Literal('1', INTEGER_TYPE), [])
                    if first:
                        loop.preceding_comment = (
                            "Compute basis/diff-basis arrays")
                        first = False
                    self._invoke.schedule.addchild(loop, cursor)
                    cursor += 1

                    dof_loop_var = "df_" + basis_fn["fspace"].mangled_name
                    loop_var_list.add(dof_loop_var)

                    symbol = self.symtab.find_or_create_tag(
                        dof_loop_var,
                        symbol_type=DataSymbol,
                        datatype=LFRicTypes("LFRicIntegerScalarDataType")())
                    inner_loop = Loop.create(
                            symbol, Literal('1', INTEGER_TYPE),
                            Reference(self.symtab.lookup(
                                        basis_fn["fspace"].ndf_name)),
                            Literal('1', INTEGER_TYPE), [])
                    loop.loop_body.addchild(inner_loop)

                    symbol = self.symtab.lookup(op_name)
                    rhs = basis_fn['arg'].generate_method_call(
                        "call_function", function_space=basis_fn['fspace'])
                    rhs.addchild(Reference(self.symtab.lookup(basis_type)))
                    rhs.addchild(Reference(self.symtab.lookup(dof_loop_var)))
                    rhs.addchild(ArrayReference.create(
                            self.symtab.lookup(f"nodes_{space.mangled_name}"),
                            [":", Reference(self.symtab.lookup(
                                                    nodal_loop_var))]))
                    inner_loop.loop_body.addchild(
                        Assignment.create(
                            lhs=ArrayReference.create(symbol, [
                                ":",
                                Reference(self.symtab.lookup(
                                    f"df_{basis_fn['fspace'].mangled_name}")),
                                Reference(self.symtab.lookup("df_nodal"))
                            ]),
                            rhs=rhs))
            else:
                raise InternalError(
                    f"Unrecognised shape '{basis_fn['''shape''']}' specified "
                    f"for basis function. Should be one of: "
                    f"{const.VALID_EVALUATOR_SHAPES}")
        return cursor

    def deallocate(self):
        '''
        Add code (at the end of the Invoke Schedule) to deallocate all
        basis/diff-basis function arrays.

        :raises InternalError: if an unrecognised type of basis function
                               is encountered.
        '''

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

        first = True
        if func_space_var_names:
            # add the required deallocate call
            dealloc = IntrinsicCall.create(
                IntrinsicCall.Intrinsic.DEALLOCATE,
                [Reference(self.symtab.lookup(name)) for name in
                 sorted(func_space_var_names)]
            )
            if first:
                dealloc.preceding_comment = "Deallocate basis arrays"
            self._invoke.schedule.children.append(dealloc)


class LFRicBoundaryConditions(LFRicCollection):
    '''
    Manages declarations and initialisation of quantities required by
    kernels that need boundary condition information.

    :param node: the Invoke or Kernel stub for which we are to handle \
                 any boundary conditions.
    :type node: :py:class:`psyclone.domain.lfric.LFRicInvoke` or \
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
        for call in self.kernel_calls:
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

    def invoke_declarations(self):
        '''
        Add declarations for any boundary-dofs arrays required by an Invoke.

        '''
        super().invoke_declarations()
        api_config = Config.get().api_conf("lfric")

        for dofs in self._boundary_dofs:
            name = "boundary_dofs_" + dofs.argument.name
            kind = api_config.default_kind["integer"]
            dtype = UnsupportedFortranType(
                f"integer(kind={kind}), pointer "
                f":: {name}(:,:) => null()",
                partial_datatype=ArrayType(
                    LFRicTypes("LFRicIntegerScalarDataType")(),
                    [ArrayType.Extent.DEFERRED]*2)
                )
            self.symtab.new_symbol(
                name,
                symbol_type=DataSymbol,
                datatype=dtype)

    def stub_declarations(self):
        '''
        Add declarations for any boundary-dofs arrays required by a kernel.
        Note that argument order is redefined later by ArgOrdering.

        '''
        super().stub_declarations()
        for dofs in self._boundary_dofs:
            name = "boundary_dofs_" + dofs.argument.name
            ndf_name = self.symtab.lookup(dofs.function_space.ndf_name)
            dtype = ArrayType(
                LFRicTypes("LFRicIntegerScalarDataType")(),
                [Reference(ndf_name), Literal("2", INTEGER_TYPE)])
            new_symbol = self.symtab.new_symbol(
                name,
                symbol_type=DataSymbol,
                datatype=dtype,
                interface=ArgumentInterface(
                    ArgumentInterface.Access.READ)
            )
            self.symtab.append_argument(new_symbol)

    def initialise(self, cursor):
        '''
        Initialise any boundary-dofs arrays required by an Invoke.

        :param int cursor: position where to add the next initialisation
            statements.
        :returns: Updated cursor value.
        :rtype: int

        '''
        for dofs in self._boundary_dofs:
            name = "boundary_dofs_" + dofs.argument.name
            self._invoke.schedule.addchild(
                Assignment.create(
                    lhs=Reference(self.symtab.lookup(name)),
                    rhs=dofs.argument.generate_method_call(
                        "get_boundary_dofs",
                        function_space=dofs.function_space),
                    is_pointer=True
                ),
                cursor)
            cursor += 1

        return cursor


class LFRicGlobalSum(GlobalSum):
    '''
    LFRic specific global sum class which can be added to and
    manipulated in a schedule.

    :param scalar: the kernel argument for which to perform a global sum.
    :type scalar: :py:class:`psyclone.lfric.LFRicKernelArgument`
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
                "It makes no sense to create an LFRicGlobalSum object when "
                "distributed memory is not enabled (dm=False).")
        # Check that the global sum argument is indeed a scalar
        if not scalar.is_scalar:
            raise InternalError(
                f"LFRicGlobalSum.init(): A global sum argument should be a "
                f"scalar but found argument of type '{scalar.argument_type}'.")
        # Check scalar intrinsic types that this class supports (only
        # "real" for now)
        if scalar.intrinsic_type != "real":
            raise GenerationError(
                f"LFRicGlobalSum currently only supports real scalars, but "
                f"argument '{scalar.name}' in Kernel '{scalar.call.name}' has "
                f"'{scalar.intrinsic_type}' intrinsic type.")
        # Initialise the parent class
        super().__init__(scalar, parent=parent)

    def lower_to_language_level(self):
        '''
        :returns: this node lowered to language-level PSyIR.
        :rtype: :py:class:`psyclone.psyir.nodes.Node`
        '''

        # Get the name strings to use
        name = self._scalar.name
        type_name = self._scalar.data_type
        mod_name = self._scalar.module_name

        # Get the symbols from the given names
        symtab = self.ancestor(InvokeSchedule).symbol_table
        sum_mod = symtab.find_or_create(mod_name, symbol_type=ContainerSymbol)
        sum_type = symtab.find_or_create(type_name,
                                         symbol_type=DataTypeSymbol,
                                         datatype=UnresolvedType(),
                                         interface=ImportInterface(sum_mod))
        sum_name = symtab.find_or_create_tag("global_sum",
                                             symbol_type=DataSymbol,
                                             datatype=sum_type)
        tmp_var = symtab.lookup(name)

        # Create the assignments
        assign1 = Assignment.create(
            lhs=StructureReference.create(sum_name, ["value"]),
            rhs=Reference(tmp_var)
        )
        assign1.preceding_comment = "Perform global sum"
        self.parent.addchild(assign1, self.position)
        assign2 = Assignment.create(
            lhs=Reference(tmp_var),
            rhs=Call.create(StructureReference.create(sum_name, ["get_sum"]))
        )
        return self.replace_with(assign2)


def _create_depth_list(halo_info_list, parent):
    '''Halo exchanges may have more than one dependency. This method
    simplifies multiple dependencies to remove duplicates and any
    obvious redundancy. For example, if one dependency is for depth=1
    and another for depth=2 then we do not need the former as it is
    covered by the latter. Similarly, if we have a depth=extent+1 and
    another for depth=extent+2 then we do not need the former as it is
    covered by the latter. It also takes into account
    needs_clean_outer, which indicates whether the outermost halo
    needs to be clean (and therefore whether there is a dependence).

    :param halo_info_list: a list containing halo access information
        derived from all read fields dependent on this halo exchange.
    :type: list[:py:class:`psyclone.lfric.HaloReadAccess`]
    :param parent: the parent PSyIR node of the related halo exchange.
    :type parent: :py:class:`psyclone.psyir.nodes.Node`

    :returns: a list containing halo depth information derived from
        the halo access information.
    :rtype: list[:py:class:`psyclone.lfric.HaloDepth]`

    '''
    # pylint: disable=too-many-branches
    depth_info_list = []
    # First look to see if all field dependencies are
    # annexed_only. If so we only care about annexed dofs.
    annexed_only = True
    for halo_info in halo_info_list:
        if not (halo_info.annexed_only or
                (halo_info.var_depth == Literal("1", INTEGER_TYPE)
                 and not halo_info.needs_clean_outer)):
            # There are two cases when we only care about accesses to
            # annexed dofs. 1) when annexed_only is set and 2) when
            # the halo depth is 1 but we only depend on annexed dofs
            # being up-to-date (needs_clean_outer is False).
            annexed_only = False
            break
    if annexed_only:
        depth_info = HaloDepth(parent)
        depth_info.set_by_value(max_depth=False,
                                var_depth=Literal("1", INTEGER_TYPE),
                                annexed_only=True,
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
                depth_info = HaloDepth(parent)
                depth_info.set_by_value(max_depth=True, var_depth=None,
                                        annexed_only=False, max_depth_m1=False)
                return [depth_info]
            # Remember that we found a max_depth-1 access.
            max_depth_m1 = True

    if max_depth_m1:
        # We have at least one max_depth-1 access.
        depth_info = HaloDepth(parent)
        depth_info.set_by_value(max_depth=False, var_depth=None,
                                annexed_only=False, max_depth_m1=True)
        depth_info_list.append(depth_info)

    for halo_info in halo_info_list:
        # Go through the halo information associated with each
        # read dependency, skipping any max_depth-1 accesses.
        if halo_info.max_depth and not halo_info.needs_clean_outer:
            continue
        var_depth = halo_info.var_depth
        if var_depth and not halo_info.needs_clean_outer:
            # Decrease depth by 1 if we don't care about the outermost
            # access.
            var_depth = BinaryOperation.create(
                BinaryOperation.Operator.SUB,
                var_depth.copy(), Literal("1", INTEGER_TYPE))

        # check whether we match with existing depth information
        for depth_info in depth_info_list:
            if depth_info.var_depth == var_depth:
                # This dependence has exactly the same depth as an existing one
                # so no need to add a new HaloDepth.
                break
        else:
            # No matches were found with existing entries so create a
            # new one if 'var_depth' is set.
            if var_depth:
                depth_info = HaloDepth(parent)
                depth_info.set_by_value(max_depth=False, var_depth=var_depth,
                                        annexed_only=False, max_depth_m1=False)
                depth_info_list.append(depth_info)
    return depth_info_list


class LFRicHaloExchange(HaloExchange):
    '''LFRic-specific halo exchange class which can be added to and
    manipulated in a schedule.

    :param field: the field that this halo exchange will act on
    :type field: :py:class:`psyclone.lfric.LFRicKernelArgument`
    :param bool check_dirty: optional argument default True indicating
        whether this halo exchange should be subject to a run-time check
        for clean/dirty halos.
    :param vector_index: optional vector index (default None) to identify
        identify which index of a vector field this halo exchange is
        responsible for.
    :type vector_index: int
    :param parent: PSyIR parent node of this object.
    :type parent: Optional[:py:class:`psyclone.psyir.nodes.Node`]

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

        :return: the PSyIR for the halo exchange depth.
        :rtype: :py:class:`psyclone.psyir.nodes.Node`

        '''
        depth_info_list = self._compute_halo_read_depth_info()
        if len(depth_info_list) == 1:
            psyir = depth_info_list[0].psyir_expression()
        else:
            psyir = IntrinsicCall.create(
                IntrinsicCall.Intrinsic.MAX,
                [depth.psyir_expression() for depth in depth_info_list])

        # Simplify the resulting expression. We need to create a fake
        # Assignment to temporarily host the expression.
        sym_maths = SymbolicMaths.get()
        fake_assign = Assignment.create(
            Reference(DataSymbol("tmp", INTEGER_TYPE)), psyir)
        self.parent.addchild(fake_assign)

        sym_maths.expand(fake_assign.rhs)
        depth_expr = fake_assign.rhs.detach()
        fake_assign.detach()
        return depth_expr

    def _compute_halo_read_depth_info(self, ignore_hex_dep=False):
        '''Take a list of `psyclone.lfric.HaloReadAccess` objects and
        create an equivalent list of `psyclone.lfric.HaloDepth`
        objects. Whilst doing this we simplify the
        `psyclone.lfric.HaloDepth` list to remove redundant depth
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
        :rtype: :func:`list` of :py:class:`psyclone.lfric.HaloDepth`

        '''
        # get our halo information
        halo_info_list = self._compute_halo_read_info(ignore_hex_dep)
        # use the halo information to generate depth information
        depth_info_list = _create_depth_list(halo_info_list, self)
        return depth_info_list

    def _compute_halo_read_info(self,
                                ignore_hex_dep: bool = False
                                ) -> List[HaloReadAccess]:
        '''Dynamically computes all halo read dependencies and returns the
        required halo information (i.e. halo depth and stencil type)
        in a list of HaloReadAccess objects. If the optional
        ignore_hex_dep argument is set to True then any read accesses
        contained in halo exchange nodes are ignored. This option can
        therefore be used to filter out any halo exchange dependencies
        and only return non-halo exchange dependencies if and when
        required.

        :param ignore_hex_dep: if True then ignore any read
            accesses contained in halo exchanges. This is an optional
            argument that defaults to False.

        :return: halo information for each read dependency.

        :raises InternalError: if there is more than one read
            dependency associated with a halo exchange.
        :raises InternalError: if there is a read dependency
            associated with a halo exchange and it is not the last
            entry in the read dependency list.
        :raises GenerationError: if there is a read dependency
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
        return [HaloReadAccess(read_dependency, self._parent) for
                read_dependency in read_dependencies]

    def _compute_halo_write_info(self):
        '''Determines how much of the halo has been cleaned from any previous
        redundant computation.

        :return: a HaloWriteAccess object containing the required
            information, or None if no dependence information is found.
        :rtype: :py:class:`psyclone.lfric.HaloWriteAccess` | None

        :raises GenerationError: if more than one write dependence is
            found for this halo exchange as this should not be possible.

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
        return HaloWriteAccess(write_dependencies[0], parent=self)

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

        :returns: (x, y) where x specifies whether this halo
            exchange is (or might be) required - True, or is not
            required - False. If the first tuple item is True then the
            second argument specifies whether we definitely know that
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

        if (Config.get().api_conf("lfric").compute_annexed_dofs and
                len(required_clean_info) == 1 and
                required_clean_info[0].annexed_only):
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

        # At this point we know that clean_info.max_depth is False
        clean_depth = clean_info.clean_depth

        if not clean_depth:
            if clean_info.dirty_outer:
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
            else:
                # if clean_depth is 0 then the writer does not
                # redundantly compute so we definitely need the halo
                # exchange
                required = True
                known = True
                return required, known

        # We have a clean_depth so iterate over the various required depths
        # to see whether or not they are all satisfied.
        required = False
        known = False
        for required_clean in required_clean_info:
            if required_clean.max_depth or required_clean.max_depth_m1:
                required = True
                known = False
                return required, known
            left_dirty = SymbolicMaths.greater_than(
                required_clean.var_depth, clean_depth,
                all_variables_positive=True)
            if left_dirty == SymbolicMaths.Fuzzy.FALSE:
                # Nothing is left dirty so no hexch required for this
                # read access.
                continue
            if left_dirty == SymbolicMaths.Fuzzy.TRUE:
                # Halo definitely left dirty to a certain depth.
                required = True
                known = True
                return required, known
            # Otherwise, we can't be sure. Continue looking at the other
            # read accesses in case one of them is definitely not satisfied.
            required = True
            continue

        # If we get to here then we may or may not require a halo exchange.
        # If we do need one, then we aren't certain about it (as we'd already
        # have returned if we were).
        if not required:
            # We know we don't require a halo exchange.
            known = True

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
        field_id = self._field.name_indexed
        return (f"{self.coloured_name(colour)}[field='{field_id}', "
                f"type='{self._compute_stencil_type()}', "
                f"depth={self._compute_halo_depth().debug_string()}, "
                f"check_dirty={runtime_check}]")

    def lower_to_language_level(self):
        '''
        :returns: this node lowered to language-level PSyIR.
        :rtype: :py:class:`psyclone.psyir.nodes.Node`
        '''
        symbol = DataSymbol(self._field.proxy_name, UnresolvedType())
        method = self._halo_exchange_name
        depth_expr = self._compute_halo_depth()

        # Create infrastructure Calls
        if self.vector_index:
            idx = Literal(str(self.vector_index), INTEGER_TYPE)
            if_condition = Call.create(
                ArrayOfStructuresReference.create(symbol, [idx], ['is_dirty']),
                [('depth', depth_expr.copy())])
            if_body = Call.create(
                ArrayOfStructuresReference.create(
                    symbol, [idx.copy()], [method]),
                [('depth', depth_expr.copy())])
        else:
            if_condition = Call.create(
                StructureReference.create(symbol, ['is_dirty']),
                [('depth', depth_expr.copy())])
            if_body = Call.create(
                StructureReference.create(symbol, [method]),
                [('depth', depth_expr.copy())])

        # Add the "if_dirty" check when necessary
        _, known = self.required()
        if not known:
            haloex = IfBlock.create(if_condition, [if_body])
        else:
            haloex = if_body

        haloex.preceding_comment = self.preceding_comment
        self.replace_with(haloex)
        return haloex


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
    :type field: :py:class:`psyclone.lfric.LFRicKernelArgument`
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
        :rtype: :py:class:`psyclone.lfric.LFRicHaloExchangeEnd`
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
    :type field: :py:class:`psyclone.lfric.LFRicKernelArgument`
    :param check_dirty: optional argument (default True) indicating
        whether this halo exchange should be subject to a run-time check
        for clean/dirty halos.
    :type check_dirty: bool
    :param vector_index: optional vector index (default None) to
        identify which index of a vector field this halo exchange is
        responsible for.
    :type vector_index: int
    :param parent: PSyIR parent node (default None) of this object.
    :type parent: Optional[:py:class:`psyclone.psyir.nodes.Node`]

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

    :param parent: the parent PSyIR node of the region containing the field
                   access that is represented.
    :type parent: :py:class:`psyclone.psyir.nodes.Node`

    :raises TypeError: if the parent argument is not a Node.

    '''
    def __init__(self, parent):
        # var_depth is used to store the PSyIR of the expression holding
        # depth of halo that is accessed.
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
        # TODO #2503: This can become invalid if the HaloExchange
        # containing this HaloDepth changes its ancestors.
        if not isinstance(parent, Node):
            raise TypeError(
                f"The HaloDepth parent argument must be a Node, but found: "
                f"{type(parent).__name__}"
            )
        self._parent = parent

    @property
    def annexed_only(self):
        '''
        :returns: True if only annexed dofs are accessed in the halo and
                  False otherwise.
        :rtype: bool
        '''
        return self._annexed_only

    @property
    def max_depth(self):
        '''
        :returns: True if the read to the field is known to access all
                  of the halo and False otherwise.
        :rtype: bool
        '''
        return self._max_depth

    @property
    def max_depth_m1(self):
        '''Returns whether the read to the field is known to access all of the
        halo except the outermost level or not.

        :returns: True if the read to the field is known to access all
                  of the halo except the outermost and False otherwise.
        :rtype: bool

        '''
        return self._max_depth_m1

    @property
    def var_depth(self):
        '''Returns the PSyIR of the expression specifying the depth of halo
        access if one is provided.

        :returns: PSyIR specifying the halo access depth
                  if one exists, and None if not.
        :rtype: :py:class:`psyclone.psyir.nodes.Statement`

        '''
        return self._var_depth

    def set_by_value(self,
                     max_depth: bool,
                     var_depth: Optional[DataNode],
                     annexed_only: bool,
                     max_depth_m1: bool):
        # pylint: disable=too-many-arguments
        '''Set halo depth information directly.

        :param max_depth: True if the field accesses all of the
            halo and False otherwise
        :param var_depth: PSyIR expression specifying the halo
            access depth, if one exists, and None if not
        :param annexed_only: True if only the halo's annexed dofs
            are accessed and False otherwise
        :param max_depth_m1: True if the field accesses all of the halo but
            does not require the outermost halo to be correct, False otherwise

        '''
        self._max_depth = max_depth
        self._annexed_only = annexed_only
        self._max_depth_m1 = max_depth_m1
        self._var_depth = None

        if not var_depth:
            return

        # Simplify any provided PSyIR expression.
        sym_maths = SymbolicMaths.get()
        # In order to be able to apply the SymbolicMaths.expand() method we
        # have to create a fake Assignment and temporarily graft it into the
        # tree.
        fake_assign = Assignment.create(
            Reference(DataSymbol("tmp", INTEGER_TYPE)), var_depth.copy())
        sched = self._parent.ancestor(Schedule, include_self=True)
        sched.addchild(fake_assign)

        sym_maths.expand(fake_assign.rhs)
        self._var_depth = fake_assign.rhs.detach()
        fake_assign.detach()

    def psyir_expression(self):
        '''
        :returns: the PSyIR expression representing this HaloDepth.
        :rtype: :py:class:`psyclone.psyir.nodes.Node`
        '''
        table = self._parent.scope.symbol_table
        if self.max_depth:
            max_depth = table.lookup_with_tag("max_halo_depth_mesh")
            return Reference(max_depth)
        if self.max_depth_m1:
            max_depth = table.lookup_with_tag("max_halo_depth_mesh")
            return BinaryOperation.create(
                        BinaryOperation.Operator.SUB,
                        Reference(max_depth),
                        Literal('1', INTEGER_TYPE))
        if self.var_depth:
            return self.var_depth.copy()

        return None


def halo_check_arg(field: LFRicKernelArgument,
                   access_types: list[AccessType]) -> Kern:
    '''
    Support function which performs checks to ensure the first argument
    is a field, that the field is contained within Kernel or Builtin
    call and that the field is accessed in one of the ways specified
    by the second argument. If no error is reported it returns the
    call object containing this argument.

    :param field: the argument object we are checking
    :param access_types: List of allowed access types.

    :return: the call containing the argument object

    :raises GenerationError: if the first argument to this function is
        the wrong type.
    :raises GenerationError: if the first argument is not accessed in one of
        the ways specified by the second argument to the function.
    :raises GenerationError: if the first argument is not contained
        within a call object.

    '''
    try:
        # Get the kernel/built-in call associated with this field
        call = field.call
    except AttributeError as err:
        raise GenerationError(
            f"HaloInfo class expects an argument of type "
            f"LFRicKernelArgument, or equivalent, on initialisation, but "
            f"found, '{type(field)}'") from err

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
    :param parent: the parent PSyIR node associated with the scoping region
                   that contains this halo access.

    '''
    def __init__(self, field: LFRicKernelArgument, parent: Node):
        super().__init__(parent)
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

    @property
    def clean_depth(self):
        '''
        Returns the depth to which this halo is clean following the write.

        :returns: PSyIR for the expression for the clean halo depth or None.
        :rtype: :py:class:`psyclone.psyir.nodes.Node` | NoneType

        '''
        if self.var_depth:
            halo_depth = self.var_depth
        elif self.max_depth:
            # halo accesses(s) is/are to the full halo
            # depth (-1 if continuous)
            table = self._parent.scope.symbol_table
            halo_depth = Reference(
                table.lookup_with_tag("max_halo_depth_mesh"))
        else:
            return None

        if self.dirty_outer:
            halo_depth = BinaryOperation.create(
                BinaryOperation.Operator.SUB,
                halo_depth.copy(), Literal("1", INTEGER_TYPE))

        sym_maths = SymbolicMaths.get()
        fake_assign = Assignment.create(
            Reference(DataSymbol("tmp", INTEGER_TYPE)),
            halo_depth)
        sched = self._parent.ancestor(Schedule)
        sched.addchild(fake_assign)

        sym_maths.expand(fake_assign.rhs)
        depth_expr = fake_assign.rhs.detach()
        fake_assign.detach()
        if depth_expr == Literal("0", INTEGER_TYPE):
            return None
        return depth_expr

    def _compute_from_field(self, field: LFRicKernelArgument):
        '''Internal method to compute what parts of a field's halo are written
        to in a certain kernel and loop. The information computed is the depth
        of access and validity of the data after writing. The depth of access
        can be the maximum halo depth or a specified (literal or variable)
        depth and the outer halo layer that is written to may be dirty or
        clean.

        :param field: the field that we are concerned with.

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
            loop.iteration_space.endswith("cell_column") and
            loop.upper_bound_name in const.HALO_ACCESS_LOOP_BOUNDS)
        depth = None
        max_depth = False
        if loop.upper_bound_name in const.HALO_ACCESS_LOOP_BOUNDS:
            # loop does redundant computation
            if loop.upper_bound_halo_depth:
                # loop redundant computation is to a certain depth.
                depth = loop.upper_bound_halo_depth
            else:
                # loop redundant computation is to the maximum depth
                max_depth = True
        # If this is an inter-grid kernel and we're writing to the
        # field on the fine mesh then the halo depth is effectively
        # doubled
        if call.is_intergrid and field.mesh == "gh_fine" and depth:
            depth = BinaryOperation.create(
                BinaryOperation.Operator.MUL,
                Literal("2", INTEGER_TYPE), depth.copy())
        # The third argument for set_by_value gives the PSyIR of the
        # expression specifying the depth.
        var_depth = depth
        # The fourth argument for set_by_value indicates whether
        # we only access annexed_dofs. At the moment this is not possible when
        # modifying a field so we always supply False. The fifth
        # argument indicates if the depth of access is the
        # maximum-1. This is not possible here so we supply False.
        HaloDepth.set_by_value(self, max_depth, var_depth,
                               False, False)


class HaloReadAccess(HaloDepth):
    '''Determines how much of a field's halo is read (the halo depth) and
    additionally the access pattern (the stencil) when a field is
    accessed in a particular kernel within a particular loop nest.

    :param field: the field for which we want information.
    :type field: :py:class:`psyclone.lfric.LFRicKernelArgument`
    :param parent: the node where this HaloDepth belongs.
    :type parent: :py:class:`psyclone.psyir.node.Node`

    '''
    def __init__(self, field, parent=None):
        super().__init__(parent)
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

    def _compute_from_field(self, field: LFRicKernelArgument):
        '''Internal method to compute which parts of a field's halo are read
        in a certain kernel and loop. The information computed is the
        depth of access and the access pattern. The depth of access
        can be the maximum halo depth or a PSyIR expression specifying the
        depth. The access pattern will only be specified if the kernel code
        performs a stencil access on the field.

        For a standard kernel that has been transformed to perform redundant
        computation and has a field argument with a stencil access, the depth
        of that field's halo that is read is computed as the *sum* of the
        depth of the redundant computation and the stencil size. However, for
        the special case of a Kernel which *must* iterate into the halo region
        (has an ``OPERATES_ON`` of ``HALO_CELL_COLUMN`` or
        ``OWNED_AND_HALO_CELL_COLUMN``) to a depth ``ndepth``, say, then the
        depth of the halo that is read is currently computed as the *maximum*
        of ``ndepth`` and the stencil size. This is because, computing the sum
        of ``ndepth`` and the stencil size results in a halo depth that is
        greater than the maximum allowed. In this particular case, the stencil
        accesses are 'parallel' to the halo and therefore this workaround is
        valid. TODO #2781 - this case should be handled by adding support for
        a new type of stencil rather than this ad-hoc fix.

        :param field: the field that we are concerned with

        :raises GenerationError: if an unsupported name for a loop
            upper-bound is found.
        :raises GenerationError: if a Kernel with a stencil access performs
            redundant computation out to the maximum halo depth.

        '''
        # pylint: disable=too-many-branches
        const = LFRicConstants()

        self._annexed_only = False
        call = halo_check_arg(field, AccessType.all_read_accesses())

        loop = call.ancestor(LFRicLoop)
        table = loop.ancestor(InvokeSchedule).symbol_table

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

        # Records whether the kernel is a special "halo" kernel that must
        # iterate into the halo for correctness.
        is_halo_kernel = (call.iterates_over in
                          const.HALO_KERNEL_ITERATION_SPACES)

        # now we have the parent loop we can work out what part of the
        # halo this field accesses
        if loop.upper_bound_name in const.HALO_ACCESS_LOOP_BOUNDS:
            # This loop performs redundant computation or is a 'halo' kernel
            # that iterates into the halo.
            if loop.upper_bound_halo_depth:
                self._var_depth = loop.upper_bound_halo_depth
            else:
                # loop redundant computation is to the maximum depth
                self._max_depth = True
        elif loop.upper_bound_name in ("ncolour",
                                       "ntiles_per_colour",
                                       "ncells_per_colour_and_tile"):
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
                        (not call.arguments.iteration_space_arg().discontinuous
                         and call.all_updates_are_writes)):
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
                    self._var_depth = Literal("1", INTEGER_TYPE)
                    self._annexed_only = True
        elif loop.upper_bound_name == "ndofs":
            # we only access owned dofs so there is no access to the
            # halo
            pass
        else:
            raise GenerationError(
                f"Internal error in HaloReadAccess._compute_from_field. Found "
                f"unexpected loop upper bound name '{loop.upper_bound_name}'")

        if self._max_depth or self._var_depth:
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
            if self._var_depth:
                # halo exchange does not support mixed accesses to the halo
                # so we simply set the stencil type as 'region'.
                self._stencil_type = "region"
            stencil_depth = field.descriptor.stencil['extent']
            if stencil_depth:
                # stencil_depth is provided in the kernel metadata
                st_depth = Literal(str(stencil_depth), INTEGER_TYPE)
            else:
                # Stencil_depth is provided by the algorithm layer.
                # It is currently not possible to specify kind for an
                # integer literal stencil depth in a kernel call. This
                # will be enabled when addressing issue #753.
                if field.stencil.extent_arg.is_literal():
                    # a literal is specified
                    value_str = field.stencil.extent_arg.text
                    st_depth = Literal(value_str, INTEGER_TYPE)
                else:
                    # a variable is specified
                    st_depth = Reference(
                        table.lookup(field.stencil.extent_arg.varname))
            if self._var_depth:
                if is_halo_kernel:
                    # 'halo' kernels (those that have halos included in their
                    # iteration space in order to get correct results) are a
                    # special case - the necessary halo depth is computed as
                    # the MAX of the halo and stencil depths, rather than as
                    # their sum.
                    # TODO #2781 - instead of this ad-hoc fix, we should add
                    # support for a new type of stencil.
                    self._var_depth = IntrinsicCall.create(
                        IntrinsicCall.Intrinsic.MAX,
                        [st_depth, self._var_depth.copy()])
                else:
                    # Not a special case so the necessary depth is the sum of
                    # the halo and stencil depths.
                    self._var_depth = BinaryOperation.create(
                        BinaryOperation.Operator.ADD,
                        st_depth, self._var_depth.copy())
            else:
                self._var_depth = st_depth
        # If this is an intergrid kernel and the field in question is on
        # the fine mesh then we must double the halo depth
        if call.is_intergrid and field.mesh == "gh_fine":
            if self._var_depth:
                self._var_depth = BinaryOperation.create(
                    BinaryOperation.Operator.MUL,
                    Literal("2", INTEGER_TYPE), self._var_depth.copy())


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
    :type descriptors: list of :py:class:`psyclone.LFRicFuncDescriptor`.

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
        :rtype: List of :py:class:`psyclone.lfric.FSDescriptor`
        '''
        return self._descriptors


def check_args(call, parent_call):
    '''
    Checks that the kernel arguments provided via the invoke call are
    consistent with the information expected, as specified by the
    kernel metadata.

    :param call: the object produced by the parser that describes the
                 kernel call to be checked.
    :type call: :py:class:`psyclone.parse.algorithm.KernelCall`
    :param parent_call: the kernel-call object.
    :type parent_call: :py:class:`psyclone.domain.lfric.LFRicKern`

    :raises: GenerationError if the kernel arguments in the Algorithm layer
             do not match up with the kernel metadata.
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

    # If a kernel operates on halo columns then it takes an extra, halo-depth
    # argument from the Algorithm layer.
    halo_depth_count = 0
    if "halo" in call.ktype.iterates_over:
        halo_depth_count = 1
    expected_arg_count = (len(call.ktype.arg_descriptors) +
                          stencil_arg_count + qr_arg_count + halo_depth_count)

    if expected_arg_count != len(call.args):
        msg = ""
        if parent_call:
            invoke = parent_call.ancestor(LFRicInvokeSchedule)
            if invoke:
                msg = f"from invoke '{invoke.name}' "
        raise GenerationError(
            f"error: expected '{expected_arg_count}' arguments for the call "
            f"to kernel '{call.ktype.name}' {msg}in the algorithm layer but "
            f"found '{len(call.args)}'. Expected "
            f"'{len(call.ktype.arg_descriptors)}' standard arguments, "
            f"'{stencil_arg_count}' stencil arguments, '{qr_arg_count}' "
            f"qr_arguments and '{halo_depth_count}' halo-depth arguments.")


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


class LFRicKernelArguments(Arguments):
    '''
    Provides information about LFRic kernel call arguments
    collectively, as specified by the kernel argument metadata.

    :param call: the kernel metadata for which to extract argument info.
    :type call: :py:class:`psyclone.parse.KernelCall`
    :param parent_call: the kernel-call object.
    :type parent_call: :py:class:`psyclone.domain.lfric.LFRicKern`
    :param bool check: whether to check for consistency between the
        kernel metadata and the algorithm layer. Defaults to True.

    :raises GenerationError: if the kernel metadata specifies stencil extent.
    '''
    def __init__(self, call, parent_call, check=True):
        # pylint: disable=too-many-branches
        Arguments.__init__(self, parent_call)

        # check that the arguments provided by the algorithm layer are
        # consistent with those expected by the kernel(s)
        check_args(call, parent_call)

        # create our arguments and add in stencil information where
        # appropriate.
        self._args = []
        idx = 0
        for arg in call.ktype.arg_descriptors:
            lfric_argument = LFRicKernelArgument(self, arg, call.args[idx],
                                                 parent_call, check)
            idx += 1
            if lfric_argument.descriptor.stencil:
                if lfric_argument.descriptor.stencil['extent']:
                    raise GenerationError("extent metadata not yet supported")
                    # if supported we would add the following
                    # line: stencil.extent =
                    # lfric_argument.descriptor.stencil['extent']
                # An extent argument has been added.
                stencil_extent_arg = call.args[idx]
                idx += 1
                if lfric_argument.descriptor.stencil['type'] == 'xory1d':
                    # a direction argument has been added
                    stencil = LFRicArgStencil(
                        name=lfric_argument.descriptor.stencil['type'],
                        extent_arg=stencil_extent_arg,
                        direction_arg=call.args[idx]
                        )
                    idx += 1
                else:
                    # Create a stencil object and store a reference to it in
                    # our new LFRicKernelArgument object.
                    stencil = LFRicArgStencil(
                        name=lfric_argument.descriptor.stencil['type'],
                        extent_arg=stencil_extent_arg
                        )
                lfric_argument.stencil = stencil
            self._args.append(lfric_argument)

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
                    symbol = symtab.find_or_create_tag(
                        tag, root, symbol_type=DataSymbol,
                        datatype=LFRicTypes("LFRicIntegerScalarDataType")()
                    )
                    arg.stencil.extent_arg.varname = symbol.name
            if arg.descriptor.stencil['type'] == 'xory1d':
                # a direction argument has been added
                if arg.stencil.direction_arg.varname and \
                   arg.stencil.direction_arg.varname not in \
                   const.VALID_STENCIL_DIRECTIONS:
                    # Register the name of the direction argument to ensure
                    # it is unique in the PSy layer
                    tag = "AlgArgs_" + arg.stencil.direction_arg.text
                    root = arg.stencil.direction_arg.varname
                    symbol = symtab.find_or_create_tag(
                        tag, root,
                        symbol_type=DataSymbol,
                        datatype=LFRicTypes("LFRicIntegerScalarDataType")()
                    )
                    arg.stencil.direction_arg.varname = symbol.name

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
        :rtype: (:py:class:`psyclone.lfric.LFRicKernelArgument`,
                 :py:class:`psyclone.domain.lfric.FunctionSpace`)
        :raises: FieldNotFoundError if no field or operator argument is found \
                 for the named function space.
        '''
        for arg in self._args:
            for function_space in arg.function_spaces:
                if function_space:
                    if func_space_name == function_space.orig_name:
                        return arg, function_space
        raise FieldNotFoundError(f"LFRicKernelArguments:get_arg_on_space_name:"
                                 f" there is no field or operator with "
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
        :rtype: :py:class:`psyclone.lfric.LFRicKernelArgument`
        :raises: FieldNotFoundError if no field or operator argument is found
                 for the specified function space.
        '''
        for arg in self._args:
            for function_space in arg.function_spaces:
                if function_space:
                    if func_space.mangled_name == function_space.mangled_name:
                        return arg

        raise FieldNotFoundError(f"LFRicKernelArguments:get_arg_on_space: "
                                 f"there is no field or operator with function"
                                 f" space {func_space.orig_name} (mangled name"
                                 f" = '{func_space.mangled_name}')")

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

    @property
    def first_field_or_operator(self):
        '''
        :returns: the first field or operator argument in the list.
        :rtype: :py:class:`psyclone.lfric.LFRicKernelArgument`

        :raises InternalError: if no field or operator argument is found.

        '''
        for arg in self._args:
            arg: LFRicKernelArgument
            if arg.is_field or arg.is_operator:
                return arg

        raise InternalError(
            f"Invalid LFRic kernel: failed to find an LFRicKernelArgument that"
            f" is a field or operator in '{self.names}'.")

    def iteration_space_arg(self):
        '''
        Returns an argument we can use to dereference the iteration
        space. This can be a field or operator that is modified or
        alternatively a field that is read if one or more scalars
        are modified. If a kernel writes to more than one argument then
        that requiring the largest iteration space is selected.

        :return: Kernel argument from which to obtain iteration space
        :rtype: :py:class:`psyclone.lfric.LFRicKernelArgument`
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
            "iteration_space_arg(). The lfric api must have a modified "
            "field, a modified operator, or an unmodified field (in the case "
            "of a modified scalar). None of these were found.")

    @property
    def dofs(self):
        ''' Currently required for Invoke base class although this
        makes no sense for LFRic. Need to refactor the Invoke base class
        and remove the need for this property (#279). '''
        return self._dofs

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
        # the LFRic API.
        return []


class LFRicKernelArgument(KernelArgument):
    '''
    This class provides information about individual LFRic kernel call
    arguments as specified by the kernel argument metadata and the
    kernel invocation in the Algorithm layer.

    :param kernel_args: object encapsulating all arguments to the \
                        kernel call.
    :type kernel_args: :py:class:`psyclone.lfric.LFRicKernelArguments`
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
        # Keep a reference to LFRicKernelArguments object that contains
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
                f"LFRicKernelArgument.__init__(): Found unsupported data "
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

    @classmethod
    def _ensure_unique_name(cls, name: str) -> str:
        '''
        Given the proposed argument name, returns a new name that will be
        unique in the final PSy routine.

        :param name: the proposed name of a kernel argument.

        :returns: a new name for the kernel argument.

        '''
        # Symbol imports for STENCILS are not yet in the symbol table (until
        # lowering time), so make sure the argument names do not overlap with
        # them.
        const = LFRicConstants()
        new_name = name
        if name.upper() in const.STENCIL_MAPPING.values():
            new_name += "_arg"
        return new_name

    def generate_method_call(self, method, function_space=None):
        '''
        Generate a PSyIR call to the given method of this object.

        :param str method: name of the method to generate a call to.
        :param Optional[str] function_space: name of the function space.

        :returns: the generated call.
        :rtype: :py:class:`psyclone.psyir.nodes.Call`
        '''

        # Go through invoke.schedule in case the link has bee updated
        symtab = self._call.ancestor(InvokeSchedule).invoke.schedule\
            .symbol_table
        # Use the proxy variable as derived type base
        symbol = symtab.lookup(self.proxy_name)

        if self._vector_size > 1:
            # For a field vector, just call the specified method on the first
            # element
            return Call.create(ArrayOfStructuresReference.create(
                symbol, [Literal('1', INTEGER_TYPE)],
                [self.ref_name(function_space), method]))
        return Call.create(StructureReference.create(
            symbol, [self.ref_name(function_space), method]))

    def generate_accessor(self, function_space=None):
        '''
        Generate a Reference accessing this object's data.

        :param Optional[str] function_space: name of the function space.

        :returns: the generated Reference.
        :rtype: :py:class:`psyclone.psyir.nodes.Reference`
        '''

        # Go through invoke.schedule in case the link has bee updated
        symtab = self._call.ancestor(InvokeSchedule).invoke\
            .schedule.symbol_table
        symbol = symtab.lookup(self.proxy_name)

        if self._vector_size > 1:
            # For a field vector, access the first element
            return ArrayOfStructuresReference.create(
                symbol, [Literal('1', INTEGER_TYPE)],
                [self.ref_name(function_space)])
        return StructureReference.create(
            symbol, [self.ref_name(function_space)])

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
                    f"LFRicKernelArgument.ref_name(fs): The supplied function "
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
                f"LFRicKernelArgument.ref_name(fs): Function space "
                f"'{function_space.orig_name}' is one of the 'gh_operator' "
                f"function spaces '{self.function_spaces}' but is not being "
                f"returned by either function_space_from "
                f"'{self.descriptor.function_space_from}' or "
                f"function_space_to '{self.descriptor.function_space_to}'.")
        raise GenerationError(
            f"LFRicKernelArgument.ref_name(fs): Found unsupported argument "
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
        :rtype: :py:class:`psyclone.lfric.LFRicArgStencil`
        '''
        return self._stencil

    @stencil.setter
    def stencil(self, value):
        '''
        Sets stencil information for this kernel argument.

        :param value: stencil information for this argument.
        :type value: :py:class:`psyclone.lfric.LFRicArgStencil`

        '''
        self._stencil = value

    def infer_datatype(self, proxy: bool = False) -> DataType:
        '''
        Infer the datatype of this kernel argument in the PSy layer using
        the LFRic API rules. If any LFRic infrastructure modules are required
        but are not already present then suitable ContainerSymbols are added
        to the outermost symbol table. Similarly, DataTypeSymbols are added for
        any required LFRic derived types that are not already in the symbol
        table.

        TODO #1258 - ultimately this routine should not have to create any
        DataTypeSymbols as that should already have been done.

        :param proxy: whether or not we want the type of the proxy
            object for this kernel argument. Defaults to False (i.e.
            return the type rather than the proxy type).

        :returns: the datatype of this argument.

        :raises NotImplementedError: if an unsupported argument type is found.

        '''
        scope = self._call.ancestor(Container)
        if scope is None:
            # Prefer the module scope, but some tests that are disconnected can
            # use the current scope
            scope = self._call.scope

        symtab = scope.symbol_table

        def _find_or_create_type(mod_name: str,
                                 type_name: str) -> DataTypeSymbol:
            '''
            Utility to find or create a DataTypeSymbol with the supplied name,
            imported from the named module.

            :param mod_name: the name of the module from which the
                             DataTypeSymbol should be imported.
            :param type_name: the name of the derived type for which to
                              create a DataTypeSymbol.

            :returns: the symbol for the requested type.

            '''
            return symtab.find_or_create(
                    type_name,
                    symbol_type=DataTypeSymbol,
                    datatype=UnresolvedType(),
                    interface=ImportInterface(symtab.find_or_create(
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
                kind_symbol = symtab.lookup(kind_name)
            except KeyError:
                mod_map = LFRicConstants().UTILITIES_MOD_MAP
                const_mod = mod_map["constants"]["module"]
                try:
                    constants_container = symtab.lookup(const_mod)
                except KeyError:
                    # TODO Once #696 is done, we should *always* have a
                    # symbol for this container at this point so should
                    # raise an exception if we haven't.
                    constants_container = LFRicTypes(const_mod)
                    symtab.add(constants_container)
                kind_symbol = DataSymbol(
                    kind_name, INTEGER_TYPE,
                    interface=ImportInterface(constants_container))
                symtab.add(kind_symbol)
            return ScalarType(prim_type, Reference(kind_symbol))

        if self.is_field or self.is_operator:
            # Find or create the DataTypeSymbol for the appropriate
            # field or operator type.
            mod_name = self._module_name
            if proxy:
                type_name = self._proxy_data_type
            else:
                type_name = self._data_type
            dts = _find_or_create_type(mod_name, type_name)
            if self.is_field and self.vector_size > 1:
                # A field vector must be of ArrayType
                return ArrayType(dts, [self.vector_size])
            return dts

        raise NotImplementedError(
            f"'{str(self)}' is not a scalar, field or operator argument")


class LFRicACCEnterDataDirective(ACCEnterDataDirective):
    '''
    Sub-classes ACCEnterDataDirective to provide an API-specific implementation
    of data_on_device().

    '''
    def data_on_device(self, _):
        '''
        Provide a hook to be able to add information about data being on a
        device (or not). This is currently not used in LFRic.

        '''
        return None


# ---------- Documentation utils -------------------------------------------- #
# The list of module members that we wish AutoAPI to generate
# documentation for.
__all__ = [
    'LFRicFuncDescriptor',
    'LFRicFunctionSpaces',
    'LFRicProxies',
    'LFRicLMAOperators',
    'LFRicCMAOperators',
    'LFRicMeshes',
    'LFRicInterGrid',
    'LFRicBasisFunctions',
    'LFRicBoundaryConditions',
    'LFRicGlobalSum',
    'LFRicHaloExchange',
    'LFRicHaloExchangeStart',
    'LFRicHaloExchangeEnd',
    'HaloDepth',
    'HaloWriteAccess',
    'HaloReadAccess',
    'FSDescriptor',
    'FSDescriptors',
    'LFRicArgStencil',
    'LFRicKernelArguments',
    'LFRicKernelArgument',
    'LFRicACCEnterDataDirective']
