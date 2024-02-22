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

''' This module implements the PSyclone LFRic API by specialising the required
    base class Kern in psyGen.py '''

# Imports
from collections import OrderedDict, namedtuple

from psyclone.configuration import Config
from psyclone.core import AccessType
from psyclone.domain.lfric import (KernCallArgList, KernStubArgList,
                                   KernelInterface, LFRicConstants)
from psyclone.errors import GenerationError, InternalError, FieldNotFoundError
from psyclone.f2pygen import (CommentGen, DeclGen, ModuleGen, SubroutineGen,
                              UseGen)
from psyclone.parse.algorithm import Arg, KernelCall
from psyclone.psyGen import InvokeSchedule, CodedKern
from psyclone.psyir.frontend.fparser2 import Fparser2Reader
from psyclone.psyir.nodes import (Loop, Literal, Reference,
                                  KernelSchedule)
from psyclone.psyir.symbols import DataSymbol, ScalarType, ArrayType


class LFRicKern(CodedKern):
    ''' Stores information about LFRic Kernels as specified by the
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
        # Import here to avoid circular dependency
        # pylint: disable=import-outside-toplevel
        if False:  # pylint: disable=using-constant-test
            from psyclone.dynamo0p3 import DynKernelArguments
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
            :py:class:`psyclone.core.VariablesAccessInfo`

        '''
        # Use the KernelCallArgList class, which can also provide variable
        # access information:
        create_arg_list = KernCallArgList(self)
        create_arg_list.generate(var_accesses)

        super().reference_accesses(var_accesses)
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
        :type parent: :py:class:`psyclone.domain.lfric.LFRicLoop`
        '''
        self._setup_basis(call.ktype)
        self._setup(call.ktype, call.module_name, call.args, parent)

    def load_meta(self, ktype):
        '''
        Sets up kernel information with the kernel type object
        which is created by the parser. The object includes the
        metadata describing the kernel code.

        :param ktype: the kernel metadata object produced by the parser
        :type ktype: :py:class:`psyclone.domain.lfric.LFRicKernMetadata`

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
                        f"Expected one of {const.VALID_SCALAR_DATA_TYPES} "
                        f"data types for a scalar argument but found "
                        f"'{descriptor.data_type}'.")
            else:
                raise GenerationError(
                    f"LFRicKern.load_meta() expected one of "
                    f"{const.VALID_ARG_TYPE_NAMES} but found "
                    f"'{descriptor.argument_type}'")
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

        :param kmetadata: The kernel metadata object produced by the parser.
        :type kmetadata: :py:class:`psyclone.domain.lfric.LFRicKernMetadata`
        '''
        for descriptor in kmetadata.func_descriptors:
            if len(descriptor.operator_names) > 0:
                self._basis_required = True
                self._eval_shapes = kmetadata.eval_shapes[:]
                break

    def _setup(self, ktype, module_name, args, parent, check=True):
        # pylint: disable=too-many-arguments
        '''Internal setup of kernel information.

        :param ktype: object holding information on the parsed metadata for \
                      this kernel.
        :type ktype: :py:class:`psyclone.domain.lfric.LFRicKernMetadata`
        :param str module_name: the name of the Fortran module that contains \
                                the source of this Kernel.
        :param args: list of Arg objects produced by the parser for the \
                     arguments of this kernel call.
        :type args: List[:py:class:`psyclone.parse.algorithm.Arg`]
        :param parent: the parent of this kernel call in the generated \
                       AST (will be a loop object).
        :type parent: :py:class:`psyclone.domain.lfric.LFRicLoop`
        :param bool check: whether to check for consistency between the \
            kernel metadata and the algorithm layer. Defaults to True.

        '''
        # Import here to avoid circular dependency
        # pylint: disable=import-outside-toplevel
        from psyclone.dynamo0p3 import DynKernelArguments, FSDescriptors
        # pylint: disable=too-many-branches, too-many-locals
        super().__init__(DynKernelArguments,
                         KernelCall(module_name, ktype, args),
                         parent, check)
        # Remove "_code" from the name if it exists to determine the
        # base name which (if LFRic naming conventions are
        # followed) is used as the root for the module and subroutine
        # names.
        if self.name.lower().endswith("_code"):
            self._base_name = self.name[:-5]
        else:
            # TODO: #11 add a warning here when logging is added
            self._base_name = self.name
        self._func_descriptors = ktype.func_descriptors
        # Keep a record of the type of CMA kernel identified when
        # parsing the kernel metadata
        self._cma_operation = ktype.cma_operation
        self._fs_descriptors = FSDescriptors(ktype.func_descriptors)

        # Record whether or not the kernel metadata specifies that this
        # is an inter-grid kernel
        self._is_intergrid = ktype.is_intergrid

        const = LFRicConstants()
        # Check that all specified evaluator shapes are recognised
        invalid_shapes = set(self._eval_shapes) \
            - set(const.VALID_EVALUATOR_SHAPES)
        if invalid_shapes:
            raise InternalError(
                f"Evaluator shape(s) {list(invalid_shapes)} is/are not "
                f"recognised. Must be one of {const.VALID_EVALUATOR_SHAPES}.")

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
                    find_or_create_integer_symbol(qr_arg.varname, tag=tag).name
            else:
                # If we don't have a name then we must be doing kernel-stub
                # generation so create a suitable name.
                # TODO #719 we don't yet have a symbol table to prevent
                # clashes.
                qr_name = "qr_"+shape.split("_")[-1]

            # LFRic api kernels require quadrature rule arguments to be
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
                raise InternalError(f"Unsupported quadrature shape "
                                    f"('{shape}') found in LFRicKern._setup")

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
                :py:class:`psyclone.domain.lfric.LFRicKern.QRRule` indexed by \
                quadrature shape.
        '''
        return self._qr_rules

    @property
    def cma_operation(self):
        '''
        :return: the type of CMA operation performed by this kernel
                 (one of 'assembly', 'apply' or 'matrix-matrix') or None
                 if the kernel does not involve CMA operators.
        :rtype: str
        '''
        return self._cma_operation

    @property
    def is_intergrid(self):
        '''
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
        sched = self.ancestor(InvokeSchedule)
        if self._is_intergrid:
            invoke = sched.invoke
            if id(self) not in invoke.meshes.intergrid_kernels:
                raise InternalError(
                    f"Colourmap information for kernel '{self.name}' has "
                    f"not yet been initialised")
            cmap = invoke.meshes.intergrid_kernels[id(self)].\
                colourmap_symbol.name
        else:
            try:
                cmap = sched.symbol_table.lookup_with_tag("cmap").name
            except KeyError:
                # We have to do this here as _init_colourmap (which calls this
                # method) is only called at code-generation time.
                cmap = sched.symbol_table.find_or_create_array(
                    "cmap", 2, ScalarType.Intrinsic.INTEGER,
                    tag="cmap").name

        return cmap

    @property
    def last_cell_all_colours_symbol(self):
        '''
        Getter for the symbol of the array holding the index of the last
        cell of each colour.

        :returns: name of the array.
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
            return (invoke.meshes.intergrid_kernels[id(self)].
                    last_cell_var_symbol)

        ubnd_name = self.ancestor(Loop).upper_bound_name
        const = LFRicConstants()

        if ubnd_name in const.HALO_ACCESS_LOOP_BOUNDS:
            return self.scope.symbol_table.find_or_create_array(
                "last_halo_cell_all_colours", 2,
                ScalarType.Intrinsic.INTEGER,
                tag="last_halo_cell_all_colours")

        return self.scope.symbol_table.find_or_create_array(
            "last_edge_cell_all_colours", 1,
            ScalarType.Intrinsic.INTEGER,
            tag="last_edge_cell_all_colours")

    @property
    def ncolours_var(self):
        '''
        Getter for the name of the variable holding the number of colours
        associated with this kernel call.

        :return: name of the variable holding the number of colours
        :rtype: Union[str, NoneType]

        :raises InternalError: if this kernel is not coloured or the \
                               colour-map information has not been initialised.
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
            ncols_sym = \
                invoke.meshes.intergrid_kernels[id(self)].ncolours_var_symbol
            if not ncols_sym:
                return None
            return ncols_sym.name

        return self.scope.symbol_table.lookup_with_tag("ncolour").name

    @property
    def fs_descriptors(self):
        '''
        :return: a list of function space descriptor objects of
                 type FSDescriptors which contain information about
                 the function spaces.
        :rtype: List[:py:class:`psyclone.FSDescriptors`].

        '''
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
        accesses = set(arg.access for arg in self.args)
        all_writes = AccessType.all_write_accesses()
        all_writes.remove(AccessType.WRITE)
        return (not accesses.intersection(set(all_writes)))

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
                f"The LFRic API kernel-stub generator supports kernels that "
                f"operate on one of {supported_operates_on} but found "
                f"'{self.iterates_over}' in kernel '{self.name}'.")

        # Create an empty PSy layer module
        psy_module = ModuleGen(self._base_name+"_mod")

        # Create the subroutine
        sub_stub = SubroutineGen(psy_module, name=self._base_name+"_code",
                                 implicitnone=True)

        # Add all the declarations
        # Import here to avoid circular dependency
        # pylint: disable=import-outside-toplevel
        from psyclone.domain.lfric import (LFRicFields, LFRicScalarArgs,
                                           LFRicStencils)
        from psyclone.dynamo0p3 import (DynCellIterators, DynDofmaps,
                                        DynFunctionSpaces, DynCMAOperators,
                                        DynBoundaryConditions,
                                        DynLMAOperators, LFRicMeshProperties,
                                        DynBasisFunctions, DynReferenceElement)
        for entities in [DynCellIterators, DynDofmaps, DynFunctionSpaces,
                         DynCMAOperators, LFRicScalarArgs, LFRicFields,
                         DynLMAOperators, LFRicStencils, DynBasisFunctions,
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
                f"The LFRic API supports calls to user-supplied kernels that "
                f"operate on one of {const.USER_KERNEL_ITERATION_SPACES}, but "
                f"kernel '{self.name}' operates on '{self.iterates_over}'.")

        # Get configuration for valid argument kinds
        api_config = Config.get().api_conf("dynamo0.3")

        parent.add(DeclGen(parent, datatype="integer",
                           kind=api_config.default_kind["integer"],
                           entity_decls=["cell"]))
        # Import here to avoid circular dependency
        # pylint: disable=import-outside-toplevel
        from psyclone.domain.lfric import LFRicLoop
        parent_loop = self.ancestor(LFRicLoop)

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
                    f"Kernel '{self._name}' reads from an operator and "
                    f"therefore cannot be used for cells beyond the level 1 "
                    f"halo. However the containing loop goes out to level "
                    f"{parent_loop.upper_bound_halo_depth}")

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
                    raise GenerationError(f"Kernel '{self._name}' has an "
                                          f"argument with INC access and "
                                          f"therefore must be coloured in "
                                          f"order to be parallelised with "
                                          f"OpenMP.")

        parent.add(CommentGen(parent, ""))

        super().gen_code(parent)

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

        :raises GenerationError: if no subroutine matching this kernel can \
            be found in the parse tree of the associated source code.
        '''
        if self._kern_schedule:
            return self._kern_schedule

        # Get the PSyIR Kernel Schedule(s)
        routines = Fparser2Reader().get_routine_schedules(self.name, self.ast)
        for routine in routines:
            # If one of the symbols is not declared in a routine then
            # this is only picked up when writing out the routine
            # (raising a VisitorError), so we check here so that
            # invalid code is not inlined. We use debug_string() to
            # minimise the overhead.

            # TODO #2271 could potentially avoid the need for
            # debug_string() within. Sergi suggests that we may be
            # missing the traversal of the declaration init
            # expressions and that might solve the problem. I'm not so
            # sure as we are talking about unknown symbols that will
            # only be resolved in the back-end (or not). If I am right
            # then one option would be to use the FortranWriter, but
            # that would be bigger overhead, or perhaps just the
            # declarations part of FortranWriter if that is possible.
            # Also see TODO issue #2336 which captures the specific
            # problem in LFRic that this fixes.
            routine.debug_string()

        if len(routines) == 1:
            sched = routines[0]
            # TODO #928: We don't validate the arguments yet because the
            # validation has many false negatives.
            # self.validate_kernel_code_args(sched.symbol_table)
        else:
            # The kernel name corresponds to an interface block. Find which
            # of the routines matches the precision of the arguments.
            for routine in routines:
                try:
                    # The validity check for the kernel arguments will raise
                    # an exception if the precisions don't match.
                    self.validate_kernel_code_args(routine.symbol_table)
                    sched = routine
                    break
                except GenerationError:
                    pass
            else:
                raise GenerationError(
                    f"Failed to find a kernel implementation with an interface"
                    f" that matches the invoke of '{self.name}'. (Tried "
                    f"routines {[item.name for item in routines]}.)")

        # TODO #935 - replace the PSyIR argument data symbols with LFRic data
        # symbols. For the moment we just return the unmodified PSyIR schedule
        # but this should use RaisePSyIR2LFRicKernTrans once KernelInterface
        # is fully functional (#928).
        ksched = KernelSchedule(sched.name,
                                symbol_table=sched.symbol_table.detach())
        for child in sched.pop_all_children():
            ksched.addchild(child)
        sched.replace_with(ksched)

        self._kern_schedule = ksched

        return self._kern_schedule

    def validate_kernel_code_args(self, table):
        '''Check that the arguments in the kernel code match the expected
        arguments as defined by the kernel metadata and the LFRic
        API.

        :param table: the symbol table to validate against the metadata.
        :type table: :py:class:`psyclone.psyir.symbols.SymbolTable`

        :raises GenerationError: if the number of arguments indicated by the \
            kernel metadata doesn't match the actual number of arguments in \
            the symbol table.

        '''
        # Get the kernel subroutine arguments
        kern_code_args = table.argument_list

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
                f"In kernel '{self.name}' the number of arguments indicated by"
                f" the kernel metadata is {expected_n_args} but the actual "
                f"number of kernel arguments found is {actual_n_args}.")

        # 2: Check that the properties of each argument match.
        for idx, kern_code_arg in enumerate(kern_code_args):
            interface_arg = interface_args[idx]
            try:
                alg_idx = interface_info.metadata_index_from_actual_index(idx)
                alg_arg = self.arguments.args[alg_idx]
            except KeyError:
                # There's no algorithm argument directly associated with this
                # kernel argument. (We only care about the data associated
                # with scalar, field and operator arguments.)
                alg_arg = None
            self._validate_kernel_code_arg(kern_code_arg, interface_arg,
                                           alg_arg)

    def _validate_kernel_code_arg(self, kern_code_arg, interface_arg,
                                  alg_arg=None):
        '''Internal method to check that the supplied argument descriptions
        match and raise appropriate exceptions if not.

        :param kern_code_arg: kernel code argument.
        :type kern_code_arg: :py:class:`psyclone.psyir.symbols.DataSymbol`
        :param interface_arg: expected argument.
        :type interface_arg: :py:class:`psyclone.psyir.symbols.DataSymbol`
        :param alg_arg: the associated argument in the Algorithm layer. Note \
            that only kernel arguments holding the data associated with \
            scalar, field and operator arguments directly correspond to \
            arguments that appear in the Algorithm layer.
        :type alg_arg: \
            Optional[:py:class`psyclone.dynamo0p3.DynKernelArgument`]

        :raises GenerationError: if the contents of the arguments do \
            not match.
        :raises InternalError: if an unexpected datatype is found.

        '''
        # 1: intrinsic datatype
        actual_datatype = kern_code_arg.datatype.intrinsic
        expected_datatype = interface_arg.datatype.intrinsic
        if actual_datatype != expected_datatype:
            raise GenerationError(
                f"Kernel argument '{kern_code_arg.name}' has datatype "
                f"'{actual_datatype}' in kernel '{self.name}' but the LFRic "
                f"API expects '{expected_datatype}'.")
        # 2: precision. An LFRic kernel is only permitted to have a precision
        #    specified by a recognised type parameter or a no. of bytes.
        actual_precision = kern_code_arg.datatype.precision
        api_config = Config.get().api_conf("dynamo0.3")
        if isinstance(actual_precision, DataSymbol):
            # Convert precision into number of bytes to support
            # mixed-precision kernels.
            # TODO #1941: it would be better if the LFRic constants_mod.f90
            # was the single source of truth for precision values.
            actual_precision = api_config.precision_map[
                actual_precision.name]
        elif not isinstance(actual_precision, int):
            raise GenerationError(
                f"An argument to an LFRic kernel must have a precision defined"
                f" by either a recognised LFRic type parameter (one of "
                f"{sorted(api_config.precision_map.keys())}) or an integer"
                f" number of bytes but argument '{kern_code_arg.name}' to "
                f"kernel '{self.name}' has precision {actual_precision}.")

        if alg_arg:
            # We have information on the corresponding argument in the
            # Algorithm layer so we can check that the precision matches.
            # This is used to identify the correct kernel subroutine for a
            # mixed-precision kernel.
            alg_precision = api_config.precision_map[alg_arg.precision]
            if alg_precision != actual_precision:
                raise GenerationError(
                    f"Precision ({alg_precision} bytes) of algorithm-layer "
                    f"argument '{alg_arg.name}' does not match that "
                    f"({actual_precision} bytes) of the corresponding kernel "
                    f"subroutine argument '{kern_code_arg.name}' for kernel "
                    f"'{self.name}'.")

        # 3: intent
        actual_intent = kern_code_arg.interface.access
        expected_intent = interface_arg.interface.access
        if actual_intent.name != expected_intent.name:
            raise GenerationError(
                f"Kernel argument '{kern_code_arg.name}' has intent "
                f"'{actual_intent.name}' in kernel '{self.name}' but the "
                f"LFRic API expects intent '{expected_intent.name}'.")
        # 4: scalar or array
        if interface_arg.is_scalar:
            if not kern_code_arg.is_scalar:
                raise GenerationError(
                    f"Argument '{kern_code_arg.name}' to kernel '{self.name}' "
                    f"should be a scalar according to the LFRic API, but it "
                    f"is not.")
        elif interface_arg.is_array:
            if not kern_code_arg.is_array:
                raise GenerationError(
                    f"Argument '{kern_code_arg.name}' to kernel '{self.name}' "
                    f"should be an array according to the LFRic API, but it "
                    f"is not.")
            # 4.1: array dimensions
            if len(interface_arg.shape) != len(kern_code_arg.shape):
                raise GenerationError(
                    f"Argument '{kern_code_arg.name}' to kernel '{self.name}' "
                    f"should be an array with {len(interface_arg.shape)} "
                    f"dimension(s) according to the LFRic API, but "
                    f"found {len(kern_code_arg.shape)}.")
            for dim_idx, kern_code_arg_dim in enumerate(kern_code_arg.shape):
                if not isinstance(kern_code_arg_dim, ArrayType.ArrayBounds):
                    continue
                if (not isinstance(kern_code_arg_dim.lower, Literal) or
                        kern_code_arg_dim.lower.value != "1"):
                    raise GenerationError(
                        f"All array arguments to LFRic kernels must have lower"
                        f" bounds of 1 for all dimensions. However, array "
                        f"'{kern_code_arg.name}' has a lower bound of "
                        f"'{kern_code_arg_dim.lower}' for dimension {dim_idx}")
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
                        raise GenerationError(
                            f"For dimension {dim_idx+1} in array argument "
                            f"'{kern_code_arg.name}' to kernel '{self.name}' "
                            f"the following error was found: "
                            f"{info.args[0]}") from info
        else:
            raise InternalError(
                f"Unexpected argument type found for '{kern_code_arg.name}' in"
                f" kernel '{self.name}'. Expecting a scalar or an array.")


# ---------- Documentation utils -------------------------------------------- #
# The list of module members that we wish AutoAPI to generate
# documentation for. (See https://psyclone-ref.readthedocs.io)
__all__ = ['LFRicKern']
