# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017-2024, Science and Technology Facilities Council
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

''' This module implements the PSyclone LFRic API by capturing the Kernel
subroutine code and metadata describing the subroutine for the LFRic API.'''

from collections import OrderedDict
import fparser

from psyclone import psyGen
from psyclone.core import AccessType
from psyclone.domain.lfric.lfric_builtins import BUILTIN_MAP
from psyclone.domain.lfric import LFRicArgDescriptor, LFRicConstants
from psyclone.dynamo0p3 import (DynFuncDescriptor03, MeshPropertiesMetaData,
                                RefElementMetaData)
from psyclone.errors import InternalError
from psyclone.parse.kernel import getkerneldescriptors, KernelType
from psyclone.parse.utils import ParseError


class LFRicKernMetadata(KernelType):
    '''
    Captures the Kernel subroutine code and metadata describing
    the subroutine for the LFRic API.

    :param ast: fparser1 AST for the kernel.
    :type ast: :py:class:`fparser.block_statements.BeginSource`
    :param str name: the name of this kernel.

    :raises ParseError: if the metadata does not conform to the
                        rules for the LFRic API.
    '''
    # pylint: disable=too-many-instance-attributes
    def __init__(self, ast, name=None):
        # pylint: disable=too-many-branches, too-many-locals

        KernelType.__init__(self, ast, name=name)

        # The type of CMA operation this kernel performs (or None if
        # no CMA operators are involved)
        self._cma_operation = None

        # Query the metadata for the evaluator shape(s) (only required if
        # kernel uses quadrature or an evaluator). If it is not
        # present then 'eval_shapes' will be an empty list.
        shape = self.get_integer_variable('gh_shape')
        if not shape:
            # There's no scalar 'gh_shape' - is it present as an array?
            self._eval_shapes = self.get_integer_array('gh_shape')
        else:
            self._eval_shapes = [shape]

        # The list of function space names for which an evaluator is
        # required. We set this up below once we have processed the
        # metadata describing the kernel arguments.
        self._eval_targets = []

        # Whether or not this is an inter-grid kernel (i.e. has a mesh
        # specified for each [field] argument). This property is
        # set to True if all the checks in '_validate_inter_grid()' pass.
        self._is_intergrid = False

        # Parse the 'arg_type' metadata
        self._arg_descriptors = []
        for idx, arg_type in enumerate(self._inits):
            self._arg_descriptors.append(
                LFRicArgDescriptor(arg_type, self.iterates_over, idx))

        # Get a list of the Type declarations in the metadata
        type_declns = [cline for cline in self._ktype.content if
                       isinstance(cline, fparser.one.typedecl_statements.Type)]

        # Parse the 'func_type' metadata if it exists
        func_types = []
        for line in type_declns:
            for entry in line.selector:
                if entry == "func_type":
                    func_types = getkerneldescriptors(
                        name, line, var_name="meta_funcs",
                        var_type="func_type")
                    break

        self._func_descriptors = []
        # Populate a list of function descriptor objects which we
        # return via the 'func_descriptors' method.
        arg_fs_names = []
        for descriptor in self._arg_descriptors:
            arg_fs_names.extend(descriptor.function_spaces)
        used_fs_names = []
        need_evaluator = False
        for func_type in func_types:
            descriptor = DynFuncDescriptor03(func_type)
            fs_name = descriptor.function_space_name
            # Check that function space names in 'meta_funcs' are specified in
            # 'meta_args'.
            if fs_name not in arg_fs_names:
                raise ParseError(
                    f"In the LFRic API all function spaces specified in "
                    f"'meta_funcs' must exist in 'meta_args', but '{fs_name}' "
                    f"breaks this rule in ...\n'{self._ktype.content}'.")
            if fs_name not in used_fs_names:
                used_fs_names.append(fs_name)
            else:
                raise ParseError(
                    f"In the LFRic API function spaces specified in "
                    f"'meta_funcs' must be unique, but '{fs_name}' is "
                    f"replicated.")

            const = LFRicConstants()
            # Check that a valid shape has been specified if
            # this function space requires a basis or differential basis
            for op_name in descriptor.operator_names:
                if op_name in const.VALID_EVALUATOR_NAMES:
                    need_evaluator = True
                    if not self._eval_shapes:
                        raise ParseError(
                            f"In the LFRic API any kernel requiring "
                            f"quadrature or an evaluator "
                            f"({const.VALID_EVALUATOR_NAMES}) must also "
                            f"supply the shape of that evaluator by setting "
                            f"'gh_shape' in the kernel metadata but "
                            f"this is missing for kernel '{self.name}'")
                    shape_set = set(self._eval_shapes)
                    if not shape_set.issubset(
                            set(const.VALID_EVALUATOR_SHAPES)):
                        raise ParseError(
                            f"In the LFRic API a kernel requiring either "
                            f"quadrature or an evaluator must request one or "
                            f"more valid 'gh_shapes' (one of "
                            f"{const.VALID_EVALUATOR_SHAPES}) but got "
                            f"'{self._eval_shapes}' for kernel '{self.name}'")

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
        # Ensure that '_eval_targets' entries are not duplicated
        for target in _targets:
            if target not in self._eval_targets:
                self._eval_targets.append(target)

        # Does this kernel require any properties of the reference element?
        self.reference_element = RefElementMetaData(self.name, type_declns)

        # Does this kernel require any properties of the mesh?
        self.mesh = MeshPropertiesMetaData(self.name, type_declns)

        # Perform further checks that the metadata we've parsed
        # conforms to the rules for this API
        self._validate(need_evaluator)

    def _validate(self, need_evaluator):
        '''
        Check that the metadata conforms to LFRic rules for a user-provided
        kernel or a built-in.

        :param bool need_evaluator: whether this kernel requires an
                                    evaluator/quadrature.
        :raises ParseError: if the kernel metadata specifies writing to the
                            read-only function space.
        :raises ParseError: if a user-supplied LFRic kernel updates/writes
                            to a scalar argument.
        :raises ParseError: if a kernel does not have at least one argument
                            that is updated/written to.
        :raises ParseError: if a kernel does not require basis or
                            differential basis functions but specifies one
                            or more gh_shapes.
        :raises ParseError: if a kernel does not require basis or
                            differential basis functions but specifies
                            gh_evaluator_targets.
        :raises ParseError: if a kernel specifies gh_evaluator_targets
                            but does not need an evaluator.
        :raises ParseError: if a kernel requires an evaluator on a
                            specific function space but does not have an
                            argument on that space.
        :raises ParseError: if a kernel that has LMA operator arguments
                            also has a field argument with an invalid
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
                        f"Found kernel metadata in '{self.name}' that "
                        f"specifies writing to the read-only function space "
                        f"'{arg.function_spaces[0]}'.")

                # We must not write to scalar arguments if it's not a
                # built-in
                if self.name not in BUILTIN_MAP and \
                   arg.argument_type in const.VALID_SCALAR_NAMES:
                    raise ParseError(
                        f"A user-supplied LFRic kernel must not write/update "
                        f"a scalar argument but kernel '{self.name}' has a "
                        f"scalar argument with "
                        f"'{arg.access.api_specific_name()}' access.")
        if write_count == 0:
            raise ParseError(f"An LFRic kernel must have at least one "
                             f"argument that is updated (written to) but "
                             f"found none for kernel '{self.name}'.")

        # Check that no shape has been supplied if no basis or
        # differential basis functions are required for the kernel
        if not need_evaluator and self._eval_shapes:
            raise ParseError(
                f"Kernel '{self.name}' specifies one or more 'gh_shapes' "
                f"({self._eval_shapes}) but does not need an evaluator because"
                f" no basis or differential basis functions are required")
        # Check that 'gh_evaluator_targets' is only present if required
        if self._eval_targets:
            if not need_evaluator:
                raise ParseError(
                    f"Kernel '{self.name}' specifies 'gh_evaluator_targets' "
                    f"({self._eval_targets}) but does not need an evaluator "
                    f"because no basis or differential basis functions are "
                    f"required")
            if "gh_evaluator" not in self._eval_shapes:
                raise ParseError(
                    f"Kernel '{self.name}' specifies 'gh_evaluator_targets' "
                    f"({self._eval_targets}) but does not need an evaluator "
                    f"because gh_shape={self._eval_shapes}")
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
                        f"Kernel '{self.name}' specifies that an evaluator is "
                        f"required on '{eval_fs}' but does not have an "
                        f"argument on this space.")

        # If we have an LMA operator as argument then only field arguments
        # with 'gh_real' data type are permitted
        lma_ops = psyGen.args_filter(self._arg_descriptors,
                                     arg_types=["gh_operator"])
        if lma_ops:
            for arg in self._arg_descriptors:
                if (arg.argument_type in const.VALID_FIELD_NAMES
                        and arg.data_type != "gh_real"):
                    raise ParseError(
                        f"In the LFRic API a kernel that has an LMA operator "
                        f"argument must only have field arguments with "
                        f"'gh_real' data type but kernel '{self.name}' has a "
                        f"field argument with '{arg.data_type}' data type.")

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
        Checks that the kernel metadata obeys the rules for LFRic inter-grid
        kernels. If none of the kernel arguments has a mesh associated with it
        then it is not an inter-grid kernel and this routine silently returns.

        :raises: ParseError: if metadata breaks inter-grid rules.
        '''
        # pylint: disable=too-many-branches
        # Dictionary of meshes associated with arguments (for inter-grid
        # kernels). Keys are the meshes, values are lists of function spaces
        # of the corresponding field arguments.
        mesh_dict = OrderedDict()
        # Whether or not any field args are missing the 'mesh_arg' specifier
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
                f"The implementation of inter-grid support in the LFRic "
                f"API assumes there are exactly two mesh types but "
                f"LFRicConstants.VALID_MESH_TYPES contains "
                f"{len(const.VALID_MESH_TYPES)}: {const.VALID_MESH_TYPES}")
        if len(mesh_list) != len(const.VALID_MESH_TYPES):
            raise ParseError(
                f"Inter-grid kernels in the LFRic API must have at least "
                f"one field argument on each of the mesh types "
                f"({const.VALID_MESH_TYPES}). However, kernel {self.name} has "
                f"arguments only on {[str(name) for name in mesh_list]}")
        # Inter-grid kernels must only have field arguments
        if non_field_arg_types:
            raise ParseError(
                f"Inter-grid kernels in the LFRic API are only permitted "
                f"to have field arguments but kernel {self.name} also has "
                f"arguments of type "
                f"{[str(name) for name in non_field_arg_types]}")
        # Check that all arguments have a mesh specified
        if missing_mesh:
            raise ParseError(
                f"Inter-grid kernels in the LFRic API must specify which mesh "
                f"each field argument is on but kernel {self.name} has at "
                f"least one field argument for which 'mesh_arg' is missing.")
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
                f"In the LFRic API field arguments to inter-grid kernels "
                f"must be on different function spaces if they are on "
                f"different meshes. However kernel {self.name} has a field on "
                f"function space(s) {[str(name) for name in fs_common]} on "
                f"each of the mesh types {[str(name) for name in mesh_list]}.")
        # Finally, record that this is a valid inter-grid kernel
        self._is_intergrid = True

    def _identify_cma_op(self, cwise_ops):
        '''
        Identify and return the type of CMA-operator-related operation
        this kernel performs (one of "assemble", "apply" or "matrix-matrix")

        :param cwise_ops: all column-wise operator arguments in a kernel.
        :type cwise_ops: list of str

        :returns: the type of CMA-operator-related operation that this
                  kernel performs.
        :rtype: str

        :raises ParseError: if the kernel metadata does not conform to the
                            LFRic rules for a kernel with a CMA operator.

        '''
        # pylint: disable=too-many-branches
        const = LFRicConstants()
        for arg in self._arg_descriptors:
            # No vector arguments are permitted
            if arg.vector_size > 1:
                raise ParseError(
                    f"Kernel '{self.name}' takes a CMA operator but has a "
                    f"vector argument '{arg.argument_type}*{arg.vector_size}'."
                    f" This is forbidden.")
            # No stencil accesses are permitted
            if arg.stencil:
                raise ParseError(
                    f"Kernel '{self.name}' takes a CMA operator but has an "
                    f"argument with a stencil access "
                    f"('{arg.stencil['type']}'). This is forbidden.")
            # Only field arguments with 'gh_real' data type are permitted
            if (arg.argument_type in const.VALID_FIELD_NAMES and
                    arg.data_type != "gh_real"):
                raise ParseError(
                    f"In the LFRic API a kernel that takes a CMA operator "
                    f"argument must only have field arguments with 'gh_real' "
                    f"data type but kernel '{self.name}' has a field argument "
                    f"with '{arg.data_type}' data type.")

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
                    f"In the LFRic API a kernel that applies a CMA operator "
                    f"must only have one such operator in its list of "
                    f"arguments but found {len(cwise_ops)} for kernel "
                    f"'{self.name}'.")
            cma_op = cwise_ops[0]
            if len(self._arg_descriptors) != 3:
                raise ParseError(
                    f"In the LFRic API a kernel that applies a CMA operator "
                    f"must have 3 arguments (the operator and two fields) but "
                    f"kernel '{self.name}' has {len(self._arg_descriptors)} "
                    f"arguments.")
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
                    f"Kernel '{self.name}' has a read-only CMA operator. In "
                    f"order to apply it the kernel must have one read-only "
                    f"field argument.")
            if len(farg_write) != 1:
                raise ParseError(
                    f"Kernel '{self.name}' has a read-only CMA operator. In "
                    f"order to apply it the kernel must write to one field "
                    f"argument.")
            # Check that the function spaces match up
            if farg_read[0].function_space != cma_op.function_space_from:
                raise ParseError(
                    f"Kernel '{self.name}' applies a CMA operator but the "
                    f"function space of the field argument it reads from "
                    f"('{farg_read[0].function_space}') does not match the "
                    f"'from' space of the operator "
                    f"('{cma_op.function_space_from}').")
            if farg_write[0].function_space != cma_op.function_space_to:
                raise ParseError(
                    f"Kernel '{self.name}' applies a CMA operator but the "
                    f"function space of the field argument it writes to "
                    f"('{farg_write[0].function_space}') does not match the "
                    f"'to' space of the operator "
                    f"('{cma_op.function_space_to}').")
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
                    f"Kernel '{self.name}' writes to a column-wise operator "
                    f"but also writes to "
                    f"{[str(arg.argument_type) for arg in write_args]} "
                    f"argument(s). This is not allowed.")
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
                    f"Kernel '{self.name}' has a single column-wise operator "
                    f"argument but does not conform to the rules for an "
                    f"Assembly kernel because it does not have any read-only "
                    f"LMA operator arguments.")
            # A valid matrix-matrix kernel must only have CMA operators
            # and scalars as arguments.
            scalar_args = psyGen.args_filter(
                self._arg_descriptors,
                arg_types=const.VALID_SCALAR_NAMES)
            if (len(scalar_args) + len(cwise_ops)) != \
               len(self._arg_descriptors):
                raise ParseError(
                    f"A column-wise matrix-matrix kernel must have only "
                    f"column-wise operators and scalars as arguments but "
                    f"kernel '{self.name}' has: "
                    f"{[str(a.argument_type) for a in self._arg_descriptors]}."
                    )
            return "matrix-matrix"
        raise ParseError(
            f"An LFRic kernel cannot update more than one CMA (column-wise) "
            f"operator but kernel '{self.name}' updates {write_count}.")

    def _validate_operates_on_domain(self, need_evaluator):
        '''
        Check whether a kernel that has operates_on == domain obeys
        the rules for the LFRic API.

        :raises ParseError: if the kernel metadata does not obey the rules
                            for an LFRic kernel with operates_on = domain.
        '''
        if self.iterates_over != "domain":
            return

        const = LFRicConstants()
        # A kernel which operates on the 'domain' is currently restricted
        # to only accepting scalar and field arguments.
        valid_arg_types = const.VALID_SCALAR_NAMES + const.VALID_FIELD_NAMES
        for arg in self._arg_descriptors:
            if arg.argument_type not in valid_arg_types:
                raise ParseError(
                    f"In the LFRic API a kernel which operates on the 'domain'"
                    f" is only permitted to accept scalar and field arguments "
                    f"but the metadata for kernel '{self.name}' includes an "
                    f"argument of type '{arg.argument_type}'")

        if need_evaluator:
            raise ParseError(
                f"In the LFRic API a kernel that operates on the 'domain' "
                f"cannot be passed basis/differential basis functions but the "
                f"metadata for kernel '{self.name}' contains an entry for "
                f"'meta_funcs'")

        if self.reference_element.properties:
            raise ParseError(
                f"Kernel '{self.name}' operates on the domain but requests "
                f"properties of the reference element "
                f"({self.reference_element.properties}). This is not "
                f"permitted in the LFRic API.")

        if self.mesh.properties:
            raise ParseError(
                f"Kernel '{self.name}' operates on the domain but requests "
                f"properties of the mesh ({self.mesh.properties}). This is "
                f"not permitted in the LFRic API.")

        if self._is_intergrid:
            raise ParseError(
                f"Kernel '{self.name}' operates on the domain but has fields "
                f"on different mesh resolutions (inter-grid). This is not "
                f"permitted in the LFRic API.")

    @property
    def func_descriptors(self):
        '''
        Returns metadata about the function spaces within a
        Kernel. This metadata is provided within Kernel code via the
        meta_funcs variable. Information is returned as a list of
        DynFuncDescriptor03 objects, one for each function space. '''
        return self._func_descriptors

    @property
    def cma_operation(self):
        '''
        Returns the type of CMA operation identified from the kernel
        metadata (one of 'assembly', 'apply' or 'matrix-matrix') or
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
        provided. This list is obtained from the GH_EVALUATOR_TARGETS metadata
        entry (if present). If this is not specified in the metadata then
        we default to providing evaluators on all of the function spaces
        associated with the arguments which this kernel updates.

        :return: list of the names of the function spaces (as they appear in
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


# ---------- Documentation utils -------------------------------------------- #
# The list of module members that we wish AutoAPI to generate
# documentation for. (See https://psyclone-ref.readthedocs.io)
__all__ = ['LFRicKernMetadata']
