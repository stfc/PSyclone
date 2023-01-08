# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2022-2023, Science and Technology Facilities Council
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
# Author R. W. Ford, STFC Daresbury Lab

'''Module containing the LFRicKernelMetadata
kernel-layer-specific class that captures the LFRic kernel metadata.

'''
from fparser.two import Fortran2003
from fparser.two.utils import walk, get_child

from psyclone.domain.lfric import LFRicConstants
from psyclone.domain.lfric.kernel.columnwise_operator_arg_metadata import \
    ColumnwiseOperatorArgMetadata
from psyclone.domain.lfric.kernel.field_arg_metadata import FieldArgMetadata
from psyclone.domain.lfric.kernel.field_vector_arg_metadata import \
    FieldVectorArgMetadata
from psyclone.domain.lfric.kernel.inter_grid_arg_metadata import \
    InterGridArgMetadata
from psyclone.domain.lfric.kernel.inter_grid_vector_arg_metadata import \
    InterGridVectorArgMetadata
from psyclone.domain.lfric.kernel.operator_arg_metadata import \
    OperatorArgMetadata


from psyclone.configuration import Config
from psyclone.domain.lfric.kernel.common_metadata import CommonMetadata
from psyclone.domain.lfric.kernel.common_meta_arg_metadata import \
    CommonMetaArgMetadata
from psyclone.domain.lfric.kernel.evaluator_targets_metadata import \
    EvaluatorTargetsMetadata
from psyclone.domain.lfric.kernel.meta_args_metadata import \
    MetaArgsMetadata
from psyclone.domain.lfric.kernel.meta_funcs_metadata import \
    MetaFuncsMetadata
from psyclone.domain.lfric.kernel.meta_mesh_metadata import \
    MetaMeshMetadata
from psyclone.domain.lfric.kernel.meta_ref_element_metadata import \
    MetaRefElementMetadata
from psyclone.domain.lfric.kernel.operates_on_metadata import \
    OperatesOnMetadata
from psyclone.domain.lfric.kernel.scalar_arg_metadata import ScalarArgMetadata
from psyclone.domain.lfric.kernel.shapes_metadata import ShapesMetadata
from psyclone.errors import InternalError
from psyclone.parse.utils import ParseError
from psyclone.psyir.symbols import DataTypeSymbol, UnknownFortranType


class LFRicKernelMetadata(CommonMetadata):
    '''Contains LFRic kernel metadata. This class supports kernel
    metadata creation, modification, loading from a fortran string,
    writing to a fortran string, raising from existing language-level
    PSyIR and lowering to language-level PSyIR.

    :param operates_on: the name of the quantity that this kernel is \
        intended to iterate over.
    :type operates_on: Optional[str]
    :param shapes: if a kernel requires basis or differential-basis \
        functions then the metadata must also specify the set of points on \
        which these functions are required. This information is provided \
        by the gh_shape component of the metadata.
    :type shapes: Optional[List[str]]
    :param evaluator_targets: the function spaces on which an \
        evaluator is required.
    :type evaluator_targets: Optional[List[str]]
    :param meta_args: a list of 'meta_arg' objects which capture the \
        metadata values of the kernel arguments.
    :type meta_args: Optional[List[:py:class:`psyclone.domain.lfric.kernel.\
        CommonArgMetadata`]]
    :param meta_funcs: a list of 'meta_func' objects which capture whether \
        quadrature or evaluator data is required for a given function space.
    :type meta_funcs: Optional[List[:py:class:`psyclone.domain.lfric.kernel.\
        MetaFuncsArgMetadata`]]
    :param meta_ref_element: a kernel that requires properties \
        of the reference element in LFRic specifies those properties \
        through the meta_reference_element metadata entry.
    :type meta_ref_element: Optional[:py:class:`psyclone.domain.lfric.kernel.\
        RefElementArgMetadata`]
    :param meta_mesh: a kernel that requires properties of the LFRic \
        mesh object specifies those properties through the meta_mesh \
        metadata entry.
    :type meta_mesh: Optional[:py:class:`psyclone.domain.lfric.kernel.\
        MetaMeshArgMetadata`]
    :param procedure_name: the name of the kernel procedure to call.
    :type procedure_name: Optional[str]
    :param name: the name of the symbol to use for the metadata in \
        language-level PSyIR.
    :type name: Optional[str]

    '''
    # The fparser2 class that captures this metadata.
    fparser2_class = Fortran2003.Derived_Type_Def

    def __init__(self, operates_on=None, shapes=None, evaluator_targets=None,
                 meta_args=None, meta_funcs=None, meta_ref_element=None,
                 meta_mesh=None, procedure_name=None, name=None):
        super().__init__()
        # Initialise internal variables
        self._operates_on = None
        self._shapes = None
        self._evaluator_targets = None
        self._meta_args = None
        self._meta_funcs = None
        self._meta_ref_element = None
        self._meta_mesh = None
        self._procedure_name = None
        self._name = None

        if operates_on is not None:
            self._operates_on = OperatesOnMetadata(operates_on)
        if shapes is not None:
            self._shapes = ShapesMetadata(shapes)
        if evaluator_targets is not None:
            self._evaluator_targets = EvaluatorTargetsMetadata(
                evaluator_targets)
        if meta_args is not None:
            self._meta_args = MetaArgsMetadata(meta_args)
        if meta_funcs is not None:
            self._meta_funcs = MetaFuncsMetadata(meta_funcs)
        if meta_ref_element is not None:
            self._meta_ref_element = MetaRefElementMetadata(
                meta_ref_element)
        if meta_mesh is not None:
            self._meta_mesh = MetaMeshMetadata(meta_mesh)
        if procedure_name is not None:
            # Validate procedure_name via setter
            self.procedure_name = procedure_name
        if name is not None:
            # Validate name via setter
            self.name = name

    def _get_kernel_type(self):
        '''Returns the type of kernel based on the supplied metadata. LFRic
        supports different types of kernel and the type can be infered
        from the kernel metadata as each kernel type has different
        constraints on the allowed metadata values and
        combinations. Also checks that the metadata conforms to any
        rules associated with the particular kernel type.

        :returns: the type of kernel that this is.
        :rtype: str

        '''
        if self.meta_args_get(
                [InterGridArgMetadata, InterGridVectorArgMetadata]):
            # This has to be an inter-grid kernel.
            self._validate_intergrid_kernel()
            return "inter-grid"
        elif self.meta_args_get(ColumnwiseOperatorArgMetadata):
            # This has to be a cma kernel.
            cma_type = self._cma_kernel_type()
            return f"cma-{cma_type}"
        elif self.operates_on == "domain":
            # This has to be a domain kernel.
            self._validate_domain_kernel()
            return "domain"
        else:
            # This has to be a general purpose kernel.
            self._validate_general_purpose_kernel()
            return "general-purpose"

    def _validate_generic_kernel(self):
        '''Validation checks common to multiple kernel types.

        raises ParseError: if any validation checks fail.

        '''
        # TODO issue #1953: A kernel must modify at least one of its arguments
        # TODO issue #1953: Function spaces of basis functions exist
        # TODO issue #1953: No shape if no basis or diff basis
        # TODO issue #1953: evaluator_targets only if required
        # TODO issue #1953: evaluator_targets function spaces exist

        # Kernel metadata with operates_on != domain must have at
        # least one meta_args argument that is a field, field vector,
        # intergrid field, intergrid vector field, LMA operator or CMA
        # operator (in order to determine the appropriate iteration
        # space).
        if self.operates_on != "domain" and not self.meta_args_get(
                [FieldArgMetadata, FieldVectorArgMetadata, OperatorArgMetadata,
                 ColumnwiseOperatorArgMetadata, InterGridArgMetadata,
                 InterGridVectorArgMetadata]):
            raise ParseError(
                f"Kernel metadata with 'operates_on != domain' must have at "
                f"least one meta_args argument that is a field, field vector, "
                f"intergrid field, intergrid vector field, LMA operator or "
                f"CMA operator (in order to determine the appropriate "
                f"iteration space). However, the kernel metadata "
                f"'{self.name}' for procedure '{self.procedure_name}' has "
                f"none.")

        # A kernel that contains an operator argument must only
        # contains real-valued fields.
        operator_args = self.meta_args_get(
            [OperatorArgMetadata, ColumnwiseOperatorArgMetadata])
        if operator_args:
            field_args = self.meta_args_get(
                [FieldArgMetadata, FieldVectorArgMetadata,
                 InterGridArgMetadata, InterGridVectorArgMetadata])
            for field_arg in field_args:
                if field_arg.datatype != "gh_real":
                    raise ParseError(
                        f"Kernel metadata with a meta_args operator argument "
                        f"must only contain meta_args real-valued "
                        f"field arguments. However, the kernel metadata "
                        f"'{self.name}' for procedure '{self.procedure_name}' "
                        f"contains a field of type '{field_arg.datatype}'.")

    def _validate_general_purpose_kernel(self):
        '''Validation checks for a general purpose kernel.

        raises ParseError: if any validation checks fail.

        '''
        # Generic constraints.
        self._validate_generic_kernel()

        # General-purpose kernels do not operate over the domain.
        if self.operates_on == "domain":
            raise ParseError(
                f"A general purpose kernel should not operate on a domain. "
                f"However, the kernel metadata '{self.name}' for procedure "
                f"'{self.procedure_name}' does.")

        # General-purpose kernels with operates_on = CELL_COLUMN only
        # accept meta_arg arguments of the following types: field,
        # field vector, LMA operator, scalar. Scalar meta_arg
        # arguments must be one of 'real', 'integer' or 'logical' (but
        # this is all supported types so no need to check). Scalar
        # meta_arg arguments must also be read only.
        if self.operates_on == "cell_column":
            for meta_arg in self.meta_args:
                if type(meta_arg) not in [
                        FieldArgMetadata, FieldVectorArgMetadata,
                        OperatorArgMetadata, ScalarArgMetadata]:
                    raise ParseError(
                        f"General purpose kernels with 'operates_on == "
                        f"cell_column' should only have meta_arg arguments "
                        f"of type field, field vector, LMA operator or "
                        f"scalar, but found '{meta_arg.check_name}' in kernel "
                        f"metadata '{self.name}' for procedure "
                        f"'{self.procedure_name}'.")

        # TODO issue #1953: constraints when operates_on == dofs
        # 1: They must have one and only one modified (i.e. written
        # to) argument.
        # 2: There must be at least one field in the argument
        # list. This is so that we know the number of DoFs to iterate
        # over in the PSy layer.
        # 3: Kernel arguments must be either fields or scalars (real-
        # and/or integer-valued).
        # 4: All field arguments to a given Built-in must be on the
        # same function space. This is because all current Built-ins
        # operate on DoFs and therefore all fields should have the
        # same number. It also means that we can determine the number
        # of DoFs uniquely when a scalar is written to;
        # 5: Built-ins that update real-valued fields can, in general, only
        # read from other real-valued fields, but they can take both real and
        # integer scalar arguments (see rule 8 for exceptions);
        # 6: Built-ins that update integer-valued fields can, in
        # general, only read from other integer-valued fields and take
        # integer scalar arguments (see rule 7 for exceptions);
        # 7: The only two exceptions from the rules 6) and 7) above
        # regarding the same data type of “write” and “read” field
        # arguments are Built-ins that convert field data from real to
        # integer, int_X, and from integer to real, real_X.

    def _validate_domain_kernel(self):
        '''Validation checks for a domain kernel.

        raises ParseError: if any validation checks fail.

        '''
        # Generic constraints.
        self._validate_generic_kernel()

        if self.operates_on != "domain":
            raise ParseError(
                f"Domain kernels should have their operates_on metadata set "
                f"to 'domain', but found '{self.operates_on}' in kernel "
                f"metadata '{self.name}' for procedure "
                f"'{self.procedure_name}'.")

        # Only scalar, field and field vector arguments are permitted.
        for meta_arg in self.meta_args:
            if type(meta_arg) not in [
                    ScalarArgMetadata, FieldArgMetadata,
                    FieldVectorArgMetadata]:
                raise ParseError(
                    f"Domain kernels should only have meta_arg arguments "
                    f"of type field, field vector, or scalar, but found "
                    f"'{meta_arg.check_name}' in kernel metadata "
                    f"'{self.name}' for procedure '{self.procedure_name}'.")

        # All fields must be on discontinuous function spaces and
        # stencil accesses are not permitted.
        lfric_constants = LFRicConstants()
        fields_metadata = self.meta_args_get(
            [FieldArgMetadata, FieldVectorArgMetadata])
        for meta_arg in fields_metadata:
            if meta_arg.function_space not in \
               lfric_constants.DISCONTINUOUS_FUNCTION_SPACES:
                raise ParseError(
                    f"Domain kernels meta_arg arguments of type field, or "
                    f"field vector should be on a discontinuous function "
                    f"space, but found '{meta_arg.function_space}' in kernel "
                    f"metadata '{self.name}' for procedure "
                    f"'{self.procedure_name}'.")
            if meta_arg.stencil:
                raise ParseError(
                    f"Domain kernels meta_arg arguments of type field, or "
                    f"field vector should not have any stencil accesses, but "
                    f"found a stencil of type '{meta_arg.stencil}' in kernel "
                    f"metadata '{self.name}' for procedure "
                    f"'{self.procedure_name}'.")

        # No basis/diff basis functions are allowed.
        if self.meta_funcs:
            raise ParseError(
                f"Domain kernels should not contain basis or differential "
                f"basis functions, but kernel metadata '{self.name}' for "
                f"procedure '{self.procedure_name}' does.")

        # No mesh properties are allowed.
        if self.meta_mesh:
            raise ParseError(
                f"Domain kernels should not contain mesh properties but "
                f"kernel metadata '{self.name}' for procedure "
                f"'{self.procedure_name}' does.")

    def _cma_kernel_type(self):
        '''Determine the type of cma kernel this is.

        :returns: the type of cma kernel this instance is.
        :rtype: str

        '''
        if self.meta_args_get(OperatorArgMetadata):
            # Only CMA assembly kernels have an LMA operator.
            self._validate_cma_assembly_kernel()
            return "assembly"
        elif self.meta_args_get(FieldArgMetadata):
            # CMA matrix-matrix kernels do not have Field arguments.
            self._validate_cma_apply_kernel()
            return "apply"
        else:
            self._validate_cma_matrix_matrix_kernel()
            return "matrix-matrix"

    def _validate_cma_kernel(self):
        '''Validation checks for a generic CMA kernel.

        raises ParseError: if any validation checks fail.

        '''
        # Generic constraints.
        self._validate_generic_kernel()

        # Must operate on a cell_column.
        if self.operates_on != "cell_column":
            raise ParseError(
                f"A CMA kernel should only operate on a 'cell_column'. "
                f"However, the kernel metadata '{self.name}' for procedure "
                f"'{self.procedure_name}' operates on '{self.operates_on}'.")

        # At least one CMA operator argument required.
        cma_ops = self.meta_args_get(ColumnwiseOperatorArgMetadata)
        if not cma_ops:
            raise ParseError(
                f"A CMA kernel should contain at least one cma operator "
                f"argument but none are specified in the meta_args metadata "
                f"in kernel metadata '{self.name}' for procedure "
                f"'{self.procedure_name}'.")

        # No intergrid arguments allowed.
        if self.meta_args_get(
                [InterGridArgMetadata, InterGridVectorArgMetadata]):
            raise ParseError(
                f"A CMA kernel should not contain any intergrid arguments, "
                f"but at least one was found in kernel metadata '{self.name}' "
                f"for procedure '{self.procedure_name}'.")

        # No field vector arguments allowed.
        if self.meta_args_get(FieldVectorArgMetadata):
            raise ParseError(
                f"A CMA kernel should not contain any field vector arguments, "
                f"but at least one was found in kernel metadata '{self.name}' "
                f"for procedure '{self.procedure_name}'.")

        # No stencils in field arguments.
        fields_metadata = self.meta_args_get(FieldArgMetadata)
        for meta_arg in fields_metadata:
            if meta_arg.stencil:
                raise ParseError(
                    f"A CMA kernel should not contain any fields with stencil "
                    f"accesses, but at least one was found in kernel metadata "
                    f"'{self.name}' for procedure '{self.procedure_name}'.")

    def _validate_cma_assembly_kernel(self):
        '''Validation checks for a CMA assembly kernel.

        raises ParseError: if any validation checks fail.

        '''
        # Generic CMA constraints.
        self._validate_cma_kernel()

        # One CMA operator argument.
        cma_ops = self.meta_args_get(ColumnwiseOperatorArgMetadata)
        if len(cma_ops) != 1:
            raise ParseError(
                f"A CMA assembly kernel should contain one CMA operator "
                f"argument, however kernel metadata '{self.name}' for "
                f"procedure '{self.procedure_name}' contains {len(cma_ops)}.")

        # CMA operator argument must have write access.
        if cma_ops[0].access != "gh_write":
            raise ParseError(
                f"A CMA assembly kernel should contain one CMA operator "
                f"argument with write access, however the operator in kernel "
                f"metadata '{self.name}' for procedure "
                f"'{self.procedure_name}' has access '{cma_ops[0].access}'.")

        # One or more LMA operators required.
        if not self.meta_args_get(OperatorArgMetadata):
            raise ParseError(
                f"A CMA assembly kernel should contain at least one LMA "
                f"operator but none were found in kernel metadata "
                f"'{self.name}' for procedure '{self.procedure_name}'.")

        # All arguments except the CMA argument must be read-only.
        for meta_arg in self.meta_args:
            if type(meta_arg) != ColumnwiseOperatorArgMetadata:
                if meta_arg.access != "gh_read":
                    raise ParseError(
                        f"A CMA assembly kernel should have all arguments as "
                        f"read-only apart from the CMA argument, but kernel "
                        f"metadata '{self.name}' for procedure "
                        f"'{self.procedure_name}' has a non-CMA argument with "
                        f"access '{meta_arg.access}'.")

    def _validate_cma_apply_kernel(self):
        '''Validation checks for a CMA apply kernel.

        raises ParseError: if any validation checks fail.

        '''
        # Generic CMA constraints.
        self._validate_cma_kernel()

        # One CMA operator argument.
        cma_ops = self.meta_args_get(ColumnwiseOperatorArgMetadata)
        if len(cma_ops) != 1:
            raise ParseError(
                f"A CMA apply kernel should contain one CMA operator "
                f"argument, however kernel metadata '{self.name}' for "
                f"procedure '{self.procedure_name}' contains {len(cma_ops)}.")
        cma_op = cma_ops[0]

        # CMA operator argument must be read only.
        if cma_op.access != "gh_read":
            raise ParseError(
                f"A CMA apply kernel should contain one CMA operator "
                f"argument with read access, however the operator in kernel "
                f"metadata '{self.name}' for procedure "
                f"'{self.procedure_name}' has access '{cma_op.access}'.")

        # Two field arguments.
        field_args = self.meta_args_get(FieldArgMetadata)
        if not field_args:
            raise ParseError(
                f"A CMA apply kernel should contain two field arguments, but "
                f"none were found in kernel metadata '{self.name}' for "
                f"procedure '{self.procedure_name}'.")
        if len(field_args) != 2:
            word = "were"
            if len(field_args) == 1:
                word = "was"
            raise ParseError(
                f"A CMA apply kernel should contain two field arguments, but "
                f"{len(field_args)} {word} found in kernel metadata "
                f"'{self.name}' for procedure '{self.procedure_name}'.")

        # One field that is read and one field that is written.
        if not ((field_args[0].access == "gh_read" and
                 field_args[1].access == "gh_write") or
                (field_args[0].access == "gh_write" and
                 field_args[1].access == "gh_read")):
            raise ParseError(
                f"A CMA apply kernel should contain two field arguments, one "
                f"of which is read and the other written, but found "
                f"'{field_args[0].access}' and '{field_args[1].access}' in "
                f"kernel metadata '{self.name}' for procedure "
                f"'{self.procedure_name}'.")

        # The function spaces of the read and written fields must
        # match the from and to spaces, respectively, of the CMA
        # operator.
        if field_args[0] == "gh_write":
            writer_field = field_args[0]
            reader_field = field_args[1]
        else:
            reader_field = field_args[0]
            writer_field = field_args[1]
        if writer_field.function_space != cma_op.function_space_to:
            raise ParseError(
                f"In a CMA apply kernel, the function space of the written "
                f"field must match the function space of the CMA operator's "
                f"'to' function space, but found "
                f"'{writer_field.function_space}' and "
                f"'{cma_op.function_space_to}' respectively in kernel "
                f"metadata '{self.name}' for procedure "
                f"'{self.procedure_name}'.")
        if reader_field.function_space != cma_op.function_space_from:
            raise ParseError(
                f"In a CMA apply kernel, the function space of the read "
                f"field must match the function space of the CMA operator's "
                f"'from' function space, but found "
                f"'{reader_field.function_space}' and "
                f"'{cma_op.function_space_from}' respectively in kernel "
                f"metadata '{self.name}' for procedure "
                f"'{self.procedure_name}'.")

    def _validate_cma_matrix_matrix_kernel(self):
        '''Validation checks for a CMA matrix-matrix kernel.

        raises ParseError: if any validation checks fail.

        '''
        # Generic CMA constraints.
        self._validate_cma_kernel()

        # Arguments must be CMA operators and, optionally, one or more scalars.
        for meta_arg in self.meta_args:
            if type(meta_arg) not in [
                    ColumnwiseOperatorArgMetadata, ScalarArgMetadata]:
                raise ParseError(
                    f"A CMA matrix-matrix kernel must only contain CMA "
                    f"operators or scalars, but found '{meta_arg.check_name}' "
                    f"in kernel metadata '{self.name}' for procedure "
                    f"'{self.procedure_name}'.")

        # Exactly one of the CMA arguments must be written to.
        cma_writers = [meta_arg for meta_arg in self.meta_args if
                       type(meta_arg) == ColumnwiseOperatorArgMetadata
                       and meta_arg.access == "gh_write"]
        if len(cma_writers) != 1:
            raise ParseError(
                f"A CMA matrix-matrix kernel must write to one CMA operator "
                f"argument, but found {len(cma_writers)} writers in kernel "
                f"metadata '{self.name}' for procedure "
                f"'{self.procedure_name}'.")

        # All arguments other than a single CMA argument should be read
        # only. CMA arguments have been checked. Only scalars remain
        # and these are constrained to be read-only anyway, so no more
        # checks are required.

    def _validate_intergrid_kernel(self):
        '''Validation checks for an inter-grid kernel.

        raises ParseError: if any validation checks fail.

        '''
        # Generic constraints.
        self._validate_generic_kernel()

        # Must operate on a cell_column.
        if self.operates_on != "cell_column":
            raise ParseError(
                f"An intergrid kernel should only operate on a 'cell_column'. "
                f"However, the kernel metadata '{self.name}' for procedure "
                f"'{self.procedure_name}' operates on '{self.operates_on}'.")

        # All args must be intergrid args.
        for meta_arg in self.meta_args:
            if type(meta_arg) not in [
                    InterGridArgMetadata, InterGridVectorArgMetadata]:
                raise ParseError(
                    f"An intergrid kernel should only have intergrid "
                    f"arguments, but found '{meta_arg.check_name}' in kernel "
                    f"metadata '{self.name}' for procedure "
                    f"'{self.procedure_name}'.")

        coarse_args = [meta_arg for meta_arg in self.meta_args
                       if meta_arg.mesh_arg == "gh_coarse"]
        # There must be at least one intergrid arg on a coarse mesh.
        if not coarse_args:
            raise ParseError(
                f"An intergrid kernel should have at least one intergrid "
                f"argument on the coarse mesh, but none were found in kernel "
                f"metadata '{self.name}' for procedure "
                f"'{self.procedure_name}'.")

        # All intergrid args on the coarse mesh are on the same
        # function space.
        coarse_function_space = coarse_args[0].function_space
        for coarse_arg in coarse_args[1:]:
            if coarse_arg.function_space != coarse_function_space:
                raise ParseError(
                    f"An intergrid kernel should have all of its arguments, "
                    f"that are on the coarse mesh, on the same function "
                    f"space. However, '{coarse_arg.function_space}' and "
                    f"'{coarse_function_space}' differ in kernel "
                    f"metadata '{self.name}' for procedure "
                    f"'{self.procedure_name}'.")

        fine_args = [meta_arg for meta_arg in self.meta_args
                     if meta_arg.mesh_arg == "gh_fine"]
        # There must be at least one intergrid arg on a fine mesh.
        if not fine_args:
            raise ParseError(
                f"An intergrid kernel should have at least one intergrid "
                f"argument on the fine mesh, but none were found in kernel "
                f"metadata '{self.name}' for procedure "
                f"'{self.procedure_name}'.")

        fine_function_space = fine_args[0].function_space
        # All intergrid args on the coarse mesh are on the same
        # function space.
        for fine_arg in fine_args[1:]:
            if fine_arg.function_space != fine_function_space:
                raise ParseError(
                    f"An intergrid kernel should have all of its arguments, "
                    f"that are on the fine mesh, on the same function "
                    f"space. However, '{fine_arg.function_space}' and "
                    f"'{fine_function_space}' differ in kernel "
                    f"metadata '{self.name}' for procedure "
                    f"'{self.procedure_name}'.")

        # The function space used by the args on the coarse mesh must
        # differ from the function space used by the args on the fine
        # mesh.
        if coarse_function_space == fine_function_space:
            raise ParseError(
                f"An intergrid kernel should have different function spaces "
                f"for the arguments on the coarse mesh and the arguments on "
                f"the fine mesh. However, both are on "
                f"'{coarse_function_space}' in kernel metadata '{self.name}' "
                f"for procedure '{self.procedure_name}'.")

    @staticmethod
    def create_from_psyir(symbol):
        '''Create a new instance of LFRicKernelMetadata populated with
        metadata from a kernel in language-level PSyIR.

        :param symbol: the symbol in which the metadata is stored \
            in language-level PSyIR.
        :type symbol: :py:class:`psyclone.psyir.symbols.DataTypeSymbol`

        :returns: an instance of LFRicKernelMetadata.
        :rtype: :py:class:`psyclone.domain.lfric.kernel.psyir.\
            LFRicKernelMetadata`

        :raises TypeError: if the symbol argument is not the expected \
            type.
        :raises InternalError: if the datatype of the provided symbol \
            is not the expected type.

        '''
        if not isinstance(symbol, DataTypeSymbol):
            raise TypeError(
                f"Expected a DataTypeSymbol but found a "
                f"{type(symbol).__name__}.")

        datatype = symbol.datatype

        if not isinstance(datatype, UnknownFortranType):
            raise InternalError(
                f"Expected kernel metadata to be stored in the PSyIR as "
                f"an UnknownFortranType, but found "
                f"{type(datatype).__name__}.")

        # In an UnknownFortranType, the declaration is stored as a
        # string, so use create_from_fortran_string()
        return LFRicKernelMetadata.create_from_fortran_string(
            datatype.declaration)

    @staticmethod
    def create_from_fparser2(fparser2_tree):
        '''Create an instance of this class from an fparser2 tree.

        :param fparser2_tree: fparser2 tree containing the metadata \
            for an LFRic Kernel.
        :type fparser2_tree: \
            :py:class:`fparser.two.Fortran2003.Derived_Type_Ref`

        :returns: an instance of LFRicKernelMetadata.
        :rtype: :py:class:`psyclone.domain.lfric.kernel.psyir.\
            LFRicKernelMetadata`

        :raises ParseError: if one of the meta_args entries is an \
            unexpected type.
        :raises ParseError: if the metadata type does not extend kernel_type.

        '''
        LFRicKernelMetadata.check_fparser2(
            fparser2_tree, Fortran2003.Derived_Type_Def)

        kernel_metadata = LFRicKernelMetadata()

        for fparser2_node in walk(
                fparser2_tree, Fortran2003.Data_Component_Def_Stmt):
            fortran_string = str(fparser2_node).lower()
            # pylint: disable=protected-access
            if "operates_on" in fortran_string:
                # the value of operates on (CELL_COLUMN, ...)
                kernel_metadata._operates_on = OperatesOnMetadata.\
                    create_from_fparser2(fparser2_node)
            elif "meta_args" in fortran_string:
                kernel_metadata._meta_args = MetaArgsMetadata.\
                    create_from_fparser2(fparser2_node)
            elif "meta_funcs" in fortran_string:
                kernel_metadata._meta_funcs = MetaFuncsMetadata.\
                    create_from_fparser2(fparser2_node)
            elif "gh_shape" in fortran_string:
                # the gh_shape values (gh_quadrature_XYoZ, ...)
                kernel_metadata._shapes = ShapesMetadata.create_from_fparser2(
                    fparser2_node)
            elif "gh_evaluator_targets" in fortran_string:
                # the gh_evaluator_targets values (w0, w1, ...)
                kernel_metadata._evaluator_targets = EvaluatorTargetsMetadata.\
                    create_from_fparser2(fparser2_node)
            elif "meta_reference_element" in fortran_string:
                kernel_metadata._meta_ref_element = MetaRefElementMetadata.\
                    create_from_fparser2(fparser2_node)
            elif "meta_mesh" in fortran_string:
                kernel_metadata._meta_mesh = MetaMeshMetadata.\
                    create_from_fparser2(fparser2_node)
            else:
                raise ParseError(
                    f"Found unexpected metadata declaration "
                    f"'{str(fparser2_node)}' in '{str(fparser2_tree)}'.")
            # pylint: enable=protected-access

        kernel_metadata.name = fparser2_tree.children[0].children[1].tostr()

        attribute_list = fparser2_tree.children[0].children[0]
        str_attribute_list = str(attribute_list).lower() \
            if attribute_list else ""
        if (attribute_list is None or
                "extends(kernel_type)" not in str_attribute_list):
            raise ParseError(
                f"The metadata type declaration should extend kernel_type, "
                f"but found '{fparser2_tree.children[0]}' in {fparser2_tree}.")
        kernel_metadata.procedure_name = \
            LFRicKernelMetadata._get_procedure_name(fparser2_tree)

        return kernel_metadata

    def lower_to_psyir(self):
        '''Lower the metadata to language-level PSyIR.

        :returns: metadata as stored in language-level PSyIR.
        :rtype: :py:class:`psyclone.psyir.symbols.DataTypeSymbol`

        '''
        return DataTypeSymbol(
            str(self.name), UnknownFortranType(self.fortran_string()))

    @staticmethod
    def _get_procedure_name(spec_part):
        '''Internal utility that extracts the procedure name from an
        fparser2 tree that captures LFRic metadata.

        TODO Issue #1946: potentially update the metadata to capture
        interface names as well as the interface itself. The procedure
        name will then no longer be optional.

        :param spec_part: the fparser2 parse tree containing the metadata.
        :type spec_part: :py:class:`fparser.two.Fortran2003.Derived_Type_Def`

        :returns: the value of the property.
        :rtype: Optional[str]

        :raises ParseError: if the metadata is invalid.

        '''
        # The value of 'code' should be found in a type bound
        # procedure (after the contains keyword)
        type_bound_procedure = get_child(
            spec_part, Fortran2003.Type_Bound_Procedure_Part)
        if not type_bound_procedure:
            return None
        if len(type_bound_procedure.children) != 2:
            raise ParseError(
                f"Expecting a type-bound procedure, but found "
                f"'{spec_part}'.")
        specific_binding = type_bound_procedure.children[1]
        if not isinstance(specific_binding, Fortran2003.Specific_Binding):
            raise ParseError(
                f"Expecting a specific binding for the type-bound "
                f"procedure, but found '{specific_binding}' in "
                f"'{spec_part}'.")
        binding_name = specific_binding.children[3]
        procedure_name = specific_binding.children[4]
        if binding_name.string.lower() != "code" and procedure_name:
            raise ParseError(
                f"Expecting the type-bound procedure binding-name to be "
                f"'code' if there is a procedure name, but found "
                f"'{str(binding_name)}' in '{spec_part}'.")
        if not procedure_name:
            # Support the alternative metadata format that does
            # not include 'code =>'
            procedure_name = binding_name
        return procedure_name.string

    def fortran_string(self):
        '''
        :returns: the metadata represented by this instance as Fortran.
        :rtype: str

        :raises ValueError: if the values for name, meta_arg, \
            operates_on and procedure_name have not been set.

        '''
        if not (self.operates_on and self._meta_args and self.name):
            raise ValueError(
                f"Values for operates_on, meta_args and name "
                f"must be provided before calling the fortran_string method, "
                f"but found '{self.operates_on}', '{self._meta_args}' "
                f"and '{self.name}' respectively.")

        operates_on = f"  {self._operates_on.fortran_string()}"
        meta_args = f"  {self._meta_args.fortran_string()}"

        shapes = ""
        if self._shapes:
            shapes = f"  {self._shapes.fortran_string()}"

        evaluator_targets = ""
        if self._evaluator_targets:
            evaluator_targets = f"  {self._evaluator_targets.fortran_string()}"

        meta_funcs = ""
        if self._meta_funcs:
            meta_funcs = f"  {self._meta_funcs.fortran_string()}"

        meta_ref_element = ""
        if self._meta_ref_element:
            meta_ref_element = f"  {self._meta_ref_element.fortran_string()}"

        meta_mesh = ""
        if self._meta_mesh:
            meta_mesh = f"  {self._meta_mesh.fortran_string()}"

        # TODO Issue #1946: potentially update the metadata to capture
        # interface names as well as the interface itself. The
        # procedure name will then no longer be optional.
        procedure = ""
        if self.procedure_name:
            procedure = (
                f"  CONTAINS\n"
                f"    PROCEDURE, NOPASS :: {self.procedure_name}\n")

        result = (
            f"TYPE, PUBLIC, EXTENDS(kernel_type) :: {self.name}\n"
            f"{meta_args}"
            f"{meta_funcs}"
            f"{meta_ref_element}"
            f"{meta_mesh}"
            f"{shapes}"
            f"{evaluator_targets}"
            f"{operates_on}"
            f"{procedure}"
            f"END TYPE {self.name}\n")
        return result

    @property
    def kernel_type(self):
        '''
        :returns: the type of kernel that this is.
        :rtype: str
        '''
        return self._get_kernel_type()

    @property
    def operates_on(self):
        '''
        :returns: the kernel operates_on property specified by the \
            metadata.
        :rtype: str
        '''
        if self._operates_on is None:
            return None
        return self._operates_on.operates_on

    @operates_on.setter
    def operates_on(self, value):
        '''
        :param str value: set the kernel operates_on property \
            in the metadata to the specified value.

        '''
        self._operates_on = OperatesOnMetadata(value)

    @property
    def shapes(self):
        '''
        :returns: a list of shape metadata values.
        :rtype: Optional[List[str]]

        '''
        if self._shapes is None:
            return None
        return self._shapes.shapes

    @shapes.setter
    def shapes(self, values):
        '''
        :param values: set the shape metadata to the \
            supplied list of values.
        :type values: List[str]

        '''
        self._shapes = ShapesMetadata(values)

    @property
    def evaluator_targets(self):
        '''
        :returns: a list of evaluator_targets metadata values.
        :rtype: Optional[List[str]]

        '''
        if self._evaluator_targets is None:
            return None
        return self._evaluator_targets.evaluator_targets

    @evaluator_targets.setter
    def evaluator_targets(self, values):
        '''
        :param values: set the evaluator_targets metadata to the \
            supplied list of values.
        :type values: List[str]

        '''
        self._evaluator_targets = EvaluatorTargetsMetadata(values)

    @property
    def meta_args(self):
        '''
        :returns: a list of 'meta_arg' objects which capture the \
            metadata values of the kernel arguments.
        :rtype: Optional[List[:py:class:`psyclone.domain.lfric.kernel.\
            CommonArg`]]

        '''
        if self._meta_args is None:
            return None
        return self._meta_args.meta_args_args

    @meta_args.setter
    def meta_args(self, values):
        '''
        :param values: set the meta_args metadata to the \
            supplied list of values.
        :type values: List[:py:class:`psyclone.domain.lfric.kernel.\
            CommonArg`]

        '''
        self._meta_args = MetaArgsMetadata(values)

    @property
    def meta_funcs(self):
        '''
        :returns: a list of meta_funcs metadata values.
        :rtype: Optional[List[:py:class:`psyclone.domain.lfric.kernel.\
            MetaFuncsArgMetadata`]]

        '''
        if self._meta_funcs is None:
            return None
        return self._meta_funcs.meta_funcs_args

    @meta_funcs.setter
    def meta_funcs(self, values):
        '''
        :param values: set the meta_funcs metadata to the \
            supplied list of values.
        :type values: List[:py:class:`psyclone.domain.lfric.kernel.\
            MetaFuncsArgMetadata`]

        '''
        self._meta_funcs = MetaFuncsMetadata(values)

    @property
    def meta_ref_element(self):
        '''
        :returns: a list of meta_reference_element metadata values.
        :rtype: Optional[List[:py:class:`psyclone.domain.lfric.kernel.\
            MetaRefElementArgMetadata`]]

        '''
        if self._meta_ref_element is None:
            return None
        return self._meta_ref_element.meta_ref_element_args

    @meta_ref_element.setter
    def meta_ref_element(self, values):
        '''
        :param values: set the meta_funcs metadata to the \
            supplied list of values.
        :type values: List[:py:class:`psyclone.domain.lfric.kernel.\
            MetaRefElementArgMetadata`]

        '''
        self._meta_ref_element = MetaRefElementMetadata(values)

    @property
    def meta_mesh(self):
        '''
        :returns: a list of meta_mesh metadata values.
        :rtype: Optional[List[:py:class:`psyclone.domain.lfric.kernel.\
            MetaMeshArgMetadata`]]

        '''
        if self._meta_mesh is None:
            return None
        return self._meta_mesh.meta_mesh_args

    @meta_mesh.setter
    def meta_mesh(self, values):
        '''
        :param values: set the meta_mesh metadata to the \
            supplied list of values.
        :type values: List[:py:class:`psyclone.domain.lfric.kernel.\
            MetaMeshArgMetadata`]

        '''
        self._meta_mesh = MetaMeshMetadata(values)

    @property
    def procedure_name(self):
        '''
        :returns: the kernel procedure name specified by the metadata.
        :rtype: str
        '''
        return self._procedure_name

    @procedure_name.setter
    def procedure_name(self, value):
        '''
        :param Optional[str] value: set the kernel procedure name in the \
            metadata to the specified value.

        :raises ValueError: if the metadata has an invalid value.

        '''
        if value is None:
            self._procedure_name = None
        else:
            config = Config.get()
            if not value or not config.valid_name.match(value):
                raise ValueError(
                    f"Expected procedure_name to be a valid Fortran name but "
                    f"found '{value}'.")
            self._procedure_name = value

    @property
    def name(self):
        '''
        :returns: the name of the symbol that will contain the \
            metadata when lowering.
        :rtype: str

        '''
        return self._name

    @name.setter
    def name(self, value):
        '''
        :param str value: set the name of the symbol that will contain \
            the metadata when lowering.

        :raises ValueError: if the name is not valid.

        '''
        config = Config.get()
        if not value or not config.valid_name.match(value):
            raise ValueError(
                f"Expected name to be a valid Fortran name but found "
                f"'{value}'.")
        self._name = value

    def meta_args_get(self, types):
        '''Return a list of meta_args entries with names that match the
        'types' argument.

        :param types: a meta_arg type or list of meta_arg types.
        :type names: :py:class:`psyclone.domain.lfric.kernel.\
            CommonMetaArgMetadata` or List[:py:class:`psyclone.domain.\
            lfric.kernel.CommonMetaArgMetadata`]

        :returns: a list of meta_args entries.
        :rtype: List[
            py:class:`psyclone.domain.lfric.kernel.CommonMetaArgMetadata`]

        :raises TypeError: if the types argument is not of the correct type.

        '''
        import inspect
        if not (isinstance(types, list) or
                (inspect.isclass(types) and
                 issubclass(types, CommonMetaArgMetadata))):
            raise TypeError(
                f"Expected a subclass of CommonMetaArgMetadata or a list for "
                f"the types argument, but found '{type(types).__name__}'.")
        if isinstance(types, list):
            my_types = types
        else:
            my_types = [types]
        for my_type in my_types:
            if not (inspect.isclass(my_type) and
                    issubclass(my_type, CommonMetaArgMetadata)):
                raise TypeError(
                    f"Expected list entries in the types argument to be "
                    f"subclasses of CommonMetaArgMetadata, but found "
                    f"'{type(my_type).__name__}'.")
        if not self.meta_args:
            return []
        return [meta_arg for meta_arg in self.meta_args
                if type(meta_arg) in my_types]


__all__ = ["LFRicKernelMetadata"]
