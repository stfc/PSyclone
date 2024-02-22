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
# Author A. R. Porter, STFC Daresbury Lab
# Modified by I. Kavcic, L. Turner and O. Brunt, Met Office
# Modified by J. Henrichs, Bureau of Meteorology
# Modified by R. W. Ford, N. Nobre and S. Siso, STFC Daresbury Lab

''' This module implements the support for 'built-in' operations in the
    PSyclone LFRic (Dynamo 0.3) API. Each supported built-in is implemented
    as a different Python class, all inheriting from the LFRicBuiltIn class.
    The LFRicBuiltInCallFactory creates the Python object required for
    a given built-in call. '''

# pylint: disable=too-many-lines
import abc

from psyclone.core import AccessType, Signature, VariablesAccessInfo
from psyclone.domain.lfric import LFRicConstants
from psyclone.domain.lfric.kernel import (
    LFRicKernelMetadata, FieldArgMetadata, ScalarArgMetadata,
    FieldVectorArgMetadata)
from psyclone.errors import InternalError
from psyclone.f2pygen import PSyIRGen
from psyclone.parse.utils import ParseError
from psyclone.psyGen import BuiltIn
from psyclone.psyir.nodes import (ArrayReference, Assignment, BinaryOperation,
                                  Reference, IntrinsicCall)
from psyclone.utils import a_or_an

# The name of the file containing the meta-data describing the
# built-in operations for this API
BUILTIN_DEFINITIONS_FILE = "lfric_builtins_mod.f90"


# Function to return the built-in operations that we support for this API.
# The meta-data describing these kernels is in lfric_builtins_mod.f90.
# The built-in operations F90 capitalised names are dictionary keys and need
# to be converted to lower case for invoke-generation purpose.
def get_lowercase_builtin_map(builtin_map_capitalised_dict):
    '''
    Convert the names of the supported built-in operations to lowercase
    for comparison and invoke-generation purpose.

    :param builtin_map_capitalised_dict: a dictionary of built-in names.
    :type builtin_map_capitalised_dict: dict of str

    :returns: a dictionary of lowercase Fortran built-in names as keys \
              and case-sensitive Python built-in names as values.
    :rtype: dict of str

    '''
    builtin_map_dict = {}
    for fortran_name in builtin_map_capitalised_dict:
        python_name = builtin_map_capitalised_dict[fortran_name]
        builtin_map_dict[fortran_name.lower()] = python_name
    return builtin_map_dict


class LFRicBuiltInCallFactory():
    '''Creates the necessary framework for a call to an LFRic built-in,
    This consists of the operation itself and the loop over unique
    DoFs.

    '''

    def __str__(self):
        return "Factory for a call to an LFRic built-in."

    @staticmethod
    def create(call, parent=None):
        '''
        Create the objects needed for a call to the built-in described in
        the call (BuiltInCall) object.

        :param call: details of the call to this built-in in the \
                     Algorithm layer.
        :type call: :py:class:`psyclone.parse.algorithm.BuiltInCall`
        :param parent: the schedule instance to which the built-in call \
                       belongs.
        :type parent: :py:class:`psyclone.dynamo0p3.DynInvokeSchedule`

        :raises ParseError: if the name of the function being called is \
                            not a recognised built-in.
        :raises InternalError: if the built-in does not iterate over DoFs.

        '''
        if call.func_name not in BUILTIN_MAP:
            raise ParseError(
                f"Unrecognised built-in call in LFRic API: found "
                f"'{call.func_name}' but expected one of "
                f"{list(BUILTIN_MAP_CAPITALISED.keys())}.")

        # Use our dictionary to get the correct Python object for
        # this built-in.
        builtin = BUILTIN_MAP[call.func_name]()

        # Create the loop over the appropriate entity.
        # Avoid circular import
        # pylint: disable=import-outside-toplevel
        from psyclone.domain.lfric import LFRicLoop

        if call.ktype.iterates_over == "dof":
            loop_type = "dof"
        else:
            raise InternalError(
                f"An LFRic built-in must iterate over DoFs but kernel "
                f"'{call.func_name}' iterates over "
                f"'{call.ktype.iterates_over}'")
        dofloop = LFRicLoop(parent=parent, loop_type=loop_type)

        # Use the call object (created by the parser) to set-up the state
        # of the infrastructure kernel
        builtin.load(call, parent=dofloop.loop_body)

        # Set-up its state
        dofloop.load(builtin)

        # As it is the innermost loop it has the kernel as a loop_body
        # child.
        dofloop.loop_body.addchild(builtin)

        # Return the outermost loop
        return dofloop


class LFRicBuiltIn(BuiltIn, metaclass=abc.ABCMeta):
    '''Abstract base class for a node representing a call to an LFRic
    built-in.

    :raises NotImplementedError: if a subclass of this abstract class \
        does not set the value of '_datatype'.

    '''
    _case_name = None
    _datatype = None

    def __init__(self):
        # Builtins do not accept quadrature
        self.qr_rules = {}
        # Builtins cannot request mesh properties
        self.mesh = None
        self._idx_name = None
        if not self._datatype:
            raise NotImplementedError(
                "An LFRicBuiltIn should be overridden by a subclass that "
                "sets the value of '_datatype', but '_datatype' is not set.")
        super().__init__()

    @staticmethod
    @abc.abstractmethod
    def metadata():
        '''Must be overridden by subclass.'''

    @classmethod
    def _builtin_metadata(cls, meta_args):
        '''Utility to take 'meta_args' metadata and return LFRic kernel
        metadata for a built-in. Assumes the metadata describes a
        built-in kernel that operates on a DoF and that the naming
        protocol uses the name of the metadata type and adds '_code'
        to it for the name of the subroutine.

        :param meta_args: a list of 'meta_args' metadata.
        :type meta_args: List[subclass of \
            :py:class:`psyclone.domain.lifric.kernel.CommonArgMetadata`]

        :returns: LFRic kernel metadata for this built-in.
        :rtype: :py:class:`psyclone.domain.lfric.kernel.LFRicKernelMetadata`

        '''
        return LFRicKernelMetadata(
            meta_args=meta_args,
            operates_on="dof",
            procedure_name=f"{cls._case_name}_code",
            name=cls._case_name)

    def __str__(self):
        metadata = self.metadata()
        plural = ""
        # Builtins are currenty limited to fields and scalars but add
        # in a check for field-vectors as well for future proofing.
        if len(metadata.meta_args_get([
                FieldArgMetadata, FieldVectorArgMetadata])) > 1:
            plural = "s"
        return (f"Built-in: {self._case_name} ("
                f"{self._datatype}-valued field{plural})")

    def reference_accesses(self, var_accesses):
        '''Get all variable access information from this node. The assigned-to
        variable will be set to 'WRITE'.

        :param var_accesses: VariablesAccessInfo instance that stores the \
            information about variable accesses.
        :type var_accesses: \
            :py:class:`psyclone.core.VariablesAccessInfo`

        :raises InternalError: if an unsupported argument type is encountered.

        '''
        table = self.scope.symbol_table
        # Collect all write access in a separate object, so they can be added
        # after all read access (which must happen before something is written)
        written = VariablesAccessInfo()
        suffix_map = LFRicConstants().ARG_TYPE_SUFFIX_MAPPING

        for arg in self.args:
            if arg.form in ["variable", "indexed_variable"]:
                if arg.is_field:
                    sym = table.lookup_with_tag(
                        f"{arg.name}:{suffix_map[arg.argument_type]}")
                    name = sym.name
                elif arg.is_scalar:
                    name = arg.declaration_name
                else:
                    raise InternalError(
                        f"LFRicBuiltin.reference_accesses only supports field "
                        f"and scalar arguments but got '{arg.name}' of type "
                        f"'{arg.argument_type}'")
                if arg.access == AccessType.WRITE:
                    written.add_access(Signature(name), arg.access, self)
                else:
                    var_accesses.add_access(Signature(name), arg.access, self)
        # Now merge the write access to the end of all other accesses:
        var_accesses.merge(written)
        # Forward location pointer to next index, since this built-in kernel
        # finishes a statement
        var_accesses.next_location()

    def load(self, call, parent=None):
        '''
        Populate the state of this object using the supplied call object.

        :param call: The BuiltIn object from which to extract information \
                     about this built-in call.
        :type call: :py:class:`psyclone.parse.algorithm.BuiltInCall`
        :param parent: The parent node of the kernel call in the PSyIR \
                       we are constructing. This will be a loop.
        :type parent: :py:class:`psyclone.domain.lfric.LFRicLoop`

        '''
        # Avoid circular import
        # pylint: disable=import-outside-toplevel
        from psyclone.dynamo0p3 import FSDescriptors, DynKernelArguments
        BuiltIn.load(self, call, DynKernelArguments, parent)
        self.arg_descriptors = call.ktype.arg_descriptors
        self._func_descriptors = call.ktype.func_descriptors
        self._fs_descriptors = FSDescriptors(call.ktype.func_descriptors)
        self._idx_name = self.get_dof_loop_index_symbol().name
        # Check that this built-in kernel is valid
        self._validate()

    def _validate(self):
        '''
        Check that this built-in conforms to the LFRic API.

        :raises ParseError: if a built-in call does not iterate over DoFs.
        :raises ParseError: if an argument to a built-in kernel is not \
                            one of valid argument types.
        :raises ParseError: if an argument to a built-in kernel has \
                            an invalid data type.
        :raises ParseError: if a built-in kernel writes to more than \
                            one argument.
        :raises ParseError: if a built-in kernel does not have at least \
                            one field argument.
        :raises ParseError: if all field arguments are not on the same space.
        :raises ParseError: if all field arguments of a non-conversion \
                            built-in do not have the same data type.

        '''
        const = LFRicConstants()
        # Check that our assumption that we're looping over DoFs is valid
        if self.iterates_over not in const.BUILTIN_ITERATION_SPACES:
            raise ParseError(
                f"In the LFRic API built-in calls must operate on one of "
                f"{const.BUILTIN_ITERATION_SPACES} but found "
                f"'{self.iterates_over}' for {self}.")
        # Check write count, field arguments and spaces
        write_count = 0  # Only one argument must be written to
        field_count = 0  # We must have one or more fields as arguments
        spaces = set()   # All field arguments must be on the same space
        # Field data types must be the same except for the conversion built-ins
        data_types = set()
        for arg in self.arg_descriptors:
            # Check valid argument types
            if arg.argument_type not in const.VALID_BUILTIN_ARG_TYPES:
                raise ParseError(
                    f"In the LFRic API an argument to a built-in kernel "
                    f"must be one of {const.VALID_BUILTIN_ARG_TYPES} but "
                    f"kernel '{self.name}' has an argument of type "
                    f"'{arg.argument_type}'.")
            # Check valid data types
            if arg.data_type not in const.VALID_BUILTIN_DATA_TYPES:
                raise ParseError(
                    f"In the LFRic API an argument to a built-in kernel "
                    f"must have one of {const.VALID_BUILTIN_DATA_TYPES} as "
                    f"a data type but kernel '{self.name}' has an argument "
                    f"of data type '{arg.data_type}'.")
            # Built-ins update fields DoF by DoF and therefore can have
            # WRITE/READWRITE access
            if arg.access in [AccessType.WRITE, AccessType.SUM,
                              AccessType.READWRITE]:
                write_count += 1
            if arg.argument_type in const.VALID_FIELD_NAMES:
                field_count += 1
                spaces.add(arg.function_space)
                data_types.add(arg.data_type)

        if write_count != 1:
            raise ParseError(f"A built-in kernel in the LFRic API must "
                             f"have one and only one argument that is "
                             f"written to but found {write_count} for "
                             f"kernel '{self.name}'.")
        if field_count == 0:
            raise ParseError(f"A built-in kernel in the LFRic API must have "
                             f"at least one field as an argument but "
                             f"kernel '{self.name}' has none.")
        if len(spaces) != 1:
            spaces_str = [str(x) for x in sorted(spaces)]
            raise ParseError(
                f"All field arguments to a built-in in the LFRic API "
                f"must be on the same space. However, found spaces "
                f"{spaces_str} for arguments to '{self.name}'")

        conversion_builtins = ["real_to_int_X",
                               "real_to_real_X",
                               "int_to_real_X"]
        conversion_builtins_lower = [x.lower() for x in conversion_builtins]
        if len(data_types) != 1 and self.name not in conversion_builtins_lower:
            data_types_str = [str(x) for x in sorted(data_types)]
            raise ParseError(
                f"In the LFRic API only the data type conversion built-ins "
                f"{conversion_builtins} are allowed to have field arguments of"
                f" different data types. However, found different data types "
                f"{data_types_str} for field arguments to '{self.name}'.")

    @property
    def undf_name(self):
        '''
        Dynamically looks up the name of the 'undf' variable for the
        space that this kernel updates.

        :returns: the name of the undf variable.
        :rtype: str

        '''
        field = self._arguments.iteration_space_arg()
        return field.function_space.undf_name

    @property
    def qr_required(self):
        '''
        Built-ins do not currently require quadrature.

        :returns: False
        :rtype: bool

        '''
        return False

    @property
    def reference_element(self):
        '''
        Built-ins do not require reference-element properties.

        :returns: None
        :rtype: NoneType

        '''
        return None

    def gen_code(self, parent):
        '''
        Generates LFRic API specific PSy code (in the form of 'f2pygen' AST
        nodes) for a call to this built-in. This method must be overridden
        if a sub-class does not yet implement 'lower_to_language_level()'.

        :param parent: Node in f2pygen tree to which to add call.
        :type parent: :py:class:`psyclone.f2pygen.BaseGen`

        '''
        # TODO #1010 Ultimately this routine will be removed once the LFRic
        # PSyIR has been fully migrated to use the PSyIR backends.
        # For now we create an f2pygen node from the PSyIR of this routine.
        parent.add(PSyIRGen(parent, self))

    @property
    def cma_operation(self):
        '''
        Built-ins do not perform operations with Column-Matrix-Assembly
        operators.

        :returns: None
        :rtype: NoneType

        '''
        return None

    @property
    def is_intergrid(self):
        '''
        We don't have any inter-grid built-ins.

        :returns: False
        :rtype: bool

        '''
        return False

    @property
    def fs_descriptors(self):
        '''
        :returns: a list of function space descriptor objects which \
                  contain information about the function spaces.
        :rtype: list of :py:class:`psyclone.dynamo0p3.FSDescriptor`

        '''
        return self._fs_descriptors

    def get_dof_loop_index_symbol(self):
        '''
        Finds or creates the symbol representing the index in any loops
        over DoFs.

        :returns: symbol representing the DoF loop index.
        :rtype: :py:class:`psyclone.psyir.symbols.DataSymbol`

        '''
        table = self.scope.symbol_table
        # The symbol representing the loop index is created in the LFRicLoop
        # constructor.
        return table.find_or_create_integer_symbol(
            "df", tag="dof_loop_idx")

    def get_indexed_field_argument_references(self):
        '''
        Creates a DoF-indexed StructureReference for each of the field
        arguments to this Built-In kernel. e.g. if the kernel has a field
        argument named 'fld1' then this routine will create an
        ArrayReference for 'fld1_data(df)' where 'df' is the DoF-loop
        variable and 'fld1_data' is the pointer to the data array within
        the fld1 object.

        :returns: a reference to the 'df'th element of each kernel argument
                  that is a field.
        :rtype: List[:py:class:`psyclone.psyir.nodes.ArrayReference`]

        '''
        table = self.scope.symbol_table
        idx_sym = self.get_dof_loop_index_symbol()
        suffixes = LFRicConstants().ARG_TYPE_SUFFIX_MAPPING

        refs = []
        for arg in self._arguments.args:
            if not arg.is_field:
                continue
            sym = table.lookup_with_tag(
                f"{arg.name}:{suffixes[arg.argument_type]}")
            refs.append(ArrayReference.create(sym, [Reference(idx_sym)]))
        return refs

    def get_scalar_argument_references(self):
        '''
        Finds or creates either a Reference (for a symbol) or PSyIR (for a
        literal expression) for any scalar arguments to this Built-In kernel.

        :returns: a Reference or PSyIR expression for each scalar kernel \
            argument.
        :rtype: list of subclasses of `:py:class:`psyclone.psyir.nodes.Node`

        '''
        return [arg.psyir_expression() for arg in self._arguments.args
                if arg.is_scalar]


# ******************************************************************* #
# ************** Built-ins for real-valued fields ******************* #
# ******************************************************************* #

# ------------------------------------------------------------------- #
# ============== Adding (scaled) real fields ======================== #
# ------------------------------------------------------------------- #


class LFRicXPlusYKern(LFRicBuiltIn):
    ''' Add one, real-valued, field to another and return the result as
    a third, real-valued, field.

    '''
    _case_name = "X_plus_Y"
    _datatype = "real"

    @classmethod
    def metadata(cls):
        '''Returns the kernel metadata describing this built-in. Implemented in
        a datatype-independent way to allow for re-use.

        :returns: kernel metadata describing this built-in.
        :rtype: :py:class:`psyclone.domain.lfric.kernel.LFRicKernelMetadata`

        '''
        gh_datatype = LFRicConstants().MAPPING_INTRINSIC_TYPES[cls._datatype]
        metadata = cls._builtin_metadata([
            FieldArgMetadata(gh_datatype, "gh_write", "any_space_1"),
            FieldArgMetadata(gh_datatype, "gh_read", "any_space_1"),
            FieldArgMetadata(gh_datatype, "gh_read", "any_space_1")])
        metadata.validate()
        return metadata

    def __str__(self):
        return (f"Built-in: {self._case_name} (add "
                f"{self._datatype}-valued fields)")

    def lower_to_language_level(self):
        '''
        Lowers this LFRic-specific built-in kernel to language-level PSyIR.
        This Built-In node is replaced by an Assignment node.

        :returns: the lowered version of this node.
        :rtype: :py:class:`psyclone.psyir.node.Node`

        '''
        # Get indexed references for each of the field (proxy) arguments.
        arg_refs = self.get_indexed_field_argument_references()

        # Create the PSyIR for the kernel:
        #      proxy0%data(df) = proxy1%data(df) + proxy2%data(df)
        rhs = BinaryOperation.create(BinaryOperation.Operator.ADD,
                                     arg_refs[1], arg_refs[2])
        assign = Assignment.create(arg_refs[0], rhs)
        # Finally, replace this kernel node with the Assignment
        self.replace_with(assign)
        return assign


class LFRicIncXPlusYKern(LFRicBuiltIn):
    ''' Add the second, real-valued, field to the first field and return it.

    '''
    _case_name = "inc_X_plus_Y"
    _datatype = "real"

    @classmethod
    def metadata(cls):
        '''Returns the kernel metadata describing this built-in.

        :returns: kernel metadata describing this built-in.
        :rtype: :py:class:`psyclone.domain.lfric.kernel.LFRicKernelMetadata`

        '''
        gh_datatype = LFRicConstants().MAPPING_INTRINSIC_TYPES[cls._datatype]
        return cls._builtin_metadata([
            FieldArgMetadata(gh_datatype, "gh_readwrite", "any_space_1"),
            FieldArgMetadata(gh_datatype, "gh_read", "any_space_1")])

    def __str__(self):
        return (f"Built-in: {self._case_name} (increment "
                f"{a_or_an(self._datatype)} {self._datatype}-valued field)")

    def lower_to_language_level(self):
        '''
        Lowers this LFRic-specific built-in kernel to language-level PSyIR.
        This BuiltIn node is replaced by an Assignment node.

        :returns: the lowered version of this node.
        :rtype: :py:class:`psyclone.psyir.node.Node`

        '''
        # Get indexed refs for both of the field (proxy) arguments.
        arg_refs = self.get_indexed_field_argument_references()

        # Create the PSyIR for the kernel:
        #      proxy0%data(df) = proxy0%data(df) + proxy1%data(df)
        lhs = arg_refs[0]
        rhs = BinaryOperation.create(BinaryOperation.Operator.ADD,
                                     lhs.copy(), arg_refs[1])
        assign = Assignment.create(lhs, rhs)
        # Finally, replace this kernel node with the Assignment
        self.replace_with(assign)
        return assign


class LFRicAPlusXKern(LFRicBuiltIn):
    ''' `Y = a + X` where `a` is a real scalar and `X` and `Y` are
    real-valued fields (DoF-wise addition of a scalar value).

    '''
    _case_name = "a_plus_X"
    _datatype = "real"

    @classmethod
    def metadata(cls):
        '''Returns the kernel metadata describing this built-in.

        :returns: kernel metadata describing this built-in.
        :rtype: :py:class:`psyclone.domain.lfric.kernel.LFRicKernelMetadata`

        '''
        gh_datatype = LFRicConstants().MAPPING_INTRINSIC_TYPES[cls._datatype]
        return cls._builtin_metadata([
            FieldArgMetadata(gh_datatype, "gh_write", "any_space_1"),
            ScalarArgMetadata(gh_datatype, "gh_read"),
            FieldArgMetadata(gh_datatype, "gh_read", "any_space_1")])

    def lower_to_language_level(self):
        '''
        Lowers this LFRic-specific built-in kernel to language-level PSyIR.
        This BuiltIn node is replaced by an Assignment node.

        :returns: the lowered version of this node.
        :rtype: :py:class:`psyclone.psyir.node.Node`

        '''
        # Get indexed references for each of the field (proxy) arguments.
        arg_refs = self.get_indexed_field_argument_references()
        # Get a reference for the kernel scalar argument.
        scalar_args = self.get_scalar_argument_references()

        # Create the PSyIR for the kernel:
        #      proxy0%data(df) = ascalar + proxy1%data(df)
        rhs = BinaryOperation.create(BinaryOperation.Operator.ADD,
                                     scalar_args[0], arg_refs[1])
        assign = Assignment.create(arg_refs[0], rhs)
        # Finally, replace this kernel node with the Assignment
        self.replace_with(assign)
        return assign


class LFRicIncAPlusXKern(LFRicBuiltIn):
    ''' `X = a + X` where `a` is a real scalar and `X` is a real-valued
    field (DoF-wise addition of a scalar value).

    '''
    _case_name = "inc_a_plus_X"
    _datatype = "real"

    @classmethod
    def metadata(cls):
        '''Returns the kernel metadata describing this built-in.

        :returns: kernel metadata describing this built-in.
        :rtype: :py:class:`psyclone.domain.lfric.kernel.LFRicKernelMetadata`

        '''
        gh_datatype = LFRicConstants().MAPPING_INTRINSIC_TYPES[cls._datatype]
        return cls._builtin_metadata([
            ScalarArgMetadata(gh_datatype, "gh_read"),
            FieldArgMetadata(gh_datatype, "gh_readwrite", "any_space_1")])

    def lower_to_language_level(self):
        '''
        Lowers this LFRic-specific built-in kernel to language-level PSyIR.
        This BuiltIn node is replaced by an Assignment node.

        :returns: the lowered version of this node.
        :rtype: :py:class:`psyclone.psyir.node.Node`

        '''
        # Get indexed references for each of the field (proxy) arguments.
        arg_refs = self.get_indexed_field_argument_references()
        # Get a reference for the kernel scalar argument.
        scalar_args = self.get_scalar_argument_references()

        # Create the PSyIR for the kernel:
        #      proxy0%data(df) = ascalar + proxy0%data(df)
        lhs = arg_refs[0]
        rhs = BinaryOperation.create(BinaryOperation.Operator.ADD,
                                     scalar_args[0], lhs.copy())
        assign = Assignment.create(lhs, rhs)
        # Finally, replace this kernel node with the Assignment
        self.replace_with(assign)
        return assign


class LFRicAXPlusYKern(LFRicBuiltIn):
    ''' `Z = a*X + Y` where `a` is a real scalar and `Z`, `X` and
    `Y` are real-valued fields.

    '''
    _case_name = "aX_plus_Y"
    _datatype = "real"

    @classmethod
    def metadata(cls):
        '''Returns the kernel metadata describing this built-in.

        :returns: kernel metadata describing this built-in.
        :rtype: :py:class:`psyclone.domain.lfric.kernel.LFRicKernelMetadata`

        '''
        gh_datatype = LFRicConstants().MAPPING_INTRINSIC_TYPES[cls._datatype]
        return cls._builtin_metadata([
            FieldArgMetadata(gh_datatype, "gh_write", "any_space_1"),
            ScalarArgMetadata(gh_datatype, "gh_read"),
            FieldArgMetadata(gh_datatype, "gh_read", "any_space_1"),
            FieldArgMetadata(gh_datatype, "gh_read", "any_space_1")])

    def lower_to_language_level(self):
        '''
        Lowers this LFRic-specific built-in kernel to language-level PSyIR.
        This BuiltIn node is replaced by an Assignment node.

        :returns: the lowered version of this node.
        :rtype: :py:class:`psyclone.psyir.node.Node`

        '''
        # Get indexed references for each of the field (proxy) arguments.
        arg_refs = self.get_indexed_field_argument_references()
        # Get a reference for the kernel scalar argument.
        scalar_args = self.get_scalar_argument_references()

        # Create the PSyIR for the kernel:
        #      proxy0%data(df) = ascalar * proxy1%data(df) + proxy2%data(df)
        mult_op = BinaryOperation.create(BinaryOperation.Operator.MUL,
                                         scalar_args[0], arg_refs[1])
        rhs = BinaryOperation.create(BinaryOperation.Operator.ADD,
                                     mult_op, arg_refs[2])
        assign = Assignment.create(arg_refs[0], rhs)
        # Finally, replace this kernel node with the Assignment
        self.replace_with(assign)
        return assign


class LFRicIncAXPlusYKern(LFRicBuiltIn):
    ''' `X = a*X + Y` where `a` is a real scalar and `X` and `Y` are
    real-valued fields.

    '''
    _case_name = "inc_aX_plus_Y"
    _datatype = "real"

    @classmethod
    def metadata(cls):
        '''Returns the kernel metadata describing this built-in.

        :returns: kernel metadata describing this built-in.
        :rtype: :py:class:`psyclone.domain.lfric.kernel.LFRicKernelMetadata`

        '''
        gh_datatype = LFRicConstants().MAPPING_INTRINSIC_TYPES[cls._datatype]
        return cls._builtin_metadata([
            ScalarArgMetadata(gh_datatype, "gh_read"),
            FieldArgMetadata(gh_datatype, "gh_readwrite", "any_space_1"),
            FieldArgMetadata(gh_datatype, "gh_read", "any_space_1")])

    def lower_to_language_level(self):
        '''
        Lowers this LFRic-specific built-in kernel to language-level PSyIR.
        This BuiltIn node is replaced by an Assignment node.

        :returns: the lowered version of this node.
        :rtype: :py:class:`psyclone.psyir.node.Node`

        '''
        # Get indexed references for each of the field (proxy) arguments.
        arg_refs = self.get_indexed_field_argument_references()
        # Get a reference for the kernel scalar argument.
        scalar_args = self.get_scalar_argument_references()

        # Create the PSyIR for the kernel:
        #      proxy0%data(df) = ascalar * proxy0%data(df) + proxy1%data(df)
        lhs = arg_refs[0]
        mult_op = BinaryOperation.create(BinaryOperation.Operator.MUL,
                                         scalar_args[0], lhs.copy())
        rhs = BinaryOperation.create(BinaryOperation.Operator.ADD,
                                     mult_op, arg_refs[1])
        assign = Assignment.create(lhs, rhs)
        # Finally, replace this kernel node with the Assignment
        self.replace_with(assign)
        return assign


class LFRicIncXPlusBYKern(LFRicBuiltIn):
    ''' `X = X + b*Y` where `b` is a real scalar and `X` and `Y` are
    real-valued fields.

    '''
    _case_name = "inc_X_plus_bY"
    _datatype = "real"

    @classmethod
    def metadata(cls):
        '''Returns the kernel metadata describing this built-in.

        :returns: kernel metadata describing this built-in.
        :rtype: :py:class:`psyclone.domain.lfric.kernel.LFRicKernelMetadata`

        '''
        gh_datatype = LFRicConstants().MAPPING_INTRINSIC_TYPES[cls._datatype]
        return cls._builtin_metadata([
            FieldArgMetadata(gh_datatype, "gh_readwrite", "any_space_1"),
            ScalarArgMetadata(gh_datatype, "gh_read"),
            FieldArgMetadata(gh_datatype, "gh_read", "any_space_1")])

    def lower_to_language_level(self):
        '''
        Lowers this LFRic-specific built-in kernel to language-level PSyIR.
        This BuiltIn node is replaced by an Assignment node.

        :returns: the lowered version of this node.
        :rtype: :py:class:`psyclone.psyir.node.Node`

        '''
        # Get indexed references for each of the field (proxy) arguments.
        arg_refs = self.get_indexed_field_argument_references()
        # Get a reference for the kernel scalar argument.
        scalar_args = self.get_scalar_argument_references()

        # Create the PSyIR for the kernel:
        #      proxy0%data(df) = proxy0%data(df) + bscalar * proxy1%data(df)
        lhs = arg_refs[0]
        mult_op = BinaryOperation.create(BinaryOperation.Operator.MUL,
                                         scalar_args[0], arg_refs[1])
        rhs = BinaryOperation.create(BinaryOperation.Operator.ADD,
                                     lhs.copy(), mult_op)
        assign = Assignment.create(lhs, rhs)
        # Finally, replace this kernel node with the Assignment
        self.replace_with(assign)
        return assign


class LFRicAXPlusBYKern(LFRicBuiltIn):
    ''' `Z = a*X + b*Y` where `a` and `b` are real scalars and `Z`, `X` and
    `Y` are real-valued fields.

    '''
    _case_name = "aX_plus_bY"
    _datatype = "real"

    @classmethod
    def metadata(cls):
        '''Returns the kernel metadata describing this built-in.

        :returns: kernel metadata describing this built-in.
        :rtype: :py:class:`psyclone.domain.lfric.kernel.LFRicKernelMetadata`

        '''
        gh_datatype = LFRicConstants().MAPPING_INTRINSIC_TYPES[cls._datatype]
        return cls._builtin_metadata([
            FieldArgMetadata(gh_datatype, "gh_write", "any_space_1"),
            ScalarArgMetadata(gh_datatype, "gh_read"),
            FieldArgMetadata(gh_datatype, "gh_read", "any_space_1"),
            ScalarArgMetadata(gh_datatype, "gh_read"),
            FieldArgMetadata(gh_datatype, "gh_read", "any_space_1")])

    def lower_to_language_level(self):
        '''
        Lowers this LFRic-specific built-in kernel to language-level PSyIR.
        This BuiltIn node is replaced by an Assignment node.

        :returns: the lowered version of this node.
        :rtype: :py:class:`psyclone.psyir.node.Node`

        '''
        # Get indexed references for each of the field (proxy) arguments.
        arg_refs = self.get_indexed_field_argument_references()
        # Get a reference for the kernel scalar argument.
        scalar_args = self.get_scalar_argument_references()

        # Create the PSyIR for the kernel:
        #      proxy0%data(df) = ascalar * proxy1%data(df) +
        #                        bscalar  *proxy2%data(df)
        mult_op_a = BinaryOperation.create(BinaryOperation.Operator.MUL,
                                           scalar_args[0], arg_refs[1])
        mult_op_b = BinaryOperation.create(BinaryOperation.Operator.MUL,
                                           scalar_args[1], arg_refs[2])
        rhs = BinaryOperation.create(BinaryOperation.Operator.ADD,
                                     mult_op_a, mult_op_b)
        assign = Assignment.create(arg_refs[0], rhs)
        # Finally, replace this kernel node with the Assignment
        self.replace_with(assign)
        return assign


class LFRicIncAXPlusBYKern(LFRicBuiltIn):
    ''' `X = a*X + b*Y` where `a` and `b` are real scalars and `X` and `Y`
    are real-valued fields.

    '''
    _case_name = "inc_aX_plus_bY"
    _datatype = "real"

    @classmethod
    def metadata(cls):
        '''Returns the kernel metadata describing this built-in.

        :returns: kernel metadata describing this built-in.
        :rtype: :py:class:`psyclone.domain.lfric.kernel.LFRicKernelMetadata`

        '''
        gh_datatype = LFRicConstants().MAPPING_INTRINSIC_TYPES[cls._datatype]
        return cls._builtin_metadata([
            ScalarArgMetadata(gh_datatype, "gh_read"),
            FieldArgMetadata(gh_datatype, "gh_readwrite", "any_space_1"),
            ScalarArgMetadata(gh_datatype, "gh_read"),
            FieldArgMetadata(gh_datatype, "gh_read", "any_space_1")])

    def lower_to_language_level(self):
        '''
        Lowers this LFRic-specific built-in kernel to language-level PSyIR.
        This BuiltIn node is replaced by an Assignment node.

        :returns: the lowered version of this node.
        :rtype: :py:class:`psyclone.psyir.node.Node`

        '''
        # Get indexed references for each of the field (proxy) arguments.
        arg_refs = self.get_indexed_field_argument_references()
        # Get a reference for the kernel scalar argument.
        scalar_args = self.get_scalar_argument_references()

        # Create the PSyIR for the kernel:
        #      proxy0%data(df) = ascalar * proxy0%data(df) +
        #                        bscalar * proxy1%data(df)
        lhs = arg_refs[0]
        mult_op_a = BinaryOperation.create(BinaryOperation.Operator.MUL,
                                           scalar_args[0], lhs.copy())
        mult_op_b = BinaryOperation.create(BinaryOperation.Operator.MUL,
                                           scalar_args[1], arg_refs[1])
        rhs = BinaryOperation.create(BinaryOperation.Operator.ADD,
                                     mult_op_a, mult_op_b)
        assign = Assignment.create(lhs, rhs)
        # Finally, replace this kernel node with the Assignment
        self.replace_with(assign)
        return assign


class LFRicAXPlusAYKern(LFRicBuiltIn):
    ''' `Z = a*X + a*Y = a*(X + Y)` where `a` is a real scalar and `Z`,
    `X` and `Y` are real-valued fields.

    '''
    _case_name = "aX_plus_aY"
    _datatype = "real"

    @classmethod
    def metadata(cls):
        '''Returns the kernel metadata describing this built-in.

        :returns: kernel metadata describing this built-in.
        :rtype: :py:class:`psyclone.domain.lfric.kernel.LFRicKernelMetadata`

        '''
        gh_datatype = LFRicConstants().MAPPING_INTRINSIC_TYPES[cls._datatype]
        return cls._builtin_metadata([
            FieldArgMetadata(gh_datatype, "gh_write", "any_space_1"),
            ScalarArgMetadata(gh_datatype, "gh_read"),
            FieldArgMetadata(gh_datatype, "gh_read", "any_space_1"),
            FieldArgMetadata(gh_datatype, "gh_read", "any_space_1")])

    def lower_to_language_level(self):
        '''
        Lowers this LFRic-specific built-in kernel to language-level PSyIR.
        This BuiltIn node is replaced by an Assignment node.

        :returns: the lowered version of this node.
        :rtype: :py:class:`psyclone.psyir.node.Node`

        '''
        # Get indexed references for each of the field (proxy) arguments.
        arg_refs = self.get_indexed_field_argument_references()
        # Get a reference for the kernel scalar argument.
        scalar_args = self.get_scalar_argument_references()

        # Create the PSyIR for the kernel:
        #      proxy0%data(df) = ascalar * (proxy1%data(df) + proxy2%data(df))
        add_op = BinaryOperation.create(BinaryOperation.Operator.ADD,
                                        arg_refs[1], arg_refs[2])
        rhs = BinaryOperation.create(BinaryOperation.Operator.MUL,
                                     scalar_args[0], add_op)
        assign = Assignment.create(arg_refs[0], rhs)
        # Finally, replace this kernel node with the Assignment
        self.replace_with(assign)
        return assign


# ------------------------------------------------------------------- #
# ============== Subtracting (scaled) real fields =================== #
# ------------------------------------------------------------------- #


class LFRicXMinusYKern(LFRicBuiltIn):
    ''' Subtract one, real-valued, field from another and return the
    result as a third, real-valued, field.

    '''
    _case_name = "X_minus_Y"
    _datatype = "real"

    @classmethod
    def metadata(cls):
        '''Returns the kernel metadata describing this built-in.

        :returns: kernel metadata describing this built-in.
        :rtype: :py:class:`psyclone.domain.lfric.kernel.LFRicKernelMetadata`

        '''
        gh_datatype = LFRicConstants().MAPPING_INTRINSIC_TYPES[cls._datatype]
        return cls._builtin_metadata([
            FieldArgMetadata(gh_datatype, "gh_write", "any_space_1"),
            FieldArgMetadata(gh_datatype, "gh_read", "any_space_1"),
            FieldArgMetadata(gh_datatype, "gh_read", "any_space_1")])

    def __str__(self):
        return (f"Built-in: {self._case_name} (subtract "
                f"{self._datatype}-valued fields)")

    def lower_to_language_level(self):
        '''
        Lowers this LFRic-specific built-in kernel to language-level PSyIR.
        This BuiltIn node is replaced by an Assignment node.

        :returns: the lowered version of this node.
        :rtype: :py:class:`psyclone.psyir.node.Node`

        '''
        # Get indexed references for each of the field (proxy) arguments.
        arg_refs = self.get_indexed_field_argument_references()

        # Create the PSyIR for the kernel:
        #      proxy0%data(df) = proxy1%data(df) - proxy2%data(df)
        rhs = BinaryOperation.create(BinaryOperation.Operator.SUB,
                                     arg_refs[1], arg_refs[2])
        assign = Assignment.create(arg_refs[0], rhs)
        # Finally, replace this kernel node with the Assignment
        self.replace_with(assign)
        return assign


class LFRicIncXMinusYKern(LFRicBuiltIn):
    ''' Subtract the second, real-valued, field from the first field
    and return it.

    '''
    _case_name = "inc_X_minus_Y"
    _datatype = "real"

    @classmethod
    def metadata(cls):
        '''Returns the kernel metadata describing this built-in.

        :returns: kernel metadata describing this built-in.
        :rtype: :py:class:`psyclone.domain.lfric.kernel.LFRicKernelMetadata`

        '''
        gh_datatype = LFRicConstants().MAPPING_INTRINSIC_TYPES[cls._datatype]
        return cls._builtin_metadata([
            FieldArgMetadata(gh_datatype, "gh_readwrite", "any_space_1"),
            FieldArgMetadata(gh_datatype, "gh_read", "any_space_1")])

    def __str__(self):
        return (f"Built-in: {self._case_name} (decrement "
                f"{a_or_an(self._datatype)} {self._datatype}-valued field)")

    def lower_to_language_level(self):
        '''
        Lowers this LFRic-specific built-in kernel to language-level PSyIR.
        This BuiltIn node is replaced by an Assignment node.

        :returns: the lowered version of this node.
        :rtype: :py:class:`psyclone.psyir.node.Node`

        '''
        # Get indexed refs for both of the field (proxy) arguments.
        arg_refs = self.get_indexed_field_argument_references()

        # Create the PSyIR for the kernel:
        #      proxy0%data(df) = proxy0%data(df) - proxy1%data(df)
        lhs = arg_refs[0]
        rhs = BinaryOperation.create(BinaryOperation.Operator.SUB,
                                     lhs.copy(), arg_refs[1])
        assign = Assignment.create(lhs, rhs)
        # Finally, replace this kernel node with the Assignment
        self.replace_with(assign)
        return assign


class LFRicAMinusXKern(LFRicBuiltIn):
    ''' `Y = a - X` where `a` is a real scalar and `X` and `Y` are real-valued
    fields (DoF-wise subtraction of field elements from a scalar value).

    '''
    _case_name = "a_minus_X"
    _datatype = "real"

    @classmethod
    def metadata(cls):
        '''Returns the kernel metadata describing this built-in.

        :returns: kernel metadata describing this built-in.
        :rtype: :py:class:`psyclone.domain.lfric.kernel.LFRicKernelMetadata`

        '''
        gh_datatype = LFRicConstants().MAPPING_INTRINSIC_TYPES[cls._datatype]
        return cls._builtin_metadata([
            FieldArgMetadata(gh_datatype, "gh_write", "any_space_1"),
            ScalarArgMetadata(gh_datatype, "gh_read"),
            FieldArgMetadata(gh_datatype, "gh_read", "any_space_1")])

    def lower_to_language_level(self):
        '''
        Lowers this LFRic-specific built-in kernel to language-level PSyIR.
        This BuiltIn node is replaced by an Assignment node.

        :returns: the lowered version of this node.
        :rtype: :py:class:`psyclone.psyir.node.Node`

        '''
        # Get indexed references for each of the field (proxy) arguments.
        arg_refs = self.get_indexed_field_argument_references()
        # Get a reference for the kernel scalar argument.
        scalar_args = self.get_scalar_argument_references()

        # Create the PSyIR for the kernel:
        #      proxy0%data(df) = ascalar - proxy1%data(df)
        rhs = BinaryOperation.create(BinaryOperation.Operator.SUB,
                                     scalar_args[0], arg_refs[1])
        assign = Assignment.create(arg_refs[0], rhs)
        # Finally, replace this kernel node with the Assignment
        self.replace_with(assign)
        return assign


class LFRicIncAMinusXKern(LFRicBuiltIn):
    ''' `X = a - X` where `a` is a real scalar and `X` is a real-valued
    field (DoF-wise subtraction of field elements from a scalar value).

    '''
    _case_name = "inc_a_minus_X"
    _datatype = "real"

    @classmethod
    def metadata(cls):
        '''Returns the kernel metadata describing this built-in.

        :returns: kernel metadata describing this built-in.
        :rtype: :py:class:`psyclone.domain.lfric.kernel.LFRicKernelMetadata`

        '''
        gh_datatype = LFRicConstants().MAPPING_INTRINSIC_TYPES[cls._datatype]
        return cls._builtin_metadata([
            ScalarArgMetadata(gh_datatype, "gh_read"),
            FieldArgMetadata(gh_datatype, "gh_readwrite", "any_space_1")])

    def lower_to_language_level(self):
        '''
        Lowers this LFRic-specific built-in kernel to language-level PSyIR.
        This BuiltIn node is replaced by an Assignment node.

        :returns: the lowered version of this node.
        :rtype: :py:class:`psyclone.psyir.node.Node`

        '''
        # Get indexed references for each of the field (proxy) arguments.
        arg_refs = self.get_indexed_field_argument_references()
        # Get a reference for the kernel scalar argument.
        scalar_args = self.get_scalar_argument_references()

        # Create the PSyIR for the kernel:
        #      proxy0%data(df) = ascalar - proxy0%data(df)
        lhs = arg_refs[0]
        rhs = BinaryOperation.create(BinaryOperation.Operator.SUB,
                                     scalar_args[0], lhs.copy())
        assign = Assignment.create(lhs, rhs)
        # Finally, replace this kernel node with the Assignment
        self.replace_with(assign)
        return assign


class LFRicXMinusAKern(LFRicBuiltIn):
    ''' `Y = X - a` where `a` is a real scalar and `X` and `Y` are real-valued
    fields (DoF-wise subtraction of a scalar value from field elements).

    '''
    _case_name = "X_minus_a"
    _datatype = "real"

    @classmethod
    def metadata(cls):
        '''Returns the kernel metadata describing this built-in.

        :returns: kernel metadata describing this built-in.
        :rtype: :py:class:`psyclone.domain.lfric.kernel.LFRicKernelMetadata`

        '''
        gh_datatype = LFRicConstants().MAPPING_INTRINSIC_TYPES[cls._datatype]
        return cls._builtin_metadata([
            FieldArgMetadata(gh_datatype, "gh_write", "any_space_1"),
            FieldArgMetadata(gh_datatype, "gh_read", "any_space_1"),
            ScalarArgMetadata(gh_datatype, "gh_read")])

    def lower_to_language_level(self):
        '''
        Lowers this LFRic-specific built-in kernel to language-level PSyIR.
        This BuiltIn node is replaced by an Assignment node.

        :returns: the lowered version of this node.
        :rtype: :py:class:`psyclone.psyir.node.Node`

        '''
        # Get indexed references for each of the field (proxy) arguments.
        arg_refs = self.get_indexed_field_argument_references()
        # Get a reference for the kernel scalar argument.
        scalar_args = self.get_scalar_argument_references()

        # Create the PSyIR for the kernel:
        #      proxy0%data(df) = proxy1%data(df) - ascalar
        rhs = BinaryOperation.create(BinaryOperation.Operator.SUB,
                                     arg_refs[1], scalar_args[0])
        assign = Assignment.create(arg_refs[0], rhs)
        # Finally, replace this kernel node with the Assignment
        self.replace_with(assign)
        return assign


class LFRicIncXMinusAKern(LFRicBuiltIn):
    ''' `X = X - a` where `a` is a real scalar and `X` is a real-valued
    field (DoF-wise subtraction of a scalar value from field elements).

    '''
    _case_name = "inc_X_minus_a"
    _datatype = "real"

    @classmethod
    def metadata(cls):
        '''Returns the kernel metadata describing this built-in.

        :returns: kernel metadata describing this built-in.
        :rtype: :py:class:`psyclone.domain.lfric.kernel.LFRicKernelMetadata`

        '''
        gh_datatype = LFRicConstants().MAPPING_INTRINSIC_TYPES[cls._datatype]
        return cls._builtin_metadata([
            FieldArgMetadata(gh_datatype, "gh_readwrite", "any_space_1"),
            ScalarArgMetadata(gh_datatype, "gh_read")])

    def lower_to_language_level(self):
        '''
        Lowers this LFRic-specific built-in kernel to language-level PSyIR.
        This BuiltIn node is replaced by an Assignment node.

        :returns: the lowered version of this node.
        :rtype: :py:class:`psyclone.psyir.node.Node`

        '''
        # Get indexed references for each of the field (proxy) arguments.
        arg_refs = self.get_indexed_field_argument_references()
        # Get a reference for the kernel scalar argument.
        scalar_args = self.get_scalar_argument_references()

        # Create the PSyIR for the kernel:
        #      proxy0%data(df) = proxy0%data(df) - ascalar
        lhs = arg_refs[0]
        rhs = BinaryOperation.create(BinaryOperation.Operator.SUB,
                                     lhs.copy(), scalar_args[0])
        assign = Assignment.create(lhs, rhs)
        # Finally, replace this kernel node with the Assignment
        self.replace_with(assign)
        return assign


class LFRicAXMinusYKern(LFRicBuiltIn):
    ''' `Z = a*X - Y` where `a` is a real scalar and `Z`, `X` and
    `Y` are real-valued fields.

    '''
    _case_name = "aX_minus_Y"
    _datatype = "real"

    @classmethod
    def metadata(cls):
        '''Returns the kernel metadata describing this built-in.

        :returns: kernel metadata describing this built-in.
        :rtype: :py:class:`psyclone.domain.lfric.kernel.LFRicKernelMetadata`

        '''
        gh_datatype = LFRicConstants().MAPPING_INTRINSIC_TYPES[cls._datatype]
        return cls._builtin_metadata([
            FieldArgMetadata(gh_datatype, "gh_write", "any_space_1"),
            ScalarArgMetadata(gh_datatype, "gh_read"),
            FieldArgMetadata(gh_datatype, "gh_read", "any_space_1"),
            FieldArgMetadata(gh_datatype, "gh_read", "any_space_1")])

    def lower_to_language_level(self):
        '''
        Lowers this LFRic-specific built-in kernel to language-level PSyIR.
        This BuiltIn node is replaced by an Assignment node.

        :returns: the lowered version of this node.
        :rtype: :py:class:`psyclone.psyir.node.Node`

        '''
        # Get indexed references for each of the field (proxy) arguments.
        arg_refs = self.get_indexed_field_argument_references()
        # Get a reference for the kernel scalar argument.
        scalar_args = self.get_scalar_argument_references()

        # Create the PSyIR for the kernel:
        #      proxy0%data(df) = ascalar * proxy1%data(df) - proxy2%data(df)
        mult_op = BinaryOperation.create(BinaryOperation.Operator.MUL,
                                         scalar_args[0], arg_refs[1])
        rhs = BinaryOperation.create(BinaryOperation.Operator.SUB,
                                     mult_op, arg_refs[2])
        assign = Assignment.create(arg_refs[0], rhs)
        # Finally, replace this kernel node with the Assignment
        self.replace_with(assign)
        return assign


class LFRicXMinusBYKern(LFRicBuiltIn):
    ''' `Z = X - b*Y` where `b` is a real scalar and `Z`, `X` and
    `Y` are real-valued fields.

    '''
    _case_name = "X_minus_bY"
    _datatype = "real"

    @classmethod
    def metadata(cls):
        '''Returns the kernel metadata describing this built-in.

        :returns: kernel metadata describing this built-in.
        :rtype: :py:class:`psyclone.domain.lfric.kernel.LFRicKernelMetadata`

        '''
        gh_datatype = LFRicConstants().MAPPING_INTRINSIC_TYPES[cls._datatype]
        return cls._builtin_metadata([
            FieldArgMetadata(gh_datatype, "gh_write", "any_space_1"),
            FieldArgMetadata(gh_datatype, "gh_read", "any_space_1"),
            ScalarArgMetadata(gh_datatype, "gh_read"),
            FieldArgMetadata(gh_datatype, "gh_read", "any_space_1")])

    def lower_to_language_level(self):
        '''
        Lowers this LFRic-specific built-in kernel to language-level PSyIR.
        This BuiltIn node is replaced by an Assignment node.

        :returns: the lowered version of this node.
        :rtype: :py:class:`psyclone.psyir.node.Node`

        '''
        # Get indexed references for each of the field (proxy) arguments.
        arg_refs = self.get_indexed_field_argument_references()
        # Get a reference for the kernel scalar argument.
        scalar_args = self.get_scalar_argument_references()

        # Create the PSyIR for the kernel:
        #      proxy0%data(df) = proxy1%data(df) - bscalar * proxy2%data(df)
        mult_op = BinaryOperation.create(BinaryOperation.Operator.MUL,
                                         scalar_args[0], arg_refs[2])
        rhs = BinaryOperation.create(BinaryOperation.Operator.SUB,
                                     arg_refs[1], mult_op)
        assign = Assignment.create(arg_refs[0], rhs)
        # Finally, replace this kernel node with the Assignment
        self.replace_with(assign)
        return assign


class LFRicIncXMinusBYKern(LFRicBuiltIn):
    ''' `X = X - b*Y` where `b` is a real scalar and `X` and `Y` are
    real-valued fields.

    '''
    _case_name = "inc_X_minus_bY"
    _datatype = "real"

    @classmethod
    def metadata(cls):
        '''Returns the kernel metadata describing this built-in.

        :returns: kernel metadata describing this built-in.
        :rtype: :py:class:`psyclone.domain.lfric.kernel.LFRicKernelMetadata`

        '''
        gh_datatype = LFRicConstants().MAPPING_INTRINSIC_TYPES[cls._datatype]
        return cls._builtin_metadata([
            FieldArgMetadata(gh_datatype, "gh_readwrite", "any_space_1"),
            ScalarArgMetadata(gh_datatype, "gh_read"),
            FieldArgMetadata(gh_datatype, "gh_read", "any_space_1")])

    def lower_to_language_level(self):
        '''
        Lowers this LFRic-specific built-in kernel to language-level PSyIR.
        This BuiltIn node is replaced by an Assignment node.

        :returns: the lowered version of this node.
        :rtype: :py:class:`psyclone.psyir.node.Node`

        '''
        # Get indexed references for each of the field (proxy) arguments.
        arg_refs = self.get_indexed_field_argument_references()
        # Get a reference for the kernel scalar argument.
        scalar_args = self.get_scalar_argument_references()

        # Create the PSyIR for the kernel:
        #      proxy0%data(df) = proxy0%data(df) - bscalar * proxy1%data(df)
        lhs = arg_refs[0]
        mult_op = BinaryOperation.create(BinaryOperation.Operator.MUL,
                                         scalar_args[0], arg_refs[1])
        rhs = BinaryOperation.create(BinaryOperation.Operator.SUB,
                                     lhs.copy(), mult_op)
        assign = Assignment.create(lhs, rhs)
        # Finally, replace this kernel node with the Assignment
        self.replace_with(assign)
        return assign


class LFRicAXMinusBYKern(LFRicBuiltIn):
    ''' `Z = a*X - b*Y` where `a` and `b` are real scalars and `Z`, `X` and
    `Y` are real-valued fields.

    '''
    _case_name = "aX_minus_bY"
    _datatype = "real"

    @classmethod
    def metadata(cls):
        '''Returns the kernel metadata describing this built-in.

        :returns: kernel metadata describing this built-in.
        :rtype: :py:class:`psyclone.domain.lfric.kernel.LFRicKernelMetadata`

        '''
        gh_datatype = LFRicConstants().MAPPING_INTRINSIC_TYPES[cls._datatype]
        return cls._builtin_metadata([
            FieldArgMetadata(gh_datatype, "gh_write", "any_space_1"),
            ScalarArgMetadata(gh_datatype, "gh_read"),
            FieldArgMetadata(gh_datatype, "gh_read", "any_space_1"),
            ScalarArgMetadata(gh_datatype, "gh_read"),
            FieldArgMetadata(gh_datatype, "gh_read", "any_space_1")])

    def lower_to_language_level(self):
        '''
        Lowers this LFRic-specific built-in kernel to language-level PSyIR.
        This BuiltIn node is replaced by an Assignment node.

        :returns: the lowered version of this node.
        :rtype: :py:class:`psyclone.psyir.node.Node`

        '''
        # Get indexed references for each of the field (proxy) arguments.
        arg_refs = self.get_indexed_field_argument_references()
        # Get a reference for the kernel scalar argument.
        scalar_args = self.get_scalar_argument_references()

        # Create the PSyIR for the kernel:
        #      proxy0%data(df) = ascalar * proxy1%data(df) -
        #                        bscalar * proxy2%data(df)
        mult_op_a = BinaryOperation.create(BinaryOperation.Operator.MUL,
                                           scalar_args[0], arg_refs[1])
        mult_op_b = BinaryOperation.create(BinaryOperation.Operator.MUL,
                                           scalar_args[1], arg_refs[2])
        rhs = BinaryOperation.create(BinaryOperation.Operator.SUB,
                                     mult_op_a, mult_op_b)
        assign = Assignment.create(arg_refs[0], rhs)
        # Finally, replace this kernel node with the Assignment
        self.replace_with(assign)
        return assign


# ------------------------------------------------------------------- #
# ============== Multiplying (scaled) real fields =================== #
# ------------------------------------------------------------------- #


class LFRicXTimesYKern(LFRicBuiltIn):
    ''' DoF-wise product of one, real-valued, field with another with
    the result returned as a third, real-valued, field.

    '''
    _case_name = "X_times_Y"
    _datatype = "real"

    @classmethod
    def metadata(cls):
        '''Returns the kernel metadata describing this built-in.

        :returns: kernel metadata describing this built-in.
        :rtype: :py:class:`psyclone.domain.lfric.kernel.LFRicKernelMetadata`

        '''
        gh_datatype = LFRicConstants().MAPPING_INTRINSIC_TYPES[cls._datatype]
        return cls._builtin_metadata([
            FieldArgMetadata(gh_datatype, "gh_write", "any_space_1"),
            FieldArgMetadata(gh_datatype, "gh_read", "any_space_1"),
            FieldArgMetadata(gh_datatype, "gh_read", "any_space_1")])

    def __str__(self):
        return (f"Built-in: {self._case_name} (multiply "
                f"{self._datatype}-valued fields)")

    def lower_to_language_level(self):
        '''
        Lowers this LFRic-specific built-in kernel to language-level PSyIR.
        This BuiltIn node is replaced by an Assignment node.

        :returns: the lowered version of this node.
        :rtype: :py:class:`psyclone.psyir.node.Node`

        '''
        # Get indexed references for each of the field (proxy) arguments.
        arg_refs = self.get_indexed_field_argument_references()

        # Create the PSyIR for the kernel:
        #      proxy0%data(df) = proxy1%data(df) * proxy2%data(df)
        rhs = BinaryOperation.create(BinaryOperation.Operator.MUL,
                                     arg_refs[1], arg_refs[2])
        assign = Assignment.create(arg_refs[0], rhs)
        # Finally, replace this kernel node with the Assignment
        self.replace_with(assign)
        return assign


class LFRicIncXTimesYKern(LFRicBuiltIn):
    ''' Multiply the first, real-valued, field by the second and return it.

    '''
    _case_name = "inc_X_times_Y"
    _datatype = "real"

    @classmethod
    def metadata(cls):
        '''Returns the kernel metadata describing this built-in.

        :returns: kernel metadata describing this built-in.
        :rtype: :py:class:`psyclone.domain.lfric.kernel.LFRicKernelMetadata`

        '''
        gh_datatype = LFRicConstants().MAPPING_INTRINSIC_TYPES[cls._datatype]
        return cls._builtin_metadata([
            FieldArgMetadata(gh_datatype, "gh_readwrite", "any_space_1"),
            FieldArgMetadata(gh_datatype, "gh_read", "any_space_1")])

    def __str__(self):
        return (f"Built-in: {self._case_name} (multiply one "
                f"{self._datatype}-valued field by another)")

    def lower_to_language_level(self):
        '''
        Lowers this LFRic-specific built-in kernel to language-level PSyIR.
        This BuiltIn node is replaced by an Assignment node.

        :returns: the lowered version of this node.
        :rtype: :py:class:`psyclone.psyir.node.Node`

        '''
        # Get indexed refs for both of the field (proxy) arguments.
        arg_refs = self.get_indexed_field_argument_references()

        # Create the PSyIR for the kernel:
        #      proxy0%data(df) = proxy0%data(df) * proxy1%data(df)
        lhs = arg_refs[0]
        rhs = BinaryOperation.create(BinaryOperation.Operator.MUL,
                                     lhs.copy(), arg_refs[1])
        assign = Assignment.create(lhs, rhs)
        # Finally, replace this kernel node with the Assignment
        self.replace_with(assign)
        return assign


class LFRicIncAXTimesYKern(LFRicBuiltIn):
    ''' `X = a*X*Y` where `a` is a real scalar and `X` and `Y` are
    real-valued fields.

    '''
    _case_name = "inc_aX_times_Y"
    _datatype = "real"

    @classmethod
    def metadata(cls):
        '''Returns the kernel metadata describing this built-in.

        :returns: kernel metadata describing this built-in.
        :rtype: :py:class:`psyclone.domain.lfric.kernel.LFRicKernelMetadata`

        '''
        gh_datatype = LFRicConstants().MAPPING_INTRINSIC_TYPES[cls._datatype]
        return cls._builtin_metadata([
            ScalarArgMetadata(gh_datatype, "gh_read"),
            FieldArgMetadata(gh_datatype, "gh_readwrite", "any_space_1"),
            FieldArgMetadata(gh_datatype, "gh_read", "any_space_1")])

    def lower_to_language_level(self):
        '''
        Lowers this LFRic-specific built-in kernel to language-level PSyIR.
        This BuiltIn node is replaced by an Assignment node.

        :returns: the lowered version of this node.
        :rtype: :py:class:`psyclone.psyir.node.Node`

        '''
        # Get indexed references for each of the field (proxy) arguments.
        arg_refs = self.get_indexed_field_argument_references()
        # Get a reference for the kernel scalar argument.
        scalar_args = self.get_scalar_argument_references()

        # Create the PSyIR for the kernel:
        #      proxy0%data(df) = ascalar * proxy0%data(df) * proxy1%data(df)
        lhs = arg_refs[0]
        mult_op = BinaryOperation.create(BinaryOperation.Operator.MUL,
                                         scalar_args[0], lhs.copy())
        rhs = BinaryOperation.create(BinaryOperation.Operator.MUL,
                                     mult_op, arg_refs[1])
        assign = Assignment.create(lhs, rhs)
        # Finally, replace this kernel node with the Assignment
        self.replace_with(assign)
        return assign


# ------------------------------------------------------------------- #
# ============== Scaling real fields ================================ #
# ------------------------------------------------------------------- #


class LFRicATimesXKern(LFRicBuiltIn):
    ''' Multiply the first, real-valued, field by a real scalar and
    return the result as a second, real-valued, field (`Y = a*X`).

    '''
    _case_name = "a_times_X"
    _datatype = "real"

    @classmethod
    def metadata(cls):
        '''Returns the kernel metadata describing this built-in.

        :returns: kernel metadata describing this built-in.
        :rtype: :py:class:`psyclone.domain.lfric.kernel.LFRicKernelMetadata`

        '''
        gh_datatype = LFRicConstants().MAPPING_INTRINSIC_TYPES[cls._datatype]
        return cls._builtin_metadata([
            FieldArgMetadata(gh_datatype, "gh_write", "any_space_1"),
            ScalarArgMetadata(gh_datatype, "gh_read"),
            FieldArgMetadata(gh_datatype, "gh_read", "any_space_1")])

    def __str__(self):
        return (f"Built-in: {self._case_name} (copy a scaled "
                f"{self._datatype}-valued field)")

    def lower_to_language_level(self):
        '''
        Lowers this LFRic-specific built-in kernel to language-level PSyIR.
        This BuiltIn node is replaced by an Assignment node.

        :returns: the lowered version of this node.
        :rtype: :py:class:`psyclone.psyir.node.Node`

        '''
        # Get indexed references for each of the field (proxy) arguments.
        arg_refs = self.get_indexed_field_argument_references()
        # Get a reference for the kernel scalar argument.
        scalar_args = self.get_scalar_argument_references()

        # Create the PSyIR for the kernel:
        #      proxy0%data(df) = ascalar * proxy1%data(df)
        rhs = BinaryOperation.create(BinaryOperation.Operator.MUL,
                                     scalar_args[0], arg_refs[1])
        assign = Assignment.create(arg_refs[0], rhs)
        # Finally, replace this kernel node with the Assignment
        self.replace_with(assign)
        return assign


class LFRicIncATimesXKern(LFRicBuiltIn):
    ''' Multiply a real-valued field by a real scalar and return it.

    '''
    _case_name = "inc_a_times_X"
    _datatype = "real"

    @classmethod
    def metadata(cls):
        '''Returns the kernel metadata describing this built-in.

        :returns: kernel metadata describing this built-in.
        :rtype: :py:class:`psyclone.domain.lfric.kernel.LFRicKernelMetadata`

        '''
        gh_datatype = LFRicConstants().MAPPING_INTRINSIC_TYPES[cls._datatype]
        return cls._builtin_metadata([
            ScalarArgMetadata(gh_datatype, "gh_read"),
            FieldArgMetadata(gh_datatype, "gh_readwrite", "any_space_1")])

    def __str__(self):
        return (f"Built-in: {self._case_name} (scale "
                f"{a_or_an(self._datatype)} {self._datatype}-valued field)")

    def lower_to_language_level(self):
        '''
        Lowers this LFRic-specific built-in kernel to language-level PSyIR.
        This BuiltIn node is replaced by an Assignment node.

        :returns: the lowered version of this node.
        :rtype: :py:class:`psyclone.psyir.node.Node`

        '''
        # Get indexed references for each of the field (proxy) arguments.
        arg_refs = self.get_indexed_field_argument_references()
        # Get a reference for the kernel scalar argument.
        scalar_args = self.get_scalar_argument_references()

        # Create the PSyIR for the kernel:
        #      proxy0%data(df) = ascalar * proxy0%data(df)
        lhs = arg_refs[0]
        rhs = BinaryOperation.create(BinaryOperation.Operator.MUL,
                                     scalar_args[0], lhs.copy())
        assign = Assignment.create(lhs, rhs)
        # Finally, replace this kernel node with the Assignment
        self.replace_with(assign)
        return assign


# ------------------------------------------------------------------- #
# ============== Dividing real fields =============================== #
# ------------------------------------------------------------------- #


class LFRicXDividebyYKern(LFRicBuiltIn):
    ''' Divide the first, real-valued, field by the second and return
    the result as a third, real-valued, field.

    '''
    _case_name = "X_divideby_Y"
    _datatype = "real"

    @classmethod
    def metadata(cls):
        '''Returns the kernel metadata describing this built-in.

        :returns: kernel metadata describing this built-in.
        :rtype: :py:class:`psyclone.domain.lfric.kernel.LFRicKernelMetadata`

        '''
        gh_datatype = LFRicConstants().MAPPING_INTRINSIC_TYPES[cls._datatype]
        return cls._builtin_metadata([
            FieldArgMetadata(gh_datatype, "gh_write", "any_space_1"),
            FieldArgMetadata(gh_datatype, "gh_read", "any_space_1"),
            FieldArgMetadata(gh_datatype, "gh_read", "any_space_1")])

    def __str__(self):
        return (f"Built-in: {self._case_name} (divide "
                f"{self._datatype}-valued fields)")

    def lower_to_language_level(self):
        '''
        Lowers this LFRic-specific built-in kernel to language-level PSyIR.
        This BuiltIn node is replaced by an Assignment node.

        :returns: the lowered version of this node.
        :rtype: :py:class:`psyclone.psyir.node.Node`

        '''
        # Get indexed references for each of the field (proxy) arguments.
        arg_refs = self.get_indexed_field_argument_references()

        # Create the PSyIR for the kernel:
        #      proxy0%data(df) = proxy1%data(df) / proxy2%data(df)
        rhs = BinaryOperation.create(BinaryOperation.Operator.DIV,
                                     arg_refs[1], arg_refs[2])
        assign = Assignment.create(arg_refs[0], rhs)
        # Finally, replace this kernel node with the Assignment
        self.replace_with(assign)
        return assign


class LFRicIncXDividebyYKern(LFRicBuiltIn):
    ''' Divide the first, real-valued, field by the second and return it.

    '''
    _case_name = "inc_X_divideby_Y"
    _datatype = "real"

    @classmethod
    def metadata(cls):
        '''Returns the kernel metadata describing this built-in.

        :returns: kernel metadata describing this built-in.
        :rtype: :py:class:`psyclone.domain.lfric.kernel.LFRicKernelMetadata`

        '''
        gh_datatype = LFRicConstants().MAPPING_INTRINSIC_TYPES[cls._datatype]
        return cls._builtin_metadata([
            FieldArgMetadata(gh_datatype, "gh_readwrite", "any_space_1"),
            FieldArgMetadata(gh_datatype, "gh_read", "any_space_1")])

    def __str__(self):
        return (f"Built-in: {self._case_name} (divide one "
                f"{self._datatype}-valued field by another)")

    def lower_to_language_level(self):
        '''
        Lowers this LFRic-specific built-in kernel to language-level PSyIR.
        This BuiltIn node is replaced by an Assignment node.

        :returns: the lowered version of this node.
        :rtype: :py:class:`psyclone.psyir.node.Node`

        '''
        # Get indexed refs for both of the field (proxy) arguments.
        arg_refs = self.get_indexed_field_argument_references()

        # Create the PSyIR for the kernel:
        #      proxy0%data(df) = proxy0%data(df) / proxy1%data(df)
        lhs = arg_refs[0]
        rhs = BinaryOperation.create(BinaryOperation.Operator.DIV,
                                     lhs.copy(), arg_refs[1])
        assign = Assignment.create(lhs, rhs)
        # Finally, replace this kernel node with the Assignment
        self.replace_with(assign)
        return assign


class LFRicXDividebyAKern(LFRicBuiltIn):
    ''' Divide a real-valued field by a real scalar and return the
    result in another, real-valued, field.

    '''
    _case_name = "X_divideby_a"
    _datatype = "real"

    @classmethod
    def metadata(cls):
        '''Returns the kernel metadata describing this built-in.

        :returns: kernel metadata describing this built-in.
        :rtype: :py:class:`psyclone.domain.lfric.kernel.LFRicKernelMetadata`

        '''
        gh_datatype = LFRicConstants().MAPPING_INTRINSIC_TYPES[cls._datatype]
        return cls._builtin_metadata([
            FieldArgMetadata(gh_datatype, "gh_write", "any_space_1"),
            FieldArgMetadata(gh_datatype, "gh_read", "any_space_1"),
            ScalarArgMetadata(gh_datatype, "gh_read")])

    def __str__(self):
        return (f"Built-in: {self._case_name} (divide a real-valued field "
                f"by {a_or_an(self._datatype)} {self._datatype} scalar "
                "(Y = X/a))")

    def lower_to_language_level(self):
        '''
        Lowers this LFRic-specific built-in kernel to language-level PSyIR.
        This BuiltIn node is replaced by an Assignment node.

        :returns: the lowered version of this node.
        :rtype: :py:class:`psyclone.psyir.node.Node`

        '''
        # Get indexed references for each of the field (proxy) arguments.
        arg_refs = self.get_indexed_field_argument_references()
        # Get a reference for the kernel scalar argument.
        scalar_args = self.get_scalar_argument_references()

        # Create the PSyIR for the kernel:
        #      proxy0%data(df) = proxy1%data(df) / ascalar
        rhs = BinaryOperation.create(BinaryOperation.Operator.DIV,
                                     arg_refs[1], scalar_args[0])
        assign = Assignment.create(arg_refs[0], rhs)
        # Finally, replace this kernel node with the Assignment
        self.replace_with(assign)
        return assign


class LFRicIncXDividebyAKern(LFRicBuiltIn):
    ''' Divide a real-valued field by a real scalar and return it.

    '''
    _case_name = "inc_X_divideby_a"
    _datatype = "real"

    @classmethod
    def metadata(cls):
        '''Returns the kernel metadata describing this built-in.

        :returns: kernel metadata describing this built-in.
        :rtype: :py:class:`psyclone.domain.lfric.kernel.LFRicKernelMetadata`

        '''
        gh_datatype = LFRicConstants().MAPPING_INTRINSIC_TYPES[cls._datatype]
        return cls._builtin_metadata([
            FieldArgMetadata(gh_datatype, "gh_readwrite", "any_space_1"),
            ScalarArgMetadata(gh_datatype, "gh_read")])

    def __str__(self):
        return (f"Built-in: {self._case_name} (divide a real-valued field "
                f"by {a_or_an(self._datatype)} {self._datatype} scalar "
                f"(X = X/a))")

    def lower_to_language_level(self):
        '''
        Lowers this LFRic-specific built-in kernel to language-level PSyIR.
        This BuiltIn node is replaced by an Assignment node.

        :returns: the lowered version of this node.
        :rtype: :py:class:`psyclone.psyir.node.Node`

        '''
        # Get indexed references for each of the field (proxy) arguments.
        arg_refs = self.get_indexed_field_argument_references()
        # Get a reference for the kernel scalar argument.
        scalar_args = self.get_scalar_argument_references()

        # Create the PSyIR for the kernel:
        #      proxy0%data(df) = proxy0%data(df) / ascalar
        lhs = arg_refs[0]
        rhs = BinaryOperation.create(BinaryOperation.Operator.DIV,
                                     lhs.copy(), scalar_args[0])
        assign = Assignment.create(lhs, rhs)
        # Finally, replace this kernel node with the Assignment
        self.replace_with(assign)
        return assign


# ------------------------------------------------------------------- #
# ============== Inverse scaling of real fields ===================== #
# ------------------------------------------------------------------- #


class LFRicADividebyXKern(LFRicBuiltIn):
    ''' DoF-wise division of a scalar value `a` by the elements
    of a real-valued field, `X`, storing the result in another,
    real-valued, field, `Y` (`Y = a/X`).

    '''
    _case_name = "a_divideby_X"
    _datatype = "real"

    @classmethod
    def metadata(cls):
        '''Returns the kernel metadata describing this built-in.

        :returns: kernel metadata describing this built-in.
        :rtype: :py:class:`psyclone.domain.lfric.kernel.LFRicKernelMetadata`

        '''
        gh_datatype = LFRicConstants().MAPPING_INTRINSIC_TYPES[cls._datatype]
        return cls._builtin_metadata([
            FieldArgMetadata(gh_datatype, "gh_write", "any_space_1"),
            ScalarArgMetadata(gh_datatype, "gh_read"),
            FieldArgMetadata(gh_datatype, "gh_read", "any_space_1")])

    def __str__(self):
        return (f"Built-in: {self._case_name} (inverse scaling of "
                f"{a_or_an(self._datatype)} {self._datatype}-valued "
                f"field (Y = a/X))")

    def lower_to_language_level(self):
        '''
        Lowers this LFRic-specific built-in kernel to language-level PSyIR.
        This BuiltIn node is replaced by an Assignment node.

        :returns: the lowered version of this node.
        :rtype: :py:class:`psyclone.psyir.node.Node`

        '''
        # Get indexed references for each of the field (proxy) arguments.
        arg_refs = self.get_indexed_field_argument_references()
        # Get a reference for the kernel scalar argument.
        scalar_args = self.get_scalar_argument_references()

        # Create the PSyIR for the kernel:
        #      proxy0%data(df) = ascalar / proxy1%data(df)
        rhs = BinaryOperation.create(BinaryOperation.Operator.DIV,
                                     scalar_args[0], arg_refs[1])
        assign = Assignment.create(arg_refs[0], rhs)
        # Finally, replace this kernel node with the Assignment
        self.replace_with(assign)
        return assign


class LFRicIncADividebyXKern(LFRicBuiltIn):
    ''' DoF-wise division of a scalar value `a` by the elements
    of a real-valued field, `X`, storing the result in the same
    field (`X = a/X`).

    '''
    _case_name = "inc_a_divideby_X"
    _datatype = "real"

    @classmethod
    def metadata(cls):
        '''Returns the kernel metadata describing this built-in.

        :returns: kernel metadata describing this built-in.
        :rtype: :py:class:`psyclone.domain.lfric.kernel.LFRicKernelMetadata`

        '''
        gh_datatype = LFRicConstants().MAPPING_INTRINSIC_TYPES[cls._datatype]
        return cls._builtin_metadata([
            ScalarArgMetadata(gh_datatype, "gh_read"),
            FieldArgMetadata(gh_datatype, "gh_readwrite", "any_space_1")])

    def __str__(self):
        return (f"Built-in: {self._case_name} (inverse scaling of "
                f"{a_or_an(self._datatype)} {self._datatype}-valued "
                f"field (X = a/X))")

    def lower_to_language_level(self):
        '''
        Lowers this LFRic-specific built-in kernel to language-level PSyIR.
        This BuiltIn node is replaced by an Assignment node.

        :returns: the lowered version of this node.
        :rtype: :py:class:`psyclone.psyir.node.Node`

        '''
        # Get indexed references for each of the field (proxy) arguments.
        arg_refs = self.get_indexed_field_argument_references()
        # Get a reference for the kernel scalar argument.
        scalar_args = self.get_scalar_argument_references()

        # Create the PSyIR for the kernel:
        #      proxy0%data(df) = ascalar / proxy0%data(df)
        lhs = arg_refs[0]
        rhs = BinaryOperation.create(BinaryOperation.Operator.DIV,
                                     scalar_args[0], lhs.copy())
        assign = Assignment.create(lhs, rhs)
        # Finally, replace this kernel node with the Assignment
        self.replace_with(assign)
        return assign


# ------------------------------------------------------------------- #
# ============== Raising a real field to a scalar =================== #
# ------------------------------------------------------------------- #


class LFRicIncXPowrealAKern(LFRicBuiltIn):
    ''' Raise a real-valued field to a real power and return it.

    '''
    _case_name = "inc_X_powreal_a"
    _datatype = "real"

    @classmethod
    def metadata(cls):
        '''Returns the kernel metadata describing this built-in.

        :returns: kernel metadata describing this built-in.
        :rtype: :py:class:`psyclone.domain.lfric.kernel.LFRicKernelMetadata`

        '''
        gh_datatype = LFRicConstants().MAPPING_INTRINSIC_TYPES[cls._datatype]
        return cls._builtin_metadata([
            FieldArgMetadata(gh_datatype, "gh_readwrite", "any_space_1"),
            ScalarArgMetadata("gh_real", "gh_read")])

    def __str__(self):
        return (f"Built-in: {self._case_name} (raise "
                f"{a_or_an(self._datatype)} {self._datatype}-valued field "
                f"to a real power)")

    def lower_to_language_level(self):
        '''
        Lowers this LFRic-specific built-in kernel to language-level PSyIR.
        This BuiltIn node is replaced by an Assignment node.

        :returns: the lowered version of this node.
        :rtype: :py:class:`psyclone.psyir.node.Node`

        '''
        # Get PSyIR for each of the arguments.
        arg_refs = self.get_indexed_field_argument_references()
        scalar_args = self.get_scalar_argument_references()

        # Create the PSyIR for the kernel:
        #      proxy0%data(df) = proxy0%data(df) ** real_power
        lhs = arg_refs[0]
        rhs = BinaryOperation.create(BinaryOperation.Operator.POW,
                                     lhs.copy(), scalar_args[0])
        assign = Assignment.create(lhs, rhs)
        self.replace_with(assign)
        return assign


class LFRicIncXPowintNKern(LFRicBuiltIn):
    ''' Raise a real-valued field to an integer power and return it.

    '''
    _case_name = "inc_X_powint_n"
    _datatype = "real"

    @classmethod
    def metadata(cls):
        '''Returns the kernel metadata describing this built-in.

        :returns: kernel metadata describing this built-in.
        :rtype: :py:class:`psyclone.domain.lfric.kernel.LFRicKernelMetadata`

        '''
        gh_datatype = LFRicConstants().MAPPING_INTRINSIC_TYPES[cls._datatype]
        return cls._builtin_metadata([
            FieldArgMetadata(gh_datatype, "gh_readwrite", "any_space_1"),
            ScalarArgMetadata("gh_integer", "gh_read")])

    def __str__(self):
        return (f"Built-in: {self._case_name} (raise "
                f"{a_or_an(self._datatype)} {self._datatype}-valued field "
                f"to an integer power)")

    def lower_to_language_level(self):
        '''
        Lowers this LFRic-specific built-in kernel to language-level PSyIR.
        This BuiltIn node is replaced by an Assignment node.

        :returns: the lowered version of this node.
        :rtype: :py:class:`psyclone.psyir.node.Node`

        '''
        # Get PSyIR for each of the arguments.
        arg_refs = self.get_indexed_field_argument_references()
        scalar_args = self.get_scalar_argument_references()

        # Create the PSyIR for the kernel:
        #      proxy0%data(df) = proxy0%data(df) ** int_power
        lhs = arg_refs[0]
        rhs = BinaryOperation.create(BinaryOperation.Operator.POW,
                                     lhs.copy(), scalar_args[0])
        assign = Assignment.create(lhs, rhs)
        self.replace_with(assign)
        return assign


# ------------------------------------------------------------------- #
# ============== Setting real field elements to a value  ============ #
# ------------------------------------------------------------------- #


class LFRicSetvalCKern(LFRicBuiltIn):
    ''' Set a real-valued field equal to a real scalar value.

    '''
    _case_name = "setval_c"
    _datatype = "real"

    @classmethod
    def metadata(cls):
        '''Returns the kernel metadata describing this built-in.

        :returns: kernel metadata describing this built-in.
        :rtype: :py:class:`psyclone.domain.lfric.kernel.LFRicKernelMetadata`

        '''
        gh_datatype = LFRicConstants().MAPPING_INTRINSIC_TYPES[cls._datatype]
        return cls._builtin_metadata([
            FieldArgMetadata(gh_datatype, "gh_write", "any_space_1"),
            ScalarArgMetadata(gh_datatype, "gh_read")])

    def __str__(self):
        return (f"Built-in: {self._case_name} (set {a_or_an(self._datatype)} "
                f"{self._datatype}-valued field to a {self._datatype} "
                f"scalar value)")

    def lower_to_language_level(self):
        '''
        Lowers this LFRic-specific built-in kernel to language-level PSyIR.
        This BuiltIn node is replaced by an Assignment node.

        :returns: the lowered version of this node.
        :rtype: :py:class:`psyclone.psyir.node.Node`

        '''
        # Get indexed references for each of the field (proxy) arguments.
        arg_refs = self.get_indexed_field_argument_references()
        # Get a reference for the kernel scalar argument.
        scalar_args = self.get_scalar_argument_references()

        # Create the PSyIR for the kernel:
        #      proxy0%data(df) = ascalar
        assign = Assignment.create(arg_refs[0], scalar_args[0])
        # Finally, replace this kernel node with the Assignment
        self.replace_with(assign)
        return assign


class LFRicSetvalXKern(LFRicBuiltIn):
    ''' Set a real-valued field equal to another, real-valued, field.

    '''
    _case_name = "setval_X"
    _datatype = "real"

    @classmethod
    def metadata(cls):
        '''Returns the kernel metadata describing this built-in.

        :returns: kernel metadata describing this built-in.
        :rtype: :py:class:`psyclone.domain.lfric.kernel.LFRicKernelMetadata`

        '''
        gh_datatype = LFRicConstants().MAPPING_INTRINSIC_TYPES[cls._datatype]
        return cls._builtin_metadata([
            FieldArgMetadata(gh_datatype, "gh_write", "any_space_1"),
            FieldArgMetadata(gh_datatype, "gh_read", "any_space_1")])

    def __str__(self):
        return (f"Built-in: {self._case_name} (set {a_or_an(self._datatype)} "
                f"{self._datatype}-valued field equal to another such field)")

    def lower_to_language_level(self):
        '''
        Lowers this LFRic-specific built-in kernel to language-level PSyIR.
        This BuiltIn node is replaced by an Assignment node.

        :returns: the lowered version of this node.
        :rtype: :py:class:`psyclone.psyir.node.Node`

        '''
        # Get indexed refs for both of the field (proxy) arguments.
        arg_refs = self.get_indexed_field_argument_references()

        # Create the PSyIR for the kernel:
        #      proxy0%data(df) = proxy1%data(df)
        assign = Assignment.create(arg_refs[0], arg_refs[1])
        # Finally, replace this kernel node with the Assignment
        self.replace_with(assign)
        return assign


class LFRicSetvalRandomKern(LFRicBuiltIn):
    ''' Fill a real-valued field with pseudo-random numbers.

    '''
    _case_name = "setval_random"
    _datatype = "real"

    @classmethod
    def metadata(cls):
        '''Returns the kernel metadata describing this built-in.

        :returns: kernel metadata describing this built-in.
        :rtype: :py:class:`psyclone.domain.lfric.kernel.LFRicKernelMetadata`

        '''
        gh_datatype = LFRicConstants().MAPPING_INTRINSIC_TYPES[cls._datatype]
        return cls._builtin_metadata([
            FieldArgMetadata(gh_datatype, "gh_write", "any_space_1")])

    def __str__(self):
        return (f"Built-in: {self._case_name} (fill {a_or_an(self._datatype)} "
                f"{self._datatype}-valued field with pseudo-random numbers)")

    def lower_to_language_level(self):
        '''
        Lowers this LFRic built-in kernel to language-level PSyIR.
        This BuiltIn node is replaced by an IntrinsicCall node.

        :returns: the lowered version of this node.
        :rtype: :py:class:`psyclone.psyir.node.IntrinsicCall`

        '''
        # Get indexed refs for the field (proxy) argument.
        arg_refs = self.get_indexed_field_argument_references()

        # Create the PSyIR for the kernel:
        #      call random_number(proxy0%data(df))
        call = IntrinsicCall.create(IntrinsicCall.Intrinsic.RANDOM_NUMBER,
                                    arg_refs)
        # Finally, replace this kernel node with the Assignment
        self.replace_with(call)
        return call

# ------------------------------------------------------------------- #
# ============== Inner product of real fields ======================= #
# ------------------------------------------------------------------- #


class LFRicXInnerproductYKern(LFRicBuiltIn):
    ''' Calculates the inner product of two real-valued fields,
    `innprod = SUM( X(:)*Y(:) )`.

    '''
    _case_name = "X_innerproduct_Y"
    _datatype = "real"

    @classmethod
    def metadata(cls):
        '''Returns the kernel metadata describing this built-in.

        :returns: kernel metadata describing this built-in.
        :rtype: :py:class:`psyclone.domain.lfric.kernel.LFRicKernelMetadata`

        '''
        gh_datatype = LFRicConstants().MAPPING_INTRINSIC_TYPES[cls._datatype]
        return cls._builtin_metadata([
            ScalarArgMetadata(gh_datatype, "gh_sum"),
            FieldArgMetadata(gh_datatype, "gh_read", "any_space_1"),
            FieldArgMetadata(gh_datatype, "gh_read", "any_space_1")])

    def lower_to_language_level(self):
        '''
        Lowers this LFRic-specific built-in kernel to language-level PSyIR.
        This BuiltIn node is replaced by an Assignment node.

        :returns: the lowered version of this node.
        :rtype: :py:class:`psyclone.psyir.node.Node`

        '''
        # Get indexed references for the field (proxy) argument.
        arg_refs = self.get_indexed_field_argument_references()
        # Get a reference for the kernel scalar reduction argument.
        lhs = self._reduction_reference()
        # Create the PSyIR for the kernel:
        #      asum = asum + proxy0%data(df) * proxy1%data(df)
        mult_op = BinaryOperation.create(BinaryOperation.Operator.MUL,
                                         arg_refs[0], arg_refs[1])
        rhs = BinaryOperation.create(BinaryOperation.Operator.ADD,
                                     lhs.copy(), mult_op)
        assign = Assignment.create(lhs, rhs)
        # Finally, replace this kernel node with the Assignment
        self.replace_with(assign)
        return assign


class LFRicXInnerproductXKern(LFRicBuiltIn):
    ''' Calculates the inner product of one real-valued field by itself,
    `innprod = SUM( X(:)*X(:) )`.

    '''
    _case_name = "X_innerproduct_X"
    _datatype = "real"

    @classmethod
    def metadata(cls):
        '''Returns the kernel metadata describing this built-in.

        :returns: kernel metadata describing this built-in.
        :rtype: :py:class:`psyclone.domain.lfric.kernel.LFRicKernelMetadata`

        '''
        gh_datatype = LFRicConstants().MAPPING_INTRINSIC_TYPES[cls._datatype]
        return cls._builtin_metadata([
            ScalarArgMetadata(gh_datatype, "gh_sum"),
            FieldArgMetadata(gh_datatype, "gh_read", "any_space_1")])

    def lower_to_language_level(self):
        '''
        Lowers this LFRic-specific built-in kernel to language-level PSyIR.
        This BuiltIn node is replaced by an Assignment node.

        :returns: the lowered version of this node.
        :rtype: :py:class:`psyclone.psyir.node.Node`

        '''
        # Get indexed references for the field (proxy) argument.
        arg_refs = self.get_indexed_field_argument_references()
        # Get a reference for the kernel scalar reduction argument.
        lhs = self._reduction_reference()
        # Create the PSyIR for the kernel:
        #      asum = asum + proxy0%data(df) * proxy0%data(df)
        mult_op = BinaryOperation.create(BinaryOperation.Operator.MUL,
                                         arg_refs[0].copy(), arg_refs[0])
        rhs = BinaryOperation.create(BinaryOperation.Operator.ADD,
                                     lhs.copy(), mult_op)
        assign = Assignment.create(lhs, rhs)
        # Finally, replace this kernel node with the Assignment
        self.replace_with(assign)
        return assign


# ------------------------------------------------------------------- #
# ============== Sum of real field elements ========================= #
# ------------------------------------------------------------------- #


class LFRicSumXKern(LFRicBuiltIn):
    ''' Computes the sum of the elements of a real-valued field.

    '''
    _case_name = "sum_X"
    _datatype = "real"

    @classmethod
    def metadata(cls):
        '''Returns the kernel metadata describing this built-in.

        :returns: kernel metadata describing this built-in.
        :rtype: :py:class:`psyclone.domain.lfric.kernel.LFRicKernelMetadata`

        '''
        gh_datatype = LFRicConstants().MAPPING_INTRINSIC_TYPES[cls._datatype]
        return cls._builtin_metadata([
            ScalarArgMetadata(gh_datatype, "gh_sum"),
            FieldArgMetadata(gh_datatype, "gh_read", "any_space_1")])

    def __str__(self):
        return (f"Built-in: {self._case_name} (sum {a_or_an(self._datatype)} "
                f"{self._datatype}-valued field)")

    def lower_to_language_level(self):
        '''
        Lowers this LFRic-specific built-in kernel to language-level PSyIR.
        This BuiltIn node is replaced by an Assignment node.

        :returns: the lowered version of this node.
        :rtype: :py:class:`psyclone.psyir.node.Node`

        '''
        # Get indexed references for the field (proxy) argument.
        arg_refs = self.get_indexed_field_argument_references()
        # Get a reference for the kernel scalar reduction argument.
        lhs = self._reduction_reference()
        # Create the PSyIR for the kernel:
        #      asum = asum + proxy0%data(df)
        rhs = BinaryOperation.create(BinaryOperation.Operator.ADD,
                                     lhs.copy(), arg_refs[0])
        assign = Assignment.create(lhs, rhs)
        # Finally, replace this kernel node with the Assignment
        self.replace_with(assign)
        return assign


# ------------------------------------------------------------------- #
# ============== Sign of real field elements ======================== #
# ------------------------------------------------------------------- #


class LFRicSignXKern(LFRicBuiltIn):
    ''' Returns the sign of a real-valued field elements using the
    Fortran intrinsic `sign` function, `Y = sign(a, X)`.

    '''
    _case_name = "sign_X"
    _datatype = "real"

    @classmethod
    def metadata(cls):
        '''Returns the kernel metadata describing this built-in.

        :returns: kernel metadata describing this built-in.
        :rtype: :py:class:`psyclone.domain.lfric.kernel.LFRicKernelMetadata`

        '''
        gh_datatype = LFRicConstants().MAPPING_INTRINSIC_TYPES[cls._datatype]
        return cls._builtin_metadata([
            FieldArgMetadata(gh_datatype, "gh_write", "any_space_1"),
            ScalarArgMetadata(gh_datatype, "gh_read"),
            FieldArgMetadata(gh_datatype, "gh_read", "any_space_1")])

    def __str__(self):
        return (f"Built-in: {self._case_name} (sign of "
                f"{a_or_an(self._datatype)} {self._datatype}-valued field)")

    def lower_to_language_level(self):
        '''
        Lowers this LFRic-specific built-in kernel to language-level PSyIR.
        This BuiltIn node is replaced by an Assignment node.

        :returns: the lowered version of this node.
        :rtype: :py:class:`psyclone.psyir.node.Node`

        '''
        # Get indexed references for each of the field (proxy) arguments.
        arg_refs = self.get_indexed_field_argument_references()
        # Get a reference for the kernel scalar argument.
        scalar_args = self.get_scalar_argument_references()

        # Create the PSyIR for the kernel:
        #      proxy0%data(df) = SIGN(ascalar, proxy1%data)
        rhs = IntrinsicCall.create(IntrinsicCall.Intrinsic.SIGN,
                                   [scalar_args[0], arg_refs[1]])
        assign = Assignment.create(arg_refs[0], rhs)
        # Finally, replace this kernel node with the Assignment
        self.replace_with(assign)
        return assign


# ------------------------------------------------------------------- #
# ============== Maximum of (real scalar, real field elements) ====== #
# ------------------------------------------------------------------- #


class LFRicMaxAXKern(LFRicBuiltIn):
    ''' Returns the maximum of a real scalar and real-valued field
    elements. The result is stored as another, real-valued, field:
    `Y = max(a, X)`.

    '''
    _case_name = "max_aX"
    _datatype = "real"

    @classmethod
    def metadata(cls):
        '''Returns the kernel metadata describing this built-in.

        :returns: kernel metadata describing this built-in.
        :rtype: :py:class:`psyclone.domain.lfric.kernel.LFRicKernelMetadata`

        '''
        gh_datatype = LFRicConstants().MAPPING_INTRINSIC_TYPES[cls._datatype]
        return cls._builtin_metadata([
            FieldArgMetadata(gh_datatype, "gh_write", "any_space_1"),
            ScalarArgMetadata(gh_datatype, "gh_read"),
            FieldArgMetadata(gh_datatype, "gh_read", "any_space_1")])

    def lower_to_language_level(self):
        '''
        Lowers this LFRic-specific built-in kernel to language-level PSyIR.
        This BuiltIn node is replaced by an Assignment node.

        :returns: the lowered version of this node.
        :rtype: :py:class:`psyclone.psyir.node.Node`

        '''
        # Get indexed references for each of the field (proxy) arguments.
        arg_refs = self.get_indexed_field_argument_references()
        # Get a reference for the kernel scalar argument.
        scalar_args = self.get_scalar_argument_references()

        # Create the PSyIR for the kernel:
        #      proxy0%data(df) = MAX(ascalar, proxy1%data)
        rhs = IntrinsicCall.create(IntrinsicCall.Intrinsic.MAX,
                                   [scalar_args[0], arg_refs[1]])
        assign = Assignment.create(arg_refs[0], rhs)
        # Finally, replace this kernel node with the Assignment
        self.replace_with(assign)
        return assign


class LFRicIncMaxAXKern(LFRicBuiltIn):
    ''' Returns the maximum of a real scalar and real-valued field
    elements. The result is stored in the same, real-valued, field:
    `X = max(a, X)`.

    '''
    _case_name = "inc_max_aX"
    _datatype = "real"

    @classmethod
    def metadata(cls):
        '''Returns the kernel metadata describing this built-in.

        :returns: kernel metadata describing this built-in.
        :rtype: :py:class:`psyclone.domain.lfric.kernel.LFRicKernelMetadata`

        '''
        gh_datatype = LFRicConstants().MAPPING_INTRINSIC_TYPES[cls._datatype]
        return cls._builtin_metadata([
            ScalarArgMetadata(gh_datatype, "gh_read"),
            FieldArgMetadata(gh_datatype, "gh_readwrite", "any_space_1")])

    def lower_to_language_level(self):
        '''
        Lowers this LFRic-specific built-in kernel to language-level PSyIR.
        This BuiltIn node is replaced by an Assignment node.

        :returns: the lowered version of this node.
        :rtype: :py:class:`psyclone.psyir.node.Node`

        '''
        # Get indexed references for each of the field (proxy) arguments.
        arg_refs = self.get_indexed_field_argument_references()
        # Get a reference for the kernel scalar argument.
        scalar_args = self.get_scalar_argument_references()

        # Create the PSyIR for the kernel:
        #      proxy0%data(df) = MAX(ascalar, proxy0%data)
        lhs = arg_refs[0]
        rhs = IntrinsicCall.create(IntrinsicCall.Intrinsic.MAX,
                                   [scalar_args[0], lhs.copy()])
        assign = Assignment.create(lhs, rhs)
        # Finally, replace this kernel node with the Assignment
        self.replace_with(assign)
        return assign


# ------------------------------------------------------------------- #
# ============== Minimum of (real scalar, real field elements) ====== #
# ------------------------------------------------------------------- #


class LFRicMinAXKern(LFRicBuiltIn):
    ''' Returns the minimum of a real scalar and real-valued field
    elements. The result is stored as another, real-valued, field:
    `Y = min(a, X)`.

    '''
    _case_name = "min_aX"
    _datatype = "real"

    @classmethod
    def metadata(cls):
        '''Returns the kernel metadata describing this built-in.

        :returns: kernel metadata describing this built-in.
        :rtype: :py:class:`psyclone.domain.lfric.kernel.LFRicKernelMetadata`

        '''
        gh_datatype = LFRicConstants().MAPPING_INTRINSIC_TYPES[cls._datatype]
        return cls._builtin_metadata([
            FieldArgMetadata(gh_datatype, "gh_write", "any_space_1"),
            ScalarArgMetadata(gh_datatype, "gh_read"),
            FieldArgMetadata(gh_datatype, "gh_read", "any_space_1")])

    def lower_to_language_level(self):
        '''
        Lowers this LFRic-specific built-in kernel to language-level PSyIR.
        This BuiltIn node is replaced by an Assignment node.

        :returns: the lowered version of this node.
        :rtype: :py:class:`psyclone.psyir.node.Node`

        '''
        # Get indexed references for each of the field (proxy) arguments.
        arg_refs = self.get_indexed_field_argument_references()
        # Get a reference for the kernel scalar argument.
        scalar_args = self.get_scalar_argument_references()

        # Create the PSyIR for the kernel:
        #      proxy0%data(df) = MIN(ascalar, proxy1%data)
        rhs = IntrinsicCall.create(IntrinsicCall.Intrinsic.MIN,
                                   [scalar_args[0], arg_refs[1]])
        assign = Assignment.create(arg_refs[0], rhs)
        # Finally, replace this kernel node with the Assignment
        self.replace_with(assign)
        return assign


class LFRicIncMinAXKern(LFRicBuiltIn):
    ''' Returns the minimum of a real scalar and real-valued field
    elements. The result is stored in the same, real-valued, field:
    `X = min(a, X)`.

    '''
    _case_name = "inc_min_aX"
    _datatype = "real"

    @classmethod
    def metadata(cls):
        '''Returns the kernel metadata describing this built-in.

        :returns: kernel metadata describing this built-in.
        :rtype: :py:class:`psyclone.domain.lfric.kernel.LFRicKernelMetadata`

        '''
        gh_datatype = LFRicConstants().MAPPING_INTRINSIC_TYPES[cls._datatype]
        return cls._builtin_metadata([
            ScalarArgMetadata(gh_datatype, "gh_read"),
            FieldArgMetadata(gh_datatype, "gh_readwrite", "any_space_1")])

    def lower_to_language_level(self):
        '''
        Lowers this LFRic-specific built-in kernel to language-level PSyIR.
        This BuiltIn node is replaced by an Assignment node.

        :returns: the lowered version of this node.
        :rtype: :py:class:`psyclone.psyir.node.Node`

        '''
        # Get indexed references for each of the field (proxy) arguments.
        arg_refs = self.get_indexed_field_argument_references()
        # Get a reference for the kernel scalar argument.
        scalar_args = self.get_scalar_argument_references()

        # Create the PSyIR for the kernel:
        #      proxy0%data(df) = MIN(ascalar, proxy0%data)
        lhs = arg_refs[0]
        rhs = IntrinsicCall.create(IntrinsicCall.Intrinsic.MIN,
                                   [scalar_args[0], lhs.copy()])
        assign = Assignment.create(lhs, rhs)
        # Finally, replace this kernel node with the Assignment
        self.replace_with(assign)
        return assign


# ------------------------------------------------------------------- #
# ============== Converting real to integer field elements ========== #
# ------------------------------------------------------------------- #


class LFRicRealToIntXKern(LFRicBuiltIn):
    ''' Converts real-valued field elements to integer-valued
    field elements using the Fortran intrinsic `INT` function,
    `Y = INT(X, kind=i_<prec>)`. Here `Y` is an integer-valued
    field of precision `i_<prec>` and `X` is the real-valued
    field being converted.

    '''
    _datatype = "integer"
    _case_name = "real_to_int_X"

    @classmethod
    def metadata(cls):
        '''Returns the kernel metadata describing this built-in.

        :returns: kernel metadata describing this built-in.
        :rtype: :py:class:`psyclone.domain.lfric.kernel.LFRicKernelMetadata`

        '''
        return cls._builtin_metadata([
            FieldArgMetadata("gh_integer", "gh_write", "any_space_1"),
            FieldArgMetadata("gh_real", "gh_read", "any_space_1")])

    def __str__(self):
        return (f"Built-in: {self._case_name} (convert a real-valued to "
                f"an integer-valued field)")

    def lower_to_language_level(self):
        '''
        Lowers this LFRic-specific built-in kernel to language-level PSyIR.
        This BuiltIn node is replaced by an Assignment node.

        :returns: the lowered version of this node.
        :rtype: :py:class:`psyclone.psyir.node.Node`

        '''
        # Get indexed references for each of the field (proxy) arguments.
        arg_refs = self.get_indexed_field_argument_references()

        # Create the PSyIR for the kernel:
        #      proxy0%data(df) = INT(proxy1%data, kind=i_<prec>)
        i_precision = arg_refs[0].datatype.precision
        rhs = IntrinsicCall.create(
            IntrinsicCall.Intrinsic.INT,
            [arg_refs[1], ("kind", Reference(i_precision))])
        assign = Assignment.create(arg_refs[0], rhs)
        # Finally, replace this kernel node with the Assignment
        self.replace_with(assign)
        return assign


# ------------------------------------------------------------------- #
# ============== Converting real to real field elements ============= #
# ------------------------------------------------------------------- #

class LFRicRealToRealXKern(LFRicBuiltIn):
    ''' Converts real-valued field elements to real-valued field elements
    of a different precision using the Fortran intrinsic `REAL` function,
    `Y = REAL(X, kind=r_<prec>)`. Here `Y` is a real-valued field of
    precision `kind=r_<prec>` and `X` is the real-valued field whose
    values are to be converted from their defined precision.

    '''
    _datatype = "real"
    _case_name = "real_to_real_X"

    @classmethod
    def metadata(cls):
        '''Returns the kernel metadata describing this built-in.

        :returns: kernel metadata describing this built-in.
        :rtype: :py:class:`psyclone.domain.lfric.kernel.LFRicKernelMetadata`

        '''
        return cls._builtin_metadata([
            FieldArgMetadata("gh_real", "gh_write", "any_space_1"),
            FieldArgMetadata("gh_real", "gh_read", "any_space_1")])

    def __str__(self):
        return (f"Built-in: {self._case_name} (convert a real-valued "
                f"to a real-valued field)")

    def lower_to_language_level(self):
        '''
        Lowers this LFRic-specific built-in kernel to language-level PSyIR.
        This BuiltIn node is replaced by an Assignment node.

        :returns: the lowered version of this node.
        :rtype: :py:class:`psyclone.psyir.node.Node`

        '''
        # Get indexed references for each of the field (proxy) arguments.
        arg_refs = self.get_indexed_field_argument_references()

        # Create the PSyIR for the kernel:
        #      proxy0%data(df) = REAL(proxy1%data, kind=r_<prec>)
        r_precision = arg_refs[0].datatype.precision
        rhs = IntrinsicCall.create(
            IntrinsicCall.Intrinsic.REAL,
            [arg_refs[1], ("kind", Reference(r_precision))])
        assign = Assignment.create(arg_refs[0], rhs)
        # Finally, replace this kernel node with the Assignment
        self.replace_with(assign)
        return assign


# ******************************************************************* #
# ************** Built-ins for integer-valued fields **************** #
# ******************************************************************* #

# ------------------------------------------------------------------- #
# ============== Adding integer fields ============================== #
# ------------------------------------------------------------------- #

class LFRicIntXPlusYKern(LFRicXPlusYKern):
    ''' Add corresponding elements of two, integer-valued, fields, `X`
    and `Y`, and return the result as a third, integer-valued, field, `Z`.
    Inherits the `lower_to_language_level` method from the real-valued
    built-in equivalent `LFRicXPlusYKern`.

    '''
    _case_name = "int_X_plus_Y"
    _datatype = "integer"


class LFRicIntIncXPlusYKern(LFRicIncXPlusYKern):
    ''' Add each element of an integer-valued field, `X`, to the
    corresponding element of another integer-valued field, `Y`, and
    store the result back in `X`.
    Inherits the `lower_to_language_level` method from the real-valued
    built-in equivalent `LFRicIncXPlusYKern`.

    '''
    _case_name = "int_inc_X_plus_Y"
    _datatype = "integer"


class LFRicIntAPlusXKern(LFRicAPlusXKern):
    ''' Add an integer scalar value, `a`, to each element of an
    integer-valued field, `X`, and return the result as a second,
    integer-valued, field, `Y`.
    Inherits the `lower_to_language_level` method from the real-valued
    built-in equivalent `LFRicAPlusXKern`.

    '''
    _case_name = "int_a_plus_X"
    _datatype = "integer"


class LFRicIntIncAPlusXKern(LFRicIncAPlusXKern):
    ''' Add an integer scalar value, `a`, to each element of an
    integer-valued field, `X`, and return the result in the
    same field.
    Inherits the `lower_to_language_level` method from the real-valued
    built-in equivalent `LFRicIncAPlusXKern`.

    '''
    _case_name = "int_inc_a_plus_X"
    _datatype = "integer"


# ------------------------------------------------------------------- #
# ============== Subtracting integer fields ========================= #
# ------------------------------------------------------------------- #


class LFRicIntXMinusYKern(LFRicXMinusYKern):
    ''' Subtract each element of an integer-valued field, `Y`, from
    the corresponding element of another, integer-valued, field, `X`,
    and return the result as a third, integer-valued, field, `Z`.
    Inherits the `lower_to_language_level` method from the real-valued
    built-in equivalent `LFRicXMinusYKern`.

    '''
    _case_name = "int_X_minus_Y"
    _datatype = "integer"


class LFRicIntIncXMinusYKern(LFRicIncXMinusYKern):
    ''' Subtract each element of an integer-valued field, `Y`, from
    the corresponding element of another, integer-valued, field, `X`,
    and store the result back in `X`.
    Inherits the `lower_to_language_level` method from the real-valued
    built-in equivalent `LFRicIncXMinusYKern`.

    '''
    _case_name = "int_inc_X_minus_Y"
    _datatype = "integer"


class LFRicIntAMinusXKern(LFRicAMinusXKern):
    ''' Subtract each element of an integer-valued field, `X`, from
    an integer scalar value, `a`, and return the result as a second,
    integer-valued, field, `Y`.
    Inherits the `lower_to_language_level` method from the real-valued
    built-in equivalent `LFRicAMinusXKern`.

    '''
    _case_name = "int_a_minus_X"
    _datatype = "integer"


class LFRicIntIncAMinusXKern(LFRicIncAMinusXKern):
    ''' Subtract each element of an integer-valued field, `X`, from
    an integer scalar value, `a`, and return the result in the
    same field.
    Inherits the `lower_to_language_level` method from the real-valued
    built-in equivalent `LFRicIncAMinusXKern`.

    '''
    _case_name = "int_inc_a_minus_X"
    _datatype = "integer"


class LFRicIntXMinusAKern(LFRicXMinusAKern):
    ''' Subtract an integer scalar value, `a`, from each element of an
    integer-valued field, `X`, and return the result as a second,
    integer-valued, field, `Y`.
    Inherits the `lower_to_language_level` method from the real-valued
    built-in equivalent `LFRicXMinusAKern`.

    '''
    _case_name = "int_X_minus_a"
    _datatype = "integer"


class LFRicIntIncXMinusAKern(LFRicIncXMinusAKern):
    ''' Subtract an integer scalar value, `a`, from each element of an
    integer-valued field, `X`, and return the result in the same field.
    Inherits the `lower_to_language_level` method from the real-valued
    built-in equivalent `LFRicIncXMinusAKern`.

    '''
    _case_name = "int_inc_X_minus_a"
    _datatype = "integer"


# ------------------------------------------------------------------- #
# ============== Multiplying integer fields ========================= #
# ------------------------------------------------------------------- #


class LFRicIntXTimesYKern(LFRicXTimesYKern):
    ''' Multiply each element of one, integer-valued, field, `X`, by
    the corresponding element of another, integer-valued, field, `Y`,
    and return the result as a third, integer-valued, field, `Z`.
    Inherits the `lower_to_language_level` method from the real-valued
    built-in equivalent `LFRicXTimesYKern`.

    '''
    _case_name = "int_X_times_Y"
    _datatype = "integer"


class LFRicIntIncXTimesYKern(LFRicIncXTimesYKern):
    ''' Multiply each element of one, integer-valued, field, `X`, by
    the corresponding element of another, integer-valued, field, `Y`,
    and store the result back in `X`.
    Inherits the `lower_to_language_level` method from the real-valued
    built-in equivalent `LFRicIncXTimesYKern`.

    '''
    _case_name = "int_inc_X_times_Y"
    _datatype = "integer"


# ------------------------------------------------------------------- #
# ============== Scaling integer fields ============================= #
# ------------------------------------------------------------------- #


class LFRicIntATimesXKern(LFRicATimesXKern):
    ''' Multiply each element of the first, integer-valued, field, `X`,
    by an integer scalar, `a`, and return the result as a second,
    integer-valued, field `Y` (`Y = a*X`).
    Inherits the `lower_to_language_level` method from the real-valued
    built-in equivalent `LFRicATimesXKern`.

    '''
    _case_name = "int_a_times_X"
    _datatype = "integer"


class LFRicIntIncATimesXKern(LFRicIncATimesXKern):
    ''' Multiply each element of an integer-valued field, `X` by
    an integer scalar, `a`, and store the result back in `X`.
    Inherits the `lower_to_language_level` method from the real-valued
    built-in equivalent `LFRicIncATimesXKern`.

    '''
    _case_name = "int_inc_a_times_X"
    _datatype = "integer"


# ------------------------------------------------------------------- #
# ============== Setting integer field elements to a value  ========= #
# ------------------------------------------------------------------- #


class LFRicIntSetvalCKern(LFRicSetvalCKern):
    ''' Assign a single constant integer scalar value, `c`, to all
    elements of an integer-valued field, `X`.
    Inherits the `lower_to_language_level` method from the real-valued
    built-in equivalent `LFRicSetvalCKern`.

    '''
    _case_name = "int_setval_c"
    _datatype = "integer"


class LFRicIntSetvalXKern(LFRicSetvalXKern):
    ''' Copy one element of an integer-valued field (second argument),
    `X`, to the corresponding element of another, integer-valued,
    field (first argument), `Y`.
    Inherits the `lower_to_language_level` method from the real-valued
    built-in equivalent `LFRicSetvalXKern`.

    '''
    _case_name = "int_setval_X"
    _datatype = "integer"


# ------------------------------------------------------------------- #
# ============== Sign of integer field elements ===================== #
# ------------------------------------------------------------------- #


class LFRicIntSignXKern(LFRicSignXKern):
    ''' Returns the sign of an integer-valued field elements using the
    Fortran intrinsic `sign` function, `Y = sign(a, X)`.
    Inherits the `lower_to_language_level` method from the real-valued
    built-in equivalent `LFRicSignXKern`.

    '''
    _case_name = "int_sign_X"
    _datatype = "integer"


# ------------------------------------------------------------------- #
# ======== Maximum of (integer scalar, integer field elements) ====== #
# ------------------------------------------------------------------- #


class LFRicIntMaxAXKern(LFRicMaxAXKern):
    ''' Returns the maximum of an integer scalar and integer-valued
    field elements. The result is stored as another, integer-valued,
    field: `Y = max(a, X)`.
    Inherits the `lower_to_language_level` method from the real-valued
    built-in equivalent `LFRicMaxAXKern`.
    '''
    _case_name = "int_max_aX"
    _datatype = "integer"


class LFRicIntIncMaxAXKern(LFRicIncMaxAXKern):
    ''' Returns the maximum of an integer scalar and integer-valued
    field elements. The result is stored in the same, integer-valued,
    field: `X = max(a, X)`.
    Inherits the `lower_to_language_level` method from the real-valued
    built-in equivalent `LFRicIncMaxAXKern`.
    '''
    _case_name = "int_inc_max_aX"
    _datatype = "integer"


# ------------------------------------------------------------------- #
# ======== Minimum of (integer scalar, integer field elements) ====== #
# ------------------------------------------------------------------- #


class LFRicIntMinAXKern(LFRicMinAXKern):
    ''' Returns the minimum of an integer scalar and integer-valued
    field elements. The result is stored as another, integer-valued,
    field: `Y = min(a, X)`.
    Inherits the `lower_to_language_level` method from the real-valued
    built-in equivalent `LFRicMinAXKern`.

    '''
    _case_name = "int_min_aX"
    _datatype = "integer"


class LFRicIntIncMinAXKern(LFRicIncMinAXKern):
    ''' Returns the minimum of an integer scalar and integer-valued
    field elements. The result is stored in the same, integer-valued,
    field: `X = min(a, X)`.
    Inherits the `lower_to_language_level` method from the real-valued
    built-in equivalent `LFRicIncMinAXKern`.

    '''
    _case_name = "int_inc_min_aX"
    _datatype = "integer"


# ------------------------------------------------------------------- #
# ============== Converting integer to real field elements ========== #
# ------------------------------------------------------------------- #

class LFRicIntToRealXKern(LFRicBuiltIn):
    ''' Converts integer-valued field elements to real-valued
    field elements using the Fortran intrinsic `REAL` function,
    `Y = REAL(X, kind=r_<prec>)`. Here `Y` is a real-valued
    field of precision `r_<prec>` and `X` is the integer-valued
    field being converted.

    '''
    _datatype = "real"
    _case_name = "int_to_real_X"

    @classmethod
    def metadata(cls):
        '''Returns the kernel metadata describing this built-in.

        :returns: kernel metadata describing this built-in.
        :rtype: :py:class:`psyclone.domain.lfric.kernel.LFRicKernelMetadata`

        '''
        return cls._builtin_metadata([
            FieldArgMetadata("gh_real", "gh_write", "any_space_1"),
            FieldArgMetadata("gh_integer", "gh_read", "any_space_1")])

    def __str__(self):
        return (f"Built-in: {self._case_name} (convert an integer-valued "
                f"to a real-valued field)")

    def lower_to_language_level(self):
        '''
        Lowers this LFRic-specific built-in kernel to language-level PSyIR.
        This BuiltIn node is replaced by an Assignment node.

        :returns: the lowered version of this node.
        :rtype: :py:class:`psyclone.psyir.node.Node`

        '''
        # Get indexed references for each of the field (proxy) arguments.
        arg_refs = self.get_indexed_field_argument_references()

        # Create the PSyIR for the kernel:
        # proxy0%data(df) = REAL(proxy1%data, kind=r_<prec>)
        r_precision = arg_refs[0].datatype.precision
        rhs = IntrinsicCall.create(
            IntrinsicCall.Intrinsic.REAL,
            [arg_refs[1], ("kind", Reference(r_precision))])
        assign = Assignment.create(arg_refs[0], rhs)
        # Finally, replace this kernel node with the Assignment
        self.replace_with(assign)
        return assign


# The built-in operations that we support for this API. The meta-data
# describing these kernels is in lfric_builtins_mod.f90. This dictionary
# can only be defined after all of the necessary 'class' statements have
# been executed (happens when this module is imported into another).
# Built-ins for real-valued fields
REAL_BUILTIN_MAP_CAPITALISED = {
    # Adding (scaled) real fields
    "X_plus_Y": LFRicXPlusYKern,
    "inc_X_plus_Y": LFRicIncXPlusYKern,
    "a_plus_X": LFRicAPlusXKern,
    "inc_a_plus_X": LFRicIncAPlusXKern,
    "aX_plus_Y": LFRicAXPlusYKern,
    "inc_aX_plus_Y": LFRicIncAXPlusYKern,
    "inc_X_plus_bY": LFRicIncXPlusBYKern,
    "aX_plus_bY": LFRicAXPlusBYKern,
    "inc_aX_plus_bY": LFRicIncAXPlusBYKern,
    "aX_plus_aY": LFRicAXPlusAYKern,
    # Subtracting (scaled) real fields
    "X_minus_Y": LFRicXMinusYKern,
    "inc_X_minus_Y": LFRicIncXMinusYKern,
    "a_minus_X": LFRicAMinusXKern,
    "inc_a_minus_X": LFRicIncAMinusXKern,
    "X_minus_a": LFRicXMinusAKern,
    "inc_X_minus_a": LFRicIncXMinusAKern,
    "aX_minus_Y": LFRicAXMinusYKern,
    "X_minus_bY": LFRicXMinusBYKern,
    "inc_X_minus_bY": LFRicIncXMinusBYKern,
    "aX_minus_bY": LFRicAXMinusBYKern,
    # Multiplying (scaled) real fields
    "X_times_Y": LFRicXTimesYKern,
    "inc_X_times_Y": LFRicIncXTimesYKern,
    "inc_aX_times_Y": LFRicIncAXTimesYKern,
    # Multiplying real fields by a real scalar (scaling fields)
    "a_times_X": LFRicATimesXKern,
    "inc_a_times_X": LFRicIncATimesXKern,
    # Dividing real fields
    "X_divideby_Y": LFRicXDividebyYKern,
    "inc_X_divideby_Y": LFRicIncXDividebyYKern,
    "X_divideby_a": LFRicXDividebyAKern,
    "inc_X_divideby_a": LFRicIncXDividebyAKern,
    # Dividing a real scalar by elements of a real field
    # (inverse scaling of fields)
    "a_divideby_X": LFRicADividebyXKern,
    "inc_a_divideby_X": LFRicIncADividebyXKern,
    # Raising a real field to a scalar
    "inc_X_powreal_a": LFRicIncXPowrealAKern,
    "inc_X_powint_n": LFRicIncXPowintNKern,
    # Setting real field elements to scalar or other
    # real field's values
    "setval_c": LFRicSetvalCKern,
    "setval_X": LFRicSetvalXKern,
    "setval_random": LFRicSetvalRandomKern,
    # Inner product of real fields
    "X_innerproduct_Y": LFRicXInnerproductYKern,
    "X_innerproduct_X": LFRicXInnerproductXKern,
    # Sum values of a real field
    "sum_X": LFRicSumXKern,
    # Sign of real field elements applied to a scalar value
    "sign_X": LFRicSignXKern,
    # Maximum of a real scalar value and real field elements
    "max_aX": LFRicMaxAXKern,
    "inc_max_aX": LFRicIncMaxAXKern,
    # Minimum of a real scalar value and real field elements
    "min_aX": LFRicMinAXKern,
    "inc_min_aX": LFRicIncMinAXKern,
    # Converting real to integer field elements
    "real_to_int_X": LFRicRealToIntXKern,
    # Converting real to real field elements
    "real_to_real_X": LFRicRealToRealXKern}

# Built-ins for integer-valued fields
INT_BUILTIN_MAP_CAPITALISED = {
    # Adding integer fields
    "int_X_plus_Y": LFRicIntXPlusYKern,
    "int_inc_X_plus_Y": LFRicIntIncXPlusYKern,
    "int_a_plus_X": LFRicIntAPlusXKern,
    "int_inc_a_plus_X": LFRicIntIncAPlusXKern,
    # Subtracting integer fields
    "int_X_minus_Y": LFRicIntXMinusYKern,
    "int_inc_X_minus_Y": LFRicIntIncXMinusYKern,
    "int_a_minus_X": LFRicIntAMinusXKern,
    "int_inc_a_minus_X": LFRicIntIncAMinusXKern,
    "int_X_minus_a": LFRicIntXMinusAKern,
    "int_inc_X_minus_a": LFRicIntIncXMinusAKern,
    # Multiplying (scaled) real fields
    "int_X_times_Y": LFRicIntXTimesYKern,
    "int_inc_X_times_Y": LFRicIntIncXTimesYKern,
    # Multiplying integer fields by an integer scalar (scaling fields)
    "int_a_times_X": LFRicIntATimesXKern,
    "int_inc_a_times_X": LFRicIntIncATimesXKern,
    # Setting an integer field elements to an integer scalar
    # or other integer field's values
    "int_setval_c": LFRicIntSetvalCKern,
    "int_setval_X": LFRicIntSetvalXKern,
    # Sign of integer field elements applied to a scalar value
    "int_sign_X": LFRicIntSignXKern,
    # Maximum of an integer scalar value and integer field elements
    "int_max_aX": LFRicIntMaxAXKern,
    "int_inc_max_aX": LFRicIntIncMaxAXKern,
    # Minimum of an integer scalar value and integer field elements
    "int_min_aX": LFRicIntMinAXKern,
    "int_inc_min_aX": LFRicIntIncMinAXKern,
    # Converting integer to real field elements
    "int_to_real_X": LFRicIntToRealXKern}

# Built-in map dictionary for all built-ins
BUILTIN_MAP_CAPITALISED = REAL_BUILTIN_MAP_CAPITALISED
BUILTIN_MAP_CAPITALISED.update(INT_BUILTIN_MAP_CAPITALISED)

# Built-in map dictionary in lowercase keys for invoke generation and
# comparison purposes. This does not enforce case sensitivity to Fortran
# built-in names.
BUILTIN_MAP = get_lowercase_builtin_map(BUILTIN_MAP_CAPITALISED)


# For AutoAPI documentation generation.
__all__ = ['LFRicBuiltInCallFactory',
           'LFRicBuiltIn',
           'LFRicXPlusYKern',
           'LFRicIncXPlusYKern',
           'LFRicAPlusXKern',
           'LFRicIncAPlusXKern',
           'LFRicAXPlusYKern',
           'LFRicIncAXPlusYKern',
           'LFRicIncXPlusBYKern',
           'LFRicAXPlusBYKern',
           'LFRicIncAXPlusBYKern',
           'LFRicAXPlusAYKern',
           'LFRicXMinusYKern',
           'LFRicIncXMinusYKern',
           'LFRicAMinusXKern',
           'LFRicIncAMinusXKern',
           'LFRicXMinusAKern',
           'LFRicIncXMinusAKern',
           'LFRicAXMinusYKern',
           'LFRicXMinusBYKern',
           'LFRicIncXMinusBYKern',
           'LFRicAXMinusBYKern',
           'LFRicXTimesYKern',
           'LFRicIncXTimesYKern',
           'LFRicIncAXTimesYKern',
           'LFRicATimesXKern',
           'LFRicIncATimesXKern',
           'LFRicXDividebyYKern',
           'LFRicIncXDividebyYKern',
           'LFRicXDividebyAKern',
           'LFRicIncXDividebyAKern',
           'LFRicADividebyXKern',
           'LFRicIncADividebyXKern',
           'LFRicIncXPowrealAKern',
           'LFRicIncXPowintNKern',
           'LFRicSetvalCKern',
           'LFRicSetvalXKern',
           'LFRicSetvalRandomKern',
           'LFRicXInnerproductYKern',
           'LFRicXInnerproductXKern',
           'LFRicSumXKern',
           'LFRicSignXKern',
           'LFRicMaxAXKern',
           'LFRicIncMaxAXKern',
           'LFRicMinAXKern',
           'LFRicIncMinAXKern',
           'LFRicRealToIntXKern',
           'LFRicRealToRealXKern',
           'LFRicIntXPlusYKern',
           'LFRicIntIncXPlusYKern',
           'LFRicIntAPlusXKern',
           'LFRicIntIncAPlusXKern',
           'LFRicIntXMinusYKern',
           'LFRicIntIncXMinusYKern',
           'LFRicIntAMinusXKern',
           'LFRicIntIncAMinusXKern',
           'LFRicIntXMinusAKern',
           'LFRicIntIncXMinusAKern',
           'LFRicIntXTimesYKern',
           'LFRicIntIncXTimesYKern',
           'LFRicIntATimesXKern',
           'LFRicIntIncATimesXKern',
           'LFRicIntSetvalCKern',
           'LFRicIntSetvalXKern',
           'LFRicIntSignXKern',
           'LFRicIntMaxAXKern',
           'LFRicIntIncMaxAXKern',
           'LFRicIntMinAXKern',
           'LFRicIntIncMinAXKern',
           'LFRicIntToRealXKern']
