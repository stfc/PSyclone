# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2022-2024, Science and Technology Facilities Council.
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
# Modified by J. Henrichs, Bureau of Meteorology
# Modified by L. Turner, Met Office

''' Provides LFRic-specific PSyclone adjoint test-harness functionality. '''

from fparser import api as fpapi

from psyclone.core import AccessType
from psyclone.domain.lfric import (
    LFRicConstants, LFRicTypes, ArgIndexToMetadataIndex)
from psyclone.domain.lfric.algorithm.lfric_alg import LFRicAlg
from psyclone.domain.lfric.algorithm.psyir import (
    LFRicAlgorithmInvokeCall, LFRicBuiltinFunctorFactory, LFRicKernelFunctor)
from psyclone.domain.lfric.transformations import RaisePSyIR2LFRicKernTrans
from psyclone.errors import InternalError, GenerationError
from psyclone.psyad.domain.common.adjoint_utils import (
    create_adjoint_name, find_container, common_real_comparison)
from psyclone.psyir.frontend.fortran import FortranReader
from psyclone.psyir.nodes import (
    IntrinsicCall, Reference, ArrayReference, Assignment,
    Literal, BinaryOperation, Routine, IfBlock)
from psyclone.psyir.symbols import (
    ImportInterface, ContainerSymbol, ScalarType, ArrayType, RoutineSymbol,
    DataTypeSymbol, DataSymbol, UnresolvedType)

#: The tolerance applied to the comparison of the inner product values in
#: the generated test-harness code.
#: TODO #1346 this tolerance should be user configurable.
INNER_PRODUCT_TOLERANCE = 1500.0


def _compute_lfric_inner_products(prog, scalars, field_sums, sum_sym):
    '''
    Adds PSyIR to a supplied Routine to compute the sum of the inner products
    of the supplied scalars and fields.

    :param prog: the Routine to which to add PSyIR.
    :type prog: :py:class:`psyclone.psyir.nodes.Routine`
    :param scalars: list of pairs of scalars to multiply and sum.
    :type scalars: List[Tuple[:py:class:`psyclone.psyir.symbols.DataSymbol`,
                              :py:class:`psyclone.psyir.symbols.DataSymbol`]]
    :param field_sums: the results of all of the inner products of the \
                       various field arguments.
    :type field_sums: List[:py:class:`psyclone.psyir.symbols.DataSymbol`]
    :param sum_sym: the symbol into which to accumulate the final sum.
    :type sum_sym: :py:class:`psyclone.psyir.symbols.DataSymbol`

    '''
    table = prog.symbol_table
    idef_sym = table.add_lfric_precision_symbol("i_def")
    idef_type = ScalarType(ScalarType.Intrinsic.REAL, idef_sym)

    # Initialise the sum to zero: sum = 0.0
    prog.addchild(Assignment.create(Reference(sum_sym),
                                    Literal("0.0", sum_sym.datatype)))
    for scalar in scalars:
        # Compute the product of the pair of scalars: scalar[0]*scalar[1]
        # (unless they are boolean).
        if scalar[0].datatype.intrinsic == ScalarType.Intrinsic.BOOLEAN:
            continue
        prod = BinaryOperation.create(BinaryOperation.Operator.MUL,
                                      Reference(scalar[0]),
                                      Reference(scalar[1]))
        # Add this product to the sum:
        #     sum = sum + scalar[0]*scalar[1]
        prog.addchild(
                Assignment.create(
                    Reference(sum_sym),
                    BinaryOperation.create(BinaryOperation.Operator.ADD,
                                           Reference(sum_sym), prod)))
    for sym in field_sums:
        if sym.is_scalar:
            # Add this result of a field inner product to the sum:
            #     sum = sum + field_sum
            prog.addchild(
                    Assignment.create(
                        Reference(sum_sym),
                        BinaryOperation.create(BinaryOperation.Operator.ADD,
                                               Reference(sum_sym),
                                               Reference(sym))))
        else:
            # For a field vector we have an array of inner-product values that
            # must be summed.
            for dim in range(int(sym.datatype.shape[0].lower.value),
                             int(sym.datatype.shape[0].upper.value)+1):
                # sum = sum + field_sum(dim)
                add_op = BinaryOperation.create(
                    BinaryOperation.Operator.ADD,
                    Reference(sum_sym),
                    ArrayReference.create(sym,
                                          [Literal(str(dim), idef_type)]))
                prog.addchild(Assignment.create(Reference(sum_sym), add_op))


def _compute_field_inner_products(routine, field_pairs):
    '''
    Constructs the assignments and kernel functors needed to compute the
    inner products of the supplied list of fields.

    :param routine: the routine to which to add the assignments.
    :type routine: :py:class:`psyclone.psyir.nodes.Routine`
    :param field_pairs: the pairs of fields for which we need to compute the \
                        inner products.
    :type field_pairs: List[ \
               Tuple[:py:class:`psyclone.psyir.symbols.DataSymbol`, \
                     :py:class:`psyclone.psyir.symbols.DataSymbol`]]

    :returns: the symbols containing the inner products and the list of \
              kernel functors that computes them.
    :rtype: Tuple[List[:py:class:`psyclone.psyir.symbols.DataSymbol`], \
              List[:py:class:`psyclone.domain.lfric.algorithm.KernelFunctor`]]

    :raises TypeError: if any of the supplied symbols are not DataSymbols.
    :raises InternalError: if any of the supplied pairs of symbols do not \
                           have matching types.
    :raises InternalError: if any of the supplied symbols are not of the \
                           correct type for an LFRic field.

    '''
    # pylint: disable=too-many-branches, too-many-locals
    table = routine.symbol_table
    rdef_sym = table.add_lfric_precision_symbol("r_def")
    rdef_type = ScalarType(ScalarType.Intrinsic.REAL, rdef_sym)
    idef_sym = table.add_lfric_precision_symbol("i_def")
    idef_type = ScalarType(ScalarType.Intrinsic.REAL, idef_sym)

    builtin_factory = LFRicBuiltinFunctorFactory.get()

    field_ip_symbols = []
    kernel_list = []
    for sym1, sym2 in field_pairs:

        if not (isinstance(sym1, DataSymbol) and isinstance(sym2, DataSymbol)):
            raise TypeError(
                f"Each pair of fields/field-vectors must be supplied as "
                f"DataSymbols but got: {type(sym1)}, {type(sym2)}")

        if sym1.datatype != sym2.datatype:
            raise InternalError(
                f"Cannot compute the inner product of fields '{sym1.name}' "
                f"and '{sym2.name}' because they are of different types: "
                f"{sym1.datatype} and {sym2.datatype}")

        if sym1 is sym2:
            inner_prod_name = f"{sym1.name}_inner_prod"
        else:
            inner_prod_name = f"{sym1.name}_{sym2.name}_inner_prod"

        if isinstance(sym1.datatype, DataTypeSymbol):
            # Create and initialise a variable to hold the result of the
            # inner product of this pair of fields.
            ip_sym = table.new_symbol(inner_prod_name,
                                      symbol_type=DataSymbol,
                                      datatype=rdef_type)
            # name_inner_prod = 0.0
            routine.addchild(Assignment.create(Reference(ip_sym),
                                               Literal("0.0", rdef_type)))
            if sym2 is sym1:
                # Inner product of field with itself.
                kernel_list.append(
                    builtin_factory.create("x_innerproduct_x", table,
                                           [Reference(ip_sym),
                                            Reference(sym1)]))
            else:
                # Inner product of two different fields.
                kernel_list.append(
                    builtin_factory.create(
                        "x_innerproduct_y", table,
                        [Reference(ip_sym), Reference(sym1), Reference(sym2)]))

        elif isinstance(sym1.datatype, ArrayType):
            # This is a pair of field vectors. We compute the inner product of
            # each component and store the results in an array of the same
            # length as the field vector.

            # Create the array in which the results will be stored.
            dtype = ArrayType(rdef_type, sym1.datatype.shape)
            ip_sym = table.new_symbol(inner_prod_name,
                                      symbol_type=DataSymbol,
                                      datatype=dtype)
            for dim in range(int(sym1.datatype.shape[0].lower.value),
                             int(sym1.datatype.shape[0].upper.value)+1):
                lit = Literal(str(dim), idef_type)
                # Zero the inner product for this component pair.
                routine.addchild(Assignment.create(
                    ArrayReference.create(ip_sym,
                                          [lit]), Literal("0.0", rdef_type)))
                if sym2 is sym1:
                    # Inner product of field with itself.
                    kernel_list.append(
                        builtin_factory.create(
                            "x_innerproduct_x", table,
                            [ArrayReference.create(ip_sym, [lit.copy()]),
                             ArrayReference.create(sym1, [lit.copy()])]))
                else:
                    # Inner product of two different fields.
                    kernel_list.append(
                        builtin_factory.create(
                            "x_innerproduct_y", table,
                            [ArrayReference.create(ip_sym, [lit.copy()]),
                             ArrayReference.create(sym1, [lit.copy()]),
                             ArrayReference.create(sym2, [lit.copy()])]))
        else:
            raise InternalError(
                f"Expected a field symbol to either be of ArrayType or have "
                f"a type specified by a DataTypeSymbol but found "
                f"{sym1.datatype} for field '{sym1.name}'")

        # Store the list of symbols holding the various inner products.
        field_ip_symbols.append(ip_sym)

    return field_ip_symbols, kernel_list


def _init_fields_random(fields, input_symbols, table):
    '''
    Creates a suitable kernel functor for each field that requires
    initialising with pseudo-random data.

    :param fields: those fields requiring initialisation.
    :type fields: List[:py:class:`psyclone.psyir.symbols.DataSymbol`]
    :param input_symbols: map from field name to corresponding field copy.
    :type input_symbols: Dict[str, \
        :py:class:`psyclone.psyir.symbols.DataTypeSymbol` | \
        :py:class:`psyclone.psyir.symbols.ArrayType`]
    :param table: the symbol table to which to add new symbols.
    :type table: :py:class:`psyclone.psyir.symbols.SymbolTable`

    :returns: the required kernel calls.
    :rtype: List[:py:class:`psyclone.domain.common.algorithm.Functor`]

    '''
    idef_sym = table.add_lfric_precision_symbol("i_def")
    idef_type = ScalarType(ScalarType.Intrinsic.REAL, idef_sym)
    # We use the setval_random builtin to initialise all fields.
    kernel_list = []
    builtin_factory = LFRicBuiltinFunctorFactory.get()
    for sym in fields:
        input_sym = input_symbols[sym.name]
        if isinstance(sym.datatype, DataTypeSymbol):
            # Initialise the field with pseudo-random numbers.
            kernel_list.append(
                builtin_factory.create("setval_random", table,
                                       [Reference(sym)]))
            # Keep a copy of the values in the associated 'input' field.
            kernel_list.append(builtin_factory.create(
                "setval_x", table, [Reference(input_sym), Reference(sym)]))
        elif isinstance(sym.datatype, ArrayType):
            # Initialise each member of the field vector with pseudo-random
            # numbers.
            for dim in range(int(sym.datatype.shape[0].lower.value),
                             int(sym.datatype.shape[0].upper.value)+1):
                lit = Literal(str(dim), idef_type)
                # Initialise this component with pseudo-random numbers.
                kernel_list.append(
                    builtin_factory.create("setval_random", table,
                                           [ArrayReference.create(sym,
                                                                  [lit])]))
                # Keep a copy of the values in the associated 'input' field.
                kernel_list.append(
                    builtin_factory.create(
                        "setval_x", table,
                        [ArrayReference.create(input_sym, [lit.copy()]),
                         ArrayReference.create(sym, [lit.copy()])]))
        else:
            raise InternalError(
                f"Expected a field symbol to either be of ArrayType or have "
                f"a type specified by a DataTypeSymbol but found "
                f"{sym.datatype} for field '{sym.name}'")

    # Return the list of kernel functors.
    return kernel_list


def _init_operators_random(operators, table):
    '''
    Creates a suitable kernel functor for each operator that requires
    initialising with pseudo-random data.

    :param fields: those operators requiring initialisation.
    :type fields: List[:py:class:`psyclone.psyir.symbols.DataSymbol`]
    :param table: the symbol table to which to add new symbols.
    :type table: :py:class:`psyclone.psyir.symbols.SymbolTable`

    :returns: the required kernel calls.
    :rtype: List[:py:class:`psyclone.domain.common.algorithm.Functor`]

    '''
    # We use the 'setop_random' kernel to do the initialisation. This
    # is a general-purpose kernel (not a built-in) that is part of the
    # PSyclone distribution.
    kname = "setop_random_kernel"
    kernel_mod = table.new_symbol(kname+"_mod",
                                  symbol_type=ContainerSymbol)
    kernel_sym = table.new_symbol(kname+"_type", symbol_type=DataTypeSymbol,
                                  datatype=UnresolvedType(),
                                  interface=ImportInterface(kernel_mod))
    # Create a functor to initialise each operator
    kernel_list = [LFRicKernelFunctor.create(kernel_sym, [Reference(sym)])
                   for sym in operators]

    # Return the list of kernel functors.
    return kernel_list


def _init_scalar_value(scalar_arg, routine, input_symbols):
    '''
    Extends the supplied Routine with the necessary statements to initialise
    the supplied scalar argument. If that argument appears in the
    `input_symbols` dict then its initial value is also assigned to the Symbol
    in the dict entry.

    :param scalar_arg: the scalar kernel argument to initialise.
    :type scalar_arg: :py:class:`psyclone.psyir.symbols.DataSymbol`
    :param routine: the routine to which to add assignments.
    :type routine: :py:class:`psyclone.psyir.nodes.Routine`
    :param input_symbols: dict containing those kernel arguments for which we \
                          need to keep copies of their input values.
    :type input_symbols: Dict[str, \
                              :py:class:`psyclone.psyir.symbols.DataSymbol`]

    :raises InternalError: if the type of the scalar argument is not supported.

    '''
    if scalar_arg.datatype.intrinsic == ScalarType.Intrinsic.REAL:
        # TODO #2087 - we use the RANDOM_NUMBER intrinsic but make no attempt
        # to scale the resulting value.
        routine.addchild(
            IntrinsicCall.create(IntrinsicCall.Intrinsic.RANDOM_NUMBER,
                                 [Reference(scalar_arg)]))
    elif scalar_arg.datatype.intrinsic == ScalarType.Intrinsic.BOOLEAN:
        # TODO #2087 - just set the variable to False for the moment.
        routine.addchild(Assignment.create(
            Reference(scalar_arg),
            Literal("false", LFRicTypes("LFRicLogicalScalarDataType")())))
    elif scalar_arg.datatype.intrinsic == ScalarType.Intrinsic.INTEGER:
        # TODO #2087 - just set the variable to 1 for the moment.
        routine.addchild(Assignment.create(
            Reference(scalar_arg),
            Literal("1", LFRicTypes("LFRicIntegerScalarDataType")())))
    else:
        raise InternalError(
            f"_init_scalar_value: only scalars of REAL, INTEGER or BOOLEAN "
            f"type are supported but got symbol '{scalar_arg.name}' of type "
            f"'{scalar_arg.datatype}'.")

    if scalar_arg.name in input_symbols:
        # We need to keep a copy of the input value of this argument.
        input_sym = input_symbols[scalar_arg.name]
        routine.addchild(Assignment.create(Reference(input_sym),
                                           Reference(scalar_arg)))


def _validate_geom_arg(kern, arg_idx, name, valid_spaces, vec_len):
    '''
    Check that the argument at the supplied index is consistent with the
    properties of the field that it is supposed to represent.

    :param kern: the kernel under consideration.
    :type kern: :py:class:`psyclone.domain.lfric.LFRicKern`
    :param in arg_idx: the 1-indexed position of the argument in the list \
                       defined in the kernel metadata.
    :param str name: the name of the argument that we are expecting.
    :param List[str] valid_spaces: the function spaces that the argument is \
                                   permitted to be on.
    :param int vec_len: the expected vector length of the argument.

    :raises ValueError: if any of the properties of the specified kernel \
                        argument are inconsistent with the supplied values.
    '''
    num_metadata_args = len(kern.arg_descriptors)
    if arg_idx < 1 or arg_idx > num_metadata_args:
        raise ValueError(
            f"The supplied LFRic TL kernel '{kern.name}' has "
            f"{num_metadata_args} arguments specified in its metadata. "
            f"Therefore, the index of the argument containing the "
            f"'{name}' field must be between 1 and {num_metadata_args} "
            f"(inclusive) but got {arg_idx}.")
    # Check that the specified argument is of the correct type.
    descriptor = kern.arg_descriptors[arg_idx-1]
    if descriptor.argument_type != 'gh_field':
        raise ValueError(
            f"The '{name}' argument is expected to be a field but argument "
            f"{arg_idx} to kernel '{kern.name}' is a "
            f"'{descriptor.argument_type}'")
    if descriptor.function_space not in valid_spaces:
        raise ValueError(
            f"The '{name}' field argument to kernel '{kern.name}' is expected "
            f"to be on one of the {valid_spaces} spaces but the argument at "
            f"the specified position ({arg_idx}) is on the "
            f"'{descriptor.function_space}' space.")
    if descriptor.vector_size != vec_len:
        if vec_len > 1:
            raise ValueError(
                f"The '{name}' field argument to kernel '{kern.name}' is "
                f"expected to be a field vector of length {vec_len} but the "
                f"argument at the specified position ({arg_idx}) has a length "
                f"of {descriptor.vector_size}.")
        raise ValueError(
            f"The '{name}' field argument to kernel '{kern.name}' is expected "
            f"to be a field but the argument at the specified position "
            f"({arg_idx}) is a field vector of length "
            f"{descriptor.vector_size}.")


def _lfric_create_real_comparison(sym_table, kernel, var1, var2):
    '''Creates PSyIR that checks the values held by Symbols var1 and var2
    for equality, allowing for machine precision and writes the
    success or failure of the checks to the LFRic-specific logging
    API.  The generic version that writes to stdout can be found in
    psyad/domain/common/adjoint_utils.py

    :param sym_table: the SymbolTable in which to put new Symbols.
    :type sym_table: :py:class:`psyclone.psyir.symbols.SymbolTable`
    :param kernel: the routine for which this adjoint test is being performed.
    :type kernel: :py:class:`psyclone.psyir.nodes.Routine`
    :param var1: the symbol holding the first value for the comparison.
    :type var1: :py:class:`psyclone.psyir.symbols.DataSymbol`
    :param var2: the symbol holding the second value for the comparison.
    :type var2: :py:class:`psyclone.psyir.symbols.DataSymbol`

    :returns: the PSyIR nodes that perform the check and write its
        success or failure to the LFRic-specific logging API.
    :rtype: List[:py:class:`psyclone.psyir.nodes.Node`]

    '''
    statements = []
    statements.extend(common_real_comparison(sym_table, var1, var2))
    statements.extend(_lfric_log_write(sym_table, kernel, var1, var2))
    return statements


def _lfric_log_write(sym_table, kernel, var1, var2):
    '''Creates PSyIR that writes whether the precision test passed or
    failed to the LFRic-specific logging API.

    :param sym_table: the SymbolTable in which to read existing Symbols.
    :type sym_table: :py:class:`psyclone.psyir.symbols.SymbolTable`
    :param kernel: the routine for which this adjoint test is being performed.
    :type kernel: :py:class:`psyclone.psyir.nodes.Routine`
    :param var1: the symbol holding the first value for the comparison.
    :type var1: :py:class:`psyclone.psyir.symbols.DataSymbol`
    :param var2: the symbol holding the second value for the comparison.
    :type var2: :py:class:`psyclone.psyir.symbols.DataSymbol`

    :returns: PSyIR nodes that write out test success or failure to
        the LFRic logging API.
    :rtype: List[:py:class:`psyclone.psyir.nodes.Node`]

    '''
    freader = FortranReader()
    statements = []
    rel_diff = sym_table.lookup_with_tag("relative_diff")
    overall_tol = sym_table.lookup_with_tag("overall_tolerance")
    log_mod = sym_table.find_or_create("log_mod", symbol_type=ContainerSymbol)
    log_scratch = sym_table.find_or_create(
        "log_scratch_space", symbol_type=DataSymbol, datatype=UnresolvedType(),
        interface=ImportInterface(log_mod))
    log_level_info = sym_table.new_symbol(
        "log_level_info", symbol_type=DataSymbol, datatype=UnresolvedType(),
        interface=ImportInterface(log_mod))
    log_level_error = sym_table.new_symbol(
        "log_level_error", symbol_type=DataSymbol, datatype=UnresolvedType(),
        interface=ImportInterface(log_mod))
    log_event = sym_table.new_symbol(
        "log_event", symbol_type=RoutineSymbol,
        interface=ImportInterface(log_mod))
    write_pass = freader.psyir_from_statement(
        f"WRITE({log_scratch.name}, *) \"PASSED {kernel.name}:\", "
        f"{var1.name}, {var2.name}, {rel_diff.name}", sym_table)
    write_fail = freader.psyir_from_statement(
        f"WRITE({log_scratch.name}, *) \"FAILED {kernel.name}:\", "
        f"{var1.name}, {var2.name}, {rel_diff.name}", sym_table)
    log_pass = freader.psyir_from_statement(
        f"call {log_event.name}( {log_scratch.name}, {log_level_info.name} )",
        sym_table)
    log_fail = freader.psyir_from_statement(
        f"call {log_event.name}( {log_scratch.name}, {log_level_error.name} )",
        sym_table)

    statements.append(
        IfBlock.create(BinaryOperation.create(BinaryOperation.Operator.LT,
                                              Reference(rel_diff),
                                              Reference(overall_tol)),
                       [write_pass, log_pass], [write_fail, log_fail]))

    return statements


def generate_lfric_adjoint_harness(tl_psyir, coord_arg_idx=None,
                                   panel_id_arg_idx=None):
    '''
    Constructs and returns the PSyIR for a Container and Routine that
    implements a test harness for the adjoint of the supplied tangent-linear
    kernel.

    :param tl_psyir: the PSyIR of an LFRic module defining a \
                     tangent-linear kernel.
    :type tl_psyir: :py:class:`psyclone.psyir.nodes.Container`
    :param Optional[int] coord_arg_idx: 1-indexed position of the coordinate \
        field in the list of arguments in the kernel metadata (if present).
    :param Optional[int] panel_id_arg_idx: 1-indexed position of the panel-id \
        field in the list of arguments in the kernel metadata (if present).

    :returns: PSyIR of an Algorithm that tests the adjoint of the supplied \
              LFRic TL kernel.
    :rtype: :py:class:`psyclone.psyir.nodes.Container`

    :raises ValueError: if the supplied PSyIR does not have a Container (that \
        is *not* a FileContainer).
    :raises ValueError: if the name of the Container (module) in the supplied \
        PSyIR does not follow the LFRic naming convention of ending in '_mod'.

    '''
    # pylint: disable=too-many-statements, too-many-locals, too-many-branches
    tl_container = find_container(tl_psyir)
    if not tl_container:
        raise ValueError(
            f"Test harness code can only be generated if the supplied TL "
            f"kernel is within a module (Container) but the supplied PSyIR "
            f"does not have a Container node:\n{tl_psyir.view(colour=False)}")

    lfalg = LFRicAlg()
    container = lfalg.create_alg_routine("adjoint_test")
    routine = container.walk(Routine)[0]
    table = routine.symbol_table

    tl_subroutine = tl_container.children[0]
    tl_subroutine_table = tl_subroutine.symbol_table
    tl_argument_list = tl_subroutine_table.argument_list

    # Parse the kernel metadata. This still uses fparser1 as that's what
    # the meta-data handling is currently based upon. We therefore have to
    # convert back from PSyIR to Fortran for the moment.
    # TODO #1806 - replace this with the new PSyIR-based metadata handling.
    # pylint: disable=import-outside-toplevel
    from psyclone.psyir.backend.fortran import FortranWriter
    writer = FortranWriter()
    tl_source = writer(tl_container)
    parse_tree = fpapi.parse(tl_source)

    # Get the name of the module that contains the kernel and create a
    # ContainerSymbol for it.
    kernel_mod_name = tl_container.name.lower()
    if not kernel_mod_name.endswith("_mod"):
        raise ValueError(
            f"The supplied LFRic TL kernel is contained within a module named "
            f"'{kernel_mod_name}'. This does not end in '_mod' and as such "
            f"does not comply with the LFRic naming convention.")

    kernel_mod = table.new_symbol(kernel_mod_name, symbol_type=ContainerSymbol)
    # Assume the LFRic naming convention is followed in order to infer the name
    # of the TL kernel. (If this convention isn't followed in the supplied code
    # then the call to `kernel_from_metadata` below will raise an appropriate
    # exception.)
    kernel_name = kernel_mod_name.replace("_mod", "_type")

    adj_mod = table.new_symbol(create_adjoint_name(kernel_mod_name),
                               symbol_type=ContainerSymbol)
    kernel_routine = table.new_symbol(kernel_name,
                                      symbol_type=DataTypeSymbol,
                                      datatype=UnresolvedType(),
                                      interface=ImportInterface(kernel_mod))
    adj_routine = table.new_symbol(create_adjoint_name(kernel_name),
                                   symbol_type=DataTypeSymbol,
                                   datatype=UnresolvedType(),
                                   interface=ImportInterface(adj_mod))

    # Construct a LFRicKern using the metadata and then use it to construct
    # the kernel argument list.
    # TODO #1806 - once we have the new PSyIR-based metadata handling then
    # we can pass PSyIR to this routine rather than an fparser1 parse tree.
    kern = lfalg.kernel_from_metadata(parse_tree, kernel_name)

    # Replace generic names for fields. operators etc generated in
    # LFRicKern with the scientific names used by the tangent-linear
    # kernel. This makes the harness code more readable. Changing the
    # names in-place within LFRicKern is the neatest solution given that
    # this is a legacy structure.

    # First raise the tangent-linear kernel PSyIR to LFRic PSyIR. This
    # gives us access to the kernel metadata.
    kern_trans = RaisePSyIR2LFRicKernTrans()
    kern_trans.apply(tl_psyir, options={"metadata_name": kernel_name})
    metadata = tl_psyir.children[0].metadata
    # Use the metadata to determine the mapping from a metadata
    # meta_arg index to the kernel argument index. Note, the meta_arg
    # index corresponds to the order of the arguments stored in
    # LFRicKern.
    index_map = ArgIndexToMetadataIndex.mapping(metadata)
    inv_index_map = {value: key for key, value in index_map.items()}

    # For each kernel argument, replace the generic name with the
    # scientific name used in the tangent-linear code.
    for idx, arg in enumerate(kern.arguments.args):
        tl_arg_idx = inv_index_map[idx]
        # pylint: disable=protected-access
        arg._name = tl_argument_list[tl_arg_idx].name
        # pylint: enable=protected-access

    kern_args = lfalg.construct_kernel_args(routine, kern)

    # Validate the index values for the coordinate and face_id fields if
    # supplied.
    geometry_arg_indices = []

    #: Properties of the various 'geometry' arguments that an LFRic kernel can
    #: accept. Used to validate that the arguments specified by the user have
    #: the expected properties.
    _geometry_arg_mapping = {
        "coordinate": {
            "valid_spaces": ["wchi"],
            "vector_len": 3
        },
        "panel-id": {
            "valid_spaces": LFRicConstants().VALID_DISCONTINUOUS_NAMES,
            "vector_len": 1
        }
    }

    if coord_arg_idx is not None:
        _validate_geom_arg(kern, coord_arg_idx, "coordinate",
                           _geometry_arg_mapping["coordinate"]["valid_spaces"],
                           _geometry_arg_mapping["coordinate"]["vector_len"])
        # Convert to 0-indexed
        coord_arg_idx -= 1
        geometry_arg_indices.append(coord_arg_idx)
    if panel_id_arg_idx is not None:
        # Check that the specified argument is of the correct type.
        _validate_geom_arg(kern, panel_id_arg_idx, "panel-id",
                           _geometry_arg_mapping["panel-id"]["valid_spaces"],
                           _geometry_arg_mapping["panel-id"]["vector_len"])
        # Convert to 0-indexed
        panel_id_arg_idx -= 1
        geometry_arg_indices.append(panel_id_arg_idx)

    # Create symbols that will store copies of the inputs to the TL kernel.
    # We don't need to do this for operators since they are never 'active'.
    field_args = [fsym for fsym, _ in kern_args.fields]
    scalar_and_field_args = kern_args.scalars + field_args
    # Double check that there aren't any operator arguments that are written to
    for op_sym, _, _ in kern_args.operators:
        idx = kern_args.arglist.index(op_sym.name)
        if kern.arguments.args[idx].access != AccessType.READ:
            raise GenerationError(
                f"Operator argument '{op_sym.name}' to TL kernel "
                f"'{kernel_name}' is written to. This is not supported.")

    input_symbols = {}
    for sym in scalar_and_field_args:
        idx = kern_args.arglist.index(sym.name)
        if (kern_args.metadata_index_from_actual_index(idx) in
                geometry_arg_indices):
            # This kernel argument is not modified by the test harness so we
            # don't need to keep a copy of it.
            continue
        input_sym = table.new_symbol(
            f"{sym.name}_input", symbol_type=DataSymbol,
            datatype=UnresolvedType())
        input_sym.copy_properties(sym)
        input_symbols[sym.name] = input_sym

    # Initialise all input field objects unless they contain geometric
    # information (as specified by the user via command-line arguments).
    kernel_input_arg_list = []
    for sym, space in kern_args.fields:
        idx = kern_args.arglist.index(sym.name)
        if (kern_args.metadata_index_from_actual_index(idx) in
                geometry_arg_indices):
            continue
        # This kernel argument is not one that is passed through from the
        # Algorithm layer.
        lfalg.initialise_field(routine, input_symbols[sym.name], space)
        kernel_input_arg_list.append(sym)

    # Initialise argument values and keep copies.

    # Scalars.
    for sym in kern_args.scalars:
        idx = kern_args.arglist.index(sym.name)
        if (kern_args.metadata_index_from_actual_index(idx) in
                geometry_arg_indices):
            # This kernel argument is not modified by the test harness
            # because it contains geometry information.
            continue
        _init_scalar_value(sym, routine, input_symbols)

    # Fields.
    kernel_list = _init_fields_random(kernel_input_arg_list, input_symbols,
                                      table)

    # Operators.
    kernel_list.extend(_init_operators_random(
        [sym for sym, _, _ in kern_args.operators], table))

    # Finally, add the kernel itself to the list for the invoke().
    arg_nodes = []
    for idx, arg in enumerate(kern_args.arglist):
        # Check whether this argument contains geometric information that
        # is passed through from the Algorithm layer.
        mdata_idx = kern_args.metadata_index_from_actual_index(idx)
        if mdata_idx is not None and mdata_idx == coord_arg_idx:
            # Replace the argument with that containing the
            # coordinate field.
            sym = table.lookup_with_tag("coord_field")
        elif mdata_idx is not None and mdata_idx == panel_id_arg_idx:
            # Replace the argument with that containing the panel-id field.
            sym = table.lookup_with_tag("panel_id_field")
        else:
            # This argument isn't special so use the existing symbol.
            sym = table.lookup(arg)
        # Create the reference to the selected symbol and add it to the list
        # of arguments for the kernel functor.
        arg_nodes.append(Reference(sym))
    kern = LFRicKernelFunctor.create(kernel_routine, arg_nodes)
    kernel_list.append(kern)

    # Compute the inner products of the results of the TL kernel. We exclude
    # any fields passed through (unmodified) from the Algorithm layer as well
    # as any operators.
    fld_pairs = []
    for sym, _ in kern_args.fields:
        if sym in kernel_input_arg_list:
            fld_pairs.append((sym, sym))
    field_ip_symbols, ip_kernels = _compute_field_inner_products(routine,
                                                                 fld_pairs)
    kernel_list.extend(ip_kernels)

    # Create the 'call invoke(...)' for the list of kernels.
    invoke_sym = table.new_symbol("invoke", symbol_type=RoutineSymbol)
    inv_call = LFRicAlgorithmInvokeCall.create(invoke_sym,
                                               kernel_list, 0)
    inv_call.preceding_comment = (
        "Initialise arguments and call the tangent-linear kernel.")
    routine.addchild(inv_call)

    rdef_sym = table.add_lfric_precision_symbol("r_def")
    rdef_type = ScalarType(ScalarType.Intrinsic.REAL, rdef_sym)

    # Compute the first inner products.
    inner1_sym = table.new_symbol("inner1", symbol_type=DataSymbol,
                                  datatype=rdef_type)

    scalars = zip(kern_args.scalars, kern_args.scalars)
    _compute_lfric_inner_products(routine, scalars, field_ip_symbols,
                                  inner1_sym)

    # Construct the functor for the adjoint kernel. We pass it the same
    # arguments as the TL kernel.
    adj_kern = LFRicKernelFunctor.create(adj_routine,
                                         [arg.copy() for arg in arg_nodes])
    # Compute the inner product of its outputs with the original inputs.
    fld_pairs = []
    for sym, _ in kern_args.fields:
        if sym in kernel_input_arg_list:
            fld_pairs.append((sym, input_symbols[sym.name]))
    field_ip_symbols, kernel_list = _compute_field_inner_products(routine,
                                                                  fld_pairs)
    kernel_list = [adj_kern] + kernel_list

    # Create the 'call invoke(...)' for the list of kernels.
    routine.addchild(LFRicAlgorithmInvokeCall.create(
        invoke_sym, kernel_list, 1))

    # Sum up the second set of inner products
    inner2_sym = table.new_symbol("inner2", symbol_type=DataSymbol,
                                  datatype=rdef_type)
    scalars = []
    for sym in kern_args.scalars:
        if sym.name in input_symbols:
            # This scalar is an input so include it in the inner product.
            scalars.append((sym, input_symbols[sym.name]))
    _compute_lfric_inner_products(routine, scalars, field_ip_symbols,
                                  inner2_sym)

    # Finally, compare the two inner products with an LFRic-specific routine.
    stmts = _lfric_create_real_comparison(table, kern, inner1_sym, inner2_sym)
    for stmt in stmts:
        routine.addchild(stmt)

    return container
