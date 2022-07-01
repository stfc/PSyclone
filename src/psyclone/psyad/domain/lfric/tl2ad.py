# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2022, Science and Technology Facilities Council.
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

''' Provides LFRic-specific PSyclone adjoint functionality. '''

from fparser import api as fpapi
from psyclone.domain.lfric.algorithm import (
    LFRicAlgorithmInvokeCall, LFRicBuiltinFunctor, LFRicKernelFunctor)
from psyclone.domain.lfric.algorithm.alg_gen import (
    _create_alg_mod, construct_kernel_args,
    initialise_field, initialise_operator)
from psyclone.dynamo0p3 import DynKern
from psyclone.errors import InternalError
from psyclone.parse.kernel import KernelTypeFactory
from psyclone.psyad.domain.common.adjoint_utils import create_adjoint_name
from psyclone.psyir.nodes import (Call, Reference, ArrayReference, Assignment,
                                  Literal, BinaryOperation, Routine)
from psyclone.psyir.symbols import (ImportInterface, ContainerSymbol,
                                    ScalarType, ArrayType, RoutineSymbol,
                                    DataTypeSymbol,
                                    DataSymbol, INTEGER_TYPE, DeferredType)


def _compute_lfric_inner_products(prog, scalars, field_sums, sum_sym):
    '''
    :param prog: the Routine to which to add PSyIR.
    :type prog:
    :param scalars: the scalars to include in the sum.
    :type scalars:
    :param field_sums: the results of all of the inner products of the \
                       various field arguments.
    :type field_sums:
    :param sum_sym: the symbol into which to accumulate the final sum.
    :type sum_sym:

    '''
    prog.addchild(Assignment.create(Reference(sum_sym),
                                    Literal("0.0", sum_sym.datatype)))
    for scalar in scalars:
        prod = BinaryOperation.create(BinaryOperation.Operator.MUL,
                                      Reference(scalar[0]),
                                      Reference(scalar[1]))
        prog.addchild(
                Assignment.create(
                    Reference(sum_sym),
                    BinaryOperation.create(BinaryOperation.Operator.ADD,
                                           Reference(sum_sym), prod)))
    for sym in field_sums:
        if sym.is_scalar:
            prog.addchild(
                    Assignment.create(
                        Reference(sum_sym),
                        BinaryOperation.create(BinaryOperation.Operator.ADD,
                                               Reference(sum_sym),
                                               Reference(sym))))
        else:
            for dim in range(int(sym.datatype.shape[0].lower.value),
                             int(sym.datatype.shape[0].upper.value)+1):
                add_op = BinaryOperation.create(
                    BinaryOperation.Operator.ADD,
                    Reference(sum_sym),
                    ArrayReference.create(sym,
                                          [Literal(str(dim), INTEGER_TYPE)]))
                prog.addchild(Assignment.create(Reference(sum_sym), add_op))


def generate_lfric_adjoint_test(tl_source):
    '''
    :param str tl_source: the Fortran source of an LFRic module defining a \
                          tangent-linear kernel.

    :returns: PSyIR of an Algorithm that tests the adjoint of the supplied \
              LFRic TL kernel.
    :rtype: :py:class:`psyclone.psyir.nodes.Container`

    '''
    container = _create_alg_mod("main")
    routine = container.walk(Routine)[0]
    table = routine.symbol_table

    # Parse the kernel metadata (this still uses fparser1 as that's what
    # the meta-data handling is currently based upon).
    parse_tree = fpapi.parse(tl_source)

    # Get the name of the module that contains the kernel and create a
    # ContainerSymbol for it.
    kernel_mod_name = parse_tree.content[0].name
    kernel_mod = table.new_symbol(kernel_mod_name, symbol_type=ContainerSymbol)
    # Assume the LFRic naming convention is followed in order to infer the name
    # of the TL kernel.
    kernel_name = kernel_mod_name.replace("_mod", "_type")

    adj_mod = table.new_symbol(create_adjoint_name(kernel_mod_name),
                               symbol_type=ContainerSymbol)
    kernel_routine = table.new_symbol(kernel_name,
                                      symbol_type=DataTypeSymbol,
                                      datatype=DeferredType(),
                                      interface=ImportInterface(kernel_mod))
    adj_routine = table.new_symbol(create_adjoint_name(kernel_name),
                                   symbol_type=DataTypeSymbol,
                                   datatype=DeferredType(),
                                   interface=ImportInterface(adj_mod))

    # Construct a DynKern using the metadata. This is used when constructing
    # the kernel argument list.
    ktype = KernelTypeFactory(api="dynamo0.3").create(parse_tree,
                                                      name=kernel_name)
    kern = DynKern()
    kern.load_meta(ktype)

    kern_args = construct_kernel_args(routine, kern)

    # Create symbols that will store copies of the inputs to the TL kernel.
    input_symbols = {}
    for arg in kern_args.arglist:
        sym = table.lookup(arg)
        input_sym = table.new_symbol(sym.name+"_input", symbol_type=DataSymbol,
                                     datatype=DeferredType())
        input_sym.copy_properties(sym)
        input_symbols[sym.name] = input_sym

    # Create symbols for the various Builtins that we will use.
    # TODO these symbols are not added to the SymbolTable because they only
    # exist as part of the DSL.
    setval_x = DataTypeSymbol("setval_x", DeferredType())
    setval_rand = DataTypeSymbol("setval_random", DeferredType())
    setop_x = DataTypeSymbol("setop_x", DeferredType())
    setop_rand = DataTypeSymbol("setop_random", DeferredType())
    x_innerprod_x = DataTypeSymbol("x_innerproduct_x", DeferredType())
    x_innerprod_y = DataTypeSymbol("x_innerproduct_y", DeferredType())

    # Initialise argument values and keep copies. For scalars we use the
    # Fortran 'random_number' intrinsic directly.
    # TODO this is Fortran specific.
    random_num = RoutineSymbol("random_number")
    for sym in kern_args.scalars:
        routine.addchild(Call.create(random_num, [Reference(sym)]))
        input_sym = table.lookup(sym.name+"_input")
        routine.addchild(Assignment.create(Reference(input_sym),
                                           Reference(sym)))

    # We use the setval_random builtin to initialise all fields.
    kernel_list = []
    for sym, space in kern_args.fields:
        input_sym = input_symbols[sym.name]
        if isinstance(sym.datatype, DataTypeSymbol):
            initialise_field(routine, input_sym, space)
            kernel_list.append(LFRicBuiltinFunctor.create(setval_rand,
                                                          [Reference(sym)]))
            kernel_list.append(LFRicBuiltinFunctor.create(
                setval_x, [Reference(input_sym), Reference(sym)]))
        elif isinstance(sym.datatype, ArrayType):
            initialise_field(routine, input_sym, space)
            for dim in range(int(sym.datatype.shape[0].lower.value),
                             int(sym.datatype.shape[0].upper.value)+1):
                lit = Literal(str(dim), INTEGER_TYPE)
                kernel_list.append(
                    LFRicBuiltinFunctor.create(setval_rand,
                                               [ArrayReference.create(sym,
                                                                      [lit])]))
                kernel_list.append(
                    LFRicBuiltinFunctor.create(
                        setval_x, [ArrayReference.create(input_sym,
                                                         [lit.copy()]),
                                   ArrayReference.create(sym, [lit.copy()])]))
        else:
            raise InternalError(
                f"Expected a field symbol to either be of ArrayType or have "
                f"a type specified by a DataTypeSymbol but found "
                f"{sym.datatype} for field '{sym.name}'")

    for sym, to_space, from_space in kern_args.operators:
        input_sym = input_symbols[sym.name]
        # Initialise the operator that will keep a copy of the input values.
        initialise_operator(routine, input_sym, to_space, from_space)
        kernel_list.append(LFRicBuiltinFunctor.create(setop_rand,
                                                      [Reference(sym)]))
        kernel_list.append(LFRicBuiltinFunctor.create(
            setop_x, [Reference(input_sym), Reference(sym)]))

    # Finally, add the kernel itself to the list for the invoke().
    arg_nodes = []
    for arg in kern_args.arglist:
        arg_nodes.append(Reference(table.lookup(arg)))
    kern = LFRicKernelFunctor.create(kernel_routine, arg_nodes)
    kernel_list.append(kern)

    rdef_type = ScalarType(ScalarType.Intrinsic.REAL,
                           table.lookup("r_def"))

    # Compute the inner products of the results of the TL kernel.
    field_ip_symbols = []
    for sym, space in kern_args.fields:
        inner_prod_name = sym.name+"_inner_prod"
        if isinstance(sym.datatype, DataTypeSymbol):
            ip_sym = table.new_symbol(inner_prod_name, symbol_type=DataSymbol,
                                      datatype=rdef_type)
            routine.addchild(Assignment.create(Reference(ip_sym),
                                               Literal("0.0", rdef_type)))
            kernel_list.append(
                LFRicBuiltinFunctor.create(x_innerprod_x, [Reference(ip_sym),
                                                           Reference(sym)]))
        elif isinstance(sym.datatype, ArrayType):
            dtype = ArrayType(rdef_type, sym.datatype.shape)
            ip_sym = table.new_symbol(inner_prod_name, symbol_type=DataSymbol,
                                      datatype=dtype)
            for dim in range(int(sym.datatype.shape[0].lower.value),
                             int(sym.datatype.shape[0].upper.value)+1):
                lit = Literal(str(dim), INTEGER_TYPE)
                routine.addchild(Assignment.create(
                    ArrayReference.create(ip_sym,
                                          [lit]), Literal("0.0", rdef_type)))
                kernel_list.append(
                    LFRicBuiltinFunctor.create(
                        x_innerprod_x,
                        [ArrayReference.create(ip_sym, [lit.copy()]),
                         ArrayReference.create(sym, [lit.copy()])]))
        else:
            raise InternalError(
                f"Expected a field symbol to either be of ArrayType or have "
                f"a type specified by a DataTypeSymbol but found "
                f"{sym.datatype} for field '{sym.name}'")
        field_ip_symbols.append(ip_sym)

    # Create the 'call invoke(...)' for the list of kernels.
    invoke_sym = table.new_symbol("invoke", symbol_type=RoutineSymbol)
    routine.addchild(LFRicAlgorithmInvokeCall.create(invoke_sym,
                                                     kernel_list, 0))

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
    kernel_list = [adj_kern]
    # Compute the inner product of its outputs
    # with the original inputs.
    field_ip_symbols = []
    for sym, space in kern_args.fields:
        inner_prod_name = sym.name+"_inner_prod"
        ip_sym = table.lookup(inner_prod_name)
        input_sym = input_symbols[sym.name]
        if isinstance(sym.datatype, DataTypeSymbol):
            routine.addchild(Assignment.create(Reference(ip_sym),
                                               Literal("0.0", rdef_type)))
            kernel_list.append(
                LFRicBuiltinFunctor.create(
                    x_innerprod_y, [Reference(ip_sym), Reference(sym),
                                    Reference(input_sym)]))
        elif isinstance(sym.datatype, ArrayType):
            dtype = ArrayType(rdef_type, sym.datatype.shape)

            for dim in range(int(sym.datatype.shape[0].lower.value),
                             int(sym.datatype.shape[0].upper.value)+1):
                lit = Literal(str(dim), INTEGER_TYPE)
                routine.addchild(Assignment.create(
                    ArrayReference.create(ip_sym, [lit]),
                    Literal("0.0", rdef_type)))
                kernel_list.append(
                    LFRicBuiltinFunctor.create(
                        x_innerprod_y, [ArrayReference.create(ip_sym,
                                                              [lit.copy()]),
                                        ArrayReference.create(sym,
                                                              [lit.copy()]),
                                        ArrayReference.create(input_sym,
                                                              [lit.copy()])]))
        else:
            raise InternalError(
                f"Expected a field symbol to either be of ArrayType or have "
                f"a type specified by a DataTypeSymbol but found "
                f"{sym.datatype} for field '{sym.name}'")
        field_ip_symbols.append(ip_sym)

    # Create the 'call invoke(...)' for the list of kernels.
    routine.addchild(LFRicAlgorithmInvokeCall.create(
        invoke_sym, kernel_list, 1))

    # Sum up the second set of inner products
    inner2_sym = table.new_symbol("inner2", symbol_type=DataSymbol,
                                  datatype=rdef_type)
    scalars = zip(kern_args.scalars,
                  [input_symbols[sym.name] for sym in kern_args.scalars])
    _compute_lfric_inner_products(routine, scalars, field_ip_symbols,
                                  inner2_sym)

    # Finally, compare the two inner products.
    # pylint: disable=import-outside-toplevel
    from psyclone.psyad.tl2ad import create_real_comparison
    stmts = create_real_comparison(table, kern, inner1_sym, inner2_sym)
    for stmt in stmts:
        routine.addchild(stmt)

    return container
