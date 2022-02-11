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
from psyclone.domain.lfric.algorithm.alg_gen import (
    create_alg_driver, create_invoke_call, construct_kernel_args,
    initialise_field)
from psyclone.dynamo0p3 import DynKern
from psyclone.errors import InternalError
from psyclone.parse.kernel import get_kernel_parse_tree, KernelTypeFactory
from psyclone.psyir.nodes import (Call, Reference, ArrayReference, Assignment,
                                  Literal, BinaryOperation)
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
    :param str kernel_name: name of the TL LFRic kernel.
    :param str kernel_path: location of the source file for the kernel.

    :returns: PSyIR of an Algorithm that tests the adjoint of the specified \
              LFRic TL kernel.
    :rtype: :py:class:`psyclone.psyir.nodes.Routine`

    '''
    prog = create_alg_driver("main", 10)
    table = prog.symbol_table

    # Parse the kernel metadata (this still uses fparser1 as that's what
    # the meta-data handling is currently based upon).
    parse_tree = fpapi.parse(tl_source)
    # TODO need some way of getting this.
    kernel_name = "tl_hydrostatic_kernel_type"

    # Get the name of the module that contains the kernel and create a
    # ContainerSymbol for it.
    kernel_mod_name = parse_tree.content[0].name
    kernel_mod = table.new_symbol(kernel_mod_name, symbol_type=ContainerSymbol)
    adj_mod = table.new_symbol(kernel_mod_name+"_adj",
                               symbol_type=ContainerSymbol)
    kernel_routine = table.new_symbol(kernel_name,
                                      interface=ImportInterface(kernel_mod))
    adj_routine = table.new_symbol(kernel_name+"_adj",
                                   interface=ImportInterface(adj_mod))

    # Construct a DynKern using the metadata. This is used when constructing
    # the kernel argument list.
    ktype = KernelTypeFactory(api="dynamo0.3").create(parse_tree,
                                                      name=kernel_name)
    kern = DynKern()
    kern.load_meta(ktype)

    kern_args = construct_kernel_args(prog, kern)

    # Create symbols that will store copies of the inputs to the TL kernel.
    input_symbols = {}
    for arg in kern_args.arglist:
        sym = table.lookup(arg)
        input_sym = table.new_symbol(sym.name+"_input", symbol_type=DataSymbol,
                                     datatype=DeferredType())
        input_sym.copy_properties(sym)
        input_symbols[sym] = input_sym

    # Initialise argument values and keep copies. For scalars we use the
    # Fortran 'random_number' intrinsic directly.
    random_num = RoutineSymbol("random_number")
    for sym in kern_args.scalars:
        prog.addchild(Call.create(random_num, [Reference(sym)]))
        input_sym = table.lookup(sym.name+"_input")
        prog.addchild(Assignment.create(Reference(input_sym), Reference(sym)))

    # We use the setval_random builtin to initialise all fields.
    kernel_list = []
    for sym, space in kern_args.fields:
        input_sym = input_symbols[sym]
        if isinstance(sym.datatype, DataTypeSymbol):
            initialise_field(prog, input_sym, space)
            kernel_list.append(("setval_random", [sym.name]))
            kernel_list.append(("setval_X", [input_sym.name, sym.name]))
        elif isinstance(sym.datatype, ArrayType):
            initialise_field(prog, input_sym, space)
            for dim in range(int(sym.datatype.shape[0].lower.value),
                             int(sym.datatype.shape[0].upper.value)+1):
                kernel_list.append(("setval_random",
                                    [f"{sym.name}({dim})"]))
                kernel_list.append(("setval_X", [f"{input_sym.name}({dim})",
                                                 f"{sym.name}({dim})"]))
        else:
            raise InternalError(
                f"Expected a field symbol to either be of ArrayType or have "
                f"a type specified by a DataTypeSymbol but found "
                f"{sym.datatype} for field '{sym.name}'")

    kernel_list.append((kernel_routine.name, kern_args.arglist))

    rdef_type = ScalarType(ScalarType.Intrinsic.REAL,
                           table.lookup("r_def"))

    # Compute the inner products of the results of the TL kernel.
    field_ip_symbols = []
    for sym, space in kern_args.fields:
        inner_prod_name = sym.name+"_inner_prod"
        if isinstance(sym.datatype, DataTypeSymbol):
            ip_sym = table.new_symbol(inner_prod_name, symbol_type=DataSymbol,
                                      datatype=rdef_type)
            prog.addchild(Assignment.create(Reference(ip_sym),
                                            Literal("0.0", rdef_type)))
            kernel_list.append(("X_innerproduct_X", [ip_sym.name, sym.name]))
        elif isinstance(sym.datatype, ArrayType):
            dtype = ArrayType(rdef_type, sym.datatype.shape)
            ip_sym = table.new_symbol(inner_prod_name, symbol_type=DataSymbol,
                                      datatype=dtype)
            for dim in range(int(sym.datatype.shape[0].lower.value),
                             int(sym.datatype.shape[0].upper.value)+1):
                prog.addchild(Assignment.create(
                    ArrayReference.create(ip_sym,
                                          [Literal(str(dim), INTEGER_TYPE)]),
                    Literal("0.0", rdef_type)))
                kernel_list.append(("X_innerproduct_X",
                                    [f"{ip_sym.name}({dim})",
                                     f"{sym.name}({dim})"]))
        else:
            raise InternalError(
                f"Expected a field symbol to either be of ArrayType or have "
                f"a type specified by a DataTypeSymbol but found "
                f"{sym.datatype} for field '{sym.name}'")
        field_ip_symbols.append(ip_sym)

    # Create the 'call invoke(...)' for the list of kernels.
    prog.addchild(create_invoke_call(kernel_list))

    # Compute the first inner products.
    inner1_sym = table.new_symbol("inner1", symbol_type=DataSymbol,
                                  datatype=rdef_type)

    scalars = zip(kern_args.scalars, kern_args.scalars)
    _compute_lfric_inner_products(prog, scalars, field_ip_symbols, inner1_sym)

    # Call the adjoint kernel.
    kernel_list = [(adj_routine.name, kern_args.arglist)]
    # Compute the inner product of its outputs
    # with the original inputs.
    field_ip_symbols = []
    for sym, space in kern_args.fields:
        inner_prod_name = sym.name+"_inner_prod"
        ip_sym = table.lookup(inner_prod_name)
        input_sym = input_symbols[sym]
        if isinstance(sym.datatype, DataTypeSymbol):
            prog.addchild(Assignment.create(Reference(ip_sym),
                                            Literal("0.0", rdef_type)))
            kernel_list.append(("X_innerproduct_Y",
                                [ip_sym.name, sym.name, input_sym.name]))
        elif isinstance(sym.datatype, ArrayType):
            dtype = ArrayType(rdef_type, sym.datatype.shape)

            for dim in range(int(sym.datatype.shape[0].lower.value),
                             int(sym.datatype.shape[0].upper.value)+1):
                prog.addchild(Assignment.create(
                    ArrayReference.create(ip_sym,
                                          [Literal(str(dim), INTEGER_TYPE)]),
                    Literal("0.0", rdef_type)))
                kernel_list.append(("X_innerproduct_Y",
                                    [f"{ip_sym.name}({dim})",
                                     f"{sym.name}({dim})",
                                     f"{input_sym.name}({dim})"]))
        else:
            raise InternalError(
                f"Expected a field symbol to either be of ArrayType or have "
                f"a type specified by a DataTypeSymbol but found "
                f"{sym.datatype} for field '{sym.name}'")
        field_ip_symbols.append(ip_sym)

    # Create the 'call invoke(...)' for the list of kernels.
    prog.addchild(create_invoke_call(kernel_list))

    # Sum up the second set of inner products
    inner2_sym = table.new_symbol("inner2", symbol_type=DataSymbol,
                                  datatype=rdef_type)
    scalars = zip(kern_args.scalars,
                  [input_symbols[sym] for sym in kern_args.scalars])
    _compute_lfric_inner_products(prog, scalars, field_ip_symbols, inner2_sym)

    # Finally, compare the two inner products.
    # pylint: disable=import-outside-toplevel
    from psyclone.psyad.tl2ad import create_real_comparison
    stmts = create_real_comparison(table, kern, inner1_sym, inner2_sym)
    for stmt in stmts:
        prog.addchild(stmt)

    return prog
