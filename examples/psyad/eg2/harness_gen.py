import sys

from psyclone.domain.lfric.algorithm.alg_gen import (
    create_alg_driver, create_invoke_call, construct_kernel_args)
from psyclone.dynamo0p3 import DynKern
from psyclone.errors import InternalError
from psyclone.line_length import FortLineLength
from psyclone.parse.kernel import get_kernel_parse_tree, KernelTypeFactory
from psyclone.psyir.backend.fortran import FortranWriter
from psyclone.psyir.nodes import (Assignment, Reference, ArrayReference,
                                  Call, Literal, BinaryOperation)
from psyclone.psyir.symbols import (INTEGER_TYPE, ArrayType, DataTypeSymbol,
                                    ImportInterface, ContainerSymbol,
                                    REAL_TYPE, ScalarType,
                                    DataSymbol, DeferredType, RoutineSymbol)


if __name__ == "__main__":

    prog = create_alg_driver("main", 10)
    table = prog.symbol_table

    kernel_name = sys.argv[1]
    kernel_path = sys.argv[2]

    # Parse the kernel metadata (this still uses fparser1 as that's what
    # the meta-data handling is currently based upon).
    parse_tree = get_kernel_parse_tree(kernel_path)

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
    #rand_kernel_mod = table.new_symbol("set_random_kernel_mod",
    #                                   symbol_type=ContainerSymbol)
    #rand_kernel_routine = table.new_symbol(
    #    "set_random_kernel_type", interface=ImportInterface(rand_kernel_mod))

    # Construct a DynKern using the metadata. This is used when constructing
    # the kernel argument list.
    ktype = KernelTypeFactory(api="dynamo0.3").create(parse_tree,
                                                      name=kernel_name)
    kern = DynKern()
    kern.load_meta(ktype)

    kern_args = construct_kernel_args(prog, kern)

    # Create symbols that will store copies of the inputs to the TL kernel.
    for arg in kern_args.arglist:
        sym = table.lookup(arg)
        input_sym = table.new_symbol(sym.name+"_input", symbol_type=DataSymbol,
                                     datatype=DeferredType())
        input_sym.copy_properties(sym)

    # Initialise argument values and keep copies. For scalars we use the
    # Fortran 'random_number' intrinsic directly.
    random_num = RoutineSymbol("random_number")
    for sym in kern_args.scalars:
        prog.addchild(Call.create(random_num, [Reference(sym)]))
        input_sym = table.lookup(sym.name+"_input")
        prog.addchild(Assignment.create(Reference(input_sym), Reference(sym)))

    # We use the set_random_kernel_type kernel to initialise all fields.
    kernel_list = []
    for sym, space in kern_args.fields:
        if isinstance(sym.datatype, DataTypeSymbol):
            kernel_list.append(("setval_random", [sym.name]))
            input_sym = table.lookup(sym.name+"_input")
            kernel_list.append(("setval_X", [input_sym.name, sym.name]))
        elif isinstance(sym.datatype, ArrayType):
            input_sym = table.lookup(sym.name+"_input")
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
    prog.addchild(Assignment.create(Reference(inner1_sym),
                                    Literal("0.0", rdef_type)))
    for sym in kern_args.scalars:
        prod = BinaryOperation.create(BinaryOperation.Operator.MUL,
                                      Reference(sym), Reference(sym))
        prog.addchild(
                Assignment.create(
                    Reference(inner1_sym),
                    BinaryOperation.create(BinaryOperation.Operator.ADD,
                                           Reference(inner1_sym), prod)))
    for sym in field_ip_symbols:
        if sym.is_scalar:
            prog.addchild(
                    Assignment.create(
                        Reference(inner1_sym),
                        BinaryOperation.create(BinaryOperation.Operator.ADD,
                                               Reference(inner1_sym),
                                               Reference(sym))))
        else:
            for dim in range(int(sym.datatype.shape[0].lower.value),
                             int(sym.datatype.shape[0].upper.value)+1):
                add_op = BinaryOperation.create(
                    BinaryOperation.Operator.ADD,
                    Reference(inner1_sym),
                    ArrayReference.create(sym,
                                          [Literal(str(dim), INTEGER_TYPE)]))
                prog.addchild(Assignment.create(Reference(inner1_sym), add_op))

    # Call the adjoint kernel and compute the inner product of its outputs
    # with the original inputs.

    writer = FortranWriter()
    gen_code = writer(prog)
    fll = FortLineLength()
    output = fll.process(gen_code)

    with open("main_alg.x90", "w", encoding="utf-8") as fmain:
        print(output, file=fmain)
