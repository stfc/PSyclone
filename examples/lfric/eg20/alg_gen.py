import sys

from psyclone.domain.lfric.algorithm.alg_gen import (
    create_alg_driver, create_invoke_call, construct_kernel_args)
from psyclone.dynamo0p3 import DynKern
from psyclone.errors import InternalError
from psyclone.line_length import FortLineLength
from psyclone.parse.kernel import get_kernel_parse_tree, KernelTypeFactory
from psyclone.psyir.backend.fortran import FortranWriter
from psyclone.psyir.nodes import (Assignment, Literal, Reference)
from psyclone.psyir.symbols import (INTEGER_TYPE, ArrayType, DataTypeSymbol,
                                    ImportInterface, ContainerSymbol)


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
    kernel_routine = table.new_symbol(kernel_name,
                                      interface=ImportInterface(kernel_mod))

    ktype = KernelTypeFactory(api="dynamo0.3").create(parse_tree,
                                                      name=kernel_name)
    # Construct a DynKern using the metadata. This is used when constructing
    # the kernel argument list.
    kern = DynKern()
    kern.load_meta(ktype)

    kern_args = construct_kernel_args(prog, kern)

    # Initialise argument values.
    for sym in kern_args.scalars:
        prog.addchild(Assignment.create(Reference(sym),
                                        Literal("0", INTEGER_TYPE)))

    # We use the setval_c builtin to initialise all fields to zero.
    kernel_list = []
    for sym, space in kern_args.fields:
        if isinstance(sym.datatype, DataTypeSymbol):
            kernel_list.append(("setval_c", [sym.name, "0.0_r_def"]))
        elif isinstance(sym.datatype, ArrayType):
            for dim in range(int(sym.datatype.shape[0].lower.value),
                             int(sym.datatype.shape[0].upper.value)+1):
                kernel_list.append(("setval_c", [f"{sym.name}({dim})",
                                                 "0.0_r_def"]))
        else:
            raise InternalError(
                f"Expected a field symbol to either be of ArrayType or have "
                f"a type specified by a DataTypeSymbol but found "
                f"{sym.datatype} for field '{sym.name}'")

    kernel_list.append((kernel_routine.name, kern_args.arglist))

    # Create the 'call invoke(...)' for the list of kernels.
    prog.addchild(create_invoke_call(kernel_list))

    writer = FortranWriter()
    gen_code = writer(prog)
    fll = FortLineLength()
    output = fll.process(gen_code)

    with open("main_alg.x90", "w", encoding="utf-8") as fmain:
        print(output, file=fmain)
