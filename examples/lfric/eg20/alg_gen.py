import sys

from fparser.two.parser import ParserFactory
from fparser.two import Fortran2003
from fparser.common.readfortran import FortranStringReader

from psyclone.domain.lfric import KernCallInvokeArgList
from psyclone.dynamo0p3 import DynKern
from psyclone.errors import InternalError
from psyclone.line_length import FortLineLength
from psyclone.parse.kernel import get_kernel_parse_tree, KernelTypeFactory
from psyclone.psyir.backend.fortran import FortranWriter
from psyclone.psyir.nodes import (CodeBlock, Assignment, Literal, Reference,
                                  Routine, Call)
from psyclone.psyir.symbols import (INTEGER_TYPE, ArrayType,
                                    DataSymbol, DataTypeSymbol, DeferredType,
                                    ImportInterface, ContainerSymbol,
                                    RoutineSymbol, UnknownFortranType)


def create_alg_driver(name, nlayers):
    '''
    Creates a standalone LFRic program with the necessary infrastructure
    set-up calls contained in a CodeBlock.

    :param str name: the name to give the created program.
    :param int nlayers: the number of vertical levels to give the model.

    :returns: an LFRic program.
    :rtype: :py:class:`psyclone.psyir.nodes.Routine`

    '''
    prog = Routine(name, is_program=True)
    table = prog.symbol_table

    # For simplicity we use a template algorithm layer taken from
    # examples/lfric/eg17.
    alg_code = f'''\
PROGRAM {name}
  USE some_mod
  IMPLICIT NONE
  global_mesh = global_mesh_base_type()
  global_mesh_ptr => global_mesh
  partitioner_ptr => partitioner_planar
  partition = partition_type(global_mesh_ptr, partitioner_ptr, 1, 1, 0, 0, 1)
  extrusion = uniform_extrusion_type(0.0_r_def, 100.0_r_def, {nlayers})
  extrusion_ptr => extrusion
  mesh = mesh_type(global_mesh_ptr, partition, extrusion_ptr)
  WRITE(*, *) "Mesh has", mesh % get_nlayers(), "layers."
END PROGRAM {name}
'''
    # Create a CodeBlock containing all the initialisation
    # that we can't represent in PSyIR (e.g. pointer assignments).
    reader = FortranStringReader(alg_code)
    ast = parser(reader)
    exe_parts = Fortran2003.walk(ast, Fortran2003.Execution_Part)
    init_block = CodeBlock(exe_parts[0].children,
                           CodeBlock.Structure.STATEMENT)
    prog.addchild(init_block)

    # Create ContainerSymbols for each of the modules that we will need.
    for mod in ["field_mod", "function_space_mod", "fs_continuity_mod",
                "global_mesh_base_mod", "mesh_mod", "partition_mod",
                "extrusion_mod", "constants_mod"]:
        table.new_symbol(mod, symbol_type=ContainerSymbol)

    table.new_symbol("field_type", symbol_type=DataTypeSymbol,
                     datatype=DeferredType(),
                     interface=ImportInterface(
                         table.lookup("field_mod")))

    table.new_symbol("function_space_type",
                     symbol_type=DataTypeSymbol,
                     datatype=DeferredType(),
                     interface=ImportInterface(
                         table.lookup("function_space_mod")))

    table.new_symbol("global_mesh_base_type",
                     symbol_type=DataTypeSymbol,
                     datatype=DeferredType(),
                     interface=ImportInterface(
                         table.lookup("global_mesh_base_mod")))

    table.new_symbol(
        "mesh_type", symbol_type=DataTypeSymbol,
        datatype=DeferredType(),
        interface=ImportInterface(table.lookup("mesh_mod")))
    table.new_symbol(
        "PLANE", symbol_type=DataSymbol,
        datatype=DeferredType(),
        interface=ImportInterface(table.lookup("mesh_mod")))
    table.new_symbol(
        "uniform_extrusion_type", symbol_type=DataTypeSymbol,
        datatype=DeferredType(),
        interface=ImportInterface(table.lookup("extrusion_mod")))
    part_type = table.new_symbol(
        "partition_type", symbol_type=DataTypeSymbol,
        datatype=DeferredType(),
        interface=ImportInterface(table.lookup("partition_mod")))
    table.new_symbol("partition", symbol_type=DataSymbol,
                     datatype=part_type)

    table.new_symbol("mesh", symbol_type=DataSymbol,
                     datatype=UnknownFortranType(
                         "TYPE(mesh_type), TARGET :: mesh"))
    table.new_symbol("global_mesh", symbol_type=DataSymbol,
                     datatype=UnknownFortranType(
                         "TYPE(global_mesh_base_type), TARGET :: global_mesh"))
    table.new_symbol("global_mesh_ptr", symbol_type=DataSymbol,
                     datatype=UnknownFortranType(
                         "CLASS(global_mesh_base_type), POINTER :: "
                         "global_mesh_ptr"))
    table.new_symbol("extrusion", symbol_type=DataSymbol,
                     datatype=UnknownFortranType(
                         "TYPE(uniform_extrusion_type), TARGET :: extrusion"))
    table.new_symbol("extrusion_ptr", symbol_type=DataSymbol,
                     datatype=UnknownFortranType(
                         "TYPE(uniform_extrusion_type), POINTER :: "
                         "extrusion_ptr"))

    table.new_symbol(
        "partitioner_planar", symbol_type=RoutineSymbol,
        interface=ImportInterface(table.lookup("partition_mod")))
    table.new_symbol(
        "partitioner_interface",
        interface=ImportInterface(table.lookup("partition_mod")))
    table.new_symbol(
        "partitioner_ptr", symbol_type=DataSymbol,
        datatype=UnknownFortranType("PROCEDURE(partitioner_interface), "
                                    "POINTER :: partitioner_ptr"))

    table.new_symbol("r_def", symbol_type=DataSymbol,
                     datatype=INTEGER_TYPE,
                     interface=ImportInterface(
                         table.lookup("constants_mod")))
    table.new_symbol("i_def", symbol_type=DataSymbol,
                     datatype=INTEGER_TYPE,
                     interface=ImportInterface(
                         table.lookup("constants_mod")))

    return prog


def create_invoke_call(call_list):
    '''
    Create the PSyIR for a `call invoke(...)`. Each argument is actually a
    Fortran structure constructor so we create CodeBlocks for them. Since
    'invoke' only exists in the DSL, we create a RoutineSymbol for it but
    never add it to a symbol table.

    :param call_list: list of kernel and argument names.
    :type call_list: list of (str, list of str)

    :returns: a Call describing this invoke.
    :rtype: :py:class:`psyclone.psyir.nodes.Call`

    '''
    invoke_args = []
    for call in call_list:
        reader = FortranStringReader(
            f"{call[0]}({','.join(call[1])})")
        ptree = Fortran2003.Structure_Constructor(reader)
        invoke_args.append(CodeBlock([ptree], CodeBlock.Structure.EXPRESSION))
    return Call.create(RoutineSymbol("invoke"), invoke_args)


if __name__ == "__main__":
    # Ensure the Fortran parser is initialised
    parser = ParserFactory().create(std="f2008")

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

    # Construct a list of which function spaces the field argument(s)
    # are on.
    field_spaces = []
    for arg in ktype.arg_descriptors:
        if arg.argument_type == "gh_field":
            if arg.function_space.lower() == "any_w2":
                # ANY_W2 means 'any W2 space' - it is not in and of itself
                # a valid function space so change it to W2.
                field_spaces.append("w2")
            else:
                field_spaces.append(arg.function_space)
    unique_function_spaces = set(field_spaces)

    elem_order = table.new_symbol("element_order", symbol_type=DataSymbol,
                                  datatype=INTEGER_TYPE,
                                  constant_value=Literal("1", INTEGER_TYPE))
    ndata_sz = table.new_symbol("ndata_sz", symbol_type=DataSymbol,
                                datatype=INTEGER_TYPE)
    prog.addchild(Assignment.create(Reference(ndata_sz),
                                    Literal("20", INTEGER_TYPE)))

    # Initialise the function spaces required by the kernel arguments.
    for space in unique_function_spaces:
        table.new_symbol(f"{space}", symbol_type=DataSymbol,
                         datatype=DeferredType(),
                         interface=ImportInterface(
                             table.lookup("fs_continuity_mod")))
        vsym = table.new_symbol(f"vector_space_{space}",
                                symbol_type=DataSymbol,
                                datatype=UnknownFortranType(
                                    f"TYPE(function_space_type), TARGET :: "
                                    f"vector_space_{space}"))
        table.new_symbol(f"vector_space_{space}_ptr", symbol_type=DataSymbol,
                         datatype=UnknownFortranType(
                             f"TYPE(function_space_type), POINTER :: "
                             f"vector_space_{space}_ptr"))
        ptree = Fortran2003.Assignment_Stmt(
            FortranStringReader(
                f"  vector_space_{space} = function_space_type("
                f"  mesh, element_order, {space}, ndata_sz)"))
        prog.addchild(CodeBlock([ptree], CodeBlock.Structure.STATEMENT))

        ptree = Fortran2003.Pointer_Assignment_Stmt(
            FortranStringReader(f"vector_space_{space}_ptr => "
                                f"vector_space_{space}\n"))
        prog.addchild(CodeBlock([ptree], CodeBlock.Structure.STATEMENT))

    # Construct the argument list and add suitable symbols to the table.
    kern_args = KernCallInvokeArgList(kern, table)
    kern_args.generate()

    # Initialise field datastructures using the symbols added to the table
    # when setting up the kernel arguments and the information on their
    # respective function spaces extracted from the kernel metadata.
    fld_idx = 0
    for sym in kern_args.fields:
        if isinstance(sym.datatype, DataTypeSymbol):
            # Single field argument.
            ptree = Fortran2003.Call_Stmt(
                f"CALL {sym.name} % initialise(vector_space = "
                f"vector_space_{field_spaces[fld_idx]}_ptr, name = "
                f"'{sym.name}')")
            prog.addchild(CodeBlock([ptree], CodeBlock.Structure.STATEMENT))

        elif isinstance(sym.datatype, ArrayType):
            # Field vector argument.
            for dim in range(int(sym.datatype.shape[0].lower.value),
                             int(sym.datatype.shape[0].upper.value)+1):
                ptree = Fortran2003.Call_Stmt(
                    f"CALL {sym.name}({dim}) % initialise(vector_space = "
                    f"vector_space_{field_spaces[fld_idx]}_ptr, name = "
                    f"'{sym.name}')")
                prog.addchild(CodeBlock([ptree],
                                        CodeBlock.Structure.STATEMENT))
        else:
            raise InternalError(
                f"Expected a field symbol to either be of ArrayType or have "
                f"a type specified by a DataTypeSymbol but found "
                f"{sym.datatype} for field '{sym.name}'")
        fld_idx += 1

    # Initialise any quadrature objects
    for qr_sym, shape in kern_args.quadrature_objects:

        if shape == "gh_quadrature_xyoz":
            # TODO 'element_order' and 'quadrature_rule' names shouldn't
            # be hardwired
            reader = FortranStringReader(
                "quadrature_xyoz_type(element_order+3, quadrature_rule)")
            ptree = Fortran2003.Structure_Constructor(reader)
            prog.addchild(Assignment.create(
                Reference(qr_sym),
                CodeBlock([ptree], CodeBlock.Structure.EXPRESSION)))

        else:
            # Quadrature rule on lateral faces only
            # qrf = quadrature_face_type(nqp_exact, .true., .false., &
            #                            reference_element, quadrature_rule)
            raise NotImplementedError(f"Initialisation for quadrature of type "
                                      f"'{shape}' is not yet implemented.")

    # Initialise argument values.
    for sym in kern_args.scalars:
        prog.addchild(Assignment.create(Reference(sym),
                                        Literal("0", INTEGER_TYPE)))
    # We use the setval_c builtin to initialise all fields to zero.
    kernel_list = []
    for sym in kern_args.fields:
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

    kernel_list.append((kernel_routine.name,
                        [name for name in kern_args.arglist]))

    # Create the 'call invoke(...)' for the list of kernels.
    prog.addchild(create_invoke_call(kernel_list))

    writer = FortranWriter()
    gen_code = writer(prog)
    fll = FortLineLength()
    output = fll.process(gen_code)

    with open("main_alg.x90", "w", encoding="utf-8") as fmain:
        print(output, file=fmain)
