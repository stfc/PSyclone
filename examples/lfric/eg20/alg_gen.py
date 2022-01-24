import sys

from fparser.two.parser import ParserFactory
from fparser.two import Fortran2003
from fparser.common.readfortran import FortranStringReader

from psyclone.domain.lfric import KernCallInvokeArgList, LFRicConstants
from psyclone.dynamo0p3 import DynKern
from psyclone.errors import InternalError
from psyclone.line_length import FortLineLength
from psyclone.parse.kernel import get_kernel_parse_tree, KernelTypeFactory
from psyclone.psyir.backend.fortran import FortranWriter
from psyclone.psyir.nodes import (CodeBlock, Assignment, Literal, Reference,
                                  Routine)
from psyclone.psyir.symbols import (INTEGER_TYPE, ArrayType,
                                    DataSymbol, DataTypeSymbol, DeferredType,
                                    ImportInterface, ContainerSymbol,
                                    RoutineSymbol, UnknownFortranType)

if __name__ == "__main__":
    # Ensure the Fortran parser is initialised
    parser = ParserFactory().create(std="f2008")

    prog = Routine("main", is_program=True)
    table = prog.symbol_table

    # Now we want to create a suitable algorithm layer that invokes
    # this kernel. For simplicity we use a template algorithm
    # layer taken from examples/lfric/eg17.
    alg_code = '''\
PROGRAM main
  USE global_mesh_base_mod, ONLY: global_mesh_base_type
  USE mesh_mod, ONLY: mesh_type, PLANE
  USE partition_mod, ONLY: partition_type, partitioner_planar, &
      partitioner_interface
  USE extrusion_mod, ONLY: uniform_extrusion_type
  USE function_space_mod, ONLY: function_space_type
  USE fs_continuity_mod, ONLY: W0, W1, W2, W2V, W2H, W3
  IMPLICIT NONE
  TYPE(global_mesh_base_type), TARGET :: global_mesh
  CLASS(global_mesh_base_type), POINTER :: global_mesh_ptr
  TYPE(partition_type) :: partition
  TYPE(mesh_type), TARGET :: mesh
  TYPE(uniform_extrusion_type), TARGET :: extrusion
  TYPE(uniform_extrusion_type), POINTER :: extrusion_ptr
  PROCEDURE(partitioner_interface), POINTER :: partitioner_ptr
  global_mesh = global_mesh_base_type()
  global_mesh_ptr => global_mesh
  partitioner_ptr => partitioner_planar
  partition = partition_type(global_mesh_ptr, partitioner_ptr, 1, 1, 0, 0, 1)
  extrusion = uniform_extrusion_type(0.0_r_def, 100.0_r_def, 5)
  extrusion_ptr => extrusion
  mesh = mesh_type(global_mesh_ptr, partition, extrusion_ptr)
  WRITE(*, *) "Mesh has", mesh % get_nlayers(), "layers."
END PROGRAM main
'''
    # To save time, we create a CodeBlock containing all the initialisation
    # that we can't represent in PSyIR (e.g. pointer assignments).
    reader = FortranStringReader(alg_code)
    ast = parser(reader)
    exe_parts = Fortran2003.walk(ast, Fortran2003.Execution_Part)
    init_block = CodeBlock(exe_parts[0].children,
                           CodeBlock.Structure.STATEMENT)

    kernel_name = sys.argv[1]
    kernel_path = sys.argv[2]

    # Parse the kernel metadata (this still uses fparser1 as that's what
    # the meta-data handling is currently based upon).
    parse_tree = get_kernel_parse_tree(kernel_path)
    # Get the name of the module that contains the kernel
    kernel_mod_name = parse_tree.content[0].name

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

    # Create ContainerSymbols for each of the modules that we will need.
    for mod in ["field_mod", "function_space_mod", "fs_continuity_mod",
                "global_mesh_base_mod", "mesh_mod", "partition_mod",
                "extrusion_mod", "constants_mod", kernel_mod_name]:
        table.new_symbol(mod, symbol_type=ContainerSymbol)

    field_type = table.new_symbol("field_type", symbol_type=DataTypeSymbol,
                                  datatype=DeferredType(),
                                  interface=ImportInterface(
                                      table.lookup("field_mod")))
    kernel_routine = table.new_symbol(kernel_name,
                                      interface=ImportInterface(
                                          table.lookup(kernel_mod_name)))
    fs_type = table.new_symbol("function_space_type",
                               symbol_type=DataTypeSymbol,
                               datatype=DeferredType(),
                               interface=ImportInterface(
                                   table.lookup("function_space_mod")))

    glob_mesh_base_type = table.new_symbol(
        "global_mesh_base_type",
        symbol_type=DataTypeSymbol,
        datatype=DeferredType(),
        interface=ImportInterface(table.lookup("global_mesh_base_mod")))

    mesh_type = table.new_symbol(
        "mesh_type", symbol_type=DataTypeSymbol,
        datatype=DeferredType(),
        interface=ImportInterface(table.lookup("mesh_mod")))
    plane = table.new_symbol(
        "PLANE", symbol_type=DataSymbol,
        datatype=DeferredType(),
        interface=ImportInterface(table.lookup("mesh_mod")))
    uni_extrusion_type = table.new_symbol(
        "uniform_extrusion_type", symbol_type=DataTypeSymbol,
        datatype=DeferredType(),
        interface=ImportInterface(table.lookup("extrusion_mod")))
    partition_type = table.new_symbol(
        "partition_type", symbol_type=DataTypeSymbol,
        datatype=DeferredType(),
        interface=ImportInterface(table.lookup("partition_mod")))
    partition = table.new_symbol("partition", symbol_type=DataSymbol,
                                 datatype=partition_type)

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

    partitioner_planar = table.new_symbol(
        "partitioner_planar", symbol_type=RoutineSymbol,
        interface=ImportInterface(table.lookup("partition_mod")))
    partitioner_interface = table.new_symbol(
        "partitioner_interface",
        interface=ImportInterface(table.lookup("partition_mod")))
    partitioner_ptr = table.new_symbol(
        "partitioner_ptr", symbol_type=DataSymbol,
        datatype=UnknownFortranType("PROCEDURE(partitioner_interface), "
                                    "POINTER :: partitioner_ptr"))

    r_def = table.new_symbol("r_def", symbol_type=DataSymbol,
                             datatype=INTEGER_TYPE,
                             interface=ImportInterface(
                                 table.lookup("constants_mod")))
    i_def = table.new_symbol("i_def", symbol_type=DataSymbol,
                             datatype=INTEGER_TYPE,
                             interface=ImportInterface(
                                 table.lookup("constants_mod")))

    elem_order = table.new_symbol("element_order", symbol_type=DataSymbol,
                                  datatype=INTEGER_TYPE,
                                  constant_value=Literal("1", INTEGER_TYPE))
    ndata_sz = table.new_symbol("ndata_sz", symbol_type=DataSymbol,
                                datatype=INTEGER_TYPE)
    prog.addchild(Assignment.create(Reference(ndata_sz),
                                    Literal("20", INTEGER_TYPE)))
    prog.addchild(init_block)

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

        elif shape == "blah":
            # Quadrature rule on lateral faces only
            #qrf = quadrature_face_type(nqp_exact, .true., .false., &
            #                       reference_element, quadrature_rule)
            pass

    # Initialise argument values.
    for sym in kern_args.scalars:
        prog.addchild(Assignment.create(Reference(sym),
                                        Literal("0", INTEGER_TYPE)))
    # We use the setval_c builtin to initialise all fields to zero.
    setval_list = []
    for sym in kern_args.fields:
        if isinstance(sym.datatype, DataTypeSymbol):
            setval_list.append(f"setval_c({sym.name}, 0.0_r_def)")
        elif isinstance(sym.datatype, ArrayType):
            for dim in range(int(sym.datatype.shape[0].lower.value),
                             int(sym.datatype.shape[0].upper.value)+1):
                setval_list.append(f"setval_c({sym.name}({dim}), 0.0_r_def)")
        else:
            raise InternalError(
                f"Expected a field symbol to either be of ArrayType or have "
                f"a type specified by a DataTypeSymbol but found "
                f"{sym.datatype} for field '{sym.name}'")

    kernel_arg_list = ','.join(name for name in kern_args.arglist)
    # Getting fparser to parse the 'invoke' is difficult so put it in its
    # own program.
    reader = FortranStringReader(f'''
PROGRAM main
  use testkern_mod
  call invoke({",".join(setval_list)}, &
              {kernel_routine.name}({kernel_arg_list}))
END PROGRAM main
''')
    ast = parser(reader)
    calls = Fortran2003.walk(ast, types=Fortran2003.Call_Stmt)
    prog.addchild(CodeBlock([calls[0]], CodeBlock.Structure.STATEMENT))

    writer = FortranWriter()
    gen_code = writer(prog)
    fll = FortLineLength()
    output = fll.process(gen_code)

    with open("main_alg.x90", "w", encoding="utf-8") as fmain:
        print(output, file=fmain)
