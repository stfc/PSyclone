import sys

from fparser.two.parser import ParserFactory
from fparser.two import Fortran2003
from fparser.common.readfortran import FortranStringReader

from psyclone.domain.lfric import KernCallInvokeArgList
from psyclone.dynamo0p3 import DynKern
from psyclone.parse.kernel import get_kernel_parse_tree, KernelTypeFactory
from psyclone.psyir.backend.fortran import FortranWriter
from psyclone.psyir.frontend.fortran import FortranReader
from psyclone.psyir.nodes import CodeBlock, Assignment, Literal, Reference, Routine
from psyclone.psyir.symbols import (INTEGER_TYPE, REAL_TYPE,
                                    DataSymbol, DataTypeSymbol, DeferredType,
                                    ImportInterface, ContainerSymbol,
                                    RoutineSymbol, UnknownFortranType)

if __name__ == "__main__":
    # Ensure the Fortran parser is initialised
    parser = ParserFactory().create(std="f2008")

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
    reader = FortranStringReader(alg_code)
    ast = parser(reader)
    exe_parts = Fortran2003.walk(ast, Fortran2003.Execution_Part)
    init_block = CodeBlock(exe_parts[0].children,
                           CodeBlock.Structure.STATEMENT)

    kernel_path = sys.argv[1]
    kernel_name = "testkern_type"
    kernel_mod_name = "testkern_mod"
    parse_tree = get_kernel_parse_tree(kernel_path)
    print(parse_tree)
    ktype = KernelTypeFactory(api="dynamo0.3").create(parse_tree,
                                                      name=kernel_name)
    # Construct a DynKern using the metadata.
    kern = DynKern()
    kern.load_meta(ktype)

    prog = Routine("main", is_program=True)
    table = prog.symbol_table

    for mod in ["field_mod", "function_space_mod", "fs_continuity_mod",
                "global_mesh_base_mod", "mesh_mod", "partition_mod",
                "extrusion_mod", "constants_mod", kernel_mod_name]:
        table.new_symbol(mod, symbol_type=ContainerSymbol)

    ftype = table.new_symbol("field_type", symbol_type=DataTypeSymbol,
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

    partitioner_planar = table.new_symbol(
        "partioner_planar", symbol_type=RoutineSymbol,
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

    spaces_decln = ""
    spaces_setup = ""
    for space in ["0", "1", "2", "2V", "2H", "3"]:
        table.new_symbol(f"W{space}", symbol_type=DataSymbol,
                         datatype=DeferredType(),
                         interface=ImportInterface(
                             table.lookup("fs_continuity_mod")))
        vsym = table.new_symbol(f"vector_space_w{space}",
                                symbol_type=DataSymbol,
                                datatype=UnknownFortranType(
                                    f"TYPE(function_space_type), TARGET :: "
                                    f"vector_space_w{space}"))
        table.new_symbol(f"vector_space_w{space}_ptr", symbol_type=DataSymbol,
                         datatype=UnknownFortranType(
                             f"TYPE(function_space_type), POINTER :: "
                             f"vector_space_w{space}_ptr"))
        ptree = Fortran2003.Assignment_Stmt(
            FortranStringReader(
                f"  vector_space_w{space} = function_space_type("
                f"  mesh, element_order, W{space}, ndata_sz)"))
        prog.addchild(CodeBlock([ptree], CodeBlock.Structure.STATEMENT))

        ptree = Fortran2003.Pointer_Assignment_Stmt(
            FortranStringReader(f"vector_space_w{space}_ptr => "
                                f"vector_space_w{space}\n"))
        prog.addchild(CodeBlock([ptree], CodeBlock.Structure.STATEMENT))

    kern_args = KernCallInvokeArgList(kern, table)
    kern_args.generate()

    kernel_arg_list = ','.join(name for name in kern_args.arglist)
    reader = FortranStringReader(f'''
PROGRAM main
  use testkern_mod
  call invoke({kernel_routine.name}({kernel_arg_list}))
END PROGRAM main
''')
    ast = parser(reader)
    calls = Fortran2003.walk(ast, types=Fortran2003.Call_Stmt)
    prog.addchild(CodeBlock([calls[0]], CodeBlock.Structure.STATEMENT))
    writer = FortranWriter()
    print(writer(prog))

    with open("main_alg.x90", "w", encoding="utf-8") as fmain:
        print(writer(prog))
