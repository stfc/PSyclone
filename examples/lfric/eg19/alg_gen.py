import sys

from fparser.two.parser import ParserFactory
from fparser.two import Fortran2003

from psyclone.parse.kernel import get_kernel_parse_tree, KernelTypeFactory
from psyclone.psyir.backend.fortran import FortranWriter
from psyclone.psyir.nodes import Routine, Call, CodeBlock
from psyclone.psyir.symbols import (INTEGER_TYPE, REAL_TYPE,
                                    DataSymbol, DataTypeSymbol, DeferredType,
                                    ImportInterface, ContainerSymbol)

if __name__ == "__main__":
    # Ensure the Fortran parser is initialised
    ParserFactory().create(std="f2008")

    kernel_path = sys.argv[1]
    kernel_name = "testkern_type"
    kernel_mod_name = "testkern_mod"
    parse_tree = get_kernel_parse_tree(kernel_path)
    print(parse_tree)
    ktype = KernelTypeFactory(api="dynamo0.3").create(parse_tree,
                                                      name=kernel_name)
    spaces_decln = ""
    spaces_setup = ""
    for space in ["0", "1", "2", "2V", "2H", "3"]:
        spaces_decln += (
            f"  TYPE(function_space_type), TARGET :: vector_space_w{space}\n"
            f"  TYPE(function_space_type), POINTER :: "
            f"vector_space_w{space}_ptr\n")
        spaces_setup += (
            f"  vector_space_w{space} = function_space_type("
            f"  mesh, element_order, W{space}, ndata_sz)\n"
            f"  vector_space_w{space}_ptr => vector_space_w{space}\n")

    # Identify what fields the kernel requires
    prog = Routine("main", is_program=True)
    table = prog.symbol_table
    field_mod = table.new_symbol("field_mod", symbol_type=ContainerSymbol)
    ftype = table.new_symbol("field_type", symbol_type=DataTypeSymbol,
                             datatype=DeferredType(),
                             interface=ImportInterface(field_mod))
    kernel_mod = table.new_symbol(kernel_mod_name, symbol_type=ContainerSymbol)
    kernel_routine = table.new_symbol("testkern_type",
                                      interface=ImportInterface(kernel_mod))
    # Loop over all of the 'arg_type' entries in the meta-data and create
    # appropriate symbols.
    arg_names = []
    field_inits = ""
    scalars_decln = ""
    fields_decln = ""
    for arg in ktype.arg_descriptors:
        print(arg.data_type)
        print(arg.function_space)
        if arg.argument_type == "gh_scalar":
            if arg.data_type == "gh_real":
                datatype = REAL_TYPE
                fortran_type = "real(kind=r_def)"
            elif arg.data_type == "gh_integer":
                datatype = INTEGER_TYPE
                fortran_type = "integer(kind=i_def)"
            else:
                raise NotImplementedError(
                    f"Scalar of type '{arg.data_type}' not supported.")
            sym = table.new_symbol("scalar",
                                   symbol_type=DataSymbol, datatype=datatype)
            scalars_decln += f"  {fortran_type} :: {sym.name}\n"

        elif arg.argument_type == "gh_field":
            sym = table.new_symbol(f"field_{arg.function_space}",
                                   symbol_type=DataSymbol, datatype=ftype)
            fields_decln += f"  TYPE(field_type) :: {sym.name}\n"
            field_inits += (
                f"  CALL {sym.name} % initialise("
                f"vector_space = vector_space_{arg.function_space}_ptr, "
                f"name = '{sym.name}')\n")
        else:
            raise NotImplementedError(
                f"Kernel argument of type '{arg.argument_type}' not "
                f"supported.")
        arg_names.append(sym.name)

    kernel_arg_list = ','.join(name for name in arg_names)
    invoke_txt = f"  call invoke({kernel_routine.name}({kernel_arg_list}))\n"
    #ptree = Fortran2003.Call_Stmt(invoke_txt)
    #prog.addchild(CodeBlock([ptree], CodeBlock.Structure.STATEMENT))
    #writer = FortranWriter()
    #print(writer(prog))

    kernel_uses = f"  use {kernel_mod_name}, only: {kernel_name}\n"
    # Now we want to create a suitable algorithm layer that invokes
    # this kernel. For simplicity we use a template algorithm
    # layer taken from examples/lfric/eg17.
    alg_code = f'''
PROGRAM main
  USE global_mesh_base_mod, ONLY: global_mesh_base_type
  USE mesh_mod, ONLY: mesh_type, PLANE
  USE partition_mod, ONLY: partition_type, partitioner_planar, &
      partitioner_interface
  USE extrusion_mod, ONLY: uniform_extrusion_type
  USE function_space_mod, ONLY: function_space_type
  USE fs_continuity_mod, ONLY: W0, W1, W2, W2V, W2H, W3
  USE field_mod, ONLY: field_type
  USE constants_mod, ONLY: r_def, i_def
  USE log_mod, ONLY: LOG_LEVEL_ALWAYS
{kernel_uses}
  IMPLICIT NONE
  TYPE(global_mesh_base_type), TARGET :: global_mesh
  CLASS(global_mesh_base_type), POINTER :: global_mesh_ptr
  TYPE(partition_type) :: partition
  TYPE(mesh_type), TARGET :: mesh
  TYPE(uniform_extrusion_type), TARGET :: extrusion
  TYPE(uniform_extrusion_type), POINTER :: extrusion_ptr
{spaces_decln}
  PROCEDURE(partitioner_interface), POINTER :: partitioner_ptr
{scalars_decln}
{fields_decln}
  !INTEGER(KIND = i_def) :: lfric_fs = W0
  INTEGER(KIND = i_def) :: element_order = 1
  INTEGER(KIND = i_def) :: ndata_sz
  global_mesh = global_mesh_base_type()
  global_mesh_ptr => global_mesh
  partitioner_ptr => partitioner_planar
  partition = partition_type(global_mesh_ptr, partitioner_ptr, 1, 1, 0, 0, 1)
  extrusion = uniform_extrusion_type(0.0_r_def, 100.0_r_def, 5)
  extrusion_ptr => extrusion
  mesh = mesh_type(global_mesh_ptr, partition, extrusion_ptr)
  WRITE(*, *) "Mesh has", mesh % get_nlayers(), "layers."
  ndata_sz = 20
{spaces_setup}
{field_inits}
{invoke_txt}
END PROGRAM main
'''
    print(alg_code)
    with open("main_alg.x90", "w", encoding="utf-8") as fmain:
        print(alg_code, file=fmain)
