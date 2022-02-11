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
# Author: A. R. Porter, STFC Daresbury Laboratory.

'''This module contains tools for creating standalone LFRic
   algorithm-layer code.

'''

from fparser.common.readfortran import FortranStringReader
from fparser.two.parser import ParserFactory
from fparser.two import Fortran2003

from psyclone.domain.lfric import KernCallInvokeArgList
from psyclone.dynamo0p3 import DynKern
from psyclone.errors import InternalError
from psyclone.parse.kernel import get_kernel_parse_tree, KernelTypeFactory
from psyclone.psyir.backend.fortran import FortranWriter
from psyclone.psyir.nodes import (Routine, CodeBlock, Call, Assignment,
                                  Reference, Literal)
from psyclone.psyir.symbols import (
    DeferredType, UnknownFortranType, DataTypeSymbol, DataSymbol, ArrayType,
    ImportInterface, ContainerSymbol, RoutineSymbol, INTEGER_TYPE)


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
    parser = ParserFactory().create(std="f2008")
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
    ndata_sz = table.new_symbol("ndata_sz", symbol_type=DataSymbol,
                                datatype=INTEGER_TYPE)
    prog.addchild(Assignment.create(Reference(ndata_sz),
                                    Literal("20", INTEGER_TYPE)))

    return prog


def create_function_spaces(prog, fspaces):
    '''
    Adds PSyIR to the supplied Routine that declares and intialises the
    specified function spaces.

    :param prog: the routine to which to add declarations and initialisation.
    :type prog: :py:class:`psyclone.psyir.nodes.Routine`
    :param fspaces: the names of the required function spaces.
    :type fspaces: list[str]

    '''
    table = prog.symbol_table

    order = table.new_symbol("element_order", tag="element_order",
                             symbol_type=DataSymbol,
                             datatype=INTEGER_TYPE,
                             constant_value=Literal("1", INTEGER_TYPE))

    # Initialise the function spaces required by the kernel arguments.
    for space in fspaces:
        table.new_symbol(f"{space}", tag=f"{space}", symbol_type=DataSymbol,
                         datatype=DeferredType(),
                         interface=ImportInterface(
                             table.lookup("fs_continuity_mod")))
        vsym = table.new_symbol(f"vector_space_{space}",
                                symbol_type=DataSymbol,
                                datatype=UnknownFortranType(
                                    f"TYPE(function_space_type), TARGET :: "
                                    f"vector_space_{space}"))
        vsym_ptr = table.new_symbol(
            f"vector_space_{space}_ptr", symbol_type=DataSymbol,
            datatype=UnknownFortranType(
                f"TYPE(function_space_type), POINTER :: "
                f"vector_space_{space}_ptr"))
        ptree = Fortran2003.Structure_Constructor(
            FortranStringReader(
                f"function_space_type(mesh, {order.name}, {space}, ndata_sz)"))
        prog.addchild(
            Assignment.create(Reference(vsym),
                              CodeBlock([ptree],
                                        CodeBlock.Structure.EXPRESSION)))

        ptree = Fortran2003.Pointer_Assignment_Stmt(
            FortranStringReader(f"{vsym_ptr.name} => {vsym.name}\n"))
        prog.addchild(CodeBlock([ptree], CodeBlock.Structure.STATEMENT))


def initialise_field(prog, sym, space):
    '''
    :param prog: the routine to which to add initialisation code.
    :type prog: :py:class:`psyclone.psyir.nodes.Routine`
    :param sym: the symbol representing the LFRic field.
    :type sym: :py:class:`psyclone.psyir.symbols.DataSymbol`
    :param str space: the function space of the field.

    '''
    if isinstance(sym.datatype, DataTypeSymbol):
        # Single field argument.
        ptree = Fortran2003.Call_Stmt(
            f"CALL {sym.name} % initialise(vector_space = "
            f"vector_space_{space}_ptr, name = '{sym.name}')")
        prog.addchild(CodeBlock([ptree], CodeBlock.Structure.STATEMENT))

    elif isinstance(sym.datatype, ArrayType):
        # Field vector argument.
        for dim in range(int(sym.datatype.shape[0].lower.value),
                         int(sym.datatype.shape[0].upper.value)+1):
            ptree = Fortran2003.Call_Stmt(
                f"CALL {sym.name}({dim}) % initialise(vector_space = "
                f"vector_space_{space}_ptr, name = '{sym.name}')")
            prog.addchild(CodeBlock([ptree],
                                    CodeBlock.Structure.STATEMENT))
    else:
        raise InternalError(
            f"Expected a field symbol to either be of ArrayType or have "
            f"a type specified by a DataTypeSymbol but found "
            f"{sym.datatype} for field '{sym.name}'")


def initialise_quadrature(prog, qr_sym, shape):
    '''
    Adds the necessary declarations and intialisation for the supplied
    quadrature to the supplied routine.

    :param prog: the routine to which to add suitable declarations etc.
    :type prog:
    :param qr_sym: the symbol representing a quadrature object.
    :type qr_sym:
    :param str shape: the shape of the quadrature.

    '''
    table = prog.symbol_table
    try:
        qr_rule_sym = table.lookup("quadrature_rule")
    except KeyError:
        qr_gaussian_mod = table.new_symbol(
            "quadrature_rule_gaussian_mod", symbol_type=ContainerSymbol)
        qr_gaussian_type = table.new_symbol(
            "quadrature_rule_gaussian_type", symbol_type=DataTypeSymbol,
            datatype=DeferredType(),
            interface=ImportInterface(qr_gaussian_mod))
        qr_rule_sym = table.new_symbol("quadrature_rule",
                                       symbol_type=DataSymbol,
                                       datatype=qr_gaussian_type)

    if shape == "gh_quadrature_xyoz":
        order = table.lookup_with_tag("element_order")
        reader = FortranStringReader(
            f"quadrature_xyoz_type({order.name}+3, {qr_rule_sym.name})")
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


def construct_kernel_args(prog, kern):
    '''
    Extends the supplied routine with all the declarations and initialisation
    required for the arguments of the supplied kernel.

    :param prog: the routine to which to add the declarations etc.
    :type prog: :py:class:`psyclone.psyir.nodes.Routine`
    :param kern: the kernel for which we are to create arguments.
    :type kern: :py:class:`psyclone.dynamo0p3.DynKern`

    '''
    # Construct a list of the names of the function spaces that the field
    # argument(s) are on.
    function_spaces = []
    for fspace in kern.arguments.unique_fss:
        name = fspace.orig_name.lower()
        if name == "any_w2":
            # ANY_W2 means 'any W2 space' - it is not in and of itself
            # a valid function space so change it to W2.
            name = "w2"
        function_spaces.append(name)
    create_function_spaces(prog, set(function_spaces))

    # Construct the argument list and add suitable symbols to the table.
    kern_args = KernCallInvokeArgList(kern, prog.symbol_table)
    kern_args.generate()

    # Initialise field datastructures using the symbols added to the table
    # when setting up the kernel arguments and the information on their
    # respective function spaces extracted from the kernel metadata.
    for sym, space in kern_args.fields:
        initialise_field(prog, sym, space)

    for qr_sym, shape in kern_args.quadrature_objects:
        initialise_quadrature(prog, qr_sym, shape)

    return kern_args


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


def generate(kernel_path):
    '''
    :param str kernel_path: location of Kernel source code.

    :returns: Fortran algorithm code.
    :rtype: str

    '''
    prog = create_alg_driver("lfric_alg", 20)
    table = prog.symbol_table

    # Parse the kernel metadata (this still uses fparser1 as that's what
    # the meta-data handling is currently based upon).
    parse_tree = get_kernel_parse_tree(kernel_path)

    # Get the name of the module that contains the kernel and create a
    # ContainerSymbol for it.
    kernel_mod_name = parse_tree.content[0].name
    # TODO this assumes that the LFRic naming scheme is strictly adhered to!
    # It would be much better if we could query the meta-data for the name
    # of the kernel.
    kernel_name = kernel_mod_name.replace("_mod", "_type")

    kernel_mod = table.new_symbol(kernel_mod_name, symbol_type=ContainerSymbol)
    kernel_routine = table.new_symbol(kernel_name,
                                      interface=ImportInterface(kernel_mod))

    ktype = KernelTypeFactory(api="dynamo0.3").create(parse_tree,
                                                      name=kernel_name)
    # Construct a DynKern using the metadata. This is used when constructing
    # the kernel argument list.
    kern = DynKern()
    kern.load_meta(ktype)

    # Declare and initialise the data structures required by the kernel
    # arguments.
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

    # Finally, add the kernel itself to the list for the invoke().
    kernel_list.append((kernel_routine.name, kern_args.arglist))

    # Create the 'call invoke(...)' for the list of kernels.
    prog.addchild(create_invoke_call(kernel_list))

    return FortranWriter()(prog)
