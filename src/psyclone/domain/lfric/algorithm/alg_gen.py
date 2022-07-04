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

   TODO #1771: these tools should be incorporated into the Alg class as they
   provide an alternative way of constructing an Algorithm layer.

'''

from psyclone.domain.lfric import KernCallInvokeArgList, LFRicConstants, psyir
from psyclone.domain.lfric.algorithm import (
    BUILTIN_FUNCTOR_MAP, LFRicAlgorithmInvokeCall, LFRicKernelFunctor)
from psyclone.dynamo0p3 import DynKern
from psyclone.errors import InternalError
from psyclone.parse.kernel import get_kernel_parse_tree, KernelTypeFactory
from psyclone.psyir.backend.fortran import FortranWriter
from psyclone.psyir.frontend.fortran import FortranReader
from psyclone.psyir.nodes import (Routine, Assignment, Reference, Literal,
                                  Container)
from psyclone.psyir.symbols import (
    DeferredType, UnknownFortranType, DataTypeSymbol, DataSymbol, ArrayType,
    ImportInterface, ContainerSymbol, RoutineSymbol, ArgumentInterface)


# The order of the finite-element scheme that will be used by any generated
# algorithm layer.
_ELEMENT_ORDER = "1"


def _create_alg_mod(name):
    '''
    Creates a standalone LFRic algorithm subroutine within a module. The
    generated subroutine has three arguments:

     * mesh: pointer to the LFRic mesh object.
     * chi: coordinate field (optional).
     * panel_id: field mapping cells to panel IDs (optional).

    :param str name: the name to give the created routine. The associated \
                     container will have "_mod" appended to this name.

    :returns: a container.
    :rtype: :py:class:`psyclone.psyir.nodes.Container`

    :raises TypeError: if the 'name' argument is of the wrong type.

    '''
    if not isinstance(name, str):
        raise TypeError(f"Supplied routine name must be a str but got "
                        f"'{type(name).__name__}'")

    alg_sub = Routine(name)
    table = alg_sub.symbol_table

    # Create Container and Type Symbols for each of the modules/types that
    # we will need.
    for root in ["field", "function_space", "function_space_collection",
                 "mesh"]:
        csym = table.new_symbol(root + "_mod", symbol_type=ContainerSymbol)

        table.new_symbol(root + "_type", symbol_type=DataTypeSymbol,
                         datatype=DeferredType(),
                         interface=ImportInterface(csym))

    # Declare the three arguments to the subroutine. All of them have to be
    # of UnknownFortranType - the mesh because it is a pointer and chi and
    # panel_id because they are optional.
    mesh_ptr_type = UnknownFortranType(
        "type(mesh_type), pointer, intent(in) :: mesh")
    mesh_ptr = DataSymbol("mesh", mesh_ptr_type, interface=ArgumentInterface())
    table.add(mesh_ptr)

    chi_type = UnknownFortranType(
        "type(field_type), dimension(3), intent(in), optional :: chi")
    chi = DataSymbol("chi", chi_type, interface=ArgumentInterface())
    table.add(chi)

    pid_type = UnknownFortranType(
        "type(field_type), intent(in), optional :: panel_id")
    pid = DataSymbol("panel_id", pid_type, interface=ArgumentInterface())
    table.add(pid)
    table.specify_argument_list([mesh_ptr, chi, pid])

    # Create top-level Container and put the new Subroutine inside it.
    container = Container(name+"_mod")
    container.addchild(alg_sub)

    return container


def _create_function_spaces(prog, fspaces):
    '''
    Adds PSyIR to the supplied Routine that declares and intialises the
    specified function spaces. The order of these spaces is set by the
    `_ELEMENT_ORDER` constant at the top of this module.

    :param prog: the routine to which to add declarations and initialisation.
    :type prog: :py:class:`psyclone.psyir.nodes.Routine`
    :param fspaces: the names of the required function spaces.
    :type fspaces: list[str]

    :raises InternalError: if a function space is supplied that is not a \
                           recognised LFRic function space.
    '''
    table = prog.symbol_table

    reader = FortranReader()

    # The order of the finite-element scheme.
    psyir.add_lfric_precision_symbol(table, "i_def")
    order = table.new_symbol("element_order", tag="element_order",
                             symbol_type=DataSymbol,
                             datatype=psyir.LfricIntegerScalarDataType(),
                             constant_value=Literal(
                                 _ELEMENT_ORDER,
                                 psyir.LfricIntegerScalarDataType()))

    fs_cont_mod = table.new_symbol("fs_continuity_mod",
                                   symbol_type=ContainerSymbol)

    # Initialise the function spaces required by the kernel arguments.
    const = LFRicConstants()

    for space in fspaces:

        if space.lower() not in const.VALID_FUNCTION_SPACE_NAMES:
            raise InternalError(
                f"Function space '{space}' is not a valid LFRic function "
                f"space (one of {const.VALID_FUNCTION_SPACE_NAMES})")

        table.new_symbol(f"{space}", tag=f"{space}", symbol_type=DataSymbol,
                         datatype=DeferredType(),
                         interface=ImportInterface(fs_cont_mod))

        vsym_ptr = table.new_symbol(
            f"vector_space_{space}_ptr", symbol_type=DataSymbol,
            datatype=UnknownFortranType(
                f"TYPE(function_space_type), POINTER :: "
                f"vector_space_{space}_ptr"))

        cblock = reader.psyir_from_statement(
            f"{vsym_ptr.name} => function_space_collection%get_fs( mesh, "
            f"{order.name}, {space})", table)

        prog.addchild(cblock)


def initialise_field(prog, sym, space):
    '''
    Creates the PSyIR for initialisation of the field or field vector
    represented by the supplied symbol and adds it to the supplied
    routine.

    :param prog: the routine to which to add initialisation code.
    :type prog: :py:class:`psyclone.psyir.nodes.Routine`
    :param sym: the symbol representing the LFRic field.
    :type sym: :py:class:`psyclone.psyir.symbols.DataSymbol`
    :param str space: the function space of the field.

    :raises InternalError: if the supplied symbol is of the wrong type.

    '''
    reader = FortranReader()

    if isinstance(sym.datatype, DataTypeSymbol):
        # Single field argument.
        prog.addchild(
            reader.psyir_from_statement(
                f"CALL {sym.name} % initialise(vector_space = "
                f"vector_space_{space}_ptr, name = '{sym.name}')",
                prog.symbol_table))

    elif isinstance(sym.datatype, ArrayType):
        # Field vector argument.
        for dim in range(int(sym.datatype.shape[0].lower.value),
                         int(sym.datatype.shape[0].upper.value)+1):
            prog.addchild(
                reader.psyir_from_statement(
                    f"CALL {sym.name}({dim}) % initialise(vector_space = "
                    f"vector_space_{space}_ptr, name = '{sym.name}')",
                    prog.symbol_table))
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
    :type prog: :py:class:`psyclone.psyir.nodes.Routine`
    :param qr_sym: the symbol representing a quadrature object.
    :type qr_sym: :py:class:`psyclone.psyir.symbols.DataSymbol`
    :param str shape: the shape of the quadrature.

    :raises NotImplementedError: if the quadrature shape is anything other \
                                 than gh_quadrature_xyoz.
    '''
    reader = FortranReader()
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
        expr = reader.psyir_from_expression(
            f"quadrature_xyoz_type({order.name}+3, {qr_rule_sym.name})", table)
        prog.addchild(Assignment.create(Reference(qr_sym), expr))

    else:
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

    :returns: object capturing all of the kernel arguments.
    :rtype: :py:class:`psyclone.domain.lfric.KernCallInvokeArgList`

    '''
    const = LFRicConstants()
    # Construct a list of the names of the function spaces that the field
    # argument(s) are on. We use LFRicConstants.specific_function_space() to
    # ensure that any 'wildcard' names in the meta-data are converted to
    # an appropriate, specific function space.
    function_spaces = []
    for fspace in kern.arguments.unique_fss:
        name = fspace.orig_name.lower()
        function_spaces.append(const.specific_function_space(name))
    _create_function_spaces(prog, set(function_spaces))

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


def generate(kernel_path):
    '''
    Generates LFRic algorithm code that calls the supplied kernel through
    an 'invoke'. All of the arguments required by the kernel are constructed
    and intialised appropriately. Fields and scalars are all set to unity.

    :param str kernel_path: location of Kernel source code.

    :returns: Fortran algorithm code.
    :rtype: str

    :raises NotImplementedError: if the specified kernel file does not \
        follow the LFRic naming convention by having a module with a name \
        ending in '_mod'.

    '''
    # TODO #1771 this code needs refactoring.
    # pylint: disable=too-many-locals

    # Create PSyIR for an algorithm routine.
    cont = _create_alg_mod("test_alg")
    sub = cont.walk(Routine)[0]
    table = sub.symbol_table

    # Parse the kernel metadata. Currently this uses fparser1 as that's what
    # the existing meta-data handling is based upon. Ultimately, this will
    # be replaced by the new, fparser2-based functionality being implemented
    # in #1631.
    parse_tree = get_kernel_parse_tree(kernel_path)

    # Get the name of the module that contains the kernel and create a
    # ContainerSymbol for it.
    kernel_mod_name = parse_tree.content[0].name
    # TODO #1453. The current meta-data parsing requires that we specify
    # the name of the kernel. It would be much better if we could query the
    # meta-data for the name of the kernel. For now we require that the LFRic
    # naming scheme is strictly adhered to (since this is simpler than trying
    # to walk through the deprecated fparser1 parse tree).
    if not kernel_mod_name.endswith("_mod"):
        raise NotImplementedError(
            f"The supplied kernel ({kernel_path}) contains a module named "
            f"'{kernel_mod_name}' which does not follow the LFRic naming "
            f"convention of ending in '_mod'.")
    kernel_name = kernel_mod_name[:-4] + "_type"

    kernel_mod = table.new_symbol(kernel_mod_name, symbol_type=ContainerSymbol)
    kernel_routine = table.new_symbol(kernel_name,
                                      symbol_type=DataTypeSymbol,
                                      datatype=DeferredType(),
                                      interface=ImportInterface(kernel_mod))

    ktype = KernelTypeFactory(api="dynamo0.3").create(parse_tree,
                                                      name=kernel_name)
    # Construct a DynKern using the metadata. This is used when constructing
    # the kernel argument list.
    kern = DynKern()
    kern.load_meta(ktype)

    # Declare and initialise the data structures required by the kernel
    # arguments. Appropriate symbols are added to the symbol table associated
    # with the routine we are constructing.
    kern_args = construct_kernel_args(sub, kern)

    # Initialise argument values to unity. Since we are using this somewhat
    # arbitrary value, we use an *integer* literal for this, irrespective of
    # the actual type of the scalar argument. The compiler/run-time will take
    # care of appropriate type casting.
    psyir.add_lfric_precision_symbol(table, "i_def")
    for sym in kern_args.scalars:
        sub.addchild(Assignment.create(
            Reference(sym),
            Literal("1", psyir.LfricIntegerScalarDataType())))

    # We use the setval_c builtin to initialise all fields to unity.
    # As with the scalar initialisation, we don't worry about precision
    # here since we are just setting the field values to unity. If the
    # field itself is of a precision other than r_def (or is perhaps
    # integer rather than real) we rely on type casting by the
    # compiler/run-time.
    psyir.add_lfric_precision_symbol(table, "r_def")
    kernel_list = []
    for sym, _ in kern_args.fields:
        kernel_list.append(
            BUILTIN_FUNCTOR_MAP["setval_c"].create(
                table,
                [Reference(sym),
                 Literal("1.0", psyir.LfricRealScalarDataType())]))

    # Finally, add the kernel itself to the list for the invoke().
    arg_nodes = []
    for arg in kern_args.arglist:
        arg_nodes.append(Reference(table.lookup(arg)))
    kern = LFRicKernelFunctor.create(kernel_routine, arg_nodes)
    kernel_list.append(kern)

    # Create the 'call invoke(...)' for the list of kernels.
    invoke_sym = table.new_symbol("invoke", symbol_type=RoutineSymbol)
    sub.addchild(LFRicAlgorithmInvokeCall.create(invoke_sym, kernel_list, 0))

    return FortranWriter()(cont)


# For automatic API documentation.
__all__ = ["initialise_field", "initialise_quadrature",
           "construct_kernel_args", "generate"]
