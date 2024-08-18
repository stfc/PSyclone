# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2022-2024, Science and Technology Facilities Council.
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
# Modified by: R. W. Ford, STFC Daresbury Laboratory.
#              L. Turner, Met Office

'''This module contains the LFRicAlg class which encapsulates tools for
   creating standalone LFRic algorithm-layer code.

'''

from psyclone.domain.lfric import (KernCallInvokeArgList, LFRicConstants,
                                   LFRicSymbolTable, LFRicTypes)
from psyclone.domain.lfric.algorithm.psyir import (
    LFRicAlgorithmInvokeCall, LFRicBuiltinFunctorFactory, LFRicKernelFunctor)
from psyclone.domain.lfric import LFRicKern
from psyclone.errors import InternalError
from psyclone.parse.kernel import get_kernel_parse_tree, KernelTypeFactory
from psyclone.parse.utils import ParseError
from psyclone.psyir.frontend.fortran import FortranReader
from psyclone.psyir.nodes import (Assignment, Container, Literal,
                                  Reference, Routine, ScopingNode)
from psyclone.psyir.symbols import (
    UnresolvedType, UnsupportedFortranType, DataTypeSymbol, DataSymbol,
    ArrayType, ImportInterface, ContainerSymbol, RoutineSymbol,
    ArgumentInterface)


class LFRicAlg:
    '''
    Encapsulates the functionality for generating an LFRic Algorithm
    layer from Kernel metadata.

    '''
    def create_from_kernel(self, name, kernel_path):
        '''
        Generates LFRic algorithm PSyIR that calls the supplied kernel through
        an 'invoke'. All of the arguments required by the kernel are
        constructed and intialised appropriately. Fields and scalars are all
        set to unity.

        :param str name: name to use for the algorithm subroutine.
        :param str kernel_path: location of Kernel source code.

        :returns: LFRic algorithm PSyIR.
        :rtype: :py:class:`psyclone.psyir.nodes.Container`

        :raises NotImplementedError: if the specified kernel file does not \
            follow the LFRic naming convention by having a module with a name \
            ending in '_mod'.

        '''
        # pylint: disable=too-many-locals

        # Create PSyIR for an algorithm routine.
        cont = self.create_alg_routine(name)
        sub = cont.walk(Routine)[0]
        table = sub.symbol_table

        # Parse the kernel metadata. Currently this uses fparser1 as that's
        # what the existing meta-data handling is based upon. Ultimately, this
        # will be replaced by the new, fparser2-based functionality being
        # implemented in #1631.
        parse_tree = get_kernel_parse_tree(kernel_path)

        # Get the name of the module that contains the kernel and create a
        # ContainerSymbol for it.
        kernel_mod_name = parse_tree.content[0].name
        # TODO #1806. The current meta-data parsing requires that we specify
        # the name of the kernel. It would be much better if we could query the
        # meta-data for the name of the kernel. For now we require that the
        # LFRic naming scheme is strictly adhered to (since this is simpler
        # than trying to walk through the deprecated fparser1 parse tree).
        if not kernel_mod_name.endswith("_mod"):
            raise NotImplementedError(
                f"The supplied kernel ({kernel_path}) contains a module named "
                f"'{kernel_mod_name}' which does not follow the LFRic naming "
                f"convention of ending in '_mod'.")
        kernel_name = kernel_mod_name[:-4] + "_type"

        kernel_mod = table.new_symbol(kernel_mod_name,
                                      symbol_type=ContainerSymbol)
        kernel_routine = table.new_symbol(
            kernel_name,
            symbol_type=DataTypeSymbol,
            datatype=UnresolvedType(),
            interface=ImportInterface(kernel_mod))

        kern = self.kernel_from_metadata(parse_tree, kernel_name)

        # Declare and initialise the data structures required by the kernel
        # arguments. Appropriate symbols are added to the symbol table
        # associated with the routine we are constructing.
        kern_args = self.construct_kernel_args(sub, kern)

        # Initialise argument values to unity. Since we are using this somewhat
        # arbitrary value, we use an *integer* literal for this, irrespective
        # of the actual type of the scalar argument. The compiler/run-time will
        # take care of appropriate type casting.
        table.add_lfric_precision_symbol("i_def")
        for sym in kern_args.scalars:
            sub.addchild(Assignment.create(
                Reference(sym),
                Literal("1", LFRicTypes("LFRicIntegerScalarDataType")())))

        # We use the setval_c builtin to initialise all fields to unity.
        # As with the scalar initialisation, we don't worry about precision
        # here since we are just setting the field values to unity. If the
        # field itself is of a precision other than r_def (or is perhaps
        # integer rather than real) we rely on type casting by the
        # compiler/run-time.
        factory = LFRicBuiltinFunctorFactory.get()
        table.add_lfric_precision_symbol("r_def")
        kernel_list = []
        for sym, _ in kern_args.fields:
            kernel_list.append(
                factory.create(
                    "setval_c", table,
                    [Reference(sym),
                     Literal("1.0",
                             LFRicTypes("LFRicRealScalarDataType")())]))

        # Finally, add the kernel itself to the list for the invoke().
        arg_nodes = []
        for arg in kern_args.arglist:
            arg_nodes.append(Reference(table.lookup(arg)))
        kern = LFRicKernelFunctor.create(kernel_routine, arg_nodes)
        kernel_list.append(kern)

        # Create the 'call invoke(...)' for the list of kernels.
        invoke_sym = table.new_symbol("invoke", symbol_type=RoutineSymbol)
        sub.addchild(LFRicAlgorithmInvokeCall.create(invoke_sym,
                                                     kernel_list, 0))
        return cont

    @staticmethod
    def create_alg_routine(name):
        '''
        Creates an LFRic algorithm subroutine within a module. The
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
        # Make sure the scoping node creates LFRicSymbolTables
        # pylint: disable=protected-access
        # TODO #1954 Remove the protected access using a factory
        ScopingNode._symbol_table_class = LFRicSymbolTable
        alg_sub = Routine(name)
        table = alg_sub.symbol_table

        # Create Container and Type Symbols for each of the modules/types that
        # we will need.
        for root in ["field", "function_space", "mesh"]:
            csym = table.new_symbol(root + "_mod", symbol_type=ContainerSymbol)

            table.new_symbol(root + "_type", symbol_type=DataTypeSymbol,
                             datatype=UnresolvedType(),
                             interface=ImportInterface(csym))

        fsc_mod = table.new_symbol("function_space_collection_mod",
                                   symbol_type=ContainerSymbol)
        table.new_symbol("function_space_collection",
                         symbol_type=DataSymbol,
                         datatype=UnresolvedType(),
                         interface=ImportInterface(fsc_mod))

        # Declare the three arguments to the subroutine. All of them have to be
        # of UnsupportedFortranType - the mesh because it is a pointer and chi
        # and panel_id because they are optional.
        mesh_ptr_type = UnsupportedFortranType(
            "type(mesh_type), pointer, intent(in) :: mesh")
        mesh_ptr = DataSymbol("mesh", mesh_ptr_type,
                              interface=ArgumentInterface())
        table.add(mesh_ptr)

        chi_type = UnsupportedFortranType(
            "type(field_type), dimension(3), intent(in), optional :: chi")
        chi = DataSymbol("chi", chi_type, interface=ArgumentInterface())
        table.add(chi, tag="coord_field")

        pid_type = UnsupportedFortranType(
            "type(field_type), intent(in), optional :: panel_id")
        pid = DataSymbol("panel_id", pid_type, interface=ArgumentInterface())
        table.add(pid, tag="panel_id_field")
        table.specify_argument_list([mesh_ptr, chi, pid])

        # Create top-level Container and put the new Subroutine inside it.
        container = Container(name+"_mod")
        container.addchild(alg_sub)

        return container

    def _create_function_spaces(self, prog, fspaces):
        '''
        Adds PSyIR to the supplied Routine that declares and intialises
        the specified function spaces. The order of these spaces is
        set by the element_order variable which is provided by the
        LFRic finite_element_config_mod module.

        :param prog: the routine to which to add declarations and \
                     initialisation.
        :type prog: :py:class:`psyclone.psyir.nodes.Routine`
        :param fspaces: the names of the required function spaces.
        :type fspaces: list[str]

        :raises InternalError: if a function space is supplied that is not a \
                               recognised LFRic function space.

        '''
        table = prog.symbol_table

        reader = FortranReader()

        # The order of the finite-element scheme.
        fe_config_mod = table.new_symbol(
            "finite_element_config_mod", symbol_type=ContainerSymbol)
        order = table.new_symbol(
            "element_order", tag="element_order",
            symbol_type=DataSymbol, datatype=UnresolvedType(),
            interface=ImportInterface(fe_config_mod))

        fs_cont_mod = table.new_symbol("fs_continuity_mod",
                                       symbol_type=ContainerSymbol)

        # Initialise the function spaces required by the kernel arguments.
        const = LFRicConstants()

        for space in fspaces:

            if space.lower() not in const.VALID_FUNCTION_SPACE_NAMES:
                raise InternalError(
                    f"Function space '{space}' is not a valid LFRic function "
                    f"space (one of {const.VALID_FUNCTION_SPACE_NAMES})")

            table.new_symbol(f"{space}", tag=f"{space}",
                             symbol_type=DataSymbol,
                             datatype=UnresolvedType(),
                             interface=ImportInterface(fs_cont_mod))

            vsym_ptr = table.new_symbol(
                f"vector_space_{space}_ptr", symbol_type=DataSymbol,
                tag=f"{space}_ptr",
                datatype=UnsupportedFortranType(
                    f"TYPE(function_space_type), POINTER :: "
                    f"vector_space_{space}_ptr"))

            cblock = reader.psyir_from_statement(
                f"{vsym_ptr.name} => function_space_collection%get_fs( mesh, "
                f"{order.name}, {space})", table)

            prog.addchild(cblock)

    @staticmethod
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

        prog.symbol_table.add_lfric_precision_symbol("i_def")

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
                        f"CALL {sym.name}({dim}_i_def) % initialise("
                        f"vector_space = vector_space_{space}_ptr, "
                        f"name = '{sym.name}')", prog.symbol_table))
        else:
            raise InternalError(
                f"Expected a field symbol to either be of ArrayType or have "
                f"a type specified by a DataTypeSymbol but found "
                f"{sym.datatype} for field '{sym.name}'")

    @staticmethod
    def initialise_operator(prog, sym, from_space, to_space):
        '''
        Creates the PSyIR for initialisation of the operator
        represented by the supplied symbol and adds it to the supplied
        routine.

        :param prog: the routine to which to add initialisation code.
        :type prog: :py:class:`psyclone.psyir.nodes.Routine`
        :param sym: the symbol representing the LFRic operator.
        :type sym: :py:class:`psyclone.psyir.symbols.DataSymbol`
        :param str from_space: the function space that the operator maps from.
        :param str to_space: the function space that the operator maps to.

        :raises InternalError: if the supplied symbol is of the wrong type.

        '''
        reader = FortranReader()

        prog.addchild(
            reader.psyir_from_statement(
                f"CALL {sym.name} % initialise("
                f"vector_space_{to_space}_ptr, vector_space_{from_space}_ptr)",
                prog.symbol_table))

    @staticmethod
    def initialise_quadrature(prog, qr_sym, shape):
        '''
        Adds the necessary declarations and intialisation for the supplied
        quadrature to the supplied routine.

        :param prog: the routine to which to add suitable declarations etc.
        :type prog: :py:class:`psyclone.psyir.nodes.Routine`
        :param qr_sym: the symbol representing a quadrature object.
        :type qr_sym: :py:class:`psyclone.psyir.symbols.DataSymbol`
        :param str shape: the shape of the quadrature.

        :raises NotImplementedError: if the quadrature shape is anything \
                                     other than gh_quadrature_xyoz.
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
                datatype=UnresolvedType(),
                interface=ImportInterface(qr_gaussian_mod))
            qr_rule_sym = table.new_symbol("quadrature_rule",
                                           symbol_type=DataSymbol,
                                           datatype=qr_gaussian_type)

        if shape == "gh_quadrature_xyoz":
            order = table.lookup_with_tag("element_order")
            expr = reader.psyir_from_expression(
                f"quadrature_xyoz_type({order.name}+3, {qr_rule_sym.name})",
                table)
            prog.addchild(Assignment.create(Reference(qr_sym), expr))

        else:
            raise NotImplementedError(f"Initialisation for quadrature of type "
                                      f"'{shape}' is not yet implemented.")

    @staticmethod
    def kernel_from_metadata(parse_tree, kernel_name):
        '''
        Given an fparser1 parse tree for an LFRic kernel, creates and returns
        a LFRicKern object.

        :param parse_tree: the fparser1 parse tree for the LFRic kernel.
        :type parse_tree: :py:class:`fparser.one.block_statements.BeginSource`
        :param str kernel_name: the name of the kernel contained in the \
            supplied parse tree for which a LFRicKern is to be created.

        :returns: a LFRicKern object describing the LFRic kernel.
        :rtype: :py:class:`psyclone.domain.lfric.LFRicKern`

        :raises ValueError: if an LFRic kernel with the specified name cannot \
                            be found in the supplied parse tree.
        '''
        try:
            ktype = KernelTypeFactory(api="dynamo0.3").create(parse_tree,
                                                              name=kernel_name)
        except ParseError as err:
            raise ValueError(
                f"Failed to find kernel '{kernel_name}' in supplied "
                f"code: '{parse_tree}'. Is it a valid LFRic kernel? Original "
                f"error was '{err}'.") from err
        # Construct a LFRicKern using the metadata.
        kern = LFRicKern()
        kern.load_meta(ktype)
        return kern

    def construct_kernel_args(self, prog, kern):
        '''
        Extends the supplied routine with all the declarations and
        initialisation required for the arguments of the supplied kernel.

        :param prog: the routine to which to add the declarations etc.
        :type prog: :py:class:`psyclone.psyir.nodes.Routine`
        :param kern: the kernel for which we are to create arguments.
        :type kern: :py:class:`psyclone.domain.lfric.LFRicKern`

        :returns: object capturing all of the kernel arguments.
        :rtype: :py:class:`psyclone.domain.lfric.KernCallInvokeArgList`

        '''
        const = LFRicConstants()
        # Construct a list of the names of the function spaces that the field
        # argument(s) are on and any operators map between. We use
        # LFRicConstants.specific_function_space()
        # to ensure that any 'wildcard' names in the meta-data are converted to
        # an appropriate, specific function space.
        function_spaces = []
        for fspace in kern.arguments.unique_fss:
            name = fspace.orig_name.lower()
            function_spaces.append(const.specific_function_space(name))
        self._create_function_spaces(prog, set(function_spaces))

        # Construct the argument list and add suitable symbols to the table.
        kern_args = KernCallInvokeArgList(kern, prog.symbol_table)
        kern_args.generate()

        # Initialise field datastructures using the symbols added to the table
        # when setting up the kernel arguments and the information on their
        # respective function spaces extracted from the kernel metadata.
        for sym, space in kern_args.fields:
            self.initialise_field(prog, sym, space)

        for sym, from_space, to_space in kern_args.operators:
            self.initialise_operator(prog, sym, from_space, to_space)

        for qr_sym, shape in kern_args.quadrature_objects:
            self.initialise_quadrature(prog, qr_sym, shape)

        return kern_args


# For automatic API documentation.
__all__ = ["LFRicAlg"]
