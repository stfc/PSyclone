# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2021-2022, Science and Technology Facilities Council.
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

'''The implementation of PSyAD : the PSyclone Adjoint
support. Transforms an LFRic tangent linear kernel to its adjoint.

'''
import logging

from fparser.two import Fortran2003
from psyclone.errors import InternalError
from psyclone.psyad import AdjointVisitor
from psyclone.psyad.transformations.preprocess import preprocess_trans
from psyclone.psyir.backend.fortran import FortranWriter
from psyclone.psyir.frontend.fortran import FortranReader
from psyclone.psyir.nodes import Routine, Assignment, Reference, Literal, \
    Call, Container, BinaryOperation, UnaryOperation, IfBlock, \
    CodeBlock, FileContainer, ArrayReference, Range
from psyclone.psyir.symbols import SymbolTable, ImportInterface, Symbol, \
    ContainerSymbol, ScalarType, ArrayType, RoutineSymbol, DataSymbol, \
    INTEGER_TYPE


#: The tolerance applied to the comparison of the inner product values in
#: the generated test-harness code.
#: TODO #1346 this tolerance should be user configurable.
INNER_PRODUCT_TOLERANCE = 1500.0
#: The suffix we will prepend to a routine and container name when generating
#: its adjoint.
ADJOINT_NAME_SUFFIX = "_adj"
#: The extent we will allocate to each dimension of arrays used in the
#: generated test-harness code.
#: TODO #1331 provide some way of configuring the extent of the test arrays
TEST_ARRAY_DIM_SIZE = 20


def generate_adjoint_str(tl_fortran_str, active_variables, create_test=False):
    '''Takes an LFRic tangent-linear kernel encoded as a string as input
    and returns its adjoint encoded as a string along with (if requested)
    a test harness, also encoded as a string.

    :param str tl_fortran_str: Fortran implementation of an LFRic \
        tangent-linear kernel.
    :param list of str active_variables: list of active variable names.
    :param bool create_test: whether or not to create test code for the \
        adjoint kernel.

    :returns: a 2-tuple consisting of a string containing the Fortran \
        implementation of the supplied tangent-linear kernel and (if \
        requested) a string containing the Fortran implementation of a test \
        harness for the adjoint kernel.
    :rtype: 2-tuple of str

    '''
    logger = logging.getLogger(__name__)
    logger.debug(tl_fortran_str)

    # TL Language-level PSyIR
    reader = FortranReader()
    tl_psyir = reader.psyir_from_source(tl_fortran_str)

    logger.debug(f"PSyIR\n{tl_psyir.view(colour=False)}")

    # Apply any required transformations to the TL PSyIR
    logger.debug("Preprocessing")
    preprocess_trans(tl_psyir, active_variables)

    logger.debug(f"PSyIR after TL preprocessing\n"
                 f"{tl_psyir.view(colour=False)}")

    # TL to AD translation
    ad_psyir = generate_adjoint(tl_psyir, active_variables)

    # AD Fortran code
    writer = FortranWriter()
    adjoint_fortran_str = writer(ad_psyir)
    logger.debug(adjoint_fortran_str)

    # Create test harness if requested
    test_fortran_str = ""
    if create_test:
        test_psyir = generate_adjoint_test(tl_psyir, ad_psyir,
                                           active_variables)
        test_fortran_str = writer(test_psyir)
        logger.debug(test_fortran_str)

    return adjoint_fortran_str, test_fortran_str


def _find_container(psyir):
    ''' Finds the first Container in the supplied PSyIR that is not a
    FileContainer. Also validates that the PSyIR contains at most one
    FileContainer which, if present, contains a Container.

    :returns: the first Container that is not a FileContainer or None if \
              there is none.
    :rtype: :py:class:`psyclone.psyir.nodes.Container` or NoneType

    :raises InternalError: if there are two Containers and the second is a \
                           FileContainer.
    :raises NotImplementedError: if there are two Containers and the first is \
                                 not a FileContainer.
    :raises NotImplementedError: if there are more than two Containers.

    '''
    containers = psyir.walk(Container)
    if not containers:
        return None

    if len(containers) == 1:
        if isinstance(containers[0], FileContainer):
            return None
        return containers[0]

    if len(containers) == 2:
        if isinstance(containers[1], FileContainer):
            raise InternalError(
                "The supplied PSyIR contains two Containers but the innermost "
                "is a FileContainer. This should not be possible.")
        if not isinstance(containers[0], FileContainer):
            raise NotImplementedError(
                "The supplied PSyIR contains two Containers and the outermost "
                "one is not a FileContainer. This is not supported.")
        return containers[1]

    raise NotImplementedError("The supplied PSyIR contains more than two "
                              "Containers. This is not supported.")


def _get_active_variables_datatype(kernel, active_variables):
    '''
    Returns a ScalarType describing the type of the active variables in
    the supplied kernel PSyIR.

    :param kernel: the PSyIR of a tangent-linear kernel.
    :type kernel: :py:class:`psyclone.psyir.nodes.KernelSchedule`
    :param active_variables: the names of the active variables.
    :type active_variables: list of str

    :returns: the type of the active variables.
    :rtype: :py:class:`psyclone.psyir.symbols.ScalarType`

    :raises InternalError: if no active variables are supplied.
    :raises NotImplementedError: if the supplied active variables are not all \
                                 of the same intrinsic type and precision.
    '''
    if not active_variables:
        raise InternalError("No active variables have been supplied.")

    precision = None
    intrinsic = None
    for var in active_variables:
        sym = kernel.symbol_table.lookup(var)
        if precision is None:
            precision = sym.datatype.precision
            intrinsic = sym.datatype.intrinsic
        else:
            if (precision != sym.datatype.precision or
                    intrinsic != sym.datatype.intrinsic):
                raise NotImplementedError(
                    "Found active variables of different datatype: '{0}' is "
                    "of intrinsic type '{1}' and precision '{2}' while '{3}' "
                    "is of intrinsic type '{4}' and precision '{5}'. This is "
                    "not currently supported.".format(
                        active_variables[0], intrinsic, precision, sym.name,
                        sym.datatype.intrinsic, sym.datatype.precision))
    return ScalarType(intrinsic, precision)


def generate_adjoint(tl_psyir, active_variables):
    '''Takes an LFRic tangent-linear kernel represented in language-level PSyIR
    and returns its adjoint represented in language-level PSyIR.

    Currently just takes a copy of the supplied PSyIR and re-names the
    Container (if there is one) and Routine nodes.

    :param tl_psyir: language-level PSyIR containing the LFRic \
        tangent-linear kernel.
    :type tl_psyir: :py:class:`psyclone.psyir.Node`
    :param list of str active_variables: list of active variable names.

    :returns: language-level PSyIR containing the adjoint of the \
        supplied tangent-linear kernel.
    :rtype: :py:class:`psyclone.psyir.Node`

    :raises InternalError: if the PSyIR does not contain any Routines.
    :raises NotImplementedError: if the PSyIR contains >1 Routine.

    '''
    logger = logging.getLogger(__name__)

    # Translate from TL to AD
    logger.debug("Translating from TL to AD.")
    adjoint_visitor = AdjointVisitor(active_variables)
    ad_psyir = adjoint_visitor(tl_psyir)

    # We permit the input code to be a single Program or Subroutine
    container = _find_container(ad_psyir)
    if container:
        # Re-name the Container for the adjoint code. Use the symbol table
        # for the existing TL code so that we don't accidentally clash with
        # e.g. the name of the kernel routine.
        container.name = container.symbol_table.next_available_name(
            container.name + ADJOINT_NAME_SUFFIX)

    routines = ad_psyir.walk(Routine)

    if not routines:
        raise InternalError("The supplied PSyIR does not contain any "
                            "routines.")

    if len(routines) != 1:
        raise NotImplementedError(
            "The supplied Fortran must contain one and only one routine "
            "but found: {0}".format([sub.name for sub in routines]))
    routine = routines[0]

    # We need to re-name the kernel routine. Have to take care in case we've
    # been supplied with a bare program/subroutine rather than a subroutine
    # within a module.
    if container:
        kernel_sym = container.symbol_table.lookup(routine.name)
        adj_kernel_name = routine.name + ADJOINT_NAME_SUFFIX
        # A symbol's name is immutable so create a new RoutineSymbol
        adj_kernel_sym = container.symbol_table.new_symbol(
            adj_kernel_name, symbol_type=RoutineSymbol,
            visibility=kernel_sym.visibility)
        container.symbol_table.remove(kernel_sym)
        routine.name = adj_kernel_sym.name
    else:
        routine.name = routine.symbol_table.next_available_name(
            routine.name + ADJOINT_NAME_SUFFIX)

    logger.debug("AD kernel will be named '%s'", routine.name)

    return ad_psyir


def generate_adjoint_test(tl_psyir, ad_psyir,
                          active_variables):
    '''
    Creates the PSyIR of a test harness for the supplied TL and adjoint
    kernels.

    :param tl_psyir: PSyIR of the tangent-linear kernel code.
    :type tl_psyir: :py:class:`psyclone.psyir.Container`
    :param ad_psyir: PSyIR of the adjoint kernel code.
    :type ad_psyir: :py:class:`psyclone.psyir.Container`
    :param list of str active_variables: list of active variable names.

    :returns: the PSyIR of the test harness.
    :rtype: :py:class:`psyclone.psyir.Routine`

    :raises NotImplementedError: if the supplied PSyIR contains more than \
        one Routine.
    :raises NotImplementedError: if the supplied TL/Adjoint PSyIR contains \
        just a Routine that is a Program (since this would have to \
        be converted to a subroutine in order to construct the test harness).
    :raises NotImplementedError: if one of the kernel arguments is \
        dimensioned by a variable that is not passed as an argument.
    :raises InternalError: if a kernel argument has a shape defined by \
        something other than ArrayType.Extent or a Reference.

    '''
    logger = logging.getLogger(__name__)

    symbol_table = SymbolTable()

    # We expect a single Container containing a single Kernel. Anything else
    # is not supported. However, we have to allow for the fact that, in
    # general, there will be an outermost FileContainer.
    container = _find_container(ad_psyir)
    if not container:
        raise NotImplementedError(
            "Generation of a test harness is only supported for a TL kernel "
            "implemented as a subroutine within a module but failed to find "
            "enclosing module.")

    # First check that there's only one routine and that it's not a Program.
    routines = tl_psyir.walk(Routine)

    if len(routines) != 1:
        raise NotImplementedError(
            "The supplied Fortran must contain one and only one subroutine "
            "but found: {0}".format([sub.name for sub in routines]))

    tl_kernel = routines[0]

    if tl_kernel.is_program:
        raise NotImplementedError(
            "Generation of a test harness for a kernel defined as a Program "
            "(as opposed to a Subroutine) is not currently supported. (Found "
            "'{0}' which is a Program.)".format(tl_kernel.name))

    # First Container is a FileContainer and that's not what we want
    container = tl_psyir.walk(Container)[1]

    # Get the Container and Routine names from the PSyIR of the adjoint.
    adjoint_kernel_name = ad_psyir.walk(Routine)[0].name
    adjoint_module_name = ad_psyir.walk(Container)[1].name

    logger.debug("Creating test harness for TL kernel '%s' and AD kernel "
                 "'%s'", tl_kernel.name, adjoint_kernel_name)

    # Create a symbol for the TL kernel so that the harness code is able
    # to call it.
    csym = ContainerSymbol(container.name)
    symbol_table.add(csym)
    tl_kernel_sym = tl_kernel.symbol_table.lookup(tl_kernel.name).copy()
    tl_kernel_sym.interface = ImportInterface(csym)
    symbol_table.add(tl_kernel_sym)

    # Create a symbol for the adjoint kernel so that the harness code is able
    # to call it.
    adj_container = ContainerSymbol(adjoint_module_name)
    symbol_table.add(adj_container)
    adj_kernel_sym = symbol_table.new_symbol(
        adjoint_kernel_name, symbol_type=RoutineSymbol,
        interface=ImportInterface(adj_container))

    # Query the TL Kernel to find out the intrinsic type and precision of
    # the active variables. The test-harness code will use this when
    # computing inner products.
    datatype = _get_active_variables_datatype(tl_kernel, active_variables)

    # If the precision of the active variables is specified by another symbol
    # then we must ensure that it is declared in the harness too.
    if isinstance(datatype.precision, Symbol):
        if datatype.precision.is_local:
            symbol_table.add(datatype.precision.copy())
        elif datatype.precision.is_import:
            kind_contr = datatype.precision.interface.container_symbol.copy()
            symbol_table.add(kind_contr)
            kind_symbol = datatype.precision.copy()
            kind_symbol.interface = ImportInterface(kind_contr)
            symbol_table.add(kind_symbol)
        else:
            raise NotImplementedError(
                "The active variables {0} have a precision specified by "
                "symbol '{1}' which is not local or explicitly imported. This "
                "is not supported.".format(active_variables,
                                           datatype.precision.name))

    # Create a symbol to hold the extent of any test arrays. This is done here
    # to avoid any clashes with any of the container and kernel names.
    dim_size_sym = symbol_table.new_symbol("array_extent",
                                           symbol_type=DataSymbol,
                                           datatype=INTEGER_TYPE,
                                           constant_value=TEST_ARRAY_DIM_SIZE)

    # Create symbols for the results of the inner products
    inner1 = symbol_table.new_symbol("inner1", symbol_type=DataSymbol,
                                     datatype=datatype)
    inner2 = symbol_table.new_symbol("inner2", symbol_type=DataSymbol,
                                     datatype=datatype)

    # Identify any arguments to the kernel that are used to dimension other
    # arguments.
    integer_scalars = []
    for arg in tl_kernel.symbol_table.argument_datasymbols:
        if arg.is_scalar and (arg.datatype.intrinsic ==
                              ScalarType.Intrinsic.INTEGER):
            integer_scalars.append(arg)
    dimensioning_args = set()
    for arg in tl_kernel.symbol_table.argument_datasymbols:
        if arg.is_array:
            for dim in arg.shape:
                if isinstance(dim, ArrayType.ArrayBounds):
                    for bound in [dim.lower, dim.upper]:
                        for ref in bound.walk(Reference):
                            if ref.symbol in integer_scalars:
                                dimensioning_args.add(ref.symbol)

    logger.debug("Kernel '%s' has the following dimensioning arguments: "
                 "%s", tl_kernel.name,
                 [arg.name for arg in dimensioning_args])

    # Create local versions of these dimensioning variables in the test
    # program. Since they are dimensioning variables, they have to be given
    # a value. We create new symbols in order to ensure any name clashes
    # are handled and add them to a dict so we can map from the original
    # kernel arguments to the new symbols in the test harness.
    new_dim_args_map = {}
    for arg in dimensioning_args:
        new_dim_args_map[arg] = symbol_table.new_symbol(
            arg.name, symbol_type=DataSymbol,
            datatype=arg.datatype,
            constant_value=Reference(dim_size_sym))

    # Create necessary variables for the kernel arguments.
    inputs = []
    input_copies = []
    new_arg_list = []
    for arg in tl_kernel.symbol_table.argument_list:
        if arg in dimensioning_args:
            # This is a dimensioning argument - look up the test-harness
            # equivalent using the map we constructed earlier.
            new_arg_list.append(new_dim_args_map[arg])
            continue
        if arg.is_scalar:
            # The arguments will be local variables in the test program. We
            # use `new_symbol` to ensure there are no name clashes.
            new_sym = symbol_table.new_symbol(arg.name, symbol_type=DataSymbol,
                                              datatype=arg.datatype)
        else:
            # Since a Symbol's shape is immutable, we have to create a new
            # symbol in case the argument is of assumed size.
            new_shape = []
            for dim in arg.datatype.shape:
                if isinstance(dim, ArrayType.Extent):
                    # This dimension is assumed size so we have to give it
                    # an explicit size in the test program.
                    new_shape.append(Reference(dim_size_sym))
                elif isinstance(dim, ArrayType.ArrayBounds):
                    new_bounds = []
                    for bound in [dim.lower, dim.upper]:
                        if isinstance(bound, Reference):
                            if bound.symbol in new_dim_args_map:
                                new_bounds.append(
                                    Reference(new_dim_args_map[bound.symbol]))
                            else:
                                raise NotImplementedError(
                                    "Found argument '{0}' to kernel '{1}' "
                                    "which has a reference to '{2}' in its "
                                    "shape. However, '{2}' is not passed as an"
                                    " argument. This is not supported.".format(
                                        arg.name, tl_kernel.name,
                                        bound.symbol.name))
                        elif isinstance(bound, Literal):
                            new_bounds.append(bound.copy())
                        else:
                            raise NotImplementedError(
                                "Found argument '{0}' to kernel '{1}' which "
                                "has an array bound specified by a '{2}' node."
                                " Only Literals or References are supported.".
                                format(arg.name, tl_kernel.name,
                                       type(bound).__name__))
                    new_shape.append(ArrayType.ArrayBounds(new_bounds[0],
                                                           new_bounds[1]))
                else:
                    raise InternalError(
                        "Argument '{0}' to kernel '{1}' contains a '{2}' in "
                        "its shape definition but expected an ArrayType."
                        "Extent or ArrayType.ArrayBounds".format(
                            arg.name, tl_kernel.name, type(dim).__name__))
            new_sym = symbol_table.new_symbol(arg.name, symbol_type=DataSymbol,
                                              datatype=ArrayType(arg.datatype,
                                                                 new_shape))
        new_arg_list.append(new_sym)
        # Create variables to hold a copy of the inputs
        input_sym = symbol_table.new_symbol(new_sym.name+"_input",
                                            symbol_type=type(new_sym),
                                            datatype=new_sym.datatype)
        inputs.append(new_sym)
        input_copies.append(input_sym)

    logger.debug("Generated symbols for new argument list: %s",
                 [arg.name for arg in new_arg_list])

    statements = []
    # Initialise those variables and keep a copy of them.
    for sym, sym_copy in zip(inputs, input_copies):
        # The PSyIR doesn't support the random_number Fortran intrinsic so we
        # create a CodeBlock for it. Happily, the intrinsic will initialise
        # all elements of an array passed to it so we don't have to take any
        # special action.
        # TODO #1345 make this code language agnostic.
        ptree = Fortran2003.Call_Stmt(
            "call random_number({0})".format(sym.name))
        statements.append(CodeBlock([ptree], CodeBlock.Structure.STATEMENT))
        statements.append(
            Assignment.create(Reference(sym_copy), Reference(sym)))
    statements[0].preceding_comment = ("Initialise the kernel arguments and "
                                       "keep copies of them")

    # Call the TL kernel
    statements.append(Call.create(tl_kernel_sym,
                                  [Reference(sym) for sym in new_arg_list]))
    statements[-1].preceding_comment = "Call the tangent-linear kernel"

    # Compute the inner product of the result of the TL kernel
    stmt_list = _create_inner_product(inner1, [(sym, sym) for sym in inputs])
    stmt_list[0].preceding_comment = ("Compute the inner product of the "
                                      "results of the tangent-linear kernel")
    statements.extend(stmt_list)

    # Call the adjoint kernel using the outputs of the TL kernel as input
    statements.append(Call.create(adj_kernel_sym,
                                  [Reference(sym) for sym in new_arg_list]))
    statements[-1].preceding_comment = "Call the adjoint of the kernel"

    # Compute inner product of result of adjoint kernel with original inputs
    stmt_list = _create_inner_product(inner2, zip(inputs, input_copies))
    stmt_list[0].preceding_comment = (
        "Compute inner product of results of adjoint kernel with the "
        "original inputs to the tangent-linear kernel")
    statements.extend(stmt_list)

    # Compare the inner products.
    statements += _create_real_comparison(symbol_table, tl_kernel, inner1,
                                          inner2)

    # Finally, create the driver program from the list of statements.
    routine = Routine.create(
        "adj_test", symbol_table, statements, is_program=True)

    logger.debug("Created test-harness program named '%s'", routine.name)

    return routine


def _create_inner_product(result, symbol_pairs):
    '''
    Creates PSyIR that computes the inner product of each pair of symbols
    in the supplied list and accumulates it into the supplied `result`
    variable (which is first zeroed).

    :param result: symbol which will accumulate result.
    :type result: :py:class:`psyclone.psyir.symbols.DataSymbol`
    :param symbol_pairs: list of pairs of symbols for which to compute inner \
                         products.
    :type symbol_pairs: list of 2-tuples of \
                        :py:class:`psyclone.psyir.symbols.DataSymbol`

    :returns: PSyIR that performs the inner product and accumulates the \
        result in the variable represented by the `result` Symbol.
    :rtype: list of :py:class:`psyclone.psyir.nodes.Assignment`

    :raises TypeError: if any pair of symbols represent different datatypes.

    '''
    # Zero the variable used to accumulate the result
    statements = [Assignment.create(Reference(result),
                                    Literal("0.0", result.datatype))]

    # Now generate code to compute the inner product of each pair of symbols
    for (sym1, sym2) in symbol_pairs:

        if sym1.is_scalar:

            if sym1.datatype != sym2.datatype:
                raise TypeError(
                    "Cannot compute inner product of Symbols '{0}' and '{1}' "
                    "because they represent different datatypes ({2} and {3}, "
                    "respectively).".format(
                        sym1.name, sym2.name, str(sym1.datatype),
                        str(sym2.datatype)))

            prod = BinaryOperation.create(BinaryOperation.Operator.MUL,
                                          Reference(sym1), Reference(sym2))
            statements.append(
                Assignment.create(
                    Reference(result),
                    BinaryOperation.create(BinaryOperation.Operator.ADD,
                                           Reference(result), prod)))
        else:

            statements.append(_create_array_inner_product(result, sym1, sym2))

    return statements


def _create_array_inner_product(result, array1, array2):
    '''
    Generates the PSyIR for the innerproduct of array1 and array2 with
    accumulation into the 'result' symbol.

    :param result: symbol which will accumulate result.
    :type result: :py:class:`psyclone.psyir.symbols.DataSymbol`
    :param array1: symbol representing first array.
    :type array1: :py:class:`psyclone.psyir.symbols.DataSymbol`
    :param array2: symbol representing second array.
    :type array2: :py:class:`psyclone.psyir.symbols.DataSymbol`

    :raises TypeError: if the array1 and array2 symbols have different
                       datatypes or are not of ArrayType.
    '''
    if array1.datatype != array2.datatype:
        raise TypeError(
            "Cannot compute inner product of Symbols '{0}' and '{1}' "
            "because they represent different datatypes ({2} and {3}, "
            "respectively).".format(
                array1.name, array2.name, str(array1.datatype),
                str(array2.datatype)))

    if not isinstance(array1.datatype, ArrayType):
        raise TypeError(
            "Supplied Symbols must represent arrays but got '{0}' for '{1}'.".
            format(array1.datatype, array1.name))

    if len(array1.datatype.shape) == 1:
        # PSyIR does not support the DOT_PRODUCT (Fortran) intrinsic
        # so we create a CodeBlock.
        ptree = Fortran2003.Expr("DOT_PRODUCT({0}, {1})".format(
            array1.name, array2.name))
        cblock = CodeBlock([ptree], CodeBlock.Structure.EXPRESSION)

        return Assignment.create(
            Reference(result),
            BinaryOperation.create(BinaryOperation.Operator.ADD,
                                   Reference(result), cblock))

    # Create a matrix inner product
    ranges1 = []
    ranges2 = []
    # Generate a Range object for each dimension of each array
    for idx in range(len(array1.datatype.shape)):
        idx_literal = Literal(str(idx+1), INTEGER_TYPE)
        lbound1 = BinaryOperation.create(BinaryOperation.Operator.LBOUND,
                                         Reference(array1),
                                         idx_literal.copy())
        ubound1 = BinaryOperation.create(BinaryOperation.Operator.UBOUND,
                                         Reference(array1),
                                         idx_literal.copy())
        ranges1.append(Range.create(lbound1, ubound1))

        lbound2 = BinaryOperation.create(BinaryOperation.Operator.LBOUND,
                                         Reference(array2),
                                         idx_literal.copy())
        ubound2 = BinaryOperation.create(BinaryOperation.Operator.UBOUND,
                                         Reference(array2),
                                         idx_literal.copy())
        ranges2.append(Range.create(lbound2, ubound2))

    # Use these Ranges to create references for all elements of both arrays
    ref1 = ArrayReference.create(array1, ranges1)
    ref2 = ArrayReference.create(array2, ranges2)
    # Element-wise product of the arrays
    prod = BinaryOperation.create(BinaryOperation.Operator.MUL,
                                  ref1, ref2)
    # Sum the resulting elements
    inner = UnaryOperation.create(UnaryOperation.Operator.SUM, prod)
    # Accumulate the result
    return Assignment.create(
        Reference(result),
        BinaryOperation.create(BinaryOperation.Operator.ADD,
                               Reference(result), inner))


def _create_real_comparison(sym_table, kernel, var1, var2):
    '''
    Creates PSyIR that checks the values held by Symbols var1 and var2 for
    equality, allowing for machine precision.

    :param sym_table: the SymbolTable in which to put new Symbols.
    :type sym_table: :py:class:`psyclone.psyir.symbols.SymbolTable`
    :param kernel: the routine for which this adjoint test is being performed.
    :type kernel: :py:class:`psyclone.psyir.nodes.Routine`
    :param var1: the symbol holding the first value for the comparison.
    :type var1: :py:class:`psyclone.psyir.symbols.DataSymbol`
    :param var2: the symbol holding the second value for the comparison.
    :type var2: :py:class:`psyclone.psyir.symbols.DataSymbol`

    :returns: the PSyIR nodes that perform the check.
    :rtype: list of :py:class:`psyclone.psyir.nodes.Node`

    '''
    statements = []
    mtol = sym_table.new_symbol("MachineTol", symbol_type=DataSymbol,
                                datatype=var1.datatype)
    rel_diff = sym_table.new_symbol("relative_diff", symbol_type=DataSymbol,
                                    datatype=var1.datatype)
    overall_tol = sym_table.new_symbol("overall_tolerance",
                                       symbol_type=DataSymbol,
                                       datatype=var1.datatype,
                                       constant_value=INNER_PRODUCT_TOLERANCE)
    # TODO #1161 - the PSyIR does not support `SPACING`
    ptree = Fortran2003.Assignment_Stmt(
        "MachineTol = SPACING ( MAX( ABS({0}), ABS({1}) ) )".format(var1.name,
                                                                    var2.name))
    statements.append(CodeBlock([ptree], CodeBlock.Structure.STATEMENT))
    statements[-1].preceding_comment = (
        "Test the inner-product values for equality, allowing for the "
        "precision of the active variables")
    sub_op = BinaryOperation.create(BinaryOperation.Operator.SUB,
                                    Reference(var1), Reference(var2))
    abs_op = UnaryOperation.create(UnaryOperation.Operator.ABS, sub_op)
    div_op = BinaryOperation.create(BinaryOperation.Operator.DIV,
                                    abs_op, Reference(mtol))
    statements.append(Assignment.create(Reference(rel_diff), div_op))

    # TODO #1345 make this code language agnostic.
    ptree1 = Fortran2003.Write_Stmt(
        "write(*,*) 'Test of adjoint of ''{0}'' PASSED: ', {1}, {2}, {3}"
        .format(kernel.name, var1.name, var2.name, rel_diff.name))
    ptree2 = Fortran2003.Write_Stmt(
        "write(*,*) 'Test of adjoint of ''{0}'' FAILED: ', {1}, {2}, {3}"
        .format(kernel.name, var1.name, var2.name, rel_diff.name))

    statements.append(
        IfBlock.create(BinaryOperation.create(BinaryOperation.Operator.LT,
                                              Reference(rel_diff),
                                              Reference(overall_tol)),
                       [CodeBlock([ptree1], CodeBlock.Structure.STATEMENT)],
                       [CodeBlock([ptree2], CodeBlock.Structure.STATEMENT)]))

    return statements


# =============================================================================
# Documentation utils: The list of module members that we wish AutoAPI to
# generate documentation for (see https://psyclone-ref.readthedocs.io).
__all__ = ["generate_adjoint_str", "generate_adjoint", "generate_adjoint_test"]
