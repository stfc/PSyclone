# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2021, Science and Technology Facilities Council.
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
from psyclone.psyir.frontend.fortran import FortranReader
from psyclone.psyir.backend.fortran import FortranWriter
from psyclone.psyir.nodes import Routine, Assignment, Reference, Literal, \
    Call, Container, BinaryOperation, UnaryOperation, Return, IfBlock, \
    CodeBlock, FileContainer, ArrayReference, Range
from psyclone.psyir.symbols import SymbolTable, LocalInterface, \
    GlobalInterface, ContainerSymbol, ScalarType, ArrayType, \
    RoutineSymbol, DataSymbol, INTEGER_TYPE, REAL_DOUBLE_TYPE


def generate_adjoint_str(tl_fortran_str, create_test=False):
    '''Takes an LFRic tangent-linear kernel encoded as a string as input
    and returns its adjoint encoded as a string.

    :param str tl_fortran_str: Fortran implementation of an LFRic \
        tangent-linear kernel.
    :param bool create_test: whether or not to create test code for the \
        adjoint kernel.

    :returns: a string containing the Fortran implementation of the \
        supplied tangent-linear kernel.
    :rtype: str

    '''
    logger = logging.getLogger(__name__)
    logger.debug(tl_fortran_str)

    # TL Language-level PSyIR
    reader = FortranReader()
    tl_psyir = reader.psyir_from_source(tl_fortran_str)

    # Addressing issue #1238 will allow the view() method to be output
    # to the logger.
    # logger.debug(tl_psyir.view())

    # TL to AD translation
    ad_psyir = generate_adjoint(tl_psyir)

    # AD Fortran code
    writer = FortranWriter()
    adjoint_fortran_str = writer(ad_psyir)
    logger.debug(adjoint_fortran_str)

    # Create test harness if requested
    test_fortran_str = ""
    if create_test:
        test_psyir = generate_adjoint_test(tl_psyir, ad_psyir)
        test_fortran_str = writer(test_psyir)

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
    :raises NotImplementedError: if there are more than two Containers.

    '''
    containers = psyir.walk(Container)
    if not containers:
        return None

    if len(containers) == 1:
        if isinstance(containers[0], FileContainer):
            return None
        return containers[0]

    elif len(containers) == 2:
        if isinstance(containers[1], FileContainer):
            raise InternalError(
                "The supplied PSyIR contains two Containers but the innermost "
                "is a FileContainer. This should not be possible.")
        return containers[1]

    raise NotImplementedError("The supplied PSyIR contains more than two "
                              "Containers. This is not supported.")


def generate_adjoint(tl_psyir):
    '''Takes an LFRic tangent-linear kernel represented in language-level PSyIR
    and returns its adjoint represented in language-level PSyIR.

    Currently just takes a copy of the supplied PSyIR and re-names the
    Container and Routine.

    :param tl_psyir: language-level PSyIR containing the LFRic \
        tangent-linear kernel.
    :type tl_psyir: :py:class:`psyclone.psyir.Node`

    :returns: language-level PSyIR containing the adjoint of the \
        supplied tangent-linear kernel.
    :rtype: :py:class:`psyclone.psyir.Node`

    :raises InternalError: if the PSyIR does not contain any Routines.
    :raises NotImplementedError: if the PSyIR contains >1 Routine.

    '''
    logger = logging.getLogger(__name__)
    name_suffix = "_adj"

    # TL LFRic-specific PSyIR
    logger.debug(
        "Translation from generic PSyIR to LFRic-specific PSyIR should be "
        "done now.")

    # Transform from TL to AD
    logger.debug("Transformation from TL to AD should be done now.")
    ad_psyir = tl_psyir.copy()

    # We permit the input code to be a single Program or Subroutine
    container = _find_container(ad_psyir)
    if container:
        # Re-name the Container for the adjoint code
        container.name = container.name + name_suffix

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
        adj_kernel_name = routine.name + name_suffix
        # A symbol's name is immutable so create a new RoutineSymbol
        adj_kernel_sym = container.symbol_table.new_symbol(
            adj_kernel_name, symbol_type=RoutineSymbol,
            visibility=kernel_sym.visibility)
        container.symbol_table.remove(kernel_sym)
        routine.name = adj_kernel_sym.name
    else:
        routine.name = routine.name + name_suffix

    return ad_psyir


def generate_adjoint_test(tl_psyir, ad_psyir):
    '''
    Creates the PSyIR of a test harness for the supplied TL and adjoint
    kernels.

    :param tl_psyir: PSyIR of the tangent-linear kernel code.
    :type tl_psyir: :py:class:`psyclone.psyir.Container`
    :param ad_psyir: PSyIR of the adjoint kernel code.
    :type ad_psyir: :py:class:`psyclone.psyir.Container`

    :returns: the PSyIR of the test harness.
    :rtype: :py:class:`psyclone.psyir.Routine`

    :raises NotImplementedError: if the supplied PSyIR contains more than \
        one Routine.
    :raises NotImplementedError: if the supplied TL/Adjoint PSyIR contains \
        just a Routine that is a Program (since this would have to \
        be converted to a subroutine in order to construct the test harness).

    '''
    symbol_table = SymbolTable()

    # TODO #1331 provide some way of configuring the extent of the test arrays
    array_dim_size = 20
    # Create a symbol to hold this value.
    dim_size_sym = symbol_table.new_symbol("array_extent",
                                           symbol_type=DataSymbol,
                                           datatype=INTEGER_TYPE,
                                           constant_value=array_dim_size)

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

    # Create a symbol for the TL kernel
    csym = ContainerSymbol(container.name)
    symbol_table.add(csym)
    tl_kernel_sym = tl_kernel.symbol_table.lookup(tl_kernel.name).copy()
    tl_kernel_sym.interface = GlobalInterface(csym)
    symbol_table.add(tl_kernel_sym)

    # Create a symbol for the adjoint kernel
    adj_container = ContainerSymbol(adjoint_module_name)
    symbol_table.add(adj_container)
    adj_kernel_sym = symbol_table.new_symbol(
        adjoint_kernel_name, symbol_type=RoutineSymbol,
        interface=GlobalInterface(adj_container))

    # Create symbols for the results of the inner products
    inner1 = symbol_table.new_symbol("inner1", symbol_type=DataSymbol,
                                     datatype=REAL_DOUBLE_TYPE)
    inner2 = symbol_table.new_symbol("inner2", symbol_type=DataSymbol,
                                     datatype=REAL_DOUBLE_TYPE)
    # Create symbol for result of the diff of the inner products
    diff_sym = symbol_table.new_symbol("abs_diff", symbol_type=DataSymbol,
                                       datatype=REAL_DOUBLE_TYPE)

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
                if not isinstance(dim, ArrayType.Extent):
                    for ref in dim.walk(Reference):
                        if ref.symbol in integer_scalars:
                            dimensioning_args.add(ref.symbol)

    # Create local versions of these dimensioning variables in the test
    # program.
    for arg in dimensioning_args:
        new_sym = arg.copy()
        # The arguments will be local variables in the test program
        new_sym.interface = LocalInterface()
        # Since they are dimensioning variables, they have to be given a value
        new_sym.constant_value = array_dim_size
        symbol_table.add(new_sym)

    # Create necessary variables for the kernel arguments.
    inputs = []
    input_copies = []
    new_arg_list = []
    for arg in tl_kernel.symbol_table.argument_list:
        if arg in dimensioning_args:
            new_arg_list.append(symbol_table.lookup(arg.name))
            continue
        if arg.is_scalar:
            new_sym = arg.copy()
            # The arguments will be local variables in the test program
            new_sym.interface = LocalInterface()
        else:
            # Since a Symbol's shape is immutable, we have to create a new
            # symbol in case the argument is of assumed size.
            new_shape = []
            for dim in arg.datatype.shape:
                if isinstance(dim, ArrayType.Extent):
                    # This dimension is assumed size so we have to give it
                    # an explicit size in the test program.
                    new_shape.append(Reference(dim_size_sym))
                else:
                    new_shape.append(dim)
            new_sym = DataSymbol(arg.name,
                                 ArrayType(arg.datatype, new_shape))
        symbol_table.add(new_sym)
        new_arg_list.append(new_sym)
        # Create variables to hold a copy of the inputs
        input_sym = symbol_table.new_symbol(new_sym.name+"_input",
                                            symbol_type=type(new_sym),
                                            datatype=new_sym.datatype)
        inputs.append(new_sym)
        input_copies.append(input_sym)

    statements = []
    # Initialise those variables and keep a copy of them.
    # TODO #1247 we need to add comments to the generated code!
    for sym, sym_copy in zip(inputs, input_copies):
        # The PSyIR doesn't support the random_number Fortran intrinsic so we
        # create a CodeBlock for it. Happily, the intrinsic will initialise
        # all elements of an array passed to it so we don't have to take any
        # special action.
        ptree = Fortran2003.Call_Stmt(
            "call random_number({0})".format(sym.name))
        statements.append(CodeBlock([ptree], CodeBlock.Structure.STATEMENT))
        statements.append(
            Assignment.create(Reference(sym_copy), Reference(sym)))

    # Call the kernel for a single cell column
    statements.append(Call.create(tl_kernel_sym,
                                  [Reference(sym) for sym in new_arg_list]))

    # Compute the inner product of the result of the TL kernel
    statements += _create_inner_product(inner1,
                                        [(sym, sym) for sym in inputs])

    # Call the adjoint kernel using the outputs of the TL kernel as input
    statements.append(Call.create(adj_kernel_sym,
                                  [Reference(sym) for sym in new_arg_list]))

    # Compute inner product of result of adjoint kernel with original inputs
    statements += _create_inner_product(inner2,
                                        zip(inputs, input_copies))

    # Compare the inner products
    tol_zero = Literal("1.0e-10", REAL_DOUBLE_TYPE)

    diff = BinaryOperation.create(BinaryOperation.Operator.SUB,
                                  Reference(inner1), Reference(inner2))
    abs_diff = UnaryOperation.create(UnaryOperation.Operator.ABS, diff)
    statements.append(Assignment.create(Reference(diff_sym), abs_diff))

    # If the test fails then the harness will print a message and return early.
    ptree = Fortran2003.Write_Stmt(
        "write(*,*) 'Test of adjoint of ''{0}'' failed: diff = ', {1}".format(
            tl_kernel.name, diff_sym.name))

    statements.append(
        IfBlock.create(BinaryOperation.create(BinaryOperation.Operator.GT,
                                              Reference(diff_sym),
                                              tol_zero.copy()),
                       [CodeBlock([ptree], CodeBlock.Structure.STATEMENT),
                        Return()]))

    # Otherwise the harness prints a message reporting that all is well.
    ptree = Fortran2003.Write_Stmt(
        "write(*,*) 'Test of adjoint of ''{0}'' passed: diff = ', {1}".format(
            tl_kernel.name, diff_sym.name))
    statements.append(CodeBlock([ptree], CodeBlock.Structure.STATEMENT))

    # Finally, create driver program from the list of statements.
    routine = Routine.create(
        "adj_test", symbol_table, statements, is_program=True)

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

    :returns: PSyIR that performs the inner product and accumulates the result\
        in the variable represented by the `result` Symbol.
    :rtype: list of :py:class:`psyclone.psyir.nodes.Assignment`

    :raises TypeError: if any pair of symbols represent different datatypes.

    '''
    # Zero the variable used to accumulate the result
    statements = [Assignment.create(Reference(result),
                                    Literal("0.0", REAL_DOUBLE_TYPE))]

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
    else:
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


__all__ = ["generate_adjoint_str", "generate_adjoint", "generate_adjoint_test"]
