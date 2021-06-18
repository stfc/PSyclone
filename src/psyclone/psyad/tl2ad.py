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
from psyclone.psyir.frontend.fortran import FortranReader
from psyclone.psyir.backend.fortran import FortranWriter
from psyclone.psyir.nodes import Routine, Assignment, Reference, Literal, \
    Call, Container, BinaryOperation, UnaryOperation, Return, IfBlock
from psyclone.psyir.symbols import SymbolTable, LocalInterface, \
    GlobalInterface, REAL_TYPE, REAL_DOUBLE_TYPE, ContainerSymbol, \
    RoutineSymbol, DataSymbol


def generate_adjoint_str(tl_fortran_str):
    '''Takes an LFRic tangent-linear kernel encoded as a string as input
    and returns its adjoint encoded as a string.

    :param str tl_fortran_str: Fortran implementation of an LFRic \
        tangent-linear kernel.

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

    return adjoint_fortran_str


def generate_adjoint(tl_psyir):
    '''Takes an LFRic tangent-linear kernel represented in language-level PSyIR
    and returns its adjoint represented in language-level PSyIR.

    :param tl_psyir: language-level PSyIR containing the LFRic \
        tangent-linear kernel.
    :type tl_psyir: :py:class:`psyclone.psyir.Node`

    :returns: language-level PSyIR containing the adjoint of the \
        supplied tangent-linear kernel.
    :rtype: :py:class:`psyclone.psyir.Node`

    '''
    logger = logging.getLogger(__name__)

    # TL LFRic-specific PSyIR
    logger.debug(
        "Translation from generic PSyIR to LFRic-specific PSyIR should be "
        "done now.")

    # Transform from TL to AD
    logger.debug("Transformation from TL to AD should be done now.")
    ad_psyir = tl_psyir

    return ad_psyir


def generate_adjoint_test_str(tl_fortran_str):
    ''' '''
    # TODO the information on the active variables will have to be
    # extracted from markup in the TL kernel source.
    active_variables = ['field']
    # TODO not yet decided how the name of the adjoint kernel and associated
    # module are made available to this routine.
    adjoint_kernel_name = 'testkern_adj_code'
    adjoint_module_name = 'testkern_adj_mod'

    # Create the language-level PSyIR for the TL kernel.
    # TODO this duplicates the work done in generate_adjoint_str.
    reader = FortranReader()
    tl_psyir = reader.psyir_from_source(tl_fortran_str)
    # First Container is a FileContainer and that's not what we want
    container = tl_psyir.walk(Container)[1]

    routines = tl_psyir.walk(Routine)

    if len(routines) != 1:
        raise NotImplementedError(
            "The supplied Fortran must contain one and only one Subroutine")

    tl_kernel = routines[0]

    symbol_table = SymbolTable()

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

    # Create necessary variables for the kernel arguments.
    inputs = []
    input_copies = []
    for arg in tl_kernel.symbol_table.argument_datasymbols:
        new_sym = arg.copy()
        # The arguments will be local variables in the test program
        new_sym.interface = LocalInterface()
        symbol_table.add(new_sym)
        # Create variables to hold a copy of the inputs
        input_sym = symbol_table.new_symbol(new_sym.name+"_input",
                                            symbol_type=type(new_sym),
                                            datatype=new_sym.datatype)
        inputs.append(new_sym)
        input_copies.append(input_sym)

    # Create additional variables for arguments to the adjoint kernel
    # (using markup from the TL kernel to identify active arguments).
    for var in active_variables:
        input_sym = symbol_table.lookup(var)
        symbol_table.new_symbol(var+"_out", symbol_type=type(input_sym),
                                datatype=input_sym.datatype)

    statements = []
    # Initialise those variables and keep a copy of them
    for sym, sym_copy in zip(inputs, input_copies):
        statements.append(
            Assignment.create(Reference(sym), Literal("0.5", REAL_TYPE)))
        statements.append(
            Assignment.create(Reference(sym_copy), Reference(sym)))

    # Call the kernel for a single cell column
    statements.append(Call.create(tl_kernel_sym,
                                  [Reference(sym) for sym in inputs]))

    # Compute the inner product of the result of the TL kernel
    statements += create_inner_product(inner1,
                                       [(sym, sym) for sym in inputs])

    # Call the adjoint kernel using the outputs of the TL kernel as input
    statements.append(Call.create(adj_kernel_sym,
                                  [Reference(sym) for sym in inputs]))

    # Compute inner product of result of adjoint kernel with original inputs
    statements += create_inner_product(inner2,
                                       zip(inputs, input_copies))

    # Compare the inner products
    tol_zero = Literal("1.0e-10", REAL_DOUBLE_TYPE)

    diff = BinaryOperation.create(BinaryOperation.Operator.SUB,
                                  Reference(inner1), Reference(inner2))
    abs_diff = UnaryOperation.create(UnaryOperation.Operator.ABS, diff)
    # TODO how do we record failure?
    statements.append(
        IfBlock.create(BinaryOperation.create(BinaryOperation.Operator.GT,
                                              abs_diff, tol_zero.copy()),
                       [Return()]))

    # Create driver program
    routine = Routine.create(
        "adj_test", symbol_table, statements, is_program=True)

    writer = FortranWriter()
    return writer(routine)


def create_inner_product(result, symbol_pairs):
    ''' '''

    zero = Literal("0.0", REAL_DOUBLE_TYPE)

    statements = [Assignment.create(Reference(result), zero.copy())]
    for (sym1, sym2) in symbol_pairs:

        prod = BinaryOperation.create(BinaryOperation.Operator.MUL,
                                      Reference(sym1), Reference(sym2))
        statements.append(
            Assignment.create(
                Reference(result),
                BinaryOperation.create(BinaryOperation.Operator.ADD,
                                       Reference(result), prod)))
    return statements


__all__ = ["generate_adjoint_str", "generate_adjoint",
           "generate_adjoint_test_str"]
