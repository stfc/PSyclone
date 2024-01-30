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
# Authors R. W. Ford, A. R. Porter and S. Siso, STFC Daresbury Lab

''' Provides various utilities in support of the PSyAD adjoint
    functionality. '''

from psyclone.errors import InternalError
from psyclone.psyir.frontend.fortran import FortranReader
from psyclone.psyir.nodes import (IntrinsicCall, BinaryOperation,
                                  Reference, Assignment, IfBlock,
                                  Container, FileContainer, Node)
from psyclone.psyir.symbols import DataSymbol


#: The prefix we will prepend to routine, container and metadata
#: names when generating the adjoint. If the original name contains
#: the tl prefix, then this is removed.
ADJOINT_NAME_PREFIX = "adj_"
TL_NAME_PREFIX = "tl_"
#: The tolerance applied to the comparison of the inner product values in
#: the generated test-harness code.
#: TODO #1346 this tolerance should be user configurable.
INNER_PRODUCT_TOLERANCE = 1500.0


def create_adjoint_name(tl_name):
    '''Create an adjoint name from the supplied tangent linear name. This
    is done by stripping the TL_NAME_PREFIX from the name if it exists
    and then adding the ADJOINT_NAME_PREFIX. The adjoint name is also
    lower-cased.

    :param str: the tangent-linear name.

    :returns: the adjoint name.
    :rtype: str

    '''
    adj_name = tl_name.lower()
    if adj_name.startswith(TL_NAME_PREFIX):
        adj_name = adj_name[len(TL_NAME_PREFIX):]
    return ADJOINT_NAME_PREFIX + adj_name


def create_real_comparison(sym_table, kernel, var1, var2):
    '''Creates PSyIR that checks the values held by Symbols var1 and var2
    for equality, allowing for machine precision and writes the
    success or failure of the checks to stdout.

    :param sym_table: the SymbolTable in which to put new Symbols.
    :type sym_table: :py:class:`psyclone.psyir.symbols.SymbolTable`
    :param kernel: the routine for which this adjoint test is being performed.
    :type kernel: :py:class:`psyclone.psyir.nodes.Routine`
    :param var1: the symbol holding the first value for the comparison.
    :type var1: :py:class:`psyclone.psyir.symbols.DataSymbol`
    :param var2: the symbol holding the second value for the comparison.
    :type var2: :py:class:`psyclone.psyir.symbols.DataSymbol`

    :returns: the PSyIR nodes that perform the check and write its
        success or failure to stdout.
    :rtype: List[:py:class:`psyclone.psyir.nodes.Node`]

    '''
    statements = []
    statements.extend(common_real_comparison(sym_table, var1, var2))
    statements.extend(_common_write(sym_table, kernel, var1, var2))
    return statements


def common_real_comparison(sym_table, var1, var2):
    '''
    Creates PSyIR that checks the values held by Symbols var1 and var2 for
    equality, allowing for machine precision.

    :param sym_table: the SymbolTable in which to put new Symbols.
    :type sym_table: :py:class:`psyclone.psyir.symbols.SymbolTable`
    :param var1: the symbol holding the first value for the comparison.
    :type var1: :py:class:`psyclone.psyir.symbols.DataSymbol`
    :param var2: the symbol holding the second value for the comparison.
    :type var2: :py:class:`psyclone.psyir.symbols.DataSymbol`

    :returns: the PSyIR nodes that perform the check.
    :rtype: List[:py:class:`psyclone.psyir.nodes.Node`]

    '''
    freader = FortranReader()
    statements = []
    mtol = sym_table.new_symbol("MachineTol", symbol_type=DataSymbol,
                                datatype=var1.datatype)
    rel_diff = sym_table.new_symbol(
        "relative_diff", symbol_type=DataSymbol, datatype=var1.datatype,
        tag="relative_diff")
    # The "overall_tolerance" tag is used to get the symbol from the
    # symbol table in subsequent routines even though the symbol
    # itself is not used in this routine.
    _ = sym_table.new_symbol("overall_tolerance",
                             tag="overall_tolerance",
                             symbol_type=DataSymbol,
                             datatype=var1.datatype,
                             is_constant=True,
                             initial_value=INNER_PRODUCT_TOLERANCE)
    assign = freader.psyir_from_statement(
        f"MachineTol = SPACING ( MAX( ABS({var1.name}), ABS({var2.name}) ) )",
        sym_table)
    statements.append(assign)
    statements[-1].preceding_comment = (
        "Test the inner-product values for equality, allowing for the "
        "precision of the active variables")
    sub_op = BinaryOperation.create(BinaryOperation.Operator.SUB,
                                    Reference(var1), Reference(var2))
    abs_op = IntrinsicCall.create(IntrinsicCall.Intrinsic.ABS, [sub_op])
    div_op = BinaryOperation.create(BinaryOperation.Operator.DIV,
                                    abs_op, Reference(mtol))
    statements.append(Assignment.create(Reference(rel_diff), div_op))

    return statements


def _common_write(sym_table, kernel, var1, var2):
    '''Creates PSyIR that writes whether the precision test passed or
    failed to stdout.

    :param sym_table: the SymbolTable in which to read existing Symbols.
    :type sym_table: :py:class:`psyclone.psyir.symbols.SymbolTable`
    :param kernel: the routine for which this adjoint test is being performed.
    :type kernel: :py:class:`psyclone.psyir.nodes.Routine`
    :param var1: the symbol holding the first value for the comparison.
    :type var1: :py:class:`psyclone.psyir.symbols.DataSymbol`
    :param var2: the symbol holding the second value for the comparison.
    :type var2: :py:class:`psyclone.psyir.symbols.DataSymbol`

    :returns: PSyIR nodes that write out test success or failure to stdout.
    :rtype: List[:py:class:`psyclone.psyir.nodes.Node`]

    '''
    # TODO #1345 make this code language agnostic.
    freader = FortranReader()
    statements = []
    rel_diff = sym_table.lookup_with_tag("relative_diff")
    overall_tol = sym_table.lookup_with_tag("overall_tolerance")
    write1 = freader.psyir_from_statement(
        f"write(*,*) 'Test of adjoint of ''{kernel.name}'' PASSED: ', "
        f"{var1.name}, {var2.name}, {rel_diff.name}", sym_table)
    write2 = freader.psyir_from_statement(
        f"write(*,*) 'Test of adjoint of ''{kernel.name}'' FAILED: ', "
        f"{var1.name}, {var2.name}, {rel_diff.name}", sym_table)

    statements.append(
        IfBlock.create(BinaryOperation.create(BinaryOperation.Operator.LT,
                                              Reference(rel_diff),
                                              Reference(overall_tol)),
                       [write1], [write2]))

    return statements


def find_container(psyir):
    ''' Finds the first Container in the supplied PSyIR that is not a
    FileContainer. Also validates that the PSyIR contains at most one
    FileContainer which, if present, contains a Container.

    :param psyir: the PSyIR to search for a Container.
    :type psyir: :py:class:`psyclone.psyir.nodes.Node`

    :returns: the first Container that is not a FileContainer or None if \
              there is none.
    :rtype: :py:class:`psyclone.psyir.nodes.Container` or NoneType

    :raises InternalError: if there are two Containers and the second is a \
                           FileContainer.
    :raises NotImplementedError: if there are two Containers and the first is \
                                 not a FileContainer.
    :raises NotImplementedError: if there are more than two Containers.

    '''
    if not isinstance(psyir, Node):
        raise TypeError(
            f"Expected a PSyIR Node but got '{type(psyir).__name__}'")

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


__all__ = ["create_adjoint_name", "create_real_comparison", "_common_write",
           "find_container"]
