# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

'''
Module containing tests for the parallel region transformation class.

'''

import logging
import pytest
from psyclone.psyir.transformations.transformation_error import (
    TransformationError)
from psyclone.psyir.nodes import (CodeBlock, Literal, Loop)
from psyclone.psyir.transformations import OMPParallelTrans
from psyclone.psyir.symbols import DataSymbol, ScalarType


def test_parallelregion_refuse_codeblock():
    ''' Check that ParallelRegionTrans.validate() rejects a loop nest that
    encloses a CodeBlock. We use OMPParallelTrans as ParallelRegionTrans
    is abstract. '''
    otrans = OMPParallelTrans()
    # Construct a valid Loop in the PSyIR with a CodeBlock in its body
    parent = Loop.create(DataSymbol("ji", ScalarType.integer_type()),
                         Literal("1", ScalarType.integer_type()),
                         Literal("10", ScalarType.integer_type()),
                         Literal("1", ScalarType.integer_type()),
                         [CodeBlock([], CodeBlock.Structure.STATEMENT,
                                    None)])
    with pytest.raises(TransformationError) as err:
        otrans.validate([parent])
    assert ("Nodes of type 'CodeBlock' cannot be enclosed by a "
            "OMPParallelTrans transformation" in str(err.value))


def test_parallelregion_check_symtab_var(fortran_reader, caplog):
    '''
    Check ParallelRegionTrans._check_symbol_table_vars try and except,
    if the logging message produces a warning when a variable is not
    in the routine scope.We use OMPParallelTrans as ParallelRegionTrans
    is abstract.
    '''
    otrans = OMPParallelTrans()
    code = """subroutine test
    integer :: i
    do i = 1, 100

    end do
    end subroutine"""
    psyir = fortran_reader.psyir_from_source(code)
    otrans.apply(psyir.children[0].children[0])
    parallel = psyir.children[0].children[0]
    caplog.clear()
    with caplog.at_level(logging.WARNING,
                         logger="psyclone.psyir.transformations"):
        otrans._check_symbol_table_vars(parallel, ("j"))
    long_string = (
        "Error: \"Could not find 'j' in the Symbol Table.\" This has been "
        "provided with the 'j' in the 'force_private' option, "
        "but there is no such symbol in this scope."
    )
    assert long_string in caplog.text
