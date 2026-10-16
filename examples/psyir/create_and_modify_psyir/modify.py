# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2021-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

'''A simple Python script showing how to modify a PSyIR tree. In order to use
it you must first install PSyclone. See README.md in the top-level psyclone
directory.

Once you have psyclone installed, this script may be run by doing:

>>> python modify.py

This will first create a tree as specified in the create.py file and
then proceed to modify the tree and generate the modified code Fortran
representation.

'''
# Different pylint configurations don't agree in the order of this imports
# pylint: disable=wrong-import-order
from psyclone.psyir.backend.fortran import FortranWriter
from psyclone.psyir.symbols import Symbol, RoutineSymbol, NoType, ScalarType
from psyclone.psyir.nodes import Reference, Literal
from create import create_psyir_tree


def modify_psyir_tree():
    ''' Apply modifications to the PSyIR tree created in create.py

    :returns: a modified PSyIR tree.
    :rtype: :py:class:`psyclone.psyir.nodes.Container`

    '''
    file_container = create_psyir_tree()
    container = file_container.children[0]
    subroutine = container.children[0]

    # Rename one of the subroutine local symbols.
    tmp_symbol = subroutine.symbol_table.lookup("psyir_tmp_1")
    subroutine.symbol_table.rename_symbol(tmp_symbol, "new_variable")

    # The type of a symbol might be unknown
    symbol = Symbol("unused")
    container.symbol_table.add(symbol)
    # later its type could be determined. However, we don't want to
    # replace the existing symbol instance with a new instance as it
    # may have references, which could then lead to inconsistencies. Therefore
    # we support the `specialise` method, which transforms the existing
    # node type to a subclass of type without changing the memory
    # location of the instance. Note, any additional subclass properties would
    # have to be added manually. In this case we have to set `datatype`.
    symbol.specialise(RoutineSymbol, datatype=NoType())

    # In some cases we may want to replace one node with another. This
    # can be simply done using a node's `replace_with` method.
    assignment = subroutine.children[2]
    assignment_rhs = assignment.rhs
    assignment_rhs.replace_with(Reference(tmp_symbol))

    # By default `replace_with` will conserve a node name in its context, but
    # this can be disabled with the `keep_name_in_context` parameter.
    dot_product_1st_arg = subroutine[5].rhs.arguments[0]
    dot_product_1st_arg.replace_with(
        Literal('2', ScalarType.integer_type()),
        keep_name_in_context=False)

    return file_container


if __name__ == "__main__":
    psyir_tree = modify_psyir_tree()

    # Write out the modified code as Fortran.
    writer = FortranWriter()
    result = writer(psyir_tree)
    print(result)
