# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2021, Science and Technology Facilities Council
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
# Author: S. Siso, STFC Daresbury Lab
# Modified: R. W. Ford, STFC Daresbury Lab
#           A. R. Porter, STFC Daresbury Lab

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
from psyclone.psyir.symbols import Symbol, RoutineSymbol, NoType
from psyclone.psyir.nodes import Reference
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
    tmp_symbol = subroutine.symbol_table.lookup("psyir_tmp")
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
    # TODO #1113, the specialise routine needs extending to support the setting
    # of such additional properties.
    symbol.specialise(RoutineSymbol)
    symbol.datatype = NoType()

    # In some cases we may want to replace one node with another. This
    # can be simply done using a node's `replace_with` method.
    assignment = subroutine.children[2]
    assignment_rhs = assignment.rhs
    assignment_rhs.replace_with(Reference(tmp_symbol))

    return file_container


if __name__ == "__main__":
    psyir_tree = modify_psyir_tree()

    # Write out the modified code as Fortran.
    writer = FortranWriter()
    result = writer(psyir_tree)
    print(result)
