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
from create import create_psyir_tree


def modify_psyir_tree():
    ''' Apply modifications to the PSyIR tree created in create.py

    :returns: a modified PSyIR tree.
    :rtype: :py:class:`psyclone.psyir.nodes.Container`

    '''
    container = create_psyir_tree()
    subroutine = container.children[0]

    # Rename one of the subroutine local symbols.
    tmp_symbol = subroutine.symbol_table.lookup("psyir_tmp")
    subroutine.symbol_table.rename_symbol(tmp_symbol, "new_variable")

    return container


if __name__ == "__main__":
    psyir_tree = modify_psyir_tree()

    # Write out the modified code as Fortran.
    writer = FortranWriter()
    result = writer(psyir_tree)
    print(result)
