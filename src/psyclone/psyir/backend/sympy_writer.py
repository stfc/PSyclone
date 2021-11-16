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
# Author: J. Henrichs, Bureau of Meteorology


'''PSyIR backend to create expressions that are handled by sympy.
'''

# pylint: disable=too-many-lines
from __future__ import absolute_import

from psyclone.errors import InternalError
from psyclone.psyir.backend.fortran import FortranWriter
from psyclone.psyir.symbols import ScalarType


class SymPyWriter(FortranWriter):
    '''Implements a PSyIR-to-sympy writer, used to create a representation
    of the PSyIR tree that can be understood by SymPy. Most Fortran
    expressions work as expected, this class implements special handling
    for constants (which can have a precision attached: 2_4) and some
    intrinsic functions (e.g. MAX, which SymPy expects to be Max).

    '''

    def literal_node(self, node):
        '''This method is called when a Literal instance is found in the PSyIR
        tree.

        :param node: a Literal PSyIR node.
        :type node: :py:class:`psyclone.psyir.nodes.Literal`

        :returns: the SymPy representation for the literal.
        :rtype: str

        '''
        if node.datatype.intrinsic == ScalarType.Intrinsic.BOOLEAN:
            # Booleans need to be converted to SymPy format
            return node.value.capitalize()

        if node.datatype.intrinsic == ScalarType.Intrinsic.CHARACTER:
            raise InternalError("SymPy cannot handle strings like '{0}'."
                                .format(node.value))
        # All real (single, double precision) and integer work by just
        # using the node value. Single and double precision both use
        # 'e' as specification, which SymPy accepts, and precision
        # information can be ignored.
        return node.value
