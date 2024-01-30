# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2023-2024, Science and Technology Facilities Council.
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

'''A PSyIR backend to create Fortran-like strings without lowering the
higher level nodes. This is useful for printing debug information. '''

from psyclone.psyir.nodes import DataNode
from psyclone.psyir.backend.fortran import FortranWriter


class DebugWriter(FortranWriter):
    '''Implements a PSyIR backend to generate Fortran-like strings but without
    lowering the higher level concepts to language constructs. This special
    backend can do it because instead of failing for an unknown construct it
    will just print the name of the higher level Node.
    The resulting code will not be compilable but it will be usable for
    generating debug information much faster than the FortranWriter because
    it does not need to lower the nodes and therefore it does not need to
    deepcopy the tree.

    '''
    # This option will disable the lowering of abstract nodes into language
    # level nodes, and as a consequence the backend does not need to deep-copy
    # the tree and is much faster to execute.
    # Be careful not to modify anything from the input tree when this option
    # is set to True as the modifications will persist after the Writer!
    _DISABLE_LOWERING = True

    def __init__(self):
        super().__init__(check_global_constraints=False)

    def node_node(self, node):
        ''' The DebugWriter must never fail with unrecognized nodes, this
        generic visitor will capture any Node that the super Fortran backend
        did not catch and output it as "< Node string >".

        :param node: the PSyIR node to translate.
        :type node: :py:class:`psyclone.psyir.node.Node`

        :returns: A string representing a node unrecognized by the other \
            visitor methods of this Writer.
        :rtype: str

        '''
        # If its not part of an expression it needs indentation and \n
        if not isinstance(node, DataNode):
            return f"{self._nindent}< {str(node)} >\n"
        return f"< {str(node)} >"
