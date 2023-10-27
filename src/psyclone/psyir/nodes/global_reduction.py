# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017-2023, Science and Technology Facilities Council.
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
#         I. Kavcic, Met Office
#         J. Henrichs, Bureau of Meteorology
# -----------------------------------------------------------------------------

''' This module contains the Global Reduction node implementation.'''

from psyclone.core import AccessType
from psyclone.f2pygen import CommentGen
from psyclone.psyir.nodes.node import Node
from psyclone.psyir.nodes.statement import Statement


class GlobalReduction(Statement):
    '''
    Generic Global Reduction class which can be added to and manipulated
    in, a schedule.

    :param scalar: the scalar that the global reduction is stored into
    :type scalar: :py:class:`psyclone.psyGen.KernelArgument`
    :param parent: optional parent (default None) of this object
    :type parent: :py:class:`psyclone.psyir.nodes.Node`

    '''
    # Textual description of the node.
    _children_valid_format = "<LeafNode>"
    _text_name = "GlobalReduction"
    _colour = "cyan"

    def __init__(self, scalar, parent=None):
        print("init GlobalReduction\n", scalar, "\n", parent)
        print("\npar parent ", parent.parent, parent.ancestor)
        Node.__init__(self, children=[], parent=parent)
        import copy
        self._scalar = copy.copy(scalar)
        if scalar:
            # Update scalar values appropriately
            # Here "readwrite" denotes how the class GlobalReduction
            # accesses/updates a scalar
            self._scalar.access = AccessType.READWRITE
            self._scalar.call = self

    @property
    def scalar(self):
        '''
        :returns: the scalar that this class reduces values
                  of a field or array to.
        :rtype: str

        '''
        return self._scalar

    @property
    def dag_name(self):
        '''
        :returns: the name to use in the DAG for this node.
        :rtype: str

        '''
        return f"globalreduction({self._scalar.name})_{self.position}"

    @property
    def args(self):
        '''
        Return the list of arguments associated with this node. Override
        the base method and simply return our argument.

        :returns: the list of scalar reduction arguments.
        :rtype: list of :py:class:`psyclone.psyGen.KernelArgument`

        '''
        return [self._scalar]

    def node_str(self, colour=True):
        '''
        Returns a text description of this node with (optional) control codes
        to generate coloured output in a terminal that supports it.

        :param bool colour: whether or not to include colour control codes.

        :returns: description of this node, possibly coloured.
        :rtype: str

        '''
        return f"{self.coloured_name(colour)}[scalar='{self._scalar.name}']"

    @staticmethod
    def initialise_reduction_variables(red_call_list, parent):
        '''
        Initialise all reduction variables associated with the calls in the call
        list.
        :param red_call_list: list of kernel calls that contain \
                              a reduction variable.
        :type red_call_list: list of `psyGen.Kern`
        :param parent: the node in the f2pygen AST to which to add content.
        :type parent: :py:class:`psyclone.f2pygen.SubroutineGen`
        '''
        # TODO #2381: It is not clear where this function belongs. For now
        # it is used in 'Loop.gen_code' and 'OMPParallelDoDirective.gen_code
        # methods. Also, it does not make sense to zero global min and max
        # variables before calculating them so this function may be renamed.
        print("initialise_reduction_variables\n", red_call_list, "\n", parent)
        print("\npar parent", parent.parent)
        if red_call_list:
            parent.add(CommentGen(parent, ""))
            parent.add(CommentGen(parent, " Initialise reduction variables"))
            parent.add(CommentGen(parent, ""))
            for call in red_call_list:
                call.zero_reduction_variable(parent)
            parent.add(CommentGen(parent, ""))

# For AutoAPI documentation generation
__all__ = ['GlobalReduction']
