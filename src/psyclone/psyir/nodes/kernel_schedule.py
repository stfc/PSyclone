# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2020, Science and Technology Facilities Council.
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

''' This module contains the KernelSchedule node implementation.'''

from psyclone.psyir.nodes.routine import Routine
from psyclone.psyir.nodes.node import Node
from psyclone.psyir.symbols.symboltable import SymbolTable
from psyclone.errors import GenerationError


class KernelSchedule(Routine):
    '''
    A KernelSchedule is the parent node of the PSyIR for Kernel source code.

    :param str name: Kernel subroutine name.
    :param parent: Parent of the KernelSchedule, defaults to None.
    :type parent: :py:class:`psyclone.psyir.nodes.Node`

    '''
    def __init__(self, name, parent=None):
        super(KernelSchedule, self).__init__(name, entry_point=False,
                                             return_type=None,
                                             children=None, parent=parent)

    @staticmethod
    def create(name, symbol_table, children):
        '''Create a KernelSchedule instance given a name, a symbol table and a
        list of child nodes.

        :param str name: the name of the KernelSchedule.
        :param symbol_table: the symbol table associated with this \
            KernelSchedule.
        :type symbol_table: :py:class:`psyclone.psyGen.SymbolTable`
        :param children: a list of PSyIR nodes contained in the \
            KernelSchedule.
        :type children: list of :py:class:`psyclone.psyir.nodes.Node`

        :returns: a KernelSchedule instance.
        :rtype: :py:class:`psyclone.psyGen.KernelInstance`

        :raises GenerationError: if the arguments to the create method \
            are not of the expected type.

        '''
        if not isinstance(name, str):
            raise GenerationError(
                "name argument in create method of KernelSchedule class "
                "should be a string but found '{0}'."
                "".format(type(name).__name__))
        if not isinstance(symbol_table, SymbolTable):
            raise GenerationError(
                "symbol_table argument in create method of KernelSchedule "
                "class should be a SymbolTable but found '{0}'."
                "".format(type(symbol_table).__name__))
        if not isinstance(children, list):
            raise GenerationError(
                "children argument in create method of KernelSchedule class "
                "should be a list but found '{0}'."
                "".format(type(children).__name__))
        for child in children:
            if not isinstance(child, Node):
                raise GenerationError(
                    "child of children argument in create method of "
                    "KernelSchedule class should be a PSyIR Node but "
                    "found '{0}'.".format(type(child).__name__))

        kern = KernelSchedule(name)
        kern._symbol_table = symbol_table
        symbol_table._node = kern
        for child in children:
            child.parent = kern
        kern.children = children
        return kern

    def node_str(self, colour=True):
        ''' Returns the name of this node with (optional) control codes
        to generate coloured output in a terminal that supports it.

        :param bool colour: whether or not to include colour control codes.

        :returns: description of this node, possibly coloured.
        :rtype: str
        '''
        return self.coloured_name(colour) + "[name:'" + self._name + "']"

    def __str__(self):
        result = self.node_str(False) + ":\n"
        for entity in self._children:
            result += str(entity)
        result += "End KernelSchedule\n"
        return result


# For automatic documentation generation
__all__ = ["KernelSchedule"]
