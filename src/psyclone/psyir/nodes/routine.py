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

''' This module contains the Routine node implementation.'''

from psyclone.psyir.nodes.schedule import Schedule


class Routine(Schedule):
    '''
    Represents a subroutine, function or program unit.

    :param str name: the name of this routine.
    :param bool entry_point: whether this Routine represents the entry point \
                             into a program (i.e. Fortran Program or C main())
    :param return_type: the return-type of this routine.
    :type return_type: :py:class:`psyclone.psyir.symbols.DataType` or NoneType
    :param children: the PSyIR nodes that are children of this Routine.
    :type children: list of :py:class:`psyclone.psyir.nodes.Node`
    :param parent: the parent node of this Schedule in the PSyIR.
    :type parent: :py:class:`psyclone.psyir.nodes.Node` or NoneType
    :param symbol_table: initialise the Schedule with a given symbol table.
    :type symbol_table: :py:class:`psyclone.psyir.symbols.SymbolTable` or \
            NoneType

    '''
    # Textual description of the node.
    _children_valid_format = "[Statement]*"
    _text_name = "Routine"
    _colour_key = "Routine"

    def __init__(self, name, entry_point=False, return_type=None,
                 children=None, parent=None, symbol_table=None):
        super(Routine, self).__init__(children=children, parent=parent,
                                      symbol_table=symbol_table)
        #TODO perform type checks here
        self._name = name
        self._entry_point = entry_point
        self._return_type = return_type

    @property
    def dag_name(self):
        '''
        :returns: The name of this node in the dag.
        :rtype: str
        '''
        return "routine_" + str(self.abs_position)

    @property
    def name(self):
        return self._name

    def __str__(self):
        result = self.coloured_name(False) + ":\n"
        for entity in self._children:
            result += str(entity) + "\n"
        result += "End " + self.coloured_name(False)
        return result


# For automatic documentation generation
__all__ = ["Routine"]
