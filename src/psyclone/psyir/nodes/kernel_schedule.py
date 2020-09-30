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


class KernelSchedule(Routine):
    '''
    A KernelSchedule is the parent node of the PSyIR for Kernel source code.

    :param str name: kernel subroutine name.
    :param parent: parent of the KernelSchedule, defaults to None.
    :type parent: :py:class:`psyclone.psyir.nodes.Node`

    '''
    _text_name = "KernelSchedule"

    def __init__(self, name, parent=None):
        # A kernel does not return anything and is not a main program
        super(KernelSchedule, self).__init__(name, is_program=False,
                                             return_type=None,
                                             parent=parent)

    @classmethod
    def create(cls, name, symbol_table, children):
        '''Create a KernelSchedule instance given a name, a symbol table and a
        list of child nodes. This is a classmethod so that no instance
        of KernelSchedule is required in order to call it and the classmethod
        in the base class can be called straightforwardly.

        :param str name: the name of the KernelSchedule.
        :param symbol_table: the symbol table associated with this \
            KernelSchedule.
        :type symbol_table: :py:class:`psyclone.psyGen.SymbolTable`
        :param children: a list of PSyIR nodes contained in the \
            KernelSchedule.
        :type children: list of :py:class:`psyclone.psyir.nodes.Node`

        :returns: a KernelSchedule instance.
        :rtype: :py:class:`psyclone.psyGen.KernelInstance`

        '''
        return super(cls, KernelSchedule).create(name, symbol_table, children,
                                                 is_program=False,
                                                 return_type=None)


# For automatic documentation generation
__all__ = ["KernelSchedule"]
