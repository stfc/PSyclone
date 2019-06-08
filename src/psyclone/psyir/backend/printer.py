# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2019, Science and Technology Facilities Council
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
# Author R. W. Ford, STFC Daresbury Lab.

'''Printer PSyIR backend. Prints the node hierarchy. Limited to PSyIR
Kernel schedules as PSy-layer PSyIR already has a gen() method to
generate Fortran.

'''

from psyclone.psyir.backend.base import PSyIRVisitor

class PrinterPSyIRVisitor(PSyIRVisitor):
    '''Prints the PSyIR hierarchy (but not currently PSyIR algorithm code
    which has its own gen method for generating Fortran).

    '''
    def nemokern(self, node):
        ''' xxx '''
        result = "{0}[ {1} start ]\n".format(self._nindent,
                                             type(node).__name__)
        self._depth += 1
        kern_schedule = node.get_kernel_schedule()
        result += self.visit(kern_schedule)
        self._depth -= 1
        result += "{0}[ {1} end ]\n".format(self._nindent, type(node).__name__)
        return result

    def node(self, node):
        '''Print the nodes class names and continue down the node hierarchy.

        :param node: A generic PSyIR node.
        :type node: subclass of :py:class:`psyclone.psyGen.Node`

        :returns: The PSyIR hierarchy as a string.
        :rtype: str

        '''
        result = "{0}[ {1} start ]\n".format(self._nindent,
                                             type(node).__name__)
        self._depth += 1
        for child in node.children:
            result += self.visit(child)
        self._depth -= 1
        result += "{0}[ {1} end ]\n".format(self._nindent, type(node).__name__)
        return result
