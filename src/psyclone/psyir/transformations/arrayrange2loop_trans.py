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
# Author R. W. Ford, STFC Daresbury Lab

'''This module contains the the Array Range to Loop transformation.
'''

import abc
import six

from psyclone.psyGen import Kern, Node, Schedule, Transformation
from psyclone.psyir.transformations.transformation_error \
    import TransformationError

from psyclone.psyir.symbols import DataSymbol, INTEGER_TYPE
from psyclone.psyir.nodes import Loop, Literal, Range, Reference, BinaryOperation, Array

class ArrayRange2LoopTrans(Transformation):
    '''This transformation replaces an access to an array section with an
    explicit loop. xxx

    '''
    def apply(self, node):
        ''' xxx '''
        self.validate(node)

        parent = node.parent
        # Replace find_symbol_table() with node.scope.symbol_table
        # when PR #795 is on master
        symbol_table = node.find_symbol_table()
        loop_variable_name = symbol_table.new_symbol_name(root_name="idx")
        loop_variable_symbol = DataSymbol(loop_variable_name, INTEGER_TYPE)
        symbol_table.add(loop_variable_symbol)
        
        # replace the first range found in all arrays with the
        # iterator and use the range from the LHS range for the loop
        # iteration space.
        for array in node.walk(Array):
            for idx, child in enumerate(array.children):
                if isinstance(child, Range):
                    if array is node.lhs:
                        # Issue #XXX If loop bounds were a Range we would just
                        # need to move the range node which would be simpler.
                        lbound = child.children[0]
                        ubound = child.children[1]
                        step = child.children[2]
                    array.children[idx] = Reference(loop_variable_symbol,
                                                        parent=array)
                    break
        loop = Loop.create(loop_variable_symbol, lbound, ubound, step, [node])
        parent.children[node.position] = loop
        loop.parent = parent

    def validate(self, node):
        ''' xxx '''
        pass

    def name(self):
        ''' xxx '''
        return 'a_name'
