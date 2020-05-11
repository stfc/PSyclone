# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2020, Science and Technology Facilities Council
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

'''A simple Python script showing how to create new PSyIR nodes and
provide them with the basic functionality. In order to use it you must
first install PSyclone. Once you have psyclone installed, this script
may be run by doing:

>>> python newnode.py

This should output a PSyIR tree containing the new node.

'''

from psyclone.psyir.nodes import Statement, DataNode


class MyNode(Statement):
    ''' MyNode is an example node that can be found anywhere where statement
    is valid, and in turn it accepts one and only one DataNode as a children.
    '''
    _text_name = "MyNodeName"
    _colour_key = "Assignment"
    _children_valid_format = "DataNode"

    @staticmethod
    def _validate_child(position, child):
        return position == 0 and isinstance(child, DataNode)


def example():
    ''' Example of MyNode usage'''
    from psyclone.psyir.nodes import Schedule, Literal
    from psyclone.psyir.symbols import INTEGER_TYPE
    psyir_schedule = Schedule()

    mynode = MyNode(children=[Literal("1", INTEGER_TYPE)])

    psyir_schedule.addchild(mynode)

    # The following statement is not valid as MyNode only accepts 1 child.
    # mynode.children.append(Literal("2", INTEGER_TYPE))

    # The following statement is not valid as Assignment expects DataNodes
    # from psyclone.psyir.nodes import Assignment
    # assignment = Assignment()
    # assignment.addchild(mynode)

    psyir_schedule.view()


if __name__ == "__main__":
    example()
