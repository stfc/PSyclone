# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2025, Science and Technology Facilities Council.
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
# Authors S. Siso, STFC Daresbury Lab
# -----------------------------------------------------------------------------

''' Performs that the node.py colored function works when no the termcolor
dependency is not installed. '''

import sys
sys.modules['termcolor'] = None
from psyclone.psyir.nodes.node import colored


def test_node_coloured_name():
    ''' Tests for the coloured_name method of the Node class. '''
    tnode = Node()
    # Exception as Node is abstract and _colour has not been set
    with pytest.raises(NotImplementedError) as err:
        _ = tnode.coloured_name()
    assert ("The _colour attribute is abstract so needs to be given a string "
            "value in the concrete class 'Node'." in str(err.value))

    # Create a node type with a colour attribute
    class MyNode(Node):
        ''' Mock node sub-class with green coloured text. '''
        _colour = "green"

    mynode = MyNode()
    assert mynode.coloured_name(False) == "MyNode"
    assert mynode.coloured_name(True) == colored("MyNode", "green")

    # Give MyNode a specific _text_name
    MyNode._text_name = "MyFancyNodeName"
    assert mynode.coloured_name(False) == "MyFancyNodeName"
    assert mynode.coloured_name(True) == colored("MyFancyNodeName", "green")
