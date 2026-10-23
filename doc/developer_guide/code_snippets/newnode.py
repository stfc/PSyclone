# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2020-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

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
    _colour = "blue"
    _children_valid_format = "DataNode"

    @staticmethod
    def _validate_child(position, child):
        return position == 0 and isinstance(child, DataNode)


def example():
    ''' Example of MyNode usage'''
    from psyclone.psyir.nodes import Schedule, Literal
    from psyclone.psyir.symbols import ScalarType
    psyir_schedule = Schedule()

    mynode = MyNode(children=[Literal("1", ScalarType.integer_type())])

    psyir_schedule.addchild(mynode)

    # The following statement is not valid as MyNode only accepts 1 child.
    # mynode.children.append(Literal("2", ScalarType.integer_type()))

    # The following statement is not valid as Assignment expects DataNodes
    # from psyclone.psyir.nodes import Assignment
    # assignment = Assignment()
    # assignment.addchild(mynode)

    print(psyir_schedule.view())


if __name__ == "__main__":
    example()
