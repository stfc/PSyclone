# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2019-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

''' Module containing various utilities to aid in the application of
    OpenACC KERNELS directives to NEMO source. Mainly required to
    workaround the vagaries of the NVIDIA compiler's support for OpenACC.
'''

from fparser.two.utils import walk
from fparser.two import Fortran2003
from psyclone.errors import InternalError
from psyclone.psyir.nodes import CodeBlock, IfBlock, Loop
from psyclone.psyir.transformations import ACCKernelsTrans
from psyclone.transformations import TransformationError


def valid_kernel(node):
    '''
    Whether the sub-tree that has `node` at its root is eligible to be
    enclosed within an OpenACC KERNELS directive.

    :param node: the node in the PSyIR to check.
    :type node: :py:class:`psyclone.psyir.nodes.Node`

    :returns: True if the sub-tree can be enclosed in a KERNELS region.
    :rtype: bool

    '''
    # PGI (18.10) often produces code that fails at run time if a Kernels
    # region includes If constructs.
    excluded_node_types = (CodeBlock, IfBlock)
    if node.walk(excluded_node_types):
        return False
    # Check that there are no derived-type references in the sub-tree (because
    # PGI deep-copy doesn't like them).
    # TODO #3341 - this check should be part of our identification of valid
    # NEMO kernels.
    if walk(node.ast, Fortran2003.Data_Ref):
        return False
    return True


def have_loops(nodes):
    '''
    Checks to see whether there are any Loops in the list of nodes and
    their sub-trees.

    :param nodes: list of PSyIR nodes to check for Loops.
    :type nodes: list of :py:class:`psyclone.psyir.nodes.Node`
    :returns: True if a Loop is found, False otherwise.
    :rtype: bool

    '''
    for node in nodes:
        if node.walk(Loop):
            return True
    return False


def add_kernels(children, default_present=True):
    '''
    Walks through the PSyIR inserting OpenACC KERNELS directives at as
    high a level as possible.

    :param children: list of sibling Nodes in PSyIR that are candidates for \
                     inclusion in an ACC KERNELS region.
    :type children: list of :py:class:`psyclone.psyir.nodes.Node`
    :param bool default_present: whether or not to supply the \
                          DEFAULT(PRESENT) clause to ACC KERNELS directives.

    '''
    if not children:
        return

    node_list = []
    for child in children[:]:
        # Can this node be included in a kernels region?
        if not valid_kernel(child):
            if have_loops(node_list):
                try_kernels_trans(node_list, default_present)
                node_list = []
            # It can't so go down a level and try again
            node_list = []
            add_kernels(child.children)
        else:
            node_list.append(child)
    if have_loops(node_list):
        try_kernels_trans(node_list, default_present)


def try_kernels_trans(nodes, default_present):
    '''
    Attempt to enclose the supplied list of nodes within a kernels
    region. If the transformation fails then the error message is
    reported but execution continues.

    :param nodes: list of Nodes to enclose within a Kernels region.
    :type nodes: list of :py:class:`psyclone.psyir.nodes.Node`
    :param bool default_present: whether or not to supply the \
                          DEFAULT(PRESENT) clause to ACC KERNELS directives.

    '''
    try:
        ACCKernelsTrans().apply(nodes, {"default_present": default_present})
    except (TransformationError, InternalError) as err:
        print(f"Failed to transform nodes: {nodes}")
        print(f"Error was: {err}")
