# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2019, Science and Technology Facilities Council.
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
# Author: A. R. Porter, STFC Daresbury Lab

''' Module containing various utilities to aid in the application of
    OpenACC KERNELS directives to NEMO source. Mainly required to
    workaround the vagaries of the PGI compiler's support for OpenACC.
'''

from __future__ import print_function


def valid_kernel(node):
    '''
    Whether the sub-tree that has `node` at its root is eligible to be
    enclosed within an OpenACC KERNELS directive.

    :param node: the node in the PSyIR to check.
    :type node: :py:class:`psyclone.psyGen.Node`

    :returns: True if the sub-tree can be enclosed in a KERNELS region.
    :rtype: bool

    '''
    from psyclone.psyGen import CodeBlock, IfBlock
    from fparser.two.utils import walk_ast
    from fparser.two import Fortran2003
    # PGI (18.10) often produces code that fails at run time if a Kernels
    # region includes If constructs.
    excluded_node_types = (CodeBlock, IfBlock)
    if node.walk(excluded_node_types):
        return False
    # Check that there are no derived-type references in the sub-tree (because
    # PGI deep-copy doesn't like them).
    # TODO #365 - this check should be part of our identification of valid
    # NEMO kernels.
    if walk_ast([node.ast], [Fortran2003.Data_Ref]):
        return False
    return True


def have_loops(nodes):
    '''
    Checks to see whether there are any Loops in the list of nodes and
    their sub-trees.

    :param nodes: list of PSyIR nodes to check for Loops.
    :type nodes: list of :py:class:`psyclone.psyGen.Node`
    :returns: True if a Loop is found, False otherwise.
    :rtype: bool

    '''
    from psyclone.nemo import NemoLoop
    for node in nodes:
        if node.walk(NemoLoop):
            return True
    return False


def add_kernels(children, default_present=True):
    '''
    Walks through the PSyIR inserting OpenACC KERNELS directives at as
    high a level as possible.

    :param children: list of sibling Nodes in PSyIR that are candidates for \
                     inclusion in an ACC KERNELS region.
    :type children: list of :py:class:`psyclone.psyGen.Node`
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
    :type nodes: list of :py:class:`psyclone.psyGen.Node`
    :param bool default_present: whether or not to supply the \
                          DEFAULT(PRESENT) clause to ACC KERNELS directives.

    '''
    from psyclone.psyGen import InternalError
    from psyclone.transformations import TransformationError, ACCKernelsTrans
    try:
        _, _ = ACCKernelsTrans().apply(nodes,
                                       {"default_present": default_present})
    except (TransformationError, InternalError) as err:
        print("Failed to transform nodes: {0}", nodes)
        print("Error was: {0}".format(str(err)))
