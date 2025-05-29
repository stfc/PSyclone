# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2018-2025, Science and Technology Facilities Council.
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
# Author J. Henrichs, Bureau of Meteorology
# Modified by R. W. Ford, A. R. Porter and S. Siso, STFC Daresbury Lab
# Modified by A. B. G. Chalk, STFC Daresbury Lab
# -----------------------------------------------------------------------------

'''This module provides the Profile transformation.
'''

from fparser.two.Fortran2003 import (Comment, Exit_Stmt,
                                     Goto_Stmt, BlockBase, StmtBase)
from psyclone.psyir.transformations import TransformationError
from psyclone.psyir.nodes import CodeBlock, Return, ProfileNode
from psyclone.psyir.transformations.psy_data_trans import PSyDataTrans


class ProfileTrans(PSyDataTrans):
    ''' Create a profile region around a list of statements. For
    example:

    >>> from psyclone.parse.algorithm import parse
    >>> from psyclone.parse.utils import ParseError
    >>> from psyclone.psyGen import PSyFactory, GenerationError
    >>> from psyclone.psyir.transformations import ProfileTrans
    >>> api = "gocean"
    >>> filename = "nemolite2d_alg.f90"
    >>> ast, invokeInfo = parse(filename, api=api, invoke_name="invoke")
    >>> psy = PSyFactory(api).create(invokeInfo)
    >>>
    >>> p_trans = ProfileTrans()
    >>>
    >>> schedule = psy.invokes.get('invoke_0').schedule
    >>> print(schedule.view())
    >>>
    >>> # Enclose all children within a single profile region
    >>> p_trans.apply(schedule.children)
    >>> print(schedule.view())

    This implementation relies completely on the base class PSyDataTrans
    for the actual work, it only adjusts the name etc, and the list
    of valid nodes.

    '''
    # Unlike other transformations we can be fairly relaxed about the nodes
    # that a region can contain as we don't have to understand them.
    excluded_node_types = (Return,)

    def __init__(self):
        super().__init__(ProfileNode)

    def validate(self, nodes, options=None):
        """
        Checks that the supplied list of nodes is valid for profiling
        callipers.

        :param nodes: a node or list of nodes to be instrumented with
                      profiling.
        :type nodes: :py:class:`psyclone.psyir.nodes.Node` or
                     list[:py:class:`psyclone.psyir.nodes.Node`]

        :raises TransformationError: if the supplied region contains an
                                     EXIT or GOTO statement.
        :raises TransformationError: if the supplied region contains a
                                     labelled statement.
        """
        # Find all the codeblocks.
        node_list = self.get_node_list(nodes)
        for node in node_list:
            codeblocks = node.walk(CodeBlock)
            for block in codeblocks:
                for child in block._fp2_nodes:
                    if isinstance(child,
                                  (Goto_Stmt, Exit_Stmt)):
                        raise TransformationError(
                            "Cannot apply the ProfileTrans to a code region "
                            "containing a Fortran EXIT or GOTO "
                            "statement."
                        )
                    # Also can't support Labelled statements.
                    if isinstance(child, BlockBase):
                        # An instance of BlockBase describes a block of code (no surprise
                        # there), so we have to examine the first statement within it. We
                        # must allow for the case where the block is empty though.
                        if (child.content and child.content[0] and
                                 (not isinstance(child.content[0],
                                                 Comment)) and
                                 child.content[0].item and
                                 child.content[0].item.label):
                            raise TransformationError(
                                "Cannot apply the ProfileTrans to a code region "
                                "containing a labelled statement."
                            )
                    elif isinstance(child, StmtBase):
                        if child.item and child.item.label:
                            raise TransformationError(
                                "Cannot apply the ProfileTrans to a code region "
                                "containing a labelled statement."
                            )

        super().validate(nodes, options)
