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

from psyclone.psyir.transformations import TransformationError
from psyclone.psyir.nodes import CodeBlock, ProfileNode, Return, Routine
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
        '''
        Checks that the supplied list of nodes is valid for profiling
        callipers.

        :param nodes: a node or list of nodes to be instrumented with
                      profiling.
        :type nodes: :py:class:`psyclone.psyir.nodes.Node` or
                     list[:py:class:`psyclone.psyir.nodes.Node`]
        :param bool options["force"]: whether to ignore potential control
                                      flow jumps when applying this
                                      transformation. Default is False.

        :raises TransformationError: if the supplied region contains a
                                     potential control flow jump that could
                                     result in skipping the end of profiling
                                     caliper, e.g. EXIT or GOTO.
        '''
        if not options:
            options = {}
        forced = options.get("force", False)
        super().validate(nodes, options)
        if forced:
            return
        node_list = self.get_node_list(nodes)
        # If the node_list is the same as a whole routine then we skip the
        # checks for internal control flow jumps.
        parent = node_list[0].parent
        if (isinstance(parent, Routine) and
                len(parent.children) == len(node_list)):
            # If the node_list is the same size and the parent of the first
            # is the routine then this is the full Routine (see
            # RegionDirective.validate for the validation).
            return

        # Find all the codeblocks and check if they contain a control
        # flow jump.
        for node in node_list:
            codeblocks = node.walk(CodeBlock)
            for block in codeblocks:
                if block.has_potential_control_flow_jump():
                    raise TransformationError(
                        f"Cannot apply the ProfileTrans to a code region "
                        f"containing a potential control flow jump, as these "
                        f"could skip the end of profiling caliper. "
                        f"Found:\n'{block.debug_string()}'")
