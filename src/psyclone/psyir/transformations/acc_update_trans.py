# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2021, Science and Technology Facilities Council.
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

'''
This module provides the ACCUpdateTrans transformation that ensures that
data is kept up-to-date on the host.
'''

from __future__ import absolute_import
from psyclone.psyir.nodes import (IfBlock, Loop, Schedule, ACCKernelsDirective,
                                  ACCParallelDirective, ACCUpdateDirective)
from psyclone.psyir.tools import DependencyTools
from psyclone.psyGen import Transformation
#, TransformationError


class ACCUpdateTrans(Transformation):
    '''
    Examines the supplied Schedule and adds "acc update" directives
    for any data accessed outside of a kernels or parallel region.

    '''
    def validate(self, sched, options=None):
        ''' '''
        if not isinstance(sched, Schedule):
            raise TransformationError()

    def apply(self, sched, options=None):
        ''' '''
        # We must walk through the Schedule and find those nodes that are
        # not within a kernels or parallel region.
        #
        # !$acc kernels
        # var(:,:) = 0.0
        # !$acc end kernels
        # !$acc update if(acc_is_present(blah)) self(blah)
        # if(blah == 1)then
        #   !$acc update if(acc_is_present(var)) self(var)
        #   var(:,1) = 0.0
        #   !$acc update device(var)
        #   !$acc kernels
        #   var2(:,:) = var(:,:)
        #   !$acc end kernels
        # end if
        dep_tools = DependencyTools()
        acc_region_nodes = (ACCParallelDirective, ACCKernelsDirective)
        node_list = []
        start_node = None
        for child in sched.children[:]:
            if not child.walk(acc_region_nodes):
                if not start_node:
                    start_node = child
                node_list.append(child)
            else:
                if isinstance(child, (IfBlock, Loop)):
                    # TODO: Update node_list with nodes from IfBlock condition
                    # or Loop bounds/step
                    pass
                if node_list:
                    inputs, outputs = dep_tools.get_in_out_parameters(
                        node_list)
                    for sig in inputs:
                        if sig.is_structure:
                            raise NotImplementedError("ARPDBG2")
                        sym = sched.symbol_table.lookup(sig.var_name)
                        update_dir = ACCUpdateDirective(sym, "host")
                        parent = start_node.parent
                        parent.addchild(update_dir,
                                        parent.children.index(start_node))
                node_list = []
                start_node = None
