#!/usr/bin/env python
# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2018-2019, Science and Technology Facilities Council.
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
# Authors: R. W. Ford and A. R. Porter, STFC Daresbury Lab

'''A transformation script that seeks to apply OpenACC DATA and KERNELS
directives to NEMO style code.  In order to use
it you must first install PSyclone. See README.md in the top-level
psyclone directory.

Once you have psyclone installed, this may be used by doing:

 $ psyclone -api nemo -s nemo_kernels_trans.py some_source_file.f90

This should produce a lot of output, ending with generated
Fortran. Note that the Fortran source files provided to PSyclone must
have already been preprocessed (if required).

'''

from __future__ import print_function
from psyclone.psyGen import TransInfo


# Get the PSyclone transformations we will use
ACC_KERN_TRANS = TransInfo().get_trans_name('ACCKernelsTrans')
ACC_DATA_TRANS = TransInfo().get_trans_name('ACCDataTrans')


def valid_kernel(node):
    '''
    Whether the sub-tree that has `node` at its root is eligible to be
    enclosed within an OpenACC KERNELS directive.

    :param node: the node in the PSyIRe to check.
    :type node: :py:class:`psyclone.psyGen.Node`

    :returns: True if the sub-tree can be enclosed in a KERNELS region.
    :rtype: bool

    '''
    from psyclone.nemo import NemoLoop, NemoIfBlock, NemoIfClause
    from psyclone.psyGen import CodeBlock
    excluded_nodes = (CodeBlock, NemoIfBlock, NemoIfClause)
    if isinstance(node, excluded_nodes):
        return False
    code_blocks = node.walk(node.children, excluded_nodes)
    if code_blocks:
        return False
    return True


def add_kernels(children):
    '''
    Walks through the PSyIR inserting OpenACC KERNELS directives at as
    high a level as possible.

    :param children: list of sibling Nodes in PSyIR that are candidates for \
                     inclusion in an ACC KERNELS region.
    :type children: list of :py:class:`psyclone.psyGen.Node`
    '''
    from psyclone.nemo import NemoLoop
    if not children:
        return

    node_list = []
    for child in children[:]:
        # Can this node be included in a kernels region?
        if not valid_kernel(child):
            if node_list:
                _, _ = ACC_KERN_TRANS.apply(node_list, default_present=True)
                node_list = []
            # It can't so go down a level and try again
            add_kernels(child.children)
        else:
            node_list.append(child)
    if node_list:
        # Does our list of nodes contain any loops?
        have_loops = False
        for node in node_list:
            if isinstance(node, NemoLoop):
                have_loops = True
            else:
                loops = node.walk(node.children, NemoLoop)
                have_loops = bool(loops)
            if have_loops:
                _, _ = ACC_KERN_TRANS.apply(node_list, default_present=True)
                break


def trans(psy):
    '''A PSyclone-script compliant transformation function. Applies
    OpenACC 'kernels' and 'data' directives to NEMO code.

    :param psy: The PSy layer object to apply transformations to.
    :type psy: :py:class:`psyclone.psyGen.PSy`
    '''
    from psyclone.psyGen import CodeBlock, ACCDirective
    print("Invokes found:")
    print(psy.invokes.names)

    for invoke in psy.invokes.invoke_list:

        sched = invoke.schedule
        if not sched:
            print("Invoke {0} has no Schedule! Skipping...".format(invoke.name))
            continue
        sched.view()

        add_kernels(sched.children)
        sched.view()

        directives = sched.walk(sched.children, ACCDirective)
        if not directives:
            # We only need a data region if we've added any directives
            continue

        # Enclose all children in the schedule within a data region. We must
        # ensure that the allocate/deallocate's are done outside this region
        # and so we search for the first and last loops
        first_idx = -1
        last_idx = -1
        for idx, child in enumerate(sched.children):
            if not isinstance(child, CodeBlock):
                first_idx = idx
                break
        for child in reversed(sched.children):
            if not isinstance(child, CodeBlock):
                last_idx = sched.children.index(child)
                break
        if first_idx > -1 and last_idx > -1:
            sched, _ = ACC_DATA_TRANS.apply(
                sched.children[first_idx:last_idx+1])

        sched.view()

        invoke.schedule = sched
