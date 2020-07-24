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
# Authors: R. W. Ford and A. R. Porter, STFC Daresbury Lab

'''A transformation script that seeks to apply profiling for every routine
found in NEMO style code.  In order to use it you must first install
PSyclone. See README.md in the top-level psyclone directory.

Once you have psyclone installed, this may be used by doing:

 $ psyclone -api nemo -s profile_trans.py some_source_file.f90

This should produce a lot of output, ending with generated
Fortran. Note that the Fortran source files provided to PSyclone must
have already been preprocessed (if required).

We must take care to exclude certain nodes (such as Return) from
within profiling regions. If a proposed region is found to contain
such a node (by the ``add_profiling`` routine) then the script
moves a level down the tree and then repeats the process of attempting
to create the largest possible profiling region.

'''

from __future__ import print_function
import logging
from psyclone.psyir.transformations import TransformationError, ProfileTrans
from psyclone.psyir.nodes import IfBlock, CodeBlock, Schedule, \
    Assignment, Return
from psyclone.nemo import NemoInvokeSchedule

PROFILE_TRANS = ProfileTrans()

# If routine names contain these substrings then we do not profile them
PROFILING_IGNORE = ["_init", "_rst", "alloc", "agrif", "flo_dom",
                    "chkdim", # Has var idim overriding Fortran intrinsic
                    "ice_thd_pnd", "macho", "mpp_", "nemo_gcm",
                    # These are small functions that the addition of profiling
                    # prevents from being in-lined (and then breaks any attempt
                    # to create OpenACC regions with calls to them)
                    "interp1", "interp2", "interp3", "integ_spline", "sbc_dcy",
                    "sum", "sign_"]


def add_profiling(children):
    '''
    Walks down the PSyIR and inserts the largest possible profiling regions.
    Code that contains OpenACC directives is excluded.

    :param children: sibling nodes in the PSyIR to which to attempt to add \
                     profiling regions.
    :type childre: list of :py:class:`psyclone.psyGen.Node`

    '''
    from psyclone.psyGen import ACCDirective

    if not children:
        return

    node_list = []
    for child in children[:]:
        # Do we want this node to be included in a profiling region?
        if child.walk((ACCDirective, Return)):
            # It contains OpenACC so we put what we have so far inside a
            # profiling region
            add_profile_region(node_list)
            # A node that is not included in a profiling region marks the
            # end of the current candidate region so reset the list.
            node_list = []
            # Now we go down a level and try again
            if isinstance(child, IfBlock):
                add_profiling(child.if_body)
                add_profiling(child.else_body)
            elif isinstance(child, (Assignment, ACCDirective)):
                # We don't attempt to put profiling in below OpenACC
                # directives or within Assignments
                pass
            else:
                add_profiling(child.children)
        else:
            # We can - add this node to our list for the current region
            node_list.append(child)
    add_profile_region(node_list)


def add_profile_region(nodes):
    '''
    Attempt to put the supplied list of nodes within a profiling region.

    :param nodes: list of sibling PSyIR nodes to enclose.
    :type nodes: list of :py:class:`psyclone.psyGen.Node`

    '''
    if nodes:
        # Check whether we should be adding profiling inside this routine
        routine_name = nodes[0].ancestor(NemoInvokeSchedule).invoke.name.lower()
        if any([ignore in routine_name for ignore in PROFILING_IGNORE]):
            return
        if len(nodes) == 1:
            if isinstance(nodes[0], CodeBlock) and \
               len(nodes[0].get_ast_nodes) == 1:
                # Don't create profiling regions for CodeBlocks consisting
                # of a single statement
                return
            if isinstance(nodes[0], IfBlock) and \
               "was_single_stmt" in nodes[0].annotations and \
               isinstance(nodes[0].if_body[0], CodeBlock):
                # We also don't put single statements consisting of
                # 'IF(condition) CALL blah()' inside profiling regions
                return
        try:
            if len(nodes) == 1 and isinstance(nodes[0], IfBlock) and \
               "was_elseif" in nodes[0].annotations:
                # Special case for IfBlocks that represent else-ifs in the
                # fparser2 parse tree - we can't apply transformations to
                # them, only to their body. TODO #435.
                PROFILE_TRANS.apply(nodes[0].if_body)
            else:
                PROFILE_TRANS.apply(nodes)
        except TransformationError:
            pass


def trans(psy):
    '''A PSyclone-script compliant transformation function. Applies
    profiling to NEMO code.

    :param psy: The PSy layer object to apply transformations to.
    :type psy: :py:class:`psyclone.psyGen.PSy`

    '''
    print("Invokes found:\n{0}\n".format(
        "\n".join([str(name) for name in psy.invokes.names])))

    for invoke in psy.invokes.invoke_list:

        sched = invoke.schedule
        if not sched:
            print("Invoke {0} has no Schedule! Skipping...".
                  format(invoke.name))
            continue

        # Add profiling instrumentation
        print("Adding profiling of non-OpenACC regions to routine {0}".
              format(invoke.name))
        add_profiling(sched.children)

        sched.view()

        invoke.schedule = sched

    return psy
