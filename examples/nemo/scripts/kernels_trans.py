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

 $ psyclone -api nemo -s kernels_trans.py some_source_file.f90

This should produce a lot of output, ending with generated
Fortran. Note that the Fortran source files provided to PSyclone must
have already been preprocessed (if required).

The transformation script attempts to insert Kernels directives at the
highest possible location(s) in the schedule tree (i.e. to enclose as
much code as possible in each Kernels region). However, due to
limitations in the PGI compiler, we must take care to exclude certain
nodes (such as If blocks) from within Kernel regions. If a proposed
region is found to contain such a node (by the ``valid_kernel``
routine) then the script moves a level down the tree and then repeats
the process of attempting to create the largest possible Kernel
region.

'''

from __future__ import print_function
from psyclone.psyGen import TransInfo
from psyclone.transformations import TransformationError


# Get the PSyclone transformations we will use
ACC_KERN_TRANS = TransInfo().get_trans_name('ACCKernelsTrans')
ACC_DATA_TRANS = TransInfo().get_trans_name('ACCDataTrans')
PROFILE_TRANS = TransInfo().get_trans_name('ProfileRegionTrans')

# Whether or not to automatically add profiling calls around
# un-accelerated regions
_AUTO_PROFILE = True
# If routine names contain these substrings then we do not profile them
PROFILING_IGNORE = ["_init", "_rst", "alloc", "agrif", "flo_dom",
                    "ice_thd_pnd", "mpp_",
                    # These are small functions that the addition of profiling
                    # prevents from being in-lined (and then breaks any attempt
                    # to create OpenACC regions with calls to them)
                    "interp1", "interp2", "interp3", "integ_spline", "sbc_dcy"]

# Routines we do not attempt to add any OpenACC to (because it breaks with
# the PGI compiler or because it just isn't worth it)
ACC_IGNORE = ["turb_ncar", # Resulting code seg. faults with PGI 19.4
              "ice_dyn_adv", # No significant compute
              "iom_open", "iom_get_123d"]

# Currently fparser has no way of distinguishing array accesses from
# function calls if the symbol is imported from some other module.
# We therefore work-around this by keeping a list of known NEMO
# functions that must be excluded from within KERNELS regions.
NEMO_FUNCTIONS = set(["alpha_charn", "cd_neutral_10m", "solfrac", "One_on_L",
                      "psi_h", "psi_m", "psi_m_coare",
                      "psi_h_coare", "psi_m_ecmwf", "psi_h_ecmwf",
                      "Ri_bulk", "visc_air", "sbc_dcy"])


def valid_kernel(node):
    '''
    Whether the sub-tree that has `node` at its root is eligible to be
    enclosed within an OpenACC KERNELS directive.

    :param node: the node in the PSyIRe to check.
    :type node: :py:class:`psyclone.psyGen.Node`

    :returns: True if the sub-tree can be enclosed in a KERNELS region.
    :rtype: bool

    '''
    from psyclone.nemo import NemoKern, NemoLoop
    from psyclone.psyGen import IfBlock, CodeBlock, Schedule, Array, \
        Assignment, BinaryOperation, NaryOperation
    from fparser.two.utils import walk_ast
    from fparser.two import Fortran2003
    # Rather than walk the tree multiple times, look for both excluded node
    # types and possibly problematic operations
    excluded_node_types = (CodeBlock, IfBlock, BinaryOperation, NaryOperation,
                           NemoLoop)
    excluded_nodes = node.walk(excluded_node_types)

    for enode in excluded_nodes:
        if isinstance(enode, CodeBlock):
            return False

        if isinstance(enode, IfBlock):
            # We permit single-statement IF blocks or those that originate
            # from WHERE constructs inside KERNELS regions
            if "was_single_stmt" in enode.annotations or \
               "was_where" in enode.annotations:
                continue
            # When using CUDA managed memory, only allocated arrays are
            # automatically put onto the GPU (although
            # this includes those that are created by compiler-generated allocs
            # e.g. for automatic arrays). We assume that all arrays of rank 2 or
            # greater are dynamically allocated.
            arrays = enode.children[0].walk(Array)
            if any([len(array.children) == 1 for array in arrays]):
                return False

        # Need to check for SUM inside implicit loop, e.g.:
        #     vt_i(:, :) = SUM(v_i(:, :, :), dim = 3)
        if isinstance(enode, NemoLoop):
            if contains_unsupported_sum(enode.ast):
                return False

    # For now we don't support putting *just* the implicit loop assignment in
    # things like:
    #    if(do_this)my_array(:,:) = 1.0
    # inside a kernels region. Once we generate Fortran instead of modifying
    # the fparser2 parse tree this will become possible.
    if isinstance(node.parent, Schedule) and \
       isinstance(node.parent.parent, IfBlock) and \
       "was_single_stmt" in node.parent.parent.annotations:
        return False
    # Check that there are no derived-type references in the sub-tree.
    # We exclude NemoKern nodes from this check as calling .ast on
    # them causes problems.
    # Should not have to do this as derived-types should end up in CodeBlocks
    # but this does not happen for implicit loops.
    if not isinstance(node, NemoKern):
        if walk_ast([node.ast], [Fortran2003.Data_Ref]):
            return False

    # Finally, check that we haven't got any 'array accesses' that are in
    # fact function calls.
    refs = node.walk(Array)
    # Since kernels are leaves in the PSyIR, we need to separately check
    # their schedules for array references too.
    kernels = node.walk(NemoKern)
    for kern in kernels:
        sched = kern.get_kernel_schedule()
        refs += sched.walk((Array, NemoLoop))
    for ref in refs:
        if isinstance(ref, Array) and ref.name.lower() in NEMO_FUNCTIONS:
            # This reference has the name of a known function. Is it on
            # the LHS or RHS of an assignment?
            ref_parent = ref.parent
            if isinstance(ref_parent, Assignment) and ref is ref_parent.lhs:
                # We're writing to it so it's not a function call.
                continue
            return False
        if isinstance(ref, NemoLoop):
            if contains_unsupported_sum(ref.ast):
                return False
    return True


def contains_unsupported_sum(fpnode):
    '''
    Examines the fparser2 parse tree represented by fpnode and returns True
    if it contains a use of the SUM intrinisc with a 'dim' argument. (If
    such a construct is included in a KERNELS region then the code produced
    by v. 18.10 of the PGI compiler seg. faults.)

    :returns: True if SUM(array(:,:), dim=blah) is found, False otherwise.
    :rtype: bool

    '''
    from fparser.two.utils import walk_ast
    from fparser.two import Fortran2003
    intrinsics = walk_ast([fpnode], [Fortran2003.Intrinsic_Function_Reference])
    for intrinsic in intrinsics:
        if str(intrinsic.items[0]).lower() == "sum":
            # If there's only one argument then we'll just have a Name
            # and not an Actual_Arg_Spec_List (in which case we don't need to
            # check for the 'dim' argument).
            if isinstance(intrinsic.items[1],
                          Fortran2003.Actual_Arg_Spec_List):
                # items[1] contains the Actual_Arg_Spec_List
                actual_args = walk_ast(intrinsic.items[1].items,
                                       [Fortran2003.Actual_Arg_Spec])
                for arg in actual_args:
                    if str(arg.items[0]).lower() == "dim":
                        return True
    return False


def have_loops(nodes):
    '''
    Checks to see whether there are any Loops in the list of nodes and
    their sub-trees.

    :param nodes: list of PSyIR nodes to check for Loops.
    :type nodes: list of :py:class:`psyclone.psyGen.Node`
    :returns: True if a Loop is found, False otherwise.
    :rtype: bool

    '''
    from psyclone.psyGen import Loop
    for node in nodes:
        if node.walk(Loop):
            return True
    return False


def add_kernels(children):
    '''
    Walks through the PSyIR inserting OpenACC KERNELS directives at as
    high a level as possible.

    :param children: list of sibling Nodes in PSyIR that are candidates for \
                     inclusion in an ACC KERNELS region.
    :type children: list of :py:class:`psyclone.psyGen.Node`

    :returns: True if any KERNELS regions are successfully added.
    :rtype: bool

    '''
    from psyclone.psyGen import IfBlock, Loop
    added_kernels = False
    if not children:
        return added_kernels

    # Are we within a Loop of some kind?
    parent_loop = children[0].ancestor(Loop)

    node_list = []
    for child in children[:]:
        # Can this node be included in a kernels region?
        if not valid_kernel(child):
            # It can't so we put what we have so far inside a kernels region
            success = try_kernels_trans(node_list)
            added_kernels |= success
            # A node that cannot be included in a kernels region marks the
            # end of the current candidate region so reset the list.
            node_list = []
            # Now we go down a level and try again
            if isinstance(child, IfBlock):
                success1 = add_kernels(child.if_body)
                success2 = add_kernels(child.else_body)
                success = success1 or success2
            else:
                success = add_kernels(child.children)
            added_kernels |= success
        else:
            # We can - add this node to our list for the current region
            node_list.append(child)
    success = try_kernels_trans(node_list)
    added_kernels |= success

    return added_kernels


def add_profiling(children):
    '''
    Walks down the PSyIR and inserts the largest possible profiling regions.
    Code that contains OpenACC directives is excluded.

    :param children: sibling nodes in the PSyIR to which to attempt to add \
                     profiling regions.
    :type childre: list of :py:class:`psyclone.psyGen.Node`

    '''
    from psyclone.psyGen import IfBlock, ACCDirective, Assignment

    if not children:
        return

    node_list = []
    for child in children[:]:
        # Do we want this node to be included in a profiling region?
        if child.walk(ACCDirective):
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
    from psyclone.psyGen import CodeBlock, IfBlock
    if nodes and _AUTO_PROFILE:
        # Check whether we should be adding profiling inside this routine
        routine_name = nodes[0].root.invoke.name.lower()
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
            _, _ = PROFILE_TRANS.apply(nodes)
        except TransformationError:
            pass


def try_kernels_trans(nodes):
    '''
    Attempt to enclose the supplied list of nodes within a kernels
    region. If the transformation fails then the error message is
    reported but execution continues.

    :param nodes: list of Nodes to enclose within a Kernels region.
    :type nodes: list of :py:class:`psyclone.psyGen.Node`

    :returns: True if the transformation was successful, False otherwise.
    :rtype: bool

    '''
    from psyclone.psyGen import InternalError, Loop

    for node in nodes:
        if node.walk(Loop):
            break
    else:
        return False

    try:
        _, _ = ACC_KERN_TRANS.apply(nodes, default_present=False)
        return True
    except (TransformationError, InternalError) as err:
        print("Failed to transform nodes: {0}", nodes)
        print("Error was: {0}".format(str(err)))
        return False


def trans(psy):
    '''A PSyclone-script compliant transformation function. Applies
    OpenACC 'kernels' directives to NEMO code. (Data movement is
    assumed to be handled through PGI's managed-memory functionality.)

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

        # Attempt to add OpenACC directives unless this routine is one
        # we ignore
        if invoke.name.lower() not in ACC_IGNORE:
            print("Transforming invoke {0}:".format(invoke.name))
            add_kernels(sched.children)
        else:
            print("Addition of OpenACC to routine {0} disabled!".
                  format(invoke.name.lower()))

        # Add profiling instrumentation
        print("Adding profiling of non-OpenACC regions to routine {0}".
              format(invoke.name))
        add_profiling(sched.children)

        sched.view()

        invoke.schedule = sched

    return psy
