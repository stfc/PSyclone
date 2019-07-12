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


# Get the PSyclone transformations we will use
ACC_KERN_TRANS = TransInfo().get_trans_name('ACCKernelsTrans')
ACC_DATA_TRANS = TransInfo().get_trans_name('ACCDataTrans')

# Currently fparser has no way of distinguishing array accesses from
# function calls if the symbol is imported from some other module.
# We therefore work-around this by keeping a list of known NEMO
# functions that must be excluded from within KERNELS regions.
NEMO_FUNCTIONS = set(["solfrac", "alfa_charn", "One_on_L", "psi_m_coare",
                      "psi_h_coare", "visc_air", "psi_m_ecmwf", "psi_h_ecmwf",
                      "Ri_bulk"])


def valid_kernel(node):
    '''
    Whether the sub-tree that has `node` at its root is eligible to be
    enclosed within an OpenACC KERNELS directive.

    :param node: the node in the PSyIRe to check.
    :type node: :py:class:`psyclone.psyGen.Node`

    :returns: True if the sub-tree can be enclosed in a KERNELS region.
    :rtype: bool

    '''
    from psyclone.nemo import NemoKern
    from psyclone.psyGen import IfBlock, CodeBlock, Schedule, Array, \
        Assignment, Operation, BinaryOperation, NaryOperation
    from fparser.two.utils import walk_ast
    from fparser.two import Fortran2003
    # Rather than walk the tree multiple times, look for both excluded node
    # types and possibly problematic operations
    excluded_node_types = (CodeBlock, IfBlock, BinaryOperation, NaryOperation)
    excluded_nodes = node.walk([node], excluded_node_types)

    for node in excluded_nodes:
        if isinstance(node, (CodeBlock, IfBlock)):
            return False
        #if isinstance(node, Operation):
        #    if (node.operator == BinaryOperation.Operator.SUM or
        #        node.operator == NaryOperation.Operator.SUM):
        #        import pdb; pdb.set_trace()

    # For now we don't support putting things like:
    #    if(do_this)my_array(:,:) = 1.0
    # inside a kernels region. Once we generate Fortran instead of modifying
    # the fparser2 parse tree this will become possible.
    if isinstance(node.parent, Schedule) and \
       isinstance(node.parent.parent, IfBlock) and \
       "was_single_stmt" in node.parent.parent._annotations:
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
    refs = node.walk([node], Array)
    # Since kernels are leaves in the PSyIR, we need to separately check
    # their schedules for array references too.
    kernels = node.walk([node], NemoKern)
    for kern in kernels:
        sched = kern.get_kernel_schedule()
        refs += sched.walk(sched.children, Array)
    if refs:
        for ref in refs:
            if ref.name.lower() in NEMO_FUNCTIONS:
                # This reference has the name of a known function. Is it on
                # the LHS or RHS of an assignment?
                ref_parent = ref.parent
                if isinstance(ref_parent, Assignment) and ref is ref_parent.lhs:
                    # We're writing to it so it's not a function call.
                    continue
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
        if node.walk([node], NemoLoop):
            return True
    return False


def add_kernels(children):
    '''
    Walks through the PSyIR inserting OpenACC KERNELS directives at as
    high a level as possible.

    :param children: list of sibling Nodes in PSyIR that are candidates for \
                     inclusion in an ACC KERNELS region.
    :type children: list of :py:class:`psyclone.psyGen.Node`

    '''
    from psyclone.psyGen import IfBlock
    if not children:
        return

    node_list = []
    for child in children[:]:
        # Can this node be included in a kernels region?
        if not valid_kernel(child):
            # It can't so we put what we have so far inside a kernels region
            # provided it contains one or more loops
            if have_loops(node_list):
                try_kernels_trans(node_list)
            # A node that cannot be included in a kernels region marks the
            # end of the current candidate region so reset the list.
            node_list = []
            # Now we go down a level and try again
            if isinstance(child, IfBlock):
                add_kernels(child.if_body)
                add_kernels(child.else_body)
            else:
                add_kernels(child.children)
        else:
            # Otherwise we add this node to our list for the current region
            node_list.append(child)
    if have_loops(node_list):
        try_kernels_trans(node_list)


def try_kernels_trans(nodes):
    '''
    Attempt to enclose the supplied list of nodes within a kernels
    region. If the transformation fails then the error message is
    reported but execution continues.

    :param nodes: list of Nodes to enclose within a Kernels region.
    :type nodes: list of :py:class:`psyclone.psyGen.Node`

    '''
    from psyclone.psyGen import InternalError
    from psyclone.transformations import TransformationError
    try:
        _, _ = ACC_KERN_TRANS.apply(nodes, default_present=False)
    except (TransformationError, InternalError) as err:
        print("Failed to transform nodes: {0}", nodes)
        print("Error was: {0}".format(str(err)))


def trans(psy):
    '''A PSyclone-script compliant transformation function. Applies
    OpenACC 'kernels' directives to NEMO code. (Data movement is
    assumed to be handled through PGI's managed-memory functionality.)

    :param psy: The PSy layer object to apply transformations to.
    :type psy: :py:class:`psyclone.psyGen.PSy`
    '''
    from psyclone.psyGen import ACCDirective

    print("Invokes found:\n{0}\n".format(
        "\n".join([str(name) for name in psy.invokes.names])))

    for invoke in psy.invokes.invoke_list:

        sched = invoke.schedule
        if not sched:
            print("Invoke {0} has no Schedule! Skipping...".
                  format(invoke.name))
            continue

        add_kernels(sched.children)
        sched.view()

        invoke.schedule = sched
