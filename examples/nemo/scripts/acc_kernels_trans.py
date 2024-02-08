# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2018-2024, Science and Technology Facilities Council.
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
# Authors: R. W. Ford, A. R. Porter, N. Nobre and S. Siso, STFC Daresbury Lab

'''A transformation script that seeks to apply OpenACC DATA and KERNELS
directives to NEMO style code. In order to use it you must first install
PSyclone. See README.md in the top-level directory.

Once you have psyclone installed, this may be used by doing:

 $ psyclone -api nemo -s kernels_trans.py some_source_file.f90

This should produce a lot of output, ending with generated
Fortran. Note that the Fortran source files provided to PSyclone must
have already been preprocessed (if required).

The transformation script attempts to insert Kernels directives at the
highest possible location(s) in the schedule tree (i.e. to enclose as
much code as possible in each Kernels region). However, due to
limitations in the Nvidia compiler, we must take care to exclude certain
nodes (such as If blocks) from within Kernel regions. If a proposed
region is found to contain such a node (by the ``valid_acc_kernel``
routine) then the script moves a level down the tree and then repeats
the process of attempting to create the largest possible Kernel region.

Tested with the NVIDIA HPC SDK version 23.7.
'''

import logging
from utils import add_profiling
from psyclone.errors import InternalError
from psyclone.nemo import NemoInvokeSchedule,  NemoLoop
from psyclone.psyGen import TransInfo
from psyclone.psyir.nodes import IfBlock, CodeBlock, Schedule, \
    ArrayReference, Assignment, BinaryOperation, Loop, WhileLoop, \
    Literal, Return, Call, ACCLoopDirective
from psyclone.psyir.transformations import TransformationError, ProfileTrans, \
                                           ACCUpdateTrans
from psyclone.transformations import ACCEnterDataTrans

# Get the PSyclone transformations we will use
ACC_KERN_TRANS = TransInfo().get_trans_name('ACCKernelsTrans')
ACC_LOOP_TRANS = TransInfo().get_trans_name('ACCLoopTrans')
ACC_ROUTINE_TRANS = TransInfo().get_trans_name('ACCRoutineTrans')
ACC_EDATA_TRANS = ACCEnterDataTrans()
ACC_UPDATE_TRANS = ACCUpdateTrans()
PROFILE_TRANS = ProfileTrans()

# Whether or not to add profiling calls around unaccelerated regions
PROFILE_NONACC = True

# Whether or not to add OpenACC enter data and update directives to explicitly
# move data between host and device memory
ACC_EXPLICIT_MEM_MANAGEMENT = False

# If routine names contain these substrings then we do not profile them
PROFILING_IGNORE = ["_init", "_rst", "alloc", "agrif", "flo_dom",
                    "macho", "mpp_", "nemo_gcm",
                    # These are small functions that the addition of profiling
                    # prevents from being in-lined (and then breaks any attempt
                    # to create OpenACC regions with calls to them)
                    "interp1", "interp2", "interp3", "integ_spline", "sbc_dcy",
                    "sum", "sign_"]

# Routines we do not attempt to add any OpenACC to (because it breaks with
# the Nvidia compiler or because it just isn't worth it)
ACC_IGNORE = ["day_mth",  # Just calendar operations
              "obs_surf_alloc", "oce_alloc",
              # Compiler fails w/ "Unsupported local variable"
              # Zero performance impact since outside execution path
              "copy_obfbdata", "merge_obfbdata",
              "turb_ncar",  # Transforming hurts performance
              "iom_open", "iom_get_123d", "iom_nf90_rp0123d",
              "trc_bc_ini", "p2z_ini", "p4z_ini"]  # Str handling, init routine

# Currently fparser has no way of distinguishing array accesses from
# function calls if the symbol is imported from some other module.
# We therefore work-around this by keeping a list of known NEMO
# functions that must be excluded from within KERNELS regions.
NEMO_FUNCTIONS = ["alpha_charn", "cd_neutral_10m", "cpl_freq", "cp_air",
                  "eos_pt_from_ct", "gamma_moist", "l_vap",
                  "sbc_dcy", "solfrac", "psi_h", "psi_m", "psi_m_coare",
                  "psi_h_coare", "psi_m_ecmwf", "psi_h_ecmwf", "q_sat",
                  "rho_air", "visc_air", "sbc_dcy", "glob_sum",
                  "glob_sum_full", "ptr_sj", "ptr_sjk", "interp1", "interp2",
                  "interp3", "integ_spline"]


class ExcludeSettings():
    '''
    Class to hold settings on what to exclude from OpenACC KERNELS regions.

    :param Optional[dict[str, bool]] settings: map of settings to override.

    '''
    def __init__(self, settings=None):
        if settings is None:
            settings = {}
        # Whether we exclude IFs where the logical expression is not a
        # comparison operation.
        self.ifs_scalars = settings.get("ifs_scalars", False)


# Routines which are exceptions to the OpenACC Kernels regions exclusion rules.
EXCLUDING = {"default": ExcludeSettings(),
             # Exclude for better GPU performance (requires further analysis).
             "dyn_spg_ts": ExcludeSettings({"ifs_scalars": True}),
             "tra_zdf_imp": ExcludeSettings({"ifs_scalars": True}),
             # Exclude due to compiler bug preventing CPU multicore executions.
             "dom_vvl_init": ExcludeSettings({"ifs_scalars": True})}


def log_msg(name, msg, node):
    '''
    Log a message indicating why a transformation could not be performed.

    :param str name: the name of the routine.
    :param str msg: the message to log.
    :param node: the PSyIR node that prevented the transformation.
    :type node: :py:class:`psyclone.psyir.nodes.Node`

    '''
    # Create a str representation of the position of the problematic node
    # in the PSyIR tree.
    node_strings = []
    parent = node
    while parent:
        node_strings.append(parent.node_str(colour=False))
        parent = parent.parent
    node_strings.reverse()
    location = "->".join(node_strings)
    # Log the message
    logging.info("%s: %s: %s", name, msg, location)


def valid_acc_kernel(node):
    '''
    Whether the sub-tree that has `node` at its root is eligible to be
    enclosed within an OpenACC KERNELS directive.

    :param node: the node in the PSyIRe to check.
    :type node: :py:class:`psyclone.psyir.nodes.Node`

    :returns: True if the sub-tree can be enclosed in a KERNELS region.
    :rtype: bool

    '''
    # The Fortran routine which our parent Invoke represents
    routine_name = node.ancestor(NemoInvokeSchedule).invoke.name

    # Allow for per-routine setting of what to exclude from within KERNELS
    # regions. This is because sometimes things work in one context but not
    # in another (with the Nvidia compiler).
    excluding = EXCLUDING.get(routine_name, EXCLUDING["default"])

    # Rather than walk the tree multiple times, look for both excluded node
    # types and possibly problematic operations
    excluded_types = (CodeBlock, Return, Call, IfBlock, NemoLoop, WhileLoop)
    excluded_nodes = node.walk(excluded_types)

    for enode in excluded_nodes:
        if isinstance(enode, Call) and enode.is_available_on_device():
            continue

        if isinstance(enode, (CodeBlock, Return, Call, WhileLoop)):
            log_msg(routine_name,
                    f"region contains {type(enode).__name__}", enode)
            return False

        if isinstance(enode, IfBlock):
            # We permit IF blocks originating from WHERE constructs and
            # single-statement IF blocks containing a Loop in KERNELS regions
            if "was_where" in enode.annotations or \
               "was_single_stmt" in enode.annotations and enode.walk(Loop):
                continue

            arrays = enode.condition.walk(ArrayReference)
            # We exclude if statements where the condition expression does
            # not refer to arrays at all as this may cause compiler issues
            # (get "Missing branch target block") or produce faster code.
            if not arrays and excluding.ifs_scalars and \
               not isinstance(enode.condition, BinaryOperation):
                log_msg(routine_name, "IF references scalars", enode)
                return False
            # When using CUDA Unified Memory, only allocated arrays reside in
            # shared memory (including those that are created by compiler-
            # -generated allocs, e.g. for automatic arrays). We assume that all
            # arrays of rank 2 or greater are dynamically allocated, whereas 1D
            # arrays are often static in NEMO. Hence, we disallow IFs where the
            # logical expression involves the latter.
            if any(len(array.children) == 1 for array in arrays):
                log_msg(routine_name,
                        "IF references 1D arrays that may be static", enode)
                return False

        elif isinstance(enode, NemoLoop):
            # Heuristic:
            # We don't want to put loops around 3D loops into KERNELS regions
            # and nor do we want to put loops over levels into KERNELS regions
            # if they themselves contain several 2D loops.
            # In general, this heuristic will depend upon how many levels the
            # model configuration will contain.
            child = enode.loop_body[0]
            if isinstance(child, Loop) and child.loop_type == "levels":
                # We have a loop around a loop over levels
                log_msg(routine_name, "Loop is around a loop over levels",
                        enode)
                return False
            if enode.loop_type == "levels" and \
               len(enode.loop_body.children) > 1:
                # The body of the loop contains more than one statement.
                # How many distinct loop nests are there?
                loop_count = 0
                for child in enode.loop_body.children:
                    if child.walk(Loop):
                        loop_count += 1
                        if loop_count > 1:
                            log_msg(routine_name,
                                    "Loop over levels contains several "
                                    "other loops", enode)
                            return False

    # For now we don't support putting *just* the implicit loop assignment in
    # things like:
    #    if(do_this)my_array(:,:) = 1.0
    # inside a kernels region. Once we generate Fortran instead of modifying
    # the fparser2 parse tree this will become possible.
    if isinstance(node.parent, Schedule) and \
       isinstance(node.parent.parent, IfBlock) and \
       "was_single_stmt" in node.parent.parent.annotations:
        log_msg(routine_name, "Would split single-line If statement", node)
        return False

    # Finally, check that we haven't got any 'array accesses' that are in
    # fact function calls.
    refs = node.walk(ArrayReference)
    for ref in refs:
        # Check if this reference has the name of a known function and if that
        # reference appears outside said known function.
        if ref.name.lower() in NEMO_FUNCTIONS and \
           ref.name.lower() != routine_name.lower():
            log_msg(routine_name,
                    f"Loop contains function call: {ref.name}", ref)
            return False
    return True


def add_kernels(children):
    '''
    Walks through the PSyIR inserting OpenACC KERNELS directives at as
    high a level as possible.

    :param children: list of sibling Nodes in PSyIR that are candidates for \
                     inclusion in an ACC KERNELS region.
    :type children: list of :py:class:`psyclone.psyir.nodes.Node`

    :returns: True if any KERNELS regions are successfully added.
    :rtype: bool

    '''
    added_kernels = False
    if not children:
        return added_kernels

    node_list = []
    for child in children[:]:
        # Can this node be included in a kernels region?
        if not valid_acc_kernel(child):
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
            elif isinstance(child, Loop):
                success = add_kernels(child.loop_body)
            else:
                success = add_kernels(child.children)
            added_kernels |= success
        else:
            # We can add this node to our list for the current region
            node_list.append(child)
    success = try_kernels_trans(node_list)
    added_kernels |= success

    return added_kernels


def try_kernels_trans(nodes):
    '''
    Attempt to enclose the supplied list of nodes within a kernels
    region. If the transformation fails then the error message is
    reported but execution continues.

    :param nodes: list of Nodes to enclose within a Kernels region.
    :type nodes: list of :py:class:`psyclone.psyir.nodes.Node`

    :returns: True if the transformation was successful, False otherwise.
    :rtype: bool

    '''
    # We only enclose the proposed region if it contains a loop.
    have_loop = False
    for node in nodes:
        if node.walk(Loop):
            have_loop = True
            break
        assigns = node.walk(Assignment)
        for assign in assigns:
            if assign.is_array_assignment:
                have_loop = True
                break
    if not have_loop:
        return False

    try:
        ACC_KERN_TRANS.apply(nodes, {"default_present": False})

        # Put COLLAPSE on any tightly-nested loops over latitude and longitude.
        for node in nodes:
            loops = node.walk(Loop)
            for loop in loops:
                if loop.ancestor(ACCLoopDirective):
                    # We've already transformed a parent Loop so skip this one.
                    continue
                # We put a COLLAPSE(2) clause on any perfectly-nested lat-lon
                # loops that have a Literal value for their step. The latter
                # condition is necessary to avoid compiler errors.
                if loop.loop_type == "lat" and \
                   isinstance(loop.step_expr, Literal) and \
                   isinstance(loop.loop_body[0], Loop) and \
                   loop.loop_body[0].loop_type == "lon" and \
                   isinstance(loop.loop_body[0].step_expr, Literal) and \
                   len(loop.loop_body.children) == 1:
                    try:
                        ACC_LOOP_TRANS.apply(loop, {"collapse": 2})
                    except (TransformationError) as err:
                        print(f"Failed to collapse lat-lon loop: {loop}")
                        print(f"Error was: {err}")

        return True
    except (TransformationError, InternalError) as err:
        print(f"Failed to insert acc kernels around nodes: {nodes}")
        print(f"Error was: {err}")
        return False


def trans(psy):
    '''A PSyclone-script compliant transformation function. Applies
    OpenACC 'kernels' directives to NEMO code. Data movement can be
    handled manually or through CUDA's managed-memory functionality.

    :param psy: The PSy layer object to apply transformations to.
    :type psy: :py:class:`psyclone.psyGen.PSy`

    '''
    logging.basicConfig(filename='psyclone.log', filemode='w',
                        level=logging.INFO)

    invoke_list = "\n".join([str(name) for name in psy.invokes.names])
    print(f"Invokes found:\n{invoke_list}\n")

    for invoke in psy.invokes.invoke_list:

        sched = invoke.schedule
        if not sched:
            print(f"Invoke {invoke.name} has no Schedule! Skipping...")
            continue

        # In the lib_fortran file we annotate each routine that does not
        # have a Loop or unsupported Calls with the OpenACC Routine Directive
        if psy.name == "psy_lib_fortran_psy":
            if not invoke.schedule.walk(Loop):
                calls = invoke.schedule.walk(Call)
                if all(call.is_available_on_device() for call in calls):
                    # SIGN_ARRAY_1D has a CodeBlock because of a WHERE without
                    # array notation. (TODO #717)
                    ACC_ROUTINE_TRANS.apply(sched, options={"force": True})
                    continue

        # Attempt to add OpenACC directives unless we are ignoring this routine
        if invoke.name.lower() not in ACC_IGNORE:
            print(f"Transforming {invoke.name} with acc kernels")
            have_kernels = add_kernels(sched.children)
            if have_kernels and ACC_EXPLICIT_MEM_MANAGEMENT:
                print(f"Transforming {invoke.name} with acc enter data")
                ACC_EDATA_TRANS.apply(sched)
        else:
            print(f"Addition of OpenACC to routine {invoke.name} disabled!")

        # Add required OpenACC update directives to every routine, including to
        # those with no device code and that execute exclusively on the host
        if ACC_EXPLICIT_MEM_MANAGEMENT:
            print(f"Transforming {invoke.name} with acc update")
            ACC_UPDATE_TRANS.apply(sched)

        # Add profiling instrumentation
        if PROFILE_NONACC:
            print(f"Adding profiling to non-OpenACC regions in {invoke.name}")
            add_profiling(sched.children)

    return psy
