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
# Authors: R. W. Ford, A. R. Porter, N. Nobre and S. Siso, STFC Daresbury Lab

'''A transformation script that seeks to apply OpenACC KERNELS and optionally,
OpenACC DATA directives to NEMO style code. In order to use it you must first
install PSyclone. See README.md in the top-level directory.

Once you have psyclone installed, this may be used by doing:

 $ psyclone -s kernels_trans.py some_source_file.f90

Note that the Fortran source files provided to PSyclone must
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
from utils import (add_profiling, enhance_tree_information, inline_calls,
                   NOT_PERFORMANT, NEMO_MODULES_TO_IMPORT)
from psyclone.errors import InternalError
from psyclone.psyGen import TransInfo
from psyclone.psyir.nodes import (
    IfBlock, ArrayReference, Assignment, BinaryOperation, Loop, Routine,
    Literal, ACCLoopDirective)
from psyclone.psyir.transformations import (ACCKernelsTrans, ACCUpdateTrans,
                                            TransformationError, ProfileTrans)
from psyclone.transformations import ACCEnterDataTrans

# Set up some loop_type inference rules in order to reference useful domain
# loop constructs by name
Loop.set_loop_type_inference_rules({
        "lon": {"variable": "ji"},
        "lat": {"variable": "jj"},
        "levels": {"variable": "jk"},
        "tracers": {"variable": "jt"}
})

# Whether to chase the imported modules to improve symbol information (it can
# also be a list of module filenames to limit the chasing to only specific
# modules). This has to be used in combination with '-I' command flag in order
# to point to the module location directory. We also strongly recommend using
# the '--enable-cache' flag to reduce the performance overhead.
RESOLVE_IMPORTS = NEMO_MODULES_TO_IMPORT

# Get the PSyclone transformations we will use
ACC_KERN_TRANS = ACCKernelsTrans()
ACC_LOOP_TRANS = TransInfo().get_trans_name('ACCLoopTrans')
ACC_ROUTINE_TRANS = TransInfo().get_trans_name('ACCRoutineTrans')
ACC_EDATA_TRANS = ACCEnterDataTrans()
ACC_UPDATE_TRANS = ACCUpdateTrans()
PROFILE_TRANS = ProfileTrans()

# Whether or not to add profiling calls around unaccelerated regions
# N.B. this can inhibit PSyclone's ability to inline!
PROFILE_NONACC = False

# Whether or not to add OpenACC enter data and update directives to explicitly
# move data between host and device memory
ACC_EXPLICIT_MEM_MANAGEMENT = False

# List of all files that psyclone will skip processing
FILES_TO_SKIP = NOT_PERFORMANT

# Routines we do not attempt to add any OpenACC to (because it breaks with
# the Nvidia compiler or because it just isn't worth it)
ACC_IGNORE = ["day_mth",  # Just calendar operations
              "obs_surf_alloc", "oce_alloc",
              # Compiler fails w/ "Unsupported local variable"
              # Zero performance impact since outside execution path
              "copy_obfbdata", "merge_obfbdata",
              "turb_ncar",  # Transforming hurts performance
              "iom_open", "iom_get_123d", "iom_nf90_rp0123d",
              "trc_bc_ini", "p2z_ini", "p4z_ini", "sto_par_init",
              "bdytide_init", "bdy_init", "bdy_segs", "sbc_cpl_init",
              "asm_inc_init", "dia_obs_init"]  # Str handling, init routine


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
    # The Fortran routine which our parent represents
    routine_name = node.ancestor(Routine).name

    try:
        # Since we do this check on a node-by-node basis, we disable the
        # check that the 'region' contains a loop.
        ACC_KERN_TRANS.validate(node, options={"disable_loop_check":
                                               True})
    except TransformationError as err:
        log_msg(routine_name,
                f"Node rejected by ACCKernelTrans.validate: "
                f"{err.value}", node)
        return False

    # Allow for per-routine setting of what to exclude from within KERNELS
    # regions. This is because sometimes things work in one context but not
    # in another (with the Nvidia compiler).
    excluding = EXCLUDING.get(routine_name, EXCLUDING["default"])

    # Rather than walk the tree multiple times, look for both excluded node
    # types and possibly problematic operations
    excluded_types = (IfBlock, Loop)
    excluded_nodes = node.walk(excluded_types)

    for enode in excluded_nodes:

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

        elif isinstance(enode, Loop):
            # Heuristic:
            # We don't want to put loops around 3D loops into KERNELS regions
            # and nor do we want to put loops over levels into KERNELS regions
            # if they themselves contain several 2D loops.
            # In general, this heuristic will depend upon how many levels the
            # model configuration will contain.
            child = enode.loop_body[0] if enode.loop_body.children else None
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
                if (loop.variable.name == "jj" and
                        isinstance(loop.step_expr, Literal) and
                        isinstance(loop.loop_body[0], Loop) and
                        loop.loop_body[0].variable.name == "ji" and
                        isinstance(loop.loop_body[0].step_expr, Literal) and
                        len(loop.loop_body.children) == 1):
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


def trans(psyir):
    '''Applies OpenACC 'kernels' directives to NEMO code. Data movement can be
    handled manually or through CUDA's managed-memory functionality.

    :param psyir: the PSyIR of the provided file.
    :type psyir: :py:class:`psyclone.psyir.nodes.FileContainer`
    '''
    logging.basicConfig(filename='psyclone.log', filemode='w',
                        level=logging.INFO)

    for subroutine in psyir.walk(Routine):
        print(f"Transforming subroutine: {subroutine.name}")

        # In the lib_fortran file we annotate each routine of the SIGN_*
        # interface with the OpenACC Routine Directive
        if psyir.name == "lib_fortran.f90":
            if subroutine.name.lower().startswith("sign_"):
                ACC_ROUTINE_TRANS.apply(subroutine)
                continue

        # Attempt to add OpenACC directives unless we are ignoring this routine
        if subroutine.name.lower() not in ACC_IGNORE:
            print(f"Transforming {subroutine.name} with acc kernels")
            enhance_tree_information(subroutine)
            inline_calls(subroutine)
            have_kernels = add_kernels(subroutine.children)
            if have_kernels and ACC_EXPLICIT_MEM_MANAGEMENT:
                print(f"Transforming {subroutine.name} with acc enter data")
                ACC_EDATA_TRANS.apply(subroutine)
        else:
            print(f"Addition of OpenACC to routine {subroutine.name} "
                  f"disabled!")

        # Add required OpenACC update directives to every routine, including to
        # those with no device code and that execute exclusively on the host
        if ACC_EXPLICIT_MEM_MANAGEMENT:
            print(f"Transforming {subroutine.name} with acc update")
            ACC_UPDATE_TRANS.apply(subroutine)

        # Add profiling instrumentation
        if PROFILE_NONACC:
            print(f"Adding profiling to non-OpenACC regions in "
                  f"{subroutine.name}")
            add_profiling(subroutine.children)
