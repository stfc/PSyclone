# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2022-2026, Science and Technology Facilities Council.
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
# Authors: A. R. Porter, N. Nobre and S. Siso, STFC Daresbury Lab

''' Utilities file to parallelise Nemo code. '''

import os
from typing import List, Union

from psyclone.domain.common.transformations import KernelModuleInlineTrans
from psyclone.psyir.nodes import (
    Assignment, Loop, Directive, Node, Reference, CodeBlock, Call, Return,
    IfBlock, Routine, Schedule, IntrinsicCall, StructureReference)
from psyclone.psyir.symbols import DataSymbol, ArrayType
from psyclone.psyir.transformations import (
    ArrayAssignment2LoopsTrans, HoistLoopBoundExprTrans, HoistLocalArraysTrans,
    HoistTrans, InlineTrans, Maxval2LoopTrans, ProfileTrans,
    OMPMinimiseSyncTrans, Reference2ArrayRangeTrans,
    ScalarisationTrans, IncreaseRankLoopArraysTrans)
from psyclone.transformations import TransformationError

# USE statements to chase to gather additional symbol information.
NEMO_MODULES_TO_IMPORT = [
    "oce", "par_oce", "par_kind", "dom_oce", "phycst", "ice", "sbc_oce", "trc",
    "obs_fbm", "flo_oce", "sbc_ice", "wet_dry", "ldfslp", "zdfiwm", "zdfmxl",
    "bdy_oce", "zdf_oce", "zdfdrg", "ldftra", "crs", "sbcapr", "tideini",
    "ldfdyn", "sbcapr", "sbctide", "zdfgls", "sbcrnf", "sbcisf", "dynldf_iso",
    "stopts", "icb_oce", "domvvl", "sms_pisces", "zdfmfc", "abl", "ice1d",
    "sed", "p2zlim", "oce_trc", "p4zpoc", "tide_mod", "sbcwave", "isf_oce",
    "step_oce", "bdyice", "lbcnfd",
    "par_ice", "in_out_manager", "sbc_oce",
    # Needed to get nie0 in icevar
    "usrdef_sbc", "sbcblk", "sbccpl", "icealb",
    # Needed to get "smask0" in icesbc
    "fldread", "iom"
]

# Files that PSyclone could process but would reduce the performance.
NOT_PERFORMANT = [
    "bdydta.f90", "bdyvol.f90", "fldread.f90", "icbclv.f90", "icbthm.f90",
    "icbdia.f90", "icbini.f90", "icbstp.f90", "iom.f90", "iom_nf90.f90",
    "obs_grid.f90", "obs_averg_h2d.f90", "obs_profiles_def.f90",
    "obs_types.f90", "obs_read_prof.f90", "obs_write.f90", "tide_mod.f90",
    "zdfosm.f90", "obs_read_surf.f90",
]

DO_MODULE_INLINING = ["ice_var_vremap", "snw_ent", "ice_perm_eff",
                      "snw_var_vremap"]

# If routine names contain these substrings then we do not profile them
PROFILING_IGNORE = (
    DO_MODULE_INLINING +
    ["flo_dom", "macho", "mpp_", "nemo_gcm", "dyn_ldf"
     # These are small functions that the addition of profiling
     # prevents from being in-lined (and then breaks any attempt
     # to create OpenACC regions with calls to them)
     "interp1", "interp2", "interp3", "integ_spline", "sbc_dcy",
     "sum", "sign_", "ddpdd", "solfrac", "psyclone_cmp_int",
     "psyclone_cmp_char", "psyclone_cmp_logical"])

# Currently fparser has no way of distinguishing array accesses from statement
# functions, the following subroutines contains known statement functions
CONTAINS_STMT_FUNCTIONS = ["sbc_dcy"]

# These files change the results from the baseline when psyclone adds
# parallelisation directives
PARALLELISATION_ISSUES = [
    "ldfc1d_c2d.f90",
    "tramle.f90",
    "traqsr.f90",
]


def _it_should_be(symbol, of_type, instance):
    ''' Make sure that symbol has the datatype as provided.

    :param symbol: the symbol to check.
    :type symbol: :py:class:`psyclone.psyir.symbol.Symbol`
    :param type of_type: the datatype type that it must be.
    :param instance: the instance of Datatype to assign as the symbol datatype.
    :type instance: :py:class:`psyclone.psyir.symbol.DataType`

    '''
    if not isinstance(symbol, DataSymbol):
        symbol.specialise(DataSymbol, datatype=instance)
    elif not isinstance(symbol.datatype, of_type):
        symbol.datatype = instance


def inline_calls(schedule):
    '''
    Looks for all Calls within the supplied Schedule and attempts to:

      1. Find the source of the routine being called.
      2. Insert that source into the same Container as the call site.

    where each step is dependent upon the success of the previous one.

    Ideally (#924), this would then be followed by:

      3. Replace the call to the routine with the body of the routine.

    but currently this functionality is not robust enough for use here.

    TODO #924 - this could be InlineAllCallsTrans.apply(schedule,
                                                        excluding={})

    :param schedule: the schedule in which to search for Calls.
    :type schedule: :py:class:`psyclone.psyir.nodes.Schedule`

    '''
    ignore_codeblocks = ["bdy_dyn3d_frs", "bdy_dyn3d_spe", "bdy_dyn3d_zro",
                         "bdy_dyn3d_zgrad"]
    mod_inline_trans = KernelModuleInlineTrans()
    inline_trans = InlineTrans()
    for call in schedule.walk(Call):
        if isinstance(call, IntrinsicCall):
            continue
        rsym = call.routine.symbol
        name = rsym.name.lower()
        if name not in DO_MODULE_INLINING:
            continue
        if rsym.is_import or rsym.is_unresolved:
            try:
                mod_inline_trans.apply(call)
                print(f"Module-inlined routine '{name}'")
            except TransformationError as err:
                print(f"Module inline of '{name}' failed:\n{err}")
                continue

        try:
            options = {}
            if name in ignore_codeblocks:
                options["force"] = True
                print(f"Forcing inlining of '{name}'")
            inline_trans.apply(call, options=options)
            print(f"Inlined routine '{name}'")
        except TransformationError as err:
            print(f"Inlining of '{name}' failed:\n{err}")
            continue


def normalise_loops(
        schedule,
        hoist_local_arrays: bool = True,
        convert_array_notation: bool = True,
        loopify_array_intrinsics: bool = True,
        convert_range_loops: bool = True,
        scalarise_loops: bool = False,
        increase_array_ranks: bool = False,
        hoist_expressions: bool = True,
        ):
    ''' Normalise all loops in the given schedule so that they are in an
    appropriate form for the Parallelisation transformations to analyse
    them.

    :param schedule: the PSyIR Schedule to transform.
    :type schedule: :py:class:`psyclone.psyir.nodes.node`
    :param bool hoist_local_arrays: whether to hoist local arrays.
    :param bool convert_array_notation: whether to convert array notation
        to explicit loops.
    :param bool loopify_array_intrinsics: whether to convert intrinsics that
        operate on arrays to explicit loops (currently only maxval).
    :param bool convert_range_loops: whether to convert ranges to explicit
        loops.
    :param scalarise_loops: whether to attempt to convert arrays to scalars
        where possible, default is False.
    :param increase_array_ranks: whether to increase the rank of selected
        arrays.
    :param hoist_expressions: whether to hoist bounds and loop invariant
        statements out of the loop nest.
    '''
    if hoist_local_arrays and schedule.name not in CONTAINS_STMT_FUNCTIONS:
        # Apply the HoistLocalArraysTrans when possible, it cannot be applied
        # to files with statement functions because it will attempt to put the
        # allocate above it, which is not valid Fortran.
        try:
            HoistLocalArraysTrans().apply(schedule)
        except TransformationError:
            pass

    if convert_array_notation:
        for reference in schedule.walk(Reference):
            try:
                Reference2ArrayRangeTrans().apply(reference)
            except TransformationError:
                pass

    if loopify_array_intrinsics:
        for intr in schedule.walk(IntrinsicCall):
            if intr.intrinsic.name == "MAXVAL":
                try:
                    Maxval2LoopTrans().apply(intr)
                except TransformationError as err:
                    print(err.value)

    if convert_range_loops:
        # Convert all array implicit loops to explicit loops
        explicit_loops = ArrayAssignment2LoopsTrans()
        for assignment in schedule.walk(Assignment):
            # TODO #2951: Fix array assignments with dependencies
            if schedule.name in ("fld_def",):
                continue
            try:
                explicit_loops.apply(
                    assignment, options={'verbose': True})
            except TransformationError:
                pass

    if scalarise_loops:
        # Apply scalarisation to every loop. Execute this in reverse order
        # as sometimes we can scalarise earlier loops if following loops
        # have already been scalarised.
        loops = schedule.walk(Loop)
        loops.reverse()
        scalartrans = ScalarisationTrans()
        for loop in loops:
            scalartrans.apply(loop)

    if increase_array_ranks:
        increase_rank_and_reorder_nemov5_loops(schedule)

    if hoist_expressions:
        # First hoist all possible expressions
        for loop in schedule.walk(Loop):
            try:
                HoistLoopBoundExprTrans().apply(loop)
            except TransformationError:
                pass

        # Hoist all possible assignments (in reverse order so the inner loop
        # constants are hoisted all the way out if possible)
        for loop in reversed(schedule.walk(Loop)):
            for statement in list(loop.loop_body):
                try:
                    HoistTrans().apply(statement)
                except TransformationError:
                    pass

    # TODO #1928: In order to perform better on the GPU, nested loops with two
    # sibling inner loops need to be fused or apply loop fission to the
    # top level. This would allow the collapse clause to be applied.


def increase_rank_and_reorder_nemov5_loops(routine: Routine):
    ''' This method increases the rank of temporary arrays used inside selected
    loops (in order to parallelise the outer loop without overlapping them)
    and then rearranges the outer loop next to the inner ones (in order to
    collapse them), so that more parallelism can be leverage. This is useful
    in GPU contexts, but it increases the memory footprint and may not be
    beneficial for caching-architectures.

    :param routine: the target routine.

    '''
    irlatrans = IncreaseRankLoopArraysTrans()

    # Map of routines and arrays
    selection = {
        "dyn_zdf": ['zwd', 'zwi', 'zws'],
        "tra_zdf_imp": ['zwd', 'zwi', 'zws', 'zwt']
    }

    if routine.name not in selection:
        return

    for outer_loop in routine.walk(Loop, stop_type=Loop):
        if outer_loop.variable.name == "jj":
            # Increase the rank of the temporary arrays in this loop
            irlatrans.apply(outer_loop, arrays=selection[routine.name])
            # Now reorder the code
            for child in outer_loop.loop_body[:]:
                # Move the contents of the jj loop outside it
                outer_loop.parent.addchild(child.detach(),
                                           index=outer_loop.position)
                # Add a new jj loop around each inner loop that is not 'jn'
                target_loop = []
                for inner_loop in child.walk(Loop, stop_type=Loop):
                    if inner_loop.variable.name != "jn":
                        target_loop.append(inner_loop)
                    else:
                        for next_loop in inner_loop.loop_body.walk(
                                            Loop, stop_type=Loop):
                            target_loop.append(next_loop)
                for inner_loop in target_loop:
                    if isinstance(inner_loop.loop_body[0], Loop):
                        inner_loop = inner_loop.loop_body[0]
                    inner_loop.replace_with(
                        Loop.create(
                            outer_loop.variable,
                            outer_loop.start_expr.copy(),
                            outer_loop.stop_expr.copy(),
                            outer_loop.step_expr.copy(),
                            children=[inner_loop.copy()]
                        )
                    )
            # Remove the now empty jj loop
            outer_loop.detach()


def insert_explicit_loop_parallelism(
        schedule,
        region_directive_trans=None,
        loop_directive_trans=None,
        collapse: bool = True,
        privatise_arrays: bool = False,
        asynchronous_parallelism: bool = False,
        uniform_intrinsics_only: bool = False,
        enable_reductions: bool = False,
        ):
    ''' For each loop in the schedule that doesn't already have a Directive
    as an ancestor, attempt to insert the given region and loop directives.

    :param schedule: the PSyIR Schedule to transform.
    :type schedule: :py:class:`psyclone.psyir.nodes.node`
    :param region_directive_trans: PSyclone transformation that inserts the
        region directive.
    :type region_directive_trans: \
        :py:class:`psyclone.transformation.Transformation`
    :param loop_directive_trans: PSyclone transformation that inserts the
        loop parallelisation directive.
    :type loop_directive_trans: \
        :py:class:`psyclone.transformation.Transformation`
    :param collapse: whether to attempt to insert the collapse clause to as
        many nested loops as possible.
    :param privatise_arrays: whether to attempt to privatise arrays that cause
        write-write race conditions.
    :param asynchronous_parallelism: whether to attempt to add asynchronocity
    to the parallel sections.
    :param uniform_intrinsics_only: if True it prevent offloading loops
        with non-reproducible device intrinsics.
    :param enable_reductions: whether to enable generation of reduction
        clauses automatically.

    '''
    nemo_v4 = os.environ.get('NEMOV4', False)
    # TODO #2937: These are both in "dynspg_ts.f90", they have a WaW dependency
    # but we currently ignore these.
    if schedule.name in ("ts_wgt", "ts_rst"):
        return

    enable_nowaits = False
    if asynchronous_parallelism and not schedule.walk(StructureReference):
        # TODO #3220: Explore the cause of the async issues
        enable_nowaits = True

    # Add the parallel directives in each loop
    for loop in schedule.walk(Loop):
        if loop.ancestor(Directive):
            continue  # Skip if an outer loop is already parallelised

        opts = {"collapse": collapse, "privatise_arrays": privatise_arrays,
                "verbose": True, "nowait": enable_nowaits,
                "enable_reductions": enable_reductions}

        if uniform_intrinsics_only:
            opts["device_string"] = "nvfortran-uniform"

        routine_name = loop.ancestor(Routine).name.lower()

        if ('dyn_spg' in routine_name and len(loop.walk(Loop)) > 2):
            loop.append_preceding_comment(
                "PSyclone: Loop not parallelised because it is in 'dyn_spg' "
                "and is not the inner loop")
            continue

        if nemo_v4:
            # Skip if it is an array operation loop on an ice routine if along
            # the third dim or higher or if the loop nests a loop over ice
            # points (npti) or if the loop and array dims do not match.
            # In addition, they often nest ice linearised loops (npti)
            # which we'd rather parallelise
            if ('ice' in routine_name
                and isinstance(loop.stop_expr, IntrinsicCall)
                and (loop.stop_expr.intrinsic in (
                        IntrinsicCall.Intrinsic.UBOUND,
                        IntrinsicCall.Intrinsic.SIZE))
                and (len(loop.walk(Loop)) > 2
                     or any(ref.symbol.name in ('npti',)
                            for lp in loop.loop_body.walk(Loop)
                            for ref in lp.stop_expr.walk(Reference))
                     or (str(len(loop.walk(Loop))) !=
                         loop.stop_expr.arguments[1].value))):
                loop.append_preceding_comment(
                    "PSyclone: ICE Loop not parallelised for performance"
                    "reasons")
                continue

            # Skip if looping over ice categories, ice or snow layers as these
            # have small trip counts if they are not collapsed
            if not collapse and any(
                    ref.symbol.name in ('jpl', 'nlay_i', 'nlay_s')
                    for ref in loop.stop_expr.walk(Reference)
            ):
                loop.append_preceding_comment(
                    "PSyclone: Loop not parallelised because stops at 'jpl',"
                    " 'nlay_i' or 'nlay_s' and is not collapsed.")
                continue

        else:
            # In NEMOv5 add the necessary explicit private symbols in icethd
            # in order to parallelise the outer loop
            all_work_arrays = []
            for sym in loop.ancestor(Routine).symbol_table.datasymbols:
                if sym.name.startswith("z") and sym.is_array:
                    all_work_arrays.append(sym)

            all_1d_work_arrays = []
            for sym in all_work_arrays:
                if isinstance(sym.datatype, ArrayType):
                    shape = sym.datatype.shape
                else:
                    shape = sym.datatype.partial_datatype.shape
                if len(shape) == 1:
                    all_1d_work_arrays.append(sym)

            if routine_name == "ice_thd_zdf_bl99":
                if isinstance(loop.stop_expr, Reference):
                    if (loop.stop_expr.symbol.name == "npti" or
                            loop.variable.name == "ji" or
                            (loop.variable.name == "jj" and
                             isinstance(loop.loop_body[0], Loop) and
                             loop.loop_body[0].variable.name == "ji")):
                        opts["force_private"] = ['zdiagbis', 'zindtbis',
                                                 'zindterm', 'ztib', 'ztrid',
                                                 'ztsb']
                        opts["ignore_dependencies_for"] = opts["force_private"]
            if routine_name == "ice_thd_sal":
                if loop.variable.name in ["ji", "jj"]:
                    opts["force_private"] = [
                        "z_mid", "z_edge", "zds", "zv_br", "zra", "zperm_eff",
                        "zw_br", "zmsk", "zs_br"]
                    opts["ignore_dependencies_for"] = opts["force_private"]
            if routine_name == "ice_thd_da":
                if loop.variable.name in ["ji", "jj"]:
                    opts["force_private"] = [sym.name for sym in
                                             all_1d_work_arrays]
                    opts["ignore_dependencies_for"] = opts["force_private"]
            if routine_name == "ice_thd_dh":
                if loop.variable.name in ["ji", "jj"]:
                    opts["force_private"] = ["icount"] + [sym.name for sym in
                                                          all_1d_work_arrays]
                    opts["ignore_dependencies_for"] = opts["force_private"]
            if routine_name == "pnd_lev":
                if loop.variable.name in ["ji", "jj"]:
                    opts["force_private"] = ["zv_br"]
                    opts["ignore_dependencies_for"] = opts["force_private"]
            if routine_name.startswith("lbc_lnk_"):
                opts["ignore_dependencies_for"] = ["ptab%pt4d"]
        try:
            # First check that the region_directive is feasible for this region
            if region_directive_trans:
                region_directive_trans.validate(loop, options=opts)

            # If it is, apply the parallelisation directive
            loop_directive_trans.apply(loop, options=opts)

            # And if successful, the region directive on top.
            if region_directive_trans:
                region_directive_trans.apply(loop.parent.parent, options=opts)
        except TransformationError:
            # This loop cannot be transformed, proceed to next loop.
            # The parallelisation restrictions will be explained with a comment
            # associated to the loop in the generated output.
            continue

    # If we are adding asynchronous parallelism then we now try to minimise
    # the number of barriers.
    if enable_nowaits:
        minsync_trans = OMPMinimiseSyncTrans()
        minsync_trans.apply(schedule)


def add_profiling(children: Union[List[Node], Schedule]):
    '''
    Walks down the PSyIR and inserts the largest possible profiling regions
    in place. Code inside functions or that contains directives is excluded.

    :param children: a Schedule or sibling nodes in the PSyIR to which to
        attempt to add profiling regions.

    '''
    if children and isinstance(children, Schedule):
        # If we are given a Schedule, we look at its children.
        children = children.children

    if not children:
        return

    # We do not want profiling calipers inside functions (such as the
    # PSyclone-generated comparison functions).
    parent_routine = children[0].ancestor(Routine)
    if parent_routine and parent_routine.return_symbol:
        return

    node_list = []
    for child in children[:]:
        # Do we want this node to be included in a profiling region?
        if child.walk((Directive, Return)):
            # It contains a directive or return statement so we put what we
            # have so far inside a profiling region.
            add_profile_region(node_list)
            # A node that is not included in a profiling region marks the
            # end of the current candidate region so reset the list.
            node_list = []
            # Now we go down a level and try again without attempting to put
            # profiling below directives or within Assignments
            if isinstance(child, IfBlock):
                add_profiling(child.if_body)
                add_profiling(child.else_body)
            elif not isinstance(child, (Assignment, Directive)):
                add_profiling(child.children)
        else:
            # We can add this node to our list for the current region
            node_list.append(child)
    add_profile_region(node_list)


def add_profile_region(nodes):
    '''
    Attempt to put the supplied list of nodes within a profiling region.

    :param nodes: list of sibling PSyIR nodes to enclose.
    :type nodes: list of :py:class:`psyclone.psyir.nodes.Node`

    '''
    if nodes:
        # Check whether we should be adding profiling inside this routine
        routine_name = nodes[0].ancestor(Routine).name.lower()
        if any(ignore in routine_name for ignore in PROFILING_IGNORE):
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
            ProfileTrans().apply(nodes)
        except TransformationError:
            pass
