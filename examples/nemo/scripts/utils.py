# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2022-2025, Science and Technology Facilities Council.
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
    Assignment, Loop, Directive, Node, Reference, CodeBlock, ArrayReference,
    Call, Return, IfBlock, Routine, Schedule, IntrinsicCall,
    StructureReference)
from psyclone.psyir.symbols import (
    DataSymbol, INTEGER_TYPE, ScalarType, RoutineSymbol)
from psyclone.psyir.transformations import (
    ArrayAssignment2LoopsTrans, HoistLoopBoundExprTrans, HoistLocalArraysTrans,
    HoistTrans, InlineTrans, Maxval2LoopTrans, ProfileTrans,
    OMPMinimiseSyncTrans, Reference2ArrayRangeTrans,
    ScalarisationTrans, IncreaseRankLoopArraysTrans)
from psyclone.transformations import TransformationError

# USE statements to chase to gather additional symbol information.
NEMO_MODULES_TO_IMPORT = [
    "oce", "par_oce", "par_kind", "dom_oce", "phycst", "ice",
    "obs_fbm", "flo_oce", "sbc_ice", "wet_dry"
]

# Files that PSyclone could process but would reduce the performance.
NOT_PERFORMANT = [
    "bdydta.f90", "bdyvol.f90", "fldread.f90", "icbclv.f90", "icbthm.f90",
    "icbdia.f90", "icbini.f90", "icbstp.f90", "iom.f90", "iom_nf90.f90",
    "obs_grid.f90", "obs_averg_h2d.f90", "obs_profiles_def.f90",
    "obs_types.f90", "obs_read_prof.f90", "obs_write.f90", "tide_mod.f90",
    "zdfosm.f90", "obs_read_surf.f90",
]

# If routine names contain these substrings then we do not profile them
PROFILING_IGNORE = ["flo_dom", "macho", "mpp_", "nemo_gcm", "dyn_ldf"
                    # These are small functions that the addition of profiling
                    # prevents from being in-lined (and then breaks any attempt
                    # to create OpenACC regions with calls to them)
                    "interp1", "interp2", "interp3", "integ_spline", "sbc_dcy",
                    "sum", "sign_", "ddpdd", "solfrac", "psyclone_cmp_int",
                    "psyclone_cmp_char", "psyclone_cmp_logical"]

# Currently fparser has no way of distinguishing array accesses from
# function calls if the symbol is imported from some other module.
# We therefore work-around this by keeping a list of known NEMO functions
# from v4 and v5.
NEMO_FUNCTIONS = [
    # Internal funtions can be obtained with:
    # $ grep -rhi "end function" src/ | awk '{print $3}' | uniq | sort
    'abl_alloc', 'add_xxx', 'Agrif_CFixed', 'agrif_external_switch_index',
    'Agrif_Fixed', 'agrif_oce_alloc', 'Agrif_Root', 'alfa_charn', 'alngam',
    'alpha_sw_sclr', 'alpha_sw_vctr', 'arr_hls', 'arr_lbnd', 'arr_lbnd_2d_dp',
    'arr_lbnd_2d_i', 'arr_lbnd_2d_sp', 'arr_lbnd_3d_dp', 'arr_lbnd_3d_i',
    'arr_lbnd_3d_sp', 'arr_lbnd_4d_dp', 'arr_lbnd_4d_i', 'arr_lbnd_4d_sp',
    'arr_lbnd_5d_dp', 'arr_lbnd_5d_i', 'arr_lbnd_5d_sp', 'atg',
    'bdy_oce_alloc', 'bdy_segs_surf', 'Cd_from_z0', 'CdN10_f_LU12',
    'CdN10_f_LU13', 'cd_n10_ncar', 'cd_neutral_10m', 'CdN_f_LG15',
    'CdN_f_LG15_light', 'CdN_f_LU12_eq36', 'ce_n10_ncar', 'charn_coare3p0',
    'charn_coare3p6', 'charn_coare3p6_wave', 'check_hdom', 'ch_n10_ncar',
    'cp_air', 'cp_air_sclr', 'cp_air_vctr', 'cpl_freq', 'crs_dom_alloc',
    'crs_dom_alloc2', 'dayjul', 'def_newlink', 'delta_skin_layer',
    'depth', 'dep_to_p', 'de_sat_dt_ice_sclr', 'de_sat_dt_ice_vctr',
    'dia_ar5_alloc', 'diadct_alloc', 'dia_hth_alloc', 'dia_ptr_alloc',
    'dia_wri_alloc', 'dom_oce_alloc', 'dom_vvl_alloc', 'dq_sat_dt_ice_sclr',
    'dq_sat_dt_ice_vctr', 'dyn_dmp_alloc', 'dyn_ldf_iso_alloc',
    'dyn_spg_ts_alloc', 'eos_pt_from_ct', 'e_sat_ice_sclr', 'e_sat_ice_vctr',
    'e_sat_sclr', 'e_sat_vctr', 'exa_mpl_alloc', 'f_h_louis_sclr',
    'f_h_louis_vctr', 'find_link', 'fintegral', 'fld_filename',
    'flo_dom_alloc', 'flo_dstnce', 'flo_oce_alloc', 'flo_rst_alloc',
    'flo_wri_alloc', 'f_m_louis_sclr', 'f_m_louis_vctr', 'frac_solar_abs',
    'fspott', 'FUNCTION_GLOBMINMAX', 'FUNCTION_GLOBSUM', 'gamain',
    'gamma_moist', 'gamma_moist_sclr', 'gamma_moist_vctr', 'get_unit',
    'grt_cir_dis', 'grt_cir_dis_saa', 'icb_alloc', 'icb_utl_bilin',
    'icb_utl_bilin_2d_h', 'icb_utl_bilin_3d_h', 'icb_utl_bilin_e',
    'icb_utl_bilin_h', 'icb_utl_bilin_x', 'icb_utl_count', 'icb_utl_heat',
    'icb_utl_mass', 'icb_utl_yearday', 'ice1D_alloc', 'ice_alloc',
    'ice_dia_alloc', 'ice_dyn_rdgrft_alloc', 'ice_perm_eff',
    'ice_thd_pnd_alloc', 'ice_update_alloc', 'ice_var_sshdyn', 'in_hdom',
    'integ_spline', 'interp', 'interp1', 'interp2', 'interp3',
    'iom_axis', 'iom_getszuld', 'iom_nf90_varid', 'iom_sdate', 'iom_use',
    'iom_varid', 'iom_xios_setid', 'iscpl_alloc', 'is_tile', 'kiss',
    'ksec_week', 'lib_mpp_alloc', 'linquad', 'L_vap', 'L_vap_sclr',
    'L_vap_vctr', 'm', 'maxdist', 'mynode', 'nblinks', 'nodal_factort',
    'oce_alloc', 'oce_SWE_alloc', 'One_on_L', 'p2z_exp_alloc',
    'p2z_lim_alloc', 'p2z_prod_alloc', 'p4z_che_alloc', 'p4z_diaz_alloc',
    'p4z_flx_alloc', 'p4z_lim_alloc', 'p4z_meso_alloc', 'p4z_opt_alloc',
    'p4z_prod_alloc', 'p4z_rem_alloc', 'p4z_sed_alloc', 'p4z_sink_alloc',
    'p5z_lim_alloc', 'p5z_meso_alloc', 'p5z_prod_alloc',
    'PHI', 'potemp', 'pres_temp_sclr', 'pres_temp_vctr', 'prt_ctl_sum_2d',
    'prt_ctl_sum_3d', 'prt_ctl_write_sum', 'psi_h', 'psi_h_andreas',
    'psi_h_coare', 'psi_h_ecmwf', 'psi_h_ice', 'psi_h_mfs', 'psi_h_ncar',
    'psi_m', 'psi_m_andreas', 'psi_m_coare', 'psi_m_ecmwf', 'psi_m_ice',
    'psi_m_mfs', 'psi_m_ncar', 'p_to_dep', 'ptr_ci_2d', 'ptr_sj_2d',
    'ptr_sj_3d', 'ptr_sjk', 'q_air_rh', 'qlw_net_sclr', 'qlw_net_vctr',
    'q_sat', 'q_sat_sclr', 'q_sat_vctr', 'qsr_ext_lev', 'rho_air',
    'rho_air_sclr', 'rho_air_vctr', 'Ri_bulk', 'Ri_bulk_sclr', 'Ri_bulk_vctr',
    'rough_leng_m', 'rough_leng_tq', 's', 'sbc_blk_alloc', 'sbc_blk_ice_alloc',
    'sbc_cpl_alloc', 'sbc_dcy', 'sbc_dcy_alloc', 'sbc_ice_alloc',
    'sbc_ice_cice_alloc', 'sbc_oce_alloc', 'sbc_rnf_alloc',
    'sbc_ssr_alloc', 'sed_adv_alloc', 'sed_alloc', 'sed_oce_alloc',
    'sms_c14_alloc', 'sms_pisces_alloc', 'snw_ent', 'solfrac',
    'sto_par_flt_fac', 'sum2d', 'sw_adtg', 'sw_ptmp', 'theta',
    'theta_exner_sclr', 'theta_exner_vctr', 't_imp', 'tra_bbl_alloc',
    'tra_dmp_alloc', 'trc_alloc', 'trc_dmp_alloc', 'trc_dmp_sed_alloc',
    'trc_oce_alloc', 'trc_oce_ext_lev', 'trc_opt_alloc', 'trc_sms_cfc_alloc',
    'trc_sms_my_trc_alloc', 'trc_sub_alloc', 'trd_ken_alloc', 'trd_mxl_alloc',
    'trdmxl_oce_alloc', 'trd_mxl_trc_alloc', 'trd_pen_alloc', 'trd_tra_alloc',
    'trd_trc_oce_alloc', 'trd_vor_alloc', 'twrk_id', 'UN10_from_CD',
    'UN10_from_ustar', 'u_star_andreas', 'virt_temp_sclr', 'virt_temp_vctr',
    'visc_air', 'visc_air_sclr', 'visc_air_vctr', 'w1', 'w2', 'z0_from_Cd',
    'z0tq_LKB', 'zdf_gls_alloc', 'zdf_iwm_alloc', 'zdf_mfc_alloc',
    'zdf_mxl_alloc', 'zdf_oce_alloc', 'zdf_osm_alloc', 'zdf_phy_alloc',
    'zdf_tke_alloc', 'zdf_tmx_alloc', 'itau2date',
    # grep -rh "INTERFACE" src | grep -v "END" | awk '{print $2}' | uniq | sort
    'alpha_sw', 'bulk_formula', 'cp_air', 'debug', 'DECAL_FEEDBACK',
    'DECAL_FEEDBACK_2D', 'depth_to_e3', 'de_sat_dt_ice', 'dia_ar5_hst',
    'dia_ptr_hst', 'div_hor', 'dom_tile_copyin', 'dom_tile_copyout',
    'dq_sat_dt_ice', 'dyn_vor', 'e3_to_depth', 'eos', 'eos_fzp',
    'eos_rab', 'e_sat', 'e_sat_ice', 'f_h_louis', 'f_m_louis',
    'gamma_moist', 'glob_2Dmax', 'glob_2Dmin', 'glob_2Dsum', 'glob_3Dmax',
    'glob_3Dmin', 'glob_3Dsum', 'halo_mng_resize', 'icb_utl_bilin_h',
    'ice_var_itd', 'ice_var_snwblow', 'ice_var_snwfra', 'iom_get',
    'iom_getatt', 'iom_nf90_get', 'iom_put', 'iom_putatt',
    'iom_rstput', 'lbc_lnk', 'lbc_lnk_neicoll', 'lbc_lnk_pt2pt',
    'lbc_nfd', 'lbnd_ij', 'ldf_eiv_trp', 'local_2Dmax', 'local_2Dmin',
    'local_2Dsum', 'local_3Dmax', 'local_3Dmin', 'local_3Dsum',
    'L_vap', 'mpp_max', 'mpp_maxloc', 'mpp_min', 'mpp_minloc',
    'mpp_nfd', 'mpp_sum', 'pres_temp', 'prt_ctl_sum', 'ptr_mpp_sum',
    'ptr_sj', 'ptr_sum', 'qlw_net', 'q_sat', 'rho_air', 'Ri_bulk',
    'SIGN', 'sum3x3', 'theta_exner', 'tra_mle_trp', 'trd_vor_zint',
    'virt_temp', 'visc_air', 'wAimp', 'wzv', 'zdf_osm_iomput',
    'zdf_osm_velocity_rotation',
]

# Currently fparser has no way of distinguishing array accesses from statement
# functions, the following subroutines contains known statement functions
CONTAINS_STMT_FUNCTIONS = ["sbc_dcy"]

# These files change the results from the baseline when psyclone adds
# parallelisation dirctives
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


def enhance_tree_information(schedule):
    ''' Manually fix some PSyIR issues produced by not having enough symbol
    information from external modules. Using RESOLVE_IMPORTS improves the
    situation but it's not complete (not all symbols are imported)
    and it is not transitive (imports that inside import other symbols).

    :param schedule: the PSyIR Schedule to transform.
    :type schedule: :py:class:`psyclone.psyir.nodes.node`

    '''
    are_integers = ('jpi', 'jpim1', 'jpj', 'jpjm1', 'jp_tem', 'jp_sal',
                    'jpkm1', 'jpiglo', 'jpni', 'jpk', 'jpiglo_crs',
                    'jpmxl_atf', 'jpmxl_ldf', 'jpmxl_zdf', 'jpnij',
                    'jpts', 'jpvor_bev', 'nleapy', 'nn_ctls', 'jpmxl_npc',
                    'jpmxl_zdfp', 'npti')

    for reference in schedule.walk(Reference):
        if reference.symbol.name in are_integers:
            # Manually set the datatype of some integer scalars that are
            # important for performance
            _it_should_be(reference.symbol, ScalarType, INTEGER_TYPE)
        elif (
            # If its an ArrayReference ...
            isinstance(reference, ArrayReference) and
            # ... with the following name ...
            (reference.symbol.name in NEMO_FUNCTIONS or
             reference.symbol.name.startswith('local_') or
             reference.symbol.name.startswith('glob_') or
             reference.symbol.name.startswith('SIGN_') or
             reference.symbol.name.startswith('netcdf_') or
             reference.symbol.name.startswith('nf90_')) and
            # ... and the symbol is unresolved
            (reference.symbol.is_import or reference.symbol.is_unresolved)
        ):
            # The parser gets these wrong, they are Calls not ArrayRefs
            if not isinstance(reference.symbol, RoutineSymbol):
                # We need to specialise the generic Symbol to a Routine
                reference.symbol.specialise(RoutineSymbol)
            if not (isinstance(reference.parent, Call) and
                    reference.parent.routine is reference):
                # We also need to replace the Reference node with a Call
                call = Call.create(reference.symbol)
                for child in reference.children[:]:
                    call.addchild(child.detach())
                reference.replace_with(call)


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
    excluding = ["ctl_nam", "ctl_stop", "ctl_warn", "prt_ctl", "eos",
                 "iom_", "hist", "mpi_", "timing_", "oasis_",
                 "fatal_error"  # TODO #2846 - is brought into scope via
                                # multiple wildcard imports
                 ]
    ignore_codeblocks = ["bdy_dyn3d_frs", "bdy_dyn3d_spe", "bdy_dyn3d_zro",
                         "bdy_dyn3d_zgrad"]
    mod_inline_trans = KernelModuleInlineTrans()
    inline_trans = InlineTrans()
    for call in schedule.walk(Call):
        if isinstance(call, IntrinsicCall):
            continue
        rsym = call.routine.symbol
        name = rsym.name.lower()
        if any(name.startswith(excl_name) for excl_name in excluding):
            print(f"Inlining of routine '{name}' is disabled.")
            continue
        if rsym.is_import or rsym.is_unresolved:
            try:
                mod_inline_trans.apply(call)
                print(f"Module-inlined routine '{name}'")
            except TransformationError as err:
                print(f"Module inline of '{name}' failed:\n{err}")
                continue

        # TODO #924 - SKIP ACTUAL INLINING FOR NOW. Currently this causes
        # failures when processing NEMO and this needs further work.
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
        # Make sure all array dimensions are explicit
        for reference in schedule.walk(Reference):
            part_of_the_call = reference.ancestor(Call)
            if part_of_the_call:
                if not part_of_the_call.is_elemental:
                    continue
            if isinstance(reference.symbol, DataSymbol):
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
            if assignment.walk(StructureReference):
                continue  # TODO #2951 Fix issues with structure_refs
            try:
                explicit_loops.apply(assignment)
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
    if schedule.name == "ts_wgt":
        return  # TODO #2937 WaW dependency incorrectly considered private
    # Add the parallel directives in each loop
    for loop in schedule.walk(Loop):
        if loop.ancestor(Directive):
            continue  # Skip if an outer loop is already parallelised

        opts = {"collapse": collapse, "privatise_arrays": privatise_arrays,
                "verbose": True, "nowait": asynchronous_parallelism,
                "enable_reductions": enable_reductions}

        if uniform_intrinsics_only:
            opts["device_string"] = "nvfortran-uniform"

        routine_name = loop.ancestor(Routine).name

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
            if routine_name == "ice_thd_zdf_BL99":
                if isinstance(loop.stop_expr, Reference):
                    if loop.stop_expr.symbol.name == "npti":
                        for variable in ['zdiagbis', 'zindtbis', 'zindterm',
                                         'ztib', 'ztrid', 'ztsb']:
                            st = loop.scope.symbol_table
                            sym = st.lookup(variable, otherwise=None)
                            if sym is not None:
                                loop.explicitly_private_symbols.add(sym)

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
            # associted to the loop in the generated output.
            continue

    # If we are adding asynchronous parallelism then we now try to minimise
    # the number of barriers.
    if asynchronous_parallelism:
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
