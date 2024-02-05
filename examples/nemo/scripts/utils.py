# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2022-2024, Science and Technology Facilities Council.
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

from psyclone.psyir.nodes import (
    Loop, Assignment, Directive, Container, Reference, CodeBlock, Call,
    Return, IfBlock, Routine, IntrinsicCall)
from psyclone.psyir.symbols import (
    DataSymbol, INTEGER_TYPE, REAL_TYPE, ArrayType, ScalarType,
    RoutineSymbol, ImportInterface)
from psyclone.psyir.transformations import (
    HoistLoopBoundExprTrans, HoistTrans, ProfileTrans, HoistLocalArraysTrans,
    Maxval2LoopTrans, Reference2ArrayRangeTrans)
from psyclone.domain.nemo.transformations import NemoAllArrayRange2LoopTrans
from psyclone.transformations import TransformationError


# If routine names contain these substrings then we do not profile them
PROFILING_IGNORE = ["_init", "_rst", "alloc", "agrif", "flo_dom",
                    "macho", "mpp_", "nemo_gcm",
                    # These are small functions that the addition of profiling
                    # prevents from being in-lined (and then breaks any attempt
                    # to create OpenACC regions with calls to them)
                    "interp1", "interp2", "interp3", "integ_spline", "sbc_dcy",
                    "sum", "sign_", "ddpdd"]

VERBOSE = False


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
    ''' Resolve imports in order to populate relevant datatype on the
    tree symbol tables.

    :param schedule: the PSyIR Schedule to transform.
    :type schedule: :py:class:`psyclone.psyir.nodes.node`
    '''

    mod_sym_tab = schedule.ancestor(Container).symbol_table

    modules_to_import = ("oce", "par_oce", "dom_oce", "phycst", "ice",
                         "obs_fbm", "flo_oce", "sbc_ice", "wet_dry")

    for module_name in modules_to_import:
        if module_name in mod_sym_tab:
            mod_symbol = mod_sym_tab.lookup(module_name)
            mod_sym_tab.resolve_imports(container_symbols=[mod_symbol])

    are_integers = ('jpi', 'jpim1', 'jpj', 'jpjm1', 'jp_tem', 'jp_sal',
                    'jpkm1', 'jpiglo', 'jpni', 'jpk', 'jpiglo_crs',
                    'jpmxl_atf', 'jpmxl_ldf', 'jpmxl_zdf', 'jpnij',
                    'jpts', 'jpvor_bev', 'nleapy', 'nn_ctls', 'jpmxl_npc',
                    'jpmxl_zdfp', 'npti')

    # Manually set the datatype of some integer and real variables that are
    # important for performance
    for reference in schedule.walk(Reference):
        if reference.symbol.name in are_integers:
            _it_should_be(reference.symbol, ScalarType, INTEGER_TYPE)
        elif reference.symbol.name in ('rn_avt_rnf', ):
            _it_should_be(reference.symbol, ScalarType, REAL_TYPE)
        elif isinstance(reference.symbol.interface, ImportInterface) and \
                reference.symbol.interface.container_symbol.name == "phycst":
            # Everything imported from phycst is a REAL
            _it_should_be(reference.symbol, ScalarType, REAL_TYPE)
        elif reference.symbol.name == 'tmask':
            if reference.ancestor(Container).name == "dom_oce":
                continue  # Do not update the original declaration
            _it_should_be(reference.symbol, ArrayType, ArrayType(REAL_TYPE, [
                        ArrayType.Extent.ATTRIBUTE,
                        ArrayType.Extent.ATTRIBUTE,
                        ArrayType.Extent.ATTRIBUTE]))
        elif reference.symbol.name == "sbc_dcy":
            # The parser gets this wrong, it is a Call not an Array access
            if not isinstance(reference.symbol, RoutineSymbol):
                # We haven't already specialised this Symbol.
                reference.symbol.specialise(RoutineSymbol)
            call = Call(reference.symbol)
            for child in reference.children:
                call.addchild(child.detach())
            reference.replace_with(call)


def normalise_loops(
        schedule,
        hoist_local_arrays: bool = True,
        convert_array_notation: bool = True,
        loopify_array_intrinsics: bool = True,
        convert_range_loops: bool = True,
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
    :param bool hoist_expressions: whether to hoist bounds and loop invariant
        statements out of the loop nest.
    '''

    if hoist_local_arrays:
        # Apply the HoistLocalArraysTrans when possible
        try:
            HoistLocalArraysTrans().apply(schedule)
        except TransformationError as _:
            pass

    if convert_array_notation:
        # Make sure all array dimensions are explicit
        for reference in schedule.walk(Reference, stop_type=Reference):
            part_of_the_call = reference.ancestor(Call)
            if part_of_the_call:
                if not part_of_the_call.is_elemental:
                    continue
            if isinstance(reference.symbol, DataSymbol):
                try:
                    Reference2ArrayRangeTrans().apply(reference)
                except TransformationError as _:
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
        explicit_loops = NemoAllArrayRange2LoopTrans()
        for assignment in schedule.walk(Assignment):
            explicit_loops.apply(assignment)

    if hoist_expressions:
        # First hoist all possible expressions
        for loop in schedule.walk(Loop):
            HoistLoopBoundExprTrans().apply(loop)

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


def insert_explicit_loop_parallelism(
        schedule,
        region_directive_trans=None,
        loop_directive_trans=None,
        collapse: bool = True,
        ):
    ''' For each loop in the schedule that doesn't already have a Directive
    as an ancestor, attempt to insert the given region and loop directives.

    :param schedule: the PSyIR Schedule to transform.
    :type schedule: :py:class:`psyclone.psyir.nodes.node`
    :param region_directive_trans: PSyclone transformation to insert the \
        region directive.
    :type region_directive_trans: \
        :py:class:`psyclone.transformation.Transformation`
    :param loop_directive_trans: PSyclone transformation to use to insert the \
        loop directive.
    :type loop_directive_trans: \
        :py:class:`psyclone.transformation.Transformation`
    :param collapse: whether to attempt to insert the collapse clause to as \
        many nested loops as possible.
    :param collapse: whether to insert directive on loops with Calls or \
        CodeBlocks in their loop body.

    '''
    # Add the parallel directives in each loop
    for loop in schedule.walk(Loop):
        if loop.ancestor(Directive):
            continue  # Skip if an outer loop is already parallelised

        opts = {}

        routine_name = loop.ancestor(Routine).invoke.name

        if ('dyn_spg' in routine_name and len(loop.walk(Loop)) > 2):
            print("Loop not parallelised because its in 'dyn_spg' and "
                  "its not the inner loop")
            continue
        # Skip if it is an array operation loop on an ice routine if along the
        # third dim or higher or if the loop nests a loop over ice points
        # (npti) or if the loop and array dims do not match.
        # In addition, they often nest ice linearised loops (npti)
        # which we'd rather parallelise
        if ('ice' in routine_name
            and isinstance(loop.stop_expr, IntrinsicCall)
            and (loop.stop_expr.intrinsic in (IntrinsicCall.Intrinsic.UBOUND,
                                              IntrinsicCall.Intrinsic.SIZE))
            and (len(loop.walk(Loop)) > 2
                 or any(ref.symbol.name in ('npti',)
                        for lp in loop.loop_body.walk(Loop)
                        for ref in lp.stop_expr.walk(Reference))
                 or (str(len(loop.walk(Loop))) !=
                     loop.stop_expr.children[1].value))):
            print("ICE Loop not parallelised for performance reasons")
            continue
        # Skip if looping over ice categories, ice or snow layers
        # as these have only 5, 4, and 1 iterations, respectively
        if (any(ref.symbol.name in ('jpl', 'nlay_i', 'nlay_s')
                for ref in loop.stop_expr.walk(Reference))):
            print("Loop not parallelised because stops at 'jpl', 'nlay_i' "
                  "or 'nlay_s'.")
            continue

        def skip_for_correctness(loop):
            for call in loop.walk(Call):
                if not isinstance(call, IntrinsicCall):
                    print(f"Loop not parallelised because it has a call to "
                          f"{call.routine.name}")
                    return True
                if not call.is_available_on_device():
                    print(f"Loop not parallelised because it has a "
                          f"{call.intrinsic.name} not available on GPUs.")
                    return True
            if loop.walk(CodeBlock):
                print("Loop not parallelised because it has a CodeBlock")
                return True
            return False

        # If we see one such ice linearised loop, we assume
        # calls/codeblocks are not a problem (they are not)
        if not any(ref.symbol.name in ('npti',)
                   for ref in loop.stop_expr.walk(Reference)):
            if skip_for_correctness(loop):
                continue

        # pnd_lev requires manual privatisation of ztmp
        if any(name in routine_name for name in ('tab_', 'pnd_')):
            opts = {"force": True}

        try:
            loop_directive_trans.apply(loop, options=opts)
            # Only add the region directive if the loop was successfully
            # parallelised.
            if region_directive_trans:
                region_directive_trans.apply(loop.parent.parent)
        except TransformationError as err:
            # This loop can not be transformed, proceed to next loop
            print("Loop not parallelised because:", str(err))
            continue

        if collapse:

            # Count the number of perfectly nested loops that can be collapsed
            num_nested_loops = 0
            next_loop = loop
            previous_variables = []
            while isinstance(next_loop, Loop):
                previous_variables.append(next_loop.variable)
                num_nested_loops += 1

                # If it has more than one children, the next loop will not be
                # perfectly nested, so stop searching
                if len(next_loop.loop_body.children) > 1:
                    break

                next_loop = next_loop.loop_body.children[0]
                if not isinstance(next_loop, Loop):
                    break

                # If it is a dependent (e.g. triangular) loop, it can not be
                # collapsed
                dependent_of_previous_variable = False
                for bound in (next_loop.start_expr, next_loop.stop_expr,
                              next_loop.step_expr):
                    for ref in bound.walk(Reference):
                        if ref.symbol in previous_variables:
                            dependent_of_previous_variable = True
                            break
                if dependent_of_previous_variable:
                    break

                # Check that the next loop has no loop-carried dependencies
                if not next_loop.independent_iterations():
                    break

            # Add collapse clause to the parent directive
            if num_nested_loops > 1:
                loop.parent.parent.collapse = num_nested_loops


def add_profiling(children):
    '''
    Walks down the PSyIR and inserts the largest possible profiling regions.
    Code that contains directives is excluded.

    :param children: sibling nodes in the PSyIR to which to attempt to add \
                     profiling regions.
    :type children: list of :py:class:`psyclone.psyir.nodes.Node`

    '''
    if not children:
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
        routine_name = nodes[0].ancestor(Routine).invoke.name.lower()
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
