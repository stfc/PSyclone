#!/usr/bin/env python
# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2022, Science and Technology Facilities Council.
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
# Authors: S. Siso, STFC Daresbury Lab

''' Utilities file to parallelise Nemo code. '''

from psyclone.psyir.nodes import Loop, Assignment, Directive, Container, \
    Reference, CodeBlock, Call, Return, IfBlock, Routine
from psyclone.psyir.symbols import DataSymbol, INTEGER_TYPE, REAL_TYPE, ArrayType
from psyclone.psyir.transformations import HoistLoopBoundExprTrans, \
    HoistTrans, ProfileTrans, HoistLocalArraysTrans, Reference2ArrayRangeTrans
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

def enhance_tree_information(schedule):
    ''' Resolve imports in order to populate relevant datatype on the
    tree symbol tables.

    :param schedule: the PSyIR Schedule to transform.
    '''

    mod_sym_tab = schedule.ancestor(Container).symbol_table

    if "oce" in mod_sym_tab:
        oce_symbol = mod_sym_tab.lookup("oce")
        mod_sym_tab.resolve_imports(container_symbols=[oce_symbol])

    if "par_oce" in mod_sym_tab:
        par_oce_symbol = mod_sym_tab.lookup("par_oce")
        mod_sym_tab.resolve_imports(container_symbols=[par_oce_symbol])

    if "dom_oce" in mod_sym_tab:
        dom_oce_symbol = mod_sym_tab.lookup("dom_oce")
        mod_sym_tab.resolve_imports(container_symbols=[dom_oce_symbol])

    if "phycst" in mod_sym_tab:
        phycst_symbol = mod_sym_tab.lookup("phycst")
        mod_sym_tab.resolve_imports(container_symbols=[phycst_symbol])

    if "ice" in mod_sym_tab:
        ice_symbol = mod_sym_tab.lookup("ice")
        mod_sym_tab.resolve_imports(container_symbols=[ice_symbol])

    # Manually set the datatype of some integer and real variables that are
    # important for performance
    for reference in schedule.walk(Reference):
        if reference.symbol.name in ('jpi', 'jpim1', 'jpj', 'jpjm1', 'jp_tem', 'jp_sal', 'jpkm1'):
            if not isinstance(reference.symbol, DataSymbol):
                reference.symbol.specialise(DataSymbol, datatype=INTEGER_TYPE)
        elif reference.symbol.name in ('rn_avt_rnf', 'rau0'):
            if not isinstance(reference.symbol, DataSymbol):
                reference.symbol.specialise(DataSymbol, datatype=REAL_TYPE)
        elif reference.symbol.name in ('tmask'):
            if not isinstance(reference.symbol, DataSymbol):
                reference.symbol.specialise(DataSymbol, datatype=
                    ArrayType(REAL_TYPE, [ArrayType.Extent.ATTRIBUTE,
                                          ArrayType.Extent.ATTRIBUTE,
                                          ArrayType.Extent.ATTRIBUTE]))

def normalise_loops(
        schedule,
        unroll_array_ranges: bool = True,
        hoist_expressions: bool = True,
        ):
    ''' Normalise all loops in the given schedule so that they are in an
    appropriate form for the Parallelisation transformations to analyse
    them.

    :param schedule: the PSyIR Schedule to transform.
    :param unroll_array_ranges: whether to convert ranges to explicit loops.
    :param hoist_expressions: whether to hoist bounds and loop invariant \
        statements out of the loop nest.
    '''

    # Apply the HoistLocalArraysTrans when possible
    try:
        HoistLocalArraysTrans().apply(schedule)
    except TransformationError as err:
        pass

    # Make sure all array dimensions are explicit
    for reference in schedule.walk(Reference, stop_type=Reference):
        if isinstance(reference.symbol, DataSymbol):
            try:
                Reference2ArrayRangeTrans().apply(reference)
            except TransformationError as err:
                pass
                #new_ref = create_explicit_array(reference)
                #reference.replace_with(new_ref)

    if unroll_array_ranges:
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

    # TODO: In order to perform better on the GPU, nested loops with two
    # sibling inner loops need to be fused or apply loop fission to the
    # top level. This would allow the collapse clause to be applied.


def insert_explicit_loop_parallelism(
        schedule,
        region_directive_trans=None,
        loop_directive_trans=None,
        collapse: bool = True,
        exclude_calls: bool = True
        ):
    ''' For each loop in the schedule that doesn't already have a Directive
    as an ancestor, attempt to insert the given region and loop directives.

    :param region_directive_trans: PSyclone transformation to insert the \
        region directive.
    :param loop_directive_trans: PSyclone transformation to use to insert the \
        loop directive.
    :param collapse: whether to attempt to insert the collapse clause to as \
        many nested loops as possible.
    '''

    # Add the parallel directives in each loop
    for loop in schedule.walk(Loop):
        if loop.ancestor(Directive):
            continue  # Skip if an outer loop is already parallelised

        if exclude_calls and loop.walk((Call, CodeBlock)):
            continue

        try:
            loop_directive_trans.apply(loop)
            # Only add the region directive if the loop was successfully
            # parallelised.
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

                # If it is a dependent (e.g. triangular) loop, it can not be
                # collapsed
                dependent_of_previous_variable = False
                for bound in next_loop.children[0:2]:
                    for ref in bound.walk(Reference):
                        if ref.symbol in previous_variables:
                            dependent_of_previous_variable = True
                if dependent_of_previous_variable:
                    break

                # Check if next_loop is parallelisable?

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
            # It contains OpenACC so we put what we have so far inside a
            # profiling region
            add_profile_region(node_list)
            # A node that is not included in a profiling region marks the
            # end of the current candidate region so reset the list.
            node_list = []
            # Now we go down a level and try again without attempting to put
            # profiling below OpenACC directives or within Assignments
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
