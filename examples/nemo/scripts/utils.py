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
    Reference, CodeBlock, Call
from psyclone.psyir.symbols import DataSymbol, INTEGER_TYPE, REAL_TYPE
from psyclone.psyir.transformations import HoistLoopBoundExprTrans, HoistTrans
from psyclone.domain.nemo.transformations import NemoAllArrayRange2LoopTrans
from psyclone.transformations import TransformationError


def enhance_tree_information(schedule):
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

    #if invoke.name == "nonosc":
    #    tmask_symbol = schedule.symbol_table.lookup("tmask")
    #    zbup_symbol = schedule.symbol_table.lookup("zbup")
    #    tmask_symbol.datatype = zbup_symbol.datatype

    for reference in schedule.walk(Reference):
        if reference.symbol.name in ('jpi', 'jpim1', 'jpj', 'jpjm1', 'jp_tem', 'jp_sal', 'jpkm1'):
            if not isinstance(reference.symbol, DataSymbol):
                reference.symbol.specialise(DataSymbol, datatype=INTEGER_TYPE)
        if reference.symbol.name in ('rn_avt_rnf'):
            if not isinstance(reference.symbol, DataSymbol):
                reference.symbol.specialise(DataSymbol, datatype=REAL_TYPE)

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

        if loop.walk(CodeBlock):
            continue  # Skip if loop has a CodeBlock (why not caught by validate?)

        if exclude_calls and loop.walk(Call):
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
