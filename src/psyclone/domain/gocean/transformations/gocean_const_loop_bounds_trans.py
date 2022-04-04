# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2021-2022, Science and Technology Facilities Council.
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
# Authors R. W. Ford, A. R. Porter and S. Siso, STFC Daresbury Lab
#         J. Henrichs, Bureau of Meteorology
#         I. Kavcic, Met Office

'''This module contains the GOConstLoopBoundsTrans.'''

from psyclone.errors import InternalError
from psyclone.gocean1p0 import GOInvokeSchedule, GOLoop
from psyclone.psyGen import Transformation
from psyclone.psyir.frontend.fortran import FortranReader
from psyclone.psyir.nodes import Assignment, Reference, StructureReference
from psyclone.psyir.symbols import INTEGER_TYPE, DataSymbol, DataTypeSymbol
from psyclone.psyir.transformations import TransformationError
from psyclone.configuration import Config


class GOConstLoopBoundsTrans(Transformation):
    ''' Use of a common constant variable for each loop bound within
    a GOInvokeSchedule. By deafault, PSyclone generates loops where
    the bounds are obtained by de-referencing a field object, e.g.:

    .. code-block:: fortran

      DO j = my_field%grid%internal%ystart, my_field%grid%internal%ystop

    Some compilers are able to produce more efficient code if they are
    provided with information on the relative trip-counts of the loops
    within an Invoke. With constant loop bounds, PSyclone generates code
    like:

    .. code-block:: fortran

      ny = my_field%grid%subdomain%internal%ystop
      ...
      DO j = 1, ny-1

    In practice, the application of the constant loop bounds transformation
    looks something like, e.g.:

    >>> from psyclone.parse.algorithm import parse
    >>> from psyclone.psyGen import PSyFactory
    >>> import os
    >>> TEST_API = "gocean1.0"
    >>> _, info = parse(os.path.join("tests", "test_files", "gocean1p0",
    ...                              "single_invoke.f90"),
    ...                 api=TEST_API)
    >>> psy = PSyFactory(TEST_API).create(info)
    >>> invoke = psy.invokes.get('invoke_0_compute_cu')
    >>> schedule = invoke.schedule
    >>>
    >>> from psyclone.transformations import GOConstLoopBoundsTrans
    >>> clbtrans = GOConstLoopBoundsTrans()
    >>>
    >>> clbtrans.apply(schedule)
    >>> print(schedule.view())

    '''
    def __str__(self):
        return "Use constant loop bounds for all loops in a GOInvokeSchedule"

    @property
    def name(self):
        '''
        :returns: the name of the Transformation as a string.
        :rtype: str

        '''
        return "GOConstLoopBoundsTrans"

    def validate(self, node, options=None):
        '''Checks if it is valid to apply the GOConstLoopBoundsTrans
        transform.

        :param node: the GOInvokeSchedule to transform.
        :type node: :py:class:`psyclone.gocean1p0.GOInvokeSchedule`
        :param options: a dictionary with options for transformations.
        :type options: dict of str:values or None

        :raises TransformationError: if the supplied node is not a \
            GOInvokeSchedule.
        :raises TransformationError: if the supplied schedule has loops with \
            a loop with loop_type different than 'inner' or 'outer'.
        :raises TransformationError: if the supplied schedule has loops with \
            attributes for index_offsets, field_space, iteration_space and \
            loop_type that don't appear in the GOLoop.bounds_lookup table.
        :raises TransformationError: if the supplied schedule doesn't have a \
            field argument.

        '''
        if not isinstance(node, GOInvokeSchedule):
            raise TransformationError(
                "GOConstLoopBoundsTrans can only be applied to "
                "'GOInvokeSchedule' but found '{0}'."
                "".format(type(node).__name__))

        for loop in node.walk(GOLoop):
            if loop.loop_type not in ["inner", "outer"]:
                raise TransformationError(
                    "GOConstLoopBoundsTrans can not transform a loop with "
                    "loop_type '{0}', only 'inner' or 'outer' loop_type values"
                    " are expected.".format(loop.loop_type))

            if loop.index_offset not in loop.bounds_lookup:
                raise TransformationError(
                    "GOConstLoopBoundsTrans can not transform a loop with "
                    "index_offset '{0}' because it is not in the bounds lookup"
                    " table, the available index_offset values are {1}."
                    "".format(loop.index_offset,
                              list(loop.bounds_lookup.keys())))

            table = loop.bounds_lookup[loop.index_offset]
            if loop.field_space not in table:
                raise TransformationError(
                    "GOConstLoopBoundsTrans can not transform a loop with "
                    "field_space '{0}' because it is not in the bounds lookup"
                    " table, the available field_space values are {1}."
                    "".format(loop.field_space, list(table.keys())))

            table = table[loop.field_space]
            if loop.iteration_space not in table:
                raise TransformationError(
                    "GOConstLoopBoundsTrans can not transform a loop with "
                    "iteration_space '{0}' because it is not in the bounds "
                    "lookup table, the available iteration_space values are "
                    "{1}.".format(loop.iteration_space, list(table.keys())))

            table = table[loop.iteration_space]
            if loop.loop_type not in table:
                raise TransformationError(
                    "GOConstLoopBoundsTrans can not transform a loop with "
                    "loop_type '{0}' because it is not in the bounds "
                    "lookup table, the available loop_type values are "
                    "{1}.".format(loop.loop_type, list(table.keys())))

        # Make sure the Invoke has at least one field argument
        for arg in node.symbol_table.argument_list:
            if isinstance(arg.datatype, DataTypeSymbol):
                if arg.datatype.name == "r2d_field":
                    break
        else:
            raise TransformationError(
                "GOConstLoopBoundsTrans can not transform invoke '{0}' because"
                " it does not have any field arguments.".format(node.name))

    def apply(self, node, options=None):
        ''' Modify the GOcean kernel loops in a GOInvokeSchedule to use
        common constant loop bound variables.

        :param node: the GOInvokeSchedule of which all loops will get the \
            constant loop bounds.
        :type node: :py:class:`psyclone.gocean1p0.GOInvokeSchedule`
        :param options: a dictionary with options for transformations.
        :type options: dict of str:values or None

        '''
        self.validate(node, options=options)

        i_stop = node.symbol_table.new_symbol(
            "istop", symbol_type=DataSymbol, datatype=INTEGER_TYPE)
        j_stop = node.symbol_table.new_symbol(
            "jstop", symbol_type=DataSymbol, datatype=INTEGER_TYPE)

        # Get a field argument from the argument list (we checked there
        # is at least one on the validation method)
        for arg in node.symbol_table.argument_list:
            if isinstance(arg.datatype, DataTypeSymbol):
                if arg.datatype.name == "r2d_field":
                    field = arg
                    break

        # Look-up the loop bounds using the first field object in the
        # list
        api_config = Config.get().api_conf("gocean1.0")
        xstop = api_config.grid_properties["go_grid_xstop"].fortran \
            .format(field)
        ystop = api_config.grid_properties["go_grid_ystop"].fortran \
            .format(field)

        # Add the assignments of the bounds to their variables at the
        # beginning of the invoke.
        assign1 = Assignment.create(
                    Reference(i_stop),
                    StructureReference.create(
                            field, xstop.split('%')[1:]))
        assign1.preceding_comment = "Look-up loop bounds"
        assign2 = Assignment.create(
                    Reference(j_stop),
                    StructureReference.create(
                        field, ystop.split('%')[1:]))
        node.children.insert(0, assign1)
        node.children.insert(1, assign2)

        # Fortran reader needed to parse constructed bound expressions
        fortran_reader = FortranReader()

        # Update all GOLoop bounds with the new variables
        for loop in node.walk(GOLoop):

            # Chose the variable depending if it is an inner or outer loop
            if loop.loop_type == "inner":
                stop = i_stop.name
            elif loop.loop_type == "outer":
                stop = j_stop.name
            else:
                raise InternalError(
                    "Found a loop with loop_type '{0}' but the only expected "
                    "values are 'inner' or 'outer'.".format(loop.loop_type))

            # Get the bounds map
            bounds = loop.bounds_lookup[loop.index_offset][loop.field_space][
                loop.iteration_space][loop.loop_type]

            # Set the lower bound
            start_expr = bounds["start"].format(start='2', stop=stop)
            start_expr = "".join(start_expr.split())  # Remove white spaces
            # This common case is a bit of compile-time computation
            # but it helps with fixing all of the test cases.
            if start_expr == "2-1":
                start_expr = "1"
            psyir = fortran_reader.psyir_from_expression(
                    start_expr, node.symbol_table)
            loop.children[0].replace_with(psyir)

            # Set the upper bound
            stop_expr = bounds["stop"].format(start='2', stop=stop)
            stop_expr = "".join(stop_expr.split())  # Remove white spaces
            if stop_expr == "2-1":
                stop_expr = "1"
            psyir = fortran_reader.psyir_from_expression(
                    stop_expr, node.symbol_table)
            loop.children[1].replace_with(psyir)
