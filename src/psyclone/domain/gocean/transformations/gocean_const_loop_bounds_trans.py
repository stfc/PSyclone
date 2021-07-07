# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2021, Science and Technology Facilities Council.
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

from fparser.common.readfortran import FortranStringReader
from fparser.two.Fortran2003 import Comment
from psyclone.psyir.transformations import TransformationError
from psyclone.gocean1p0 import GOInvokeSchedule
from psyclone.psyGen import Transformation
from psyclone.psyir.symbols import INTEGER_TYPE, DataSymbol, DataTypeSymbol
from psyclone.psyir.nodes import (CodeBlock, Assignment, Reference,
                                  StructureReference)
from psyclone.configuration import Config


class GOConstLoopBoundsTrans(Transformation):
    ''' Switch on (or off) the use of constant loop bounds within
    a GOInvokeSchedule. In the absence of constant loop bounds, PSyclone will
    generate loops where the bounds are obtained by de-referencing a field
    object, e.g.:

    .. code-block:: fortran

      DO j = my_field%grid%internal%ystart, my_field%grid%internal%ystop

    Some compilers are able to produce more efficient code if they are
    provided with information on the relative trip-counts of the loops
    within an Invoke. With constant loop bounds switched on, PSyclone
    generates code like:

    .. code-block:: fortran

      ny = my_field%grid%subdomain%internal%ystop
      ...
      DO j = 1, ny-1

    In practice, the application of the constant loop bounds looks
    something like, e.g.:

    >>> from psyclone.parse.algorithm import parse
    >>> from psyclone.psyGen import PSyFactory
    >>> import os
    >>> TEST_API = "gocean1.0"
    >>> _, info = parse(os.path.join("tests", "test_files", "gocean1p0",
    >>>                              "single_invoke.f90"),
    >>>                 api=TEST_API)
    >>> psy = PSyFactory(TEST_API).create(info)
    >>> invoke = psy.invokes.get('invoke_0_compute_cu')
    >>> schedule = invoke.schedule
    >>>
    >>> from psyclone.transformations import GOConstLoopBoundsTrans
    >>> clbtrans = GOConstLoopBoundsTrans()
    >>>
    >>> clbtrans.apply(schedule)
    >>> # or, to turn off const. looop bounds:
    >>> # clbtrans.apply(schedule, const_bounds=False)
    >>>
    >>> schedule.view()

    '''

    def __str__(self):
        return "Use constant loop bounds for all loops in a GOInvokeSchedule"

    @property
    def name(self):
        ''' Return the name of the Transformation as a string.'''
        return "GOConstLoopBoundsTrans"

    def validate(self, node, options=None):
        '''Checks if it is valid to apply the GOConstLoopBoundsTrans
        transform.

        :param node: the GOInvokeSchedule to transform.
        :type node: :py:class:`psyclone.gocean1p0.GOInvokeSchedule`
        :param options: a dictionary with options for transformations.
        :type options: dictionary of string:values or None

        :raises TransformationError: if the supplied node is not a \
                                     GOInvokeSchedule.

        '''
        if not isinstance(node, GOInvokeSchedule):
            raise TransformationError("Error in GOConstLoopBoundsTrans: "
                                      "node is not a GOInvokeSchedule")

    def apply(self, node, options=None):
        '''Switches constant loop bounds on or off for all loops in a
        GOInvokeSchedule. Default is 'off'.

        :param node: the GOInvokeSchedule of which all loops will get the
            constant loop bounds switched on or off.
        :type node: :py:class:`psyclone.gocean1p0.GOInvokeSchedule`
        :param options: a dictionary with options for transformations.
        :type options: dictionary of string:values or None

        :param bool options["const_bounds"]: whether the constant loop should\
            be used (True) or not (False). Default is True.

        :returns: 2-tuple of new schedule and memento of transform.
        :rtype: (:py:class:`psyclone.gocean1p0.GOInvokeSchedule`, \
                 :py:class:`psyclone.undoredo.Memento`)

        '''
        self.validate(node, options=options)

        node.const_loop_bounds = True

        i_stop = node.symbol_table.new_symbol(
            node.iloop_stop, symbol_type=DataSymbol,
            datatype=INTEGER_TYPE)
        j_stop = node.symbol_table.new_symbol(
            node.jloop_stop, symbol_type=DataSymbol,
            datatype=INTEGER_TYPE)

        # Look-up the loop bounds using the first field object in the
        # list
        api_config = Config.get().api_conf("gocean1.0")
        arg = node.symbol_table.argument_list[0].name
        xstop = api_config.grid_properties["go_grid_xstop"].fortran \
            .format(arg)
        ystop = api_config.grid_properties["go_grid_ystop"].fortran \
            .format(arg)

        # Add a comment
        block = Comment(FortranStringReader(
            "! Look-up loop bounds\n", ignore_comments=False))
        codeblock = CodeBlock([block], CodeBlock.Structure.STATEMENT)
        node.children.insert(0, codeblock)

        # Get a field argument from the argument list
        for arg in node.symbol_table.argument_list:
            if isinstance(arg.datatype, DataTypeSymbol):
                if arg.datatype.name == "r2d_field":
                    field = arg
                    break
        assign1 = Assignment.create(
                    Reference(i_stop),
                    StructureReference.create(
                        field, xstop.split('%')[1:]))
        assign2 = Assignment.create(
                    Reference(j_stop),
                    StructureReference.create(
                        field, ystop.split('%')[1:]))
        node.children.insert(1, assign1)
        node.children.insert(2, assign2)

        return node, None
