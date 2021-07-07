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

from psyclone.psyir.transformations import TransformationError
from psyclone.gocean1p0 import GOInvokeSchedule
from psyclone.psyGen import Transformation


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

        # Check node is a Schedule
        if not isinstance(node, GOInvokeSchedule):
            raise TransformationError("Error in GOConstLoopBoundsTrans: "
                                      "node is not a GOInvokeSchedule")
        if not options:
            options = {}

        node.const_loop_bounds = options.get("const_bounds", True)

        return node, None
