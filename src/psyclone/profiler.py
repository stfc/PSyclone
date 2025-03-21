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
# Author J. Henrichs, Bureau of Meteorology
# Modified by A. R. Porter, R. W. Ford and N. Nobre, STFC Daresbury Lab
# Modified by L. Turner, Met Office
# -----------------------------------------------------------------------------

''' This module provides support for adding profiling to code
    generated by PSyclone. '''

import sys
from psyclone.psyGen import Kern
from psyclone.psyir.nodes import (ACCDirective, Directive, OMPTargetDirective,
                                  Return)
from psyclone.psyir.transformations import ProfileTrans


class Profiler():
    ''' This class wraps all profiling related settings.'''

    # Command line option to use for the various profiling options
    # INVOKES: Automatically add a region for each invoke. i.e. at
    #          the start and end of each PSyclone created/processed
    #          subroutine.
    # ROUTINES: a synonym for INVOKES.
    # KERNELS: Automatically add a profile region around every
    #          kernel call including the loop structure created.
    INVOKES = "invokes"
    ROUTINES = "routines"
    KERNELS = "kernels"
    SUPPORTED_OPTIONS = [INVOKES, ROUTINES, KERNELS]
    _options = []

    # -------------------------------------------------------------------------
    @staticmethod
    def set_options(options, api=""):
        '''Sets the option the user required.

        :param options: options selected by the user, or None to
                        disable all automatic profiling.
        :type options: Optional[list[str]]
        :param str api: the PSyclone API that is in use.

        :raises ValueError: if an invalid option is supplied.

        '''
        # Test that all options are valid
        for index, option in enumerate(options):
            if option not in Profiler.SUPPORTED_OPTIONS:
                # Create a 'nice' representation of the allowed options.
                # [1:-1] cuts out the '[' and ']' that surrounding the
                # string of the list.
                allowed_options = str(Profiler.SUPPORTED_OPTIONS)[1:-1]
                raise ValueError(f"Options for automatic profiling "
                                 f"must be one of {allowed_options} but "
                                 f"found '{option}' at {index}")

        if not api and (Profiler.KERNELS in options or
                        Profiler.INVOKES in options):
            raise ValueError(
                f"The profiling '{Profiler.KERNELS}' and '{Profiler.INVOKES}'"
                f" options are only available when using PSyKAl DSLs.")

        # Store options so they can be queried later
        Profiler._options = options

    # -------------------------------------------------------------------------
    @staticmethod
    def profile_kernels():
        '''Returns true if kernel profiling is enabled.

        :return: whether or not kernels should be profiled.
        :rtype: bool
        '''
        return Profiler.KERNELS in Profiler._options

    # -------------------------------------------------------------------------
    @staticmethod
    def profile_invokes():
        '''
        :return: True if invokes (routines) should be profiled.
        :rtype: bool
        '''
        return (Profiler.INVOKES in Profiler._options or
                Profiler.ROUTINES in Profiler._options)

    # -------------------------------------------------------------------------
    @staticmethod
    def add_profile_nodes(schedule, loop_class):
        '''This function inserts all required Profiling Nodes (for invokes
        and kernels, as specified on the command line) into a schedule. An
        invoke will not be profiled if it contains no statements, if it
        contains more than one Return or if the Return is not the last
        statement.

        :param schedule: The schedule to instrument.
        :type schedule: :py:class:`psyclone.psyGen.InvokeSchedule` or subclass
        :param loop_class: The loop class (e.g. GOLoop, LFRicLoop) to
                            instrument.
        :type loop_class: :py:class:`psyclone.psyir.nodes.Loop` or subclass

        '''
        profile_trans = ProfileTrans()
        if Profiler.profile_kernels():
            # We allow for kernels that have been fused (such that they
            # share a loop) by looking for sets of 'siblings'.
            kernel_lists = schedule.get_sibling_lists(Kern)
            for klist in kernel_lists:
                # We only need to look at the first of any Kernel siblings
                # in order to identify the outermost loop.
                kernel = klist[0]
                # For that kernel, we walk back up to find the outermost loop
                # of the specified class
                target = None
                parent_loop = kernel.ancestor(loop_class)
                while parent_loop:
                    target = parent_loop
                    parent_loop = parent_loop.ancestor(loop_class)
                if not target:
                    # At the minute, every Kernel must have a parent loop (even
                    # a DomainKernel) but that might not always be true in
                    # future.
                    target_nodes = klist
                else:
                    # Have to take care that the target loop does not have
                    # a directive applied to it. We distinguish this case
                    # from that of a directive defining a region by checking
                    # the number of children of the directive.
                    if (isinstance(target.parent.parent, Directive) and
                            len(target.parent.parent.dir_body.children) == 1):
                        # Parent is a Directive that has only the current
                        # loop as a child. Therefore, enclose the Directive
                        # within the profiling region too.
                        target_nodes = [target.parent.parent]
                    else:
                        target_nodes = [target]
                # We only add profiling if we're not within some OpenACC
                # or OpenMP Target region (as otherwise, the PSyData routines
                # being called would have to be compiled for the GPU).
                if not target_nodes[0].ancestor((ACCDirective,
                                                 OMPTargetDirective)):
                    profile_trans.apply(target_nodes)
                # TODO #11 - log that profiling couldn't be added here.

        if Profiler.profile_invokes():
            # We cannot include Return statements within profiling regions
            returns = schedule.walk(Return)
            if returns:
                if len(returns) == 1 and returns[0] is schedule.children[-1]:
                    # There's only one Return and it's the last statement so
                    # simply exclude it from the profiling region.
                    profile_trans.apply(schedule.children[:-1])
                else:
                    # TODO #11 use logging instead.
                    print(f"Not adding profiling to routine '{schedule.name}' "
                          f"because it contains one or more Return "
                          f"statements.", file=sys.stderr)
            else:
                if schedule.children:
                    profile_trans.apply(schedule.children)
                else:
                    # TODO #11 use logging instead.
                    print(f"Not adding profiling to routine '{schedule.name}' "
                          f"because it does not contain any statements.",
                          file=sys.stderr)
