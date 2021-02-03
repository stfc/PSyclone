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

'''This module contains the GOcean-specific OpenCL transformation.
'''

import six
from psyclone.psyGen import Transformation
from psyclone.undoredo import Memento
from psyclone.transformations import TransformationError, KernelTrans
from psyclone.psyGen import args_filter, InvokeSchedule
from psyclone.gocean1p0 import GOInvokeSchedule


class OCLTrans(Transformation):
    '''
    Switches on/off the generation of an OpenCL PSy layer for a given
    InvokeSchedule. Additionally, it will generate OpenCL kernels for
    each of the kernels referenced by the Invoke. For example:

    >>> invoke = ...
    >>> schedule = invoke.schedule
    >>>
    >>> ocl_trans = OCLTrans()
    >>> new_sched, _ = ocl_trans.apply(schedule)

    '''
    @property
    def name(self):
        '''
        :returns: the name of this transformation.
        :rtype: str
        '''
        return "OCLTrans"

    def apply(self, sched, options=None):
        '''
        Apply the OpenCL transformation to the supplied GOInvokeSchedule. This
        causes PSyclone to generate an OpenCL version of the corresponding
        PSy-layer routine. The generated code makes use of the FortCL
        library (https://github.com/stfc/FortCL) in order to manage the
        OpenCL device directly from Fortran.

        :param sched: the InvokeSchedule to transform.
        :type sched: :py:class:`psyclone.psyGen.GOInvokeSchedule`
        :param options: set of option to tune the OpenCL generation.
        :type options: dictionary of string:values or None
        :param bool options["opencl"]: whether or not to enable OpenCL \
                                       generation.

        :returns: 2-tuple of new schedule and memento of transform.
        :rtype: (:py:class:`psyclone.dynamo0p3.DynInvokeSchedule`, \
                 :py:class:`psyclone.undoredo.Memento`)
        '''
        if not options:
            options = {}
        opencl = options.get("opencl", True)

        if opencl:
            self.validate(sched, options)

        # Create a memento of the schedule and the proposed transformation
        keep = Memento(sched, self, [sched, opencl])
        # All we have to do here is set the flag in the Schedule. When this
        # flag is True PSyclone produces OpenCL at code-generation time.
        sched.opencl = opencl

        try:
            # Store the provided OpenCL options in the InvokeSchedule.
            sched.set_opencl_options(options)

        # The raised exceptions are converted to 'TransformationError's.
        except (TypeError, AttributeError) as error:
            six.raise_from(TransformationError(str(error)), error)

        return sched, keep

    def validate(self, sched, options=None):
        '''
        Checks that the supplied InvokeSchedule is valid and that an OpenCL
        version of it can be generated.

        :param sched: the Schedule to check.
        :type sched: :py:class:`psyclone.psyGen.InvokeSchedule`
        :param options: a dictionary with options for transformations.
        :type options: dictionary of string:values or None

        :raises TransformationError: if the InvokeSchedule is not for the \
                                     GOcean1.0 API.
        :raises NotImplementedError: if any of the kernels have arguments \
                                     passed by value.
        '''

        if isinstance(sched, InvokeSchedule):
            if not isinstance(sched, GOInvokeSchedule):
                raise TransformationError(
                    "OpenCL generation is currently only supported for the "
                    "GOcean API but got an InvokeSchedule of type: '{0}'".
                    format(type(sched)))
        else:
            raise TransformationError(
                "Error in OCLTrans: the supplied node must be a (sub-class "
                "of) InvokeSchedule but got {0}".format(type(sched)))

        # Now we need to check the arguments of all the kernels
        args = args_filter(sched.args, arg_types=["scalar"], is_literal=True)
        for arg in args:
            if arg.is_literal:
                raise NotImplementedError(
                    "Cannot generate OpenCL for Invokes that contain "
                    "kernels with arguments passed by value")

        # Check that we can construct the PSyIR and SymbolTable of each of
        # the kernels in this Schedule. Also check that none of them access
        # any form of global data (that is not a routine argument).
        for kern in sched.kernels():
            KernelTrans.validate(kern)
            ksched = kern.get_kernel_schedule()
            global_variables = ksched.symbol_table.global_symbols
            if global_variables:
                raise TransformationError(
                    "The Symbol Table for kernel '{0}' contains the following "
                    "symbols with 'global' scope: {1}. An OpenCL kernel cannot"
                    " call other kernels and all of the data it accesses must "
                    "be passed by argument. Use the KernelGlobalsToArguments "
                    "transformation to convert such symbols to kernel "
                    "arguments first.".
                    format(kern.name, [sym.name for sym in global_variables]))
