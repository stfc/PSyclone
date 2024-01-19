# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017-2024, Science and Technology Facilities Council.
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
# Modified I. Kavcic, A. Coughtrie and L. Turner, Met Office
# Modified J. Henrichs, Bureau of Meteorology
# Modified A. B. G. Chalk and N. Nobre, STFC Daresbury Lab

''' This module implements a class that creates the necessary framework for an
    LFRic kernel call.

'''

# Imports
from psyclone.domain.lfric import LFRicKern, LFRicLoop


class LFRicKernCallFactory():
    ''' Create the necessary framework for an LFRic kernel call.
    This consists of a Loop over cells containing a call to the
    user-supplied kernel routine.

    '''
    # pylint: disable=too-few-public-methods
    @staticmethod
    def create(call, parent=None):
        '''
        Create the objects needed for a call to the kernel
        described in the call object.

        :param call: information on the kernel call as obtained from the \
                     Algorithm layer.
        :type call: :py:class:`psyclone.parse.algorithm.KernelCall`
        :param parent: the parent of this kernel call in the PSyIR.
        :type parent: :py:class:`psyclone.psyir.nodes.Schedule`

        '''
        if call.ktype.iterates_over == "domain":
            # Kernel operates on whole domain so there is no loop.
            # We still need a loop object though as that is where the logic
            # for handling halo exchanges is currently implemented.
            loop_type = "null"
        else:
            # Loop over cells, indicated by an empty string.
            loop_type = ""
        cloop = LFRicLoop(parent=parent, loop_type=loop_type)

        # The kernel itself
        kern = LFRicKern()
        kern.load(call, cloop.loop_body)

        # Add the kernel as a child of the loop
        cloop.loop_body.addchild(kern)

        # Set-up the loop now we have the kernel object
        cloop.load(kern)
        return cloop


# ---------- Documentation utils -------------------------------------------- #
# The list of module members that we wish AutoAPI to generate
# documentation for (see https://psyclone-ref.readthedocs.io).
__all__ = ['LFRicKernCallFactory']
