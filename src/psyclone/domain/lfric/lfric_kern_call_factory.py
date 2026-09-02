# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

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
        elif call.ktype.iterates_over == "dof":
            # Loop over dofs within a field.
            loop_type = "dof"
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
# documentation for.
__all__ = ['LFRicKernCallFactory']
