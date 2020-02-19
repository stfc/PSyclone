# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2018-2020, Science and Technology Facilities Council.
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
# Author: A. R. Porter, STFC Daresbury Lab

'''Python script intended to be passed to PSyclone's generate()
function via the -s option. Transforms all kernels in the invoke
to have them compiled for an OpenACC accelerator. '''


def trans(psy):
    ''' Take the supplied psy object, apply OpenACC transformations
    to the schedule of the first invoke and return the new psy object '''
    from psyclone.transformations import ACCParallelTrans, \
        ACCEnterDataTrans, ACCLoopTrans, ACCRoutineTrans, \
        KernelGlobalsToArguments
    ptrans = ACCParallelTrans()
    ltrans = ACCLoopTrans()
    dtrans = ACCEnterDataTrans()
    ktrans = ACCRoutineTrans()
    g2localtrans = KernelGlobalsToArguments()

    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    schedule.view()

    # Apply the OpenACC Loop transformation to *every* loop
    # nest in the schedule
    from psyclone.psyir.nodes import Loop
    for child in schedule.children:
        if isinstance(child, Loop):
            newschedule, _ = ltrans.apply(child, {"collapse": 2})
            schedule = newschedule

    # Put all of the loops in a single parallel region
    newschedule, _ = ptrans.apply(schedule.children)

    # Add an enter-data directive
    newschedule, _ = dtrans.apply(schedule)

    # Convert any accesses to global data into kernel arguments and then
    # put an 'acc routine' directive inside each kernel
    for kern in schedule.coded_kernels():
        if kern.name == "kern_use_var_code":
            # TODO #490 and #663. This currently won't work because the
            # KernelGlobalsToArguments transformation works on the PSyIR but
            # the subsequent ACCRoutineTrans works on the fparser2 parse tree.
            g2localtrans.apply(kern)
        _, _ = ktrans.apply(kern)
        # Ideally we would module-inline the kernel here (to save having to
        # rely on the compiler to do it) but this does not currently work
        # for the fparser2 AST (issue #229).
        # _, _ = itrans.apply(kern)

    invoke.schedule = newschedule
    newschedule.view()
    return psy
