# flake8: noqa
# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2025-2026, Science and Technology Facilities Council.
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
# Author: J. Henrichs, Bureau of Meteorology

'''Python script intended to be passed to PSyclone via the -s option.
It adds OpenMP offload directives to all kernels.
'''

from psyclone.domain.common.transformations import KernelModuleInlineTrans
from psyclone.gocean1p0 import GOKern
from psyclone.psyir.nodes import Directive, FileContainer, Loop, Routine
from psyclone.psyir.transformations import TransformationError, OMPTargetTrans
from psyclone.transformations import OMPDeclareTargetTrans, OMPLoopTrans

from fuse_loops import trans as fuse_trans


def trans(psyir: FileContainer) -> None:
    '''
    Take the supplied psyir object, apply kernel inlining and loop fusion,
    then add OpenMP offloading directives.

    :param psyir: the PSyIR layer to transform.
    :type psyir: :py:class:`psyclone.psyir.nodes.FileContainer`

    '''

    declare_target = OMPDeclareTargetTrans()

    # Use existing fuse script to fuse all loops
    fuse_trans(psyir)

    # Module inline all kernels (so they can be modified)
    # Then add an OpenMP routine statement to each of them:
    module_inline = KernelModuleInlineTrans()
    for kern in psyir.walk(GOKern):
        module_inline.apply(kern)
        TODO3: Add a the declare target

    loop_offloading = OMPLoopTrans(
    TODO1 Create the teamsdistributeparalleldo transformation
        )
    target_trans = OMPTargetTrans()

    for subroutine in psyir.walk(Routine):
        # TODO2: Either use walk, then you need to check for
        #  outer loops, or you loop over children of the schedule,
        #  but then you need to check that each child is indeed a loop
        for loop in subroutine.walk(Loop):
            TODO2: check loop.loop_type, or if the child
                   is a loop, then apply loop_offloading

            TODO3: Now you also need to add target_trans around the
                   directive. The ``ancestor`` method is the easiest
                   option (look for the first node of type ``Directive``).
                   Using ``parent`` works, but be aware that there is a
                   ``Schedule`` node, so you would need ``parent.parent``
                target_trans.apply()
