# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2024-2025, Science and Technology Facilities Council
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

'''A simple generic transformation script to apply omp parallel and omp do.
'''

from psyclone.transformations import OMPLoopTrans, OMPParallelTrans
from psyclone.psyir.nodes import Loop

# Set up some loop_type inference rules in order to reference useful domain
# loop constructs by name
Loop.set_loop_type_inference_rules({
        "lon": {"variable": "TODO #Add the variable name used in inner loops"},
        "lat": {"variable": "TODO #Add the variable name used in outer loops"},
})


def trans(psyir):
    ''' Transform a specific Schedule by making all loops
    over latitudes OpenMP parallel, and adding an omp parallel
    in the calling subroutine.

    :param psyir: the PSyIR of the provided file.
    :type psyir: :py:class:`psyclone.psyir.nodes.FileContainer`

    '''
    omp_parallel = OMPParallelTrans()
    omp_do = OMPLoopTrans()

    # The argument psyir is a FileContainer
    print("Filename is", psyir.name)

    # Apply it to each loop over latitudes containing a kernel
    for loop in psyir.walk(Loop):
        if loop.loop_type == "lat":
            # Apply transformation. Note that you need to specify
            # "--backend disable-validation" on the PSyclone command line,
            # since PSyclone will otherwise prevent you from adding a `omp do`
            # with no surrounding omp parallel.
            omp_do.apply(loop)
        elif loop.loop_type is None and  # Check file name before applying
            # TODO: Add omp parallel in the time stepping loop
