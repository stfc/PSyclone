# flake8: noqa
# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2024-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

"""
A simple generic transformation script to apply omp parallel and omp do.
"""

from psyclone.transformations import OMPLoopTrans, OMPMasterTrans
from psyclone.psyir.nodes import Call, FileContainer, Loop
from psyclone.psyir.transformations import OMPParallelTrans

# Set up some loop_type inference rules in order to reference useful domain
# loop constructs by name
Loop.set_loop_type_inference_rules({
        "lon": {"variable": "TODO #Add the variable name used in inner loops"},
        "lat": {"variable": "TODO #Add the variable name used in outer loops"},
})


def trans(psyir: FileContainer) -> None:
"""
    Transform a specific Schedule by making all loops
    over latitudes OpenMP parallel, and adding an omp parallel
    in the calling subroutine. Also add an omp master region
    around the output function.

    :param psyir: the PSyIR of the provided file.
    """

    omp_parallel = OMPParallelTrans()
    omp_do = OMPLoopTrans()
    omp_master = OMPMasterTrans()

    # The argument psyir is a FileContainer. Ideally, the build system of
    # the application would call different scripts for the different
    # functions - here we use just one script, and then use the name of
    # the transformed file to trigger different behaviour.
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

    # In the time stepping function, we need to add omp master around
    # calls to output field (otherwise each thread would print the
    # output)
    if psyir.name == "time_step_mod.f90":
        for call in psyir.walk(Call):
            if call.routine.name == "output_field":
                omp_master.apply(call)
