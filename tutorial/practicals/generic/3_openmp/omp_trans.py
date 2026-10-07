# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2020-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

'''A *very* simple transformation script which acts as a starting point for
the tutorial on the introduction of OpenMP with PSyclone. In order to use it
you must first install PSyclone. See README.md in the top-level psyclone
directory.

Once you have PSyclone installed, this script may be used by doing:

 >>> psyclone -s ./omp_trans.py my_file.F90

'''
from psyclone.psyir.nodes import Loop, Routine
from psyclone.transformations import OMPParallelLoopTrans, TransformationError

# Get the transformation we will apply
OMP_TRANS = OMPParallelLoopTrans()

# Specify some loop-type inference rules to make it easier to identify
# loops of interest.
Loop.set_loop_type_inference_rules({"levels": {"variable": "jk"},
                                    "tracers": {"variable": "jt"}})


def trans(psyir):
    ''' Parallelise the provided file by making all loops over vertical (jk)
    levels OpenMP parallel.

    NOTE: this is a deliberately poor implementation. You will improve upon
    it as a part of the tutorial.

    :param psyir: the PSyIR of the provided file.
    :type psyir: :py:class:`psyclone.psyir.nodes.FileContainer`

    '''
    routine = psyir.walk(Routine)[0]
    for child in routine.children:
        if isinstance(child, Loop) and child.loop_type == "levels":
            OMP_TRANS.apply(child)
