# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2022-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

'''Module providing a transformation script that converts the supplied
PSyIR to the Stencil intermediate representation (SIR).

'''
from psyclone.psyir.backend.sir import SIRWriter

from psyclone.psyir.nodes import Assignment, Loop, Routine
from psyclone.psyir.transformations import (
    HoistTrans, AllArrayAccess2LoopTrans, ArrayAssignment2LoopsTrans,
    TransformationError)


def trans(psyir):
    '''Transformation routine for use with PSyclone. Applies the
    ArrayAssignment2LoopsTrans, AllArrayAccess2LoopTrans and
    HoistTrans transformations and then produces the SIR representation
    of the given code.

    :param psyir: the PSyIR of the provided file.
    :type psyir: :py:class:`psyclone.psyir.nodes.FileContainer`

    '''
    array_range_trans = ArrayAssignment2LoopsTrans()
    array_access_trans = AllArrayAccess2LoopTrans()
    hoist_trans = HoistTrans()

    sir_writer = SIRWriter()

    for subroutine in psyir.walk(Routine):
        # Transform any single index accesses in array assignments
        # (e.g. a(1)) into 1-trip loops.
        for assignment in subroutine.walk(Assignment):
            array_access_trans.apply(assignment)

        # Transform any array assignments (Fortran ':' notation) into loops.
        for assignment in subroutine.walk(Assignment):
            try:
                array_range_trans.apply(assignment)
            except TransformationError:
                pass

        # Remove any loop invariant assignments inside k-loops to make
        # them perfectly nested.
        for loop in subroutine.walk(Loop, stop_type=Loop):  # outermost only
            for child in loop.loop_body[:]:
                if isinstance(child, Assignment):
                    hoist_trans.apply(child)

        try:
            kern = sir_writer(subroutine)
            # There is no backend support for writing out SIR.
            print(kern)
        except Exception as e:
            print(f"Failed to transform {subroutine.name}: {e}")
