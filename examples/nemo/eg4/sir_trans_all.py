# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2020-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

'''Module providing a transformation script that converts the supplied
PSyIR to the Stencil intermediate representation (SIR) and

1) modifies any PSyIR min, max, abs and sign intrinsics to PSyIR code
beforehand using transformations, as SIR does not support intrinsics.

2) transforms implicit loops to explicit loops as the SIR does not
have the concept of implicit loops.

'''
from psyclone.psyir.backend.sir import SIRWriter
from psyclone.psyir.backend.fortran import FortranWriter
from psyclone.psyir.nodes import IntrinsicCall, Assignment, Loop, Routine
from psyclone.psyir.transformations import (
    Abs2CodeTrans, Sign2CodeTrans, Min2CodeTrans, Max2CodeTrans, HoistTrans,
    AllArrayAccess2LoopTrans, ArrayAssignment2LoopsTrans, TransformationError)


def trans(psyir):
    '''Transformation routine for use with PSyclone. Applies the PSyIR2SIR
    transform to the supplied code after replacing any ABS, SIGN or
    MIN intrinsics with equivalent code. This is done because the SIR
    does not support intrinsics.

    :param psyir: the PSyIR of the provided file.
    :type psyir: :py:class:`psyclone.psyir.nodes.FileContainer`

    '''
    abs_trans = Abs2CodeTrans()
    sign_trans = Sign2CodeTrans()
    min_trans = Min2CodeTrans()
    max_trans = Max2CodeTrans()
    array_range_trans = ArrayAssignment2LoopsTrans()
    array_access_trans = AllArrayAccess2LoopTrans()
    hoist_trans = HoistTrans()

    sir_writer = SIRWriter()
    fortran_writer = FortranWriter()

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

        for icall in subroutine.walk(IntrinsicCall):
            if icall.intrinsic == IntrinsicCall.Intrinsic.ABS:
                # Apply ABS transformation
                abs_trans.apply(icall)
            elif icall.intrinsic == IntrinsicCall.Intrinsic.SIGN:
                # Apply SIGN transformation
                sign_trans.apply(icall)
            elif icall.intrinsic == IntrinsicCall.Intrinsic.MIN:
                # Apply (2-n arg) MIN transformation
                min_trans.apply(icall)
            elif icall.intrinsic in IntrinsicCall.Intrinsic.MAX:
                # Apply (2-n arg) MAX transformation
                max_trans.apply(icall)

        # Remove any loop invariant assignments inside k-loops to make
        # them perfectly nested.
        for loop in subroutine.walk(Loop, stop_type=Loop):  # outermost only
            for child in loop.loop_body[:]:
                if isinstance(child, Assignment):
                    hoist_trans.apply(child)

        kern = fortran_writer(subroutine)
        print(kern)
        try:
            kern = sir_writer(subroutine)
            print(kern)
        except Exception as e:
            print(f"Failed to transform {subroutine.name}: {e}")
