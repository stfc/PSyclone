# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2020-2025, Science and Technology Facilities Council
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
# Author: R. W. Ford, STFC Daresbury Lab
# Modified: S. Siso, STFC Daresbury Lab

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
        # them perfectly nested. At the moment this transformation
        # does not perform any dependence analysis validation so could
        # move code that should not be moved, see issue
        # #1387. However, it is known that it is safe do apply this
        # transformation to this particular code
        # (tra_adv_compute.F90).
        for loop in subroutine.walk(Loop, stop_type=Loop):  # outermost only
            for child in loop.loop_body[:]:
                if isinstance(child, Assignment):
                    hoist_trans.apply(child)

        kern = fortran_writer(subroutine)
        print(kern)
        kern = sir_writer(subroutine)
        print(kern)
