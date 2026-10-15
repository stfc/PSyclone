# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2020-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

'''An example PSyclone transformation script to demonstrate
optimisations to the matrix vector kernel to improve its performance
on CPUs.

The matrix vector kernel has been hand optimised for CPUs. This script
will automate these optimisations.

Optimising matvec in PSyclone is work in progress. At the moment the
only automated optimisations possible are the replacement of the
matmul intrinsic with inline code and the kernel constant
transformation (see eg13). The latter transformation does little to
matvec as it stands so the only optimisation included in this script
is the former transformation.

Below is a list of things that will be implemented to improve
performance but are not yet supported as transformations in PSyclone.

1) loop fuse gather and matmul loop
2) move indexing lookup before scatter loop
3) loop fuse scatter loop and matmul loop
4) remove scatter and gather
5) interchange k loop to make it inner
6) re-order data-layout for matrix
7) replicate kernel to support specific function spaces (psy-layer
   optimisation)
8) add kernel constants for nlayers, ndf2, ndf1 (existing transformation)

This script can be applied via the '-s' option when running PSyclone:

$ psyclone -api lfric -s ./matvec_opt.py \
../code/gw_mixed_schur_preconditioner_alg_mod.x90 \
-oalg /dev/null -opsy /dev/null

'''
import sys
from psyclone.domain.common.transformations import KernelModuleInlineTrans
from psyclone.psyir.nodes import FileContainer, IntrinsicCall
from psyclone.psyir.transformations import Matmul2CodeTrans
from psyclone.psyir.backend.fortran import FortranWriter


def trans(psyir: FileContainer):
    '''PSyclone transformation script for the LFRic API to optimise
    the matvec kernel for many-core CPUs. For the moment simply find
    the first matvec kernel in the example, bring it into the same
    module as the PSy-layer, transform the matmul intrinsic to equivalent
    inline code and then print out its PSyIR and output it as Fortran using
    the PSyIR Fortran back-end.

    :param psyir: the PSyIR of the PSy-layer.

    '''
    matmul2code_trans = Matmul2CodeTrans()
    fortran_writer = FortranWriter()
    mod_inline_trans = KernelModuleInlineTrans()

    for kernel in psyir.coded_kernels():
        if kernel.name.lower() == "matrix_vector_code":
            # Module-inline the kernel so that we can transform it.
            mod_inline_trans.apply(kernel)
            kernel_schedules = kernel.get_callees()
            # For simplicity, ASSUME that the kernel is not polymorphic and
            # thus only has one schedule.
            kernel_schedule = kernel_schedules[0]
            # Replace matmul with inline code
            for icall in kernel_schedule.walk(IntrinsicCall):
                if icall.intrinsic is IntrinsicCall.Intrinsic.MATMUL:
                    matmul2code_trans.apply(icall)
            # Future optimisations will go here.
            print(kernel_schedule.view())
            result = fortran_writer(kernel_schedule)
            print(result)
            # Abort after the first matrix vector kernel for the
            # time being.
            print("Aborting to view the modifications to the matrix "
                  "vector kernel")
            sys.exit()
