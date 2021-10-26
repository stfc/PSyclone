# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017-2021, Science and Technology Facilities Council.
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
# Authors: R. W. Ford, A. R. Porter and S. Siso, STFC Daresbury Lab

'''A simple test script showing the introduction of OpenMP with PSyclone.
In order to use it you must first install PSyclone like so:

 >>> pip install --user psyclone

(or see the Getting Going section in ../../psyclone.pdf.) Once PSyclone is
installed, this script may be run by doing:

 >>> python runme_openmp.py

This should generate a lot of output, ending with generated
Fortran. In subroutine invoke_0 you will see the code that has
been parallelised:

 >>>    SUBROUTINE invoke_0(cu_fld, p_fld, u_fld, cv_fld, v_fld, z_fld, h_fld)
 >>>      ...
 >>>          !$omp parallel private(i,j)
 >>>   !$omp single
 >>>   !$omp taskloop
 >>>   do j = cu_fld%internal%ystart, cu_fld%internal%ystop, 1
 >>>     do i = cu_fld%internal%xstart, cu_fld%internal%xstop, 1
 >>>       call compute_cu_code(i, j, cu_fld%data, p_fld%data, u_fld%data)
 >>>     enddo
 >>>   enddo
 >>>   !$omp end taskloop
 >>>   !$omp taskloop
 >>>   do j = cv_fld%internal%ystart, cv_fld%internal%ystop, 1
 >>>     do i = cv_fld%internal%xstart, cv_fld%internal%xstop, 1
 >>>       call compute_cv_code(i, j, cv_fld%data, p_fld%data, v_fld%data)
 >>>     enddo
 >>>   enddo
 >>>   !$omp end taskloop
 >>>   !$omp taskloop
 >>>   do j = z_fld%internal%ystart, z_fld%internal%ystop, 1
 >>>     do i = z_fld%internal%xstart, z_fld%internal%xstop, 1
 >>>       call compute_z_code(i, j, z_fld%data, p_fld%data, u_fld%data, v_fld%data, p_fld%grid%dx, p_fld%grid%dy)
 >>>     enddo
 >>>   enddo
 >>>   !$omp end taskloop
 >>>   !$omp taskloop
 >>>   do j = h_fld%internal%ystart, h_fld%internal%ystop, 1
 >>>     do i = h_fld%internal%xstart, h_fld%internal%xstop, 1
 >>>       call compute_h_code(i, j, h_fld%data, p_fld%data, u_fld%data, v_fld%data)
 >>>     enddo
 >>>   enddo
 >>>   !$omp end taskloop
 >>>   !$omp end single
 >>>   !$omp end parallel
 >>>    END SUBROUTINE invoke_0

'''

from __future__ import print_function
from psyclone.parse.algorithm import parse
from psyclone.psyGen import PSyFactory, TransInfo
from psyclone.psyir.backend.fortran import FortranWriter

API = "gocean1.0"
_, INVOKEINFO = parse("shallow_alg.f90", api=API)
PSY = PSyFactory(API, distributed_memory=False).create(INVOKEINFO)
fwriter = FortranWriter()

SCHEDULE = PSY.invokes.get('invoke_0').schedule
SCHEDULE.view()

TRANS_INFO = TransInfo()
FUSE_TRANS = TRANS_INFO.get_trans_name('LoopFuseTrans')
OMP_TRANS = TRANS_INFO.get_trans_name('GOceanOMPLoopTrans')
from psyclone.transformations import OMPParallelTrans, OMPSingleTrans
from psyclone.transformations import OMPTaskloopTrans, OMPTaskwaitTrans
singletrans = OMPSingleTrans()
paralleltrans = OMPParallelTrans()
tasklooptrans = OMPTaskloopTrans(nogroup=False)
taskwaittrans = OMPTaskwaitTrans()


# invoke0
tasklooptrans.apply(SCHEDULE.children[0])
tasklooptrans.apply(SCHEDULE.children[1])
tasklooptrans.apply(SCHEDULE.children[2])
tasklooptrans.apply(SCHEDULE.children[3])

singletrans.apply(SCHEDULE.children)
paralleltrans.apply(SCHEDULE.children)
taskwaittrans.apply(SCHEDULE.children[0])


print(fwriter(PSY.container))
