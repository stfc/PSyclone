# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2019-2021, Science and Technology Facilities Council.
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
# Authors J. Henrichs, Bureau of Meteorology
#         S. Siso, STFC Daresbury Lab

'''This module contains the transformations for GOcean.
'''

from psyclone.domain.gocean.transformations.gocean_extract_trans \
    import GOceanExtractTrans
from psyclone.domain.gocean.transformations.gocean_opencl_trans \
    import GOOpenCLTrans
from psyclone.domain.gocean.transformations. \
    gocean_move_iteration_boundaries_inside_kernel_trans import \
    GOMoveIterationBoundariesInsideKernelTrans
from psyclone.domain.gocean.transformations.gocean_loop_fuse_trans \
    import GOceanLoopFuseTrans
from psyclone.domain.gocean.transformations.gocean_const_loop_bounds_trans \
    import GOConstLoopBoundsTrans

# The entities in the __all__ list are made available to import directly from
# this package e.g.:
# from psyclone.domain.gocean.transformations import GOceanExtractTrans

__all__ = ['GOceanExtractTrans',
           'GOMoveIterationBoundariesInsideKernelTrans',
           'GOceanLoopFuseTrans',
           'GOOpenCLTrans',
           'GOConstLoopBoundsTrans',
           'GOceanLoopFuseTrans']
