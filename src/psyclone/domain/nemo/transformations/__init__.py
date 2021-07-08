# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2020-2021, Science and Technology Facilities Council.
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
# Authors: R. W. Ford, STFC Daresbury Lab
#          A. R. Porter, STFC Daresbury Lab

'''Transformations module for NEMO.
'''

from psyclone.domain.nemo.transformations.create_nemo_invoke_schedule_trans \
    import CreateNemoInvokeScheduleTrans
from psyclone.domain.nemo.transformations.create_nemo_kernel_trans \
    import CreateNemoKernelTrans
from psyclone.domain.nemo.transformations.create_nemo_loop_trans \
    import CreateNemoLoopTrans
from psyclone.domain.nemo.transformations.create_nemo_psy_trans \
    import CreateNemoPSyTrans
from psyclone.domain.nemo.transformations.nemo_arrayaccess2loop_trans \
    import NemoArrayAccess2LoopTrans
from psyclone.domain.nemo.transformations.nemo_arrayrange2loop_trans \
    import NemoArrayRange2LoopTrans
from psyclone.domain.nemo.transformations.nemo_allarrayaccess2loop_trans \
    import NemoAllArrayAccess2LoopTrans
from psyclone.domain.nemo.transformations.nemo_allarrayrange2loop_trans \
    import NemoAllArrayRange2LoopTrans
from psyclone.domain.nemo.transformations.nemo_loop_fuse \
    import NemoLoopFuseTrans
from psyclone.domain.nemo.transformations.nemo_outerarrayrange2loop_trans \
    import NemoOuterArrayRange2LoopTrans

# The entities in the __all__ list are made available to import directly from
# this package e.g.:
# from psyclone.domain.nemo.transformations import NemoArrayRange2LoopTrans

__all__ = ['CreateNemoInvokeScheduleTrans',
           'CreateNemoKernelTrans',
           'CreateNemoLoopTrans',
           'CreateNemoPSyTrans',
           'NemoAllArrayRange2LoopTrans',
           'NemoArrayRange2LoopTrans',
           'NemoLoopFuseTrans',
           'NemoOuterArrayRange2LoopTrans',
           'NemoArrayAccess2LoopTrans',
           'NemoAllArrayAccess2LoopTrans']
