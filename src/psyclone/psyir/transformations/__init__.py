# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2019-2024, Science and Technology Facilities Council.
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
# Author J. Henrichs, Bureau of Meteorology
# Modified by: R. W. Ford, STFC Daresbury Lab
#              A. R. Porter, STFC Daresbury Lab
#              S. Siso, STFC Daresbury Lab
#              N. Nobre, STFC Daresbury Lab

'''Transformation module, containing all generic (API independent)
transformations and base classes.
'''

# Order of TransformationError is not alphabetical because otherwise it
# produces an ImportError due to cyclic dependencies
from psyclone.psyir.transformations.transformation_error \
    import TransformationError
from psyclone.psyir.transformations.acc_update_trans import ACCUpdateTrans
from psyclone.psyir.transformations.arrayrange2loop_trans import \
    ArrayRange2LoopTrans
from psyclone.psyir.transformations.chunk_loop_trans import ChunkLoopTrans
from psyclone.psyir.transformations.extract_trans import ExtractTrans
from psyclone.psyir.transformations.fold_conditional_return_expressions_trans \
    import FoldConditionalReturnExpressionsTrans
from psyclone.psyir.transformations.hoist_local_arrays_trans import \
    HoistLocalArraysTrans
from psyclone.psyir.transformations.hoist_loop_bound_expr_trans import \
    HoistLoopBoundExprTrans
from psyclone.psyir.transformations.hoist_trans import HoistTrans
from psyclone.psyir.transformations.inline_trans import InlineTrans
from psyclone.psyir.transformations.intrinsics.abs2code_trans import \
    Abs2CodeTrans
from psyclone.psyir.transformations.intrinsics.dotproduct2code_trans import \
    DotProduct2CodeTrans
from psyclone.psyir.transformations.intrinsics.matmul2code_trans import \
    Matmul2CodeTrans
from psyclone.psyir.transformations.intrinsics.max2code_trans import \
    Max2CodeTrans
from psyclone.psyir.transformations.intrinsics.maxval2loop_trans import \
    Maxval2LoopTrans
from psyclone.psyir.transformations.intrinsics.min2code_trans import \
    Min2CodeTrans
from psyclone.psyir.transformations.intrinsics.minval2loop_trans import \
    Minval2LoopTrans
from psyclone.psyir.transformations.intrinsics.sign2code_trans import \
    Sign2CodeTrans
from psyclone.psyir.transformations.intrinsics.sum2loop_trans import \
    Sum2LoopTrans
from psyclone.psyir.transformations.loop_fuse_trans import LoopFuseTrans
from psyclone.psyir.transformations.loop_swap_trans import LoopSwapTrans
from psyclone.psyir.transformations.loop_tiling_2d_trans \
    import LoopTiling2DTrans
from psyclone.psyir.transformations.loop_trans import LoopTrans
from psyclone.psyir.transformations.nan_test_trans import NanTestTrans
from psyclone.psyir.transformations.omp_loop_trans import OMPLoopTrans
from psyclone.psyir.transformations.omp_target_trans import OMPTargetTrans
from psyclone.psyir.transformations.omp_taskwait_trans import OMPTaskwaitTrans
from psyclone.psyir.transformations.omp_task_trans import OMPTaskTrans
from psyclone.psyir.transformations.parallel_loop_trans import \
    ParallelLoopTrans
from psyclone.psyir.transformations.intrinsics.product2loop_trans import \
    Product2LoopTrans
from psyclone.psyir.transformations.profile_trans import ProfileTrans
from psyclone.psyir.transformations.psy_data_trans import PSyDataTrans
from psyclone.psyir.transformations.read_only_verify_trans \
    import ReadOnlyVerifyTrans
from psyclone.psyir.transformations.region_trans import RegionTrans
from psyclone.psyir.transformations.replace_induction_variables_trans import \
     ReplaceInductionVariablesTrans
from psyclone.psyir.transformations.reference2arrayrange_trans import \
    Reference2ArrayRangeTrans


# For AutoAPI documentation generation
__all__ = ['ACCUpdateTrans',
           'ArrayRange2LoopTrans',
           'ChunkLoopTrans',
           'ExtractTrans',
           'FoldConditionalReturnExpressionsTrans',
           'HoistLocalArraysTrans',
           'HoistLoopBoundExprTrans',
           'HoistTrans',
           'InlineTrans',
           'Abs2CodeTrans',
           'DotProduct2CodeTrans',
           'Matmul2CodeTrans',
           'Max2CodeTrans',
           'Min2CodeTrans',
           'Sign2CodeTrans',
           'Sum2LoopTrans',
           'LoopFuseTrans',
           'LoopSwapTrans',
           'LoopTiling2DTrans',
           'LoopTrans',
           'Maxval2LoopTrans',
           'Minval2LoopTrans',
           'NanTestTrans',
           'OMPLoopTrans',
           'OMPTargetTrans',
           'OMPTaskTrans',
           'OMPTaskwaitTrans',
           'ParallelLoopTrans',
           'Product2LoopTrans',
           'ProfileTrans',
           'PSyDataTrans',
           'ReadOnlyVerifyTrans',
           'Reference2ArrayRangeTrans',
           'RegionTrans',
           'ReplaceInductionVariablesTrans',
           'TransformationError']
