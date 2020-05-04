# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2019-2020, Science and Technology Facilities Council.
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
# Modified by R. W. Ford, STFC Daresbury Lab

'''Transformation module, containing all generic (API independent)
transformations and base classes.
'''

from psyclone.psyir.transformations.extract_trans import ExtractTrans
from psyclone.psyir.transformations.profile_trans import ProfileTrans
from psyclone.psyir.transformations.psy_data_trans import PSyDataTrans
from psyclone.psyir.transformations.region_trans import RegionTrans
from psyclone.psyir.transformations.intrinsics.abs2code_trans import \
    Abs2CodeTrans
from psyclone.psyir.transformations.intrinsics.matmul2code_trans import \
    Matmul2CodeTrans
from psyclone.psyir.transformations.intrinsics.min2code_trans import \
    Min2CodeTrans
from psyclone.psyir.transformations.intrinsics.sign2code_trans import \
    Sign2CodeTrans
from psyclone.psyir.transformations.transformation_error \
    import TransformationError

# The entities in the __all__ list are made available to import directly from
# this package e.g.:
# from psyclone.psyir.transformations import ExtractTrans

__all__ = ['ExtractTrans',
           'ProfileTrans',
           'PSyDataTrans',
           'RegionTrans',
           'Abs2CodeTrans',
           'Matmul2CodeTrans',
           'Min2CodeTrans',
           'Sign2CodeTrans',
           'TransformationError']
