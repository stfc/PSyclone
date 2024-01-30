# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2022-2024, Science and Technology Facilities Council
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
# Author R. W. Ford, STFC Daresbury Lab

'''Module for Kernels in the LFRic domain.'''


from psyclone.domain.lfric.kernel.columnwise_operator_arg_metadata import \
    ColumnwiseOperatorArgMetadata
from psyclone.domain.lfric.kernel.common_arg_metadata import CommonArgMetadata
from psyclone.domain.lfric.kernel.common_declaration_metadata import \
    CommonDeclarationMetadata
from psyclone.domain.lfric.kernel.common_meta_arg_metadata import \
    CommonMetaArgMetadata
from psyclone.domain.lfric.kernel.common_metadata import CommonMetadata
from psyclone.domain.lfric.kernel.evaluator_targets_metadata import \
    EvaluatorTargetsMetadata
from psyclone.domain.lfric.kernel.field_arg_metadata import FieldArgMetadata
from psyclone.domain.lfric.kernel.field_vector_arg_metadata import \
    FieldVectorArgMetadata
from psyclone.domain.lfric.kernel.inter_grid_arg_metadata import \
    InterGridArgMetadata
from psyclone.domain.lfric.kernel.inter_grid_vector_arg_metadata import \
    InterGridVectorArgMetadata
from psyclone.domain.lfric.kernel.lfric_kernel_metadata import \
    LFRicKernelMetadata
from psyclone.domain.lfric.kernel.meta_args_metadata import MetaArgsMetadata
from psyclone.domain.lfric.kernel.meta_funcs_arg_metadata import \
    MetaFuncsArgMetadata
from psyclone.domain.lfric.kernel.meta_funcs_metadata import MetaFuncsMetadata
from psyclone.domain.lfric.kernel.meta_mesh_arg_metadata import \
    MetaMeshArgMetadata
from psyclone.domain.lfric.kernel.meta_mesh_metadata import MetaMeshMetadata
from psyclone.domain.lfric.kernel.meta_ref_element_arg_metadata import \
    MetaRefElementArgMetadata
from psyclone.domain.lfric.kernel.meta_ref_element_metadata import \
    MetaRefElementMetadata
from psyclone.domain.lfric.kernel.operates_on_metadata import \
    OperatesOnMetadata
from psyclone.domain.lfric.kernel.operator_arg_metadata import \
    OperatorArgMetadata
from psyclone.domain.lfric.kernel.psyir import LFRicKernelContainer
from psyclone.domain.lfric.kernel.scalar_arg_metadata import ScalarArgMetadata
from psyclone.domain.lfric.kernel.shapes_metadata import ShapesMetadata
