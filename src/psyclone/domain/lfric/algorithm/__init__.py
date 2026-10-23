# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2021-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

'''Module to capture LFRic-specific classes for the Algorithm layer.

'''
from psyclone.domain.lfric.algorithm.lfric_alg import LFRicAlg
from psyclone.domain.lfric.algorithm.psyir import (
    LFRicAlgorithmInvokeCall, LFRicKernelFunctor, LFRicBuiltinFunctor,
    LFRicBuiltinFunctorFactory)
from psyclone.domain.lfric.algorithm.psyir.lfric_kernel_functor import \
    LFRicFunctor


# For AutoAPI documentation generation.
__all__ = [
    'LFRicAlg',
    'LFRicAlgorithmInvokeCall',
    'LFRicBuiltinFunctor',
    'LFRicBuiltinFunctorFactory',
    'LFRicFunctor',
    'LFRicKernelFunctor']
