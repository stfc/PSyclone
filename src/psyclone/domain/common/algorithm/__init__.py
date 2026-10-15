# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2021-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

'''Module to capture PSyclone-specific PSyIR for the Algorithm
layer.

'''
from psyclone.domain.common.algorithm.psyir import \
    AlgorithmInvokeCall, KernelFunctor

# The entities in the __all__ list are made available to import directly from
# this package e.g.:
# from psyclone.domain.common.algorithm import AlgorithmInvokeCall

__all__ = [
    'AlgorithmInvokeCall',
    'KernelFunctor']
