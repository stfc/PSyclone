# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2022-2026, Science and Technology Facilities Council.
# All rights reserved.
# -----------------------------------------------------------------------------
"""Immutable, PSyIR-based LFRic kernel metadata."""

from psyclone.domain.lfric.kernel.metadata import (
    ColumnwiseOperatorArgMetadata,
    FieldArgMetadata,
    FieldVectorArgMetadata,
    InterGridArgMetadata,
    InterGridVectorArgMetadata,
    KernelProcedure,
    LFRicArgDescriptor,
    LFRicFuncDescriptor,
    LFRicKernMetadata,
    LFRicKernelMetadata,
    LFRicPropertyMetadata,
    MetaFuncsArgMetadata,
    MetaMeshArgMetadata,
    MetaRefElementArgMetadata,
    OperatorArgMetadata,
    ScalarArgMetadata,
    ScalarArrayArgMetadata,
    find_metadata_symbol,
)
from psyclone.domain.lfric.kernel.psyir import LFRicKernelContainer

__all__ = [
    "ColumnwiseOperatorArgMetadata",
    "FieldArgMetadata",
    "FieldVectorArgMetadata",
    "InterGridArgMetadata",
    "InterGridVectorArgMetadata",
    "KernelProcedure",
    "LFRicArgDescriptor",
    "LFRicFuncDescriptor",
    "LFRicKernMetadata",
    "LFRicKernelContainer",
    "LFRicKernelMetadata",
    "LFRicPropertyMetadata",
    "MetaFuncsArgMetadata",
    "MetaMeshArgMetadata",
    "MetaRefElementArgMetadata",
    "OperatorArgMetadata",
    "ScalarArgMetadata",
    "ScalarArrayArgMetadata",
    "find_metadata_symbol",
]
