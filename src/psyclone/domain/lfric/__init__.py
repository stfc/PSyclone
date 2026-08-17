# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2020-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

'''Module for the LFRic domain.
'''

# The order here is not alphabetical, but important because
# there are various dependencies between the modules (e.g.
# KernCallAccArgList imports KernCallArgList, ArgOrdering
# imports LFRicArgDescriptor, ...).
from psyclone.domain.lfric.function_space import FunctionSpace
from psyclone.domain.lfric.lfric_constants import LFRicConstants
from psyclone.domain.lfric.kernel import (
    LFRicArgDescriptor, LFRicKernMetadata)
from psyclone.domain.lfric.lfric_halo_depths import LFRicHaloDepths
from psyclone.domain.lfric.arg_ordering import ArgOrdering
from psyclone.domain.lfric.kern_call_arg_list import KernCallArgList
from psyclone.domain.lfric.kern_call_acc_arg_list import KernCallAccArgList
from psyclone.domain.lfric.kern_call_invoke_arg_list import \
    KernCallInvokeArgList
from psyclone.domain.lfric.kernel_interface import KernelInterface
from psyclone.domain.lfric.lfric_cell_iterators import LFRicCellIterators
from psyclone.domain.lfric.lfric_driver_creator import \
    LFRicDriverCreator
from psyclone.domain.lfric.lfric_types import LFRicTypes
from psyclone.domain.lfric.kern_stub_arg_list import KernStubArgList
from psyclone.domain.lfric.lfric_invoke import LFRicInvoke
from psyclone.domain.lfric.metadata_to_arguments_rules import \
    MetadataToArgumentsRules
from psyclone.domain.lfric.arg_index_to_metadata_index import \
    ArgIndexToMetadataIndex
from psyclone.domain.lfric.lfric_kern import LFRicKern
from psyclone.domain.lfric.lfric_loop import LFRicLoop
from psyclone.domain.lfric.lfric_kern_call_factory import LFRicKernCallFactory
from psyclone.domain.lfric.lfric_collection import LFRicCollection
from psyclone.domain.lfric.lfric_fields import LFRicFields
from psyclone.domain.lfric.lfric_global_reductions import (
    LFRicGlobalMax, LFRicGlobalMin, LFRicGlobalSum)
from psyclone.domain.lfric.lfric_run_time_checks import LFRicRunTimeChecks
from psyclone.domain.lfric.lfric_invokes import LFRicInvokes
from psyclone.domain.lfric.lfric_scalar_args import LFRicScalarArgs
from psyclone.domain.lfric.lfric_scalar_array_args import LFRicScalarArrayArgs
from psyclone.domain.lfric.lfric_loop_bounds import LFRicLoopBounds
from psyclone.domain.lfric.lfric_psy import LFRicPSy
from psyclone.domain.lfric.lfric_invoke_schedule import LFRicInvokeSchedule
from psyclone.domain.lfric.lfric_dofmaps import LFRicDofmaps
from psyclone.domain.lfric.lfric_stencils import LFRicStencils
