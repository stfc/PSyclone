# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2022-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

'''
LFRic-specific support for PSyAD.
'''

from psyclone.psyad.domain.lfric.lfric_adjoint import generate_lfric_adjoint
from psyclone.psyad.domain.lfric.lfric_adjoint_harness import (
    generate_lfric_adjoint_harness)
