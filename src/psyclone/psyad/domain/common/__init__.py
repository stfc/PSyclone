# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2022-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

'''
Functionality common to all API support in PSyAD.
'''

from psyclone.psyad.domain.common.adjoint_utils import (
    find_container, create_adjoint_name, create_real_comparison,
    common_real_comparison)
