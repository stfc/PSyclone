# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2021-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

'''PSyAD, the PSyclone adjoint generation module.'''

from psyclone.psyad.adjoint_visitor import AdjointVisitor
from psyclone.psyad.main import main
from psyclone.psyad.tl2ad import generate_adjoint_str, generate_adjoint, \
    generate_adjoint_test
