#!/usr/bin/env python
# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2024-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

''' Process Nemo code with PSyclone but don't do any changes. This file is only
needed to provide a FILES_TO_SKIP list. '''

import os

# A environment variable can inform if this is targeting NEMOv4
NEMOV4 = os.environ.get('NEMOV4', False)

# List of all files that psyclone will skip processing
FILES_TO_SKIP = []
if not NEMOV4:
    # TODO #3112: These produce diverging run.stat results in gcc NEMOv5 BENCH
    FILES_TO_SKIP = [
        "dynhpg.f90",
        "dynspg_ts.f90",
        "sbcssm.f90",
        "tramle.f90",
        "trazdf.f90",
        # These fail with nvfortran
        "icefrm.f90",  # Has unsupported implicit symbol declaration
    ]


def trans(_):
    ''' Don't do any changes. '''
