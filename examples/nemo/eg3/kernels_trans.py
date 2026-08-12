# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2018-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

'''A transformation script that seeks to apply OpenACC DATA and KERNELS
directives to NEMO style code.  In order to use it you must first install
PSyclone. See README.md in the top-level psyclone directory.

Once you have psyclone installed, this may be used by doing:

 $ psyclone -s ./kernels_trans.py some_source_file.f90

Note that the Fortran source files provided to PSyclone must
have already been preprocessed (if required).

The transformation script attempts to insert Kernels directives at the
highest possible location(s) in the schedule tree (i.e. to enclose as
much code as possible in each Kernels region). However, due to
limitations in the NVIDIA compiler, we must take care to exclude certain
nodes (such as If blocks) from within Kernel regions. If a proposed
region is found to contain such a node (by the ``valid_kernel``
routine) then the script moves a level down the tree and then repeats
the process of attempting to create the largest possible Kernel
region.

Once the Kernels regions have been created, the script then simply
encloses each of them within an OpenACC Data region (since these have
already been made as large as possible). In reality, the purpose of a
data region is to keep data on the remote GPU device for as long as
possible, ideally between Kernel regions. However, this requires more
sophisticated dependency analysis than is yet implemented in
PSyclone. Issue #309 will tackle this limitation.

'''

from kernel_utils import add_kernels
from psyclone.psyGen import TransInfo
from psyclone.psyir.nodes import Routine, ACCDirective


# Get the PSyclone transformations we will use
ACC_DATA_TRANS = TransInfo().get_trans_name('ACCDataTrans')


def trans(psyir):
    ''' Applies OpenACC 'kernels' and 'data' directives to NEMO code.

    :param psyir: the PSyIR of the provided file.
    :type psyir: :py:class:`psyclone.psyir.nodes.FileContainer`
    '''

    for subroutine in psyir.walk(Routine):
        print(f"Transforming subroutine: {subroutine.name}")

        add_kernels(subroutine.children)

        directives = subroutine.walk(ACCDirective)
        if not directives:
            # We only need a data region if we've added any directives
            continue

        # Since we've already taken care to only include recognised code within
        # 'kernels' directives, we simply put each of those directives inside
        # a data region. In reality we would want to try and make the data
        # regions bigger but this is only an example.
        for directive in directives:
            ACC_DATA_TRANS.apply([directive])
