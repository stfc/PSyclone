# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2023-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

'''A transformation script that applies kernel data extraction to a
stand-alone version of one of the tracer-advection routines from the
NEMO ocean model. It was originally extracted by Silvia Mocavero
of CMCC. The code can be found in the `../code` directory.

This script is called for `tra_adv.F90` and applies the generic
:py:class:`psyclone.psyir.transformations.ExtractTrans` to each
invoke, as automatically identified by PSyclone.

    $ psyclone -l output --config ../../../psyclone.cfg -l all \
        -s ./extract_kernels.py -opsy psy.f90 ../code/tra_adv.F90

You can inspect the output file `psy.f90` to see the instrumentation, e.g.:

    CALL extract_psy_data_2 % PreStart("tra_adv", "r2", 1, 2)
    CALL extract_psy_data_2 % PreDeclareVariable("jpk", jpk)
    CALL extract_psy_data_2 % PreDeclareVariable("jk_post", jk)
    CALL extract_psy_data_2 % PreDeclareVariable("rnfmsk_z_post", rnfmsk_z)
    CALL extract_psy_data_2 % PreEndDeclaration
    CALL extract_psy_data_2 % ProvideVariable("jpk", jpk)
    CALL extract_psy_data_2 % PreEnd
    do jk = 1, jpk, 1
      rnfmsk_z(jk) = jk / jpk
    enddo
    CALL extract_psy_data_2 % PostStart
    CALL extract_psy_data_2 % ProvideVariable("jk_post", jk)
    CALL extract_psy_data_2 % ProvideVariable("rnfmsk_z_post", rnfmsk_z)
    CALL extract_psy_data_2 % PostEnd

Note that the Fortran source files provided to PSyclone must have already
been preprocessed (if required).

'''

from psyclone.psyGen import PSy
from psyclone.psyir.nodes import Loop
from psyclone.transformations import TransformationError
from psyclone.psyir.transformations import ExtractTrans


def trans(psyir: PSy):
    '''A PSyclone-script compliant transformation function. Applies
    the kernel extraction to any invoke identified in the PSy layer object.

    :param psyir: the PSyIR of the provided file.
    '''

    extract = ExtractTrans()

    for loop in psyir.walk(Loop):
        # Don't extract the content of an iteration loop:
        if loop.variable.name == "jt":
            continue
        ancestor = loop.ancestor(Loop)
        # Extract any loop that either has no outer loop, or only
        # an iteration loop as outer.
        if ancestor is None or ancestor.variable.name == "jt":
            try:
                # Note that driver creation is not yet supported.
                extract.apply(loop)
            except TransformationError as err:
                # Typically that's caused by a kernel having a CodeBlock
                # inside. In this example there is a write statement
                print(f"Ignoring error '{err.value}'.")
