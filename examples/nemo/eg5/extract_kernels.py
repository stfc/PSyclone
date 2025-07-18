# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2023-2025 Science and Technology Facilities Council.
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
#
# * Redistributions of source code must retain the above copyright notice, this
#   list of conditions and the following disclaimer.
#
# * Redistributions in binary form must reproduce the above copyright notice,
#   this list of conditions and the following disclaimer in the documentation
#   and/or other materials provided with the distribution.
#
# * Neither the name of the copyright holder nor the names of its
#   contributors may be used to endorse or promote products derived from
#   this software without specific prior written permission.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
# "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
# LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
# FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
# COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
# INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
# BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
# LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
# CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
# LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
# ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
# POSSIBILITY OF SUCH DAMAGE.
# -----------------------------------------------------------------------------
# Author: J. Henrichs, Bureau of Meteorology

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
