# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2019, Science and Technology Facilities Council
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
# Author: R. W. Ford, STFC Daresbury Lab

'''Module providing a transformation script that converts the supplied
Kernels to CUDA via the SIR intermediate representation and DAWN
backend.

'''
from __future__ import print_function
from psyclone.psyir.backend.sir import SIRPSyIRVisitor
from psyclone.psyir.backend.printer import PrinterPSyIRVisitor

def trans(psy):
    '''Transformation routine for use with PSyclone. Applies the
    PSyIR2SIR transform to the supplied kernels and then calls the
    DAWN backend to generate CUDA code.

    :param psy: the PSy object which this script will transform.
    :type psy: :py:class:`psyclone.psyGen.PSy`
    :returns: the transformed PSy object.
    :rtype: :py:class:`psyclone.psyGen.PSy`

    '''
    sir_psyir_visitor = SIRPSyIRVisitor()
    printer_psyir_visitor = PrinterPSyIRVisitor()
    
    # For each Invoke
    for invoke in psy.invokes.invoke_list:
        sched = invoke.schedule
        kern = sir_psyir_visitor.visit(sched)
        #kern = printer_psyir_visitor.visit(sched)
        print(kern)
    exit(1)
    return psy

# 1) They have a LiteralAccessExpr. We have unary operator and a literal
# This relates to Fortran which does not have a negative constant in an expresssion, it must be a unary operator and a constant but the "-" binds to a level2 expresssion which means that -1.0*a becomes -(1.0*a).
# 2) They provide type of a literal but we do not.
# 3) ?We do not distinguish betwen different types of arrays and they assume they are all field accesses
# 4) ?We call scalars references
# 5) Does SIR support explicit loops?
# 6) PSyIR has general indexing and does not capture stencils.
# 7) ?If statements not supported in SIR.
# 8) is_temporary=True - how to determine this?
# 9) name for stencil and cpp file. How to determine/specify?
