# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2025, Science and Technology Facilities Council.
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

'''
Python script intended to be passed to PSyclone via the -s option.
It analyses the algorithm layer to identify arrays that are not used
outside of the invoke (i.e. are temporary arrays). It then fuses the
kernel loops, applies full inlining, and then replaces
arrays with scalars if the arrays are not used elsewhere.
'''

from psyclone.core import AccessType, Signature
from psyclone.domain.common.transformations import KernelModuleInlineTrans
from psyclone.domain.gocean.transformations import GOceanLoopFuseTrans
from psyclone.psyGen import InvokeSchedule
from psyclone.psyir.nodes import Call, FileContainer, Loop, Reference, Routine
from psyclone.psyir.symbols import AutomaticInterface, DataSymbol, REAL8_TYPE
from psyclone.psyir.transformations import InlineTrans


# This list is used to store all used r2d_fields that are local to the
# algorithm (this ensures we don't replace arrays that might be used
# elsewhere). The information is collected from the algorithm layer (using
# `trans_alg`), since on the psy layer this information is not available
# anymore (since all fields are just arguments there). These symbols will
# then in the kernel transformation be further analysed to potentially
# replace arrays with scalars.
potential_symbols = []

# While the code here is generic, it will replace the fields `born`
# and `die` with scalars (`born_scalar` and `die_scalar`)


def trans_alg(psyir: FileContainer) -> None:
    '''This function is called on the algorithm layer. It detects all local
    fields (i.e. not coming from an import or as a argument), so when
    transforming the PSy layer, we will try to replace them with scalars

    :param psyir: the PSyIR of the Algorithm-layer.
    '''

    # Replace arrays with scalars if possible - after inlining
    # there is no need to store temporary values back to memory.
    psyir.lower_to_language_level()
    for routine in psyir.walk(Routine):
        symbol_table = routine.symbol_table
        r2d_field = symbol_table.lookup("r2d_field")
        var_info = routine.reference_accesses()
        for sig in var_info:
            sym = symbol_table.lookup(sig[0])
            if not isinstance(sym.interface, AutomaticInterface):
                # Ignore any non-local variables, we don't know their scope
                continue

            if not sym.datatype == r2d_field:
                # Only replace r2d fields
                continue
            print("potential replacement", sig)
            potential_symbols.append(sig)


def trans(psyir: FileContainer) -> None:
    '''
    Fuse the kernel loops, full inline the kernels, and then
    replace any temporary array (based on analysing the algorithm
    layer) with a scalar.

    '''
    # pylint: disable=too-many-locals
    # We know that there is only one schedule
    schedule = psyir.walk(InvokeSchedule)[0]

    fuse = GOceanLoopFuseTrans()
    # do j do i count
    # do j do i born
    # do j do i die
    # do j do i combine

    start = 1
    # First merge the first two j loops
    fuse.apply(schedule[start], schedule[start+1])
    # do j do i count
    #      do i born
    # do j do i die
    # do j do i combine

    # Then merge the (previous third, now second) loop to the
    # fused loop
    fuse.apply(schedule[start], schedule[start+1])
    # do j do i count
    #      do i born
    #      do i die
    # do j do i combine

    # You cannot fuse the two remaining outer loops!

    # Fuse the three inner loops: first the first two
    fuse.apply(schedule[start].loop_body[0], schedule[start].loop_body[1])
    # do j do i count born
    #      do i die
    # do j do i combine

    # Then merge in the previous third, now second) loop
    fuse.apply(schedule[start].loop_body[0], schedule[start].loop_body[1])
    # do j do i count born die
    # do j do i combine

    kmit = KernelModuleInlineTrans()
    module_inline = InlineTrans()

    # Inline all kernels. We need to lower to language level before
    # we can inline. Also, any kernel must first be module inlined.
    psyir.lower_to_language_level()
    for call in psyir.walk(Call):
        kmit.apply(call)
        module_inline.apply(call)

    # Now try to replace fields (that are locally used in the algorithm
    # layer) with scalars
    var_info = schedule.reference_accesses()

    for sig in potential_symbols:
        # On the lowered level, the symbols have now "%data" attached, so
        # update the signature used here:
        sig_data = Signature(f"{sig}%data")
        accesses = var_info[sig_data]
        if accesses[0].access_type in AccessType.all_write_accesses():
            if not (all(i.node.ancestor(Loop) ==
                        accesses[0].node.ancestor(Loop) for i in accesses)):
                print("IN DIFFERENT LOOPS", sig)
                continue
            symbol_table = schedule.symbol_table
            sym = symbol_table.lookup(sig[0])
            # Now replace all instances of the array with a scalar:
            print("REPLACE", sig, sym.datatype, sym.interface,
                  sym.datatype.datatype)
            new_sym = \
                symbol_table.find_or_create(f"{sig[0]}_scalar",
                                            symbol_type=DataSymbol,
                                            interface=AutomaticInterface(),
                                            datatype=REAL8_TYPE)
            for ref in schedule.walk(Reference):
                ref_sig, _ = ref.get_signature_and_indices()
                if ref_sig == sig_data:
                    new_ref = Reference(new_sym)
                    ref.replace_with(new_ref)
