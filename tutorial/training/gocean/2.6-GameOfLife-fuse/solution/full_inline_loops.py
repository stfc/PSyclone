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

'''Python script intended to be passed to PSyclone's generate()
function via the -s option. It uses a custom version to allow
real inlining of a kernel, and then adds kernel fusion code to
all invokes.
'''

from psyclone.core import AccessType, Signature, VariablesAccessInfo
from psyclone.domain.gocean.transformations import GOceanLoopFuseTrans
from psyclone.psyGen import InvokeSchedule
from psyclone.psyir.nodes import (ArrayReference, Assignment, Call,
                                  Loop, Reference, Routine, Statement)
from psyclone.psyir.symbols import AutomaticInterface, DataSymbol, REAL8_TYPE
from psyclone.psyir.transformations import InlineTrans, TransformationError


class GOceanInlineTrans(InlineTrans):
    '''A simple wrapper around InlineTrans that will ignore an error
    that happens in GOcean, but that can be safely ignored.

    THIS IS A BIG HACK TILL PSYCLONE PROPERLY SUPPORTS INLINING
    OF GOCEAN KERNELS!!!!! USE AT YOUR OWN RISK.
    '''

    def validate(self, node, options=None):
        '''ATM we can't use inlining with GOcean, since we get the error:
            "the type of the actual argument 'neighbours' corresponding to an"
            "array formal argument ('neighbours') is unknown."
        Since in this case we know it is safe to inline, ignore this error.

        :param node: the call to inline
        :type node: :py:class:`psyclone.psyir.nodes.Call`
        :param options: a dictionary with options for transformations.
        :type options: Optional[Dict[str, Any]]

        '''
        try:
            super().validate(node, options)

        except TransformationError as err:
            if ("cannot be inlined because the type of the actual argument"
                    in str(err.value) and "corresponding to an array formal "
                    "argument " in str(err.value) and "is unknown" in
                    str(err.value)):
                return
            raise TransformationError(str(err.value)) from err


potential_symbols = []

def trans_alg(psyir):

    psyir.lower_to_language_level()
    for routine in psyir.walk(Routine):
        print("XX", routine.view())
        symbol_table = routine.symbol_table
        r2d_field = symbol_table.lookup("r2d_field")
        var_info = VariablesAccessInfo(routine)
        for sig in var_info:
            print("XX varinfo", sig, var_info[sig])
            sym = symbol_table.lookup(sig[0])
            if not isinstance(sym.interface, AutomaticInterface):
                # Ignore any non-local variables, we don't know their scope
                continue
            if not sym.datatype == r2d_field:
                continue
            if len(var_info[sig].all_accesses) > 1:
                # More than one access in alg layer, which indicates that this
                # field cannot be replaced with a scalar
                continue
            if len(var_info[sig].all_accesses) == 1:
                # Just one access, check if it's the constructor
                node = var_info[sig].all_accesses[0].node
                statement = node.ancestor(Statement)
                if not (isinstance(statement, Assignment) and
                        statement.lhs.symbol == sym and
                        isinstance(statement.rhs, ArrayReference) and
                        statement.rhs.name == "r2d_field"):
                    continue

            print("potential replacement", sig)
            potential_symbols.append(sig)

def trans(psyir):
    '''
    Take the supplied psyir object, and fuse the first two loops

    :param psyir: the PSyIR of the PSy-layer.
    :type psyir: :py:class:`psyclone.psyir.nodes.FileContainer`

    '''

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

    inline = GOceanInlineTrans()

    # Inline all kernels. We need to replace kernel with actual calls
    # for that to work:
    psyir.lower_to_language_level()

    for call in psyir.walk(Call):
        inline.apply(call)

    var_info = VariablesAccessInfo(schedule)

    for sig in potential_symbols:
        sig_data = Signature(f"{sig}%data")
        accesses = var_info[sig_data]
        if accesses[0].access_type in AccessType.all_write_accesses():
            if not (all(i.node.ancestor(Loop) ==
                        accesses[0].node.ancestor(Loop) for i in accesses)):
                print("IN DIFFERENT LOOPS", sig)
                continue
            symbol_table = schedule.symbol_table
            sym = symbol_table.lookup(sig[0])
            print("REPLACE", sig, sym.datatype, sym.interface,
                  sym.datatype.datatype)
            new_sym = \
                symbol_table.find_or_create(f"{sig[0]}_scalar",
                                            symbol_type=DataSymbol,
                                            interface=AutomaticInterface(),
                                            #datatype = sym.datatype)
                                            datatype = REAL8_TYPE)
            for ref in schedule.walk(Reference):
                ref_sig, _ = ref.get_signature_and_indices()
                if ref_sig == sig_data:
                    #print("REF", ref)
                    new_ref = Reference(new_sym)
                    ref.replace_with(new_ref)
