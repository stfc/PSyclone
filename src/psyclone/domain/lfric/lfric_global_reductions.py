# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017-2025, Science and Technology Facilities Council.
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
# Authors: R. W. Ford, A. R. Porter, S. Siso and N. Nobre, STFC Daresbury Lab
# Modified by I. Kavcic and L. Turner, Met Office
# Modified by C.M. Maynard, Met Office / University of Reading
# Modified by J. Henrichs, Bureau of Meteorology
# -----------------------------------------------------------------------------

''' This module provides implementations of the various global reduction
    nodes supported in the LFRic DSL. '''

from psyclone.domain.common.psylayer.global_reduction import GlobalReduction
from psyclone.psyGen import InvokeSchedule
from psyclone.psyir.nodes import (Assignment, Call, Node, Reference,
                                  StructureReference)
from psyclone.psyir.symbols import (
    ContainerSymbol, DataSymbol, DataTypeSymbol, ImportInterface,
    REAL_TYPE, UnresolvedType)


class LFRicGlobalReduction(GlobalReduction):
    '''
    LFRic-specific base class for all global-reduction operations.

    '''
    _reduction_name = ""
    _method_name = ""

    def lower_to_language_level(self) -> Node:
        '''
        :returns: this node lowered to language-level PSyIR.

        '''
        # Get the name strings to use
        name = self._operand.name

        symtab = self.ancestor(InvokeSchedule).symbol_table

        # We'll need the LFRic mpi_type.
        mpi_mod = symtab.find_or_create_tag("lfric_mpi_mod",
                                            symbol_type=ContainerSymbol)
        mpi_type = symtab.find_or_create_tag(
            "lfric_mpi_type",
            symbol_type=DataTypeSymbol,
            datatype=UnresolvedType(),
            interface=ImportInterface(mpi_mod))
        mpi_obj = symtab.new_symbol("mpi", symbol_type=DataSymbol,
                                    datatype=mpi_type)
        # Symbol holding the local value.
        loc_min = symtab.lookup(name)

        # Symbol holding the global value.
        result = symtab.new_symbol(f"glob_{name}", symbol_type=DataSymbol,
                                   # TODO - get correct type.
                                   datatype=REAL_TYPE)

        # Obtain a suitable mpi object from one of the field arguments.
        for sym in symtab.datasymbols:
            if (isinstance(sym.datatype, DataTypeSymbol) and
                    sym.datatype.name == "field_type"):
                break
        get_mpi = StructureReference.create(sym, ["get_mpi"])
        self.parent.addchild(Assignment.create(lhs=Reference(mpi_obj),
                                               rhs=Call.create(get_mpi)),
                             index=0)

        # Call the method to compute the global min.
        sref = StructureReference.create(mpi_obj, [self._method_name])
        call = Call.create(sref, [Reference(loc_min), Reference(result)])
        call.preceding_comment = f"Perform global {self._reduction_name}"
        self.parent.addchild(call, self.position)
        assign = Assignment.create(lhs=Reference(loc_min),
                                   rhs=Reference(result))
        return self.replace_with(assign)


class LFRicGlobalMax(LFRicGlobalReduction):
    '''
    Represents the operation to find the global maximum value of a scalar.

    '''
    _reduction_name = "max"
    _method_name = "global_max"
    _text_name = "GlobalMax"


class LFRicGlobalMin(LFRicGlobalReduction):
    '''
    Represents the operation to find the global minimum value of a scalar.

    '''
    _reduction_name = "min"
    _method_name = "global_min"
    _text_name = "GlobalMin"


class LFRicGlobalSum(LFRicGlobalReduction):
    '''
    Represents a global sum in the LFRic DSL.

    '''
    _text_name = "GlobalSum"

    def lower_to_language_level(self) -> Node:
        '''
        :returns: this node lowered to language-level PSyIR.

        '''
        # Get the name strings to use
        name = self._operand.name
        type_name = self._operand.data_type
        mod_name = self._operand.module_name

        # Get the symbols from the given names
        symtab = self.ancestor(InvokeSchedule).symbol_table
        sum_mod = symtab.find_or_create(mod_name, symbol_type=ContainerSymbol)
        sum_type = symtab.find_or_create(type_name,
                                         symbol_type=DataTypeSymbol,
                                         datatype=UnresolvedType(),
                                         interface=ImportInterface(sum_mod))
        sum_name = symtab.find_or_create_tag("global_sum",
                                             symbol_type=DataSymbol,
                                             datatype=sum_type)
        tmp_var = symtab.lookup(name)

        # Create the assignments
        assign1 = Assignment.create(
            lhs=StructureReference.create(sum_name, ["value"]),
            rhs=Reference(tmp_var)
        )
        assign1.preceding_comment = "Perform global sum"
        self.parent.addchild(assign1, self.position)
        assign2 = Assignment.create(
            lhs=Reference(tmp_var),
            rhs=Call.create(StructureReference.create(sum_name, ["get_sum"]))
        )
        return self.replace_with(assign2)


# =============================================================================
# Documentation utils: The list of module members that we wish AutoAPI to
# generate documentation for.
__all__ = [
    'LFRicGlobalReduction',
    'LFRicGlobalMax',
    'LFRicGlobalMin',
    'LFRicGlobalSum'
]
