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
# Authors R. W. Ford, A. R. Porter and S. Siso, STFC Daresbury Lab
# Modified I. Kavcic, A. Coughtrie, L. Turner and O. Brunt, Met Office
# Modified J. Henrichs, Bureau of Meteorology
# Modified A. B. G. Chalk and N. Nobre, STFC Daresbury Lab
# -----------------------------------------------------------------------------

'''This module contains the LFRicGlobalReduction implementation.'''

from psyclone.configuration import Config
from psyclone.domain.common.psylayer.global_reduction import GlobalReduction
from psyclone.errors import GenerationError, InternalError
from psyclone.psyGen import InvokeSchedule
from psyclone.psyir.nodes import (Assignment, Call, Reference,
                                  StructureReference)
from psyclone.psyir.symbols import (
    ContainerSymbol, DataSymbol, DataTypeSymbol, ImportInterface,
    UnresolvedType)


class LFRicGlobalReduction(GlobalReduction):
    '''
    LFRic specific global-reduction class which can be added to and
    manipulated in a schedule.

    :param operation: the type of global reduction to perform.
    :param operand: the kernel argument for which to perform a global
                    reduction.
    :type operand: :py:class:`psyclone.lfric.LFRicKernelArgument`
    :param parent: the parent node of this node in the PSyIR.
    :type parent: :py:class:`psyclone.psyir.nodes.Node`

    :raises GenerationError: if distributed memory is not enabled.
    :raises InternalError: if the supplied argument is not a scalar.
    :raises GenerationError: if the operand is not of "real" intrinsic type.

    '''
    def __init__(self, operation, operand, parent=None):
        # Check that distributed memory is enabled
        if not Config.get().distributed_memory:
            raise GenerationError(
                "It makes no sense to create an LFRicGlobalReduction object "
                f"when distributed memory is not enabled (dm=False).")
        # Check that the global sum argument is indeed a scalar
        if not operand.is_scalar:
            raise InternalError(
                f"LFRicGlobalReduction.init(): A global reduction argument "
                f"should be a scalar but found argument of type "
                f"'{operand.argument_type}'.")
        # Check scalar intrinsic types that this class supports (only
        # "real" for now)
        if operand.intrinsic_type != "real":
            raise GenerationError(
                f"LFRicGlobalReduction currently only supports real scalars, "
                f"but argument '{operand.name}' in Kernel "
                f"'{operand.call.name}' has '{operand.intrinsic_type}' "
                f"intrinsic type.")
        # Initialise the parent class
        super().__init__(operation, operand, parent=parent)

    def lower_to_language_level(self):
        '''
        :returns: this node lowered to language-level PSyIR.
        :rtype: :py:class:`psyclone.psyir.nodes.Node`
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
