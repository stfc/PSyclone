# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017-2026, Science and Technology Facilities Council.
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


'''Provides the LFRic-specific global reduction nodes.'''

from psyclone.domain.common.psylayer import GlobalReduction
from psyclone.errors import GenerationError
from psyclone.lfric import LFRicKernelArgument
from psyclone.psyGen import InvokeSchedule
from psyclone.psyir.nodes import (
    Assignment, Call, Node, Reference, StructureReference)
from psyclone.psyir.symbols import (
    ContainerSymbol, DataSymbol, DataTypeSymbol, ImportInterface,
    UnresolvedType)


class _LFRicGlobalReduction(GlobalReduction):
    '''
    LFRic-specific class representing a GlobalReduction which can be added to
    and manipulated in a schedule. Cannot be directly instantiated - use one
    of the specific sub-classes instead.

    :param scalar: the kernel argument for which to perform a global reduction.
    :param parent: the parent node of this node in the PSyIR.

    :raises GenerationError: if the scalar is not of real or integer
                             intrinsic type.
    '''
    _text_name = "LFRicGlobalReduction"
    # Needs to be set in sub-class.
    _reduction_type = ""

    def __new__(cls, *args, **kwargs):
        # We can't rely on abc.ABC to prevent this class from being
        # instantiated as it doesn't have any abstract methods.
        if cls is _LFRicGlobalReduction:
            raise TypeError(
                f"Only sub-classes of '{cls.__name__}' may be instantiated")
        return GlobalReduction.__new__(cls)

    def __init__(self, scalar: LFRicKernelArgument, parent: Node = None):
        # Initialise the parent class
        super().__init__(scalar, parent=parent)
        # Check scalar intrinsic types that this class supports.
        if scalar.intrinsic_type not in ["real", "integer"]:
            raise GenerationError(
                f"{self._text_name} only supports real or integer scalars, "
                f"but argument '{scalar.name}' in Kernel '{scalar.call.name}' "
                f"has '{scalar.intrinsic_type}' intrinsic type.")

    def lower_to_language_level(self) -> Node:
        ''':returns: this node lowered to language-level PSyIR.

        '''
        # Get the name of the LFRic scalar type and the module from which
        # to import it.
        name = self._scalar.name
        type_name = self._scalar.data_type
        mod_name = self._scalar.module_name

        # Get the symbols from the given names.
        symtab = self.ancestor(InvokeSchedule).symbol_table
        # The Container from which to import the scalar type.
        scal_mod = symtab.find_or_create(mod_name, symbol_type=ContainerSymbol)
        # The scalar type.
        scal_type = symtab.find_or_create(type_name,
                                          symbol_type=DataTypeSymbol,
                                          datatype=UnresolvedType(),
                                          interface=ImportInterface(scal_mod))
        # An instance of scalar type that we will use to get the global
        # min/max/sum.
        red_name = symtab.find_or_create_tag(f"global_{self._reduction_type}",
                                             symbol_type=DataSymbol,
                                             datatype=scal_type)
        tmp_var = symtab.lookup(name)

        # Assign the value of the local scalar to the new scalar_type quantity
        assign1 = Assignment.create(
            lhs=StructureReference.create(red_name, ["value"]),
            rhs=Reference(tmp_var)
        )
        assign1.preceding_comment = f"Perform global {self._reduction_type}"
        self.parent.addchild(assign1, self.position)
        # Use the 'get_xxx' method to compute the global xxx (min/max/sum).
        assign2 = Assignment.create(
            lhs=Reference(tmp_var),
            rhs=Call.create(StructureReference.create(
                red_name, [f"get_{self._reduction_type}"]))
        )
        return self.replace_with(assign2)


class LFRicGlobalSum(_LFRicGlobalReduction):
    '''
    LFRic-specific global sum class which can be added to and
    manipulated in a schedule.

    '''
    _text_name = "LFRicGlobalSum"
    _reduction_type = "sum"


class LFRicGlobalMin(_LFRicGlobalReduction):
    '''
    LFRic-specific global min class which can be added to and
    manipulated in a schedule.

    Represents finding the global minimum value of a scalar.

    '''
    _text_name = "LFRicGlobalMin"
    _reduction_type = "min"


class LFRicGlobalMax(_LFRicGlobalReduction):
    '''
    LFRic-specific global max class which can be added to and
    manipulated in a schedule.

    Represents finding the global maximum value of a scalar.

    '''
    _text_name = "LFRicGlobalMax"
    _reduction_type = "max"


# For AutoAPI documentation generation.
__all__ = ["LFRicGlobalSum",
           "LFRicGlobalMin",
           "LFRicGlobalMax"]
