# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2021-2022, Science and Technology Facilities Council.
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
# Authors: R. W. Ford and A. R. Porter, STFC Daresbury Lab.

'''This module contains LFRic Algorithm-layer-specific PSyIR classes.

'''
from psyclone.domain.common.algorithm import (AlgorithmInvokeCall,
                                              KernelFunctor)
from psyclone.domain.lfric.lfric_builtins import BUILTIN_MAP_CAPITALISED


class LFRicAlgorithmInvokeCall(AlgorithmInvokeCall):
    '''An invoke call from the LFRic Algorithm layer.'''

    _children_valid_format = "[LFRicKernelFunctor|LFRicBuiltinFunctor]*"
    _text_name = "LFRicAlgorithmInvokeCall"

    @staticmethod
    def _validate_child(position, child):
        '''
        :param int position: the position to be validated.
        :param child: a child to be validated.
        :type child: :py:class:`psyclone.psyir.nodes.Node`

        :returns: whether the given child and position are valid for this node.
        :rtype: bool

        '''
        return isinstance(child, (LFRicKernelFunctor, LFRicBuiltinFunctor))

    @staticmethod
    def _def_container_root_name(node):
        '''
        :returns: the root name to use for the container.
        :rtype: str
        '''
        return f"{node.name}_psy"


class LFRicBuiltinFunctor(KernelFunctor):
    '''Object containing an LFRic builtin call, a description of its
    required interface and the arguments to be passed to it.

    '''
    _text_name = "LFRicBuiltinFunctor"


class LFRicKernelFunctor(KernelFunctor):
    '''Object containing an LFRic kernel call, a description of its
    required interface and the arguments to be passed to it.

    '''
    _text_name = "LFRicKernelFunctor"


# Generate classes representing LFRic BuiltIn Functors.

#: Dictionary of BuiltIn Functors, indexed by lower-case name.
BUILTIN_FUNCTOR_MAP = {}

for name in BUILTIN_MAP_CAPITALISED:
    code = (
        f"class LFRic_{name}_Functor(LFRicBuiltinFunctor):\n"
        f"\n"
        f"    @classmethod\n"
        f"    def create(cls, table, arguments):\n"
        f"        from psyclone.psyir.symbols import DataTypeSymbol, "
        f"StructureType, Symbol\n"
        # We can't use find_or_create() here as that raises an Exception if
        # the symbol that is found is not of the correct type.
        f"        try:\n"
        f"            sym = table.lookup('{name}')\n"
        f"            if type(sym) is Symbol:\n"
        f"                sym.specialise(DataTypeSymbol)\n"
        f"                sym.datatype = StructureType()\n"
        f"        except KeyError:\n"
        f"            sym = table.new_symbol('{name}',\n"
        f"                                   symbol_type=DataTypeSymbol,\n"
        f"                                   datatype=StructureType())\n"
        f"\n"
        f"        return super().create(sym, arguments)\n"
        f"\n"
        f"    def lower_to_language_level(self):\n"
        f"        ''' Remove the symbol representing this BuiltIn as it only\n"
        f"        exists in the DSL. '''\n"
        f"        table = self.scope.symbol_table\n"
        f"        try:\n"
        f"            sym = table.lookup('{name}')\n"
        f"            table.remove(sym)\n"
        f"        except KeyError:\n"
        f"            pass\n"
        f"\n"
        f"BUILTIN_FUNCTOR_MAP['{name.lower()}'] = LFRic_{name}_Functor\n")
    exec(code)


# For AutoAPI documentation generation.
__all__ = ['LFRicAlgorithmInvokeCall', 'LFRicBuiltinFunctor',
           'LFRicKernelFunctor', 'BUILTIN_FUNCTOR_MAP']
