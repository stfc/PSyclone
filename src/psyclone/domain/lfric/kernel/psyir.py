# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2022-2024, Science and Technology Facilities Council
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
# Modified: S. Siso, STFC Daresbury Lab

'''Module containing the LFRic-specific Container class for a Kernel.

'''
from psyclone.psyir.nodes import Container


class LFRicKernelContainer(Container):
    '''An LFRic-specific Container. This specialises the generic Container
    node and adds in any domain-specific information.

    :param str name: the name of the container.
    :param metadata: the metadata object.
    :type metadata: \
        :py:class:`psyclone.domain.lfric.kernel.LFRicKernelMetadata`
    :param kwargs: additional keyword arguments to pass to parent \
        constructor.
    :type kwargs: unwrapped dict

    '''
    def __init__(self, name, metadata, **kwargs):
        super().__init__(name, **kwargs)
        # The metadata object capturing LFRic kernel metadata.
        self._metadata = metadata

    @classmethod
    def create(cls, name, metadata, symbol_table, children):
        '''Create an LFRic Container instance given a name, metadata, a symbol
        table and a list of child nodes. An LFRic-specific kernel is
        created with the metadata describing the kernel interface for
        a single kernel routine within the container.

        :param str name: the name of the Container.
        :param metadata: the metadata object.
        :type metadata: \
            :py:class:`psyclone.domain.lfric.kernel.LFRicKernelMetadata`
        :param symbol_table: the symbol table associated with this \
            Container.
        :type symbol_table: :py:class:`psyclone.psyir.symbols.SymbolTable`
        :param children: a list of PSyIR nodes contained in the \
            Container. These must be Containers or Routines.
        :type children: List[:py:class:`psyclone.psyir.nodes.Container` \
            | :py:class:`psyclone.psyir.nodes.Routine`]

        :returns: an instance of `cls`.
        :rtype: :py:class:`psyclone.psyir.nodes.Container` or subclass
            thereof.

        '''
        return cls(name, metadata, children=children,
                   symbol_table=symbol_table.detach())

    @property
    def metadata(self):
        '''
        :returns the LFRic metadata object.
        :rtype: :py:class:`psyclone.domain.lfric.kernel.psyir.\
            LFRicKernelMetadata`
        '''
        return self._metadata

    def lower_to_language_level(self):
        '''Lower this LFRic-specific container to language level psyir.

        :returns: the lowered version of this node.
        :rtype: :py:class:`psyclone.psyir.node.Node`

        '''
        # Create metadata symbol and add it to the container symbol
        # table.
        data_symbol = self.metadata.lower_to_psyir()
        self.symbol_table.add(data_symbol)

        # Replace this LFRic container with a generic container
        children = self.pop_all_children()
        generic_container = Container.create(
            self.name, self.symbol_table.detach(), children)
        self.replace_with(generic_container)
        return generic_container


__all__ = ["LFRicKernelContainer"]
