# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2022, Science and Technology Facilities Council.
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
# Author R. W. Ford STFC Daresbury Lab

'''Raise generic PSyIR representing a kernel-layer routine to
PSyclone kernel-layer-specific PSyIR which uses specialised classes.

'''
import re

from psyclone.domain.gocean.kernel import GOceanKernelMetadata, GOceanContainer
from psyclone.psyGen import Transformation
from psyclone.psyir.nodes import Container, ScopingNode, FileContainer
from psyclone.psyir.transformations import TransformationError


def find_symbol(node, name):
    '''Utility method to find the symbol associated with the supplied
    name. The supplied node and all of its siblings are searched and the
    symbol and its scoping node are returned if the symbol is found.

    :param node: the starting node for the search.
    :type: :py:class:`psyclone.psyir.nodes.node`
    :param str name: the name of the symbol being searched.

    :returns: a tuple containing the symbol with the same name as the \
        supplied name and the scoping node for that symbol.
    :rtype: Optional[(:py:class:`psyclone.psyir.symbols.datasymbol, \
        :py:class:`psyclone.psyir.nodes.node`)]

    '''
    symbol = None
    scoping_node = None
    for test_node in node.walk(ScopingNode):
        if name in test_node.symbol_table:
            symbol = test_node.symbol_table.lookup(name)
            scoping_node = test_node
            break
    return (symbol, scoping_node)


class RaisePSyIR2GOceanKernTrans(Transformation):
    '''Raise a generic PSyIR representation of a kernel-layer routine
    to a PSyclone version with specialised domain-specific nodes and
    symbols. This is currently limited to the specialisation of kernel
    metadata.

    >>> from psyclone.domain.gocean.transformations import kern_trans
    >>> from psyclone.psyir.frontend.fortran import FortranReader
    >>> CODE = (
    ...     "MODULE example\n"
    ...     "  TYPE, EXTENDS(kernel_type) :: compute_cu\n"
    ...     "    TYPE(go_arg), DIMENSION(4) :: meta_args = (          &\n"
    ...     "      go_arg(GO_WRITE, GO_CU, GO_POINTWISE),             &\n"
    ...     "      go_arg(GO_READ, GO_CT, GO_STENCIL(000, 011, 000)), &\n"
    ...     "      go_arg(GO_READ, GO_GRID_AREA_T),                   &\n"
    ...     "      go_arg(GO_READ, GO_R_SCALAR, GO_POINTWISE)/)\n"
    ...     "    INTEGER :: ITERATES_OVER = GO_ALL_PTS\n"
    ...     "    INTEGER :: index_offset = GO_OFFSET_SW\n"
    ...     "    CONTAINS\n"
    ...     "      PROCEDURE, NOPASS :: code => compute_cu_code\n"
    ...     "  END TYPE compute_cu\n"
    ...     "contains\n"
    ...     "  subroutine compute_cu_code()\n"
    ...     "  end subroutine\n"
    ...     "end module\n")
    >>> fortran_reader = FortranReader()
    >>> kernel_container = fortran_reader.psyir_from_source(CODE)
    >>> trans = kern_trans("compute_cu")
    >>> trans.apply(kernel_container)

    :param str metadata_name: the name of the symbol containing the \
        required kernel metadata in language-level PSyIR.

    :raises TransformationError: if the supplied metadata_name is \
        invalid.

    '''
    VALID_NAME = re.compile(r'[a-zA-Z_][\w]*')

    def __init__(self, metadata_name):
        super().__init__()

        if not metadata_name or not \
           RaisePSyIR2GOceanKernTrans.VALID_NAME.match(metadata_name):
            raise TransformationError(
                f"Error in {self.name} transformation. The "
                f"RaisePSyIR2GOceanKernTrans transformation requires the "
                f"name of the variable containing the metadata to be set to a "
                f"valid value, but found '{metadata_name}'.")

        # The name of the PSyIR symbol containing the metadata
        self._metadata_name = metadata_name

    def validate(self, node, options=None):
        '''Validate the supplied PSyIR tree.

        :param node: a PSyIR node that is the root of a PSyIR tree.
        :type node: Union[:py:class:`psyclone.psyir.node.Routine`, \
            :py:class:`psyclone.psyir.node.Container`]
        :param options: a dictionary with options for transformations.
        :type options: Optional[Dict[str: str]]

        :raises TransformationError: if the metadata name has not been \
            set or does not exist in the code.
        :raises TransformationError: if the supplied node is not a \
            Routine or a Container.
        :raises TransformationError: if the supplied node argument has \
            a parent.

        '''
        if not isinstance(node, Container):
            raise TransformationError(
                f"Error in {self.name} transformation. The supplied node "
                f"should be a Container but found '{type(node).__name__}'.")

        if node.parent:
            raise TransformationError(
                f"Error in {self.name} transformation. The supplied node "
                f"should be the root of a PSyIR tree but this node has a "
                f"parent ({type(node.parent).__name__}).")

        metadata_symbol, scoping_node = find_symbol(node, self._metadata_name)
        if not metadata_symbol:
            raise TransformationError(
                f"Error in {self.name} transformation. The metadata name "
                f"({self._metadata_name}) provided to the transformation "
                f"does not correspond to a symbol in the supplied PSyIR.")

        # Find the nearest ancestor container including self.  There
        # will always be at least one ancestor container as otherwise
        # an earlier test will fail.
        container = scoping_node.ancestor(Container, include_self=True)
        if isinstance(container, FileContainer):
            raise TransformationError(
                f"Error in {self.name} transformation. The Container in "
                f"which the metadata symbol resides is a FileContainer, "
                f"but should be a generic Container.")

        # Check that the metadata can be generated without any errors.
        _ = GOceanKernelMetadata.create_from_psyir(metadata_symbol)

    def apply(self, node, options=None):
        '''Raise the supplied language-level GOcean kernel PSyIR to
        GOcean-specific kernel PSyIR. Specialises the kernel container
        to a GOcean-specific subclass, populates this subclass with
        the kernel metadata extracted from the metadata symbol as
        specified in metadata_name and removes the symbol from the
        symbol table.

        :param node: a kernel represented in generic PSyIR.
        :type node: Union[:py:class:`psyclone.psyir.node.Routine`, \
            :py:class:`psyclone.psyir.node.Container`]
        :param options: a dictionary with options for transformations.
        :type options: Optional[Dict[str: str]]

        '''
        self.validate(node, options=options)

        # Find the metadata symbol based on the supplied name.
        metadata_symbol, scoping_node = find_symbol(node, self._metadata_name)

        # Find the container in which this metadata resides.
        container = scoping_node.ancestor(Container, include_self=True)

        # Create metadata
        metadata = GOceanKernelMetadata.create_from_psyir(metadata_symbol)

        # Remove metadata symbol.
        # TODO issue #898: support needs to be added for removing a
        # DataSymbol from the symbol table. At the moment we need to
        # use internal methods.
        # pylint: disable=protected-access
        symbol_table = scoping_node.symbol_table
        norm_name = symbol_table._normalize(metadata_symbol.name)
        symbol_table._symbols.pop(norm_name)

        # Replace container
        children = container.pop_all_children()
        gocean_container = GOceanContainer.create(
            container.name, metadata, container.symbol_table.detach(),
            children)
        container.replace_with(gocean_container)


__all__ = ['RaisePSyIR2GOceanKernTrans', 'find_symbol']
