# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2022-2024, Science and Technology Facilities Council.
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
# Authors: R. W. Ford and A. R. Porter, STFC Daresbury Lab

'''Raise generic PSyIR representing a kernel-layer routine to
LFRic kernel-layer-specific PSyIR which uses specialised classes.

'''
from psyclone.domain.lfric.kernel.lfric_kernel_metadata import \
    LFRicKernelMetadata
from psyclone.domain.lfric.kernel.psyir import LFRicKernelContainer
from psyclone.psyGen import Transformation
from psyclone.psyir.frontend.fortran import FortranReader
from psyclone.psyir.nodes import Container, ScopingNode, FileContainer
from psyclone.psyir.transformations import TransformationError


# TODO issue #1877. Find an appropriate place for the find_symbol()
# method as it can be useful beyond this particular
# transformation. Although the generic one would probably not return
# the scoping node as well.
def find_symbol(node, name):
    '''Utility method to find the symbol associated with the supplied
    name. The supplied node and all of its descendants are searched
    and the symbol and its scoping node are returned if the symbol is
    found. If not found, None is returned for the symbol and scoping
    node.

    :param node: the starting node for the search.
    :type: :py:class:`psyclone.psyir.nodes.node`
    :param str name: the name of the symbol being searched.

    :returns: a tuple containing the symbol with the same name as the \
        supplied name and the scoping node for that symbol.
    :rtype: Tuple[Optional[:py:class:`psyclone.psyir.symbols.datasymbol], \
        Optional[:py:class:`psyclone.psyir.nodes.node`]]

    '''
    for test_node in node.walk(ScopingNode):
        if name in test_node.symbol_table:
            symbol = test_node.symbol_table.lookup(name)
            scoping_node = test_node
            return (symbol, scoping_node)
    return (None, None)


class RaisePSyIR2LFRicKernTrans(Transformation):
    '''Raise a generic PSyIR representation of a kernel-layer routine and
    metadata to an LFRic version with specialised domain-specific
    nodes and symbols. This is currently limited to the specialisation
    of kernel metadata.

    >>> from psyclone.domain.lfric.transformations import \
            RaisePSyIR2LFRicKernTrans
    >>> from psyclone.psyir.frontend.fortran import FortranReader
    >>> CODE = ("""
    ... MODULE example
    ... TYPE, EXTENDS(kernel_type) :: compute_cu
    ...   TYPE(arg_type), DIMENSION(4) :: meta_args = (/     &
    ...     arg_type(GH_FIELD, GH_REAL, GH_INC, W1),         &
    ...     arg_type(GH_FIELD, GH_REAL, GH_READ, W3),        &
    ...     arg_type(GH_FIELD, GH_REAL, GH_READ, W3),        &
    ...     arg_type(GH_FIELD, GH_REAL, GH_READ, W3)/)
    ...   INTEGER :: OPERATES_ON = CELL_COLUMN
    ... CONTAINS
    ...   PROCEDURE, NOPASS :: code => compute_cu_code
    ... END TYPE compute_cu
    ... contains
    ...   subroutine compute_cu_code()
    ...   end subroutine
    ... end module""")
    >>> fortran_reader = FortranReader()
    >>> kernel_container = fortran_reader.psyir_from_source(CODE)
    >>> trans = RaisePSyIR2LFRicKernTrans()
    >>> trans.apply(kernel_container, {"metadata_name": "compute_cu"})

    '''
    def __init__(self):
        super().__init__()

    def validate(self, node, options=None):
        '''Validate the supplied PSyIR tree.

        :param node: a PSyIR node that is the root of a PSyIR tree.
        :type node: :py:class:`psyclone.psyir.node.Container`
        :param options: a dictionary with options for transformations.
        :type options: Optional[Dict[str: str]]

        :raises TransformationError: if the supplied node is not a
            Container.
        :raises TransformationError: if the supplied node argument has
            a parent.
        :raises TransformationError: if the metadata name has not been
            provided in the options argument.
        :raises TransformationError: if the metadata name has not been
            set or does not exist in the code.
        :raises TransformationError: if the metadata symbol does not
            reside in a Container (as opposed to a FileContainer).

        '''
        super().validate(node, options=options)

        if not isinstance(node, Container):
            raise TransformationError(
                f"Error in {self.name} transformation. The supplied node "
                f"should be a Container but found '{type(node).__name__}'.")

        if node.parent:
            raise TransformationError(
                f"Error in {self.name} transformation. The supplied node "
                f"should be the root of a PSyIR tree but this node has a "
                f"parent ({type(node.parent).__name__}).")

        try:
            metadata_name = options["metadata_name"]
        except (TypeError, KeyError) as info:
            names = list(options.keys()) if options else []
            raise TransformationError(
                f"Error in {self.name} transformation. This "
                f"transformation requires the name of the variable "
                f"containing the metadata to be provided in the options "
                f"argument with lookup name 'metadata_name', but found "
                f"'{names}'.") from info

        try:
            FortranReader.validate_name(metadata_name)
        except (TypeError, ValueError) as err:
            raise TransformationError(
                f"Error in {self.name} transformation. This "
                f"transformation requires the name of the variable "
                f"containing the metadata to be set to a "
                f"valid Fortran name, but found '{metadata_name}'.") from err

        metadata_symbol, scoping_node = find_symbol(node, metadata_name)
        if not metadata_symbol:
            raise TransformationError(
                f"Error in {self.name} transformation. The metadata name "
                f"'{metadata_name}' provided to the transformation "
                f"does not correspond to a symbol in the supplied PSyIR.")

        # Find the nearest ancestor container including self. There
        # will always be at least one ancestor container as otherwise
        # an earlier test will fail.
        # TODO issue #1886. Avoid replicating tests (see gocean
        # raising transformation).
        container = scoping_node.ancestor(Container, include_self=True)
        if isinstance(container, FileContainer):
            raise TransformationError(
                f"Error in {self.name} transformation. The Container in "
                f"which the metadata symbol resides is a FileContainer, "
                f"but should be a generic Container.")

        # Check that the metadata can be generated without any errors.
        _ = LFRicKernelMetadata.create_from_psyir(metadata_symbol)

    def apply(self, node, options=None):
        '''Raise the supplied language-level kernel to LFRic-specific kernel
        PSyIR. Specialises the kernel container to an LFRic-specific
        subclass, populates this subclass with the kernel metadata
        extracted from the metadata symbol as specified in
        metadata_name (which is supplied via the options argument) and
        removes the symbol from the symbol table.

        :param node: a kernel represented in generic PSyIR.
        :type node: :py:class:`psyclone.psyir.node.Container`
        :param options: a dictionary with options for transformations. \
            This is expected to contain the metadata_name.
        :type options: Optional[Dict[str: str]]

        '''
        self.validate(node, options=options)

        # The name of the PSyIR symbol containing the metadata.
        metadata_name = options["metadata_name"]

        # Find the metadata symbol based on the supplied name.
        metadata_symbol, scoping_node = find_symbol(node, metadata_name)

        # Find the container in which this metadata resides.
        container = scoping_node.ancestor(Container, include_self=True)

        # Create the metadata.
        metadata = LFRicKernelMetadata.create_from_psyir(metadata_symbol)

        # Remove metadata symbol.
        # TODO issue #898: support needs to be added for removing a
        # DataSymbol from the symbol table. At the moment we need to
        # use internal methods.
        symbol_table = scoping_node.symbol_table
        # pylint: disable=protected-access
        norm_name = symbol_table._normalize(metadata_symbol.name)
        symbol_table._symbols.pop(norm_name)
        # pylint: enable=protected-access

        # Replace container
        children = container.pop_all_children()
        lfric_container = LFRicKernelContainer.create(
            container.name, metadata, container.symbol_table.detach(),
            children)
        container.replace_with(lfric_container)


# The list of module members that we wish AutoAPI to generate
# documentation for. (See https://psyclone-ref.readthedocs.io)
__all__ = ['RaisePSyIR2LFRicKernTrans', 'find_symbol']
