# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2022-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

'''Raise generic PSyIR representing a kernel-layer routine to
LFRic kernel-layer-specific PSyIR which uses specialised classes.

'''
from typing import Any, Optional

from psyclone.domain.lfric.kernel import (
    LFRicKernelContainer, LFRicKernelMetadata)
from psyclone.parse.utils import ParseError
from psyclone.psyGen import Transformation
from psyclone.psyir.frontend.fortran import FortranReader
from psyclone.psyir.nodes import Container
from psyclone.psyir.transformations import TransformationError
from psyclone.utils import transformation_documentation_wrapper


@transformation_documentation_wrapper
class RaisePSyIR2LFRicKernTrans(Transformation):
    '''Raise a generic PSyIR representation of a kernel-layer routine and
    metadata to an LFRic version with specialised domain-specific
    nodes and symbols. This is currently limited to the specialisation
    of kernel metadata.

    >>> from psyclone.configuration import Config
    >>> from psyclone.domain.lfric.transformations import \
            RaisePSyIR2LFRicKernTrans
    >>> from psyclone.psyir.frontend.fortran import FortranReader
    >>> config = Config.get().api_conf("lfric")
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

    def validate(self, node: Container,
                 options: Optional[dict[str, str]] = None,
                 **kwargs: Any) -> None:
        '''Validate the supplied PSyIR tree.

        :param node: a PSyIR node that is the root of a PSyIR tree.
        :param options: a dictionary with options for transformations.

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
        super().validate(node, options=options, **kwargs)

        if not options:
            self.validate_options(**kwargs)

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

        try:
            _, metadata_symbol = LFRicKernelMetadata.find_symbol(
                node, metadata_name)
        except ParseError as err:
            raise TransformationError(
                f"Error in {self.name} transformation. The metadata name "
                f"'{metadata_name}' provided to the transformation "
                f"does not correspond to a symbol in the supplied PSyIR."
            ) from err

        # Check that the metadata can be generated without any errors.
        _ = LFRicKernelMetadata.create_from_psyir(metadata_symbol)

    def apply(self, node: Container,
              options: Optional[dict[str, str]] = None,
              **kwargs: Any) -> None:
        '''Raise the supplied language-level kernel to LFRic-specific kernel
        PSyIR. Specialises the kernel container to an LFRic-specific
        subclass, populates this subclass with the kernel metadata
        extracted from the metadata symbol as specified in
        metadata_name (which is supplied via the options argument) and
        removes the symbol from the symbol table.

        :param node: a kernel represented in generic PSyIR.
        :param options: a dictionary with options for transformations.

        '''
        self.validate(node, options=options, **kwargs)

        # The name of the PSyIR symbol containing the metadata.
        metadata_name = options["metadata_name"]

        # Find the metadata symbol based on the supplied name.
        container, metadata_symbol = LFRicKernelMetadata.find_symbol(
            node, metadata_name)

        # Create the metadata.
        metadata = LFRicKernelMetadata.create_from_psyir(metadata_symbol)

        # Remove metadata symbol.
        # TODO issue #898: support needs to be added for removing a
        # DataSymbol from the symbol table. At the moment we need to
        # use internal methods.
        symbol_table = container.symbol_table
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
# documentation for.
__all__ = ['RaisePSyIR2LFRicKernTrans']
