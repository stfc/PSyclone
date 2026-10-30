# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2022-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

'''Raise generic PSyIR representing a kernel-layer routine to
PSyclone kernel-layer-specific PSyIR which uses specialised classes.

'''
from typing import Any, Optional

from psyclone.domain.gocean.kernel import (
    GOceanKernelMetadata, GOceanKernelContainer)
from psyclone.errors import PSycloneError
from psyclone.gocean1p0 import GOSymbolTable, GOKernelSchedule
from psyclone.parse.utils import ParseError
from psyclone.psyGen import Transformation
from psyclone.psyir.frontend.fortran import FortranReader
from psyclone.psyir.nodes import Container, Routine
from psyclone.psyir.transformations import TransformationError
from psyclone.utils import transformation_documentation_wrapper


@transformation_documentation_wrapper
class RaisePSyIR2GOceanKernTrans(Transformation):
    '''Raise a generic PSyIR representation of a kernel-layer routine
    to a PSyclone version with specialised domain-specific nodes and
    symbols. This is currently limited to the specialisation of kernel
    metadata.

    >>> from psyclone.domain.gocean.transformations import \
            RaisePSyIR2GOceanKernTrans
    >>> from psyclone.psyir.frontend.fortran import FortranReader
    >>> CODE = ("""
    ... MODULE example
    ... TYPE, EXTENDS(kernel_type) :: compute_cu
    ...   TYPE(go_arg), DIMENSION(4) :: meta_args = (/         &
    ...     go_arg(GO_WRITE, GO_CU, GO_POINTWISE),             &
    ...     go_arg(GO_READ, GO_CT, GO_STENCIL(000, 011, 000)), &
    ...     go_arg(GO_READ, GO_GRID_AREA_T),                   &
    ...     go_arg(GO_READ, GO_R_SCALAR, GO_POINTWISE)/)
    ...   INTEGER :: ITERATES_OVER = GO_ALL_PTS
    ...   INTEGER :: index_offset = GO_OFFSET_SW
    ... CONTAINS
    ...   PROCEDURE, NOPASS :: code => compute_cu_code
    ... END TYPE compute_cu
    ... contains
    ...   subroutine compute_cu_code()
    ...   end subroutine
    ... end module""")
    >>> fortran_reader = FortranReader()
    >>> kernel_container = fortran_reader.psyir_from_source(CODE)
    >>> trans = RaisePSyIR2GOceanKernTrans("compute_cu")
    >>> trans.apply(kernel_container)

    :param metadata_name: the name of the symbol containing the
        required kernel metadata in language-level PSyIR.

    :raises TransformationError: if the supplied metadata_name is
        invalid.

    '''
    def __init__(self, metadata_name: str) -> None:
        super().__init__()

        try:
            FortranReader.validate_name(metadata_name)
        except (TypeError, ValueError) as err:
            raise TransformationError(
                f"Error in {self.name} transformation. The "
                f"RaisePSyIR2GOceanKernTrans transformation requires the "
                f"name of the variable containing the metadata to be set to a "
                f"valid value, but found '{metadata_name}'.") from err

        # The name of the PSyIR symbol containing the metadata
        self._metadata_name = metadata_name

    def validate(self, node: Container,
                 options: Optional[dict[str, str]] = None,
                 **kwargs: Any) -> None:
        '''Validate the supplied PSyIR tree.

        :param node: a PSyIR node that is the root of a PSyIR tree.
        :param options: a dictionary with options for transformations.

        :raises TransformationError: if the metadata name has not been \
            set or does not exist in the code.
        :raises TransformationError: if the supplied node is not a Container.
        :raises TransformationError: if the supplied node argument has \
            a parent.
        :raises TransformationError: if the kernel metadata cannot be parsed.
        :raises TransformationError: if the Container does not contain the \
            routine which implements the kernel.

        '''

        super().validate(node, options=options, **kwargs)
        # TODO #2668: Deprecate options dict.
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
            container, metadata_symbol = GOceanKernelMetadata.find_symbol(
                node, self._metadata_name)
        except ParseError as err:
            raise TransformationError(
                f"Error in {self.name} transformation. The metadata name "
                f"({self._metadata_name}) provided to the transformation "
                f"does not correspond to a symbol in the supplied PSyIR."
            ) from err

        # Check that the metadata can be generated without any errors.
        try:
            mdata = GOceanKernelMetadata.create_from_psyir(metadata_symbol)
        except PSycloneError as err:
            raise TransformationError(
                f"Error in {self.name} transformation. Failed to create "
                f"metadata for kernel '{metadata_symbol.name}' from PSyIR. "
                f"Error was:\n{err.value}") from err

        proc_name = mdata.procedure_name.lower()
        for routine in container.walk(Routine):
            if routine.name.lower() == proc_name:
                break
        else:
            raise TransformationError(
                f"Error in {self.name} transformation. The Container in which"
                f" the metadata symbol resides does not contain the routine "
                f"that it names as implementing the kernel ('{proc_name}').")
        # TODO #288: Validate kernel arguments against metadata.

    def apply(self, node: Container,
              options: Optional[dict[str, str]] = None,
              **kwargs: Any) -> None:
        '''Raise the supplied language-level GOcean kernel PSyIR to
        GOcean-specific kernel PSyIR. Specialises the kernel container
        to a GOcean-specific subclass, populates this subclass with
        the kernel metadata extracted from the metadata symbol as
        specified in metadata_name and removes the symbol from the
        symbol table.

        :param node: a kernel represented in generic PSyIR.
        :param options: a dictionary with options for transformations.

        '''
        self.validate(node, options=options, **kwargs)

        # Find the metadata symbol based on the supplied name.
        container, metadata_symbol = GOceanKernelMetadata.find_symbol(
            node, self._metadata_name)

        # Create metadata.
        metadata = GOceanKernelMetadata.create_from_psyir(metadata_symbol)

        # Remove metadata symbol.
        # TODO #898: support needs to be added for removing a DataSymbol from
        # the symbol table. At the moment we need to use internal methods.
        # pylint: disable=protected-access
        symbol_table = container.symbol_table
        norm_name = symbol_table._normalize(metadata_symbol.name)
        symbol_table._symbols.pop(norm_name)
        # pylint: enable=protected-access

        # Replace container
        children = container.pop_all_children()
        gocean_container = GOceanKernelContainer.create(
            container.name, metadata, container.symbol_table.detach(),
            children)
        container.replace_with(gocean_container)

        # Replace the Routine with a GOcean Kernel Schedule. The validate()
        # method has checked that the routine exists.
        for routine in gocean_container.walk(Routine):
            if routine.name == metadata.procedure_name:
                break

        # The validate() method has already checked that the routine exists.
        # pylint: disable=undefined-loop-variable
        gotable = GOSymbolTable.create_from_table(routine.symbol_table)
        gokernsched = GOKernelSchedule(routine.symbol,
                                       symbol_table=gotable.detach())
        for child in routine.pop_all_children():
            gokernsched.addchild(child)
        routine.replace_with(gokernsched)


__all__ = ['RaisePSyIR2GOceanKernTrans']
