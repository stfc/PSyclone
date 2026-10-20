# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2026 Science and Technology
#                         Facilities Council.
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

"""Common kernel definitions and domain-specific kernel PSyIR nodes."""

from __future__ import annotations

from dataclasses import dataclass
from typing import Optional

from psyclone.domain.common.kernel.metadata import KernelMetadata
from psyclone.parse.utils import ParseError
from psyclone.psyir.nodes import Container, Node, Routine


@dataclass(frozen=True)
class Kernel:
    """The psyir of its source implementation together with its metadata"""

    metadata: "KernelMetadata"
    psyir: Optional[Node] = None
    procedures: tuple[Routine, ...] = ()
    resolved_procedure_name: Optional[str] = None

    def __post_init__(self) -> None:
        """Validate the common kernel-information container.

        :raises TypeError: if metadata or procedures have unsupported types.
        """
        if not isinstance(self.metadata, KernelMetadata):
            raise TypeError(
                "Kernel metadata must implement KernelMetadata but "
                f"found '{type(self.metadata).__name__}'."
            )
        if not isinstance(self.procedures, tuple) or not all(
                isinstance(procedure, Routine)
                for procedure in self.procedures):
            raise TypeError(
                "Kernel procedures must be a tuple of Routines.")

    @property
    def procedure_name(self) -> Optional[str]:
        """:returns: the resolved procedure or interface name."""
        return (self.resolved_procedure_name or
                getattr(self.metadata, "procedure_name", None))

    @classmethod
    def create_from_psyir(
            cls,
            metadata_type: type[KernelMetadata],
            psyir: Node,
            name: Optional[str] = None,
    ) -> Kernel:
        """Assemble metadata and implementation from complete kernel PSyIR.

        :param metadata_type: the KernelMetadata subclass to construct.
        :param psyir: the complete kernel PSyIR tree.
        :param name: optional metadata type name.

        :returns: the metadata, source PSyIR and resolved implementations.

        :raises TypeError: if the PSyIR or metadata class is unsupported.

        :raises ParseError: if metadata validation or procedure resolution
            fails.
        """
        if not isinstance(psyir, Node):
            raise TypeError(
                f"Expected PSyIR Node but found '{type(psyir).__name__}'.")
        if (not isinstance(metadata_type, type) or
                not issubclass(metadata_type, KernelMetadata)):
            raise TypeError(
                "Expected a KernelMetadata subclass but found "
                f"'{metadata_type}'.")
        container, symbol = metadata_type.find_symbol(psyir, name=name)
        try:
            metadata = metadata_type.create_from_psyir(symbol)
            metadata.validate()
        except (TypeError, ValueError) as err:
            raise ParseError(
                f"Invalid kernel metadata in '{symbol.name}': {err}") from err
        procedure_name, implementations = metadata.resolve_procedures(
            container)
        return cls(metadata, psyir, implementations, procedure_name)


class KernelContainer(Container):
    '''A domain-specific Container. This specialises the generic Container
    node and adds in any domain-specific information.

    :param str name: the name of the container.
    :param metadata: the metadata object.
    :type metadata: \
        :py:class:`psyclone.domain.common.kernel.KernelMetadata`
    :param kwargs: additional keyword arguments to pass to parent \
        constructor.
    :type kwargs: unwrapped dict

    '''
    def __init__(self, name, metadata, **kwargs):
        super().__init__(name, **kwargs)
        # The metadata object capturing kernel metadata.
        self._metadata = metadata

    @classmethod
    def create(cls, name, metadata, symbol_table, children):
        '''Create a kernel Container instance given a name, metadata, a symbol
        table and a list of child nodes. A domain-specific kernel is
        created with the metadata describing the kernel interface for
        a single kernel routine within the container.

        :param str name: the name of the Container.
        :param metadata: the metadata object.
        :type metadata: \
            :py:class:`psyclone.domain.common.kernel.KernelMetadata`
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
        :returns: the kernel metadata object.
        :rtype: :py:class:`psyclone.domain.common.kernel.KernelMetadata`
        '''
        return self._metadata

    def lower_to_language_level(self):
        '''Lower this domain-specific container to language level psyir.

        :returns: the lowered version of this node.
        :rtype: :py:class:`psyclone.psyir.nodes.Node`

        '''
        # Create metadata symbol and add it to the container symbol
        # table.
        data_symbol = self.metadata.lower_to_psyir()
        self.symbol_table.add(data_symbol)

        # Replace this kernel container with a generic container
        children = self.pop_all_children()
        generic_container = Container.create(
            self.name, self.symbol_table.detach(), children)
        self.replace_with(generic_container)
        return generic_container


__all__ = ["Kernel", "KernelContainer"]
