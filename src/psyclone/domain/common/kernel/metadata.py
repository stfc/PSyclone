# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2026 Science and Technology
#                         Facilities Council.
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

"""Common interfaces and PSyIR utilities for kernel metadata."""

from __future__ import annotations

from abc import ABC, abstractmethod
from dataclasses import dataclass
from typing import Iterable, Optional, TYPE_CHECKING, Union

from psyclone.parse.utils import ParseError
from psyclone.psyir.frontend.fortran import FortranReader
from psyclone.psyir.nodes import (
    ArrayConstructor, Container, FileContainer, Literal, Node, Reference,
    Routine)
from psyclone.psyir.symbols import (
    ArrayType, DataTypeSymbol, StructureType)

if TYPE_CHECKING:
    from psyclone.psyir.symbols import DataSymbol


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


class KernelMetadata(ABC):
    """Common interface implemented by API-specific kernel metadata."""

    @classmethod
    @abstractmethod
    def create_from_psyir(
            cls,
            psyir: Union[DataTypeSymbol, Node],
            name: Optional[str] = None,
    ) -> KernelMetadata:
        """Create metadata from language-level PSyIR.

        :param psyir: metadata type symbol or PSyIR containing its declaration.
        :param name: optional metadata type name to select from the PSyIR.
        :returns: the API-specific metadata parsed from the declaration.
        :raises TypeError: if the supplied PSyIR has an unsupported type.
        :raises ParseError: if the metadata declaration is invalid.
        """

    @classmethod
    @abstractmethod
    def find_symbol(
            cls,
            psyir: Node,
            name: Optional[str] = None,
    ) -> tuple[Container, DataTypeSymbol]:
        """Find the containing module and metadata symbol for a kernel.

        API-specific naming and discovery rules apply here.

        :param psyir: the complete kernel PSyIR tree.
        :param name: optional metadata type name; API naming rules apply if
            omitted.
        :returns: the containing module and selected metadata symbol.
        :raises ParseError: if the required metadata declaration cannot be
            identified.
        """

    @abstractmethod
    def resolve_procedures(
            self,
            container: Container,
    ) -> tuple[str, tuple[Routine, ...]]:
        """Return the resolved procedure name and tuple of implementations.

        :param container: the module containing the kernel implementation.
        :returns: the resolved procedure name and implementing routines.
        :raises ParseError: if the implementation cannot be resolved.
        """

    def validate(self) -> None:
        """Validate additional constraints before assembling a complete kernel.

        APIs whose constructors fully validate their metadata need no further
        checks. Other APIs override this method with their remaining checks.
        """

    @property
    @abstractmethod
    def nargs(self) -> int:
        """:returns: the number of algorithm-layer arguments."""

    @abstractmethod
    def fortran_string(self) -> str:
        """:returns: the metadata as a Fortran derived-type declaration."""

    def lower_to_psyir(self) -> DataTypeSymbol:
        """:returns: a language-level PSyIR symbol for this metadata."""
        source = (f"module metadata_mod\n{self.fortran_string()}"
                  "end module metadata_mod\n")
        container = next(
            node for node in FortranReader().psyir_from_source(source).walk(
                Container) if not isinstance(node, FileContainer))
        return container.symbol_table.lookup(getattr(self, "name"))

    @staticmethod
    def _validate_and_get_value(node: Node) -> str:
        """Return a lower-case name represented by a PSyIR metadata node.

        :param node: a metadata reference or literal.
        :returns: the lower-case metadata name or literal value.
        :raises ParseError: if the node is neither a reference nor a literal.
        """
        if isinstance(node, Reference):
            return node.symbol.name.lower()
        if isinstance(node, Literal):
            return node.value.lower()
        raise ParseError(
            "Expected a metadata name or literal but found "
            f"'{type(node).__name__}'."
        )

    @staticmethod
    def _validate_and_get_array_values(
            component: Union[DataSymbol, StructureType.ComponentType],
            component_name: str,
    ) -> tuple[Node, ...]:
        """Validate a rank-one metadata component and return its values.

        :param component: the metadata array component.
        :param component_name: the component name to use in diagnostics.
        :returns: the values from the component's initialisation expression.
        :raises ParseError: if the array is not rank one with literal integer
            bounds,
            or its extent does not match the number of initial values.
        """
        component_type = component.datatype
        if (not isinstance(component_type, ArrayType) or
                len(component_type.shape) != 1 or
                not isinstance(component_type.shape[0],
                               ArrayType.ArrayBounds) or
                not isinstance(component_type.shape[0].lower, Literal) or
                not isinstance(component_type.shape[0].upper, Literal)):
            raise ParseError(
                f"Metadata component '{component_name}' must be an array."
            )
        bounds = component_type.shape[0]
        try:
            extent = int(bounds.upper.value) - int(bounds.lower.value) + 1
        except ValueError as err:
            raise ParseError(
                f"Metadata component '{component_name}' must have a literal "
                "extent."
            ) from err
        values = (
            tuple(component.initial_value.children)
            if isinstance(component.initial_value, ArrayConstructor)
            else (component.initial_value,)
        )
        if extent != len(values):
            raise ParseError(
                f"Metadata component '{component_name}' has extent {extent} "
                f"but its constructor contains {len(values)} values."
            )
        return values

    @staticmethod
    def find_symbols(
            psyir: Node,
            name: Optional[str] = None,
    ) -> list[tuple[Container, DataTypeSymbol]]:
        """Find kernel metadata symbols and their containing modules.

        :param psyir: the PSyIR tree to search.
        :param name: optional metadata type name to match, ignoring case.
        :returns: matching module and metadata-symbol pairs in traversal order.
        :raises TypeError: if psyir is not a PSyIR node.
        """
        if not isinstance(psyir, Node):
            raise TypeError(
                f"Expected PSyIR Node but found '{type(psyir).__name__}'."
            )
        result = []
        for container in psyir.walk(Container):
            if isinstance(container, FileContainer):
                continue
            for symbol in container.symbol_table.symbols:
                # pylint: disable=too-many-boolean-expressions
                if (isinstance(symbol, DataTypeSymbol) and
                        isinstance(symbol.datatype, StructureType) and
                        symbol.datatype.extends and
                        (symbol.datatype.extends.name.lower() ==
                         "kernel_type") and
                        (name is None or symbol.name.lower() == name.lower())):
                    result.append((container, symbol))
        return result

    @classmethod
    def _validate_and_get_symbol(
            cls,
            psyir: Union[DataTypeSymbol, Node],
            api_name: str,
            name: Optional[str] = None,
    ) -> DataTypeSymbol:
        """Return the unique metadata symbol in a PSyIR tree or the supplied
        symbol itself.

        :param psyir: a metadata type symbol or tree containing its
            declaration.
        :param api_name: the API name to use in diagnostics.
        :param name: optional metadata type name to select from a tree.
        :returns: the supplied symbol or the unique matching metadata symbol.
        :raises ParseError: if a tree contains zero or multiple matching
            declarations.
        """
        if isinstance(psyir, Node):
            candidates = cls.find_symbols(psyir, name=name)
            if len(candidates) != 1:
                raise ParseError(
                    f"Expected exactly one {api_name} kernel metadata "
                    f"declaration but found {len(candidates)}."
                )
            symbol = candidates[0][1]
        else:
            symbol = psyir
        return symbol

    @staticmethod
    def _validate_and_get_structure(
            symbol: DataTypeSymbol,
            api_name: str,
    ) -> StructureType:
        """Validate a kernel metadata symbol and return its structure type.

        :param symbol: the metadata type symbol to validate.
        :param api_name: the API name to use in diagnostics.
        :returns: the structure datatype of the metadata declaration.
        :raises TypeError: if symbol is not a DataTypeSymbol with a
            StructureType.
        :raises ParseError: if the declared type does not extend kernel_type.
        """
        if not isinstance(symbol, DataTypeSymbol):
            raise TypeError(
                f"Expected a DataTypeSymbol but found "
                f"'{type(symbol).__name__}'."
            )
        if not isinstance(symbol.datatype, StructureType):
            raise TypeError(
                "Expected kernel metadata to use StructureType but found "
                f"'{type(symbol.datatype).__name__}'."
            )
        datatype = symbol.datatype
        if (not datatype.extends or
                datatype.extends.name.lower() != "kernel_type"):
            raise ParseError(
                f"{api_name} kernel metadata must extend kernel_type.")
        return datatype


def normalise(
        value: str,
        description: str,
        valid_values: Optional[Iterable[str]] = None,
) -> str:
    """Validate and lower-case one metadata name.

    :param value: the metadata string to normalise.
    :param description: the name of the metadata property for diagnostics.
    :param valid_values: optional collection of permitted lower-case values.
    :returns: the validated lower-case string.
    :raises TypeError: if value is not a string.
    :raises ValueError: if the normalised value is not permitted.
    """
    if not isinstance(value, str):
        raise TypeError(
            f"Expected {description} to be a string but found "
            f"'{type(value).__name__}'."
        )
    result = value.lower()
    if valid_values and result not in valid_values:
        raise ValueError(
            f"Expected {description} to be one of {valid_values} but found "
            f"'{result}'."
        )
    return result


__all__ = ["Kernel", "KernelMetadata", "normalise"]
