# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2026 Science and Technology
#                         Facilities Council.
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

"""Common interfaces and PSyIR utilities for kernel metadata."""

from abc import ABC, abstractmethod
from dataclasses import dataclass
from typing import Optional

from psyclone.domain.common.kernel.source import (
    parse_fortran_file, parse_fortran_source)
from psyclone.parse.utils import ParseError
from psyclone.psyir.frontend.fortran import FortranReader
from psyclone.psyir.nodes import (
    ArrayConstructor, Container, FileContainer, Literal, Node, Reference,
    Routine)
from psyclone.psyir.symbols import (
    ArrayType, DataTypeSymbol, StructureType)


def normalise(value, description, valid_values=None):
    """Validate and lower-case one metadata name."""
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


def metadata_value(node):
    """Return a lower-case name represented by a PSyIR metadata node."""
    if isinstance(node, Reference):
        return node.symbol.name.lower()
    if isinstance(node, Literal):
        return node.value.lower()
    raise ParseError(
        "Expected a metadata name or literal but found "
        f"'{type(node).__name__}'."
    )


def array_component_values(component, component_name):
    """Validate a rank-one metadata component and return its values."""
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


def kernel_metadata_symbols(psyir, name=None):
    """Find kernel metadata symbols and their containing modules."""
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
                    symbol.datatype.extends.name.lower() == "kernel_type" and
                    (name is None or symbol.name.lower() == name.lower())):
                result.append((container, symbol))
    return result


def metadata_structure(symbol, api_name):
    """Validate a kernel metadata symbol and return its structure type."""
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
        raise ParseError(f"{api_name} kernel metadata must extend kernel_type.")
    return datatype


@dataclass(frozen=True, slots=True)
class KernelInfo:
    """A metadata declaration together with its source implementation."""

    metadata: "KernelMetadata"
    psyir: Optional[Node] = None
    procedures: tuple[Routine, ...] = ()
    resolved_procedure_name: Optional[str] = None

    def __post_init__(self):
        """Validate the common kernel-information container."""
        if not isinstance(self.metadata, KernelMetadata):
            raise TypeError(
                "KernelInfo metadata must implement KernelMetadata but "
                f"found '{type(self.metadata).__name__}'."
            )
        if not isinstance(self.procedures, tuple) or not all(
                isinstance(procedure, Routine)
                for procedure in self.procedures):
            raise TypeError("KernelInfo procedures must be a tuple of Routines.")

    @property
    def procedure_name(self):
        """:returns: the resolved procedure or interface name."""
        return (self.resolved_procedure_name or
                getattr(self.metadata, "procedure_name", None))

    @classmethod
    def create_from_psyir(cls, metadata_type, psyir, name=None):
        """Create kernel information using an API-specific metadata class."""
        return metadata_type.create_from_kernel_psyir(psyir, name=name)

    @classmethod
    def create_from_file(cls, metadata_type, path, name=None,
                         line_length=False):
        """Create kernel information from a Fortran file."""
        psyir = parse_fortran_file(path, line_length=line_length)
        return cls.create_from_psyir(metadata_type, psyir, name=name)

    @classmethod
    def create_from_source(cls, metadata_type, source, name=None):
        """Create kernel information from complete Fortran source."""
        psyir = parse_fortran_source(source)
        return cls.create_from_psyir(metadata_type, psyir, name=name)


class KernelMetadata(ABC):
    """Common interface implemented by API-specific kernel metadata."""

    @classmethod
    @abstractmethod
    def create_from_psyir(cls, symbol):
        """Create metadata from one language-level PSyIR type symbol."""

    @classmethod
    @abstractmethod
    def create_from_kernel_psyir(cls, psyir, name=None):
        """Extract metadata and procedures from complete kernel PSyIR."""

    @property
    @abstractmethod
    def nargs(self):
        """:returns: the number of algorithm-layer arguments."""

    @abstractmethod
    def fortran_string(self):
        """:returns: the metadata as a Fortran derived-type declaration."""

    @classmethod
    def create_from_fortran_string(cls, source):
        """Create metadata from a standalone derived-type declaration."""
        if not isinstance(source, str):
            raise TypeError("Kernel metadata source must be a string.")
        wrapped = f"module metadata_mod\n{source}\nend module metadata_mod\n"
        try:
            psyir = FortranReader().psyir_from_source(wrapped)
        except Exception as err:
            raise ValueError(
                "Expected kernel metadata to be a Fortran derived type, but "
                f"found '{source}'."
            ) from err
        symbols = kernel_metadata_symbols(psyir)
        if len(symbols) != 1:
            raise ParseError(
                "Expected exactly one kernel metadata declaration."
            )
        return cls.create_from_psyir(symbols[0][1])

    def lower_to_psyir(self):
        """:returns: a language-level PSyIR symbol for this metadata."""
        source = (f"module metadata_mod\n{self.fortran_string()}"
                  "end module metadata_mod\n")
        container = next(
            node for node in FortranReader().psyir_from_source(source).walk(
                Container) if not isinstance(node, FileContainer))
        return container.symbol_table.lookup(getattr(self, "name"))


__all__ = [
    "array_component_values",
    "kernel_metadata_symbols",
    "metadata_structure",
    "KernelInfo",
    "KernelMetadata",
    "metadata_value",
    "normalise",
]
