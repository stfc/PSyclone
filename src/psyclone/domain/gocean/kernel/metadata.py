# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2026, Science and Technology Facilities Council.
# All rights reserved.
# -----------------------------------------------------------------------------
"""Immutable GOcean kernel metadata extracted from language-level PSyIR."""

from dataclasses import dataclass, field
import re
from typing import ClassVar, Optional

from psyclone.configuration import Config
from psyclone.core import AccessType
from psyclone.domain.gocean.gocean_constants import GOceanConstants
from psyclone.errors import GenerationError, InternalError
from psyclone.parse.utils import ParseError
from psyclone.psyir.frontend.fortran import FortranReader
from psyclone.psyir.nodes import (
    ArrayConstructor, Call, Container, FileContainer, Literal, Reference,
    Node, Routine)
from psyclone.psyir.symbols import DataTypeSymbol, UnsupportedFortranType


def _normalise(value, description):
    """Return a validated, lower-case metadata name."""
    if not isinstance(value, str):
        raise TypeError(
            f"Expected {description} to be a string but found "
            f"'{type(value).__name__}'."
        )
    return value.lower()


def _check(value, valid_values, description):
    """Return a normalised value after checking its vocabulary."""
    value = _normalise(value, description)
    if value not in valid_values:
        raise ValueError(
            f"Expected {description} to be one of {valid_values} but found "
            f"'{value}'."
        )
    return value


@dataclass(frozen=True, slots=True)
class GOceanStencil:
    """Immutable stencil information consumed by the GOcean PSy layer."""

    rows: Optional[tuple[str, str, str]] = None
    pointwise_name: ClassVar[str] = "go_pointwise"

    def __post_init__(self):
        if self.rows is None:
            return
        if not isinstance(self.rows, tuple) or len(self.rows) != 3:
            raise ValueError(
                "A GOcean stencil must contain exactly three rows."
            )
        for row in self.rows:
            if not isinstance(row, str):
                raise TypeError("GOcean stencil rows must be strings.")
            if not re.fullmatch(r"[0-9]{3}", row):
                raise ValueError(
                    "GOcean stencil rows must each contain three decimal "
                    f"depths but found '{row}'."
                )
        if (
            self.rows[0] == "000"
            and self.rows[1] in ("000", "010")
            and self.rows[2] == "000"
        ):
            raise ValueError(
                "A zero-sized stencil must use go_pointwise."
            )

    @property
    def has_stencil(self):
        """Whether this represents a non-pointwise access."""
        return self.rows is not None

    @property
    def name(self):
        """Return the pointwise name, or ``None`` for an explicit stencil."""
        return None if self.has_stencil else self.pointwise_name

    def depth(self, index0, index1):
        """Return the access depth at the supplied relative indices."""
        if index0 not in (-1, 0, 1) or index1 not in (-1, 0, 1):
            raise GenerationError(
                "The indices arguments to the depth method in the "
                "GOStencil object must be between -1 and 1 but found "
                f"({index0},{index1})"
            )
        if self.rows is None:
            return int(index0 == 0 and index1 == 0)
        return int(self.rows[1 - index1][index0 + 1])


@dataclass(frozen=True, slots=True)
class GOceanFieldArgMetadata:
    """Metadata for one GOcean field argument."""

    access: str
    grid_point_type: str
    stencil: GOceanStencil = field(default_factory=GOceanStencil)

    def __post_init__(self):
        const = GOceanConstants()
        object.__setattr__(
            self, "access",
            _check(self.access, const.VALID_ACCESS_TYPES, "field access"))
        object.__setattr__(
            self, "grid_point_type",
            _check(
                self.grid_point_type, const.VALID_FIELD_GRID_TYPES,
                "field grid-point type"))
        if not isinstance(self.stencil, GOceanStencil):
            raise TypeError("Field stencil must be a GOceanStencil.")

    @property
    def form(self):
        """Return the metadata form."""
        return (
            GOceanConstants().VALID_STENCIL_NAME
            if self.stencil.has_stencil
            else "go_pointwise"
        )

    def fortran_string(self):
        """Return this argument as GOcean constructor syntax."""
        if self.stencil.has_stencil:
            form = (
                f"{GOceanConstants().VALID_STENCIL_NAME}"
                f"({', '.join(self.stencil.rows)})"
            )
        else:
            form = "go_pointwise"
        return (
            f"go_arg({self.access}, {self.grid_point_type}, {form})"
        )


@dataclass(frozen=True, slots=True)
class GOceanScalarArgMetadata:
    """Metadata for one GOcean scalar argument."""

    access: str
    datatype: str
    form: str = "go_pointwise"

    def __post_init__(self):
        const = GOceanConstants()
        object.__setattr__(
            self, "access",
            _check(self.access, const.VALID_ACCESS_TYPES, "scalar access"))
        object.__setattr__(
            self, "datatype",
            _check(
                self.datatype, const.VALID_SCALAR_TYPES, "scalar datatype"))
        object.__setattr__(
            self, "form",
            _check(
                self.form, const.VALID_STENCIL_NAMES, "scalar access form"))

    def fortran_string(self):
        """Return this argument as GOcean constructor syntax."""
        return f"go_arg({self.access}, {self.datatype}, {self.form})"


@dataclass(frozen=True, slots=True)
class GOceanGridPropertyArgMetadata:
    """Metadata for one GOcean grid-property argument."""

    access: str
    name: str

    def __post_init__(self):
        const = GOceanConstants()
        object.__setattr__(
            self, "access",
            _check(
                self.access, const.VALID_ACCESS_TYPES,
                "grid-property access"))
        name = _normalise(self.name, "grid-property name")
        properties = Config.get().api_conf("gocean").grid_properties
        if name not in properties:
            raise ValueError(
                f"Expected grid-property name to be one of "
                f"{list(properties)} but found '{name}'."
            )
        object.__setattr__(self, "name", name)

    def fortran_string(self):
        """Return this argument as GOcean constructor syntax."""
        return f"go_arg({self.access}, {self.name})"


_META_ARG_TYPES = (
    GOceanFieldArgMetadata,
    GOceanScalarArgMetadata,
    GOceanGridPropertyArgMetadata,
)


@dataclass(frozen=True, slots=True)
class GOceanKernelProcedure:
    """The name and optional PSyIR implementation of a GOcean kernel."""

    name: str
    ast: Optional[Routine] = field(default=None, compare=False, repr=False)


@dataclass(frozen=True, slots=True)
class GOceanArgDescriptor:
    """Immutable consumer-facing view of a GOcean metadata argument."""

    access: AccessType
    function_space: str
    metadata_index: int
    stencil: GOceanStencil
    argument_type: str
    grid_prop: str = ""

    def __post_init__(self):
        if not isinstance(self.access, AccessType):
            raise TypeError("Descriptor access must be an AccessType.")
        if not isinstance(self.metadata_index, int) or self.metadata_index < 0:
            raise InternalError(
                "The metadata index must be an integer and greater than or "
                f"equal to zero but got: {self.metadata_index}"
            )

    def __repr__(self):
        return (
            f"Descriptor({self.access}, {self.function_space}, "
            f"{self.metadata_index})"
        )

    __str__ = __repr__


@dataclass(frozen=True, slots=True)
class GOceanKernelMetadata:
    """Complete immutable GOcean kernel metadata."""

    iterates_over: str
    index_offset: str
    meta_args: tuple[
        GOceanFieldArgMetadata
        | GOceanScalarArgMetadata
        | GOceanGridPropertyArgMetadata,
        ...,
    ]
    procedure_name: str
    name: str
    psyir: Optional[Node] = field(default=None, compare=False, repr=False)

    # Preserve the concise nested names used by metadata-to-argument rules.
    FieldArg: ClassVar[type] = GOceanFieldArgMetadata
    ScalarArg: ClassVar[type] = GOceanScalarArgMetadata
    GridArg: ClassVar[type] = GOceanGridPropertyArgMetadata

    def __post_init__(self):
        const = GOceanConstants()
        object.__setattr__(
            self, "iterates_over",
            _check(
                self.iterates_over, const.VALID_ITERATES_OVER,
                "iterates_over"))
        object.__setattr__(
            self, "index_offset",
            _check(
                self.index_offset, const.VALID_OFFSET_NAMES,
                "index_offset"))
        object.__setattr__(self, "meta_args", tuple(self.meta_args))
        if any(
            not isinstance(argument, _META_ARG_TYPES)
            for argument in self.meta_args
        ):
            raise TypeError(
                "All meta_args entries must be GOcean argument metadata."
            )
        object.__setattr__(
            self, "procedure_name",
            _normalise(self.procedure_name, "procedure name"))
        object.__setattr__(self, "name", _normalise(self.name, "kernel name"))
        FortranReader.validate_name(self.procedure_name)
        FortranReader.validate_name(self.name)
        has_grid_property = any(
            isinstance(argument, GOceanGridPropertyArgMetadata)
            for argument in self.meta_args
        )
        has_field = any(
            isinstance(argument, GOceanFieldArgMetadata)
            for argument in self.meta_args
        )
        if has_grid_property and not has_field:
            raise ParseError(
                f"Kernel {self.name} requires a property of the grid but "
                "does not have any field objects as arguments."
            )

    @classmethod
    def create_from_psyir(cls, symbol):
        """Create metadata from a language-level PSyIR type symbol."""
        if not isinstance(symbol, DataTypeSymbol):
            raise TypeError(
                f"Expected a DataTypeSymbol but found "
                f"'{type(symbol).__name__}'."
            )
        if not isinstance(symbol.datatype, UnsupportedFortranType):
            raise InternalError(
                "Expected kernel metadata to be stored in the PSyIR as an "
                "UnsupportedFortranType, but found "
                f"'{type(symbol.datatype).__name__}'."
            )
        try:
            return _metadata_from_declaration(
                symbol.name, symbol.datatype.declaration)
        except (TypeError, ValueError) as err:
            raise ParseError(
                f"Invalid GOcean metadata '{symbol.name}': {err}"
            ) from err

    @classmethod
    def create_from_kernel_psyir(cls, psyir, name=None):
        """Extract the unique named GOcean metadata from complete PSyIR."""
        container, symbol, _ = find_metadata_symbol(psyir, name)
        metadata = cls.create_from_psyir(symbol)
        routines = [
            routine
            for routine in container.walk(Routine)
            if routine.name.lower() == metadata.procedure_name
        ]
        if not routines:
            raise ParseError(
                f"Kernel subroutine '{metadata.procedure_name}' not found."
            )
        return cls(
            metadata.iterates_over,
            metadata.index_offset,
            metadata.meta_args,
            metadata.procedure_name,
            metadata.name,
            psyir,
        )

    @classmethod
    def create_from_fortran_string(cls, source):
        """Create metadata by first translating its declaration to PSyIR."""
        if not isinstance(source, str):
            raise TypeError("GOcean metadata source must be a string.")
        wrapped = f"module metadata_mod\n{source}\nend module metadata_mod\n"
        try:
            psyir = FortranReader().psyir_from_source(wrapped)
        except Exception as err:
            raise ValueError(
                "Expected kernel metadata to be a Fortran derived type, but "
                f"found '{source}'."
            ) from err
        symbols = [
            symbol
            for container in psyir.walk(Container)
            if not isinstance(container, FileContainer)
            for symbol in container.symbol_table.symbols
            if isinstance(symbol, DataTypeSymbol)
            and isinstance(symbol.datatype, UnsupportedFortranType)
            and "extends(kernel_type)"
            in symbol.datatype.declaration.lower().replace(" ", "")
        ]
        if len(symbols) != 1:
            raise ParseError(
                "Expected exactly one GOcean kernel metadata declaration."
            )
        return cls.create_from_psyir(symbols[0])

    @property
    def _ast(self):
        """Return the complete language-level PSyIR retained by the parser."""
        return self.psyir

    @property
    def procedure(self):
        """Return the kernel procedure information."""
        implementation = None
        if self.psyir is not None:
            matches = [
                routine
                for routine in self.psyir.walk(Routine)
                if routine.name.lower() == self.procedure_name
            ]
            implementation = matches[0] if matches else None
        return GOceanKernelProcedure(self.procedure_name, implementation)

    @property
    def arg_descriptors(self):
        """Return immutable descriptors consumed by the GOcean PSy layer."""
        const = GOceanConstants()
        descriptors = []
        for index, argument in enumerate(self.meta_args):
            if isinstance(argument, GOceanFieldArgMetadata):
                descriptors.append(
                    GOceanArgDescriptor(
                        const.ACCESS_MAPPING[argument.access],
                        argument.grid_point_type,
                        index,
                        argument.stencil,
                        "field",
                    )
                )
            elif isinstance(argument, GOceanScalarArgMetadata):
                descriptors.append(
                    GOceanArgDescriptor(
                        const.ACCESS_MAPPING[argument.access],
                        argument.datatype,
                        index,
                        GOceanStencil(),
                        "scalar",
                    )
                )
            else:
                descriptors.append(
                    GOceanArgDescriptor(
                        const.ACCESS_MAPPING[argument.access],
                        "",
                        index,
                        GOceanStencil(),
                        "grid_property",
                        argument.name,
                    )
                )
        return tuple(descriptors)

    @property
    def nargs(self):
        """Return the number of arguments supplied by the algorithm layer."""
        return sum(
            not isinstance(argument, GOceanGridPropertyArgMetadata)
            for argument in self.meta_args
        )

    def fortran_string(self):
        """Return this metadata as a Fortran derived-type declaration."""
        arguments = ", &\n".join(
            f"    {argument.fortran_string()}"
            for argument in self.meta_args
        )
        return (
            f"TYPE, EXTENDS(kernel_type) :: {self.name}\n"
            f"  TYPE(go_arg), DIMENSION({len(self.meta_args)}) :: "
            f"meta_args = (/ &\n{arguments}/)\n"
            f"  INTEGER :: ITERATES_OVER = {self.iterates_over}\n"
            f"  INTEGER :: INDEX_OFFSET = {self.index_offset}\n"
            "  CONTAINS\n"
            f"    PROCEDURE, NOPASS :: code => {self.procedure_name}\n"
            f"END TYPE {self.name}\n"
        )

    def lower_to_psyir(self):
        """Return the language-level PSyIR symbol for this metadata."""
        return DataTypeSymbol(
            self.name, UnsupportedFortranType(self.fortran_string()))

    def __str__(self):
        return (
            f"GOcean kernel {self.name}, index-offset = "
            f"{self.index_offset}, iterates-over = {self.iterates_over}"
        )


def _expression(source):
    """Parse one metadata initializer into PSyIR."""
    try:
        return FortranReader().psyir_from_expression(source)
    except Exception as err:
        raise ParseError(
            f"Failed to parse metadata initializer '{source}'."
        ) from err


def _call_name(node):
    """Return the lower-case routine name for one PSyIR call."""
    if not isinstance(node, Call):
        raise ParseError(
            f"Expected a metadata constructor but found "
            f"'{type(node).__name__}'."
        )
    return node.routine.symbol.name.lower()


def _name(node):
    """Return a scalar metadata name or literal."""
    if isinstance(node, Reference):
        return node.symbol.name.lower()
    if isinstance(node, Literal):
        return node.value.lower()
    raise ParseError(
        f"Expected a metadata name or literal but found "
        f"'{type(node).__name__}'."
    )


def _parse_meta_arg(node):
    """Convert one go_arg PSyIR constructor to typed metadata."""
    if _call_name(node) != "go_arg":
        raise ParseError(
            "Each meta_args entry must use the go_arg constructor."
        )
    arguments = tuple(node.arguments)
    if len(arguments) not in (2, 3):
        raise ParseError(
            "Each go_arg constructor must contain two or three arguments "
            f"but found {len(arguments)}."
        )
    access = _name(arguments[0])
    second = _name(arguments[1])
    if len(arguments) == 2:
        return GOceanGridPropertyArgMetadata(access, second)
    const = GOceanConstants()
    if second in const.VALID_FIELD_GRID_TYPES:
        form = arguments[2]
        if isinstance(form, Call):
            if _call_name(form) != const.VALID_STENCIL_NAME:
                raise ParseError(
                    "A field metadata call must use go_stencil."
                )
            rows = tuple(_name(value) for value in form.arguments)
            stencil = GOceanStencil(rows)
        else:
            value = _name(form)
            if value not in const.VALID_STENCIL_NAMES:
                raise ValueError(
                    f"Expected field access form to be one of "
                    f"{const.VALID_STENCIL_NAMES} or "
                    f"{const.VALID_STENCIL_NAME} but found '{value}'."
                )
            stencil = GOceanStencil()
        return GOceanFieldArgMetadata(access, second, stencil)
    if second in const.VALID_SCALAR_TYPES:
        return GOceanScalarArgMetadata(
            access, second, _name(arguments[2]))
    raise ParseError(
        "Expected the second go_arg entry to identify a field or scalar, "
        f"but found '{second}'."
    )


def _component_initializers(declaration):
    """Return component initializers from a normalised declaration."""
    result = {}
    for line in declaration.splitlines():
        if "=" not in line or "::" not in line:
            continue
        lhs, rhs = line.split("=", 1)
        component = lhs.split("::", 1)[1].strip()
        for name in ("meta_args", "iterates_over", "index_offset"):
            if re.match(rf"(?i)^{name}\b", component):
                result[name] = (line, rhs.strip())
                break
    return result


def _extent(line):
    """Return a literal declared rank-one extent."""
    match = re.search(r"(?i)\bdimension\s*\(\s*(\d+)\s*\)", line)
    if not match:
        match = re.search(r"(?i)\bmeta_args\s*\(\s*(\d+)\s*\)", line)
    return int(match.group(1)) if match else None


def _metadata_from_declaration(name, declaration):
    """Create GOcean metadata from an UnsupportedFortranType declaration."""
    compact_header = declaration.splitlines()[0].lower().replace(" ", "")
    if "extends(kernel_type)" not in compact_header:
        raise ParseError("GOcean kernel metadata must extend kernel_type.")
    components = _component_initializers(declaration)
    missing = {
        value
        for value in ("meta_args", "iterates_over", "index_offset")
        if value not in components
    }
    if missing:
        raise ParseError(
            f"Missing GOcean metadata component(s): {sorted(missing)}."
        )
    line, rhs = components["meta_args"]
    expression = _expression(rhs)
    if not isinstance(expression, ArrayConstructor):
        raise ParseError("meta_args must be an array constructor.")
    extent = _extent(line)
    if extent is None:
        raise ParseError("meta_args must declare a literal extent.")
    if extent != len(expression.children):
        raise ParseError(
            f"meta_args has extent {extent} but its constructor contains "
            f"{len(expression.children)} entries."
        )
    arguments = tuple(
        _parse_meta_arg(node) for node in expression.children)
    iterates_over = _name(_expression(components["iterates_over"][1]))
    index_offset = _name(_expression(components["index_offset"][1]))
    match = re.search(
        r"(?im)^\s*procedure\b[^:]*::\s*"
        r"(?:code\s*=>\s*)?([a-z][a-z0-9_]*)\s*$",
        declaration,
    )
    if not match:
        raise ParseError(
            "GOcean metadata must bind a kernel procedure."
        )
    return GOceanKernelMetadata(
        iterates_over,
        index_offset,
        arguments,
        match.group(1),
        name,
    )


def _module_containers(psyir):
    """Return module containers from complete PSyIR."""
    if not hasattr(psyir, "walk"):
        raise TypeError(
            f"Expected PSyIR but found '{type(psyir).__name__}'."
        )
    return [
        node
        for node in psyir.walk(Container)
        if not isinstance(node, FileContainer)
    ]


def find_metadata_symbol(psyir, name=None):
    """Find one unique GOcean metadata type symbol in complete PSyIR."""
    candidates = []
    for container in _module_containers(psyir):
        for symbol in container.symbol_table.symbols:
            if (
                isinstance(symbol, DataTypeSymbol)
                and isinstance(symbol.datatype, UnsupportedFortranType)
                and "extends(kernel_type)"
                in symbol.datatype.declaration.lower().replace(" ", "")
                and (name is None or symbol.name.lower() == name.lower())
            ):
                candidates.append((container, symbol))
    if not candidates:
        description = f" '{name}'" if name else ""
        raise ParseError(
            f"GOcean kernel metadata{description} does not exist in PSyIR."
        )
    if len(candidates) != 1:
        raise ParseError("GOcean kernel metadata is not unique in PSyIR.")
    return candidates[0][0], candidates[0][1], candidates[0][1].name
